-module(channel_ds).
%%%
% channel_ds 是 channel domain service 缩写
% 频道领域服务层，提供缓存和复杂业务操作
%%%

-export([create_channel/4]).
-export([is_subscribed/2]).
-export([subscriber_uids/1]).
-export([subscribe/2]).
-export([unsubscribe/2]).
-export([publish_message/5]).
-export([list_by_ids_since/2]).
%% G3 thin wrappers for channel_logic_*
-export([find_by_id/2]).
-export([find_by_id_with_price/1]).
-export([list_managed/1]).
-export([find_by_custom_id/1]).
-export([update/2]).
-export([delete/1]).
-export([search/3]).
-export([list_discover/2]).
-export([list_subscribed/2]).
-export([increment_subscribers/3]).
-export([get_reaction_count/1]).
-export([has_viewed_message/2]).
-export([insert_message_view/4]).
-export([insert_reaction/5]).
-export([delete_reaction/4]).
-export([get_daily_stats/2]).
-export([page/5]).

-include("cache.hrl").
-include("log.hrl").

-define(CHANNEL_CACHE_KEY(ChannelId), {channel, ChannelId}).
-define(CHANNEL_SUBS_KEY(ChannelId), {channel_subs, ChannelId}).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 创建频道（事务）
%% @param Uid 创建者用户ID
%% @param Name 频道名称
%% @param Type 频道类型
%% @param Opts 其他选项（description, avatar, custom_id, tags）
-spec create_channel(integer(), binary(), integer(), map()) -> {ok, integer()} | {error, any()}.
create_channel(Uid, Name, Type, Opts) ->
    Now = elib_dt:now(),
    Data = #{
        name => Name,
        type => Type,
        creator_uid => Uid,
        created_at => Now,
        updated_at => Now
    },
    Data2 = add_optional_fields(Data, Opts),

    case
        elib_pg:with_tx(fun(Conn) ->
            % 创建频道
            case channel_repo:add(Conn, Data2) of
                {ok, ChannelId} ->
                    % 添加创建者为管理员（角色3）
                    AdminData = #{
                        channel_id => ChannelId,
                        user_id => Uid,
                        % 创建者
                        role => 3,
                        created_at => Now
                    },
                    case channel_admin_repo:add(Conn, AdminData) of
                        {ok, _} -> {ok, ChannelId};
                        {error, Reason} -> throw({abort_tx, Reason})
                    end;
                {error, Reason} ->
                    throw({abort_tx, Reason})
            end
        end)
    of
        {error, Reason} ->
            {error, normalize_error(Reason)};
        Result ->
            Result
    end.

%% @doc 添加可选字段
add_optional_fields(Data, Opts) ->
    Fields = [description, avatar, custom_id, tags],
    lists:foldl(
        fun(Field, Acc) ->
            case maps:get(Field, Opts, undefined) of
                undefined -> Acc;
                Val -> Acc#{Field => Val}
            end
        end,
        Data,
        Fields
    ).

%% @doc 检查用户是否已订阅频道（委托到 channel_subscription_ds 统一路径）
-spec is_subscribed(integer(), integer()) -> boolean().
is_subscribed(ChannelId, Uid) ->
    channel_subscription_ds:is_subscribed(ChannelId, Uid).

%% @doc 获取频道订阅者用户ID列表（带缓存）
-spec subscriber_uids(integer()) -> [integer()].
subscriber_uids(ChannelId) ->
    CacheKey = ?CHANNEL_SUBS_KEY(ChannelId),
    case imboy_cache:get(CacheKey) of
        {ok, Uids} ->
            Uids;
        undefined ->
            case channel_subscription_repo:list_by_channel(ChannelId) of
                {ok, Rows} ->
                    Uids = [maps:get(<<"user_id">>, R) || R <- Rows],
                    imboy_cache:set(CacheKey, Uids, ?HOUR),
                    Uids;
                _ ->
                    []
            end
    end.

%% @doc 订阅频道（使用事务保证原子性，P0-2 修复）
-spec subscribe(integer(), integer()) -> ok | {error, any()}.
subscribe(ChannelId, Uid) ->
    StartMs = elib_dt:millisecond(),
    % 使用事务保证订阅和计数更新的原子性
    Result = elib_pg:with_tx(fun(Conn) ->
        case channel_subscription_repo:upsert_active(Conn, ChannelId, Uid) of
            {ok, true} ->
                % 仅在真实状态变更时增加计数
                case channel_repo:increment_subscribers(Conn, ChannelId, 1) of
                    {ok, _} -> changed;
                    {error, Reason} -> throw({abort_tx, Reason})
                end;
            {ok, false} ->
                % 幂等订阅：原本已订阅
                noop;
            {error, Reason} ->
                throw({abort_tx, Reason})
        end
    end),
    case Result of
        changed ->
            % 清除订阅者缓存
            imboy_cache:flush(?CHANNEL_SUBS_KEY(ChannelId)),
            imboy_cache:flush(?CHANNEL_CACHE_KEY(ChannelId)),
            log_subscription_action(Uid, ChannelId, <<"subscribe">>, <<"changed">>, StartMs),
            ok;
        noop ->
            imboy_cache:flush(?CHANNEL_SUBS_KEY(ChannelId)),
            imboy_cache:flush(?CHANNEL_CACHE_KEY(ChannelId)),
            log_subscription_action(Uid, ChannelId, <<"subscribe">>, <<"noop">>, StartMs),
            ok;
        {error, Reason} ->
            log_subscription_action(Uid, ChannelId, <<"subscribe">>, <<"error">>, StartMs),
            {error, normalize_error(Reason)}
    end.

%% @doc 取消订阅频道（添加幂等检查，P0-2 修复）
-spec unsubscribe(integer(), integer()) -> ok | {error, any()}.
unsubscribe(ChannelId, Uid) ->
    StartMs = elib_dt:millisecond(),
    % 使用事务保证删除和计数更新的原子性
    Result = elib_pg:with_tx(fun(Conn) ->
        case channel_subscription_repo:delete(Conn, ChannelId, Uid) of
            {ok, Affected} when Affected > 0 ->
                % 只有实际发生状态变更才更新计数
                case channel_repo:increment_subscribers(Conn, ChannelId, -1) of
                    {ok, _} -> changed;
                    {error, Reason} -> throw({abort_tx, Reason})
                end;
            {ok, 0} ->
                % 幂等取消：原本已取消或不存在
                noop;
            {error, Reason} ->
                throw({abort_tx, Reason})
        end
    end),
    case Result of
        changed ->
            % 清除缓存
            imboy_cache:flush(?CHANNEL_SUBS_KEY(ChannelId)),
            imboy_cache:flush(?CHANNEL_CACHE_KEY(ChannelId)),
            log_subscription_action(Uid, ChannelId, <<"unsubscribe">>, <<"changed">>, StartMs),
            ok;
        noop ->
            imboy_cache:flush(?CHANNEL_SUBS_KEY(ChannelId)),
            imboy_cache:flush(?CHANNEL_CACHE_KEY(ChannelId)),
            log_subscription_action(Uid, ChannelId, <<"unsubscribe">>, <<"noop">>, StartMs),
            ok;
        {error, Reason} ->
            log_subscription_action(Uid, ChannelId, <<"unsubscribe">>, <<"error">>, StartMs),
            {error, normalize_error(Reason)}
    end.

%% @doc 发布频道消息
-spec publish_message(integer(), integer(), binary(), binary(), map()) ->
    {ok, integer()} | {error, any()}.
publish_message(ChannelId, AuthorId, Content, MsgType, Payload) ->
    % 获取作者信息
    User = user_repo:find_by_id(AuthorId, <<"nickname,avatar">>),
    AuthorName = maps:get(<<"nickname">>, User, <<>>),
    AuthorAvatar = maps:get(<<"avatar">>, User, <<>>),

    % 消息JSON编码
    PayloadJson =
        case jsone_encode:encode(Payload, [native_utf8]) of
            {ok, Json} -> Json;
            _ -> <<"{}">>
        end,

    Now = elib_dt:now(),
    Data = #{
        channel_id => ChannelId,
        author_id => AuthorId,
        author_name => AuthorName,
        author_avatar => AuthorAvatar,
        content => Content,
        msg_type => MsgType,
        payload => PayloadJson,
        created_at => Now
    },

    case channel_message_repo:add(Data) of
        {ok, MessageId} ->
            % 增加所有订阅者的未读计数
            increment_all_unread(ChannelId, AuthorId),
            {ok, MessageId};
        {error, Reason} ->
            {error, normalize_error(Reason)}
    end.

%% @doc 增加所有订阅者的未读计数
-spec increment_all_unread(integer(), integer()) -> ok.
increment_all_unread(ChannelId, AuthorId) ->
    % 使用批量更新SQL提高性能
    Tb = channel_subscription_repo:tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET unread_count = unread_count + 1 "
            "WHERE channel_id = $1 AND status = 1 AND user_id <> $2">>,
    _ = elib_pg:execute(Sql, [ChannelId, AuthorId]),
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec log_subscription_action(integer(), integer(), binary(), binary(), integer()) -> ok.
log_subscription_action(Uid, ChannelId, Action, Result, StartMs) ->
    CostMs = erlang:max(0, elib_dt:millisecond() - StartMs),
    ?INFO_LOG([
        channel_subscription_action,
        {uid, Uid},
        {channel_id, ChannelId},
        {action, Action},
        {result, Result},
        {cost_ms, CostMs}
    ]),
    ok.

-spec normalize_error(term()) -> binary().
normalize_error(Reason) when is_binary(Reason) ->
    Reason;
normalize_error(Reason) ->
    elib_cnv:safe_to_binary(Reason).

%% G3: channel_logic_sync 不应直调 channel_repo
-spec list_by_ids_since(list(integer()), integer() | binary()) ->
    {ok, list(map())} | {error, any()}.
list_by_ids_since(ChannelIds, Since) -> channel_repo:list_by_ids_since(ChannelIds, Since).

%% G3 thin wrappers: channel_logic_* 不应直调 channel_repo
-spec find_by_id(integer(), binary()) -> map() | {error, any()}.
find_by_id(ChannelId, Column) -> channel_repo:find_by_id(ChannelId, Column).

%% @doc 查找频道详情并包含 price/currency（付费频道展示用）
-spec find_by_id_with_price(integer() | binary()) -> map() | {error, any()}.
find_by_id_with_price(ChannelId) -> channel_repo:find_by_id_with_price(ChannelId).

-spec list_managed(integer()) -> {ok, list(map())} | {error, any()}.
list_managed(Uid) -> channel_repo:list_managed(Uid).

-spec find_by_custom_id(binary()) -> map() | {error, any()}.
find_by_custom_id(CustomId) -> channel_repo:find_by_custom_id(CustomId).

-spec update(integer(), map()) -> {ok, integer()} | {error, any()}.
update(ChannelId, Data) -> channel_repo:update(ChannelId, Data).

-spec delete(integer()) -> {ok, integer()} | {error, any()}.
delete(ChannelId) -> channel_repo:delete(ChannelId).

-spec search(binary(), integer(), binary()) -> {ok, list(map())} | {error, any()}.
search(Keyword, Limit, Column) -> channel_repo:search(Keyword, Limit, Column).

-spec list_discover(integer(), binary()) -> {ok, list(map())} | {error, any()}.
list_discover(Limit, Column) -> channel_repo:list_discover(Limit, Column).

-spec list_subscribed(integer(), binary()) -> {ok, list(map())} | {error, any()}.
list_subscribed(Uid, Column) -> channel_repo:list_subscribed(Uid, Column).

-spec increment_subscribers(epgsql:connection(), integer(), integer()) ->
    {ok, integer()} | {error, any()}.
increment_subscribers(Conn, ChannelId, Delta) ->
    channel_repo:increment_subscribers(Conn, ChannelId, Delta).

-spec get_reaction_count(integer()) -> {ok, map()} | {error, any()}.
get_reaction_count(ChannelId) -> channel_repo:get_reaction_count(ChannelId).

-spec has_viewed_message(integer(), integer()) -> boolean().
has_viewed_message(MessageId, UserId) -> channel_repo:has_viewed_message(MessageId, UserId).

-spec insert_message_view(integer(), integer(), integer(), binary()) -> ok | {error, any()}.
insert_message_view(ChannelId, MessageId, UserId, ViewedAt) ->
    channel_repo:insert_message_view(ChannelId, MessageId, UserId, ViewedAt).

-spec insert_reaction(integer(), integer(), integer(), binary(), binary()) -> ok | {error, any()}.
insert_reaction(ChannelId, MessageId, UserId, ReactionType, CreatedAt) ->
    channel_repo:insert_reaction(ChannelId, MessageId, UserId, ReactionType, CreatedAt).

-spec delete_reaction(integer(), integer(), integer(), binary()) -> ok | {error, any()}.
delete_reaction(ChannelId, MessageId, UserId, ReactionType) ->
    channel_repo:delete_reaction(ChannelId, MessageId, UserId, ReactionType).

-spec get_daily_stats(integer(), integer()) -> {ok, list(map())} | {error, any()}.
get_daily_stats(ChannelId, Days) -> channel_repo:get_daily_stats(ChannelId, Days).

-spec page(binary(), map(), binary(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(Column, Where, Order, Page, Size) ->
    Tb = channel_repo:tablename(),
    elib_pg:page_with_total(Tb, Column, Where, Order, Page, Size).
