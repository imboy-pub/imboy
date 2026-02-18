-module(channel_ds).
%%%
% channel_ds 是 channel domain service 缩写
% 频道领域服务层，提供缓存和复杂业务操作
%%%

-export([channel_id/0]).
-export([create_channel/4]).
-export([is_subscribed/2]).
-export([subscriber_uids/1]).
-export([subscribe/2]).
-export([unsubscribe/2]).
-export([publish_message/5]).
-export([get_channel/1]).

-include("cache.hrl").
-include("log.hrl").

-define(CHANNEL_CACHE_KEY(ChannelId), {channel, ChannelId}).
-define(CHANNEL_SUBS_KEY(ChannelId), {channel_subs, ChannelId}).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 生成新的频道ID
-spec channel_id() -> integer().
channel_id() ->
    {ok, [#{<<"cid">> := Cid}]} = elib_pg:query(
        "select nextval('public.channel_id_seq') as cid", []),
    Cid.

%% @doc 获取频道信息（带缓存）
-spec get_channel(integer()) -> map() | {error, any()}.
get_channel(ChannelId) ->
    CacheKey = ?CHANNEL_CACHE_KEY(ChannelId),
    case imboy_cache:get(CacheKey) of
        {ok, Channel} ->
            Channel;
        undefined ->
            case channel_repo:find_by_id(ChannelId, <<"*">>) of
                {error, Reason} -> {error, Reason};
                Channel ->
                    imboy_cache:set(CacheKey, Channel, ?HOUR),
                    Channel
            end
    end.

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

    elib_pg:with_tx(fun(Conn) ->
        % 创建频道
        case channel_repo:add(Conn, Data2) of
            {ok, ChannelId, _} ->
                % 添加创建者为管理员（角色3）
                AdminData = #{
                    channel_id => ChannelId,
                    user_id => Uid,
                    role => 3,  % 创建者
                    created_at => Now
                },
                case channel_admin_repo:add(Conn, AdminData) of
                    {ok, _, _} -> {ok, ChannelId};
                    {error, Reason} -> throw({abort_tx, Reason})
                end;
            {error, Reason} ->
                throw({abort_tx, Reason})
        end
    end).

%% @doc 添加可选字段
add_optional_fields(Data, Opts) ->
    Fields = [description, avatar, custom_id, tags],
    lists:foldl(fun(Field, Acc) ->
        case maps:get(Field, Opts, undefined) of
            undefined -> Acc;
            Val -> Acc#{Field => Val}
        end
    end, Data, Fields).

%% @doc 检查用户是否已订阅频道
-spec is_subscribed(integer(), integer()) -> boolean().
is_subscribed(ChannelId, Uid) ->
    Sub = channel_subscription_repo:find(ChannelId, Uid),
    map_size(Sub) > 0.

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

%% @doc 订阅频道
-spec subscribe(integer(), integer()) -> ok | {error, any()}.
subscribe(ChannelId, Uid) ->
    % 检查是否已订阅
    case is_subscribed(ChannelId, Uid) of
        true ->
            ok;
        false ->
            Now = elib_dt:now(),
            Data = #{
                channel_id => ChannelId,
                user_id => Uid,
                subscribed_at => Now,
                status => 1
            },
            case channel_subscription_repo:add(Data) of
                {ok, _} ->
                    % 更新订阅者数量
                    _ = channel_repo:increment_subscribers(ChannelId, 1),
                    % 清除订阅者缓存
                    imboy_cache:flush(?CHANNEL_SUBS_KEY(ChannelId)),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 取消订阅频道
-spec unsubscribe(integer(), integer()) -> ok | {error, any()}.
unsubscribe(ChannelId, Uid) ->
    case channel_subscription_repo:delete(ChannelId, Uid) of
        {ok, _} ->
            % 更新订阅者数量
            _ = channel_repo:increment_subscribers(ChannelId, -1),
            % 清除缓存
            imboy_cache:flush(?CHANNEL_SUBS_KEY(ChannelId)),
            imboy_cache:flush(?CHANNEL_CACHE_KEY(ChannelId)),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 发布频道消息
-spec publish_message(integer(), integer(), binary(), binary(), map()) -> {ok, integer()} | {error, any()}.
publish_message(ChannelId, AuthorId, Content, MsgType, Payload) ->
    % 获取作者信息
    User = user_repo:find_by_id(AuthorId, <<"nickname,avatar">>),
    AuthorName = maps:get(<<"nickname">>, User, <<>>),
    AuthorAvatar = maps:get(<<"avatar">>, User, <<>>),

    % 消息JSON编码
    PayloadJson = case jsone_encode:encode(Payload, [native_utf8]) of
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
        {ok, MessageId, _} ->
            % 增加所有订阅者的未读计数
            increment_all_unread(ChannelId),
            {ok, MessageId};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 增加所有订阅者的未读计数
-spec increment_all_unread(integer()) -> ok.
increment_all_unread(ChannelId) ->
    % 使用批量更新SQL提高性能
    Tb = channel_subscription_repo:tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET unread_count = unread_count + 1 "
            "WHERE channel_id = $1 AND status = 1">>,
    _ = elib_pg:execute(Sql, [ChannelId]),
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
