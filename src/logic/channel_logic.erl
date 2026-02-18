-module(channel_logic).
%%%
% channel_logic 是频道业务逻辑模块
% 提供频道创建、订阅、消息发布等业务逻辑
%%%

-export([channel_transfer/1]).
-export([message_transfer/1]).
-export([create_channel/5]).
-export([get_channel/2]).
-export([get_channel_by_custom_id/1]).
-export([update_channel/3]).
-export([delete_channel/2]).
-export([subscribe/2]).
-export([unsubscribe/2]).
-export([get_subscribed_channels/1]).
-export([get_managed_channels/1]).
-export([publish_message/5]).
-export([get_messages/3]).
-export([mark_as_read/3]).
-export([search_channels/2]).
-export([get_discover_channels/1]).
-export([add_admin/4]).
-export([remove_admin/3]).
% 统计相关
-export([get_channel_stats/1]).
-export([record_message_view/3]).
-export([add_reaction/4]).
-export([remove_reaction/4]).
-export([get_daily_stats/2]).
% 消息管理
-export([pin_message/3]).
-export([delete_message/2]).
% 订阅者管理
-export([get_subscribers/3]).
% 邀请相关（私有频道）
-export([create_invitation/3]).
-export([accept_invitation/2]).
-export([reject_invitation/2]).
-export([get_my_invitations/1]).
-export([get_sent_invitations/1]).
% 订单相关（付费频道）
-export([create_order/2]).
-export([pay_order/2]).
-export([get_my_orders/1]).
-export([get_order/2]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 转换频道数据中的 ID 字段为 HashID 格式
-spec channel_transfer(map()) -> map().
channel_transfer(Channel) when is_map(Channel) ->
    Channel2 = elib_hashids:replace_id(Channel),
    Channel3 = elib_hashids:replace_id(Channel2, <<"creator_uid">>),
    Channel3.

%% @doc 转换消息数据中的 ID 字段
-spec message_transfer(map()) -> map().
message_transfer(Message) when is_map(Message) ->
    Message2 = elib_hashids:replace_id(Message),
    Message3 = elib_hashids:replace_id(Message2, <<"channel_id">>),
    Message4 = elib_hashids:replace_id(Message3, <<"author_id">>),
    Message4.

%% @doc 创建频道
-spec create_channel(integer(), binary(), integer(), map(), integer()) ->
    {ok, map()} | {error, binary()}.
create_channel(Uid, Name, Type, Opts, MaxChannels) ->
    % 检查用户已创建的频道数量
    {ok, Channels} = channel_repo:list_managed(Uid),
    case length(Channels) >= MaxChannels of
        true ->
            {error, <<"已达频道创建上限"/utf8>>};
        false ->
            % 检查 custom_id 是否重复
            case maps:get(custom_id, Opts, undefined) of
                undefined ->
                    do_create_channel(Uid, Name, Type, Opts);
                CustomId when is_binary(CustomId), CustomId =/= <<>> ->
                    case channel_repo:find_by_custom_id(CustomId) of
                        {error, _} ->
                            do_create_channel(Uid, Name, Type, Opts);
                        _ ->
                            {error, <<"自定义ID已被使用"/utf8>>}
                    end;
                _ ->
                    do_create_channel(Uid, Name, Type, Opts)
            end
    end.

%% @doc 执行创建频道
do_create_channel(Uid, Name, Type, Opts) ->
    case channel_ds:create_channel(Uid, Name, Type, Opts) of
        {ok, ChannelId} ->
            case channel_repo:find_by_id(ChannelId, <<"*">>) of
                {error, Reason} -> {error, Reason};
                Channel -> {ok, channel_transfer(Channel)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 获取频道信息
-spec get_channel(binary(), integer()) -> {ok, map()} | {error, binary()}.
get_channel(ChannelIdBin, Uid) ->
    ChannelId = elib_hashids:decode(ChannelIdBin),
    case channel_repo:find_by_id(ChannelId, <<"*">>) of
        {error, _} -> {error, <<"频道不存在"/utf8>>};
        Channel when map_size(Channel) =:= 0 -> {error, <<"频道不存在"/utf8>>};
        Channel ->
            % 获取用户角色
            UserRole = channel_admin_repo:get_role(ChannelId, Uid),
            % 检查用户是否订阅了该频道
            IsSubscribed = case UserRole of
                0 -> channel_subscription_repo:is_subscribed(ChannelId, Uid);
                _ -> true  % 管理员视为已订阅
            end,
            Channel2 = Channel#{
                user_role => UserRole,
                is_subscribed => IsSubscribed
            },
            {ok, channel_transfer(Channel2)}
    end.

%% @doc 通过自定义ID获取频道
-spec get_channel_by_custom_id(binary()) -> {ok, map()} | {error, binary()}.
get_channel_by_custom_id(CustomId) ->
    case channel_repo:find_by_custom_id(CustomId) of
        {error, _} -> {error, <<"频道不存在"/utf8>>};
        Channel -> {ok, channel_transfer(Channel)}
    end.

%% @doc 更新频道信息
-spec update_channel(integer(), binary(), map()) -> {ok, map()} | {error, binary()}.
update_channel(Uid, ChannelIdBin, Data) ->
    ChannelId = elib_hashids:decode(ChannelIdBin),
    % 验证权限：只有创建者和管理员可以更新
    Role = channel_admin_repo:get_role(ChannelId, Uid),
    case Role >= 2 of
        false ->
            {error, <<"无权限操作"/utf8>>};
        true ->
            % 过滤允许更新的字段
            AllowedFields = [<<"name">>, <<"description">>, <<"avatar">>, <<"tags">>],
            FilteredData = maps:filter(fun(K, _) -> lists:member(K, AllowedFields) end, Data),
            case channel_repo:update(ChannelId, FilteredData#{updated_at => elib_dt:now()}) of
                {ok, _} ->
                    case channel_repo:find_by_id(ChannelId, <<"*">>) of
                        {error, Reason} -> {error, Reason};
                        Channel ->
                            % 通知订阅者频道更新
                            notify_channel_update(ChannelId, Channel),
                            {ok, channel_transfer(Channel)}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 删除频道
-spec delete_channel(integer(), binary()) -> ok | {error, binary()}.
delete_channel(Uid, ChannelIdBin) ->
    ChannelId = elib_hashids:decode(ChannelIdBin),
    % 只有创建者可以删除频道
    Role = channel_admin_repo:get_role(ChannelId, Uid),
    case Role == 3 of
        false ->
            {error, <<"只有创建者可以删除频道"/utf8>>};
        true ->
            % 获取订阅者列表用于通知
            SubscriberUids = channel_ds:subscriber_uids(ChannelId),
            % 删除频道
            case channel_repo:delete(ChannelId) of
                {ok, _} ->
                    % 发送删除通知
                    notify_channel_deleted(ChannelId, SubscriberUids),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 订阅频道
-spec subscribe(integer(), binary()) -> ok | {error, binary()}.
subscribe(Uid, ChannelIdBin) ->
    ChannelId = elib_hashids:decode(ChannelIdBin),
    % 检查频道是否存在
    case channel_repo:find_by_id(ChannelId, <<"id,type">>) of
        {error, _} ->
            {error, <<"频道不存在"/utf8>>};
        Channel ->
            Type = maps:get(<<"type">>, Channel, 0),
            case Type of
                1 -> % 私有频道 - 需要邀请
                    subscribe_private_channel(Uid, ChannelId);
                2 -> % 付费频道 - 需要购买
                    subscribe_paid_channel(Uid, ChannelId);
                _ -> % 公开频道
                    case channel_ds:subscribe(ChannelId, Uid) of
                        ok ->
                            % 发送订阅通知
                            notify_channel_subscribed(ChannelId, Uid),
                            ok;
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

%% @doc 订阅私有频道（内部函数）
-spec subscribe_private_channel(integer(), integer()) -> ok | {error, binary()}.
subscribe_private_channel(Uid, ChannelId) ->
    % 检查是否被邀请
    case channel_subscribe_ds:is_invited(ChannelId, Uid) of
        true ->
            % 使用邀请订阅
            case channel_subscribe_ds:subscribe_private(ChannelId, Uid, undefined) of
                ok ->
                    % 发送订阅通知
                    notify_channel_subscribed(ChannelId, Uid),
                    ok;
                {error, Reason} when is_binary(Reason) ->
                    {error, Reason};
                {error, Reason} ->
                    {error, elib:convert(Reason, binary)}
            end;
        false ->
            {error, <<"私有频道需要邀请才能订阅"/utf8>>}
    end.

%% @doc 订阅付费频道（内部函数）
-spec subscribe_paid_channel(integer(), integer()) -> ok | {error, binary()}.
subscribe_paid_channel(Uid, ChannelId) ->
    % 检查是否已购买
    case channel_subscribe_ds:has_purchased(ChannelId, Uid) of
        true ->
            % 已购买，直接创建订阅
            case channel_ds:subscribe(ChannelId, Uid) of
                ok ->
                    % 发送订阅通知
                    notify_channel_subscribed(ChannelId, Uid),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, <<"付费频道需要先购买"/utf8>>}
    end.

%% @doc 取消订阅频道
-spec unsubscribe(integer(), binary()) -> ok | {error, binary()}.
unsubscribe(Uid, ChannelIdBin) ->
    ChannelId = elib_hashids:decode(ChannelIdBin),
    case channel_ds:unsubscribe(ChannelId, Uid) of
        ok ->
            % 发送取消订阅通知
            notify_channel_unsubscribed(ChannelId, Uid),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 获取用户订阅的频道列表
-spec get_subscribed_channels(integer()) -> {ok, list(map())}.
get_subscribed_channels(Uid) ->
    case channel_repo:list_subscribed(Uid, <<"*">>) of
        {ok, Channels} ->
            {ok, [channel_transfer(C) || C <- Channels]};
        _ ->
            {ok, []}
    end.

%% @doc 获取用户管理的频道列表
-spec get_managed_channels(integer()) -> {ok, list(map())}.
get_managed_channels(Uid) ->
    case channel_repo:list_managed(Uid) of
        {ok, Channels} ->
            {ok, [channel_transfer(C) || C <- Channels]};
        _ ->
            {ok, []}
    end.

%% @doc 发布频道消息
-spec publish_message(integer(), binary(), binary(), binary(), map()) ->
    {ok, map()} | {error, binary()}.
publish_message(Uid, ChannelIdBin, Content, MsgType, Payload) ->
    ChannelId = elib_hashids:decode(ChannelIdBin),
    % 验证权限：管理员才能发布消息
    Role = channel_admin_repo:get_role(ChannelId, Uid),
    case Role < 1 of
        true ->
            {error, <<"只有管理员可以发布消息"/utf8>>};
        false ->
            case channel_ds:publish_message(ChannelId, Uid, Content, MsgType, Payload) of
                {ok, MessageId} ->
                    % 获取完整的消息信息
                    Message = channel_message_repo:find_by_id(MessageId),
                    Message2 = message_transfer(Message),
                    % 发送消息给所有订阅者
                    broadcast_channel_message(ChannelId, Message2),
                    {ok, Message2};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 获取频道消息列表
-spec get_messages(binary(), integer(), integer()) -> {ok, list(map())}.
get_messages(ChannelIdBin, Cursor, Limit) ->
    ChannelId = elib_hashids:decode(ChannelIdBin),
    case channel_message_repo:list_by_channel(ChannelId, Cursor, Limit) of
        {ok, Messages} ->
            {ok, [message_transfer(M) || M <- Messages]};
        _ ->
            {ok, []}
    end.

%% @doc 标记消息已读
-spec mark_as_read(integer(), binary(), binary()) -> ok.
mark_as_read(_Uid, ChannelIdBin, _MessageIdBin) ->
    ChannelId = elib_hashids:decode(ChannelIdBin),
    _ = channel_subscription_repo:clear_unread(ChannelId),
    ok.

%% @doc 搜索频道
-spec search_channels(binary(), integer()) -> {ok, list(map())}.
search_channels(Keyword, Limit) ->
    case channel_repo:search(Keyword, Limit, <<"*">>) of
        {ok, Channels} ->
            {ok, [channel_transfer(C) || C <- Channels]};
        _ ->
            {ok, []}
    end.

%% @doc 获取发现频道列表（推荐频道）
%% 返回公开的、活跃的频道，按订阅数和创建时间排序
-spec get_discover_channels(integer()) -> {ok, list(map())}.
get_discover_channels(Limit) ->
    case channel_repo:list_discover(Limit, <<"*">>) of
        {ok, Channels} ->
            {ok, [channel_transfer(C) || C <- Channels]};
        _ ->
            {ok, []}
    end.

%% @doc 添加频道管理员
-spec add_admin(integer(), binary(), integer(), integer()) -> ok | {error, binary()}.
add_admin(Uid, ChannelIdBin, NewAdminUid, Role) ->
    ChannelId = elib_hashids:decode(ChannelIdBin),
    % 验证权限：只有创建者可以添加管理员
    CurrentRole = channel_admin_repo:get_role(ChannelId, Uid),
    case CurrentRole == 3 of
        false ->
            {error, <<"只有创建者可以添加管理员"/utf8>>};
        true ->
            Now = elib_dt:now(),
            Data = #{
                channel_id => ChannelId,
                user_id => NewAdminUid,
                role => Role,
                created_at => Now
            },
            case channel_admin_repo:add(Data) of
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end
    end.

%% @doc 移除频道管理员
-spec remove_admin(integer(), binary(), integer()) -> ok | {error, binary()}.
remove_admin(Uid, ChannelIdBin, AdminUid) ->
    ChannelId = elib_hashids:decode(ChannelIdBin),
    % 验证权限：只有创建者可以移除管理员
    CurrentRole = channel_admin_repo:get_role(ChannelId, Uid),
    case CurrentRole == 3 of
        false ->
            {error, <<"只有创建者可以移除管理员"/utf8>>};
        true ->
            case channel_admin_repo:delete(ChannelId, AdminUid) of
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end
    end.

%% ===================================================================
%% 统计相关 API
%% ===================================================================

%% @doc 获取频道统计数据
-spec get_channel_stats(binary()) -> {ok, map()} | {error, binary()}.
get_channel_stats(ChannelIdBin) ->
    ChannelId = elib_hashids:decode(ChannelIdBin),
    % 检查频道是否存在
    case channel_repo:find_by_id(ChannelId, <<"id,name,subscriber_count">>) of
        {error, _} ->
            {error, <<"频道不存在"/utf8>>};
        Channel ->
            % 获取消息统计
            {ok, Messages} = channel_message_repo:list_by_channel(ChannelId, 0, 1000),
            TotalViews = lists:foldl(fun(M, Acc) ->
                Acc + maps:get(<<"view_count">>, M, 0)
            end, 0, Messages),
            TotalMessages = length(Messages),
            % 获取反应统计
            {ok, Reactions} = channel_repo:get_reaction_count(ChannelId),
            Stats = #{
                <<"channel_id">> => ChannelIdBin,
                <<"subscriber_count">> => maps:get(<<"subscriber_count">>, Channel, 0),
                <<"total_messages">> => TotalMessages,
                <<"total_views">> => TotalViews,
                <<"total_reactions">> => Reactions
            },
            {ok, Stats}
    end.

%% @doc 记录消息阅读
-spec record_message_view(integer(), binary(), binary()) -> ok | {error, binary()}.
record_message_view(Uid, ChannelIdBin, MessageIdBin) ->
    ChannelId = elib_hashids:decode(ChannelIdBin),
    MessageId = elib_hashids:decode(MessageIdBin),
    % 检查是否已阅读
    case channel_repo:has_viewed_message(MessageId, Uid) of
        true ->
            ok;  % 已阅读，直接返回成功
        false ->
            % 插入阅读记录（触发器会自动更新 view_count）
            Now = elib_dt:now(),
            case channel_repo:insert_message_view(ChannelId, MessageId, Uid, Now) of
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end
    end.

%% @doc 添加消息反应
-spec add_reaction(integer(), binary(), binary(), binary()) -> ok | {error, binary()}.
add_reaction(Uid, ChannelIdBin, MessageIdBin, ReactionType) ->
    ChannelId = elib_hashids:decode(ChannelIdBin),
    MessageId = elib_hashids:decode(MessageIdBin),
    Now = elib_dt:now(),
    case channel_repo:insert_reaction(ChannelId, MessageId, Uid, ReactionType, Now) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 移除消息反应
-spec remove_reaction(integer(), binary(), binary(), binary()) -> ok | {error, binary()}.
remove_reaction(Uid, ChannelIdBin, MessageIdBin, ReactionType) ->
    ChannelId = elib_hashids:decode(ChannelIdBin),
    MessageId = elib_hashids:decode(MessageIdBin),
    case channel_repo:delete_reaction(ChannelId, MessageId, Uid, ReactionType) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 获取频道每日统计数据
-spec get_daily_stats(binary(), integer()) -> {ok, list(map())} | {error, binary()}.
get_daily_stats(ChannelIdBin, Days) ->
    ChannelId = elib_hashids:decode(ChannelIdBin),
    case channel_repo:get_daily_stats(ChannelId, Days) of
        {ok, Stats} ->
            % 转换 ID 格式
            Stats2 = [elib_hashids:replace_id(S, <<"channel_id">>) || S <- Stats],
            {ok, Stats2};
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 通知订阅者频道更新
notify_channel_update(ChannelId, Channel) ->
    SubscriberUids = channel_ds:subscriber_uids(ChannelId),
    ChannelIdBin = elib_hashids:encode(ChannelId),
    Action = <<"channel_updated">>,
    Payload = #{<<"channel_id">> => ChannelIdBin, <<"channel">> => Channel},
    msg_s2c_ds:send(0, SubscriberUids, Action, <<>>, null, Payload, no_save).

%% @doc 通知订阅者频道删除
notify_channel_deleted(ChannelId, SubscriberUids) ->
    ChannelIdBin = elib_hashids:encode(ChannelId),
    Action = <<"channel_deleted">>,
    Payload = #{<<"channel_id">> => ChannelIdBin},
    msg_s2c_ds:send(0, SubscriberUids, Action, <<>>, null, Payload, save).

%% @doc 通知频道被订阅
notify_channel_subscribed(ChannelId, Uid) ->
    ChannelIdBin = elib_hashids:encode(ChannelId),
    Action = <<"channel_subscribed">>,
    Payload = #{<<"channel_id">> => ChannelIdBin},
    msg_s2c_ds:send(0, [Uid], Action, <<>>, null, Payload, no_save).

%% @doc 通知取消订阅
notify_channel_unsubscribed(ChannelId, Uid) ->
    ChannelIdBin = elib_hashids:encode(ChannelId),
    Action = <<"channel_unsubscribed">>,
    Payload = #{<<"channel_id">> => ChannelIdBin},
    msg_s2c_ds:send(0, [Uid], Action, <<>>, null, Payload, no_save).

%% @doc 广播频道消息给所有订阅者
broadcast_channel_message(ChannelId, Message) ->
    SubscriberUids = channel_ds:subscriber_uids(ChannelId),
    ChannelIdBin = elib_hashids:encode(ChannelId),
    Action = <<"channel_message">>,
    Payload = Message#{
        <<"channel_id">> => ChannelIdBin,
        <<"type">> => <<"CHANNEL">>
    },
    msg_s2c_ds:send(0, SubscriberUids, Action, <<>>, null, Payload, save).

%% ===================================================================
%% 消息管理 API
%% ===================================================================

%% @doc 置顶/取消置顶消息
-spec pin_message(integer(), binary(), boolean()) -> {ok, map()} | {error, binary()}.
pin_message(Uid, MessageIdBin, IsPinned) ->
    MessageId = elib_hashids:decode(MessageIdBin),
    % 获取消息信息
    case channel_message_repo:find_by_id(MessageId) of
        {error, _} ->
            {error, <<"消息不存在"/utf8>>};
        Message ->
            ChannelId = maps:get(<<"channel_id">>, Message),
            % 验证权限：管理员才能置顶
            Role = channel_admin_repo:get_role(ChannelId, Uid),
            case Role < 2 of
                true ->
                    {error, <<"只有管理员可以置顶消息"/utf8>>};
                false ->
                    Now = elib_dt:now(),
                    case channel_message_repo:update(MessageId, #{
                        is_pinned => IsPinned,
                        updated_at => Now
                    }) of
                        {ok, _} ->
                            % 获取更新后的消息
                            Message2 = channel_message_repo:find_by_id(MessageId),
                            {ok, message_transfer(Message2)};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

%% @doc 删除消息
-spec delete_message(integer(), binary()) -> ok | {error, binary()}.
delete_message(Uid, MessageIdBin) ->
    MessageId = elib_hashids:decode(MessageIdBin),
    % 获取消息信息
    case channel_message_repo:find_by_id(MessageId) of
        {error, _} ->
            {error, <<"消息不存在"/utf8>>};
        Message ->
            ChannelId = maps:get(<<"channel_id">>, Message),
            AuthorId = maps:get(<<"author_id">>, Message),
            % 验证权限：管理员或消息作者可以删除
            Role = channel_admin_repo:get_role(ChannelId, Uid),
            case Role >= 2 orelse AuthorId == Uid of
                false ->
                    {error, <<"无权限删除此消息"/utf8>>};
                true ->
                    case channel_message_repo:delete(MessageId) of
                        {ok, _} ->
                            % 通知订阅者消息被删除
                            notify_message_deleted(ChannelId, MessageId),
                            ok;
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

%% ===================================================================
%% 订阅者管理 API
%% ===================================================================

%% @doc 获取频道订阅者列表
-spec get_subscribers(binary(), integer(), integer()) -> {ok, list(map())}.
get_subscribers(ChannelIdBin, Cursor, Limit) ->
    ChannelId = elib_hashids:decode(ChannelIdBin),
    case channel_subscription_repo:list_by_channel(ChannelId, Cursor, Limit) of
        {ok, Subscribers} ->
            % 转换用户 ID
            Subscribers2 = lists:map(fun(S) ->
                elib_hashids:replace_id(S, <<"user_id">>)
            end, Subscribers),
            {ok, Subscribers2};
        _ ->
            {ok, []}
    end.

%% @doc 通知消息被删除
notify_message_deleted(ChannelId, MessageId) ->
    SubscriberUids = channel_ds:subscriber_uids(ChannelId),
    ChannelIdBin = elib_hashids:encode(ChannelId),
    MessageIdBin = elib_hashids:encode(MessageId),
    Action = <<"channel_message_deleted">>,
    Payload = #{
        <<"channel_id">> => ChannelIdBin,
        <<"message_id">> => MessageIdBin
    },
    msg_s2c_ds:send(0, SubscriberUids, Action, <<>>, null, Payload, save).

%% ===================================================================
%% 频道邀请 API（私有频道）
%% ===================================================================

%% @doc 创建邀请
%% @param Uid 邀请人ID
%% @param ChannelIdBin 频道ID（HashID编码）
%% @param InviteeUid 被邀请人ID
%% @returns {ok, Invitation} | {error, Reason}
-spec create_invitation(integer(), binary(), integer()) -> {ok, map()} | {error, binary()}.
create_invitation(Uid, ChannelIdBin, InviteeUid) ->
    ChannelId = elib_hashids:decode(ChannelIdBin),
    % 检查频道类型（必须是私有频道）
    case channel_repo:find_by_id(ChannelId, <<"id,type,status">>) of
        {error, _} ->
            {error, <<"频道不存在"/utf8>>};
        Channel ->
            Type = maps:get(<<"type">>, Channel, 0),
            Status = maps:get(<<"status">>, Channel, 0),
            if
                Status =/= 1 ->
                    {error, <<"频道已禁用或删除"/utf8>>};
                Type =/= 1 ->
                    {error, <<"只有私有频道支持邀请功能"/utf8>>};
                true ->
                    case channel_subscribe_ds:create_invitation(ChannelId, Uid, InviteeUid) of
                        {ok, InvitationId} ->
                            % 获取完整的邀请信息
                            case channel_invitation_repo:find_by_id(InvitationId) of
                                {ok, Invitation} ->
                                    % 转换ID并通知被邀请人
                                    Invitation2 = invitation_transfer(Invitation),
                                    notify_invitation_created(ChannelId, Uid, InviteeUid),
                                    {ok, Invitation2};
                                {error, Reason} ->
                                    {error, elib:convert(Reason, binary)}
                            end;
                        {error, Reason} when is_binary(Reason) ->
                            {error, Reason};
                        {error, Reason} ->
                            {error, elib:convert(Reason, binary)}
                    end
            end
    end.

%% @doc 接受邀请
-spec accept_invitation(integer(), integer()) -> ok | {error, binary()}.
accept_invitation(Uid, InvitationId) ->
    case channel_subscribe_ds:accept_invitation(InvitationId, Uid) of
        ok ->
            % 获取邀请信息并发送通知
            case channel_invitation_repo:find_by_id(InvitationId) of
                {ok, Invitation} ->
                    ChannelId = maps:get(<<"channel_id">>, Invitation),
                    InviterUid = maps:get(<<"inviter_uid">>, Invitation),
                    notify_invitation_accepted(ChannelId, InviterUid, Uid);
                _ ->
                    ok
            end;
        {error, Reason} when is_binary(Reason) ->
            {error, Reason};
        {error, Reason} ->
            {error, elib:convert(Reason, binary)}
    end.

%% @doc 拒绝邀请
-spec reject_invitation(integer(), integer()) -> ok | {error, binary()}.
reject_invitation(Uid, InvitationId) ->
    case channel_subscribe_ds:reject_invitation(InvitationId, Uid) of
        ok -> ok;
        {error, Reason} when is_binary(Reason) ->
            {error, Reason};
        {error, Reason} ->
            {error, elib:convert(Reason, binary)}
    end.

%% @doc 获取我的邀请列表
-spec get_my_invitations(integer()) -> {ok, [map()]}.
get_my_invitations(Uid) ->
    case channel_invitation_repo:list_pending_by_invitee(Uid) of
        {ok, Invitations} ->
            Invitations2 = lists:map(fun invitation_transfer/1, Invitations),
            {ok, Invitations2};
        _ ->
            {ok, []}
    end.

%% @doc 获取我发出的邀请列表
-spec get_sent_invitations(integer()) -> {ok, [map()]}.
get_sent_invitations(Uid) ->
    case channel_invitation_repo:list_by_inviter(Uid, 50) of
        {ok, Invitations} ->
            Invitations2 = lists:map(fun invitation_transfer/1, Invitations),
            {ok, Invitations2};
        _ ->
            {ok, []}
    end.

%% @doc 转换邀请数据
-spec invitation_transfer(map()) -> map().
invitation_transfer(Invitation) ->
    Invitation2 = elib_hashids:replace_id(Invitation),
    Invitation3 = elib_hashids:replace_id(Invitation2, <<"channel_id">>),
    Invitation4 = elib_hashids:replace_id(Invitation3, <<"inviter_uid">>),
    Invitation5 = elib_hashids:replace_id(Invitation4, <<"invitee_uid">>),
    Invitation5.

%% ===================================================================
%% 频道订单 API（付费频道）
%% ===================================================================

%% @doc 创建订单
%% @param Uid 用户ID
%% @param ChannelIdBin 频道ID（HashID编码）
%% @returns {ok, Order} | {error, Reason}
-spec create_order(integer(), binary()) -> {ok, map()} | {error, binary()}.
create_order(Uid, ChannelIdBin) ->
    ChannelId = elib_hashids:decode(ChannelIdBin),
    % 检查频道类型（必须是付费频道）
    case channel_repo:find_by_id(ChannelId, <<"id,type,status">>) of
        {error, _} ->
            {error, <<"频道不存在"/utf8>>};
        Channel ->
            Type = maps:get(<<"type">>, Channel, 0),
            Status = maps:get(<<"status">>, Channel, 0),
            if
                Status =/= 1 ->
                    {error, <<"频道已禁用或删除"/utf8>>};
                Type =/= 2 ->
                    {error, <<"只有付费频道支持购买"/utf8>>};
                true ->
                    case channel_subscribe_ds:create_order(ChannelId, Uid, #{}) of
                        {ok, OrderNo} ->
                            % 获取完整的订单信息
                            case channel_order_repo:find_by_order_no(OrderNo) of
                                {ok, Order} ->
                                    Order2 = order_transfer(Order),
                                    {ok, Order2};
                                {error, Reason} ->
                                    {error, elib:convert(Reason, binary)}
                            end;
                        {error, Reason} when is_binary(Reason) ->
                            {error, Reason};
                        {error, Reason} ->
                            {error, elib:convert(Reason, binary)}
                    end
            end
    end.

%% @doc 支付订单（模拟支付）
-spec pay_order(integer(), binary()) -> ok | {error, binary()}.
pay_order(Uid, OrderNo) ->
    % 验证订单归属
    case channel_order_repo:find_by_order_no(OrderNo) of
        {ok, Order} ->
            OrderUserId = maps:get(<<"user_id">>, Order),
            if
                OrderUserId =/= Uid ->
                    {error, <<"无权操作此订单"/utf8>>};
                true ->
                    PaymentData = #{
                        payment_no => generate_payment_no(),
                        payment_method => <<"mock">>
                    },
                    case channel_subscribe_ds:pay_order(OrderNo, PaymentData) of
                        ok ->
                            % 获取频道信息并发送通知
                            ChannelId = maps:get(<<"channel_id">>, Order),
                            notify_order_paid(ChannelId, Uid);
                        {error, Reason} when is_binary(Reason) ->
                            {error, Reason};
                        {error, Reason} ->
                            {error, elib:convert(Reason, binary)}
                    end
            end;
        {error, _} ->
            {error, <<"订单不存在"/utf8>>}
    end.

%% @doc 获取订单列表
-spec get_my_orders(integer()) -> {ok, [map()]}.
get_my_orders(Uid) ->
    case channel_order_repo:list_by_user(Uid, 50) of
        {ok, Orders} ->
            Orders2 = lists:map(fun order_transfer/1, Orders),
            {ok, Orders2};
        _ ->
            {ok, []}
    end.

%% @doc 获取订单详情
-spec get_order(integer(), binary()) -> {ok, map()} | {error, binary()}.
get_order(Uid, OrderNo) ->
    case channel_order_repo:find_by_order_no(OrderNo) of
        {ok, Order} ->
            OrderUserId = maps:get(<<"user_id">>, Order),
            if
                OrderUserId =/= Uid ->
                    {error, <<"无权查看此订单"/utf8>>};
                true ->
                    {ok, order_transfer(Order)}
            end;
        {error, _} ->
            {error, <<"订单不存在"/utf8>>}
    end.

%% @doc 转换订单数据
-spec order_transfer(map()) -> map().
order_transfer(Order) ->
    Order2 = elib_hashids:replace_id(Order),
    Order3 = elib_hashids:replace_id(Order2, <<"channel_id">>),
    Order3.

%% @doc 生成支付流水号
generate_payment_no() ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(1000000) - 1,
    iolist_to_binary(["PAY", integer_to_binary(Timestamp), integer_to_binary(Random)]).

%% ===================================================================
%% 通知函数
%% ===================================================================

%% @doc 通知邀请已创建
notify_invitation_created(ChannelId, _InviterUid, InviteeUid) ->
    ChannelIdBin = elib_hashids:encode(ChannelId),
    Action = <<"channel_invitation_created">>,
    Payload = #{<<"channel_id">> => ChannelIdBin},
    msg_s2c_ds:send(0, [InviteeUid], Action, <<>>, null, Payload, save).

%% @doc 通知邀请已接受
notify_invitation_accepted(ChannelId, InviterUid, InviteeUid) ->
    ChannelIdBin = elib_hashids:encode(ChannelId),
    InviteeUidBin = elib_hashids:encode(InviteeUid),
    % 通知邀请人
    Action1 = <<"channel_invitation_accepted">>,
    Payload1 = #{<<"channel_id">> => ChannelIdBin, <<"invitee_uid">> => InviteeUidBin},
    msg_s2c_ds:send(0, [InviterUid], Action1, <<>>, null, Payload1, no_save),
    % 通知被邀请人订阅成功
    Action2 = <<"channel_subscribed">>,
    Payload2 = #{<<"channel_id">> => ChannelIdBin},
    msg_s2c_ds:send(0, [InviteeUid], Action2, <<>>, null, Payload2, no_save).

%% @doc 通知订单已支付
notify_order_paid(ChannelId, Uid) ->
    ChannelIdBin = elib_hashids:encode(ChannelId),
    Action = <<"channel_order_paid">>,
    Payload = #{<<"channel_id">> => ChannelIdBin},
    msg_s2c_ds:send(0, [Uid], Action, <<>>, null, Payload, no_save),
    % 同时发送订阅成功通知
    Action2 = <<"channel_subscribed">>,
    msg_s2c_ds:send(0, [Uid], Action2, <<>>, null, Payload, no_save).
