-module(channel_logic_subscription).
%% Internal channel_content subscription/application logic.

-export([subscribe/2]).
-export([unsubscribe/2]).
-export([get_subscribed_channels/1]).
-export([get_managed_channels/1]).
-export([get_unread_summary/1]).
-export([get_subscribers/3]).
-export([remove_subscriber/3]).

-define(CHANNEL_CACHE_KEY(ChannelId), {channel, ChannelId}).
-define(CHANNEL_SUBS_KEY(ChannelId), {channel_subs, ChannelId}).

-spec subscribe(integer(), binary()) -> ok | {error, binary()}.
subscribe(Uid, ChannelIdBin) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case channel_repo:find_by_id(ChannelId, <<"id,type">>) of
                {error, _} ->
                    {error, <<"频道不存在"/utf8>>};
                Channel when is_map(Channel) ->
                    % 直接走订阅流程，底层 upsert_active 保证幂等，
                    % 避免先 is_subscribed 再 subscribe 的 TOCTOU 竞态
                    Type = maps:get(<<"type">>, Channel, 0),
                    case Type of
                        1 ->
                            subscribe_private_channel(Uid, ChannelId);
                        2 ->
                            subscribe_paid_channel(Uid, ChannelId);
                        _ ->
                            case channel_ds:subscribe(ChannelId, Uid) of
                                ok ->
                                    channel_logic_notify:notify_channel_subscribed(ChannelId, Uid),
                                    ok;
                                {error, Reason} ->
                                    {error, elib_cnv:safe_to_binary(Reason)}
                            end
                    end;
                _ ->
                    {error, <<"频道不存在"/utf8>>}
            end
    end.

-spec subscribe_private_channel(integer(), integer()) -> ok | {error, binary()}.
subscribe_private_channel(Uid, ChannelId) ->
    case channel_subscribe_ds:is_invited(ChannelId, Uid) of
        true ->
            case channel_subscribe_ds:subscribe_private(ChannelId, Uid, undefined) of
                ok ->
                    channel_logic_notify:notify_channel_subscribed(ChannelId, Uid),
                    ok;
                {error, Reason} when is_binary(Reason) ->
                    {error, Reason};
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end;
        _ ->
            {error, <<"私有频道需要邀请才能订阅"/utf8>>}
    end.

-spec subscribe_paid_channel(integer(), integer()) -> ok | {error, binary()}.
subscribe_paid_channel(Uid, ChannelId) ->
    case channel_subscribe_ds:has_purchased(ChannelId, Uid) of
        true ->
            case channel_ds:subscribe(ChannelId, Uid) of
                ok ->
                    channel_logic_notify:notify_channel_subscribed(ChannelId, Uid),
                    ok;
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end;
        _ ->
            {error, <<"付费频道需要先购买"/utf8>>}
    end.

-spec unsubscribe(integer(), binary()) -> ok | {error, binary()}.
unsubscribe(Uid, ChannelIdBin) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case channel_ds:unsubscribe(ChannelId, Uid) of
                ok ->
                    channel_logic_notify:notify_channel_unsubscribed(ChannelId, Uid),
                    ok;
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)};
                Other ->
                    {error, elib_cnv:safe_to_binary(Other)}
            end
    end.

-spec get_subscribed_channels(integer()) -> {ok, list(map())} | {error, binary()}.
get_subscribed_channels(Uid) ->
    case channel_repo:list_subscribed(Uid, <<"*">>) of
        {ok, Channels} when is_list(Channels) ->
            {ok, [channel_logic_common:channel_transfer(C) || C <- Channels, is_map(C)]};
        {ok, Other} ->
            {error, elib_cnv:safe_to_binary(Other)};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        Reason ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

-spec get_managed_channels(integer()) -> {ok, list(map())} | {error, binary()}.
get_managed_channels(Uid) ->
    case channel_repo:list_managed(Uid) of
        {ok, Channels} when is_list(Channels) ->
            {ok, [channel_logic_common:channel_transfer(C) || C <- Channels, is_map(C)]};
        {ok, Other} ->
            {error, elib_cnv:safe_to_binary(Other)};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        Reason ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

-spec get_unread_summary(integer()) -> {ok, map()} | {error, binary()}.
get_unread_summary(Uid) ->
    TotalUnread = channel_subscription_repo:count_unread(Uid),
    UnreadChannels = channel_subscription_repo:count_unread_channels(Uid),
    case channel_subscription_repo:list_unread_by_uid(Uid) of
        {ok, Rows} when is_list(Rows) ->
            ChannelUnreadList = [
                #{
                    <<"channel_id">> => ChannelId,
                    <<"unread_count">> => UnreadCount
                }
                || #{
                    <<"channel_id">> := ChannelId,
                    <<"unread_count">> := UnreadCount
                } <- Rows,
                is_integer(ChannelId),
                ChannelId > 0,
                is_integer(UnreadCount),
                UnreadCount > 0
            ],
            {ok, #{
                <<"total_unread">> => TotalUnread,
                <<"unread_channels">> => UnreadChannels,
                <<"channels">> => ChannelUnreadList
            }};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        Other ->
            {error, elib_cnv:safe_to_binary(Other)}
    end.

-spec get_subscribers(binary(), integer(), integer()) -> {ok, list(map())} | {error, binary()}.
get_subscribers(ChannelIdBin, Cursor, Limit) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case channel_subscription_repo:list_by_channel(ChannelId, Cursor, Limit) of
                {ok, Subscribers} when is_list(Subscribers) ->
                    {ok, [S || S <- Subscribers, is_map(S)]};
                {ok, Other} ->
                    {error, elib_cnv:safe_to_binary(Other)};
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)};
                Reason ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end
    end.

-spec remove_subscriber(integer(), integer() | binary(), integer()) -> ok | {error, binary()}.
remove_subscriber(Uid, ChannelId, TargetUid) when is_binary(ChannelId) ->
    case decode_positive_id(ChannelId) of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        DecodedChannelId ->
            remove_subscriber(Uid, DecodedChannelId, TargetUid)
    end;
remove_subscriber(Uid, ChannelId, TargetUid) ->
    StartMs = elib_dt:millisecond(),
    case channel_logic_common:get_user_role(ChannelId, Uid) of
        Role when Role >= 2 ->
            Result = elib_pg:with_tx(fun(Conn) ->
                case channel_subscription_repo:delete(Conn, ChannelId, TargetUid) of
                    {ok, Affected} when Affected > 0 ->
                        case channel_repo:increment_subscribers(Conn, ChannelId, -1) of
                            {ok, _} ->
                                changed;
                            {error, Reason} ->
                                throw({abort_tx, Reason})
                        end;
                    {ok, 0} ->
                        noop;
                    {error, Reason} ->
                        throw({abort_tx, Reason})
                end
            end),
            case Result of
                changed ->
                    flush_remove_subscriber_cache(ChannelId),
                    channel_logic_common:log_channel_action(
                        Uid,
                        ChannelId,
                        undefined,
                        <<"remove_subscriber">>,
                        <<"changed">>,
                        StartMs
                    ),
                    ok;
                noop ->
                    flush_remove_subscriber_cache(ChannelId),
                    channel_logic_common:log_channel_action(
                        Uid,
                        ChannelId,
                        undefined,
                        <<"remove_subscriber">>,
                        <<"noop">>,
                        StartMs
                    ),
                    ok;
                {error, _} ->
                    channel_logic_common:log_channel_action(
                        Uid,
                        ChannelId,
                        undefined,
                        <<"remove_subscriber">>,
                        <<"error">>,
                        StartMs
                    ),
                    {error, <<"移除订阅者失败"/utf8>>}
            end;
        _ ->
            channel_logic_common:log_channel_action(
                Uid,
                ChannelId,
                undefined,
                <<"remove_subscriber">>,
                <<"denied">>,
                StartMs
            ),
            {error, <<"无权限操作，需要管理员及以上权限"/utf8>>}
    end.

-spec decode_positive_id(term()) -> integer().
decode_positive_id(Value) ->
    case catch ec_cnv:to_integer(Value) of
        Id when is_integer(Id), Id > 0 ->
            Id;
        _ ->
            0
    end.

-spec flush_remove_subscriber_cache(integer()) -> ok.
flush_remove_subscriber_cache(ChannelId) ->
    imboy_cache:flush(?CHANNEL_SUBS_KEY(ChannelId)),
    imboy_cache:flush(?CHANNEL_CACHE_KEY(ChannelId)),
    ok.
