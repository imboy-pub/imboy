-module(channel_logic_message).
%% Internal channel_content message/application logic.

-export([channel_transfer/1]).
-export([message_transfer/1]).
-export([create_channel/5]).
-export([get_channel/2]).
-export([get_channel_by_custom_id/2]).
-export([update_channel/3]).
-export([delete_channel/2]).
-export([publish_message/5]).
-export([get_messages/4]).
-export([mark_as_read/3]).
-export([search_channels/2]).
-export([get_discover_channels/1]).
-export([add_admin/4]).
-export([remove_admin/3]).
-export([pin_message/3]).
-export([delete_message/2]).
-export([revoke_message/3]).
-export([get_admins/1]).
-export([update_admin_role/4]).

-spec channel_transfer(map()) -> map().
channel_transfer(Channel) when is_map(Channel) ->
    channel_logic_common:channel_transfer(Channel).

-spec message_transfer(map()) -> map().
message_transfer(Message) when is_map(Message) ->
    Message2 = elib_hashids:replace_id(Message),
    Message3 = elib_hashids:replace_id(Message2, <<"channel_id">>),
    Message4 = elib_hashids:replace_id(Message3, <<"author_id">>),
    Message4.

-spec create_channel(integer(), binary(), integer(), map(), integer()) ->
    {ok, map()} | {error, binary()}.
create_channel(Uid, Name, Type, Opts, MaxChannels) ->
    case channel_repo:list_managed(Uid) of
        {ok, Channels} when is_list(Channels) ->
            case length(Channels) >= MaxChannels of
                true ->
                    {error, <<"已达频道创建上限"/utf8>>};
                false ->
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
            end;
        {ok, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        Reason ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

do_create_channel(Uid, Name, Type, Opts) ->
    case channel_ds:create_channel(Uid, Name, Type, Opts) of
        {ok, ChannelId} ->
            case channel_repo:find_by_id(ChannelId, <<"*">>) of
                {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)};
                Channel when is_map(Channel) -> {ok, channel_transfer(Channel)};
                Other -> {error, elib_cnv:safe_to_binary(Other)}
            end;
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

-spec get_channel(binary(), integer()) -> {ok, map()} | {error, binary()}.
get_channel(ChannelIdBin, Uid) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case channel_repo:find_by_id(ChannelId, <<"*">>) of
                {error, _} -> {error, <<"频道不存在"/utf8>>};
                Channel when is_map(Channel), map_size(Channel) =:= 0 -> {error, <<"频道不存在"/utf8>>};
                Channel when is_map(Channel) ->
                    UserRole = channel_logic_common:get_user_role(ChannelId, Uid),
                    IsSubscribed = case UserRole of
                        0 -> channel_subscription_repo:is_subscribed(ChannelId, Uid);
                        _ -> true
                    end,
                    Channel2 = Channel#{
                        user_role => UserRole,
                        is_subscribed => IsSubscribed
                    },
                    {ok, channel_transfer(Channel2)};
                _ ->
                    {error, <<"频道不存在"/utf8>>}
            end
    end.

-spec get_channel_by_custom_id(binary(), integer()) -> {ok, map()} | {error, binary()}.
get_channel_by_custom_id(CustomId, Uid) ->
    case channel_repo:find_by_custom_id(CustomId) of
        {error, _} -> {error, <<"频道不存在"/utf8>>};
        Channel when is_map(Channel), map_size(Channel) =:= 0 ->
            {error, <<"频道不存在"/utf8>>};
        Channel when is_map(Channel) ->
            ChannelId = maps:get(<<"id">>, Channel, 0),
            case is_integer(ChannelId) andalso ChannelId > 0 of
                false ->
                    {error, <<"频道不存在"/utf8>>};
                true ->
                    UserRole = channel_logic_common:get_user_role(ChannelId, Uid),
                    IsSubscribed = case UserRole of
                        0 -> channel_subscription_repo:is_subscribed(ChannelId, Uid);
                        _ -> true
                    end,
                    Channel2 = Channel#{
                        user_role => UserRole,
                        is_subscribed => IsSubscribed
                    },
                    {ok, channel_transfer(Channel2)}
            end;
        _ ->
            {error, <<"频道不存在"/utf8>>}
    end.

-spec update_channel(integer(), binary(), map()) -> {ok, map()} | {error, binary()}.
update_channel(Uid, ChannelIdBin, Data) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            Role = channel_logic_common:get_user_role(ChannelId, Uid),
            case Role >= 2 of
                false ->
                    {error, <<"无权限操作"/utf8>>};
                true ->
                    AllowedFields = [<<"name">>, <<"description">>, <<"avatar">>, <<"tags">>],
                    FilteredData = maps:filter(fun(K, _) -> lists:member(K, AllowedFields) end, Data),
                    case channel_repo:update(ChannelId, FilteredData#{updated_at => elib_dt:now()}) of
                        {ok, _} ->
                            case channel_repo:find_by_id(ChannelId, <<"*">>) of
                                {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)};
                                Channel when is_map(Channel) ->
                                    channel_logic_notify:notify_channel_update(ChannelId, Channel),
                                    {ok, channel_transfer(Channel)};
                                Other ->
                                    {error, elib_cnv:safe_to_binary(Other)}
                            end;
                        {error, Reason} ->
                            {error, elib_cnv:safe_to_binary(Reason)}
                    end
            end
    end.

-spec delete_channel(integer(), binary()) -> ok | {error, binary()}.
delete_channel(Uid, ChannelIdBin) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            Role = channel_logic_common:get_user_role(ChannelId, Uid),
            case Role == 3 of
                false ->
                    {error, <<"只有创建者可以删除频道"/utf8>>};
                true ->
                    SubscriberUids = safe_subscriber_uids(ChannelId),
                        case channel_repo:delete(ChannelId) of
                            {ok, _} ->
                                channel_logic_notify:notify_channel_deleted(ChannelId, SubscriberUids),
                                ok;
                            {error, Reason} ->
                                {error, elib_cnv:safe_to_binary(Reason)}
                        end
            end
    end.

-spec publish_message(integer(), binary(), binary(), binary(), map()) ->
    {ok, map()} | {error, binary()}.
publish_message(Uid, ChannelIdBin, Content, MsgType, Payload) ->
    ChannelId = channel_logic_common:resolve_channel_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            Role = channel_logic_common:get_user_role(ChannelId, Uid),
            case Role < 1 of
                true ->
                    {error, <<"只有管理员可以发布消息"/utf8>>};
                false ->
                    case channel_ds:publish_message(ChannelId, Uid, Content, MsgType, Payload) of
                        {ok, MessageId} ->
                            case channel_message_repo:find_by_id(MessageId) of
                                {error, Reason} ->
                                    {error, elib_cnv:safe_to_binary(Reason)};
                                Message when is_map(Message) ->
                                    Message2 = message_transfer(Message),
                                    channel_logic_notify:broadcast_channel_message(ChannelId, Message2),
                                    push_unread_updates(ChannelId),
                                    {ok, Message2};
                                Other ->
                                    {error, elib_cnv:safe_to_binary(Other)}
                            end;
                        {error, Reason} ->
                            {error, elib_cnv:safe_to_binary(Reason)}
                    end
            end
    end.

-spec get_messages(integer(), binary(), integer(), integer()) -> {ok, list(map())} | {error, binary()}.
get_messages(Uid, ChannelIdBin, Cursor, Limit) ->
    ChannelId = channel_logic_common:resolve_channel_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case channel_logic_common:ensure_channel_content_access(Uid, ChannelId) of
                ok ->
                    case channel_message_repo:list_by_channel(ChannelId, Cursor, Limit) of
                        {ok, Messages} when is_list(Messages) ->
                            {ok, [message_transfer(M) || M <- Messages, is_map(M)]};
                        {ok, Reason} ->
                            {error, elib_cnv:safe_to_binary(Reason)};
                        {error, Reason} ->
                            {error, elib_cnv:safe_to_binary(Reason)};
                        Reason ->
                            {error, elib_cnv:safe_to_binary(Reason)}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec mark_as_read(integer(), binary(), binary()) -> ok | {error, binary()}.
mark_as_read(Uid, ChannelIdBin, _MessageIdBin) ->
    ChannelId = channel_logic_common:resolve_channel_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case channel_logic_common:ensure_channel_content_access(Uid, ChannelId) of
                ok ->
                    _ = channel_subscription_repo:clear_unread(ChannelId, Uid),
                    channel_logic_notify:notify_channel_unread_count(ChannelId, Uid, 0),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec search_channels(binary(), integer()) -> {ok, list(map())} | {error, binary()}.
search_channels(Keyword, Limit) ->
    case channel_repo:search(Keyword, Limit, <<"*">>) of
        {ok, Channels} when is_list(Channels) ->
            {ok, [channel_transfer(C) || C <- Channels, is_map(C)]};
        {ok, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        Reason ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

-spec get_discover_channels(integer()) -> {ok, list(map())} | {error, binary()}.
get_discover_channels(Limit) ->
    case channel_repo:list_discover(Limit, <<"*">>) of
        {ok, Channels} when is_list(Channels) ->
            {ok, [channel_transfer(C) || C <- Channels, is_map(C)]};
        {ok, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        Reason ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

-spec add_admin(integer(), binary(), integer(), integer()) -> ok | {error, binary()}.
add_admin(Uid, ChannelIdBin, NewAdminUid, Role) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            CurrentRole = channel_logic_common:get_user_role(ChannelId, Uid),
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
                        {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)}
                    end
            end
    end.

-spec remove_admin(integer(), binary(), integer()) -> ok | {error, binary()}.
remove_admin(Uid, ChannelIdBin, AdminUid) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            CurrentRole = channel_logic_common:get_user_role(ChannelId, Uid),
            case CurrentRole == 3 of
                false ->
                    {error, <<"只有创建者可以移除管理员"/utf8>>};
                true ->
                    case channel_admin_repo:delete(ChannelId, AdminUid) of
                        {ok, _} -> ok;
                        {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)}
                    end
            end
    end.

-spec pin_message(integer(), binary(), boolean()) -> {ok, map()} | {error, binary()}.
pin_message(Uid, MessageIdBin, IsPinned) ->
    MessageId = decode_positive_id(MessageIdBin),
    case MessageId of
        0 ->
            {error, <<"消息不存在"/utf8>>};
        _ ->
            case channel_message_repo:find_by_id(MessageId) of
                {error, _} ->
                    {error, <<"消息不存在"/utf8>>};
                Message when is_map(Message) ->
                    ChannelId = maps:get(<<"channel_id">>, Message, 0),
                    case is_integer(ChannelId) andalso ChannelId > 0 of
                        true ->
                            Role = channel_logic_common:get_user_role(ChannelId, Uid),
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
                                            case channel_message_repo:find_by_id(MessageId) of
                                                {error, Reason} ->
                                                    {error, elib_cnv:safe_to_binary(Reason)};
                                                Message2 when is_map(Message2) ->
                                                    {ok, message_transfer(Message2)};
                                                Other ->
                                                    {error, elib_cnv:safe_to_binary(Other)}
                                            end;
                                        {error, Reason} ->
                                            {error, elib_cnv:safe_to_binary(Reason)}
                                    end
                            end;
                        false ->
                            {error, <<"消息不存在"/utf8>>}
                    end;
                _ ->
                    {error, <<"消息不存在"/utf8>>}
            end
    end.

-spec delete_message(integer(), binary()) -> ok | {error, binary()}.
delete_message(Uid, MessageIdBin) ->
    MessageId = decode_positive_id(MessageIdBin),
    case MessageId of
        0 ->
            {error, <<"消息不存在"/utf8>>};
        _ ->
            case channel_message_repo:find_by_id(MessageId) of
                {error, _} ->
                    {error, <<"消息不存在"/utf8>>};
                Message when is_map(Message) ->
                    ChannelId = maps:get(<<"channel_id">>, Message, 0),
                    AuthorId = maps:get(<<"author_id">>, Message, 0),
                    case is_integer(ChannelId)
                        andalso ChannelId > 0
                        andalso is_integer(AuthorId)
                        andalso AuthorId > 0 of
                        false ->
                            {error, <<"消息不存在"/utf8>>};
                        true ->
                            Role = channel_logic_common:get_user_role(ChannelId, Uid),
                            case Role >= 2 orelse AuthorId == Uid of
                                false ->
                                    {error, <<"无权限删除此消息"/utf8>>};
                                true ->
                                    case channel_message_repo:delete(MessageId) of
                                        {ok, _} ->
                                            channel_logic_notify:notify_message_deleted(ChannelId, MessageId),
                                            ok;
                                        {error, Reason} ->
                                            {error, elib_cnv:safe_to_binary(Reason)}
                                    end
                            end
                    end;
                _ ->
                    {error, <<"消息不存在"/utf8>>}
            end
    end.

-spec revoke_message(integer(), binary(), binary()) -> ok | {error, binary()}.
revoke_message(Uid, ChannelIdBin, MessageIdBin) ->
    StartMs = elib_dt:millisecond(),
    ChannelId = channel_logic_common:resolve_channel_id(ChannelIdBin),
    MessageId = decode_positive_id(MessageIdBin),
    Result = case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ when MessageId =:= 0 ->
            {error, <<"消息不存在"/utf8>>};
        _ ->
            case channel_message_repo:find_by_id(MessageId) of
                {error, _} ->
                    {error, <<"消息不存在"/utf8>>};
                Message when is_map(Message) ->
                    MessageChannelId = maps:get(<<"channel_id">>, Message, 0),
                    MessageAuthorId = maps:get(<<"author_id">>, Message, 0),
                    case is_integer(MessageChannelId)
                        andalso MessageChannelId > 0
                        andalso is_integer(MessageAuthorId)
                        andalso MessageAuthorId > 0 of
                        false ->
                            {error, <<"消息不存在"/utf8>>};
                        true ->
                            case MessageChannelId =:= ChannelId of
                                false ->
                                    {error, <<"消息不存在"/utf8>>};
                                true ->
                                    Role = channel_logic_common:get_user_role(ChannelId, Uid),
                                    case Role >= 2 orelse MessageAuthorId =:= Uid of
                                        false ->
                                            {error, <<"无权限撤回此消息"/utf8>>};
                                        true ->
                                            Revoked = maps:get(<<"revoked">>, Message, false),
                                            case Revoked =:= true of
                                                true ->
                                                    ok;
                                                false ->
                                                    case is_within_revoke_window(Message) of
                                                        false ->
                                                            {error, <<"撤回时间已超出限制"/utf8>>};
                                                        true ->
                                                            RevokedAt = elib_dt:now(),
                                                            case channel_message_repo:revoke(MessageId, Uid, RevokedAt) of
                                                                {ok, Affected} when Affected > 0 ->
                                                                    channel_logic_notify:notify_message_revoked(
                                                                        ChannelId,
                                                                        MessageId,
                                                                        Uid,
                                                                        RevokedAt
                                                                    ),
                                                                    ok;
                                                                {ok, 0} ->
                                                                    ok;
                                                                {error, Reason} ->
                                                                    {error, elib_cnv:safe_to_binary(Reason)}
                                                            end
                                                    end
                                            end
                                    end
                            end
                    end;
                _ ->
                    {error, <<"消息不存在"/utf8>>}
            end
    end,
    channel_logic_common:log_channel_action(Uid, ChannelId, MessageId, <<"revoke_message">>, Result, StartMs),
    Result.

-spec get_admins(integer() | binary()) -> {ok, list(map())} | {error, binary()}.
get_admins(ChannelId) when is_binary(ChannelId) ->
    DecodedChannelId = decode_positive_id(ChannelId),
    case DecodedChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            get_admins(DecodedChannelId)
    end;
get_admins(ChannelId) when not is_integer(ChannelId); ChannelId =< 0 ->
    {error, <<"频道不存在"/utf8>>};
get_admins(ChannelId) ->
    case channel_admin_repo:list_by_channel(ChannelId) of
        {ok, Admins} when is_list(Admins) ->
            {ok, [
                elib_hashids:replace_fields(A, [<<"id">>, <<"user_id">>, <<"channel_id">>])
                || A <- Admins, is_map(A)
            ]};
        {ok, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        Reason ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

-spec update_admin_role(integer(), integer() | binary(), integer(), integer()) -> ok | {error, binary()}.
update_admin_role(Uid, ChannelId, TargetUid, Role) when is_binary(ChannelId) ->
    case decode_positive_id(ChannelId) of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        DecodedChannelId ->
            update_admin_role(Uid, DecodedChannelId, TargetUid, Role)
    end;
update_admin_role(_Uid, ChannelId, _TargetUid, _Role) when not is_integer(ChannelId); ChannelId =< 0 ->
    {error, <<"频道不存在"/utf8>>};
update_admin_role(Uid, ChannelId, TargetUid, Role) ->
    case channel_logic_common:get_user_role(ChannelId, Uid) of
        3 ->
            case channel_admin_repo:update_role(ChannelId, TargetUid, Role) of
                {ok, _} -> ok;
                {error, _} -> {error, <<"更新角色失败"/utf8>>}
            end;
        _ ->
            {error, <<"无权限操作，仅创建者可修改角色"/utf8>>}
    end.

-spec decode_positive_id(term()) -> integer().
decode_positive_id(Value) ->
    case catch elib_hashids:decode(Value) of
        Id when is_integer(Id), Id > 0 ->
            Id;
        _ ->
            0
    end.

-spec push_unread_updates(integer()) -> ok.
push_unread_updates(ChannelId) ->
    case channel_subscription_repo:list_unread_counts_by_channel(ChannelId) of
        {ok, Rows} when is_list(Rows) ->
            lists:foreach(fun(Row) ->
                case {
                    maps:get(<<"user_id">>, Row, 0),
                    maps:get(<<"unread_count">>, Row, 0)
                } of
                    {Uid, Count} when is_integer(Uid), Uid > 0, is_integer(Count), Count >= 0 ->
                        channel_logic_notify:notify_channel_unread_count(ChannelId, Uid, Count);
                    _ ->
                        ok
                end
            end, Rows),
            ok;
        _ ->
            ok
    end.

-spec is_within_revoke_window(map()) -> boolean().
is_within_revoke_window(Message) ->
    WindowSecs = channel_logic_common:channel_revoke_window_seconds(),
    case WindowSecs =< 0 of
        true ->
            true;
        false ->
            CreatedAt = maps:get(<<"created_at">>, Message, undefined),
            CreatedAtMs = case catch elib_dt:rfc3339_to(CreatedAt, millisecond) of
                Value when is_integer(Value) ->
                    Value;
                _ ->
                    0
            end,
            NowMs = elib_dt:millisecond(),
            is_integer(CreatedAtMs)
                andalso CreatedAtMs > 0
                andalso NowMs >= CreatedAtMs
                andalso (NowMs - CreatedAtMs =< WindowSecs * 1000)
    end.

-spec safe_subscriber_uids(integer()) -> list(integer()).
safe_subscriber_uids(ChannelId) ->
    case catch channel_ds:subscriber_uids(ChannelId) of
        Uids when is_list(Uids) ->
            Uids;
        _ ->
            []
    end.
