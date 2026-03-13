-module(channel_logic_notify).

-include("log.hrl").

-export([notify_channel_update/2]).
-export([notify_channel_deleted/2]).
-export([notify_channel_subscribed/2]).
-export([notify_channel_unsubscribed/2]).
-export([notify_channel_unread_count/3]).
-export([broadcast_channel_message/2]).
-export([notify_message_deleted/2]).
-export([notify_message_revoked/4]).
-export([notify_invitation_created/2]).
-export([notify_invitation_accepted/3]).
-export([notify_order_paid/2]).

-spec notify_channel_update(integer(), map()) -> ok.
notify_channel_update(ChannelId, Channel) ->
    Action = <<"channel_updated">>,
    SubscriberUids = subscriber_uids_safe(ChannelId, Action),
    ChannelIdBin = elib_hashids:encode(ChannelId),
    Payload = #{<<"channel_id">> => ChannelIdBin, <<"channel">> => Channel},
    send_safe(SubscriberUids, Action, Payload, no_save).

-spec notify_channel_deleted(integer(), list(integer())) -> ok.
notify_channel_deleted(ChannelId, SubscriberUids) ->
    ChannelIdBin = elib_hashids:encode(ChannelId),
    Action = <<"channel_deleted">>,
    Payload = #{<<"channel_id">> => ChannelIdBin},
    send_safe(SubscriberUids, Action, Payload, save).

-spec notify_channel_subscribed(integer(), integer()) -> ok.
notify_channel_subscribed(ChannelId, Uid) ->
    ChannelIdBin = elib_hashids:encode(ChannelId),
    Action = <<"channel_subscribed">>,
    Payload = #{<<"channel_id">> => ChannelIdBin},
    send_safe([Uid], Action, Payload, no_save).

-spec notify_channel_unsubscribed(integer(), integer()) -> ok.
notify_channel_unsubscribed(ChannelId, Uid) ->
    ChannelIdBin = elib_hashids:encode(ChannelId),
    Action = <<"channel_unsubscribed">>,
    Payload = #{<<"channel_id">> => ChannelIdBin},
    send_safe([Uid], Action, Payload, no_save).

-spec notify_channel_unread_count(integer(), integer(), integer()) -> ok.
notify_channel_unread_count(ChannelId, Uid, UnreadCount) ->
    ChannelIdBin = elib_hashids:encode(ChannelId),
    Action = <<"channel_unread_count">>,
    Payload = #{
        <<"channel_id">> => ChannelIdBin,
        <<"unread_count">> => erlang:max(UnreadCount, 0)
    },
    send_safe([Uid], Action, Payload, no_save).

-spec broadcast_channel_message(integer(), map()) -> ok.
broadcast_channel_message(ChannelId, Message) ->
    Action = <<"channel_message">>,
    SubscriberUids = subscriber_uids_safe(ChannelId, Action),
    ChannelIdBin = elib_hashids:encode(ChannelId),
    Payload = Message#{
        <<"channel_id">> => ChannelIdBin,
        <<"type">> => <<"CHANNEL">>
    },
    send_safe(SubscriberUids, Action, Payload, save).

-spec notify_message_deleted(integer(), integer()) -> ok.
notify_message_deleted(ChannelId, MessageId) ->
    Action = <<"channel_message_deleted">>,
    SubscriberUids = subscriber_uids_safe(ChannelId, Action),
    ChannelIdBin = elib_hashids:encode(ChannelId),
    MessageIdBin = elib_hashids:encode(MessageId),
    Payload = #{
        <<"channel_id">> => ChannelIdBin,
        <<"message_id">> => MessageIdBin
    },
    send_safe(SubscriberUids, Action, Payload, save).

-spec notify_message_revoked(integer(), integer(), integer(), binary()) -> ok.
notify_message_revoked(ChannelId, MessageId, RevokedBy, RevokedAt) ->
    Action = <<"channel_message_revoked">>,
    SubscriberUids = subscriber_uids_safe(ChannelId, Action),
    ChannelIdBin = elib_hashids:encode(ChannelId),
    MessageIdBin = elib_hashids:encode(MessageId),
    RevokedByBin = elib_hashids:encode(RevokedBy),
    Payload = #{
        <<"channel_id">> => ChannelIdBin,
        <<"message_id">> => MessageIdBin,
        <<"revoked_by">> => RevokedByBin,
        <<"revoked_at">> => RevokedAt
    },
    send_safe(SubscriberUids, Action, Payload, save).

-spec notify_invitation_created(integer(), integer()) -> ok.
notify_invitation_created(ChannelId, InviteeUid) ->
    ChannelIdBin = elib_hashids:encode(ChannelId),
    Action = <<"channel_invitation_created">>,
    Payload = #{<<"channel_id">> => ChannelIdBin},
    send_safe([InviteeUid], Action, Payload, save).

-spec notify_invitation_accepted(integer(), integer(), integer()) -> ok.
notify_invitation_accepted(ChannelId, InviterUid, InviteeUid) ->
    ChannelIdBin = elib_hashids:encode(ChannelId),
    InviteeUidBin = elib_hashids:encode(InviteeUid),
    Action1 = <<"channel_invitation_accepted">>,
    Payload1 = #{<<"channel_id">> => ChannelIdBin, <<"invitee_uid">> => InviteeUidBin},
    send_safe([InviterUid], Action1, Payload1, no_save),
    Action2 = <<"channel_subscribed">>,
    Payload2 = #{<<"channel_id">> => ChannelIdBin},
    send_safe([InviteeUid], Action2, Payload2, no_save).

-spec notify_order_paid(integer(), integer()) -> ok.
notify_order_paid(ChannelId, Uid) ->
    ChannelIdBin = elib_hashids:encode(ChannelId),
    Action = <<"channel_order_paid">>,
    Payload = #{<<"channel_id">> => ChannelIdBin},
    send_safe([Uid], Action, Payload, no_save),
    Action2 = <<"channel_subscribed">>,
    send_safe([Uid], Action2, Payload, no_save).

-spec send_safe(term(), binary(), map(), save | no_save) -> ok.
send_safe(Uids, Action, Payload, SaveMode) when is_list(Uids) ->
    NormalizedUids = normalize_uids(Uids),
    UidCount = length(NormalizedUids),
    case NormalizedUids of
        [] ->
            ok;
        _ ->
            case catch msg_s2c_ds:send(0, NormalizedUids, Action, <<>>, null, Payload, SaveMode) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?WARN_LOG([channel_logic_notify_send_failed, Action, {uid_count, UidCount}, Reason]),
                    ok;
                {'EXIT', Reason} ->
                    ?ERROR_LOG([channel_logic_notify_send_crashed, Action, {uid_count, UidCount}, Reason]),
                    ok;
                Other ->
                    ?WARN_LOG([channel_logic_notify_send_unexpected, Action, {uid_count, UidCount}, Other]),
                    ok
            end
    end;
send_safe(Uids, Action, _Payload, _SaveMode) ->
    ?ERROR_LOG([channel_logic_notify_invalid_uids, Action, Uids]),
    ok.

-spec subscriber_uids_safe(integer(), binary()) -> list(integer()).
subscriber_uids_safe(ChannelId, Action) ->
    case catch channel_ds:subscriber_uids(ChannelId) of
        Uids when is_list(Uids) ->
            Uids;
        {'EXIT', Reason} ->
            ?ERROR_LOG([channel_logic_notify_subscriber_lookup_crashed, Action, ChannelId, Reason]),
            [];
        Other ->
            ?WARN_LOG([channel_logic_notify_subscriber_lookup_unexpected, Action, ChannelId, Other]),
            []
    end.

-spec normalize_uids(list()) -> list(integer()).
normalize_uids(Uids) ->
    ValidUids = [Uid || Uid <- Uids, is_integer(Uid), Uid > 0],
    case length(ValidUids) =:= length(Uids) of
        true ->
            ValidUids;
        false ->
            ?WARN_LOG([channel_logic_notify_filtered_invalid_uids, {raw_count, length(Uids)}, {valid_count, length(ValidUids)}]),
            ValidUids
    end.
