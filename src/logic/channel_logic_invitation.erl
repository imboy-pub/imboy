-module(channel_logic_invitation).
-compile([nowarn_deprecated_catch]).

-export([create_invitation/3]).
-export([accept_invitation/2]).
-export([reject_invitation/2]).
-export([get_my_invitations/1]).
-export([get_sent_invitations/1]).

-spec create_invitation(integer(), binary(), integer()) -> {ok, map()} | {error, binary()}.
create_invitation(Uid, ChannelIdBin, InviteeUid) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case channel_ds:find_by_id(ChannelId, <<"id,type,status">>) of
                {error, _} ->
                    {error, <<"频道不存在"/utf8>>};
                Channel when is_map(Channel) ->
                    Type = maps:get(<<"type">>, Channel, 0),
                    Status = maps:get(<<"status">>, Channel, 0),
                    if
                        Status =/= 1 ->
                            {error, <<"频道已禁用或删除"/utf8>>};
                        Type =/= 1 ->
                            {error, <<"只有私有频道支持邀请功能"/utf8>>};
                        true ->
                            do_create_invitation(ChannelId, Uid, InviteeUid)
                    end;
                _ ->
                    {error, <<"频道不存在"/utf8>>}
            end
    end.

-spec do_create_invitation(integer(), integer(), integer()) ->
    {ok, map()} | {error, binary()}.
do_create_invitation(ChannelId, InviterUid, InviteeUid) ->
    case channel_subscription_ds:is_subscribed(ChannelId, InviterUid) of
        true ->
            Data = #{
                channel_id => ChannelId,
                inviter_uid => InviterUid,
                invitee_uid => InviteeUid
            },
            case channel_invitation_ds:create(Data) of
                {ok, InvitationId} ->
                    case channel_invitation_ds:find_by_id(InvitationId) of
                        {ok, Invitation} when is_map(Invitation) ->
                            Invitation2 = invitation_transfer(Invitation),
                            channel_logic_notify:notify_invitation_created(ChannelId, InviteeUid),
                            {ok, Invitation2};
                        {error, Reason} ->
                            {error, elib_cnv:safe_to_binary(Reason)}
                    end;
                {error, Reason} when is_binary(Reason) ->
                    {error, Reason};
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end;
        false ->
            {error, <<"您不是频道订阅者，无法邀请他人"/utf8>>}
    end.

-spec accept_invitation(integer(), integer()) -> ok | {error, binary()}.
accept_invitation(Uid, InvitationId) ->
    case channel_invitation_ds:find_by_id(InvitationId) of
        {ok, Invitation} when is_map(Invitation) ->
            InviteeUid = maps:get(<<"invitee_uid">>, Invitation, 0),
            ChannelId = maps:get(<<"channel_id">>, Invitation, 0),
            if
                InviteeUid =/= Uid ->
                    {error, <<"无权接受此邀请"/utf8>>};
                true ->
                    do_accept_invitation(ChannelId, Uid, InvitationId, Invitation)
            end;
        {ok, _} ->
            {error, <<"邀请不存在"/utf8>>};
        {error, not_found} ->
            {error, <<"邀请不存在"/utf8>>};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

-spec do_accept_invitation(integer(), integer(), integer(), map()) ->
    ok | {error, binary()}.
do_accept_invitation(ChannelId, Uid, InvitationId, Invitation) ->
    case channel_invitation_ds:accept(InvitationId, Uid) of
        ok ->
            case channel_ds:subscribe(ChannelId, Uid) of
                ok ->
                    InviterUid = maps:get(<<"inviter_uid">>, Invitation, 0),
                    case is_integer(InviterUid) andalso InviterUid > 0 of
                        true ->
                            channel_logic_notify:notify_invitation_accepted(
                                ChannelId, InviterUid, Uid
                            );
                        false ->
                            ok
                    end;
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end;
        {error, not_found_or_expired} ->
            %% race: already accepted; ensure subscription is active
            case channel_ds:subscribe(ChannelId, Uid) of
                ok -> ok;
                {error, _} -> ok
            end;
        {error, already_accepted} ->
            ok;
        {error, not_found} ->
            {error, <<"邀请不存在"/utf8>>};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

-spec reject_invitation(integer(), integer()) -> ok | {error, binary()}.
reject_invitation(Uid, InvitationId) ->
    case channel_invitation_ds:reject(InvitationId, Uid) of
        ok ->
            ok;
        {error, Reason} when is_binary(Reason) ->
            {error, Reason};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

-spec get_my_invitations(integer()) -> {ok, [map()]} | {error, binary()}.
get_my_invitations(Uid) ->
    case channel_invitation_ds:list_pending_by_invitee(Uid) of
        {ok, Invitations} when is_list(Invitations) ->
            Invitations2 = lists:map(
                fun invitation_transfer/1, [I || I <- Invitations, is_map(I)]
            ),
            {ok, Invitations2};
        {ok, Other} ->
            {error, elib_cnv:safe_to_binary(Other)};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

-spec get_sent_invitations(integer()) -> {ok, [map()]} | {error, binary()}.
get_sent_invitations(Uid) ->
    case channel_invitation_ds:list_by_inviter(Uid, 50) of
        {ok, Invitations} when is_list(Invitations) ->
            Invitations2 = lists:map(
                fun invitation_transfer/1, [I || I <- Invitations, is_map(I)]
            ),
            {ok, Invitations2};
        {ok, Other} ->
            {error, elib_cnv:safe_to_binary(Other)};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

-spec decode_positive_id(term()) -> integer().
decode_positive_id(Value) ->
    case catch ec_cnv:to_integer(Value) of
        Id when is_integer(Id), Id > 0 ->
            Id;
        _ ->
            0
    end.

-spec invitation_transfer(map()) -> map().
invitation_transfer(Invitation) ->
    Invitation.
