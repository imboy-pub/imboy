-module(channel_logic_invitation).

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
                            case channel_subscribe_ds:create_invitation(ChannelId, Uid, InviteeUid) of
                                {ok, InvitationId} ->
                                    case channel_invitation_ds:find_by_id(InvitationId) of
                                        {ok, Invitation} when is_map(Invitation) ->
                                            Invitation2 = invitation_transfer(Invitation),
                                            channel_logic_notify:notify_invitation_created(ChannelId, InviteeUid),
                                            {ok, Invitation2};
                                        {ok, Other} ->
                                            {error, elib_cnv:safe_to_binary(Other)};
                                        {error, Reason} ->
                                            {error, elib_cnv:safe_to_binary(Reason)}
                                    end;
                                {error, Reason} when is_binary(Reason) ->
                                    {error, Reason};
                                {error, Reason} ->
                                    {error, elib_cnv:safe_to_binary(Reason)};
                                _Other ->
                                    {error, elib_cnv:safe_to_binary(_Other)}
                            end
                    end;
                _ ->
                    {error, <<"频道不存在"/utf8>>}
            end
    end.

-spec accept_invitation(integer(), integer()) -> ok | {error, binary()}.
accept_invitation(Uid, InvitationId) ->
    case channel_subscribe_ds:accept_invitation(InvitationId, Uid) of
        ok ->
            case channel_invitation_ds:find_by_id(InvitationId) of
                {ok, Invitation} when is_map(Invitation) ->
                    ChannelId = maps:get(<<"channel_id">>, Invitation, 0),
                    InviterUid = maps:get(<<"inviter_uid">>, Invitation, 0),
                    case is_integer(ChannelId)
                        andalso ChannelId > 0
                        andalso is_integer(InviterUid)
                        andalso InviterUid > 0 of
                        true ->
                            channel_logic_notify:notify_invitation_accepted(ChannelId, InviterUid, Uid);
                        false ->
                            ok
                    end;
                _ ->
                    ok
            end;
        {error, already_accepted} ->
            ok;
        {error, Reason} when is_binary(Reason) ->
            {error, Reason};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        _Other ->
            {error, elib_cnv:safe_to_binary(_Other)}
    end.

-spec reject_invitation(integer(), integer()) -> ok | {error, binary()}.
reject_invitation(Uid, InvitationId) ->
    case channel_subscribe_ds:reject_invitation(InvitationId, Uid) of
        ok ->
            ok;
        {error, Reason} when is_binary(Reason) ->
            {error, Reason};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        _Other ->
            {error, elib_cnv:safe_to_binary(_Other)}
    end.

-spec get_my_invitations(integer()) -> {ok, [map()]} | {error, binary()}.
get_my_invitations(Uid) ->
    case channel_invitation_ds:list_pending_by_invitee(Uid) of
        {ok, Invitations} when is_list(Invitations) ->
            Invitations2 = lists:map(fun invitation_transfer/1, [I || I <- Invitations, is_map(I)]),
            {ok, Invitations2};
        {ok, Other} ->
            {error, elib_cnv:safe_to_binary(Other)};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        _Reason ->
            {error, elib_cnv:safe_to_binary(_Reason)}
    end.

-spec get_sent_invitations(integer()) -> {ok, [map()]} | {error, binary()}.
get_sent_invitations(Uid) ->
    case channel_invitation_ds:list_by_inviter(Uid, 50) of
        {ok, Invitations} when is_list(Invitations) ->
            Invitations2 = lists:map(fun invitation_transfer/1, [I || I <- Invitations, is_map(I)]),
            {ok, Invitations2};
        {ok, Other} ->
            {error, elib_cnv:safe_to_binary(Other)};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        _Reason ->
            {error, elib_cnv:safe_to_binary(_Reason)}
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
