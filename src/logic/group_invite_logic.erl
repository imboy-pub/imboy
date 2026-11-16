-module(group_invite_logic).
-moduledoc "群二维码授权 / Server-issued group QR invitations.".

-export([issue/2, redeem/3, allows_public_join/1]).

-define(TTL_SECONDS, 7 * 24 * 60 * 60).
-define(INVITE_AUDIENCE, <<"group_invite_v1">>).

%% A client-shipped key cannot authorize group membership. Derive a separate
%% signing domain from the server JWT secret; invite tokens cannot be used as
%% login tokens even if an auth consumer forgets to check the audience.
issue(Uid, Gid) when is_integer(Uid), Uid > 0, is_integer(Gid), Gid > 0 ->
    case group_member_ds:is_member(Gid, Uid) of
        false -> denied();
        true ->
            case signing_key() of
                {error, _} = Error -> Error;
                {ok, Key} ->
                    Now = erlang:system_time(second),
                    Exp = Now + ?TTL_SECONDS,
                    Claims = #{
                        <<"aud">> => ?INVITE_AUDIENCE,
                        <<"sub">> => ?INVITE_AUDIENCE,
                        <<"gid">> => integer_to_binary(Gid),
                        <<"inviter_uid">> => integer_to_binary(Uid),
                        <<"iat">> => Now,
                        <<"exp">> => Exp
                    },
                    {ok, #{
                        <<"gid">> => integer_to_binary(Gid),
                        <<"exp">> => Exp * 1000,
                        <<"token">> => imboy_jwt:sign(Claims, Key)
                    }}
            end
    end;
issue(_, _) -> denied().

redeem(Uid, Gid, Token) when
    is_integer(Uid), Uid > 0, is_integer(Gid), Gid > 0,
    is_binary(Token), byte_size(Token) > 0, byte_size(Token) =< 2048
->
    case verify(Gid, Token) of
        {ok, Issuer} ->
            case group_member_logic:get_group_capacity(Gid) of
                #{<<"member_max">> := Max, <<"member_count">> := Count} ->
                    %% Membership is checked again under a row lock in the same
                    %% transaction as the join. Removed inviters revoke their QR.
                    case group_member_logic:join_group(
                        <<"scan_qr_code">>, Uid, Gid,
                        #{max_members => Max, current_count => Count,
                            invite_issuer => Issuer}
                    ) of
                        ok -> ok;
                        {error, invalid_group_invitation} -> denied();
                        {error, _} = Error -> Error
                    end;
                _ -> {error, {404, <<"群组不存在"/utf8>>}}
            end;
        {error, _} = Error -> Error
    end;
redeem(_, _, _) -> denied().

%% Self-join without an invitation is allowed only by the server's explicit
%% public/open policy. The DS repeats this check under the group row lock.
allows_public_join(Gid) ->
    case group_ds:find_by_id(Gid, <<"id,status,type,join_limit">>) of
        #{<<"status">> := 1, <<"type">> := 1, <<"join_limit">> := 1} -> true;
        _ -> false
    end.

verify(Gid, Token) ->
    case signing_key() of
        {error, _} = Error -> Error;
        {ok, Key} ->
            ExpectedGid = integer_to_binary(Gid),
            case imboy_jwt:verify(Token, Key) of
                {ok, #{
                    <<"aud">> := ?INVITE_AUDIENCE,
                    <<"sub">> := ?INVITE_AUDIENCE,
                    <<"gid">> := ExpectedGid,
                    <<"inviter_uid">> := IssuerBin,
                    <<"iat">> := IssuedAt,
                    <<"exp">> := Exp
                }} when is_binary(IssuerBin), is_integer(IssuedAt),
                        is_integer(Exp), Exp > IssuedAt,
                        Exp - IssuedAt =< ?TTL_SECONDS ->
                    try binary_to_integer(IssuerBin) of
                        Issuer when Issuer > 0 -> {ok, Issuer};
                        _ -> denied()
                    catch _:_ -> denied()
                    end;
                _ -> denied()
            end
    end.

signing_key() ->
    case config_ds:env(jwt_key, <<>>) of
        Key when is_binary(Key), byte_size(Key) >= 32 ->
            {ok, crypto:mac(hmac, sha256, Key, <<"imboy/group-invite/v1">>)};
        _ -> {error, {503, <<"群邀请服务暂不可用"/utf8>>}}
    end.

denied() -> {error, {403, <<"群邀请无效或已过期，请重新获取群二维码"/utf8>>}}.
