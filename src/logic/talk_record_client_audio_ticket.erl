-module(talk_record_client_audio_ticket).
-moduledoc "Independent purpose-derived bearer capability; no user JWT or provider Key is transported.".
-export([issue/3, verify/3, max_ttl/0]).
-define(PURPOSE, <<"talk_record_client_audio_v1">>).
-define(MAX_TTL, 300).

max_ttl() -> ?MAX_TTL.

issue(Binding, Secret, Now) ->
    try
        true = valid_secret(Secret),
        true = talk_record_client_audio_contract:validate_binding(Binding),
        Issued = maps:get(<<"source_issued_at">>, Binding),
        Exp = maps:get(<<"source_expires_at">>, Binding),
        true = is_integer(Now) andalso Issued =< Now andalso Now < Exp,
        Claims = Binding#{
            <<"aud">> => ?PURPOSE, <<"sub">> => ?PURPOSE, <<"iat">> => Issued, <<"exp">> => Exp
        },
        {ok, imboy_jwt:sign(Claims, key(Secret)), Exp}
    catch
        _:_ -> {error, audio_source_denied}
    end.

verify(Ticket, Secret, Now) when
    is_binary(Ticket), byte_size(Ticket) > 0, byte_size(Ticket) =< 8192
->
    try
        true = valid_secret(Secret),
        true = is_integer(Now),
        {ok,
            #{
                <<"aud">> := ?PURPOSE,
                <<"sub">> := ?PURPOSE,
                <<"iat">> := Issued,
                <<"exp">> := Expires
            } = Claims} =
            imboy_jwt:verify(Ticket, key(Secret)),
        true =
            is_integer(Issued) andalso is_integer(Expires) andalso Issued =< Now andalso
                Now < Expires,
        true = Expires > Issued andalso Expires =< Issued + ?MAX_TTL,
        B = maps:without([<<"aud">>, <<"sub">>, <<"iat">>, <<"exp">>], Claims),
        true = talk_record_client_audio_contract:validate_binding(B),
        Issued = maps:get(<<"source_issued_at">>, B),
        Expires = maps:get(<<"source_expires_at">>, B),
        {ok, B}
    catch
        _:_ -> {error, audio_source_denied}
    end;
verify(_, _, _) ->
    {error, audio_source_denied}.

valid_secret(S) -> is_binary(S) andalso byte_size(S) >= 32.
key(S) -> crypto:mac(hmac, sha256, S, <<"imboy:talk-record-client-audio:v1">>).
