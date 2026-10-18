-module(imboy_jwt_upload_capability_tests).

-include_lib("eunit/include/eunit.hrl").
-include("error_code.hrl").

%% Synthetic fixtures only; the minimum accepted signing key is exactly 32 bytes.
-define(KEY, <<"0123456789abcdef0123456789abcdef">>).
-define(ROTATED_KEY, <<"abcdef0123456789abcdef0123456789ab">>).
-define(OBJECT_KEY, <<"u7/file/upload.txt">>).
-define(MIME, <<"text/plain">>).
-define(UNAVAILABLE, {error, upload_signing_key_unavailable}).

roundtrip_uses_jwt_key_test() ->
    with_upload_env([{jwt_key, ?KEY}, {jwt_secret, ?ROTATED_KEY}], fun() ->
        Before = erlang:system_time(second),
        Exp = Before + 3600,
        {ok, Token} = imboy_jwt:issue_upload_capability(?OBJECT_KEY, ?MIME, Exp),
        {ok, Claims} = imboy_jwt:verify_upload_capability(Token),
        Issued = maps:get(<<"iat">>, Claims),
        ?assert(is_integer(Issued)),
        ?assert(Issued >= Before),
        ?assert(Issued =< erlang:system_time(second)),
        ?assertEqual(#{
            <<"aud">> => <<"upload">>,
            <<"sub">> => <<"upload">>,
            <<"key">> => ?OBJECT_KEY,
            <<"mime">> => ?MIME,
            <<"iat">> => Issued,
            <<"exp">> => Exp
        }, Claims),
        ?assertEqual({ok, Claims}, imboy_jwt:verify(Token, ?KEY)),
        ?assertEqual({error, invalid_signature}, imboy_jwt:verify(Token, ?ROTATED_KEY))
    end).

invalid_signing_configuration_test_() ->
    Cases = [
        {"missing jwt_key", []},
        {"empty jwt_key", [{jwt_key, <<>>}]},
        {"31-byte jwt_key", [{jwt_key, binary:copy(<<"k">>, 31)}]},
        {"string jwt_key", [{jwt_key, binary_to_list(?KEY)}]},
        {"atom jwt_key", [{jwt_key, undefined}]},
        {"integer jwt_key", [{jwt_key, 32}]},
        {"map jwt_key", [{jwt_key, #{key => ?KEY}}]},
        {"non-byte-aligned jwt_key", [{jwt_key, <<0:257>>}]},
        {"jwt_secret only", [{jwt_secret, ?KEY}]},
        {"empty jwt_key with jwt_secret", [{jwt_key, <<>>}, {jwt_secret, ?KEY}]},
        {"short jwt_key with jwt_secret", [{jwt_key, <<"short">>}, {jwt_secret, ?KEY}]}
    ],
    [{Name, fun() ->
        with_upload_env(Env, fun() ->
            Claims = upload_claims(),
            ValidToken = imboy_jwt:sign(Claims, ?KEY),
            EmptyKeyForgery = imboy_jwt:sign(Claims, <<>>),
            ?assertEqual(?UNAVAILABLE, imboy_jwt:issue_upload_capability(
                ?OBJECT_KEY, ?MIME, maps:get(<<"exp">>, Claims)
            )),
            ?assertEqual(?UNAVAILABLE, imboy_jwt:verify_upload_capability(ValidToken)),
            ?assertEqual(?UNAVAILABLE, imboy_jwt:verify_upload_capability(EmptyKeyForgery))
        end)
    end} || {Name, Env} <- Cases].

empty_key_forgery_rejected_with_configured_key_test() ->
    with_upload_env([{jwt_key, ?KEY}], fun() ->
        Claims = upload_claims(),
        Forgery = imboy_jwt:sign(Claims, <<>>),
        %% The generic API still permits explicit keys; the upload boundary must not.
        ?assertEqual({ok, Claims}, imboy_jwt:verify(Forgery, <<>>)),
        ?assertEqual({error, invalid_signature}, imboy_jwt:verify_upload_capability(Forgery))
    end).

legacy_jwt_secret_capability_rejected_test() ->
    with_upload_env([{jwt_key, ?KEY}, {jwt_secret, ?ROTATED_KEY}], fun() ->
        Token = imboy_jwt:sign(upload_claims(), ?ROTATED_KEY),
        ?assertEqual({error, invalid_signature}, imboy_jwt:verify_upload_capability(Token))
    end).

key_rotation_invalidates_existing_capability_test() ->
    with_upload_env([{jwt_key, ?KEY}], fun() ->
        Exp = erlang:system_time(second) + 3600,
        {ok, OldToken} = imboy_jwt:issue_upload_capability(?OBJECT_KEY, ?MIME, Exp),
        ?assertMatch({ok, _}, imboy_jwt:verify_upload_capability(OldToken)),
        ok = application:set_env(imboy, jwt_key, ?ROTATED_KEY),
        ?assertEqual({error, invalid_signature}, imboy_jwt:verify_upload_capability(OldToken)),
        {ok, NewToken} = imboy_jwt:issue_upload_capability(?OBJECT_KEY, ?MIME, Exp),
        ?assertMatch({ok, _}, imboy_jwt:verify_upload_capability(NewToken)),
        ?assertEqual({error, invalid_signature}, imboy_jwt:verify(NewToken, ?KEY))
    end).

audience_is_upload_only_test() ->
    with_upload_env([{jwt_key, ?KEY}], fun() ->
        Claims = upload_claims(),
        lists:foreach(fun(OtherClaims) ->
            Token = imboy_jwt:sign(OtherClaims, ?KEY),
            ?assertEqual({error, bad_audience}, imboy_jwt:verify_upload_capability(Token))
        end, [
            Claims#{<<"aud">> => <<"download">>},
            Claims#{<<"aud">> => [<<"upload">>]},
            maps:remove(<<"aud">>, Claims),
            #{<<"sub">> => <<"tk">>, <<"uid">> => 7, <<"exp">> => maps:get(<<"exp">>, Claims)}
        ])
    end).

subject_is_upload_only_test() ->
    with_upload_env([{jwt_key, ?KEY}], fun() ->
        Claims = upload_claims(),
        lists:foreach(fun(OtherClaims) ->
            Token = imboy_jwt:sign(OtherClaims, ?KEY),
            ?assertEqual({error, bad_subject}, imboy_jwt:verify_upload_capability(Token))
        end, [
            maps:remove(<<"sub">>, Claims),
            Claims#{<<"sub">> => <<"tk">>},
            Claims#{<<"sub">> => <<"seat_tk">>},
            Claims#{<<"sub">> => <<"rtk">>},
            Claims#{<<"sub">> => <<>>},
            Claims#{<<"sub">> => [<<"upload">>]},
            Claims#{<<"sub">> => 7}
        ])
    end).

upload_capability_cannot_authenticate_as_user_test() ->
    with_upload_env([{jwt_key, ?KEY}], fun() ->
        Exp = erlang:system_time(second) + 3600,
        {ok, Capability} = imboy_jwt:issue_upload_capability(?OBJECT_KEY, ?MIME, Exp),
        ?assertMatch({ok, _}, imboy_jwt:verify_upload_capability(Capability)),
        Expected = {error, ?ERR_TOKEN_INVALID, <<"TOKEN PURPOSE NOT ALLOWED">>},
        ?assertEqual(Expected, auth_ds:verify_token(Capability)),
        ?assertEqual(Expected, auth_ds:verify_token(<<"Bearer ", Capability/binary>>)),
        ?assertEqual(Expected, auth_ds:verify_seat_token(Capability)),
        %% Existing legacy user-token behavior remains unchanged, with no DB access.
        UserToken = token_ds:encrypt_token(7),
        ?assertEqual({ok, 7, <<>>}, auth_ds:verify_token(UserToken)),
        ?assertEqual({error, bad_audience}, imboy_jwt:verify_upload_capability(UserToken))
    end).

bound_object_key_and_mime_cannot_be_tampered_test() ->
    with_upload_env([{jwt_key, ?KEY}], fun() ->
        Claims = upload_claims(),
        Token = imboy_jwt:sign(Claims, ?KEY),
        [Header, _, Signature] = binary:split(Token, <<".">>, [global]),
        lists:foreach(fun(OtherClaims) ->
            OtherToken = imboy_jwt:sign(OtherClaims, ?KEY),
            [_, Payload, _] = binary:split(OtherToken, <<".">>, [global]),
            Forgery = <<Header/binary, ".", Payload/binary, ".", Signature/binary>>,
            ?assertEqual({error, invalid_signature}, imboy_jwt:verify_upload_capability(Forgery))
        end, [
            Claims#{<<"key">> => <<"u8/file/other.txt">>},
            Claims#{<<"mime">> => <<"application/octet-stream">>}
        ])
    end).

time_claims_remain_enforced_test() ->
    with_upload_env([{jwt_key, ?KEY}], fun() ->
        Claims = upload_claims(),
        Now = erlang:system_time(second),
        Cases = [
            {Claims#{<<"exp">> => Now - 60}, {error, expired}},
            {maps:remove(<<"exp">>, Claims), {error, expired}},
            {Claims#{<<"exp">> => <<"invalid">>}, {error, invalid}},
            {Claims#{<<"nbf">> => Now + 3600}, {error, {invalid_claim, nbf}}},
            {Claims#{<<"iat">> => Now + 3600}, {error, {invalid_claim, iat}}}
        ],
        lists:foreach(fun({OtherClaims, Error}) ->
            Token = imboy_jwt:sign(OtherClaims, ?KEY),
            ?assertEqual(Error, imboy_jwt:verify_upload_capability(Token))
        end, Cases)
    end).

upload_claims() ->
    Now = erlang:system_time(second),
    #{
        <<"aud">> => <<"upload">>,
        <<"sub">> => <<"upload">>,
        <<"key">> => ?OBJECT_KEY,
        <<"mime">> => ?MIME,
        <<"iat">> => Now,
        <<"exp">> => Now + 3600
    }.

%% Keep the tests sequential and restore the caller's complete relevant env.
with_upload_env(Values, Fun) ->
    Keys = [jwt_key, jwt_secret],
    Previous = [{Key, application:get_env(imboy, Key)} || Key <- Keys],
    try
        [application:unset_env(imboy, Key) || Key <- Keys],
        [application:set_env(imboy, Key, Value) || {Key, Value} <- Values],
        Fun()
    after
        lists:foreach(fun
            ({Key, {ok, Value}}) -> application:set_env(imboy, Key, Value);
            ({Key, undefined}) -> application:unset_env(imboy, Key)
        end, Previous)
    end.
