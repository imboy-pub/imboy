-module(auth_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%% NOTE: get_token/3 was removed from auth_ds as 0-caller dead code; its test
%% was left behind and failed with error:undef. Removed as part of E2EE-019
%% baseline cleanup (see docs/e2ee/v2/evidence/E2EE-019-automated-baseline.md).

verify_sign_with_valid_sign_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'header', 3, fun
                    (<<"vsn">>, _Req, <<"0.1.1">>) -> <<"1.0.0">>;
                    (<<"pkg">>, _Req, <<"pub.imboy.apk">>) -> <<"pub.imboy.apk">>;
                    (<<"did">>, _Req, <<>>) -> <<"device123">>;
                    (<<"cos">>, _Req, <<>>) -> <<"android">>;
                    (<<"sk">>, _Req, <<"1.0.0">>) -> <<"1.0.0">>
                end},
                {'header', 2, fun
                    (<<"sign">>, _Req) -> <<"valid_sign">>;
                    (<<"method">>, _Req) -> <<"sha256">>
                end}
            ]},
            {app_version_ds, [
                {'sign_key', 3, fun(_ClientOS, _Vsn, _Pkg) ->
                    <<"test_key">>
                end}
            ]},
            {elib_hasher, [
                {'hmac_sha256', 2, fun(_PlainText, <<"test_key">>) ->
                    <<"valid_sign">>
                end}
            ]}
        ],
        fun() ->
            Req = #{},
            Env = #{},
            ?assertMatch({ok, _, _}, auth_ds:verify_sign(Req, Env))
        end
    ).

verify_sign_with_invalid_sign_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'header', 3, fun
                    (<<"vsn">>, _Req, <<"0.1.1">>) -> <<"1.0.0">>;
                    (<<"pkg">>, _Req, <<"pub.imboy.apk">>) -> <<"pub.imboy.apk">>;
                    (<<"did">>, _Req, <<>>) -> <<"device123">>;
                    (<<"cos">>, _Req, <<>>) -> <<"android">>;
                    (<<"sk">>, _Req, <<"1.0.0">>) -> <<"1.0.0">>
                end},
                {'header', 2, fun
                    (<<"sign">>, _Req) -> <<"wrong_sign">>;
                    (<<"method">>, _Req) -> <<"sha256">>
                end}
            ]},
            {app_version_ds, [
                {'sign_key', 3, fun(_ClientOS, _Vsn, _Pkg) ->
                    <<"test_key">>
                end}
            ]},
            {elib_hasher, [
                {'hmac_sha256', 2, fun(_PlainText, <<"test_key">>) ->
                    <<"valid_sign">>
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, <<"签名验证失败，请更新客户端"/utf8>>, ?ERR_SIGNATURE_INVALID) ->
                    error_req
                end}
            ]}
        ],
        fun() ->
            Req = #{},
            Env = #{},
            ?assertEqual({stop, error_req}, auth_ds:verify_sign(Req, Env))
        end
    ).

verify_sign_with_missing_sign_header_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'header', 3, fun
                    (<<"vsn">>, _Req, <<"0.1.1">>) -> <<"1.0.0">>;
                    (<<"pkg">>, _Req, <<"pub.imboy.apk">>) -> <<"pub.imboy.apk">>;
                    (<<"did">>, _Req, <<>>) -> <<"device123">>;
                    (<<"cos">>, _Req, <<>>) -> <<"android">>;
                    (<<"sk">>, _Req, <<"1.0.0">>) -> <<"1.0.0">>
                end},
                {'header', 2, fun
                    (<<"sign">>, _Req) -> undefined;
                    (<<"method">>, _Req) -> undefined
                end}
            ]},
            {app_version_ds, [
                {'sign_key', 3, fun(_ClientOS, _Vsn, _Pkg) ->
                    <<"test_key">>
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, <<"签名验证失败，请更新客户端"/utf8>>, ?ERR_SIGNATURE_INVALID) ->
                    error_req
                end}
            ]}
        ],
        fun() ->
            Req = #{},
            Env = #{},
            ?assertEqual({stop, error_req}, auth_ds:verify_sign(Req, Env))
        end
    ).

do_verify_sign_with_sha256_test_() ->
    ?WITH_MECK(
        elib_hasher,
        [
            {'hmac_sha256', 2, fun(_PlainText, _Key) ->
                <<"correct_sha256">>
            end}
        ],
        fun() ->
            ?assertEqual(
                true,
                auth_ds:do_verify_sign(
                    <<"correct_sha256">>, <<"plaintext">>, <<"key">>, <<"sha256">>
                )
            )
        end
    ).

do_verify_sign_with_invalid_input_test() ->
    ?assertEqual(
        false, auth_ds:do_verify_sign(undefined, <<"plaintext">>, <<"key">>, <<"sha256">>)
    ),
    ?assertEqual(
        false, auth_ds:do_verify_sign(<<"sign">>, <<"plaintext">>, undefined, <<"sha256">>)
    ),
    ?assertEqual(false, auth_ds:do_verify_sign(<<"sign">>, <<"plaintext">>, <<"key">>, <<"md5">>)).

verify_token_with_valid_token_test_() ->
    ?WITH_MECKS(
        [
            {token_ds, [
                %% E2EE-013：decrypt_token 返回 5 元组（含绑定 DID）。
                {'decrypt_token', 1, fun(_Token) ->
                    {ok, 123, <<"2026-03-16">>, <<"tk">>, <<"dev-9">>}
                end}
            ]},
            %% did 绑定的 token 需设备仍在（设备被移除 = token 吊销）
            {user_device_ds, [
                {'is_active', 2, fun(123, <<"dev-9">>) -> true end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, 123, <<"dev-9">>}, auth_ds:verify_token(<<"Bearer valid_token">>)
            )
        end
    ).

verify_token_with_refresh_token_test_() ->
    ?WITH_MECK(
        token_ds,
        [
            {'decrypt_token', 1, fun(_Token) ->
                {ok, 123, <<"2026-03-16">>, <<"rtk">>, <<"dev-9">>}
            end}
        ],
        fun() ->
            ?assertEqual(
                {error, ?ERR_TOKEN_REFRESH_NOT_ALLOWED, <<"TOKEN REFRESH NOT ALLOWED"/utf8>>},
                auth_ds:verify_token(<<"Bearer refresh_token">>)
            )
        end
    ).

parse_authorization_header_with_bearer_prefix_test() ->
    ?assertEqual(<<"token123">>, auth_ds:parse_authorization_header(<<"Bearer token123">>)),
    ?assertEqual(<<"raw">>, auth_ds:parse_authorization_header(<<"raw">>)).

remove_last_forward_slash_test() ->
    ?assertEqual(<<"/abc">>, auth_ds:remove_last_forward_slash(<<"/abc/">>)),
    ?assertEqual(<<"/">>, auth_ds:remove_last_forward_slash(<<"/">>)).

strip_version_prefix_test() ->
    ?assertEqual(<<"/user/info">>, auth_ds:strip_version_prefix(<<"/v1/user/info">>, <<"/v1">>)),
    ?assertEqual(<<"/user/info">>, auth_ds:strip_version_prefix(<<"/user/info">>, <<"/v1">>)).

current_uid_default_test() ->
    ?assertEqual(123, auth_ds:current_uid(#{current_uid => 123})),
    ?assertEqual(0, auth_ds:current_uid(#{})).

%% E2EE-013：current_did 从认证上下文取绑定 DID；legacy/无绑定返回 <<>>。
current_did_default_test() ->
    ?assertEqual(<<"dev-9">>, auth_ds:current_did(#{current_did => <<"dev-9">>})),
    ?assertEqual(<<>>, auth_ds:current_did(#{})).
