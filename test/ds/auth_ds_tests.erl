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

%% DEVICE_SIGN_v1 §6 回归：crypto:hash_equals/2 对不等长入参抛 badarg，
%% 且签名验证在 JWT 门之前 —— 未捕获即匿名可触发的 HTTP 500。
%% 修复后畸形/错配长度的 sign 必须收敛为 false（⇒ 干净 902），等长常时比较不变。
do_verify_sign_malformed_sign_never_throws_test() ->
    PlainText = <<"did-1|1.0.0|android|pub.imboy.apk">>,
    Key = <<"test_key">>,
    %% method=sha256（期望 44 字符 base64）但传 88 字符（sha512 长度错配）
    ?assertEqual(
        false,
        auth_ds:do_verify_sign(binary:copy(<<"A">>, 88), PlainText, Key, <<"sha256">>)
    ),
    %% method=sha512（期望 88 字符）但传 44 字符（sha256 长度错配）
    ?assertEqual(
        false,
        auth_ds:do_verify_sign(binary:copy(<<"B">>, 44), PlainText, Key, <<"sha512">>)
    ),
    %% 过短 / 空签名
    ?assertEqual(false, auth_ds:do_verify_sign(<<"abc">>, PlainText, Key, <<"sha256">>)),
    ?assertEqual(false, auth_ds:do_verify_sign(<<>>, PlainText, Key, <<"sha512">>)).

%% 等长但不匹配的签名仍走常时比较返回 false；正确签名正常通过（修复不破坏主路径）。
do_verify_sign_correct_length_and_valid_sign_test() ->
    PlainText = <<"did-1|1.0.0|android|pub.imboy.apk">>,
    Key = <<"test_key">>,
    Sha256Sign = elib_hasher:hmac_sha256(PlainText, Key),
    Sha512Sign = elib_hasher:hmac_sha512(PlainText, Key),
    ?assertEqual(true, auth_ds:do_verify_sign(Sha256Sign, PlainText, Key, <<"sha256">>)),
    ?assertEqual(true, auth_ds:do_verify_sign(Sha512Sign, PlainText, Key, <<"sha512">>)),
    %% 等长但内容不同 ⇒ false
    Forged = flip_first_byte(Sha256Sign),
    ?assertEqual(false, auth_ds:do_verify_sign(Forged, PlainText, Key, <<"sha256">>)).

flip_first_byte(<<First, Rest/binary>>) when First =:= $A ->
    <<$B, Rest/binary>>;
flip_first_byte(<<_, Rest/binary>>) ->
    <<$A, Rest/binary>>.

verify_token_with_valid_token_test_() ->
    ?WITH_MECKS(
        [
            {token_ds, [
                %% E2EE-013：decrypt_token 返回 6 元组（含绑定 DID）。
                %% Task 10 / LT-04：第 6 位为会话 epoch claim（ep）。
                {'decrypt_token', 1, fun(_Token) ->
                    {ok, 123, <<"2026-03-16">>, <<"tk">>, <<"dev-9">>, 2}
                end}
            ]},
            %% did 绑定的 token 需设备仍在（设备被移除 = token 吊销）
            {user_device_ds, [
                {'is_active', 2, fun(123, <<"dev-9">>) -> true end}
            ]},
            %% Task 10 / LT-04：epoch=2 未被 bump（现势=2）→ 未吊销
            {auth_session_ds, [
                {'revoked', 2, fun(123, 2) -> false end}
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
                {ok, 123, <<"2026-03-16">>, <<"rtk">>, <<"dev-9">>, 1}
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

%% 未登录请求必须发送 401 + 统一错误信封，不能让 Cowboy 自动结束为 204 空响应。
do_authorization_without_token_test_() ->
    ?WITH_MECK(
        elib_response,
        [
            {'error_with_status', 4, fun(#{}, 401, <<"未登录，请先登录"/utf8>>, ?ERR_TOKEN_MISSING) ->
                unauthorized_req
            end}
        ],
        fun() ->
            ?assertEqual(
                {stop, unauthorized_req},
                auth_ds:condition(false, false, undefined, #{}, #{})
            )
        end
    ).

%% MFS3-F01：过期 token（token_ds 细分码 705）属认证边界，必须返回真实
%% HTTP 401 + envelope code 705（客户端按 code 细分"可刷新"），而不是
%% 200 + 业务错误——否则 moya request.ts 的 401 单飞刷新链、imboyapp
%% 的 shouldReLogin 均不触发，token 失效无法自动恢复。
do_authorization_expired_token_maps_http_401_test_() ->
    ?WITH_MECKS(
        [
            {token_ds, [
                {'decrypt_token', 1, fun(_Token) ->
                    {error, 705, <<"Please refresh token">>, #{}}
                end}
            ]},
            {elib_response, [
                {'error_with_status', 4, fun(_Req, 401, _Msg, 705) ->
                    expired_401_req
                end},
                {'error', 3, fun(_Req, _Msg, _Code) ->
                    should_not_happen_200_req
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {stop, expired_401_req},
                auth_ds:condition(false, false, <<"Bearer expired_token">>, #{}, #{})
            )
        end
    ).

%% MFS3-F02：伪造/坏 token（细分码 706）同属认证边界，HTTP 401 + code 706。
do_authorization_invalid_token_maps_http_401_test_() ->
    ?WITH_MECKS(
        [
            {token_ds, [
                {'decrypt_token', 1, fun(_Token) ->
                    {error, 706, <<"Invalid token">>, #{}}
                end}
            ]},
            {elib_response, [
                {'error_with_status', 4, fun(_Req, 401, _Msg, 706) ->
                    invalid_401_req
                end},
                {'error', 3, fun(_Req, _Msg, _Code) ->
                    should_not_happen_200_req
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {stop, invalid_401_req},
                auth_ds:condition(false, false, <<"Bearer forged_token">>, #{}, #{})
            )
        end
    ).

%% 回归保护：非认证边界错误（如 901 用 rtk 充当 access token）仍走
%% 200 + envelope 业务错误，不受 705/706 → 401 映射影响。
do_authorization_refresh_token_keeps_business_error_test_() ->
    ?WITH_MECKS(
        [
            {token_ds, [
                {'decrypt_token', 1, fun(_Token) ->
                    {ok, 123, <<"2026-03-16">>, <<"rtk">>, <<"dev-9">>, 1}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(
                    _Req, <<"TOKEN REFRESH NOT ALLOWED"/utf8>>, ?ERR_TOKEN_REFRESH_NOT_ALLOWED
                ) ->
                    business_200_req
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {stop, business_200_req},
                auth_ds:condition(false, false, <<"Bearer refresh_token">>, #{}, #{})
            )
        end
    ).
