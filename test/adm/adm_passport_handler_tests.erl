-module(adm_passport_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_passport_handler 模块的 EUnit 测试
%%%
%%% 目标：验证管理员认证处理器功能
%%% 覆盖：验证码生成、登录页面、登录提交、错误处理
%%%===================================================================

%% ===================================================================
%% 基础测试验证
%% ===================================================================

%% @doc 验证模块可以正常加载
module_loaded_test_() ->
    ?TEST_SIMPLE(fun() ->
        code:ensure_loaded(adm_passport_handler),
        ?assertMatch({file, _}, code:is_loaded(adm_passport_handler))
    end).

%% ===================================================================
%% 验证码测试
%% ===================================================================

%% @doc 测试验证码生成功能
captcha_generates_image_test_() ->
    ?WITH_MECKS([
        {simple_captcha, [
            {'create', 0, fun() ->
                {<<"crypt_key_123">>, <<"<<PNG binary data>>">>}
            end}
        ]},
        {cowboy_req, [
            {'set_resp_cookie', 4, fun(_Name, _Value, Req, _Opts) ->
                Req#{has_captcha_cookie => true}
            end},
            {'reply', 4, fun(Status, Headers, Body, Req) ->
                Req#{
                    response_status => Status,
                    response_headers => Headers,
                    response_body => Body
                }
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>
        }),

        {ok, Req, _State} = adm_passport_handler:init(MockReq, #{action => captcha}),

        {StatusCode, Headers, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_EQUAL(<<"image/png; charset=utf-8">>, maps:get(<<"content-type">>, Headers, undefined)),
        ?assert(maps:is_key(<<"captcha_key">>, Req) orelse maps:get(has_captcha_cookie, Req, false))
    end).

%% ===================================================================
%% 登录页面测试 (GET)
%% ===================================================================

%% @doc 测试登录页面返回 - 正常情况
login_page_returns_html_with_csrf_test_() ->
    ?WITH_MECKS([
        {elib_id, [
            {'gen', 1, fun(_Prefix) ->
                <<"csrf_token_abc123">>
            end}
        ]},
        {imboy_cache, [
            {'set', 2, fun(_Key, _Value) ->
                ok
            end}
        ]},
        {config_ds, [
            {'get', 1, fun(_Key) ->
                <<"-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA\n-----END PUBLIC KEY-----">>
            end}
        ]},
        {imboy_dtl, [
            {'template', 3, fun(_Template, Data, _App) ->
                {ok, iolist_to_binary([
                    "<html><body>",
                    proplists:get_value(system_name, Data),
                    " | ",
                    proplists:get_value(csrf_token, Data),
                    "</body></html>"
                ])}
            end}
        ]},
        {cowboy_req, [
            {'reply', 4, fun(Status, Headers, Body, Req) ->
                Req#{
                    response_status => Status,
                    response_headers => Headers,
                    response_body => Body
                }
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>
        }),

        {ok, Req, _State} = adm_passport_handler:init(MockReq, #{action => login}),

        {StatusCode, Headers, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_EQUAL(<<"text/html; charset=utf-8">>, maps:get(<<"content-type">>, Headers, undefined)),
        ?assert(binary:match(Body, <<"IMBoy Admin System">>) =/= nomatch),
        ?assert(binary:match(Body, <<"csrf_token_abc123">>) =/= nomatch)
    end).

%% ===================================================================
%% 登录提交测试 (POST) - 成功场景
%% ===================================================================

%% @doc 测试登录元数据接口返回 JSON payload
login_meta_returns_payload_test_() ->
    ?WITH_MECKS([
        {elib_id, [
            {'gen', 1, fun(_Prefix) ->
                <<"csrf_meta_123">>
            end}
        ]},
        {imboy_cache, [
            {'set', 2, fun(_Key, _Value) ->
                ok
            end}
        ]},
        {config_ds, [
            {'get', 1, fun(_Key) ->
                <<"-----BEGIN PUBLIC KEY-----\nABCDEF\n-----END PUBLIC KEY-----">>
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Data) ->
                Req#{
                    response_status => 200,
                    response_data => Data
                }
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>
        }),

        {ok, Req, _State} = adm_passport_handler:init(MockReq, #{action => meta}),

        ResponseData = maps:get(response_data, Req, #{}),
        ?ASSERT_EQUAL(<<"csrf_meta_123">>, maps:get(<<"csrf_token">>, ResponseData)),
        ?assert(maps:is_key(<<"public_key">>, ResponseData)),
        ?ASSERT_EQUAL(<<"IMBoy Admin System">>, maps:get(<<"system_name">>, ResponseData))
    end).

%% @doc 测试登录提交 - 验证码和CSRF验证成功，登录成功
login_post_success_with_valid_credentials_test_() ->
    ?WITH_MECKS([
        {elib_req, [
            {'cookie', 2, fun
                (<<"captcha_key">>, _Req) -> <<"valid_crypt_key">>;
                (<<"back_uri">>, _Req) -> <<"/adm/dashboard">>
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"account">> => <<"admin">>,
                    <<"pwd">> => <<"encrypted_password">>,
                    <<"captcha">> => <<"1234">>,
                    <<"csrf_token">> => <<"valid_csrf_token">>
                }
            end}
        ]},
        {imboy_cache, [
            {'get', 1, fun(<<"valid_csrf_token">>) ->
                {ok, 1}
            end},
            {'flush', 1, fun(_Key) ->
                ok
            end}
        ]},
        {simple_captcha, [
            {'check', 2, fun(_CryptKey, _Captcha) ->
                true
            end}
        ]},
        {elib_cipher, [
            {'rsa_decrypt', 1, fun(_EncryptedPwd) ->
                <<"decrypted_password">>
            end}
        ]},
        {adm_passport_logic, [
            {'do_login', 2, fun(<<"admin">>, <<"decrypted_password">>) ->
                {ok, #{
                    <<"id">> => <<"admin_id_123">>,
                    <<"account">> => <<"admin">>,
                    <<"nickname">> => <<"Administrator"/utf8>>,
                    <<"avatar">> => <<>>,
                    <<"role_id">> => 1
                }}
            end}
        ]},
        {cowboy_req, [
            {'set_resp_cookie', 4, fun(_Name, _Value, _Req, _Opts) ->
                cowboy_req_h:new(#{
                    has_adm_user_id_cookie => true
                })
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Data, _Msg) ->
                Req#{
                    response_status => 200,
                    response_data => Data
                }
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>
        }),

        {ok, Req, _State} = adm_passport_handler:init(MockReq, #{action => login}),

        {StatusCode, _Headers, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),

        % 验证返回数据包含用户信息和 next URL
        ResponseData = maps:get(response_data, Req, #{}),
        ?assert(maps:is_key(<<"id">>, ResponseData)),
        ?assert(maps:is_key(<<"next">>, ResponseData)),
        ?ASSERT_EQUAL(<<"admin_id_123">>, maps:get(<<"id">>, ResponseData)),
        ?ASSERT_EQUAL(<<"/adm/dashboard">>, maps:get(<<"next">>, ResponseData))
    end).

%% @doc 测试登录提交 - local/test 环境允许固定验证码，且不会再调用 simple_captcha:check/2
login_post_accepts_fixed_test_captcha_in_local_env_test_() ->
    {setup,
        fun() ->
            PrevImboyEnv = os:getenv("IMBOYENV"),
            os:putenv("IMBOYENV", "local"),
            PrevImboyEnv
        end,
        fun restore_imboyenv/1,
        fun(_) ->
            ?WITH_MECKS([
                {elib_req, [
                    {'cookie', 2, fun
                        (<<"captcha_key">>, _Req) -> <<"valid_crypt_key">>;
                        (<<"back_uri">>, _Req) -> <<"/adm/dashboard">>
                    end}
                ]},
                {elib_param, [
                    {'post', 1, fun(_Req) ->
                        #{
                            <<"account">> => <<"admin">>,
                            <<"pwd">> => <<"encrypted_password">>,
                            <<"captcha">> => <<"1234">>,
                            <<"csrf_token">> => <<"valid_csrf_token">>
                        }
                    end}
                ]},
                {imboy_cache, [
                    {'get', 1, fun(<<"valid_csrf_token">>) ->
                        {ok, 1}
                    end},
                    {'flush', 1, fun(_Key) ->
                        ok
                    end}
                ]},
                {simple_captcha, [
                    {'check', 2, fun(_, _) ->
                        erlang:error(simple_captcha_should_not_be_called)
                    end}
                ]},
                {elib_cipher, [
                    {'rsa_decrypt', 1, fun(_EncryptedPwd) ->
                        <<"decrypted_password">>
                    end}
                ]},
                {adm_passport_logic, [
                    {'do_login', 2, fun(<<"admin">>, <<"decrypted_password">>) ->
                        {ok, #{
                            <<"id">> => <<"admin_id_123">>,
                            <<"account">> => <<"admin">>,
                            <<"nickname">> => <<"Administrator"/utf8>>,
                            <<"avatar">> => <<>>,
                            <<"role_id">> => 1
                        }}
                    end}
                ]},
                {cowboy_req, [
                    {'set_resp_cookie', 4, fun(_Name, _Value, _Req, _Opts) ->
                        cowboy_req_h:new(#{has_adm_user_id_cookie => true})
                    end}
                ]},
                {elib_response, [
                    {'success', 3, fun(Req, Data, _Msg) ->
                        Req#{
                            response_status => 200,
                            response_data => Data
                        }
                    end}
                ]}
            ], fun() ->
                MockReq = cowboy_req_h:new(#{
                    method => <<"POST">>
                }),

                {ok, Req, _State} = adm_passport_handler:init(MockReq, #{action => login}),

                {StatusCode, _Headers, _Body} = cowboy_req_h:response(Req),
                ?ASSERT_EQUAL(200, StatusCode),
                ResponseData = maps:get(response_data, Req, #{}),
                ?ASSERT_EQUAL(<<"admin_id_123">>, maps:get(<<"id">>, ResponseData)),
                ?ASSERT_EQUAL(<<"/adm/dashboard">>, maps:get(<<"next">>, ResponseData))
            end)
        end}.

%% @doc 测试登录提交 - 默认跳转到 /adm/
login_post_success_default_redirect_test_() ->
    ?WITH_MECKS([
        {elib_req, [
            {'cookie', 2, fun
                (<<"captcha_key">>, _Req) -> <<"valid_crypt_key">>;
                (<<"back_uri">>, _Req) -> undefined
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"account">> => <<"admin">>,
                    <<"pwd">> => <<"encrypted_password">>,
                    <<"captcha">> => <<"1234">>,
                    <<"csrf_token">> => <<"valid_csrf_token">>
                }
            end}
        ]},
        {imboy_cache, [
            {'get', 1, fun(_) -> {ok, 1} end},
            {'flush', 1, fun(_) -> ok end}
        ]},
        {simple_captcha, [
            {'check', 2, fun(_, _) -> true end}
        ]},
        {elib_cipher, [
            {'rsa_decrypt', 1, fun(_) -> <<"password">> end}
        ]},
        {adm_passport_logic, [
            {'do_login', 2, fun(_, _) ->
                {ok, #{<<"id">> => <<"admin_id">>, <<"account">> => <<"admin">>}}
            end}
        ]},
        {cowboy_req, [
            {'set_resp_cookie', 4, fun(_, _, _, _) ->
                cowboy_req_h:new(#{})
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Data, _) ->
                Req#{response_data => Data}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>
        }),

        {ok, Req, _State} = adm_passport_handler:init(MockReq, #{action => login}),

        ResponseData = maps:get(response_data, Req, #{}),
        ?ASSERT_EQUAL(<<"/adm/">>, maps:get(<<"next">>, ResponseData))
    end).

%% ===================================================================
%% 登录提交测试 (POST) - 失败场景
%% ===================================================================

%% @doc 测试登录提交 - CSRF token 错误
login_post_fails_with_invalid_csrf_token_test_() ->
    ?WITH_MECKS([
        {elib_req, [
            {'cookie', 2, fun(_, _) -> undefined end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"captcha">> => <<"1234">>, <<"csrf_token">> => <<"invalid_csrf">>}
            end}
        ]},
        {imboy_cache, [
            {'get', 1, fun(_) -> {error, not_found} end}
        ]},
        {elib_response, [
            {'error', 2, fun(Req, _Msg) ->
                Req#{response_status => 400}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>
        }),

        {ok, Req, _State} = adm_passport_handler:init(MockReq, #{action => login}),

        {StatusCode, _, _} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(400, StatusCode)
    end).

%% @doc 测试登录提交 - 验证码错误
login_post_fails_with_invalid_captcha_test_() ->
    ?WITH_MECKS([
        {elib_req, [
            {'cookie', 2, fun(_, _) -> <<"crypt_key">> end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"captcha">> => <<"wrong">>, <<"csrf_token">> => <<"valid_csrf">>}
            end}
        ]},
        {imboy_cache, [
            {'get', 1, fun(_) -> {ok, 1} end}
        ]},
        {simple_captcha, [
            {'check', 2, fun(_, _) -> false end}
        ]},
        {elib_response, [
            {'error', 2, fun(Req, _Msg) ->
                Req#{response_status => 400}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>
        }),

        {ok, Req, _State} = adm_passport_handler:init(MockReq, #{action => login}),

        {StatusCode, _, _} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(400, StatusCode)
    end).

%% @doc 测试登录提交 - 密码解密异常不会导致请求进程崩溃
login_post_fails_with_invalid_encrypted_password_test_() ->
    ?WITH_MECKS([
        {elib_req, [
            {'cookie', 2, fun(_, _) -> <<"crypt_key">> end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"account">> => <<"admin">>,
                    <<"pwd">> => <<"broken_cipher_text">>,
                    <<"captcha">> => <<"1234">>,
                    <<"csrf_token">> => <<"valid_csrf">>
                }
            end}
        ]},
        {imboy_cache, [
            {'get', 1, fun(_) -> {ok, 1} end}
        ]},
        {simple_captcha, [
            {'check', 2, fun(_, _) -> true end}
        ]},
        {elib_cipher, [
            {'rsa_decrypt', 1, fun(_) -> throw({error, invalid_padding}) end}
        ]},
        {elib_response, [
            {'error', 2, fun(Req, _Msg) ->
                Req#{response_status => 400}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>
        }),

        {ok, Req, _State} = adm_passport_handler:init(MockReq, #{action => login}),

        {StatusCode, _, _} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(400, StatusCode)
    end).

%% @doc 测试登录提交 - 账号或密码错误
login_post_fails_with_invalid_credentials_test_() ->
    ?WITH_MECKS([
        {elib_req, [
            {'cookie', 2, fun(_, _) -> <<"crypt_key">> end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"account">> => <<"admin">>,
                    <<"pwd">> => <<"encrypted">>,
                    <<"captcha">> => <<"1234">>,
                    <<"csrf_token">> => <<"valid_csrf">>
                }
            end}
        ]},
        {imboy_cache, [
            {'get', 1, fun(_) -> {ok, 1} end}
        ]},
        {simple_captcha, [
            {'check', 2, fun(_, _) -> true end}
        ]},
        {elib_cipher, [
            {'rsa_decrypt', 1, fun(_) -> <<"wrong_password">> end}
        ]},
        {adm_passport_logic, [
            {'do_login', 2, fun(_, _) ->
                {error, <<"密码错误"/utf8>>}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(Req, _Msg) ->
                Req#{response_status => 401}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>
        }),

        {ok, Req, _State} = adm_passport_handler:init(MockReq, #{action => login}),

        {StatusCode, _, _} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(401, StatusCode)
    end).

%% @doc 测试登录提交 - 账号不存在
login_post_fails_with_nonexistent_account_test_() ->
    ?WITH_MECKS([
        {elib_req, [
            {'cookie', 2, fun(_, _) -> <<"crypt_key">> end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"account">> => <<"nonexistent">>,
                    <<"pwd">> => <<"encrypted">>,
                    <<"captcha">> => <<"1234">>,
                    <<"csrf_token">> => <<"valid_csrf">>
                }
            end}
        ]},
        {imboy_cache, [
            {'get', 1, fun(_) -> {ok, 1} end}
        ]},
        {simple_captcha, [
            {'check', 2, fun(_, _) -> true end}
        ]},
        {elib_cipher, [
            {'rsa_decrypt', 1, fun(_) -> <<"password">> end}
        ]},
        {adm_passport_logic, [
            {'do_login', 2, fun(_, _) ->
                {error, <<"账号不存在"/utf8>>}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(Req, _Msg) ->
                Req#{response_status => 404}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>
        }),

        {ok, Req, _State} = adm_passport_handler:init(MockReq, #{action => login}),

        {StatusCode, _, _} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(404, StatusCode)
    end).

%% @doc 测试登录提交 - 账号被禁用
login_post_fails_with_disabled_account_test_() ->
    ?WITH_MECKS([
        {elib_req, [
            {'cookie', 2, fun(_, _) -> <<"crypt_key">> end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"account">> => <<"disabled_admin">>,
                    <<"pwd">> => <<"encrypted">>,
                    <<"captcha">> => <<"1234">>,
                    <<"csrf_token">> => <<"valid_csrf">>
                }
            end}
        ]},
        {imboy_cache, [
            {'get', 1, fun(_) -> {ok, 1} end}
        ]},
        {simple_captcha, [
            {'check', 2, fun(_, _) -> true end}
        ]},
        {elib_cipher, [
            {'rsa_decrypt', 1, fun(_) -> <<"password">> end}
        ]},
        {adm_passport_logic, [
            {'do_login', 2, fun(_, _) ->
                {error, <<"账号被禁用"/utf8>>}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(Req, _Msg) ->
                Req#{response_status => 403}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>
        }),

        {ok, Req, _State} = adm_passport_handler:init(MockReq, #{action => login}),

        {StatusCode, _, _} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(403, StatusCode)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

%% @doc 测试登录提交 - 缺少必需参数
login_post_fails_with_missing_parameters_test_() ->
    ?WITH_MECKS([
        {elib_req, [
            {'cookie', 2, fun(_, _) -> <<"crypt_key">> end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                % 缺少 account 和 pwd
                #{<<"captcha">> => <<"1234">>, <<"csrf_token">> => <<"valid_csrf">>}
            end}
        ]},
        {imboy_cache, [
            {'get', 1, fun(_) -> {ok, 1} end}
        ]},
        {simple_captcha, [
            {'check', 2, fun(_, _) -> true end}
        ]},
        {elib_cipher, [
            {'rsa_decrypt', 1, fun(undefined) -> <<>> end}
        ]},
        {adm_passport_logic, [
            {'do_login', 2, fun(undefined, <<>>) ->
                {error, <<"密码有误"/utf8>>}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(Req, _Msg) ->
                Req#{response_status => 400}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>
        }),

        {ok, Req, _State} = adm_passport_handler:init(MockReq, #{action => login}),

        {StatusCode, _, _} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(400, StatusCode)
    end).

%% @doc 测试登录提交 - 空密码
login_post_fails_with_empty_password_test_() ->
    ?WITH_MECKS([
        {elib_req, [
            {'cookie', 2, fun(_, _) -> <<"crypt_key">> end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"account">> => <<"admin">>,
                    <<"pwd">> => <<>>,
                    <<"captcha">> => <<"1234">>,
                    <<"csrf_token">> => <<"valid_csrf">>
                }
            end}
        ]},
        {imboy_cache, [
            {'get', 1, fun(_) -> {ok, 1} end}
        ]},
        {simple_captcha, [
            {'check', 2, fun(_, _) -> true end}
        ]},
        {elib_cipher, [
            {'rsa_decrypt', 1, fun(<<>>) -> <<>> end}
        ]},
        {adm_passport_logic, [
            {'do_login', 2, fun(_, <<>>) ->
                {error, <<"密码有误"/utf8>>}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(Req, _Msg) ->
                Req#{response_status => 400}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>
        }),

        {ok, Req, _State} = adm_passport_handler:init(MockReq, #{action => login}),

        {StatusCode, _, _} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(400, StatusCode)
    end).

%% @doc 测试验证码 cookie 缺失
login_post_fails_with_missing_captcha_cookie_test_() ->
    ?WITH_MECKS([
        {elib_req, [
            {'cookie', 2, fun
                (<<"captcha_key">>, _) -> undefined;
                (<<"back_uri">>, _) -> <<"/adm/">>
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"account">> => <<"admin">>,
                    <<"pwd">> => <<"encrypted">>,
                    <<"captcha">> => <<"1234">>,
                    <<"csrf_token">> => <<"valid_csrf">>
                }
            end}
        ]},
        {imboy_cache, [
            {'get', 1, fun(_) -> {ok, 1} end}
        ]},
        {simple_captcha, [
            {'check', 2, fun(undefined, _) -> false end}
        ]},
        {elib_response, [
            {'error', 2, fun(Req, _Msg) ->
                Req#{response_status => 400}
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>
        }),

        {ok, Req, _State} = adm_passport_handler:init(MockReq, #{action => login}),

        {StatusCode, _, _} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(400, StatusCode)
    end).

%% @doc 测试无效的 action
invalid_action_returns_unchanged_request_test_() ->
    ?TEST_SIMPLE(fun() ->
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>
        }),

        % action 为 false 时应该返回未修改的请求
        {ok, Req, _State} = adm_passport_handler:init(MockReq, #{action => false}),
        ?assertEqual(MockReq, Req)
    end).

%% ===================================================================
%% 集成测试场景
%% ===================================================================

%% @doc 测试完整的登录流程
complete_login_flow_test_() ->
    ?WITH_MECKS([
        % 步骤 1: 获取验证码
        {simple_captcha, [
            {'create', 0, fun() ->
                {<<"crypt_key_flow">>, <<"<<PNG>>">>}
            end},
            {'check', 2, fun
                (<<"crypt_key_flow">>, <<"1234">>) -> true;
                (_, _) -> false
            end}
        ]},
        {cowboy_req, [
            {'reply', 4, fun(Status, Headers, Body, Req) ->
                Req#{
                    response_status => Status,
                    response_headers => Headers,
                    response_body => Body
                }
            end},
            {'set_resp_cookie', 4, fun(_, _, Req, _) ->
                Req#{adm_cookie_set => true}
            end}
        ]},
        % 步骤 2: 获取登录页面
        {elib_id, [
            {'gen', 1, fun(_) -> <<"csrf_flow">> end}
        ]},
        {imboy_cache, [
            {'set', 2, fun(_, _) -> ok end},
            {'get', 1, fun(<<"csrf_flow">>) -> {ok, 1} end},
            {'flush', 1, fun(_) -> ok end}
        ]},
        {config_ds, [
            {'get', 1, fun(_) -> <<"PUBLIC_KEY">> end}
        ]},
        {imboy_dtl, [
            {'template', 3, fun(_, _, _) ->
                {ok, <<"<html>登录页面</html>">>}
            end}
        ]},
        % 步骤 3: 提交登录
        {elib_req, [
            {'cookie', 2, fun
                (<<"captcha_key">>, _) -> <<"crypt_key_flow">>;
                (<<"back_uri">>, _) -> undefined
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_) ->
                #{
                    <<"account">> => <<"admin">>,
                    <<"pwd">> => <<"encrypted_pwd">>,
                    <<"captcha">> => <<"1234">>,
                    <<"csrf_token">> => <<"csrf_flow">>
                }
            end}
        ]},
        {elib_cipher, [
            {'rsa_decrypt', 1, fun(_) -> <<"real_password">> end}
        ]},
        {adm_passport_logic, [
            {'do_login', 2, fun(_, _) ->
                {ok, #{
                    <<"id">> => <<"admin_id">>,
                    <<"account">> => <<"admin">>,
                    <<"nickname">> => <<"管理员"/utf8>>
                }}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Data, _) ->
                Req#{response_status => 200, response_data => Data}
            end}
        ]}
    ], fun() ->
        % 步骤 1: 获取验证码
        MockReq1 = cowboy_req_h:new(#{method => <<"GET">>}),
        {ok, Req1, _State1} = adm_passport_handler:init(MockReq1, #{action => captcha}),
        {Status1, _, _} = cowboy_req_h:response(Req1),
        ?ASSERT_EQUAL(200, Status1),

        % 步骤 2: 获取登录页面
        MockReq2 = cowboy_req_h:new(#{method => <<"GET">>}),
        {ok, Req2, _State2} = adm_passport_handler:init(MockReq2, #{action => login}),
        {Status2, _, Body2} = cowboy_req_h:response(Req2),
        ?ASSERT_EQUAL(200, Status2),
        ?assert(binary:match(Body2, <<"登录页面">>) =/= nomatch),

        % 步骤 3: 提交登录
        MockReq3 = cowboy_req_h:new(#{method => <<"POST">>}),
        {ok, Req3, _State3} = adm_passport_handler:init(MockReq3, #{action => login}),
        {Status3, _, _} = cowboy_req_h:response(Req3),
        ?ASSERT_EQUAL(200, Status3),
        ResponseData = maps:get(response_data, Req3, #{}),
        ?assert(maps:is_key(<<"id">>, ResponseData)),
        ?assert(maps:is_key(<<"next">>, ResponseData))
    end).

%% ===================================================================
%% 退出登录测试 (POST)
%% ===================================================================

%% @doc 测试退出登录 - 清理鉴权 Cookie 并返回成功
logout_post_clears_auth_cookies_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'set_resp_cookie', 4, fun(Name, Value, Req, Opts) ->
                ?assertEqual(<<>>, Value),
                ?assertEqual(0, maps:get(max_age, Opts)),
                ?assertEqual(<<"/adm">>, maps:get(path, Opts)),
                Cleared = maps:get(cleared_cookies, Req, []),
                Req#{cleared_cookies => [Name | Cleared]}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Data, _Msg) ->
                Req#{
                    response_status => 200,
                    response_data => Data
                }
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
        {ok, Req, _State} = adm_passport_handler:init(MockReq, #{action => logout}),

        ?ASSERT_EQUAL(200, maps:get(response_status, Req)),
        ClearedCookies = maps:get(cleared_cookies, Req, []),
        ?assert(lists:member(<<"adm_user_id">>, ClearedCookies)),
        ?assert(lists:member(<<"adm_user_sig">>, ClearedCookies)),
        ?assert(lists:member(<<"back_uri">>, ClearedCookies))
    end).

%% @doc 测试退出登录 - 非 POST 方法返回 405
logout_non_post_method_returns_405_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'reply', 4, fun(Status, _Headers, _Body, Req) ->
            Req#{response_status => Status}
        end}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
        {ok, Req, _State} = adm_passport_handler:init(MockReq, #{action => logout}),
        ?ASSERT_EQUAL(405, maps:get(response_status, Req))
    end).

restore_imboyenv(PrevImboyEnv) ->
    case PrevImboyEnv of
        false -> os:unsetenv("IMBOYENV");
        Value -> os:putenv("IMBOYENV", Value)
    end.
