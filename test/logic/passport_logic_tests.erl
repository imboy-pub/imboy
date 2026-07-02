-module(passport_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% passport_logic 模块的 EUnit 测试
%%%
%%% 目标：验证登录注册业务逻辑功能
%%% 覆盖：注册、登录、验证码发送、用户验证、快速登录
%%%
%%% 注意：passport_logic API 签名已更新，测试与当前源码对齐：
%%%   - do_login/3 -> do_login/5 (带 DType, Did)
%%%   - quick_login/4 (Service, Operator, Token, PostVals)
%%%   - verify_user/2 (Password, UserMap)
%%%   - signup/4, login/3 (兼容入口)
%%%===================================================================

%% ===================================================================
%% signup/4 测试 (兼容旧入口)
%% ===================================================================

signup_with_valid_data_succeeds_test_() ->
    ?WITH_MECKS(
        [
            {elib_password, [
                {'generate', 1, fun(_Pwd) -> <<"$2a$12$hash">> end}
            ]},
            {account_ds, [
                {'allocate', 0, fun() -> <<"test_account">> end}
            ]},
            {user_ds, [
                {'find_by_mobile', 2, fun(_Mobile, _Fields) -> #{} end},
                {'insert_and_get_id', 1, fun(_Data) -> {ok, 1001} end},
                {'find_by_id', 2, fun(_Id, _Fields) ->
                    #{
                        <<"id">> => 1001,
                        <<"email">> => <<>>,
                        <<"nickname">> => <<"13800138000">>,
                        <<"avatar">> => <<>>,
                        <<"account">> => <<"test">>,
                        <<"gender">> => 0,
                        <<"region">> => <<>>,
                        <<"sign">> => <<>>,
                        <<"status">> => 1
                    }
                end}
            ]},
            {token_ds, [
                {'encrypt_token', 1, fun(_Id) -> <<"encrypted_token">> end},
                {'encrypt_refreshtoken', 1, fun(_Id) -> <<"refresh_token">> end}
            ]}
        ],
        fun() ->
            Mobile = <<"+8613800138000">>,
            Password = <<"Test@123456">>,
            Email = <<"">>,
            PostVals = #{},

            Result = passport_logic:signup(Mobile, Password, Email, PostVals),
            ?assertMatch({ok, _Map}, Result)
        end
    ).

signup_with_short_password_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = passport_logic:signup(<<"+8613800138000">>, <<"123">>, <<"">>, #{}),
        ?assertMatch({error, _, _}, Result)
    end).

%% ===================================================================
%% login/3 测试 (兼容旧入口)
%% ===================================================================

login_with_valid_credentials_succeeds_test_() ->
    ?WITH_MECKS(
        [
            {elib_type, [
                {'is_email', 1, fun(_) -> false end}
            ]},
            {user_ds, [
                {'find_by_mobile', 2, fun(_Mobile, _Fields) ->
                    #{
                        <<"id">> => 123,
                        <<"password">> => <<"$2a$12$hash">>,
                        <<"email">> => <<"">>,
                        <<"nickname">> => <<"Test">>,
                        <<"avatar">> => <<>>,
                        <<"account">> => <<"test">>,
                        <<"gender">> => 0,
                        <<"region">> => <<>>,
                        <<"sign">> => <<>>,
                        <<"status">> => 1
                    }
                end},
                {'update_friends_last_seen_at', 2, fun(_, _) -> ok end}
            ]},
            {elib_password, [
                {'verify', 2, fun(_Pwd, _Hash) -> {ok, true} end}
            ]},
            {token_ds, [
                {'encrypt_token', 1, fun(_Id) -> <<"encrypted_token">> end},
                {'encrypt_refreshtoken', 1, fun(_Id) -> <<"refresh_token">> end}
            ]},
            {user_device_logic, [
                {'validate_device_type', 1, fun(_) -> true end},
                {'check_login_conflict', 2, fun(_, _) -> {ok, no_conflict} end}
            ]},
            {user_device_ds, [
                {'save', 4, fun(_, _, _, _) -> ok end}
            ]},
            {message_ds, [
                {'check_and_notify_offline_msgs', 2, fun(_, _) -> ok end}
            ]}
        ],
        fun() ->
            Account = <<"+8613800138000">>,
            Password = <<"Test@123456">>,
            PostVals = #{<<"did">> => <<"device_1">>},

            Result = passport_logic:login(Account, Password, PostVals),
            ?assertMatch({ok, #{<<"uid">> := _, <<"token">> := _}}, Result)
        end
    ).

login_with_empty_password_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = passport_logic:login(<<"+8613800138000">>, <<"">>, #{}),
        ?assertMatch({error, _, _}, Result)
    end).

%% ===================================================================
%% do_login/3 测试
%% ===================================================================

do_login_delegates_to_5_test_() ->
    ?WITH_MECKS(
        [
            {elib_type, [
                {'is_email', 1, fun(_) -> false end}
            ]},
            {user_ds, [
                {'find_by_mobile', 2, fun(_Mobile, _Fields) ->
                    #{
                        <<"id">> => 123,
                        <<"password">> => <<"$2a$12$hash">>,
                        <<"email">> => <<"">>,
                        <<"nickname">> => <<"Test">>,
                        <<"avatar">> => <<>>,
                        <<"account">> => <<"test">>,
                        <<"gender">> => 0,
                        <<"region">> => <<>>,
                        <<"sign">> => <<>>,
                        <<"status">> => 1
                    }
                end}
            ]},
            {elib_password, [
                {'verify', 2, fun(_Pwd, _Hash) -> {ok, true} end}
            ]},
            {token_ds, [
                {'encrypt_token', 1, fun(_Id) -> <<"encrypted_token">> end},
                {'encrypt_refreshtoken', 1, fun(_Id) -> <<"refresh_token">> end}
            ]},
            {user_device_logic, [
                {'validate_device_type', 1, fun(_) -> true end},
                {'check_login_conflict', 2, fun(_, _) -> {ok, no_conflict} end}
            ]}
        ],
        fun() ->
            Result = passport_logic:do_login(<<"mobile">>, <<"+8613800138000">>, <<"Test@123456">>),
            ?assertMatch({ok, #{<<"uid">> := _, <<"token">> := _}}, Result)
        end
    ).

do_login_with_empty_password_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = passport_logic:do_login(<<"mobile">>, <<"+8613800138000">>, <<>>),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% verify_user/2 测试 -- 注意签名是 (Password, UserMap)
%% ===================================================================

verify_user_with_empty_password_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = passport_logic:verify_user(<<>>, #{}),
        ?assertMatch({error, _}, Result)
    end).

verify_user_with_empty_user_map_returns_error_test() ->
    Result = passport_logic:verify_user(<<"Test@123456">>, #{}),
    ?assertEqual({error, <<"账号不存在"/utf8>>}, Result).

verify_user_with_valid_password_succeeds_test_() ->
    ?WITH_MECKS(
        [
            {elib_password, [
                {'verify', 2, fun(_Pwd, _Hash) -> {ok, true} end}
            ]},
            {token_ds, [
                {'encrypt_token', 1, fun(_Id) -> <<"encrypted_token">> end},
                {'encrypt_refreshtoken', 1, fun(_Id) -> <<"refresh_token">> end}
            ]}
        ],
        fun() ->
            User = #{
                <<"id">> => 12345,
                <<"password">> => <<"$2a$12$hash">>,
                <<"email">> => <<"test@example.com">>,
                <<"nickname">> => <<"Test User">>,
                <<"avatar">> => <<"https://example.com/avatar.jpg">>,
                <<"account">> => <<"test_account">>,
                <<"gender">> => 1,
                <<"region">> => <<"Beijing">>,
                <<"sign">> => <<"Hello">>,
                <<"status">> => 1
            },
            Result = passport_logic:verify_user(<<"Test@123456">>, User),
            ?assertMatch({ok, #{<<"uid">> := 12345, <<"token">> := _}}, Result)
        end
    ).

verify_user_with_wrong_password_fails_test_() ->
    ?WITH_MECKS(
        [
            {elib_password, [
                {'verify', 2, fun(_Pwd, _Hash) -> {error, <<"密码有误"/utf8>>} end}
            ]}
        ],
        fun() ->
            User = #{
                <<"id">> => 12345,
                <<"password">> => <<"$2a$12$hash">>,
                <<"email">> => <<>>,
                <<"nickname">> => <<"Test">>,
                <<"avatar">> => <<>>,
                <<"account">> => <<"test">>,
                <<"gender">> => 0,
                <<"region">> => <<>>,
                <<"sign">> => <<>>,
                <<"status">> => 1
            },
            Result = passport_logic:verify_user(<<"WrongPassword">>, User),
            ?assertMatch({error, _}, Result)
        end
    ).

verify_user_with_disabled_account_fails_test_() ->
    ?WITH_MECKS(
        [
            {elib_password, [
                {'verify', 2, fun(_Pwd, _Hash) -> {ok, true} end}
            ]}
        ],
        fun() ->
            User = #{
                <<"id">> => 12345,
                <<"password">> => <<"$2a$12$hash">>,
                <<"email">> => <<>>,
                <<"nickname">> => <<"T">>,
                <<"avatar">> => <<>>,
                <<"account">> => <<"t">>,
                <<"gender">> => 0,
                <<"region">> => <<>>,
                <<"sign">> => <<>>,
                <<"status">> => 0
            },
            Result = passport_logic:verify_user(<<"Test@123456">>, User),
            ?assertMatch({error, _}, Result)
        end
    ).

verify_user_with_deleted_account_fails_test_() ->
    ?WITH_MECKS(
        [
            {elib_password, [
                {'verify', 2, fun(_Pwd, _Hash) -> {ok, true} end}
            ]}
        ],
        fun() ->
            User = #{
                <<"id">> => 12345,
                <<"password">> => <<"$2a$12$hash">>,
                <<"email">> => <<>>,
                <<"nickname">> => <<"T">>,
                <<"avatar">> => <<>>,
                <<"account">> => <<"t">>,
                <<"gender">> => 0,
                <<"region">> => <<>>,
                <<"sign">> => <<>>,
                <<"status">> => -1
            },
            Result = passport_logic:verify_user(<<"Test@123456">>, User),
            ?assertMatch({error, _}, Result)
        end
    ).

%% ===================================================================
%% do_login_verify/4 测试 (内部函数，通过 do_login/3 间接测试)
%% 注意：do_login_verify/4 未导出，不能直接调用
%% ===================================================================

%% ===================================================================
%% quick_login/4 测试
%% ===================================================================

quick_login_with_empty_service_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = passport_logic:quick_login(<<>>, undefined, <<"token">>, #{}),
        ?assertMatch({error, _}, Result)
    end).

quick_login_with_unsupported_service_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = passport_logic:quick_login(<<"unknown">>, <<"op">>, <<"token">>, #{}),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% send_code/2 测试
%% ===================================================================

send_code_with_sms_type_calls_throttle_test_() ->
    ?WITH_MECKS(
        [
            {throttle, [
                {'check', 2, fun(_Type, _Key) -> ok end}
            ]},
            {verification_code_ds, [
                {'find_by_id', 1, fun(_Id) -> #{} end},
                {'save', 4, fun(_Id, _Code, _Validity, _Now) -> ok end}
            ]},
            {elib_cipher, [
                {'num_random', 1, fun(_N) -> 123456 end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> 1700000000000 end},
                {'add', 2, fun(_Now, _Dur) -> <<"2099-01-01T00:00:00Z">> end},
                {'minus', 2, fun(_Now, _Dur) -> 1699999880000 end}
            ]},
            {ec_cnv, [
                {'to_binary', 1, fun(X) -> integer_to_binary(X) end}
            ]},
            {imboy_sms, [
                {'send', 3, fun(_Mobile, _Content, _Type) -> ok end}
            ]}
        ],
        fun() ->
            Result = passport_logic:send_code(<<"+8613800138000">>, <<"sms">>),
            ?assertMatch({ok, _}, Result)
        end
    ).

send_code_with_email_type_succeeds_test_() ->
    ?WITH_MECKS(
        [
            {verification_code_ds, [
                {'find_by_id', 1, fun(_Id) -> #{} end},
                {'save', 4, fun(_Id, _Code, _Validity, _Now) -> ok end}
            ]},
            {elib_cipher, [
                {'num_random', 1, fun(_N) -> 654321 end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> 1700000000000 end},
                {'add', 2, fun(_Now, _Dur) -> <<"2099-01-01T00:00:00Z">> end}
            ]},
            {elib_email, [
                {'send', 2, fun(_To, _Msg) -> ok end}
            ]}
        ],
        fun() ->
            Result = passport_logic:send_code(<<"test@example.com">>, <<"email">>),
            ?assertMatch({ok, _}, Result)
        end
    ).

send_code_with_invalid_type_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = passport_logic:send_code(<<"test">>, <<"fax">>),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% do_signup/5 测试
%% ===================================================================

do_signup_with_email_succeeds_test_() ->
    ?WITH_MECKS(
        [
            {elib_type, [
                {'is_email', 1, fun(_) -> true end}
            ]},
            {verification_code_ds, [
                {'find_by_id', 1, fun(_Id) ->
                    #{<<"code">> => <<"666666">>, <<"validity_at">> => <<"2099-01-01T00:00:00Z">>}
                end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> 1700000000000 end}
            ]},
            {imboy_env, [
                {'current', 0, fun() -> <<"local">> end}
            ]},
            {user_ds, [
                {'find_id_by_email', 1, fun(_) -> 0 end},
                {'insert_and_get_id', 1, fun(_Data) -> {ok, 2001} end}
            ]},
            {elib_password, [
                {'generate', 1, fun(_Pwd) -> <<"$2a$12$hash">> end}
            ]},
            {account_ds, [
                {'allocate', 0, fun() -> <<"test_account">> end}
            ]},
            {ec_cnv, [
                {'to_integer', 1, fun(X) -> X end}
            ]}
        ],
        fun() ->
            %% 密码已由 handler 层 safe_rsa_decrypt 解密，logic 层直接使用
            Result = passport_logic:do_signup(
                <<"email">>,
                <<"test@example.com">>,
                <<"plaintext_pwd">>,
                <<"666666">>,
                #{<<"nickname">> => <<"Tester">>}
            ),
            ?assertMatch({ok, _}, Result)
        end
    ).

%% 回归测试：rsa_encrypt=off 时密码为 MD5 明文，不应再调 rsa_decrypt
do_signup_email_with_rsa_encrypt_off_test_() ->
    ?WITH_MECKS(
        [
            {elib_type, [
                {'is_email', 1, fun(_) -> true end}
            ]},
            {verification_code_ds, [
                {'find_by_id', 1, fun(_Id) ->
                    #{<<"code">> => <<"666666">>, <<"validity_at">> => <<"2099-01-01T00:00:00Z">>}
                end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> 1700000000000 end}
            ]},
            {imboy_env, [
                {'current', 0, fun() -> <<"local">> end}
            ]},
            {user_ds, [
                {'find_id_by_email', 1, fun(_) -> 0 end},
                {'insert_and_get_id', 1, fun(_Data) -> {ok, 2002} end}
            ]},
            {elib_password, [
                {'generate', 1, fun(_Pwd) -> <<"$2a$12$hash">> end}
            ]},
            {account_ds, [
                {'allocate', 0, fun() -> <<"test_account">> end}
            ]},
            {ec_cnv, [
                {'to_integer', 1, fun(X) -> X end}
            ]}
        ],
        fun() ->
            %% 模拟客户端 rsa_encrypt=off，密码为 MD5 hash 明文
            Md5Pwd = <<"7fef6171469e80d32c0559f88b377245">>,
            Result = passport_logic:do_signup(
                <<"email">>,
                <<"118@imboy.pub">>,
                Md5Pwd,
                <<"666666">>,
                #{<<"nickname">> => <<"imboy118">>}
            ),
            ?assertMatch({ok, _}, Result)
        end
    ).

do_signup_with_mobile_succeeds_test_() ->
    ?WITH_MECKS(
        [
            {verification_code_ds, [
                {'find_by_id', 1, fun(_Id) ->
                    #{<<"code">> => <<"666666">>, <<"validity_at">> => <<"2099-01-01T00:00:00Z">>}
                end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> 1700000000000 end}
            ]},
            {imboy_env, [
                {'current', 0, fun() -> <<"local">> end}
            ]},
            {account_ds, [
                {'allocate', 0, fun() -> <<"test_account_signup">> end}
            ]},
            {user_ds, [
                {'find_id_by_mobile', 1, fun(_) -> 0 end},
                {'insert_and_get_id', 1, fun(_Data) -> {ok, 3001} end}
            ]},
            {elib_password, [
                {'generate', 1, fun(_Pwd) -> <<"$2a$12$hash">> end}
            ]},
            {ec_cnv, [
                {'to_integer', 1, fun(X) -> X end}
            ]}
        ],
        fun() ->
            Result = passport_logic:do_signup(
                <<"mobile">>,
                <<"+8613800138000">>,
                <<"plaintext_pwd">>,
                <<"666666">>,
                #{<<"nickname">> => <<"Tester">>}
            ),
            ?assertMatch({ok, _}, Result)
        end
    ).

do_signup_with_unsupported_type_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = passport_logic:do_signup(
            <<"fax">>,
            <<"account">>,
            <<"pwd">>,
            <<"123">>,
            #{}
        ),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% find_password/5 测试
%% ===================================================================

find_password_with_email_succeeds_test_() ->
    ?WITH_MECKS(
        [
            {elib_type, [
                {'is_email', 1, fun(_) -> true end}
            ]},
            {verification_code_ds, [
                {'find_by_id', 1, fun(_Id) ->
                    #{<<"code">> => <<"666666">>, <<"validity_at">> => <<"2099-01-01T00:00:00Z">>}
                end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> 1700000000000 end}
            ]},
            {imboy_env, [
                {'current', 0, fun() -> <<"local">> end}
            ]},
            {elib_log, [
                {'internal_log', 4, fun(_Level, _Msg, _Mod, _Line) -> ok end}
            ]},
            {user_ds, [
                {'find_id_by_email', 1, fun(_) -> 123 end},
                {'update_password', 2, fun(_, _) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            Result = passport_logic:find_password(
                <<"email">>,
                <<"test@example.com">>,
                <<"new_pwd">>,
                <<"666666">>,
                #{}
            ),
            ?assertMatch({ok, _}, Result)
        end
    ).

find_password_email_not_found_fails_test_() ->
    ?WITH_MECKS(
        [
            {elib_type, [
                {'is_email', 1, fun(_) -> true end}
            ]},
            {verification_code_ds, [
                {'find_by_id', 1, fun(_Id) ->
                    #{<<"code">> => <<"666666">>, <<"validity_at">> => <<"2099-01-01T00:00:00Z">>}
                end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> 1700000000000 end}
            ]},
            {imboy_env, [
                {'current', 0, fun() -> <<"local">> end}
            ]},
            {elib_log, [
                {'internal_log', 4, fun(_Level, _Msg, _Mod, _Line) -> ok end}
            ]},
            {user_ds, [
                {'find_id_by_email', 1, fun(_) -> 0 end}
            ]}
        ],
        fun() ->
            Result = passport_logic:find_password(
                <<"email">>,
                <<"notfound@example.com">>,
                <<"pwd">>,
                <<"666666">>,
                #{}
            ),
            ?assertMatch({error, _}, Result)
        end
    ).

find_password_unsupported_type_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = passport_logic:find_password(
            <<"fax">>,
            <<"account">>,
            <<"pwd">>,
            <<"123">>,
            #{}
        ),
        ?assertMatch({error, _}, Result)
    end).
