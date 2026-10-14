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
                % 套件隔离治理：本地库累积用户已超社区版 License 配额，
                % quota_guard 会 402 拒注册；钉死 count=0 保持单测语义。
                {'count', 0, fun() -> 0 end},
                {'find_by_mobile', 2, fun(_Mobile, _Fields) -> #{} end},
                {'insert_and_get_id', 1, fun(_Data) -> {ok, 1001} end},
                {'count', 0, fun() -> 100 end},

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
            %% CI-00 修桩：显式控制 license 配额（多轮测试后真库用户数超
            %% trial 上限，quota_guard 402 非本用例语义）。
            {imboy_license, [{'check_user_quota', 1, fun(_) -> ok end}]},
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
            ]},
            {login_security_logic, [
                {'check_login_allowed', 2, fun(_, _) -> {ok, true} end},
                {'record_login_success', 2, fun(_, _) -> ok end}
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
            ]},
            {login_security_logic, [
                {'check_login_allowed', 2, fun(_, _) -> {ok, true} end},
                {'record_login_success', 2, fun(_, _) -> ok end}
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
        Result = passport_logic:verify_user(<<>>, #{}, <<>>),
        ?assertMatch({error, _}, Result)
    end).

verify_user_with_empty_user_map_returns_error_test() ->
    Result = passport_logic:verify_user(<<"Test@123456">>, #{}, <<>>),
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
            Result = passport_logic:verify_user(<<"Test@123456">>, User, <<>>),
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
            Result = passport_logic:verify_user(<<"WrongPassword">>, User, <<>>),
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
            Result = passport_logic:verify_user(<<"Test@123456">>, User, <<>>),
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
            Result = passport_logic:verify_user(<<"Test@123456">>, User, <<>>),
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
                {'consume', 2, fun(_Id, _Code) -> {ok, <<"verified">>} end},
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
            {config_ds, [
                %% send_sms_code 先查总开关（off 跳过外发）再查平台；
                %% 此处 on 走外发路径，才能断言 imboy_sms:send 被调
                {'env', 2, fun
                    ([sms, switch], _Default) -> <<"on">>;
                    ([sms, platform], _Default) -> <<"jsms">>
                end}
            ]},
            {imboy_sms, [
                {'send', 3, fun(_Mobile, Content, Type) ->
                    ?assertEqual(<<"jsms">>, Type),
                    ?assertEqual(<<"123456">>, Content),
                    {ok, #{<<"msg_id">> => <<"message-1">>}}
                end}
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
                {'consume', 2, fun(_Id, _Code) -> {ok, <<"verified">>} end},
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
            ]},
            %% email 路径已补 throttle 守卫（此前只有 sms 有）
            {throttle, [
                {'check', 2, fun(per_minute_once, {send_code, _Target}) -> ok end}
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
                end},
                {'consume', 2, fun(_Id, _Code) -> {ok, <<"verified">>} end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> 1700000000000 end}
            ]},
            {imboy_env, [
                {'current', 0, fun() -> <<"local">> end}
            ]},
            {user_ds, [
                % 钉死 count=0：绕过 License 配额 402（见文件头说明）
                {'count', 0, fun() -> 0 end},
                {'find_id_by_email', 1, fun(_) -> 0 end},
                {'insert_and_get_id', 1, fun(_Data) -> {ok, 2001} end}
            ]},
            {imboy_license, [{'check_user_quota', 1, fun(_) -> ok end}]},

            {elib_password, [
                {'generate', 1, fun(_Pwd) -> <<"$2a$12$hash">> end}
            ]},
            {account_ds, [
                {'allocate', 0, fun() -> <<"test_account">> end}
            ]},
            {ec_cnv, [
                {'to_integer', 1, fun(X) -> X end}
            ]},
            {login_security_logic, [
                {'check_login_allowed', 2, fun(_, _) -> {ok, true} end},
                {'record_login_success', 2, fun(_, _) -> ok end}
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
                end},
                {'consume', 2, fun(_Id, _Code) -> {ok, <<"verified">>} end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> 1700000000000 end}
            ]},
            {imboy_env, [
                {'current', 0, fun() -> <<"local">> end}
            ]},
            {user_ds, [
                % 钉死 count=0：绕过 License 配额 402（见文件头说明）
                {'count', 0, fun() -> 0 end},
                {'find_id_by_email', 1, fun(_) -> 0 end},
                {'insert_and_get_id', 1, fun(_Data) -> {ok, 2002} end}
            ]},
            {imboy_license, [{'check_user_quota', 1, fun(_) -> ok end}]},

            {elib_password, [
                {'generate', 1, fun(_Pwd) -> <<"$2a$12$hash">> end}
            ]},
            {account_ds, [
                {'allocate', 0, fun() -> <<"test_account">> end}
            ]},
            {ec_cnv, [
                {'to_integer', 1, fun(X) -> X end}
            ]},
            {login_security_logic, [
                {'check_login_allowed', 2, fun(_, _) -> {ok, true} end},
                {'record_login_success', 2, fun(_, _) -> ok end}
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
                end},
                {'consume', 2, fun(_Id, _Code) -> {ok, <<"verified">>} end}
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
                % 钉死 count=0：绕过 License 配额 402（见文件头说明）
                {'count', 0, fun() -> 0 end},
                {'find_id_by_mobile', 1, fun(_) -> 0 end},
                {'insert_and_get_id', 1, fun(_Data) -> {ok, 3001} end}
            ]},
            {elib_password, [
                {'generate', 1, fun(_Pwd) -> <<"$2a$12$hash">> end}
            ]},
            {ec_cnv, [
                {'to_integer', 1, fun(X) -> X end}
            ]},
            {login_security_logic, [
                {'check_login_allowed', 2, fun(_, _) -> {ok, true} end},
                {'record_login_success', 2, fun(_, _) -> ok end}
            ]},
            %% CI-00 修桩：显式控制 license 配额（同上，避免真库用户数超限 402）
            {imboy_license, [{'check_user_quota', 1, fun(_) -> ok end}]}
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
                end},
                {'consume', 2, fun(_Id, _Code) -> {ok, <<"verified">>} end}
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
            ]},
            %% Task 10 / LT-04：忘记密码成功后全端会话吊销（bump + 踢会话）
            {auth_session_ds, [
                {'bump', 1, fun(_Uid) -> ok end},
                {'kick_all_sessions', 1, fun(_Uid) -> ok end}
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
                end},
                {'consume', 2, fun(_Id, _Code) -> {ok, <<"verified">>} end}
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

%% ===================================================================
%% E2EE-013: quick_login 的 did 绑定
%% ===================================================================

quick_login_did_user_row() ->
    #{
        <<"id">> => 1001,
        <<"email">> => <<>>,
        <<"nickname">> => <<"quick">>,
        <<"avatar">> => <<>>,
        <<"account">> => <<"quick_acc">>,
        <<"gender">> => 0,
        <<"region">> => <<>>,
        <<"sign">> => <<>>,
        <<"status">> => 1
    }.

quick_login_binds_did_into_token_test_() ->
    ?WITH_MECKS(
        [
            {imboy_sms, [
                {'jverification', 1, fun(_Token) -> {ok, <<"13800138000">>} end}
            ]},
            {user_ds, [
                {'find_by_mobile', 2, fun(_Mobile, _Fields) -> quick_login_did_user_row() end}
            ]},
            {token_ds, [
                {'encrypt_token', 2, fun(1001, <<"dev-abc">>) -> <<"tk_with_did">> end},
                {'encrypt_refreshtoken', 2, fun(1001, <<"dev-abc">>) -> <<"rtk_with_did">> end}
            ]}
        ],
        fun() ->
            {ok, Data} = passport_logic:quick_login(
                <<"jverify">>,
                <<"CM">>,
                <<"jv_token">>,
                #{<<"did">> => <<"dev-abc">>}
            ),
            ?assertEqual(<<"tk_with_did">>, maps:get(<<"token">>, Data)),
            ?assertEqual(<<"rtk_with_did">>, maps:get(<<"refreshtoken">>, Data))
        end
    ).

%% 调用方未上报 did（当前 passport_handler 即如此）→ 仍签发空 did token，不报错
quick_login_without_did_issues_legacy_token_test_() ->
    ?WITH_MECKS(
        [
            {imboy_sms, [
                {'jverification', 1, fun(_Token) -> {ok, <<"13800138000">>} end}
            ]},
            {user_ds, [
                {'find_by_mobile', 2, fun(_Mobile, _Fields) -> quick_login_did_user_row() end}
            ]},
            {token_ds, [
                {'encrypt_token', 2, fun(1001, <<>>) -> <<"tk_legacy">> end},
                {'encrypt_refreshtoken', 2, fun(1001, <<>>) -> <<"rtk_legacy">> end}
            ]}
        ],
        fun() ->
            {ok, Data} = passport_logic:quick_login(
                <<"jverify">>, <<"CM">>, <<"jv_token">>, #{}
            ),
            ?assertEqual(<<"tk_legacy">>, maps:get(<<"token">>, Data)),
            ?assertEqual(<<"rtk_legacy">>, maps:get(<<"refreshtoken">>, Data))
        end
    ).

record_login_device_failure_test_() ->
    ?WITH_MECKS(
        [
            {user_device_ds, [
                {'save', 4, fun(_Now, 995011, <<"synthetic-device">>, _Post) ->
                    {error, synthetic_device_write_failure}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, synthetic_device_write_failure},
                passport_logic:record_login_device(995011, #{<<"did">> => <<"synthetic-device">>})
            )
        end
    ).

%% ===================================================================
%% Limiter V1 L3（计划附录 C、§9.2 AC-01/02/17/19）。
%%
%% 真实链 = passport_logic 编排 + login_security_logic + 真实
%% login_attempt_ds（owner + ETS，limiter_test_support 装配）；只 mock
%% 存储边界（user_ds / verification_code_ds 消费观测 / token 签发 /
%% 密码 verify）与故障注入场景的 DS 单函数（AC-12 许可）。
%% oracle 为 L1 冻结合同的正确行为。
%% ===================================================================

-define(L0_ID, <<"limiter-l0@e2e.test">>).
-define(L0_ACCT, <<"limiter-l0-acct">>).
-define(L3_UID, 990001).
-define(L3_ID_USER, {user, ?L3_UID}).
-define(L3_LOCK_MSG, <<"失败次数过多，账户已临时锁定，请稍后再试"/utf8>>).
-define(L3_SVC_MSG, <<"服务暂不可用，请稍后重试"/utf8>>).

ensure_l3_ds() ->
    ok = limiter_test_support:ensure_owner().

cleanup_l3_ds() ->
    try
        limiter_test_support:ensure_owner()
    catch
        _:_ ->
            ok
    end,
    ok.

l3_user_map() ->
    #{
        <<"id">> => ?L3_UID,
        <<"password">> => <<"$2a$12$hash">>,
        <<"email">> => ?L0_ID,
        <<"nickname">> => <<"L3">>,
        <<"avatar">> => <<>>,
        <<"account">> => ?L0_ACCT,
        <<"mobile">> => <<"+8619900010000">>,
        <<"gender">> => 0,
        <<"region">> => <<>>,
        <<"sign">> => <<>>,
        <<"status">> => 1
    }.

l3_user_map(StatusOverride) ->
    (l3_user_map())#{<<"status">> => StatusOverride}.

%% 密码 verify（mock 边界）：expected = {ok, true} | {error, Msg}
l3_pwd_mocks(VerifyResult) ->
    {elib_password, [
        {'verify', 2, fun(_Pwd, _Hash) -> VerifyResult end},
        %% KDF 升级链不需要真实跑（合成 hash）
        {'verify_and_plan', 2, fun(_Pwd, _Hash) -> {error, skip} end}
    ]}.

%% 登录链共享 mock：用户查询可控 + token 签发可观测 + 存储边界静音。
%% FindEmail / FindAccount 按入参分支：合成用户命中，其它 → 空查无。
l3_base_mocks(FindEmail, FindAccount) ->
    [
        {elib_log, [
            {'internal_log', 4, fun(_Level, _Msg, _Module, _Line) -> ok end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1790000000 end}
        ]},
        {user_ds, [
            {'find_by_email', 2, fun(Id, _Fields) -> FindEmail(Id) end},
            {'find_by_account', 2, fun(Id, _Fields) -> FindAccount(Id) end},
            {'find_by_mobile', 2, fun(_Id, _Fields) -> #{} end},
            {'update_friends_last_seen_at', 2, fun(_, _) -> ok end}
        ]},
        {token_ds, [
            {'encrypt_token', 2, fun(_Uid, _Did) -> <<"l3-token">> end},
            {'encrypt_token', 1, fun(_Uid) -> <<"l3-token">> end},
            {'encrypt_refreshtoken', 2, fun(_Uid, _Did) -> <<"l3-rtoken">> end},
            {'encrypt_refreshtoken', 1, fun(_Uid) -> <<"l3-rtoken">> end}
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
        ]},
        {user_log_ds, [
            {'add_internal', 5, fun(_, _, _, _, _) -> ok end}
        ]},
        {elib_async, [
            {'async', 1, fun(_F) -> ok end}
        ]}
    ].

l3_code_consume_mock(ConsumeFun) ->
    {verification_code_ds, [
        {'consume', 2, ConsumeFun}
    ]}.

%% AC-02（F2）fixed oracle：真实 DS 置锁（真实 failure 累计到阈值），
%% 真实 P/S 执行 do_login_by_code → 锁定拒绝、验证码未被消费（consume
%% 调用与否是观测点）、返回锁定消息。反例：若实现回退为旧二元匹配
%% 漏掉 {error, locked, _}，锁定门失效 → consume 会被调用，本用例的
%% consumed=0 断言首先变红。
limiter_code_locked_test_() ->
    FindEmail = fun
        (?L0_ID) -> l3_user_map();
        (_) -> #{}
    end,
    FindAccount = fun
        (?L0_ACCT) -> l3_user_map();
        (_) -> #{}
    end,
    ?WITH_MECKS(
        l3_base_mocks(FindEmail, FindAccount) ++
            [l3_code_consume_mock(fun(_Id, _Code) -> {ok, <<"verified">>} end)],
        fun() ->
            ok = ensure_l3_ds(),
            try
                %% 真实累计到阈值置锁（已知用户键 {user, Uid}，跨别名）
                lists:foreach(
                    fun(_) ->
                        {ok, _} = login_security_logic:record_login_failure(?L3_ID_USER)
                    end,
                    lists:seq(1, 5)
                ),
                {ok, Snap} = login_attempt_ds:snapshot(?L3_ID_USER),
                ?assertEqual(true, maps:get(locked, Snap)),
                persistent_term:put({?MODULE, l3_consumed}, 0),
                try
                    ok = meck:expect(verification_code_ds, consume, fun(_Id, _Code) ->
                        persistent_term:put({?MODULE, l3_consumed}, 1),
                        {ok, <<"verified">>}
                    end),
                    Result = passport_logic:do_login_by_code(
                        <<"email">>, ?L0_ID, <<"123456">>, <<"android">>, <<"l3-did">>
                    ),
                    %% 锁定拒绝 + 明确锁定消息
                    ?assertEqual({error, ?L3_LOCK_MSG}, Result),
                    %% 关键观测：锁定时验证码未被消费（不触碰验证码存储）
                    ?assertEqual(0, persistent_term:get({?MODULE, l3_consumed}, 0))
                after
                    persistent_term:erase({?MODULE, l3_consumed})
                end,
                ok
            after
                cleanup_l3_ds()
            end
        end
    ).

%% AC-19/09 业务部分（F3 修复）：已知用户的所有登录别名合并到同一
%% {user, Uid} 键——email 失败 K 次 + account 失败 M 次 = K+M；两个不同
%% 未知账号计数互不影响、未知与已知分键。反例：若实现回退为旧键分叉
%% （输入别名 vs User.account 两个键），合并断言 count=K+M 首先变红。
limiter_identity_key_fork_test_() ->
    FindEmail = fun
        (?L0_ID) -> l3_user_map();
        (<<"unknown-a@e2e.test">>) -> #{};
        (<<"unknown-b@e2e.test">>) -> #{};
        (_) -> #{}
    end,
    FindAccount = fun
        (?L0_ACCT) -> l3_user_map();
        (_) -> #{}
    end,
    ?WITH_MECKS(
        l3_base_mocks(FindEmail, FindAccount) ++ [l3_pwd_mocks({error, <<"密码有误"/utf8>>})],
        fun() ->
            ok = ensure_l3_ds(),
            try
                %% email 别名失败 K=2
                lists:foreach(
                    fun(N) ->
                        ?assertMatch(
                            {error, <<"密码有误"/utf8>>},
                            passport_logic:do_login(<<"email">>, ?L0_ID, <<"wrong">>)
                        ),
                        {ok, Snap} = login_attempt_ds:snapshot(?L3_ID_USER),
                        ?assertEqual(N, maps:get(count, Snap))
                    end,
                    lists:seq(1, 2)
                ),
                %% account 别名失败 M=3（查到同一用户 → 同一 {user, Uid} 键）
                lists:foreach(
                    fun(N) ->
                        ?assertMatch(
                            {error, <<"密码有误"/utf8>>},
                            passport_logic:do_login(<<"account">>, ?L0_ACCT, <<"wrong">>)
                        ),
                        {ok, Snap} = login_attempt_ds:snapshot(?L3_ID_USER),
                        ?assertEqual(2 + N, maps:get(count, Snap))
                    end,
                    lists:seq(1, 3)
                ),
                {ok, Snap5} = login_attempt_ds:snapshot(?L3_ID_USER),
                ?assertEqual(5, maps:get(count, Snap5)),
                %% 未知账号：有界独立键，不并入已知用户
                ?assertMatch(
                    {error, <<"账号不存在"/utf8>>},
                    passport_logic:do_login(<<"email">>, <<"unknown-a@e2e.test">>, <<"wrong">>)
                ),
                ?assertMatch(
                    {error, <<"账号不存在"/utf8>>},
                    passport_logic:do_login(<<"email">>, <<"unknown-b@e2e.test">>, <<"wrong">>)
                ),
                {ok, SnapA} = login_attempt_ds:snapshot(
                    {unknown, email, <<"unknown-a@e2e.test">>}
                ),
                ?assertEqual(1, maps:get(count, SnapA)),
                {ok, SnapB} = login_attempt_ds:snapshot(
                    {unknown, email, <<"unknown-b@e2e.test">>}
                ),
                ?assertEqual(1, maps:get(count, SnapB)),
                {ok, SnapUser} = login_attempt_ds:snapshot(?L3_ID_USER),
                ?assertEqual(5, maps:get(count, SnapUser)),
                ok
            after
                cleanup_l3_ds()
            end
        end
    ).

%% AC-17 部分成功点（F3b 修复）：禁用账户 + 正确验证码 → 不 reset
%% （计数保留）、按禁用错误返回；允许账户 → reset 确认成功才签发。
%% 反例：若实现回退为旧「reset 早于 status 检查」，禁用段后计数断言
%% count=3 首先变红；若 reset 未确认就签发，允许段的 unavailable 注入
%% 下 token 计数断言首先变红。
limiter_code_reset_before_status_test_() ->
    FindAccount = fun
        (?L0_ACCT) -> l3_user_map();
        (_) -> #{}
    end,
    FindEmail = fun
        (?L0_ID) -> l3_user_map();
        (_) -> #{}
    end,
    ?WITH_MECKS(
        l3_base_mocks(FindEmail, FindAccount) ++
            [l3_code_consume_mock(fun(_Id, _Code) -> {ok, <<"verified">>} end)],
        fun() ->
            ok = ensure_l3_ds(),
            try
                %% 段一：禁用账户（status=0）+ 正确验证码
                ok = meck:expect(user_ds, find_by_account, fun(?L0_ACCT, _F) ->
                    l3_user_map(0)
                end),
                lists:foreach(
                    fun(_) ->
                        {ok, _} = login_security_logic:record_login_failure(?L3_ID_USER)
                    end,
                    lists:seq(1, 3)
                ),
                ?assertMatch(
                    {error, <<"账号被禁用"/utf8>>},
                    passport_logic:do_login_by_code(
                        <<"account">>, ?L0_ACCT, <<"123456">>, <<"android">>, <<"l3-did">>
                    )
                ),
                %% 不 reset：失败历史完整保留（旧实现此处已清零）
                {ok, Snap} = login_attempt_ds:snapshot(?L3_ID_USER),
                ?assertEqual(3, maps:get(count, Snap)),
                %% 段二：允许账户 + 正确验证码 → reset 确认后才签发
                ok = cleanup_l3_ds(),
                ok = meck:expect(user_ds, find_by_account, fun(?L0_ACCT, _F) ->
                    l3_user_map()
                end),
                persistent_term:put({?MODULE, l3_tokens}, 0),
                try
                    ok = meck:expect(token_ds, encrypt_token, fun(_Uid, _Did) ->
                        persistent_term:put({?MODULE, l3_tokens}, 1),
                        <<"l3-token">>
                    end),
                    ?assertMatch(
                        {ok, #{<<"token">> := <<"l3-token">>}},
                        passport_logic:do_login_by_code(
                            <<"account">>, ?L0_ACCT, <<"123456">>, <<"android">>, <<"l3-did">>
                        )
                    ),
                    ?assertEqual(1, persistent_term:get({?MODULE, l3_tokens}, 0)),
                    {ok, Snap2} = login_attempt_ds:snapshot(?L3_ID_USER),
                    ?assertEqual(0, maps:get(count, Snap2))
                after
                    persistent_term:erase({?MODULE, l3_tokens})
                end,
                ok
            after
                cleanup_l3_ds()
            end
        end
    ).

%% AC-17 顺序协议（reset 失败注入）：正确密码 + reset 强制失败 →
%% 不签发（token 未生成）、logic 层返回服务不可用标记。反例：若实现
%% 回退为旧「login_resp 先于 reset 且 reset 忽略结果」，token 计数
%% 断言首先变红。
limiter_password_reset_fail_no_token_test_() ->
    FindEmail = fun
        (?L0_ID) -> l3_user_map();
        (_) -> #{}
    end,
    FindAccount = fun
        (?L0_ACCT) -> l3_user_map();
        (_) -> #{}
    end,
    ?WITH_MECKS(
        l3_base_mocks(FindEmail, FindAccount) ++ [l3_pwd_mocks({ok, true})],
        fun() ->
            ok = ensure_l3_ds(),
            try
                %% 故障注入（AC-12 许可）：meck DS 单函数模拟 degraded reset
                ok = meck:new(login_attempt_ds, [passthrough]),
                ok = meck:expect(login_attempt_ds, reset, fun(_Identity) ->
                    {error, unavailable}
                end),
                persistent_term:put({?MODULE, l3_tokens}, 0),
                try
                    ok = meck:expect(token_ds, encrypt_token, fun(_Uid, _Did) ->
                        persistent_term:put({?MODULE, l3_tokens}, 1),
                        <<"l3-token">>
                    end),
                    Result = passport_logic:do_login(
                        <<"email">>, ?L0_ID, <<"right-pwd">>, <<"android">>, <<"l3-did">>
                    ),
                    ?assertEqual({error, service_unavailable}, Result),
                    %% reset 未确认 → 无凭据生成、无凭据返回
                    ?assertEqual(0, persistent_term:get({?MODULE, l3_tokens}, 0))
                after
                    persistent_term:erase({?MODULE, l3_tokens}),
                    catch meck:unload(login_attempt_ds)
                end,
                ok
            after
                cleanup_l3_ds()
            end
        end
    ).

%% AC-01 logic 侧：F 次错误密码逐次 +1，第 F 次后锁定拒绝且**不继续
%% 认证**（verify 不再被调用）、错误密码不泄露锁定前计数。反例：若
%% 实现回退为旧空 IP 断链，计数恒 0，第一次 +1 断言即变红；若锁定门
%% 丢失，第 F+1 次 verify 计数断言变红。
limiter_password_lock_after_f_test_() ->
    FindEmail = fun
        (?L0_ID) -> l3_user_map();
        (_) -> #{}
    end,
    FindAccount = fun
        (?L0_ACCT) -> l3_user_map();
        (_) -> #{}
    end,
    ?WITH_MECKS(
        l3_base_mocks(FindEmail, FindAccount) ++ [l3_pwd_mocks({error, <<"密码有误"/utf8>>})],
        fun() ->
            ok = ensure_l3_ds(),
            try
                lists:foreach(
                    fun(N) ->
                        ?assertMatch(
                            {error, <<"密码有误"/utf8>>},
                            passport_logic:do_login(<<"email">>, ?L0_ID, <<"wrong">>)
                        ),
                        {ok, Snap} = login_attempt_ds:snapshot(?L3_ID_USER),
                        ?assertEqual(N, maps:get(count, Snap))
                    end,
                    lists:seq(1, 5)
                ),
                {ok, Snap5} = login_attempt_ds:snapshot(?L3_ID_USER),
                ?assertEqual(true, maps:get(locked, Snap5)),
                persistent_term:put({?MODULE, l3_verified}, 0),
                try
                    ok = meck:expect(elib_password, verify, fun(_Pwd, _Hash) ->
                        persistent_term:put({?MODULE, l3_verified}, 1),
                        {ok, true}
                    end),
                    %% 第 F+1 次：锁定拒绝（含正确密码也不认证）
                    Result = passport_logic:do_login(
                        <<"email">>, ?L0_ID, <<"right-pwd">>, <<"android">>, <<"l3-did">>
                    ),
                    ?assertEqual({error, ?L3_LOCK_MSG}, Result),
                    ?assertEqual(0, persistent_term:get({?MODULE, l3_verified}, 0)),
                    {ok, Snap} = login_attempt_ds:snapshot(?L3_ID_USER),
                    ?assertEqual(5, maps:get(count, Snap))
                after
                    persistent_term:erase({?MODULE, l3_verified})
                end,
                ok
            after
                cleanup_l3_ds()
            end
        end
    ).

%% AC-19 导出兼容：login/3、do_login/3、do_login/5、verify_user/3 各走
%% 成功/失败；计数只发生在 do_login_verify 层（一次失败 = +1），直调
%% verify_user 不触发计数。反例：若低层另加重复计数，count 步进断言
%% （+1 而非 +2）首先变红；若 verify_user 直调计数，末段 count=0 断言
%% 变红。
limiter_exports_compat_test_() ->
    FindEmail = fun
        (?L0_ID) -> l3_user_map();
        (_) -> #{}
    end,
    FindAccount = fun
        (?L0_ACCT) -> l3_user_map();
        (_) -> #{}
    end,
    ?WITH_MECKS(
        l3_base_mocks(FindEmail, FindAccount) ++ [l3_pwd_mocks({ok, true})],
        fun() ->
            ok = ensure_l3_ds(),
            try
                %% login/3：输入是合法 email → type=email；未知账户查无用户
                %% 仍计失败（C.2，verify 不被触达）= 计数 +1，返回兼容
                %% {error, Msg, 400}。elib_type 未 mock（真实判定）。
                ?assertMatch(
                    {error, <<"账号不存在"/utf8>>, 400},
                    passport_logic:login(<<"unknown-a@e2e.test">>, <<"whatever">>, #{
                        <<"did">> => <<"d1">>
                    })
                ),
                {ok, Snap1} = login_attempt_ds:snapshot(
                    {unknown, email, <<"unknown-a@e2e.test">>}
                ),
                ?assertEqual(1, maps:get(count, Snap1)),
                %% 已知用户：do_login/5 失败一次 = +1（verify mock 失败路径）
                ok = meck:expect(elib_password, verify, fun(_Pwd, _Hash) ->
                    {error, <<"密码有误"/utf8>>}
                end),
                ?assertMatch(
                    {error, <<"密码有误"/utf8>>},
                    passport_logic:do_login(<<"email">>, ?L0_ID, <<"wrong">>, <<"ios">>, <<"d1">>)
                ),
                {ok, Snap2} = login_attempt_ds:snapshot(?L3_ID_USER),
                ?assertEqual(1, maps:get(count, Snap2)),
                %% do_login/3 成功一次 = reset 清零 + 签发
                ok = meck:expect(elib_password, verify, fun(_Pwd, _Hash) -> {ok, true} end),
                ?assertMatch(
                    {ok, #{<<"token">> := <<"l3-token">>}},
                    passport_logic:do_login(<<"email">>, ?L0_ID, <<"right-pwd">>)
                ),
                {ok, Snap3} = login_attempt_ds:snapshot(?L3_ID_USER),
                ?assertEqual(0, maps:get(count, Snap3)),
                %% login/3 成功（已知用户走 mobile 键）→ 返回兼容 {ok, Data}
                ok = meck:expect(user_ds, find_by_mobile, fun(_M, _F) -> l3_user_map() end),
                ?assertMatch(
                    {ok, #{<<"uid">> := ?L3_UID, <<"token">> := <<"l3-token">>}},
                    passport_logic:login(<<"+8619900010000">>, <<"right-pwd">>, #{
                        <<"did">> => <<"d1">>
                    })
                ),
                %% verify_user/3 直调：失败 + 成功各一次，均**不计数**
                ?assertMatch(
                    {error, <<"账号不存在"/utf8>>},
                    passport_logic:verify_user(<<"x">>, #{}, <<>>)
                ),
                ?assertMatch(
                    {ok, #{<<"uid">> := ?L3_UID}},
                    passport_logic:verify_user(<<"right-pwd">>, l3_user_map(), <<>>)
                ),
                {ok, Snap4} = login_attempt_ds:snapshot(?L3_ID_USER),
                ?assertEqual(0, maps:get(count, Snap4)),
                ok
            after
                cleanup_l3_ds()
            end
        end
    ).
