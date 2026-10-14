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
%% Limiter V1 L0 缺陷复现（计划 docs/plans/2026-10-13-limiter-v1-*
%% §5.F2/F3、§9.2 AC-02）。真实 login_security_logic + 真实
%% login_attempt_ds（含 ETS），只 mock 存储边界（user_ds /
%% verification_code_ds / token 签发）。oracle 说明：断言**旧实现的坏
%% 结果**（复现成功 = 测试通过）；L3 修复后按 AC-02 翻转 oracle。
%% ===================================================================

-define(L0_ID, <<"limiter-l0@e2e.test">>).
-define(L0_ACCT, <<"limiter-l0-acct">>).
-define(L0_NOW_TS, <<"2026-10-14T00:00:00Z">>).

l0_ensure_ds() ->
    case whereis(login_attempt_ds) of
        undefined -> {ok, _} = login_attempt_ds:start_link();
        _ -> ok
    end,
    ets:delete_all_objects(login_attempt_ets),
    application:set_env(imboy, login_max_attempts, 5),
    application:set_env(imboy, login_lock_duration_minutes, 30),
    ok.

l0_mock_log() ->
    {elib_log, [
        {'internal_log', 4, fun(_Level, _Msg, _Module, _Line) -> ok end}
    ]}.

l0_user_map() ->
    #{
        <<"id">> => 990001,
        <<"password">> => <<"$2a$12$hash">>,
        <<"email">> => ?L0_ID,
        <<"nickname">> => <<"L0">>,
        <<"avatar">> => <<>>,
        <<"account">> => ?L0_ACCT,
        <<"gender">> => 0,
        <<"region">> => <<>>,
        <<"sign">> => <<>>,
        <<"status">> => 1
    }.

l0_user_map(StatusOverride) ->
    (l0_user_map())#{<<"status">> => StatusOverride}.

%% code 路径锁定门所需的共享 mock：验证码消费可观测、用户查询可控。
l0_code_mocks(UserMap, ConsumeFun) ->
    [
        l0_mock_log(),
        {elib_dt, [
            {'now', 0, fun() -> ?L0_NOW_TS end},
            {'add', 2, fun(_From, _Dur) -> <<"2026-10-14T00:30:00Z">> end}
        ]},
        {verification_code_ds, [
            {'consume', 2, ConsumeFun}
        ]},
        {user_ds, [
            {'find_by_email', 2, fun(?L0_ID, _Fields) -> UserMap end},
            {'find_by_account', 2, fun(?L0_ACCT, _Fields) -> UserMap end}
        ]},
        {token_ds, [
            {'encrypt_token', 2, fun(_Uid, _Did) -> <<"l0-token">> end},
            {'encrypt_token', 1, fun(_Uid) -> <<"l0-token">> end},
            {'encrypt_refreshtoken', 2, fun(_Uid, _Did) -> <<"l0-rtoken">> end},
            {'encrypt_refreshtoken', 1, fun(_Uid) -> <<"l0-rtoken">> end}
        ]},
        {user_device_logic, [
            {'validate_device_type', 1, fun(_) -> true end},
            {'check_login_conflict', 2, fun(_, _) -> {ok, no_conflict} end}
        ]},
        {user_log_ds, [
            {'add_internal', 5, fun(_, _, _, _, _) -> ok end}
        ]}
    ].

%% AC-02（F2）旧侧复现：check_login_allowed 的真实返回是三元组
%% {error, locked, LockInfo}，而 do_login_by_code/5 只匹配二元组
%% {error, LockMsg}——锁定结果落到默认分支继续消费验证码。真实 D/S，
%% 锁定状态按计划 §5.F2 以 ETS fixture 注入该入口实际使用的
%% {Account, <<>>} 键（record_failure 拒绝空 IP，真实链无法写入此键，
%% 这本身就是 F1 的另一半证据）。
limiter_code_locked_test_() ->
    {timeout, 30,
        ?WITH_MECKS(
            l0_code_mocks(l0_user_map(), fun(_Id, _Code) -> {ok, <<"verified">>} end),
            fun() ->
                ok = l0_ensure_ds(),
                %% 注入锁定：count 已达 5（阈值），窗口内
                ets:insert(
                    login_attempt_ets,
                    {
                        login_attempt_ds:cache_key(?L0_ID, <<>>),
                        #{<<"count">> => 5, <<"first_fail_at">> => ?L0_NOW_TS}
                    }
                ),
                ?assertEqual(true, login_attempt_ds:is_locked(?L0_ID, <<>>)),
                persistent_term:put({?MODULE, l0_consumed}, 0),
                try
                    ok = meck:expect(verification_code_ds, consume, fun(_Id, _Code) ->
                        persistent_term:put({?MODULE, l0_consumed}, 1),
                        {ok, <<"verified">>}
                    end),
                    Result = passport_logic:do_login_by_code(
                        <<"email">>, ?L0_ID, <<"123456">>, <<"android">>, <<"l0-did">>
                    ),
                    %% L0 旧侧 oracle：锁定门失效——验证码仍被消费，且锁定状态下
                    %% 登录竟然成功
                    ?assertEqual(1, persistent_term:get({?MODULE, l0_consumed}, 0)),
                    ?assertMatch({ok, _}, Result)
                after
                    %% 断言失败也保证清理 VM 级键与 ETS 状态
                    persistent_term:erase({?MODULE, l0_consumed}),
                    ets:delete_all_objects(login_attempt_ets)
                end,
                ok
            end
        )}.

%% F3a 旧侧复现：code 路径的失败计数键是**输入** Account（do_login_by_code
%% 的 check 与 record_login_failure 均用输入值），成功 reset 的键却是
%% **User.account**（do_login_by_code_verify 从查回用户提取）——同一账户
%% 两个别名落两个键，成功登录清不掉别名的失败历史（反之亦然）。
%% 失败历史按计划允许以 ETS fixture 注入（真实 record_failure 因空 IP
%% 断链写不进，见 F1/AC-01）。
limiter_identity_key_fork_test_() ->
    {timeout, 30,
        ?WITH_MECKS(
            l0_code_mocks(l0_user_map(), fun(_Id, _Code) -> {ok, <<"verified">>} end),
            fun() ->
                ok = l0_ensure_ds(),
                %% 以 email 输入失败 4 次后的历史（键 = 输入 email）
                ets:insert(
                    login_attempt_ets,
                    {
                        login_attempt_ds:cache_key(?L0_ID, <<>>),
                        #{<<"count">> => 4, <<"first_fail_at">> => ?L0_NOW_TS}
                    }
                ),
                %% 以同一 email 输入 + 正确验证码成功登录
                Result = passport_logic:do_login_by_code(
                    <<"email">>, ?L0_ID, <<"123456">>, <<"android">>, <<"l0-did">>
                ),
                ?assertMatch({ok, _}, Result),
                %% L0 旧侧 oracle：成功登录后，输入键的失败历史原样残留
                %%（reset 清的是 User.account 键，别名的失败历史不被成功清除）
                ?assertEqual(
                    {ok, 4},
                    login_attempt_ds:get_attempts(?L0_ID, <<>>)
                ),
                ?assertEqual(
                    {ok, 0},
                    login_attempt_ds:get_attempts(?L0_ACCT, <<>>)
                ),
                ets:delete_all_objects(login_attempt_ets),
                ok
            end
        )}.

%% F3b 旧侧复现：code 路径在检查用户 status **之前**就 reset（成功 reset
%% 早于"账户被禁用"判定）——禁用账户的一次失败登录会清掉全部失败历史。
limiter_code_reset_before_status_test_() ->
    {timeout, 30,
        ?WITH_MECKS(
            l0_code_mocks(l0_user_map(0), fun(_Id, _Code) -> {ok, <<"verified">>} end),
            fun() ->
                ok = l0_ensure_ds(),
                %% 键 = 输入 account（与 User.account 相同，隔离键分叉变量）
                ets:insert(
                    login_attempt_ets,
                    {
                        login_attempt_ds:cache_key(?L0_ACCT, <<>>),
                        #{<<"count">> => 3, <<"first_fail_at">> => ?L0_NOW_TS}
                    }
                ),
                %% 有效验证码 + 禁用账户（status=0）
                Result = passport_logic:do_login_by_code(
                    <<"account">>, ?L0_ACCT, <<"123456">>, <<"android">>, <<"l0-did">>
                ),
                ?assertMatch({error, _}, Result),
                %% L0 旧侧 oracle：登录失败，但失败计数已被清零（过早 reset）
                ?assertEqual(
                    {ok, 0},
                    login_attempt_ds:get_attempts(?L0_ACCT, <<>>)
                ),
                ets:delete_all_objects(login_attempt_ets),
                ok
            end
        )}.
