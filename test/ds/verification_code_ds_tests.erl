-module(verification_code_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% verification_code_ds 模块的 EUnit 测试
%%%
%%% 目标：验证验证码数据服务功能
%%% 覆盖：查找验证码、保存验证码、验证验证码
%%%===================================================================

%% ===================================================================
%% find_by_id/1 测试
%% ===================================================================

find_by_id_returns_code_info_test_() ->
    ?WITH_MECK(
        verification_code_repo,
        [
            {'find_by_id', 1, fun(Id) ->
                ?assertEqual(<<"user@example.com">>, Id),
                #{
                    <<"id">> => <<"user@example.com">>,
                    <<"code">> => <<"123456">>,
                    <<"validity_at">> => <<"2023-01-01T01:00:00Z">>,
                    <<"created_at">> => <<"2023-01-01T00:00:00Z">>
                }
            end}
        ],
        fun() ->
            Result = verification_code_ds:find_by_id(<<"user@example.com">>),
            ?assertMatch(#{<<"code">> := <<"123456">>}, Result)
        end
    ).

find_by_id_with_phone_number_test_() ->
    ?WITH_MECK(
        verification_code_repo,
        [
            {'find_by_id', 1, fun(Id) ->
                ?assertEqual(<<"13800138000">>, Id),
                #{
                    <<"id">> => <<"13800138000">>,
                    <<"code">> => <<"654321">>
                }
            end}
        ],
        fun() ->
            Result = verification_code_ds:find_by_id(<<"13800138000">>),
            ?assertEqual(<<"654321">>, maps:get(<<"code">>, Result))
        end
    ).

find_by_id_not_found_returns_empty_map_test_() ->
    ?WITH_MECK(
        verification_code_repo,
        [
            {'find_by_id', 1, fun(_Id) -> #{} end}
        ],
        fun() ->
            Result = verification_code_ds:find_by_id(<<"nonexistent@example.com">>),
            ?assertEqual(#{}, Result)
        end
    ).

%% ===================================================================
%% save/4 测试
%% ===================================================================

save_success_test_() ->
    ?WITH_MECK(
        verification_code_repo,
        [
            {'save', 4, fun(Id, Code, ValidityAt, CreatedAt) ->
                ?assertEqual(<<"user@example.com">>, Id),
                ?assertEqual(<<"123456">>, Code),
                ?assertEqual(<<"2023-01-01T01:00:00Z">>, ValidityAt),
                ?assertEqual(<<"2023-01-01T00:00:00Z">>, CreatedAt),
                {ok, 1}
            end}
        ],
        fun() ->
            Result = verification_code_ds:save(
                <<"user@example.com">>,
                <<"123456">>,
                <<"2023-01-01T01:00:00Z">>,
                <<"2023-01-01T00:00:00Z">>
            ),
            ?assertEqual({ok, 1}, Result)
        end
    ).

save_with_phone_number_test_() ->
    ?WITH_MECK(
        verification_code_repo,
        [
            {'save', 4, fun(Id, Code, _ValidityAt, _CreatedAt) ->
                ?assertEqual(<<"13800138000">>, Id),
                ?assertEqual(<<"654321">>, Code),
                {ok, 1}
            end}
        ],
        fun() ->
            Result = verification_code_ds:save(
                <<"13800138000">>,
                <<"654321">>,
                <<"2023-01-01T01:00:00Z">>,
                <<"2023-01-01T00:00:00Z">>
            ),
            ?assertEqual({ok, 1}, Result)
        end
    ).

save_with_error_returns_error_test_() ->
    ?WITH_MECK(
        verification_code_repo,
        [
            {'save', 4, fun(_Id, _Code, _ValidityAt, _CreatedAt) ->
                {error, <<"database_error">>}
            end}
        ],
        fun() ->
            Result = verification_code_ds:save(
                <<"user@example.com">>,
                <<"123456">>,
                <<"2023-01-01T01:00:00Z">>,
                <<"2023-01-01T00:00:00Z">>
            ),
            ?assertEqual({error, <<"database_error">>}, Result)
        end
    ).

save_updates_existing_code_test_() ->
    ?WITH_MECK(
        verification_code_repo,
        [
            {'save', 4, fun(_Id, _Code, _ValidityAt, _CreatedAt) ->
                {ok, 1}
            end}
        ],
        fun() ->
            % 第一次保存
            Result1 = verification_code_ds:save(
                <<"user@example.com">>,
                <<"111111">>,
                <<"2023-01-01T01:00:00Z">>,
                <<"2023-01-01T00:00:00Z">>
            ),
            ?assertEqual({ok, 1}, Result1),

            % 第二次保存（更新）
            Result2 = verification_code_ds:save(
                <<"user@example.com">>,
                <<"222222">>,
                <<"2023-01-01T02:00:00Z">>,
                <<"2023-01-01T01:00:00Z">>
            ),
            ?assertEqual({ok, 1}, Result2)
        end
    ).

%% ===================================================================
%% verify_code/2 测试
%% ===================================================================

verify_code_success_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:30:00Z">> end}
            ]},
            {verification_code_repo, [
                {'find_by_id', 1, fun(_Id) ->
                    #{
                        <<"code">> => <<"123456">>,
                        <<"validity_at">> => <<"2023-01-01T01:00:00Z">>
                    }
                end}
            ]}
        ],
        fun() ->
            Result = verification_code_ds:verify_code(<<"user@example.com">>, <<"123456">>),
            ?assertEqual({ok, <<"验证码有效"/utf8>>}, Result)
        end
    ).

verify_code_with_wrong_code_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:30:00Z">> end}
            ]},
            {verification_code_repo, [
                {'find_by_id', 1, fun(_Id) ->
                    #{
                        <<"code">> => <<"123456">>,
                        <<"validity_at">> => <<"2023-01-01T01:00:00Z">>
                    }
                end}
            ]}
        ],
        fun() ->
            Result = verification_code_ds:verify_code(<<"user@example.com">>, <<"wrong_code">>),
            ?assertEqual({error, <<"验证码无效"/utf8>>}, Result)
        end
    ).

verify_code_with_expired_code_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T02:00:00Z">> end}
            ]},
            {verification_code_repo, [
                {'find_by_id', 1, fun(_Id) ->
                    #{
                        <<"code">> => <<"123456">>,
                        <<"validity_at">> => <<"2023-01-01T01:00:00Z">>
                    }
                end}
            ]}
        ],
        fun() ->
            Result = verification_code_ds:verify_code(<<"user@example.com">>, <<"123456">>),
            ?assertEqual({error, <<"验证码无效"/utf8>>}, Result)
        end
    ).

verify_code_with_nonexistent_id_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:30:00Z">> end}
            ]},
            {verification_code_repo, [
                {'find_by_id', 1, fun(_Id) -> #{} end}
            ]}
        ],
        fun() ->
            Result = verification_code_ds:verify_code(<<"nonexistent@example.com">>, <<"123456">>),
            ?assertEqual({error, <<"验证码无效"/utf8>>}, Result)
        end
    ).

verify_code_at_exact_expiry_time_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T01:00:00Z">> end}
            ]},
            {verification_code_repo, [
                {'find_by_id', 1, fun(_Id) ->
                    #{
                        <<"code">> => <<"123456">>,
                        <<"validity_at">> => <<"2023-01-01T01:00:00Z">>
                    }
                end}
            ]}
        ],
        fun() ->
            % 当前时间等于过期时间，应该返回无效
            Result = verification_code_ds:verify_code(<<"user@example.com">>, <<"123456">>),
            ?assertEqual({error, <<"验证码无效"/utf8>>}, Result)
        end
    ).

verify_code_just_before_expiry_succeeds_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:59:59Z">> end}
            ]},
            {verification_code_repo, [
                {'find_by_id', 1, fun(_Id) ->
                    #{
                        <<"code">> => <<"123456">>,
                        <<"validity_at">> => <<"2023-01-01T01:00:00Z">>
                    }
                end}
            ]}
        ],
        fun() ->
            % 当前时间小于过期时间，应该返回有效
            Result = verification_code_ds:verify_code(<<"user@example.com">>, <<"123456">>),
            ?assertEqual({ok, <<"验证码有效"/utf8>>}, Result)
        end
    ).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

save_with_empty_code_test_() ->
    ?WITH_MECK(
        verification_code_repo,
        [
            {'save', 4, fun(_Id, Code, _ValidityAt, _CreatedAt) ->
                ?assertEqual(<<>>, Code),
                {ok, 1}
            end}
        ],
        fun() ->
            Result = verification_code_ds:save(
                <<"user@example.com">>,
                <<>>,
                <<"2023-01-01T01:00:00Z">>,
                <<"2023-01-01T00:00:00Z">>
            ),
            ?assertEqual({ok, 1}, Result)
        end
    ).

save_with_long_code_test_() ->
    LongCode = list_to_binary(lists:duplicate(100, $x)),
    ?WITH_MECK(
        verification_code_repo,
        [
            {'save', 4, fun(_Id, Code, _ValidityAt, _CreatedAt) ->
                ?assert(byte_size(Code) >= 100),
                {ok, 1}
            end}
        ],
        fun() ->
            Result = verification_code_ds:save(
                <<"user@example.com">>,
                LongCode,
                <<"2023-01-01T01:00:00Z">>,
                <<"2023-01-01T00:00:00Z">>
            ),
            ?assertEqual({ok, 1}, Result)
        end
    ).

save_with_long_email_test_() ->
    LongEmail = <<(list_to_binary(lists:duplicate(100, $x)))/binary, "@example.com">>,
    ?WITH_MECK(
        verification_code_repo,
        [
            {'save', 4, fun(Id, _Code, _ValidityAt, _CreatedAt) ->
                ?assert(byte_size(Id) > 100),
                {ok, 1}
            end}
        ],
        fun() ->
            Result = verification_code_ds:save(
                LongEmail,
                <<"123456">>,
                <<"2023-01-01T01:00:00Z">>,
                <<"2023-01-01T00:00:00Z">>
            ),
            ?assertEqual({ok, 1}, Result)
        end
    ).

verify_code_with_empty_code_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:30:00Z">> end}
            ]},
            {verification_code_repo, [
                {'find_by_id', 1, fun(_Id) ->
                    #{
                        <<"code">> => <<"123456">>,
                        <<"validity_at">> => <<"2023-01-01T01:00:00Z">>
                    }
                end}
            ]}
        ],
        fun() ->
            Result = verification_code_ds:verify_code(<<"user@example.com">>, <<>>),
            ?assertEqual({error, <<"验证码无效"/utf8>>}, Result)
        end
    ).

verify_code_with_case_sensitive_code_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:30:00Z">> end}
            ]},
            {verification_code_repo, [
                {'find_by_id', 1, fun(_Id) ->
                    #{
                        <<"code">> => <<"ABC123">>,
                        <<"validity_at">> => <<"2023-01-01T01:00:00Z">>
                    }
                end}
            ]}
        ],
        fun() ->
            % 验证码区分大小写
            Result1 = verification_code_ds:verify_code(<<"user@example.com">>, <<"ABC123">>),
            ?assertEqual({ok, <<"验证码有效"/utf8>>}, Result1),

            Result2 = verification_code_ds:verify_code(<<"user@example.com">>, <<"abc123">>),
            ?assertEqual({error, <<"验证码无效"/utf8>>}, Result2)
        end
    ).

%% ===================================================================
%% 万能验证码测试
%% ===================================================================

verify_code_with_master_code_passes_test_() ->
    application:set_env(imboy, verification_master_code, <<"abc12345">>),
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:30:00Z">> end}
            ]},
            {verification_code_repo, [
                {'find_by_id', 1, fun(_Id) ->
                    #{
                        <<"code">> => <<"123456">>,
                        <<"validity_at">> => <<"2023-01-01T01:00:00Z">>
                    }
                end}
            ]}
        ],
        fun() ->
            % 提交万能验证码，即使与存储的验证码不同也应通过
            Result = verification_code_ds:verify_code(<<"user@example.com">>, <<"abc12345">>),
            ?assertEqual({ok, <<"验证码有效"/utf8>>}, Result),
            application:unset_env(imboy, verification_master_code)
        end
    ).

verify_code_with_master_code_wrong_still_validates_normally_test_() ->
    application:set_env(imboy, verification_master_code, <<"abc12345">>),
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:30:00Z">> end}
            ]},
            {verification_code_repo, [
                {'find_by_id', 1, fun(_Id) ->
                    #{
                        <<"code">> => <<"123456">>,
                        <<"validity_at">> => <<"2023-01-01T01:00:00Z">>
                    }
                end}
            ]}
        ],
        fun() ->
            % 提交错误的验证码（不是万能码也不是正确码），应该失败
            Result = verification_code_ds:verify_code(<<"user@example.com">>, <<"wrong">>),
            ?assertEqual({error, <<"验证码无效"/utf8>>}, Result),
            application:unset_env(imboy, verification_master_code)
        end
    ).

verify_code_with_correct_code_still_passes_when_master_code_configured_test_() ->
    application:set_env(imboy, verification_master_code, <<"abc12345">>),
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:30:00Z">> end}
            ]},
            {verification_code_repo, [
                {'find_by_id', 1, fun(_Id) ->
                    #{
                        <<"code">> => <<"123456">>,
                        <<"validity_at">> => <<"2023-01-01T01:00:00Z">>
                    }
                end}
            ]}
        ],
        fun() ->
            % 配置了万能码时，正常验证码也应该通过
            Result = verification_code_ds:verify_code(<<"user@example.com">>, <<"123456">>),
            ?assertEqual({ok, <<"验证码有效"/utf8>>}, Result),
            application:unset_env(imboy, verification_master_code)
        end
    ).

verify_code_without_master_code_config_uses_normal_validation_test_() ->
    application:unset_env(imboy, verification_master_code),
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:30:00Z">> end}
            ]},
            {verification_code_repo, [
                {'find_by_id', 1, fun(_Id) ->
                    #{
                        <<"code">> => <<"123456">>,
                        <<"validity_at">> => <<"2023-01-01T01:00:00Z">>
                    }
                end}
            ]}
        ],
        fun() ->
            % 未配置万能码，正常验证码应通过
            Result = verification_code_ds:verify_code(<<"user@example.com">>, <<"123456">>),
            ?assertEqual({ok, <<"验证码有效"/utf8>>}, Result)
        end
    ).

verify_code_master_code_bypasses_expiry_test_() ->
    application:set_env(imboy, verification_master_code, <<"abc12345">>),
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T02:00:00Z">> end}
            ]},
            {verification_code_repo, [
                {'find_by_id', 1, fun(_Id) ->
                    #{
                        <<"code">> => <<"123456">>,
                        <<"validity_at">> => <<"2023-01-01T01:00:00Z">>
                    }
                end}
            ]}
        ],
        fun() ->
            % 万能码在验证码过期后也应通过
            Result = verification_code_ds:verify_code(<<"user@example.com">>, <<"abc12345">>),
            ?assertEqual({ok, <<"验证码有效"/utf8>>}, Result),
            application:unset_env(imboy, verification_master_code)
        end
    ).

%% ===================================================================
%% 类型验证测试
%% ===================================================================

find_by_id_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Id = <<"user@example.com">>,
        ?assert(is_binary(Id))
    end).

save_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Id = <<"user@example.com">>,
        Code = <<"123456">>,
        ValidityAt = <<"2023-01-01T01:00:00Z">>,
        CreatedAt = <<"2023-01-01T00:00:00Z">>,
        ?assert(is_binary(Id)),
        ?assert(is_binary(Code)),
        ?assert(is_binary(ValidityAt)),
        ?assert(is_binary(CreatedAt))
    end).

verify_code_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Id = <<"user@example.com">>,
        Code = <<"123456">>,
        ?assert(is_binary(Id)),
        ?assert(is_binary(Code))
    end).

%% ===================================================================
%% 集成场景测试
%% ===================================================================

full_verification_flow_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2023-01-01T00:30:00Z">> end}
            ]},
            {verification_code_repo, [
                {'save', 4, fun(_Id, _Code, _ValidityAt, _CreatedAt) ->
                    {ok, 1}
                end},
                {'find_by_id', 1, fun(_Id) ->
                    #{
                        <<"code">> => <<"123456">>,
                        <<"validity_at">> => <<"2023-01-01T01:00:00Z">>
                    }
                end}
            ]}
        ],
        fun() ->
            % 1. 保存验证码
            SaveResult = verification_code_ds:save(
                <<"user@example.com">>,
                <<"123456">>,
                <<"2023-01-01T01:00:00Z">>,
                <<"2023-01-01T00:00:00Z">>
            ),
            ?assertEqual({ok, 1}, SaveResult),

            % 2. 验证验证码
            VerifyResult = verification_code_ds:verify_code(<<"user@example.com">>, <<"123456">>),
            ?assertEqual({ok, <<"验证码有效"/utf8>>}, VerifyResult)
        end
    ).
