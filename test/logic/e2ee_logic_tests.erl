-module(e2ee_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

device_key_rotation_logs_do_not_expose_identity_or_error_test_() ->
    Canary = #{uid => 123, secret => <<"synthetic-key-canary">>},
    Cases = [
        {{error, Canary}, {ok, 1}, e2ee_report_device_key_error, {error, <<"internal_error">>}},
        {
            {ok, 0},
            {error, Canary},
            e2ee_report_device_key_create_error,
            {error, <<"internal_error">>}
        },
        {{ok, 0}, {ok, 1}, e2ee_report_device_key_created, {ok, 0}},
        {{ok, 1}, {ok, 1}, e2ee_report_device_key_updated, {ok, 0}}
    ],
    [
        device_key_log_case(Update, Save, Event, Expected)
     || {Update, Save, Event, Expected} <- Cases
    ].

device_key_log_case(Update, Save, Event, Expected) ->
    ?WITH_MECKS(
        [
            {user_device_ds, [
                {update_public_key, 5, fun(_, _, _, _, _) -> Update end},
                {save, 4, fun(_, _, _, _) -> Save end},
                {count_other_device_keys, 2, fun(_, _) -> 0 end}
            ]},
            {friend_ds, [{list_by_uid, 1, fun(_) -> [] end}]},
            {msg_s2c_ds, [{send, 7, fun(_, _, _, _, _, _, _) -> ok end}]},
            {elib_log, [{internal_log, 4, fun(_, _, _, _) -> ok end}]}
        ],
        fun() ->
            ?assertEqual(
                Expected,
                e2ee_logic:report_device_key(
                    123,
                    <<"synthetic-device">>,
                    <<"android">>,
                    undefined,
                    <<"synthetic-public-key">>,
                    <<"synthetic-key-id">>
                )
            ),
            ?assertEqual(1, meck:num_calls(elib_log, internal_log, ['_', Event, '_', '_'])),
            ?assertEqual(1, meck:num_calls(elib_log, internal_log, 4))
        end
    ).

key_query_error_logs_do_not_expose_raw_reason_test_() ->
    Canary = #{uid => 123, secret => <<"synthetic-key-canary">>},
    ?WITH_MECKS(
        [
            {user_device_ds, [{list_public_keys, 1, fun(_) -> {error, Canary} end}]},
            {friend_ds, [{list_by_uid, 1, fun(_) -> [456] end}]},
            {elib_pg, [{query, 2, fun(_, _) -> {error, Canary} end}]},
            {elib_log, [{internal_log, 4, fun(_, _, _, _) -> ok end}]}
        ],
        fun() ->
            ?assertEqual({error, <<"internal_error">>, 500}, e2ee_logic:user_keys(123, 456)),
            ?assertEqual(
                {error, <<"internal_error">>}, e2ee_logic:pull_key_notifications(123, 0, 10)
            ),
            lists:foreach(
                fun(Event) ->
                    ?assertEqual(1, meck:num_calls(elib_log, internal_log, ['_', Event, '_', '_']))
                end,
                [e2ee_user_keys_db_error, pull_key_notifications_db_error]
            ),
            ?assertEqual(2, meck:num_calls(elib_log, internal_log, 4))
        end
    ).

group_failure_logs_do_not_expose_identity_or_raw_result_test_() ->
    Canary = #{uid => 123, gid => 42, secret => <<"synthetic-key-canary">>},
    Cases = [
        {{ok, lists:duplicate(4097, #{})}, e2ee_group_member_keys_limit_exceeded},
        {{error, fanout_limit_exceeded}, e2ee_group_member_count_limit_exceeded},
        {{error, Canary}, e2ee_group_member_snapshot_db_error}
    ],
    [group_failure_log_case(Result, Event, Canary) || {Result, Event} <- Cases].

group_failure_log_case(Result, Event, Canary) ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {member_public_keys_authoritative, 3, fun(_, _, _) -> Result end},
                {authorize_group_history, 3, fun(_, _, _) -> {error, Canary} end}
            ]},
            {elib_log, [{internal_log, 4, fun(_, _, _, _) -> ok end}]}
        ],
        fun() ->
            ?assertMatch({error, _, _}, e2ee_logic:group_member_keys(123, 42)),
            ?assertEqual(
                {error, <<"internal_error">>, 500},
                e2ee_logic:group_history_grant(123, 42, <<"synthetic-session">>)
            ),
            ?assertEqual(
                1, meck:num_calls(elib_log, internal_log, ['_', {Event, failure}, '_', '_'])
            ),
            ?assertEqual(
                1,
                meck:num_calls(
                    elib_log,
                    internal_log,
                    ['_', {e2ee_group_history_grant_invalid_result, failure}, '_', '_']
                )
            ),
            ?assertEqual(2, meck:num_calls(elib_log, internal_log, 4))
        end
    ).

%%%===================================================================
%%% @doc
%%% e2ee_logic 模块的 EUnit 测试
%%%
%%% 目标：验证端到端加密密钥管理功能
%%% 覆盖：用户公钥获取、群成员公钥获取、权限验证
%%%===================================================================

%% ⚠️ eunit 不解释 {Desc, fun} 返回的 {setup,...} spec（探针实证），
%% ?WITH_MECKS 包在 {Desc, fun} 体内 = 静默空转。此 helper 立即执行等价语义：
%% setup → 执行断言 → cleanup，使断言真实生效（simple fun 与 generator 同进程，
%% Self 哨兵可用，无需改进程字典）。
run_with_mocks(MockConfigs, TestFun) ->
    lists:foreach(
        fun({Module, Expectations}) ->
            case meck_helper:setup_mock(Module, Expectations) of
                {ok, _} ->
                    ok;
                {error, Reason} ->
                    erlang:error({mock_setup_failed, Module, Reason})
            end
        end,
        MockConfigs
    ),
    try
        TestFun()
    after
        lists:foreach(
            fun({Module, _}) -> meck_helper:cleanup_mock(Module) end,
            MockConfigs
        )
    end.

%% ===================================================================
%% user_keys/2 测试
%% ===================================================================

user_keys_same_user_success_test_() ->
    ?WITH_MECK(
        user_device_ds,
        [
            {'list_public_keys', 1, fun(_Uid) ->
                {ok, [
                    #{
                        <<"device_id">> => <<"device_1">>,
                        <<"public_key">> => <<"public_key_1">>
                    },
                    #{
                        <<"device_id">> => <<"device_2">>,
                        <<"public_key">> => <<"public_key_2">>
                    }
                ]}
            end}
        ],
        fun() ->
            CurrentUid = 123,
            % 同一个用户
            TargetUid = 123,

            Result = e2ee_logic:user_keys(CurrentUid, TargetUid),
            ?assertMatch(
                {ok, #{
                    <<"uid">> := 123,
                    <<"devices">> := _
                }},
                Result
            )
        end
    ).

user_keys_friend_success_test_() ->
    ?WITH_MECK(
        user_device_ds,
        [
            {'list_public_keys', 1, fun(_Uid) ->
                {ok, [
                    #{
                        <<"device_id">> => <<"device_1">>,
                        <<"public_key">> => <<"MIIBIjANBgkqhkiG9w...">>
                    }
                ]}
            end}
        ],
        fun() ->
            CurrentUid = 123,
            % 好友
            TargetUid = 456,

            Result = e2ee_logic:user_keys(CurrentUid, TargetUid),
            ?assertMatch(
                {ok, #{
                    <<"uid">> := 456,
                    <<"devices">> := [_]
                }},
                Result
            )
        end
    ).

user_keys_non_friend_still_returns_public_keys_test_() ->
    ?WITH_MECK(
        user_device_ds,
        [
            {'list_public_keys', 1, fun(_Uid) ->
                {ok, [
                    #{
                        <<"device_id">> => <<"device_public">>,
                        <<"public_key">> => <<"public_key_non_friend">>
                    }
                ]}
            end}
        ],
        fun() ->
            CurrentUid = 123,
            % 非好友
            TargetUid = 789,

            Result = e2ee_logic:user_keys(CurrentUid, TargetUid),
            ?assertMatch(
                {ok, #{
                    <<"uid">> := 789,
                    <<"devices">> := [_]
                }},
                Result
            )
        end
    ).

user_keys_in_denylist_still_returns_public_keys_test_() ->
    ?WITH_MECK(
        user_device_ds,
        [
            {'list_public_keys', 1, fun(_Uid) ->
                {ok, [
                    #{
                        <<"device_id">> => <<"device_denylist">>,
                        <<"public_key">> => <<"public_key_denylist">>
                    }
                ]}
            end}
        ],
        fun() ->
            CurrentUid = 123,
            % 在黑名单
            TargetUid = 999,

            Result = e2ee_logic:user_keys(CurrentUid, TargetUid),
            ?assertMatch(
                {ok, #{
                    <<"uid">> := 999,
                    <<"devices">> := [_]
                }},
                Result
            )
        end
    ).

user_keys_database_error_returns_500_test_() ->
    ?WITH_MECK(
        user_device_ds,
        [
            {'list_public_keys', 1, fun(_Uid) ->
                {error, database_connection_lost}
            end}
        ],
        fun() ->
            CurrentUid = 123,
            % 同一个用户
            TargetUid = 123,

            Result = e2ee_logic:user_keys(CurrentUid, TargetUid),
            ?assertEqual({error, <<"internal_error">>, 500}, Result)
        end
    ).

user_keys_with_multiple_devices_test_() ->
    ?WITH_MECK(
        user_device_ds,
        [
            {'list_public_keys', 1, fun(_Uid) ->
                {ok, [
                    #{
                        <<"device_id">> => <<"ios_device">>,
                        <<"public_key">> => <<"ios_public_key">>,
                        <<"device_type">> => <<"ios">>
                    },
                    #{
                        <<"device_id">> => <<"android_device">>,
                        <<"public_key">> => <<"android_public_key">>,
                        <<"device_type">> => <<"android">>
                    },
                    #{
                        <<"device_id">> => <<"web_device">>,
                        <<"public_key">> => <<"web_public_key">>,
                        <<"device_type">> => <<"web">>
                    }
                ]}
            end}
        ],
        fun() ->
            CurrentUid = 123,
            TargetUid = 123,

            Result = e2ee_logic:user_keys(CurrentUid, TargetUid),
            ?assertMatch(
                {ok, #{
                    <<"uid">> := 123,
                    <<"devices">> := _
                }},
                Result
            ),
            {ok, #{<<"devices">> := Devices}} = Result,
            ?assertEqual(3, length(Devices))
        end
    ).

%% ===================================================================
%% group_member_keys/2 测试
%% ===================================================================

group_member_keys_member_success_test_() ->
    ?WITH_MECK(
        group_ds,
        [
            {'member_public_keys_authoritative', 3, fun(_Gid, _CurrentUid, 4097) ->
                {ok, [
                    #{
                        <<"user_id">> => 123,
                        <<"device_id">> => <<"device_1">>,
                        <<"public_key">> => <<"key_1">>
                    },
                    #{
                        <<"user_id">> => 456,
                        <<"device_id">> => <<"device_2">>,
                        <<"public_key">> => <<"key_2">>
                    },
                    #{
                        <<"user_id">> => 789,
                        <<"device_id">> => <<"device_3">>,
                        <<"public_key">> => <<"key_3">>
                    }
                ]}
            end}
        ],
        fun() ->
            CurrentUid = 123,
            Gid = 1,
            Result = e2ee_logic:group_member_keys(CurrentUid, Gid),
            ?assertMatch({ok, #{<<"gid">> := 1, <<"members">> := _}}, Result),
            {ok, #{<<"members">> := Members}} = Result,
            ?assertEqual(3, length(Members))
        end
    ).

group_member_keys_non_member_forbidden_test_() ->
    ?WITH_MECK(
        group_ds,
        [
            {'member_public_keys_authoritative', 3, fun(_Gid, _CurrentUid, 4097) ->
                {error, forbidden}
            end}
        ],
        fun() ->
            CurrentUid = 123,
            % 不是群成员
            Gid = 999,

            Result = e2ee_logic:group_member_keys(CurrentUid, Gid),
            ?assertEqual({error, <<"forbidden">>, 403}, Result)
        end
    ).

group_member_keys_database_error_returns_500_test_() ->
    ?WITH_MECK(
        group_ds,
        [
            {'member_public_keys_authoritative', 3, fun(_Gid, _CurrentUid, 4097) ->
                {error, database_timeout}
            end}
        ],
        fun() ->
            Result = e2ee_logic:group_member_keys(123, 1),
            ?assertEqual({error, <<"internal_error">>, 500}, Result)
        end
    ).

group_member_keys_authorized_without_keys_returns_empty_test_() ->
    ?WITH_MECK(
        group_ds,
        [
            {'member_public_keys_authoritative', 3, fun(_Gid, _CurrentUid, 4097) ->
                {ok, []}
            end}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{<<"gid">> => 1, <<"members">> => []}},
                e2ee_logic:group_member_keys(123, 1)
            )
        end
    ).

group_member_keys_limit_boundary_test_() ->
    ?WITH_MECK(
        group_ds,
        [
            {'member_public_keys_authoritative', 3, fun(_Gid, _CurrentUid, 4097) ->
                {ok, [
                    #{<<"user_id">> => 123, <<"device_id">> => integer_to_binary(N)}
                 || N <- lists:seq(1, 4096)
                ]}
            end}
        ],
        fun() ->
            ?assertMatch({ok, _}, e2ee_logic:group_member_keys(123, 1))
        end
    ).

group_member_keys_limit_plus_one_fails_closed_test_() ->
    ?WITH_MECK(
        group_ds,
        [
            {'member_public_keys_authoritative', 3, fun(_Gid, _CurrentUid, 4097) ->
                {ok, [#{<<"user_id">> => 123} || _ <- lists:seq(1, 4097)]}
            end}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"group_key_fanout_limit_exceeded">>, 409},
                e2ee_logic:group_member_keys(123, 1)
            )
        end
    ).

group_member_keys_member_limit_fails_closed_test_() ->
    ?WITH_MECK(
        group_ds,
        [
            {'member_public_keys_authoritative', 3, fun(_Gid, _CurrentUid, 4097) ->
                {error, fanout_limit_exceeded}
            end}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"group_key_fanout_limit_exceeded">>, 409},
                e2ee_logic:group_member_keys(123, 1)
            )
        end
    ).

group_history_grant_returns_authoritative_range_test_() ->
    ?WITH_MECK(
        group_ds,
        [
            {'authorize_group_history', 3, fun(123, 42, <<"session-a">>) ->
                {ok, #{generation_no => 2, start_seq => 481, end_seq => 900}}
            end}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{
                    <<"gid">> => 42,
                    <<"session_id">> => <<"session-a">>,
                    <<"epoch_id">> => <<"session-a">>,
                    <<"generation_no">> => 2,
                    <<"start_seq">> => 481,
                    <<"end_seq">> => 900
                }},
                e2ee_logic:group_history_grant(123, 42, <<"session-a">>)
            )
        end
    ).

group_history_grant_denied_test_() ->
    ?WITH_MECK(
        group_ds,
        [{'authorize_group_history', 3, fun(123, 42, <<"session-a">>) -> {error, denied} end}],
        fun() ->
            ?assertEqual(
                {error, <<"forbidden">>, 403},
                e2ee_logic:group_history_grant(123, 42, <<"session-a">>)
            )
        end
    ).

group_history_grant_invalid_boundary_fails_closed_test_() ->
    ?WITH_MECK(
        group_ds,
        [
            {'authorize_group_history', 3, fun(123, 42, <<"session-a">>) ->
                {ok, #{generation_no => 2, start_seq => 0, end_seq => 900}}
            end}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"internal_error">>, 500},
                e2ee_logic:group_history_grant(123, 42, <<"session-a">>)
            )
        end
    ).

group_history_grant_open_end_fails_closed_test_() ->
    ?WITH_MECK(
        group_ds,
        [
            {'authorize_group_history', 3, fun(123, 42, <<"session-a">>) ->
                {ok, #{generation_no => 2, start_seq => 481, end_seq => null}}
            end}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"internal_error">>, 500},
                e2ee_logic:group_history_grant(123, 42, <<"session-a">>)
            )
        end
    ).

%% ===================================================================
%% group_by_uid/1 测试
%% ===================================================================

group_by_uid_single_device_test_() ->
    Input = [
        #{
            <<"user_id">> => 123,
            <<"device_id">> => <<"device_1">>,
            <<"public_key">> => <<"key_1">>
        }
    ],
    ?TEST_SIMPLE(fun() ->
        Result = e2ee_logic:group_by_uid(Input),
        ?assertMatch([#{<<"uid">> := 123, <<"devices">> := [_]}], Result),
        [#{<<"devices">> := [Device | _]}] = Result,
        ?assertEqual(<<"device_1">>, maps:get(<<"device_id">>, Device)),
        ?assertEqual(<<"key_1">>, maps:get(<<"public_key">>, Device))
    end).

group_by_uid_multiple_devices_same_user_test_() ->
    Input = [
        #{
            <<"user_id">> => 123,
            <<"device_id">> => <<"device_1">>,
            <<"public_key">> => <<"key_1">>
        },
        #{
            <<"user_id">> => 123,
            <<"device_id">> => <<"device_2">>,
            <<"public_key">> => <<"key_2">>
        }
    ],
    ?TEST_SIMPLE(fun() ->
        Result = e2ee_logic:group_by_uid(Input),
        ?assertMatch(
            [
                #{
                    <<"uid">> := 123,
                    <<"devices">> := [_, _]
                }
            ],
            Result
        ),
        [#{<<"devices">> := Devices}] = Result,
        ?assertEqual(2, length(Devices))
    end).

group_by_uid_multiple_users_test_() ->
    Input = [
        #{
            <<"user_id">> => 123,
            <<"device_id">> => <<"device_1">>,
            <<"public_key">> => <<"key_1">>
        },
        #{
            <<"user_id">> => 456,
            <<"device_id">> => <<"device_2">>,
            <<"public_key">> => <<"key_2">>
        }
    ],
    ?TEST_SIMPLE(fun() ->
        Result = e2ee_logic:group_by_uid(Input),
        ?assertMatch([_, _], Result),
        ?assertEqual(2, length(Result))
    end).

group_by_uid_removes_user_id_field_test_() ->
    Input = [
        #{
            <<"user_id">> => 123,
            <<"device_id">> => <<"device_1">>,
            <<"public_key">> => <<"key_1">>
        }
    ],
    ?TEST_SIMPLE(fun() ->
        Result = e2ee_logic:group_by_uid(Input),
        [#{<<"devices">> := [Device | _]}] = Result,
        % user_id 字段应被移除
        ?assertEqual(undefined, maps:get(<<"user_id">>, Device, undefined))
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

user_keys_with_no_devices_test_() ->
    ?WITH_MECK(
        user_device_ds,
        [
            {'list_public_keys', 1, fun(_Uid) ->
                {ok, []}
            end}
        ],
        fun() ->
            CurrentUid = 123,
            TargetUid = 123,

            Result = e2ee_logic:user_keys(CurrentUid, TargetUid),
            ?assertMatch(
                {ok, #{
                    <<"uid">> := 123,
                    <<"devices">> := []
                }},
                Result
            )
        end
    ).

group_member_keys_sorts_by_uid_test_() ->
    ?WITH_MECK(
        group_ds,
        [
            {'member_public_keys_authoritative', 3, fun(_Gid, _CurrentUid, 4097) ->
                {ok, [
                    #{
                        <<"user_id">> => 789,
                        <<"device_id">> => <<"device_3">>,
                        <<"public_key">> => <<"key_3">>
                    },
                    #{
                        <<"user_id">> => 123,
                        <<"device_id">> => <<"device_1">>,
                        <<"public_key">> => <<"key_1">>
                    },
                    #{
                        <<"user_id">> => 456,
                        <<"device_id">> => <<"device_2">>,
                        <<"public_key">> => <<"key_2">>
                    }
                ]}
            end}
        ],
        fun() ->
            Result = e2ee_logic:group_member_keys(123, 1),
            {ok, #{<<"members">> := Members}} = Result,
            Uids = [maps:get(<<"uid">>, M) || M <- Members],
            ?assert(Uids =:= lists:sort(Uids))
        end
    ).

%% ===================================================================
%% pull_key_notifications/3 limit 夹取回归
%% ===================================================================

%% 回归：未受信 limit 必须夹到 [1,1000]，避免负值经 SQL LIMIT 触发
%% PG "LIMIT must not be negative" → 500，以及超大 limit 的无界查询。
pull_key_notifications_clamps_limit_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {list_by_uid, 1, fun(_Uid) -> [1002] end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, [_Friends, _SinceRfc, Limit]) ->
                    put(captured_limit, Limit),
                    {ok, []}
                end}
            ]}
        ],
        fun() ->
            %% 超大 limit → 夹到 1000
            {ok, _} = e2ee_logic:pull_key_notifications(9999, 0, 999999),
            ?assertEqual(1000, get(captured_limit)),
            %% 负 limit → 夹到 1（否则 PG "LIMIT must not be negative"）
            {ok, _} = e2ee_logic:pull_key_notifications(9999, 0, -5),
            ?assertEqual(1, get(captured_limit)),
            %% 合法 limit → 原样通过
            {ok, _} = e2ee_logic:pull_key_notifications(9999, 0, 50),
            ?assertEqual(50, get(captured_limit))
        end
    ).
