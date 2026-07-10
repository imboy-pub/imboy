-module(agent_task_observer_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%
% agent_task_observer 单测：任务事件→群消息映射 + 审批仲裁 + 可靠性档位 + E2EE 红线。
% 全 meck 隔离投递层（imboy_syn/msg_c2g_logic/group_ds/user_logic）；用不同 task_id
% 避免公共 ETS 审批表跨用例串味。E2EE 契约 fail-closed：proceeding 事件须显式 e2ee=false。
%%%

%% ① 过渡态 working → ephemeral 扇出在线成员（不落库），终态通路不触发
working_ephemeral_online_only_test_() ->
    ?WITH_MECKS(
        [
            {user_logic, [
                {'is_online', 1, fun
                    (11) -> true;
                    (12) -> false;
                    (_) -> true
                end}
            ]},
            {imboy_syn, [
                {'publish', 2, fun(U, _Json) ->
                    put({pub, U}, true),
                    {ok, 1}
                end}
            ]},
            {msg_c2g_logic, [
                {'c2g', 3, fun(_, _, _) ->
                    put(c2g, true),
                    ok
                end}
            ]}
        ],
        fun() ->
            lists:foreach(fun erase/1, [{pub, 11}, {pub, 12}, c2g]),
            ok = agent_task_observer:emit(#{
                task_id => <<"t_working">>,
                agent_uid => 100,
                group_id => 5,
                status => working,
                member_uids => [11, 12],
                e2ee => false
            }),
            ?assertEqual(true, get({pub, 11})),
            ?assertEqual(undefined, get({pub, 12})),
            ?assertEqual(undefined, get(c2g))
        end
    ).

%% ①③ 终态 completed → 可靠群消息（msg_c2g_logic:c2g，落库），不走 ephemeral
completed_durable_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [{'generate', 0, fun() -> 1 end}]},
            {imboy_syn, [
                {'publish', 2, fun(_, _) ->
                    put(pub, true),
                    {ok, 1}
                end}
            ]},
            {msg_c2g_logic, [
                {'c2g', 3, fun(_, _, Data) ->
                    put(c2g, Data),
                    ok
                end}
            ]}
        ],
        fun() ->
            lists:foreach(fun erase/1, [pub, c2g]),
            ok = agent_task_observer:emit(#{
                task_id => <<"t_done">>,
                agent_uid => 100,
                group_id => 5,
                status => completed,
                text => <<"结果X"/utf8>>,
                e2ee => false
            }),
            ?assertEqual(undefined, get(pub)),
            Data = get(c2g),
            ?assert(is_map(Data)),
            Meta = maps:get(<<"agent_task">>, maps:get(<<"payload">>, Data)),
            ?assertEqual(<<"completed">>, maps:get(<<"status">>, Meta)),
            ?assertEqual(<<"t_done">>, maps:get(<<"task_id">>, Meta))
        end
    ).

%% E2EE 红线：e2ee=true → 既不 ephemeral 也不落库群消息
e2ee_true_skip_test_() ->
    ?WITH_MECKS(
        [
            {imboy_syn, [
                {'publish', 2, fun(_, _) ->
                    put(pub, true),
                    {ok, 1}
                end}
            ]},
            {msg_c2g_logic, [
                {'c2g', 3, fun(_, _, _) ->
                    put(c2g, true),
                    ok
                end}
            ]}
        ],
        fun() ->
            lists:foreach(fun erase/1, [pub, c2g]),
            ok = agent_task_observer:emit(#{
                task_id => <<"t_e2ee">>,
                agent_uid => 100,
                group_id => 5,
                status => working,
                member_uids => [11],
                e2ee => true
            }),
            ?assertEqual(undefined, get(pub)),
            ?assertEqual(undefined, get(c2g))
        end
    ).

%% C2 fail-closed：缺省 e2ee（未声明=未知）→ 同样跳过，绝不"漏投进 E2EE 群"
missing_e2ee_failclosed_test_() ->
    ?WITH_MECKS(
        [
            {imboy_syn, [
                {'publish', 2, fun(_, _) ->
                    put(pub, true),
                    {ok, 1}
                end}
            ]},
            {msg_c2g_logic, [
                {'c2g', 3, fun(_, _, _) ->
                    put(c2g, true),
                    ok
                end}
            ]}
        ],
        fun() ->
            lists:foreach(fun erase/1, [pub, c2g]),
            %% 未带 e2ee 字段
            ok = agent_task_observer:emit(#{
                task_id => <<"t_noe2ee">>,
                agent_uid => 100,
                group_id => 5,
                status => working,
                member_uids => [11]
            }),
            ?assertEqual(undefined, get(pub)),
            ?assertEqual(undefined, get(c2g))
        end
    ).

%% C1 回归：非枚举 binary status → 不投递、不崩溃（绝不 binary_to_atom 耗尽原子表）
unknown_status_no_atom_no_emit_test_() ->
    ?WITH_MECKS(
        [
            {imboy_syn, [
                {'publish', 2, fun(_, _) ->
                    put(pub, true),
                    {ok, 1}
                end}
            ]},
            {msg_c2g_logic, [
                {'c2g', 3, fun(_, _, _) ->
                    put(c2g, true),
                    ok
                end}
            ]}
        ],
        fun() ->
            lists:foreach(fun erase/1, [pub, c2g]),
            ok = agent_task_observer:emit(#{
                task_id => <<"t_bogus">>,
                agent_uid => 100,
                group_id => 5,
                status => <<"totally_bogus_high_cardinality_xyz">>,
                member_uids => [11],
                e2ee => false
            }),
            ?assertEqual(undefined, get(pub)),
            ?assertEqual(undefined, get(c2g))
        end
    ).

%% ②③ awaiting_approval → 原子登记待审 + 可靠审批卡片（含 actions）
awaiting_registers_and_cards_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [{'generate', 0, fun() -> 1 end}]},
            {msg_c2g_logic, [
                {'c2g', 3, fun(_, _, Data) ->
                    put(c2g, Data),
                    ok
                end}
            ]}
        ],
        fun() ->
            erase(c2g),
            T = <<"t_appr">>,
            ok = agent_task_observer:emit(#{
                task_id => T,
                agent_uid => 100,
                group_id => 5,
                status => awaiting_approval,
                e2ee => false
            }),
            ?assertMatch({pending, 5, 100}, agent_task_observer:lookup(T)),
            Data = get(c2g),
            Meta = maps:get(<<"agent_task">>, maps:get(<<"payload">>, Data)),
            ?assertEqual(<<"awaiting_approval">>, maps:get(<<"status">>, Meta)),
            ?assertEqual([<<"approve">>, <<"reject">>], maps:get(<<"actions">>, Meta))
        end
    ).

%% ② 审批仲裁：群内有权成员抢先批准 → 通过；后到者幂等 no-op（already_decided）
approve_first_wins_dedup_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [{'generate', 0, fun() -> 1 end}]},
            {msg_c2g_logic, [{'c2g', 3, fun(_, _, _) -> ok end}]},
            {group_ds, [{'member_uids', 1, fun(_) -> [10, 11, 12] end}]}
        ],
        fun() ->
            T = <<"t_firstwin">>,
            ok = agent_task_observer:emit(#{
                task_id => T,
                agent_uid => 100,
                group_id => 5,
                status => awaiting_approval,
                e2ee => false
            }),
            ?assertEqual({ok, approved}, agent_task_observer:approve(T, 10)),
            ?assertEqual({error, already_decided}, agent_task_observer:approve(T, 11)),
            ?assertEqual({error, already_decided}, agent_task_observer:reject(T, 12)),
            ?assertEqual({approved, 10}, agent_task_observer:lookup(T)),
            %% c2g：卡片1 + 通过定稿1 = 2（后到审批不再投递）
            ?assertEqual(2, meck:num_calls(msg_c2g_logic, c2g, '_'))
        end
    ).

%% ② 非群成员批准 → not_authorized，任务仍 pending
approve_unauthorized_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [{'generate', 0, fun() -> 1 end}]},
            {msg_c2g_logic, [{'c2g', 3, fun(_, _, _) -> ok end}]},
            {group_ds, [{'member_uids', 1, fun(_) -> [10, 11] end}]}
        ],
        fun() ->
            T = <<"t_unauth">>,
            ok = agent_task_observer:emit(#{
                task_id => T,
                agent_uid => 100,
                group_id => 5,
                status => awaiting_approval,
                e2ee => false
            }),
            ?assertEqual({error, not_authorized}, agent_task_observer:approve(T, 99)),
            ?assertMatch({pending, _, _}, agent_task_observer:lookup(T))
        end
    ).

%% C3：agent 本人（即便是群成员）不得审批自己的任务 → not_authorized
agent_self_approve_rejected_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [{'generate', 0, fun() -> 1 end}]},
            {msg_c2g_logic, [{'c2g', 3, fun(_, _, _) -> ok end}]},
            %% agent 100 也在成员列表里
            {group_ds, [{'member_uids', 1, fun(_) -> [100, 10] end}]}
        ],
        fun() ->
            T = <<"t_selfappr">>,
            ok = agent_task_observer:emit(#{
                task_id => T,
                agent_uid => 100,
                group_id => 5,
                status => awaiting_approval,
                e2ee => false
            }),
            %% agent 自批 → 挡下，任务仍 pending
            ?assertEqual({error, not_authorized}, agent_task_observer:approve(T, 100)),
            ?assertMatch({pending, _, _}, agent_task_observer:lookup(T))
        end
    ).

%% ② reject 落 rejected 终态
reject_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [{'generate', 0, fun() -> 1 end}]},
            {msg_c2g_logic, [{'c2g', 3, fun(_, _, _) -> ok end}]},
            {group_ds, [{'member_uids', 1, fun(_) -> [10] end}]}
        ],
        fun() ->
            T = <<"t_reject">>,
            ok = agent_task_observer:emit(#{
                task_id => T,
                agent_uid => 100,
                group_id => 5,
                status => awaiting_approval,
                e2ee => false
            }),
            ?assertEqual({ok, rejected}, agent_task_observer:reject(T, 10)),
            ?assertEqual({rejected, 10}, agent_task_observer:lookup(T))
        end
    ).

%% ② 审批未登记的任务 → not_found
decide_not_found_test_() ->
    ?WITH_MECKS(
        [{group_ds, [{'member_uids', 1, fun(_) -> [10] end}]}],
        fun() ->
            ?assertEqual({error, not_found}, agent_task_observer:approve(<<"t_nope">>, 10))
        end
    ).

%% H1：decide 内部异常（group_ds 抛错）被容错为 {error, internal_error}，不穿透
decide_error_contained_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [{'generate', 0, fun() -> 1 end}]},
            {msg_c2g_logic, [{'c2g', 3, fun(_, _, _) -> ok end}]},
            {group_ds, [{'member_uids', 1, fun(_) -> erlang:error(db_down) end}]}
        ],
        fun() ->
            T = <<"t_err">>,
            ok = agent_task_observer:emit(#{
                task_id => T,
                agent_uid => 100,
                group_id => 5,
                status => awaiting_approval,
                e2ee => false
            }),
            ?assertEqual({error, internal_error}, agent_task_observer:approve(T, 10))
        end
    ).
