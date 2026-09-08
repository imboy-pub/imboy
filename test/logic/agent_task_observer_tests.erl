-module(agent_task_observer_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%
% agent_task_observer 单测：任务事件→群消息映射 + 审批仲裁 + 可靠性档位 + E2EE 红线。
% DATA-01 起：登记/仲裁真源=DB（migration 00000090 三表），本套件对投递层全 meck
% （imboy_syn/msg_c2g_logic/group_ds/user_logic），DB 走真连接（eunit-local）。
% task_id 必须满足契约格式 ^[A-Za-z0-9_-]{16,64}$ 且每次运行唯一（DB 行跨运行保留，
% 迁移幂等性依赖全新 task_id）。
%%%

%% 契约合法且每次运行唯一的 task_id
uid() ->
    Hex = binary:encode_hex(crypto:strong_rand_bytes(12), lowercase),
    <<"ot_", Hex/binary, "_end">>.

%% ① 过渡态 working → ephemeral 扇出在线成员（不落库），终态通路不触发
working_ephemeral_online_only_test_() ->
    ?TEST_WITH_DB(fun() ->
        Mecks = [
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
        ok = setup_mecks(Mecks),
        try
            lists:foreach(fun erase/1, [{pub, 11}, {pub, 12}, c2g]),
            T = uid(),
            ok = agent_task_observer:emit(#{
                task_id => T,
                agent_uid => 100,
                group_id => 5,
                status => working,
                member_uids => [11, 12],
                e2ee => false
            }),
            ?assertEqual(true, get({pub, 11})),
            ?assertEqual(undefined, get({pub, 12})),
            ?assertEqual(undefined, get(c2g))
        after
            cleanup_mecks([user_logic, imboy_syn, pub, ok, msg_c2g_logic])
        end
    end).

%% ①③ 终态 completed → 可靠群消息（msg_c2g_logic:c2g，落库），不走 ephemeral
%% FSM 边：completed 仅可自 working（edge #7），故先 emit(working) 再 emit(completed)
completed_durable_test_() ->
    ?TEST_WITH_DB(fun() ->
        Mecks = [
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
        ok = setup_mecks(Mecks),
        try
            lists:foreach(fun erase/1, [pub, c2g]),
            T = uid(),
            ok = agent_task_observer:emit(#{
                task_id => T,
                agent_uid => 100,
                group_id => 5,
                status => working,
                e2ee => false
            }),
            ok = agent_task_observer:emit(#{
                task_id => T,
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
            ?assertEqual(T, maps:get(<<"task_id">>, Meta))
        after
            cleanup_mecks([elib_tsid, imboy_syn, ok, msg_c2g_logic])
        end
    end).

%% ③A03 重复终态事件 → 持久层去重，不重复投递 durable 消息
duplicate_completed_no_dup_delivery_test_() ->
    ?TEST_WITH_DB(fun() ->
        Mecks = [
            {imboy_syn, [{'publish', 2, fun(_, _) -> {ok, 1} end}]},
            {msg_c2g_logic, [
                {'c2g', 3, fun(_, _, _) ->
                    put(c2g_count, get(c2g_count, 0) + 1),
                    ok
                end}
            ]}
        ],
        ok = setup_mecks(Mecks),
        try
            erase(c2g_count),
            T = uid(),
            Event = #{
                task_id => T,
                agent_uid => 100,
                group_id => 5,
                status => working,
                e2ee => false
            },
            ok = agent_task_observer:emit(Event),
            Completed = Event#{status => completed},
            ok = agent_task_observer:emit(Completed),
            ok = agent_task_observer:emit(Completed),
            ok = agent_task_observer:emit(Completed),
            %% working 是 ephemeral（不走 c2g）；completed 投递恰 1 次
            ?assertEqual(1, get(c2g_count))
        after
            cleanup_mecks([imboy_syn, ok, msg_c2g_logic])
        end
    end).

get(K, Default) ->
    case get(K) of
        undefined -> Default;
        V -> V
    end.

%% E2EE 红线：e2ee=true → 既不 ephemeral 也不落库群消息（不触 DB）
e2ee_true_skip_test_() ->
    ?TEST_WITH_DB(fun() ->
        Mecks = [
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
        ok = setup_mecks(Mecks),
        try
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
        after
            cleanup_mecks([imboy_syn, ok, msg_c2g_logic])
        end
    end).

%% C2 fail-closed：缺省 e2ee（未声明=未知）→ 同样跳过，绝不"漏投进 E2EE 群"
missing_e2ee_failclosed_test_() ->
    ?TEST_WITH_DB(fun() ->
        Mecks = [
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
        ok = setup_mecks(Mecks),
        try
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
        after
            cleanup_mecks([imboy_syn, ok, msg_c2g_logic])
        end
    end).

%% C1 回归：非枚举 binary status → 不投递、不崩溃、不落库（绝不 binary_to_atom）
unknown_status_no_atom_no_emit_test_() ->
    ?TEST_WITH_DB(fun() ->
        Mecks = [
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
        ok = setup_mecks(Mecks),
        try
            lists:foreach(fun erase/1, [pub, c2g]),
            ok = agent_task_observer:emit(#{
                task_id => <<"t_bogus_high_cardinality_status">>,
                agent_uid => 100,
                group_id => 5,
                status => <<"totally_bogus_high_cardinality_xyz">>,
                member_uids => [11],
                e2ee => false
            }),
            ?assertEqual(undefined, get(pub)),
            ?assertEqual(undefined, get(c2g))
        after
            cleanup_mecks([imboy_syn, ok, msg_c2g_logic])
        end
    end).

%% ②③ awaiting_approval → 可靠审批卡片（含 actions）；状态真源=DB
awaiting_registers_and_cards_test_() ->
    ?TEST_WITH_DB(fun() ->
        Mecks = [
            {elib_tsid, [{'generate', 0, fun() -> 1 end}]},
            {msg_c2g_logic, [
                {'c2g', 3, fun(_, _, Data) ->
                    put(c2g, Data),
                    ok
                end}
            ]}
        ],
        ok = setup_mecks(Mecks),
        try
            erase(c2g),
            T = uid(),
            ok = agent_task_observer:emit(#{
                task_id => T,
                agent_uid => 100,
                group_id => 5,
                status => working,
                e2ee => false
            }),
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
            ?assertEqual([<<"approve">>, <<"reject">>], maps:get(<<"actions">>, Meta)),
            %% 重复 awaiting_approval → 持久层去重，不重复投卡片
            ok = agent_task_observer:emit(#{
                task_id => T,
                agent_uid => 100,
                group_id => 5,
                status => awaiting_approval,
                e2ee => false
            }),
            ?assertMatch({pending, 5, 100}, agent_task_observer:lookup(T))
        after
            cleanup_mecks([elib_tsid, msg_c2g_logic])
        end
    end).

%% ② 审批仲裁：群内有权成员抢先批准 → 通过；后到者幂等 no-op（already_decided）
approve_first_wins_dedup_test_() ->
    ?TEST_WITH_DB(fun() ->
        Mecks = [
            {elib_tsid, [{'generate', 0, fun() -> 1 end}]},
            {msg_c2g_logic, [{'c2g', 3, fun(_, _, _) -> ok end}]},
            {group_ds, [{'member_uids', 1, fun(_) -> [10, 11, 12] end}]}
        ],
        ok = setup_mecks(Mecks),
        try
            T = uid(),
            ok = agent_task_observer:emit(#{
                task_id => T,
                agent_uid => 100,
                group_id => 5,
                status => working,
                e2ee => false
            }),
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
        after
            cleanup_mecks([elib_tsid, msg_c2g_logic, group_ds])
        end
    end).

%% ② 非群成员批准 → not_authorized，任务仍 pending
approve_unauthorized_test_() ->
    ?TEST_WITH_DB(fun() ->
        Mecks = [
            {elib_tsid, [{'generate', 0, fun() -> 1 end}]},
            {msg_c2g_logic, [{'c2g', 3, fun(_, _, _) -> ok end}]},
            {group_ds, [{'member_uids', 1, fun(_) -> [10, 11] end}]}
        ],
        ok = setup_mecks(Mecks),
        try
            T = uid(),
            ok = agent_task_observer:emit(#{
                task_id => T,
                agent_uid => 100,
                group_id => 5,
                status => working,
                e2ee => false
            }),
            ok = agent_task_observer:emit(#{
                task_id => T,
                agent_uid => 100,
                group_id => 5,
                status => awaiting_approval,
                e2ee => false
            }),
            ?assertEqual({error, not_authorized}, agent_task_observer:approve(T, 99)),
            ?assertMatch({pending, _, _}, agent_task_observer:lookup(T))
        after
            cleanup_mecks([elib_tsid, msg_c2g_logic, group_ds])
        end
    end).

%% C3：agent 本人（即便是群成员）不得审批自己的任务 → not_authorized
agent_self_approve_rejected_test_() ->
    ?TEST_WITH_DB(fun() ->
        Mecks = [
            {elib_tsid, [{'generate', 0, fun() -> 1 end}]},
            {msg_c2g_logic, [{'c2g', 3, fun(_, _, _) -> ok end}]},
            %% agent 100 也在成员列表里
            {group_ds, [{'member_uids', 1, fun(_) -> [100, 10] end}]}
        ],
        ok = setup_mecks(Mecks),
        try
            T = uid(),
            ok = agent_task_observer:emit(#{
                task_id => T,
                agent_uid => 100,
                group_id => 5,
                status => working,
                e2ee => false
            }),
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
        after
            cleanup_mecks([elib_tsid, msg_c2g_logic, group_ds])
        end
    end).

%% ② reject 落 rejected 终态
reject_test_() ->
    ?TEST_WITH_DB(fun() ->
        Mecks = [
            {elib_tsid, [{'generate', 0, fun() -> 1 end}]},
            {msg_c2g_logic, [{'c2g', 3, fun(_, _, _) -> ok end}]},
            {group_ds, [{'member_uids', 1, fun(_) -> [10] end}]}
        ],
        ok = setup_mecks(Mecks),
        try
            T = uid(),
            ok = agent_task_observer:emit(#{
                task_id => T,
                agent_uid => 100,
                group_id => 5,
                status => working,
                e2ee => false
            }),
            ok = agent_task_observer:emit(#{
                task_id => T,
                agent_uid => 100,
                group_id => 5,
                status => awaiting_approval,
                e2ee => false
            }),
            ?assertEqual({ok, rejected}, agent_task_observer:reject(T, 10)),
            ?assertEqual({rejected, 10}, agent_task_observer:lookup(T))
        after
            cleanup_mecks([elib_tsid, msg_c2g_logic, group_ds])
        end
    end).

%% ② 审批未登记的任务 → not_found
decide_not_found_test_() ->
    ?TEST_WITH_DB(fun() ->
        Mecks = [
            {group_ds, [{'member_uids', 1, fun(_) -> [10] end}]}
        ],
        ok = setup_mecks(Mecks),
        try
            ?assertEqual(
                {error, not_found},
                agent_task_observer:approve(<<"t_nope_not_a_task_id">>, 10)
            )
        after
            cleanup_mecks([group_ds])
        end
    end).

%% H1：decide 内部异常（group_ds 抛错）被容错为 {error, internal_error}，不穿透
decide_error_contained_test_() ->
    ?TEST_WITH_DB(fun() ->
        Mecks = [
            {elib_tsid, [{'generate', 0, fun() -> 1 end}]},
            {msg_c2g_logic, [{'c2g', 3, fun(_, _, _) -> ok end}]},
            {group_ds, [{'member_uids', 1, fun(_) -> erlang:error(db_down) end}]}
        ],
        ok = setup_mecks(Mecks),
        try
            T = uid(),
            ok = agent_task_observer:emit(#{
                task_id => T,
                agent_uid => 100,
                group_id => 5,
                status => working,
                e2ee => false
            }),
            ok = agent_task_observer:emit(#{
                task_id => T,
                agent_uid => 100,
                group_id => 5,
                status => awaiting_approval,
                e2ee => false
            }),
            ?assertEqual({error, internal_error}, agent_task_observer:approve(T, 10))
        after
            cleanup_mecks([elib_tsid, msg_c2g_logic, group_ds])
        end
    end).

%% ===================================================================
%% meck 辅助（与 DB fixture 共存：TEST_WITH_DB 启动应用后手工 setup/cleanup）
%% ===================================================================

setup_mecks(Mecks) ->
    lists:foreach(
        fun({Module, Expectations}) ->
            case meck_helper:setup_mock(Module, Expectations) of
                {ok, _} -> ok;
                {error, Reason} -> erlang:error({meck_setup, Module, Reason})
            end
        end,
        Mecks
    ),
    ok.

cleanup_mecks(Modules) ->
    lists:foreach(
        fun(M) ->
            try
                meck_helper:cleanup_mock(M)
            catch
                _:_ -> ok
            end
        end,
        Modules
    ),
    ok.
