%% @doc AG31-04B：agent_run FSM 纯域测试（架构合同 §9.3 Frozen AgentRun FSM；
%% AG-A0 裁决 CS-3/CS-4/CS-5/CS-6/CS-7 落地）。
%%
%% 零 mock、零 I/O（铁律 4）：全部为 agent_run_fsm 纯函数断言；
%% 唯一一组用例借 create_run 的**前置校验路径**（DB 访问之前返回）验证
%% immutable context 校验，传入的"连接"永不触达。
-module(agent_run_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% 套件组织
%% ===================================================================

agent_run_fsm_test_() ->
    [
        {"a: eight states exactly; timeout/dispatching are NOT run states", fun() ->
            t_states()
        end},
        {"b: all 16 legal edges accepted (E01-E16)", fun() ->
            t_legal_edges()
        end},
        {"c: sampled illegal edges rejected with illegal_transition", fun() ->
            t_illegal_edges()
        end},
        {"d: terminal states have zero out-edges", fun() ->
            t_terminal_zero_out()
        end},
        {"e: unknown forbids new effects (gate)", fun() ->
            t_unknown_gate()
        end},
        {"f: timeout maps to failed + reason_code=timeout (CS-3)", fun() ->
            t_timeout_mapping()
        end},
        {"g: effect sub-state matrix 11 legal edges (CS-7)", fun() ->
            t_effect_matrix()
        end},
        {"h: attempt cap frozen at 3 (CS-6)", fun() ->
            t_attempt_cap()
        end},
        {"i: per-edge default actor_kind within frozen set (CS-4/CS-5)", fun() ->
            t_actor_kinds()
        end},
        {"j: create_run validates immutable context BEFORE any DB access", fun() ->
            t_ctx_validation()
        end}
    ].

%% ===================================================================
%% a: 八状态枚举
%% ===================================================================

t_states() ->
    States = agent_run_fsm:states(),
    ?assertEqual(8, length(States)),
    ?assertEqual(
        [created, queued, running, waiting_approval, succeeded, failed, cancelled, unknown],
        States
    ),
    %% timeout / dispatching / expired 不是 Run 存储态（§9.3 L451；04A V1）
    ?assertNot(lists:member(timeout, States)),
    ?assertNot(lists:member(dispatching, States)),
    ?assertNot(lists:member(expired, States)),
    %% dispatching 是 agent_effect 的态（§9.4 L507），不是 Run 的态
    ?assert(lists:member(dispatching, agent_run_fsm:effect_states())),
    ok.

%% ===================================================================
%% b: 16 条合法边全过（AG31-04A 64 格矩阵的 legal 侧逐格回归）
%% ===================================================================

t_legal_edges() ->
    Edges = agent_run_fsm:legal_edges(),
    ?assertEqual(16, length(Edges)),
    lists:foreach(
        fun({Id, From, To, _Actor, _Label}) ->
            ?assertMatch({ok, Id, _}, agent_run_fsm:edge_for(From, To)),
            ?assert(agent_run_fsm:transition_allowed(From, To)),
            ?assertEqual(ok, agent_run_fsm:assert_transition(From, To))
        end,
        Edges
    ),
    %% E01-E16 覆盖检查：created→{queued,failed,cancelled}, queued→{running,failed,cancelled},
    %% running→{waiting_approval,succeeded,failed,cancelled,unknown},
    %% waiting_approval→{queued,failed,cancelled}, unknown→{succeeded,failed}
    lists:foreach(
        fun({F, T}) -> ?assert(agent_run_fsm:transition_allowed(F, T)) end,
        [
            {created, queued},
            {created, failed},
            {created, cancelled},
            {queued, running},
            {queued, failed},
            {queued, cancelled},
            {running, waiting_approval},
            {running, succeeded},
            {running, failed},
            {running, cancelled},
            {running, unknown},
            {waiting_approval, queued},
            {waiting_approval, failed},
            {waiting_approval, cancelled},
            {unknown, succeeded},
            {unknown, failed}
        ]
    ),
    ok.

%% ===================================================================
%% c: 抽样非法边拒绝（未列出的边一律非法，§9.3 L450）
%% ===================================================================

t_illegal_edges() ->
    Illegal = [
        %% 终态出边
        {succeeded, succeeded},
        {succeeded, failed},
        {failed, queued},
        {failed, running},
        {cancelled, running},
        {cancelled, unknown},
        %% 自环
        {created, created},
        {running, running},
        {unknown, unknown},
        %% 跨层回跳 / 跳跃
        {running, queued},
        {waiting_approval, running},
        {queued, created},
        {created, running},
        {queued, succeeded},
        {created, succeeded},
        {queued, unknown},
        {waiting_approval, unknown},
        {created, unknown},
        %% unknown 无 cancelled 出口、禁止自动转 running/终态以外路径
        {unknown, cancelled},
        {unknown, queued},
        {unknown, running},
        {unknown, waiting_approval}
    ],
    lists:foreach(
        fun({F, T}) ->
            ?assertMatch(
                {error, illegal_transition}, agent_run_fsm:assert_transition(F, T)
            ),
            ?assertNot(agent_run_fsm:transition_allowed(F, T))
        end,
        Illegal
    ),
    ok.

%% ===================================================================
%% d: 终态零出边
%% ===================================================================

t_terminal_zero_out() ->
    lists:foreach(
        fun(T) ->
            ?assert(agent_run_fsm:is_terminal(T)),
            lists:foreach(
                fun(S) -> ?assertNot(agent_run_fsm:transition_allowed(T, S)) end,
                agent_run_fsm:states()
            )
        end,
        agent_run_fsm:terminal_states()
    ),
    ?assertEqual([succeeded, failed, cancelled], agent_run_fsm:terminal_states()),
    ok.

%% ===================================================================
%% e: unknown 禁止执行新 Effect（§9.3 L450-451）
%% ===================================================================

t_unknown_gate() ->
    ?assertEqual(ok, agent_run_fsm:new_effect_gate(running)),
    ?assertEqual({error, run_not_running}, agent_run_fsm:new_effect_gate(created)),
    ?assertEqual({error, run_not_running}, agent_run_fsm:new_effect_gate(queued)),
    ?assertEqual({error, run_awaiting_approval}, agent_run_fsm:new_effect_gate(waiting_approval)),
    ?assertEqual(
        {error, run_unknown_no_new_effect}, agent_run_fsm:new_effect_gate(unknown)
    ),
    ?assertEqual({error, run_terminal}, agent_run_fsm:new_effect_gate(succeeded)),
    ?assertEqual({error, run_terminal}, agent_run_fsm:new_effect_gate(failed)),
    ?assertEqual({error, run_terminal}, agent_run_fsm:new_effect_gate(cancelled)),
    ok.

%% ===================================================================
%% f: timeout 映射 failed + reason_code=timeout（CS-3；预算值不进 schema）
%% ===================================================================

t_timeout_mapping() ->
    ?assertEqual({failed, timeout}, agent_run_fsm:timeout_failure()),
    %% E05 queued→failed 与 E09 running→failed 是 timeout 的合法载体边
    ?assert(agent_run_fsm:transition_allowed(queued, failed)),
    ?assert(agent_run_fsm:transition_allowed(running, failed)),
    ok.

%% ===================================================================
%% g: agent_effect 子状态迁移矩阵（CS-7）
%% ===================================================================

t_effect_matrix() ->
    Legal = agent_run_fsm:effect_legal_edges(),
    %% created→{denied,waiting_approval,authorized}; waiting_approval→{authorized,denied};
    %% authorized→dispatching; dispatching→{succeeded,failed,unknown}; unknown→{succeeded,failed}
    ?assertEqual(11, length(Legal)),
    ?assertEqual(8, length(agent_run_fsm:effect_states())),
    lists:foreach(
        fun({F, T}) ->
            ?assert(agent_run_fsm:effect_transition_allowed(F, T)),
            ?assertEqual(ok, agent_run_fsm:effect_edge_for(F, T))
        end,
        Legal
    ),
    %% 抽样非法：跳阶段 / 回跳 / 终态出边 / dispatching 自环 / unknown 出口越权
    Illegal = [
        {authorized, succeeded},
        {authorized, failed},
        {created, dispatching},
        {created, succeeded},
        {waiting_approval, dispatching},
        {dispatching, authorized},
        {dispatching, dispatching},
        {dispatching, created},
        {succeeded, unknown},
        {succeeded, failed},
        {failed, unknown},
        {denied, created},
        {denied, authorized},
        {unknown, denied},
        {unknown, authorized},
        {unknown, dispatching},
        {unknown, cancelled},
        {unknown, unknown}
    ],
    lists:foreach(
        fun({F, T}) ->
            ?assertMatch(
                {error, illegal_transition}, agent_run_fsm:effect_edge_for(F, T)
            ),
            ?assertNot(agent_run_fsm:effect_transition_allowed(F, T))
        end,
        Illegal
    ),
    %% effect 终态 denied/succeeded/failed 零出边；unknown 非终态（reconcile 可收敛）
    ?assertEqual([denied, succeeded, failed], agent_run_fsm:effect_terminal_states()),
    lists:foreach(
        fun(T) ->
            lists:foreach(
                fun(S) -> ?assertNot(agent_run_fsm:effect_transition_allowed(T, S)) end,
                agent_run_fsm:effect_states()
            )
        end,
        agent_run_fsm:effect_terminal_states()
    ),
    ok.

%% ===================================================================
%% h: attempt 上限=3（CS-6；§14 无盲重试）
%% ===================================================================

t_attempt_cap() ->
    ?assertEqual(3, agent_run_fsm:max_attempts()),
    ?assertEqual(ok, agent_run_fsm:assert_attempt_below_cap(0)),
    ?assertEqual(ok, agent_run_fsm:assert_attempt_below_cap(1)),
    ?assertEqual(ok, agent_run_fsm:assert_attempt_below_cap(2)),
    ?assertEqual(
        {error, max_attempts_exceeded}, agent_run_fsm:assert_attempt_below_cap(3)
    ),
    ?assertEqual(
        {error, max_attempts_exceeded}, agent_run_fsm:assert_attempt_below_cap(99)
    ),
    ok.

%% ===================================================================
%% i: per-edge 默认 actor（CS-4 值域 / CS-5 逐边语义）
%% ===================================================================

t_actor_kinds() ->
    ActorKinds = agent_run_fsm:actor_kinds(),
    ?assertEqual([human, system, agent], ActorKinds),
    lists:foreach(
        fun({Id, _F, _T, Actor, _Label}) ->
            ?assert(lists:member(Actor, ActorKinds)),
            ?assertMatch({ok, Actor}, agent_run_fsm:edge_default_actor_kind(Id))
        end,
        agent_run_fsm:legal_edges()
    ),
    %% CS-5 语义抽查：cancel/approve 类=human；lease/reconcile/trigger 类=system
    ?assertMatch({ok, human}, agent_run_fsm:edge_default_actor_kind(e03)),
    ?assertMatch({ok, human}, agent_run_fsm:edge_default_actor_kind(e06)),
    ?assertMatch({ok, human}, agent_run_fsm:edge_default_actor_kind(e10)),
    ?assertMatch({ok, human}, agent_run_fsm:edge_default_actor_kind(e12)),
    ?assertMatch({ok, human}, agent_run_fsm:edge_default_actor_kind(e14)),
    ?assertMatch({ok, system}, agent_run_fsm:edge_default_actor_kind(e01)),
    ?assertMatch({ok, system}, agent_run_fsm:edge_default_actor_kind(e04)),
    ?assertMatch({ok, system}, agent_run_fsm:edge_default_actor_kind(e11)),
    ?assertMatch({ok, system}, agent_run_fsm:edge_default_actor_kind(e15)),
    ?assertMatch({ok, system}, agent_run_fsm:edge_default_actor_kind(e16)),
    ok.

%% ===================================================================
%% j: create_run 前置校验（任何 DB 访问之前返回 → "连接"可为任意项且永不触达）
%% ===================================================================

t_ctx_validation() ->
    %% 校验失败路径先于一切 I/O，该 pid 永不被用作连接
    DummyConn = self(),
    Good = base_ctx(),
    %% 坏 trigger_type
    ?assertEqual(
        {error, validation_failed},
        agent_run_command:create_run(DummyConn, Good#{trigger_type => cron})
    ),
    %% 坏 runtime_type
    ?assertEqual(
        {error, validation_failed},
        agent_run_command:create_run(DummyConn, Good#{runtime_type => wasm})
    ),
    %% 缺 organization_id
    ?assertEqual(
        {error, validation_failed},
        agent_run_command:create_run(DummyConn, maps:remove(organization_id, Good))
    ),
    %% 空 context_digest
    ?assertEqual(
        {error, validation_failed},
        agent_run_command:create_run(DummyConn, Good#{context_digest => <<>>})
    ),
    %% grant_version_at_start = 0
    ?assertEqual(
        {error, validation_failed},
        agent_run_command:create_run(DummyConn, Good#{grant_version_at_start => 0})
    ),
    %% workspace_id 非法（负数）
    ?assertEqual(
        {error, validation_failed},
        agent_run_command:create_run(DummyConn, Good#{workspace_id => -1})
    ),
    %% 缺 now
    ?assertEqual(
        {error, validation_failed},
        agent_run_command:create_run(DummyConn, maps:remove(now, Good))
    ),
    ok.

base_ctx() ->
    #{
        agent_id => 11,
        organization_id => 22,
        workspace_id => undefined,
        grant_id => 33,
        grant_version_at_start => 1,
        delegating_principal_id => 44,
        trigger_type => message,
        trigger_id => <<"trigger-1">>,
        runtime_type => mock,
        context_digest => <<"sha256:abc">>,
        idempotency_key => <<"idem-1">>,
        now => {{2026, 9, 16}, {12, 0, 0}}
    }.
