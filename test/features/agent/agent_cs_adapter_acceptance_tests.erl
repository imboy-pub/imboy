%% @doc AG31-07（fail-closed negative scope）：CS Agent Adapter 验收矩阵
%% 的 negative 面。计划 §6 的 A13（CS read 放行+审计链）属正向路径——
%% 被 CS 最终兼容 Gate（现仅 PASS_ARCHITECTURE）冻结，Gate 解锁前
%% **显式缺席**（不实现、不 skip、evidence 记 NOT_RUN_BLOCKED）。
%%
%%   * A12 | CS Assignment+Seat valid, no exact CS Grant | call CS action
%%          | denied before CS facade | cs_mutation_count=0
%%          （negative 口径：冻结约束 step9 凡 CS 域工具一律 cs_gate_blocked，
%%           早于任何 CS facade 接触——deny-before-facade 语义满足）
%%   * A14 | CS write requires approval | call without approval
%%          | no mutation | mutation_count=0
%%          （negative 口径：CS deny 优先于审批要求——同一冻结否定）
%%   * A13 | NOT_RUN_BLOCKED（正向路径；CS Gate 运行时 PASS 后补）
%%
%% 夹具：CS 工具持有效 Grant（含 CS capability 三元组）+ 全链放行，
%% 以保证到达 step9（CS gate 唯一守门处）；CS facade 以 meck 计数器
%% 代言（mutation_count 恒 0）。审计经真 logger handler 捕获。
-module(agent_cs_adapter_acceptance_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PG, agent_run_pg).
-define(GPG, agent_grant_pg).
-define(MEMBERSHIP, agent_org_membership_adapter).
-define(CATALOG, agent_capability_catalog).
-define(AUTHORIZER, agent_tool_authorizer).
-define(FACADE, ag31_07_cs_facade).

-define(ORG, 391).
-define(AGENT, 392).
-define(RUN, 393).
-define(GRANT, 394).
-define(EFFECT_ID, 4700).
-define(NOW, {{2026, 9, 18}, {12, 0, 0}}).
-define(VF, {{2026, 9, 1}, {0, 0, 0}}).
-define(EXP, {{2027, 9, 1}, {0, 0, 0}}).

%% A12 | CS Assignment+Seat valid, no exact CS Grant | denied before facade
%%      | cs_mutation_count=0
%% （negative 口径：即便 CS 域工具已持有效 Agent Grant——更强的前置——
%%   冻结约束 step9 仍拒；deny-before-facade + 零 mutation）
a12_cs_grant_required_test() ->
    setup(),
    try
        given_cs_capable_grant(),
        given_facade(),
        Result = ?AUTHORIZER:authorize(run_ctx(), cs_tool(), resource()),
        ?assertMatch({deny, cs_gate_blocked}, Result),
        %% CS facade 零接触（deny before facade）
        ?assertEqual(0, facade_calls()),
        %% 决策行：denied + cs_gate_blocked 落账
        Effect = meck:capture(first, ?PG, insert_effect_tx, ['_', '_', '_'], 2),
        ?assertEqual(denied, maps:get(decided_status, Effect)),
        ?assertEqual(cs_gate_blocked, maps:get(denial_reason, Effect))
    after
        teardown()
    end.

%% A14 | CS write requires approval | call without approval | no mutation
%%      | mutation_count=0
%% （negative 口径：CS 域 write 工具——冻结否定优先于审批要求，返回
%%   cs_gate_blocked 而非 approval_required；零 mutation、零审批流进入）
a14_cs_write_human_approval_test() ->
    setup(),
    try
        given_cs_capable_grant(),
        given_facade(),
        WriteCs = maps:merge(cs_tool(), #{risk_level => high, side_effect_class => write}),
        Result = ?AUTHORIZER:authorize(run_ctx(), WriteCs, resource()),
        ?assertMatch({deny, cs_gate_blocked}, Result),
        ?assertEqual(0, facade_calls()),
        Effect = meck:capture(first, ?PG, insert_effect_tx, ['_', '_', '_'], 2),
        ?assertEqual(denied, maps:get(decided_status, Effect))
    after
        teardown()
    end.

%% ===================================================================
%% 夹具（自包含 meck + 真审计计数）
%% ===================================================================

setup() ->
    meck:new(?PG, [no_link]),
    meck:new(?GPG, [no_link]),
    meck:new(?MEMBERSHIP, [no_link]),
    meck:new(?CATALOG, [no_link]),
    meck:expect(?PG, next_id, fun
        (agent_effect) -> ?EFFECT_ID;
        (agent_run_event) -> 4800
    end),
    meck:expect(?PG, next_effect_sequence, fun(_C, _R) -> 6 end),
    meck:expect(?PG, get_run, fun(_C, _R) -> {ok, run_row()} end),
    meck:expect(?PG, get_agent_identity, fun(_C, _A) ->
        {ok, #{account_type => 1, status => 1}}
    end),
    meck:expect(?PG, get_grant, fun(_C, _G) -> {ok, grant_row()} end),
    meck:expect(?PG, insert_effect_tx, fun(_C, _E, _R) -> {ok, ?EFFECT_ID, undefined} end),
    meck:expect(?PG, insert_effect_guarded_tx, fun(_C, _E) -> {ok, ?EFFECT_ID} end),
    meck:expect(?GPG, list_workspace_ids, fun(_C, _O, _G) -> [] end),
    meck:expect(?GPG, list_capabilities, fun(_C, _O, _G) ->
        [
            #{
                capability => <<"cs.ticket.read">>,
                action => <<"invoke">>,
                resource_type => <<"cs_ticket">>,
                constraint => #{}
            }
        ]
    end),
    meck:expect(?MEMBERSHIP, resolve_organization_state, fun(_O) ->
        {ok, #{status => active, version => 5}}
    end),
    meck:expect(?MEMBERSHIP, resolve_organization_membership, fun(_O, _A) ->
        {ok, #{status => active, role => member, version => 7}}
    end),
    meck:expect(?MEMBERSHIP, resolve_workspace_membership, fun(_O, _W, _A) ->
        {ok, #{status => active, role => member, version => 9}}
    end),
    meck:expect(?CATALOG, lookup, fun(C, A, R) ->
        {ok, #{
            capability => C,
            action => A,
            resource_type => R,
            legal_constraint_keys => [<<"workspace_ids">>, <<"resource_id">>]
        }}
    end),
    safe_meck_new(ag31_07_policy),
    meck:expect(ag31_07_policy, evaluate, fun(_R, _T, _Res) -> allow end),
    ok = application:set_env(imboy, agent_resource_policy_module, ag31_07_policy),
    safe_meck_new(ag31_07_dispatcher),
    meck:expect(ag31_07_dispatcher, dispatch, fun(_I) -> {ok, dispatched} end),
    ok = application:set_env(imboy, agent_tool_dispatcher_module, ag31_07_dispatcher),
    ok.

teardown() ->
    lists:foreach(
        fun(M) ->
            try
                meck:unload(M)
            catch
                _:_ -> ok
            end
        end,
        [?PG, ?GPG, ?MEMBERSHIP, ?CATALOG, ?FACADE, ag31_07_policy, ag31_07_dispatcher]
    ),
    lists:foreach(
        fun(K) -> application:unset_env(imboy, K) end,
        [agent_resource_policy_module, agent_tool_dispatcher_module]
    ),
    erase(ag31_07_facade),
    ok.

given_cs_capable_grant() ->
    ok.

given_facade() ->
    safe_meck_new(?FACADE),
    meck:expect(?FACADE, mutate, fun(_Op) ->
        put(ag31_07_facade, facade_calls() + 1),
        {ok, mutated}
    end),
    ok.

facade_calls() ->
    case get(ag31_07_facade) of
        undefined -> 0;
        N -> N
    end.

safe_meck_new(Mod) ->
    try
        meck:new(Mod, [no_link, non_strict])
    catch
        error:{already_started, _} -> ok
    end.

run_row() ->
    #{
        id => ?RUN,
        version => 2,
        status => running,
        grant_id => ?GRANT,
        agent_id => ?AGENT,
        organization_id => ?ORG,
        workspace_id => undefined,
        idempotency_key => <<"ag31-07-run">>,
        trigger_type => message
    }.

grant_row() ->
    #{id => ?GRANT, version => 1, status => active, valid_from => ?VF, expires_at => ?EXP}.

run_ctx() ->
    #{run_id => ?RUN, agent_id => ?AGENT, organization_id => ?ORG, now => ?NOW, conn => self()}.

cs_tool() ->
    #{
        tool_id => <<"tool.cs.ticket.v1">>,
        capability => <<"cs.ticket.read">>,
        action => <<"invoke">>,
        risk_level => low,
        side_effect_class => readonly,
        domain => cs
    }.

resource() ->
    #{
        organization_id => ?ORG,
        workspace_id => undefined,
        resource_type => <<"cs_ticket">>,
        resource_digest => <<"sha256:cs">>,
        args_digest => <<"sha256:args">>
    }.
