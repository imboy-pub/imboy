%% @doc AG31-06：可执行验收矩阵命名测试（实施计划 §6 A16/A17 行——本文件
%% 两个 named test 逐字不可改，是 AG31-06 的验收权威）。
%%
%%   * A16 | MCP client allows, Agent Grant absent | call MCP Tool
%%          | Agent gate denies first | exit=0; MCP dispatch=0
%%   * A17 | Agent Grant allows, MCP client denies | call MCP Tool
%%          | MCP gate still denies | exit=0; handler_count=0
-module(agent_mcp_adapter_acceptance_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PG, agent_run_pg).
-define(GPG, agent_grant_pg).
-define(MEMBERSHIP, agent_org_membership_adapter).
-define(CATALOG, agent_capability_catalog).
-define(MCPGOV, mcp_governance_logic).
-define(ADAPTER, agent_mcp_tool_adapter).

-define(ORG, 321).
-define(AGENT, 322).
-define(RUN, 323).
-define(GRANT, 324).
-define(EFFECT_ID, 3221).
-define(NOW, {{2026, 9, 17}, {12, 0, 0}}).

%% A16 | MCP client allows, Agent Grant absent | call MCP Tool
%%      | Agent gate denies first | MCP dispatch=0
a16_agent_grant_precedes_mcp_test() ->
    setup(),
    try
        %% MCP client 侧放行（治理面 allow）……
        meck:expect(?MCPGOV, authorize, fun(_OwnerUid, _ToolName) -> allow end),
        %% ……但 Agent Grant 缺失
        meck:expect(?PG, get_grant, fun(_C, _G) -> {error, not_found} end),
        given_executor(),
        %% Agent gate denies first：MCP 治理面与 executor 零调用
        ?assertEqual(
            {deny, grant_missing}, ?ADAPTER:invoke(run_ctx(), tool(), resource())
        ),
        ?assertEqual(0, meck:num_calls(?MCPGOV, authorize, '_')),
        ?assertEqual(0, executor_calls())
    after
        teardown()
    end.

%% A17 | Agent Grant allows, MCP client denies | call MCP Tool
%%      | MCP gate still denies | handler_count=0
a17_mcp_grant_still_required_test() ->
    setup(),
    try
        %% Agent 全链放行（Grant 有效 + readonly → allow）……
        %% ……但 MCP client deny（enforce-on 语义：client 未批）
        meck:expect(?MCPGOV, authorize, fun(_OwnerUid, _ToolName) ->
            {deny, <<"client_not_approved">>}
        end),
        given_executor(),
        ?assertEqual(
            {deny, mcp_gate_denied}, ?ADAPTER:invoke(run_ctx(), tool(), resource())
        ),
        ?assertEqual(1, meck:num_calls(?MCPGOV, authorize, '_')),
        %% handler_count=0
        ?assertEqual(0, executor_calls())
    after
        teardown()
    end.

%% ===================================================================
%% 夹具（自包含 meck 实例 + env 自清理）
%% ===================================================================

setup() ->
    meck:new(?PG, [no_link]),
    meck:new(?GPG, [no_link]),
    meck:new(?MEMBERSHIP, [no_link]),
    meck:new(?CATALOG, [no_link]),
    meck:new(?MCPGOV, [no_link]),
    meck:expect(?PG, next_id, fun
        (agent_effect) -> ?EFFECT_ID;
        (agent_run_event) -> 3300
    end),
    meck:expect(?PG, next_effect_sequence, fun(_C, _R) -> 3 end),
    meck:expect(?PG, get_run, fun(_C, _R) -> {ok, run_row()} end),
    meck:expect(?PG, get_agent_identity, fun(_C, _A) ->
        {ok, #{account_type => 1, status => 1}}
    end),
    meck:expect(?PG, get_grant, fun(_C, _G) -> {ok, grant_row()} end),
    meck:expect(?PG, get_effect, fun(_C, _E) -> {error, not_found} end),
    meck:expect(?PG, insert_effect_tx, fun(_C, _E, _R) -> {ok, ?EFFECT_ID, undefined} end),
    meck:expect(?PG, insert_effect_guarded_tx, fun(_C, _E) -> {ok, ?EFFECT_ID} end),
    meck:expect(?GPG, list_workspace_ids, fun(_C, _O, _G) -> [] end),
    meck:expect(?GPG, list_capabilities, fun(_C, _O, _G) ->
        [
            #{
                capability => <<"demo.read">>,
                action => <<"invoke">>,
                resource_type => <<"demo">>,
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
    safe_meck_new(ag31_06_acc_policy),
    meck:expect(ag31_06_acc_policy, evaluate, fun(_R, _T, _Res) -> allow end),
    ok = application:set_env(imboy, agent_resource_policy_module, ag31_06_acc_policy),
    safe_meck_new(ag31_06_acc_dispatcher),
    meck:expect(ag31_06_acc_dispatcher, dispatch, fun(_I) -> {ok, dispatched} end),
    ok = application:set_env(imboy, agent_tool_dispatcher_module, ag31_06_acc_dispatcher).

teardown() ->
    lists:foreach(
        fun(M) ->
            try
                meck:unload(M)
            catch
                _:_ -> ok
            end
        end,
        [
            ?PG,
            ?GPG,
            ?MEMBERSHIP,
            ?CATALOG,
            ?MCPGOV,
            ag31_06_acc_mcp_executor,
            ag31_06_acc_policy,
            ag31_06_acc_dispatcher
        ]
    ),
    lists:foreach(
        fun(K) -> application:unset_env(imboy, K) end,
        [
            agent_resource_policy_module,
            agent_tool_dispatcher_module,
            agent_mcp_tool_executor_module
        ]
    ),
    erase(ag31_06_acc_exec),
    ok.

given_executor() ->
    safe_meck_new(ag31_06_acc_mcp_executor),
    meck:expect(ag31_06_acc_mcp_executor, execute, fun(_N, _A, _C) ->
        put(ag31_06_acc_exec, exec_calls() + 1),
        {ok, #{<<"raw">> => <<"payload">>}}
    end),
    ok = application:set_env(imboy, agent_mcp_tool_executor_module, ag31_06_acc_mcp_executor),
    erase(ag31_06_acc_exec).

exec_calls() ->
    case get(ag31_06_acc_exec) of
        undefined -> 0;
        N -> N
    end.

executor_calls() ->
    exec_calls().

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
        idempotency_key => <<"ag31-06-acc">>,
        trigger_type => message
    }.

grant_row() ->
    #{
        id => ?GRANT,
        version => 1,
        status => active,
        valid_from => {{2026, 9, 1}, {0, 0, 0}},
        expires_at => {{2027, 9, 1}, {0, 0, 0}}
    }.

run_ctx() ->
    #{run_id => ?RUN, agent_id => ?AGENT, organization_id => ?ORG, now => ?NOW, conn => self()}.

tool() ->
    #{
        tool_id => <<"tool.mcp.acc">>,
        capability => <<"demo.read">>,
        action => <<"invoke">>,
        risk_level => low,
        side_effect_class => readonly
    }.

resource() ->
    #{
        organization_id => ?ORG,
        workspace_id => undefined,
        resource_type => <<"demo">>,
        resource_digest => <<"sha256:res">>,
        args_digest => <<"sha256:args">>
    }.
