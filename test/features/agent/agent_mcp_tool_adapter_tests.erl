%% @doc AG31-06：MCP Tool adapter 单元测试——双门交集四象限、MCP 治理面
%% 归并、executor seam 默认 fail-closed、sanitize、崩溃 fail closed。
-module(agent_mcp_tool_adapter_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PG, agent_run_pg).
-define(GPG, agent_grant_pg).
-define(MEMBERSHIP, agent_org_membership_adapter).
-define(CATALOG, agent_capability_catalog).
-define(MCPGOV, mcp_governance_logic).
-define(ADAPTER, agent_mcp_tool_adapter).

-define(ORG, 311).
-define(AGENT, 312).
-define(RUN, 313).
-define(GRANT, 314).
-define(EFFECT_ID, 3211).
-define(NOW, {{2026, 9, 17}, {12, 0, 0}}).

adapter_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_Conn) ->
        [
            {"四象限 Agent✓MCP✓ -> executor 恰一 + sanitize", fun t_both_allow/0},
            {"四象限 Agent✗MCP? -> Agent 门先拒，MCP 治理面零调用", fun t_agent_deny_first/0},
            {"四象限 Agent✓MCP✗ -> deny mcp_gate_denied + executor 0", fun t_mcp_deny/0},
            {"四象限 Agent✓MCP 治理面崩溃 -> deny mcp_gate_unavailable", fun t_mcp_crash/0},
            {"executor env 未配置(默认) -> deny mcp_executor_not_configured",
                fun t_executor_default_deny/0},
            {"duplicate effect 重试 -> MCP 门/executor 不再被调", fun t_duplicate_short_circuit/0},
            {"approval_required -> MCP 面零接触", fun t_approval_zero_touch/0},
            {"executor crash -> {error, tool_crashed}", fun t_executor_crash/0}
        ]
    end}.

%% ------------------------------------------------------------------

setup() ->
    meck:new(?PG, [no_link]),
    meck:new(?GPG, [no_link]),
    meck:new(?MEMBERSHIP, [no_link]),
    meck:new(?CATALOG, [no_link]),
    meck:new(?MCPGOV, [no_link]),
    given_all_gates_pass(),
    ok.

teardown(_Conn) ->
    lists:foreach(
        fun(M) ->
            try
                meck:unload(M)
            catch
                _:_ -> ok
            end
        end,
        [?PG, ?GPG, ?MEMBERSHIP, ?CATALOG, ?MCPGOV, ag31_06_mcp_executor]
    ),
    lists:foreach(
        fun(K) -> application:unset_env(imboy, K) end,
        [
            agent_resource_policy_module,
            agent_tool_dispatcher_module,
            agent_mcp_tool_executor_module
        ]
    ),
    ok.

given_all_gates_pass() ->
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
    meck:expect(?GPG, list_capabilities, fun(_C, _O, _G) -> [cap_row(#{})] end),
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
    safe_meck_new(ag31_06_policy_allow),
    meck:expect(ag31_06_policy_allow, evaluate, fun(_R, _T, _Res) -> allow end),
    ok = application:set_env(imboy, agent_resource_policy_module, ag31_06_policy_allow),
    safe_meck_new(ag31_06_dispatcher),
    meck:expect(ag31_06_dispatcher, dispatch, fun(_I) -> {ok, dispatched} end),
    ok = application:set_env(imboy, agent_tool_dispatcher_module, ag31_06_dispatcher),
    %% MCP 治理面默认 allow（enforce-off 语义）
    meck:expect(?MCPGOV, authorize, fun(_OwnerUid, _ToolName) -> allow end).

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
        idempotency_key => <<"ag31-06-mcp">>,
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

cap_row(Constraint) ->
    #{
        capability => <<"demo.read">>,
        action => <<"invoke">>,
        resource_type => <<"demo">>,
        constraint => Constraint
    }.

run_ctx() ->
    #{run_id => ?RUN, agent_id => ?AGENT, organization_id => ?ORG, now => ?NOW, conn => self()}.

tool() ->
    #{
        tool_id => <<"tool.mcp.demo">>,
        capability => <<"demo.read">>,
        action => <<"invoke">>,
        risk_level => low,
        side_effect_class => readonly
    }.

resource() ->
    resource(#{}).

resource(Over) ->
    maps:merge(
        #{
            organization_id => ?ORG,
            workspace_id => undefined,
            resource_type => <<"demo">>,
            resource_digest => <<"sha256:res">>,
            args_digest => <<"sha256:args">>
        },
        Over
    ).

given_executor(Handler) ->
    safe_meck_new(ag31_06_mcp_executor),
    meck:expect(ag31_06_mcp_executor, execute, fun(_N, _A, _C) -> Handler() end),
    ok = application:set_env(imboy, agent_mcp_tool_executor_module, ag31_06_mcp_executor).

executor_calls() ->
    try
        meck:num_calls(ag31_06_mcp_executor, execute, '_')
    catch
        _:_ -> 0
    end.

%% 每用例重置（eunit 组内同进程 inorder：stub/计数/env 会跨用例泄漏）
reset_gates() ->
    given_all_gates_pass(),
    lists:foreach(
        fun(M) ->
            try
                meck:reset(M)
            catch
                _:_ -> ok
            end
        end,
        [?PG, ?GPG, ?MEMBERSHIP, ?CATALOG, ?MCPGOV, ag31_06_mcp_executor, ag31_06_dispatcher]
    ),
    application:unset_env(imboy, agent_mcp_tool_executor_module),
    ok.

%% ------------------------------------------------------------------

t_both_allow() ->
    reset_gates(),
    given_executor(fun() -> {ok, #{<<"mcp_raw">> => <<"payload">>}} end),
    {ok, Sanitized} = ?ADAPTER:invoke(run_ctx(), tool(), resource()),
    ?assertEqual(1, executor_calls()),
    ?assertEqual(<<"tool.mcp.demo">>, maps:get(tool_id, Sanitized)),
    ?assertEqual(?EFFECT_ID, maps:get(effect_id, Sanitized)),
    ?assert(is_binary(maps:get(result_digest, Sanitized))),
    %% MCP 治理面被调恰一次（OwnerUid=agent_id, ToolName=tool_id）
    ?assertEqual(1, meck:num_calls(?MCPGOV, authorize, [?AGENT, <<"tool.mcp.demo">>])).

t_agent_deny_first() ->
    reset_gates(),
    meck:expect(?PG, get_grant, fun(_C, _G) -> {error, not_found} end),
    given_executor(fun() -> {ok, raw} end),
    ?assertEqual({deny, grant_missing}, ?ADAPTER:invoke(run_ctx(), tool(), resource())),
    ?assertEqual(0, meck:num_calls(?MCPGOV, authorize, '_')),
    ?assertEqual(0, executor_calls()).

t_mcp_deny() ->
    reset_gates(),
    given_executor(fun() -> {ok, raw} end),
    meck:expect(?MCPGOV, authorize, fun(_O, _T) -> {deny, <<"client not approved"/utf8>>} end),
    ?assertEqual({deny, mcp_gate_denied}, ?ADAPTER:invoke(run_ctx(), tool(), resource())),
    ?assertEqual(0, executor_calls()).

t_mcp_crash() ->
    reset_gates(),
    meck:expect(?MCPGOV, authorize, fun(_O, _T) -> erlang:error(gov_boom) end),
    ?assertEqual({deny, mcp_gate_unavailable}, ?ADAPTER:invoke(run_ctx(), tool(), resource())).

t_executor_default_deny() ->
    reset_gates(),
    application:unset_env(imboy, agent_mcp_tool_executor_module),
    ?assertEqual(
        {deny, mcp_executor_not_configured}, ?ADAPTER:invoke(run_ctx(), tool(), resource())
    ).

t_duplicate_short_circuit() ->
    reset_gates(),
    given_executor(fun() -> {ok, raw} end),
    {ok, _} = ?ADAPTER:invoke(run_ctx(), tool(), resource(#{external_idempotency_key => <<"m1">>})),
    meck:expect(?PG, insert_effect_guarded_tx, fun(_C, _E) ->
        {error, {duplicate_effect, <<"uq_ae_tool_external_idem">>}}
    end),
    ?assertEqual(
        {deny, duplicate_effect},
        ?ADAPTER:invoke(run_ctx(), tool(), resource(#{external_idempotency_key => <<"m1">>}))
    ),
    %% 恰一执行 + MCP 治理面只在第一次调用
    ?assertEqual(1, executor_calls()),
    ?assertEqual(1, meck:num_calls(?MCPGOV, authorize, '_')).

t_approval_zero_touch() ->
    reset_gates(),
    given_executor(fun() -> {ok, raw} end),
    WriteTool = maps:merge(tool(), #{risk_level => medium, side_effect_class => write}),
    {approval_required, _AC} = ?ADAPTER:invoke(run_ctx(), WriteTool, resource()),
    ?assertEqual(0, meck:num_calls(?MCPGOV, authorize, '_')),
    ?assertEqual(0, executor_calls()).

t_executor_crash() ->
    reset_gates(),
    given_executor(fun() -> erlang:error(exec_boom) end),
    ?assertEqual({error, tool_crashed}, ?ADAPTER:invoke(run_ctx(), tool(), resource())).
