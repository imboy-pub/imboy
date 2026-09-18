%% @doc AG31-06：Native Tool adapter 单元测试——authorize 组合序、handler
%% 恰一/deny 零触、registry seam 默认 fail-closed、sanitize、崩溃 fail closed。
-module(agent_native_tool_adapter_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PG, agent_run_pg).
-define(GPG, agent_grant_pg).
-define(MEMBERSHIP, agent_org_membership_adapter).
-define(CATALOG, agent_capability_catalog).
-define(ADAPTER, agent_native_tool_adapter).

-define(ORG, 301).
-define(AGENT, 302).
-define(RUN, 303).
-define(GRANT, 304).
-define(EFFECT_ID, 3201).
-define(NOW, {{2026, 9, 17}, {12, 0, 0}}).
-define(VF, {{2026, 9, 1}, {0, 0, 0}}).
-define(EXP, {{2027, 9, 1}, {0, 0, 0}}).

adapter_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_Conn) ->
        [
            {"allow -> handler invoked exactly once, sanitized result (零原始载荷)", fun t_allow/0},
            {"authorize deny -> registry/handler 零接触", fun t_deny_zero_touch/0},
            {"approval_required -> handler 零接触", fun t_approval_zero_touch/0},
            {"registry env 未配置(默认) -> deny unknown_tool", fun t_registry_default_deny/0},
            {"registry not_found -> deny unknown_tool", fun t_registry_not_found/0},
            {"duplicate effect 同幂等键重试 -> handler_count=1 + dispatch 0", fun t_duplicate_once/0},
            {"handler crash -> {error, tool_crashed}（授权判定不被改写）", fun t_handler_crash/0},
            {"registry crash -> deny（fail closed）", fun t_registry_crash/0},
            {"result 不含原始载荷字段", fun t_sanitize_shape/0}
        ]
    end}.

%% ------------------------------------------------------------------

setup() ->
    meck:new(?PG, [no_link]),
    meck:new(?GPG, [no_link]),
    meck:new(?MEMBERSHIP, [no_link]),
    meck:new(?CATALOG, [no_link]),
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
        [
            ?PG,
            ?GPG,
            ?MEMBERSHIP,
            ?CATALOG,
            mcp_governance_logic,
            ag31_06_native_registry,
            ag31_06_dispatcher
        ]
    ),
    lists:foreach(
        fun(K) -> application:unset_env(imboy, K) end,
        [
            agent_resource_policy_module,
            agent_tool_dispatcher_module,
            agent_native_tool_registry_module
        ]
    ),
    erase(ag31_06_native_handler),
    ok.

%% authorizer 全关放行（readonly → allow；决策持久化成功）
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
    ok = application:set_env(imboy, agent_tool_dispatcher_module, ag31_06_dispatcher).

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
        idempotency_key => <<"ag31-06-run">>,
        trigger_type => message
    }.

grant_row() ->
    #{id => ?GRANT, version => 1, status => active, valid_from => ?VF, expires_at => ?EXP}.

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
        tool_id => <<"tool.native.demo">>,
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

given_registry(Handler) ->
    safe_meck_new(ag31_06_native_registry),
    meck:expect(ag31_06_native_registry, lookup, fun(_T) -> {ok, Handler} end),
    ok = application:set_env(imboy, agent_native_tool_registry_module, ag31_06_native_registry).

handler_calls() ->
    case get(ag31_06_native_handler) of
        undefined -> 0;
        N -> N
    end.

bump_handler() ->
    put(ag31_06_native_handler, handler_calls() + 1).

ok_handler() ->
    fun(_Res) ->
        bump_handler(),
        {ok, #{<<"secret_payload">> => <<"raw-data">>}}
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
        [?PG, ?GPG, ?MEMBERSHIP, ?CATALOG, ag31_06_native_registry, ag31_06_dispatcher]
    ),
    application:unset_env(imboy, agent_native_tool_registry_module),
    erase(ag31_06_native_handler),
    ok.

%% ------------------------------------------------------------------

t_allow() ->
    reset_gates(),
    given_registry(ok_handler()),
    put(ag31_06_native_handler, 0),
    {ok, Sanitized} = ?ADAPTER:invoke(run_ctx(), tool(), resource()),
    ?assertEqual(1, handler_calls()),
    ?assertEqual(<<"tool.native.demo">>, maps:get(tool_id, Sanitized)),
    ?assertEqual(?EFFECT_ID, maps:get(effect_id, Sanitized)),
    ?assertEqual(ok, maps:get(status, Sanitized)),
    ?assert(is_binary(maps:get(result_digest, Sanitized))).

t_deny_zero_touch() ->
    reset_gates(),
    meck:expect(?PG, get_grant, fun(_C, _G) -> {error, not_found} end),
    given_registry(ok_handler()),
    put(ag31_06_native_handler, 0),
    ?assertEqual({deny, grant_missing}, ?ADAPTER:invoke(run_ctx(), tool(), resource())),
    ?assertEqual(0, handler_calls()),
    ?assertEqual(0, meck:num_calls(ag31_06_native_registry, lookup, '_')).

t_approval_zero_touch() ->
    reset_gates(),
    given_registry(ok_handler()),
    put(ag31_06_native_handler, 0),
    WriteTool = maps:merge(tool(), #{risk_level => medium, side_effect_class => write}),
    {approval_required, _AC} = ?ADAPTER:invoke(run_ctx(), WriteTool, resource()),
    ?assertEqual(0, handler_calls()).

t_registry_default_deny() ->
    reset_gates(),
    application:unset_env(imboy, agent_native_tool_registry_module),
    ?assertEqual({deny, unknown_tool}, ?ADAPTER:invoke(run_ctx(), tool(), resource())).

t_registry_not_found() ->
    reset_gates(),
    safe_meck_new(ag31_06_native_registry),
    meck:expect(ag31_06_native_registry, lookup, fun(_T) -> {error, not_found} end),
    ok = application:set_env(imboy, agent_native_tool_registry_module, ag31_06_native_registry),
    ?assertEqual({deny, unknown_tool}, ?ADAPTER:invoke(run_ctx(), tool(), resource())).

t_duplicate_once() ->
    reset_gates(),
    given_registry(ok_handler()),
    put(ag31_06_native_handler, 0),
    %% 第一次：allow + handler 恰一
    {ok, _} = ?ADAPTER:invoke(run_ctx(), tool(), resource(#{external_idempotency_key => <<"k1">>})),
    ?assertEqual(1, handler_calls()),
    %% 第二次同幂等键：authorizer duplicate_effect deny → handler 不再调
    meck:expect(?PG, insert_effect_guarded_tx, fun(_C, _E) ->
        {error, {duplicate_effect, <<"uq_ae_tool_external_idem">>}}
    end),
    ?assertEqual(
        {deny, duplicate_effect},
        ?ADAPTER:invoke(run_ctx(), tool(), resource(#{external_idempotency_key => <<"k1">>}))
    ),
    ?assertEqual(1, handler_calls()).

t_handler_crash() ->
    reset_gates(),
    given_registry(fun(_Res) ->
        bump_handler(),
        erlang:error(boom)
    end),
    put(ag31_06_native_handler, 0),
    ?assertEqual({error, tool_crashed}, ?ADAPTER:invoke(run_ctx(), tool(), resource())),
    ?assertEqual(1, handler_calls()).

t_registry_crash() ->
    reset_gates(),
    safe_meck_new(ag31_06_native_registry),
    meck:expect(ag31_06_native_registry, lookup, fun(_T) -> erlang:error(reg_boom) end),
    ok = application:set_env(imboy, agent_native_tool_registry_module, ag31_06_native_registry),
    ?assertEqual(
        {deny, {tool_registry_failed, registry_crashed}},
        ?ADAPTER:invoke(run_ctx(), tool(), resource())
    ).

t_sanitize_shape() ->
    reset_gates(),
    given_registry(ok_handler()),
    put(ag31_06_native_handler, 0),
    {ok, Sanitized} = ?ADAPTER:invoke(run_ctx(), tool(), resource()),
    %% 原始载荷键不得透传：返回面恰为四键
    ?assertEqual(undefined, maps:get(<<"secret_payload">>, Sanitized, undefined)),
    ?assertEqual([effect_id, result_digest, status, tool_id], lists:sort(maps:keys(Sanitized))).
