%% @doc AG31-08：Trigger adapter 单元测试——验证序（幂等→Org→Agent→Grant→
%% ws 候选）、webhook 验签门、default workspace seam（禁 min-ID）、错误收敛。
-module(agent_trigger_adapter_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PG, agent_run_pg).
-define(MEMBERSHIP, agent_org_membership_adapter).
-define(ADAPTER, agent_trigger_adapter).

-define(ORG, 341).
-define(AGENT, 342).
-define(GRANT, 343).
-define(RUN_ID, 3440).
-define(NOW, {{2026, 9, 17}, {12, 0, 0}}).

adapter_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_C) ->
        [
            {"全过 → created（Run 行经 create_run 冻结链）", fun t_created/0},
            {"A08 同 source key 重投 → existing（不新建）", fun t_duplicate_delivery/0},
            {"Org 非 active → org_not_active 零 Run（三类 trigger 同拒）", fun t_org_gates/0},
            {"Agent 禁用/非 Agent → 收敛拒绝", fun t_agent_gates/0},
            {"Grant 缺失/失效 → 收敛拒绝", fun t_grant_gates/0},
            {"webhook 未验签 → webhook_unverified（validated 前）", fun t_webhook_gate/0},
            {"webhook 已验签照常走验证序", fun t_webhook_verified_ok/0},
            {"显式 workspace 直接采用", fun t_explicit_ws/0},
            {"默认 ws 模块未配置 → 不启用默认语义（ws=undefined 照建）", fun t_default_ws_unconfigured/0},
            {"默认 ws 模块漂移(not_found) → default_workspace_missing 拒绝", fun t_default_ws_drift/0},
            {"默认 ws 模块返回漂移解析 → default_workspace_missing", fun t_default_ws_error/0},
            {"请求形状非法 → invalid_trigger_request", fun t_bad_request/0}
        ]
    end}.

setup() ->
    meck:new(?PG, [no_link]),
    meck:new(?MEMBERSHIP, [no_link]),
    meck:expect(?PG, next_id, fun(agent_run) -> ?RUN_ID end),
    meck:expect(?MEMBERSHIP, resolve_organization_state, fun(_O) ->
        {ok, #{status => active, version => 5}}
    end),
    meck:expect(?PG, get_agent_identity, fun(_C, _A) ->
        {ok, #{account_type => 1, status => 1}}
    end),
    meck:expect(?PG, get_grant, fun(_C, _G) -> {ok, grant_row()} end),
    meck:expect(?PG, find_run_by_trigger, fun(_C, _A, _O, _T, _Ti, _K) -> {error, not_found} end),
    ok = meck:new(agent_run_command, [no_link]),
    meck:expect(agent_run_command, create_run, fun(_C, _Ctx) ->
        {ok, #{id => ?RUN_ID, status => created}}
    end),
    ok.

teardown(_C) ->
    lists:foreach(
        fun(M) ->
            try
                meck:unload(M)
            catch
                _:_ -> ok
            end
        end,
        [?PG, ?MEMBERSHIP, agent_run_command, ag31_08_default_ws]
    ),
    application:unset_env(imboy, agent_default_workspace_module),
    ok.

grant_row() ->
    #{
        id => ?GRANT,
        version => 1,
        status => active,
        valid_from => {{2026, 9, 1}, {0, 0, 0}},
        expires_at => {{2027, 9, 1}, {0, 0, 0}}
    }.

req() ->
    req(#{}).

req(Over) ->
    maps:merge(
        #{
            trigger_type => message,
            trigger_id => <<"msg-1">>,
            idempotency_key => <<"idem-1">>,
            agent_id => ?AGENT,
            organization_id => ?ORG,
            grant_id => ?GRANT,
            runtime_type => mock,
            context_digest => <<"sha256:ctx">>,
            now => ?NOW,
            conn => conn()
        },
        Over
    ).

conn() ->
    self().

run_row() ->
    #{id => 7788, status => created, agent_id => ?AGENT}.

%% 每用例重置（eunit 组内同进程 inorder：stub/计数跨用例泄漏）
reset_gates() ->
    lists:foreach(
        fun(M) ->
            try
                meck:reset(M)
            catch
                _:_ -> ok
            end
        end,
        [?PG, ?MEMBERSHIP, agent_run_command, ag31_08_default_ws]
    ),
    %% meck:reset 清计数但保留期望——happy path 期望须重设
    meck:expect(?MEMBERSHIP, resolve_organization_state, fun(_O) ->
        {ok, #{status => active, version => 5}}
    end),
    meck:expect(?PG, get_agent_identity, fun(_C, _A) ->
        {ok, #{account_type => 1, status => 1}}
    end),
    meck:expect(?PG, get_grant, fun(_C, _G) -> {ok, grant_row()} end),
    meck:expect(?PG, find_run_by_trigger, fun(_C, _A, _O, _T, _Ti, _K) ->
        {error, not_found}
    end),
    meck:expect(agent_run_command, create_run, fun(_C, _Ctx) ->
        {ok, #{id => ?RUN_ID, status => created}}
    end),
    application:unset_env(imboy, agent_default_workspace_module),
    ok.

%% ------------------------------------------------------------------

t_created() ->
    reset_gates(),
    {ok, created, Run} = ?ADAPTER:start_run(conn(), req()),
    ?assertEqual(?RUN_ID, maps:get(id, Run)),
    ?assertEqual(1, meck:num_calls(agent_run_command, create_run, '_')).

t_duplicate_delivery() ->
    reset_gates(),
    meck:expect(?PG, find_run_by_trigger, fun(_C, _A, _O, _T, _Ti, _K) ->
        {ok, run_row()}
    end),
    {ok, existing, Run} = ?ADAPTER:start_run(conn(), req()),
    ?assertEqual(7788, maps:get(id, Run)),
    ?assertEqual(0, meck:num_calls(agent_run_command, create_run, '_')).

t_org_gates() ->
    reset_gates(),
    meck:expect(?MEMBERSHIP, resolve_organization_state, fun(_O) ->
        {ok, #{status => archived, version => 5}}
    end),
    %% message/schedule：org 门直接拒
    lists:foreach(
        fun(T) ->
            ?assertEqual(
                {error, org_not_active},
                ?ADAPTER:start_run(conn(), req(#{trigger_type => T}))
            )
        end,
        [message, schedule]
    ),
    %% webhook（已验签）同样被 org 门拒——A15 三 trigger 全矩阵零 Run；
    %% 未验签 webhook 在形状门先拒（webhook_unverified 先于 org 门，正确序）
    ?assertEqual(
        {error, org_not_active},
        ?ADAPTER:start_run(conn(), req(#{trigger_type => webhook, verified => true}))
    ),
    ?assertEqual(
        {error, webhook_unverified},
        ?ADAPTER:start_run(conn(), req(#{trigger_type => webhook}))
    ),
    ?assertEqual(0, meck:num_calls(agent_run_command, create_run, '_')).

t_agent_gates() ->
    reset_gates(),
    meck:expect(?PG, get_agent_identity, fun(_C, _A) ->
        {ok, #{account_type => 1, status => 0}}
    end),
    ?assertEqual({error, agent_disabled}, ?ADAPTER:start_run(conn(), req())),
    meck:expect(?PG, get_agent_identity, fun(_C, _A) ->
        {ok, #{account_type => 3, status => 1}}
    end),
    ?assertEqual({error, agent_not_agent}, ?ADAPTER:start_run(conn(), req())),
    meck:expect(?PG, get_agent_identity, fun(_C, _A) -> {error, not_found} end),
    ?assertEqual({error, agent_not_found}, ?ADAPTER:start_run(conn(), req())).

t_grant_gates() ->
    reset_gates(),
    meck:expect(?PG, get_grant, fun(_C, _G) -> {error, not_found} end),
    ?assertEqual({error, grant_not_found}, ?ADAPTER:start_run(conn(), req())),
    meck:expect(?PG, get_grant, fun(_C, _G) ->
        {ok, maps:merge(grant_row(), #{status => revoked})}
    end),
    ?assertEqual({error, grant_not_active}, ?ADAPTER:start_run(conn(), req())).

t_webhook_gate() ->
    reset_gates(),
    Req = req(#{trigger_type => webhook}),
    ?assertEqual({error, webhook_unverified}, ?ADAPTER:start_run(conn(), Req)),
    ?assertEqual(0, meck:num_calls(?MEMBERSHIP, resolve_organization_state, '_')).

t_webhook_verified_ok() ->
    reset_gates(),
    Req = req(#{trigger_type => webhook, verified => true}),
    {ok, created, _Run} = ?ADAPTER:start_run(conn(), Req),
    ?assertEqual(1, meck:num_calls(agent_run_command, create_run, '_')).

t_explicit_ws() ->
    reset_gates(),
    {ok, created, _} = ?ADAPTER:start_run(conn(), req(#{workspace_id => 55})),
    Ctx = meck:capture(first, agent_run_command, create_run, ['_', '_'], 2),
    ?assertEqual(55, maps:get(workspace_id, Ctx)).

t_default_ws_unconfigured() ->
    reset_gates(),
    application:unset_env(imboy, agent_default_workspace_module),
    {ok, created, _} = ?ADAPTER:start_run(conn(), req()),
    Ctx = meck:capture(first, agent_run_command, create_run, ['_', '_'], 2),
    ?assertEqual(undefined, maps:get(workspace_id, Ctx)).

t_default_ws_drift() ->
    reset_gates(),
    ok = meck:new(ag31_08_default_ws, [no_link, non_strict]),
    meck:expect(ag31_08_default_ws, resolve_default_workspace, fun(_O) ->
        {error, not_found}
    end),
    ok = application:set_env(imboy, agent_default_workspace_module, ag31_08_default_ws),
    ?assertEqual({error, default_workspace_missing}, ?ADAPTER:start_run(conn(), req())),
    ?assertEqual(0, meck:num_calls(agent_run_command, create_run, '_')).

t_default_ws_error() ->
    reset_gates(),
    try
        meck:new(ag31_08_default_ws, [no_link, non_strict])
    catch
        error:{already_started, _} -> ok
    end,
    meck:expect(ag31_08_default_ws, resolve_default_workspace, fun(_O) -> erlang:error(boom) end),
    ok = application:set_env(imboy, agent_default_workspace_module, ag31_08_default_ws),
    ?assertEqual({error, default_workspace_missing}, ?ADAPTER:start_run(conn(), req())).

t_bad_request() ->
    reset_gates(),
    ?assertEqual({error, invalid_trigger_request}, ?ADAPTER:start_run(conn(), #{})),
    ?assertEqual(
        {error, invalid_trigger_request}, ?ADAPTER:start_run(conn(), req(#{trigger_type => rss}))
    ),
    ?assertEqual({error, invalid_trigger_request}, ?ADAPTER:start_run(conn(), not_a_map)).
