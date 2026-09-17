%%% @doc AG31-04B review 追加：authorize_effect deny 记账行为套件（meck agent_run_pg，
%%% 零 DB）。
%%%
%%% 背景：2e1a4f03 修复 decide_effect deny 分支——此前 insert_decided 的结果被丢弃，
%%% 授权/审计持久化失败会被静默吞成 {error, denied_by_policy}（违反架构 §14 fail
%%% closed：审计行未落地却对上层呈现"已按策略拒绝"）。修复后持久化失败必须原样
%%% 暴露。dialyzer 只验证了类型面；真库套件（agent_run_pg_tests）只能覆盖持久化
%%% 成功侧，注入存储失败必须 mock——本套件即为此回归锚：
%%%   * 上层裁决 deny + 持久化成功 → effect 行 denied/denied_by_policy + 上层
%%%     {error, denied_by_policy}；
%%%   * 上层裁决 deny + 持久化失败 → 存储错误原样暴露（2e1a4f03 回归锚）；
%%%   * Grant revoked（实时重检拒绝）+ 持久化成功 → denial_reason=grant_revoked；
%%%   * Grant revoked + 持久化失败 → 同样 fail closed 原样暴露（decide_deny 侧）。
-module(agent_run_command_deny_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PG, agent_run_pg).

%% ===================================================================
%% 套件夹具
%% ===================================================================

deny_accounting_test_() ->
    {foreach,
        fun() ->
            meck:new(?PG, [no_link]),
            ok
        end,
        fun(_) ->
            meck:unload(?PG),
            ok
        end,
        [
            fun upstream_deny_tests/1,
            fun grant_deny_tests/1
        ]}.

upstream_deny_tests(_) ->
    [
        {"upstream deny: denied effect persisted, caller gets denied_by_policy", fun() ->
            given_running_run(),
            meck:expect(?PG, insert_effect_tx, fun(_C, Effect, RunOp) ->
                ?assertEqual(none, RunOp),
                ?assertEqual(denied, maps:get(decided_status, Effect)),
                ?assertEqual(denied_by_policy, maps:get(denial_reason, Effect)),
                ?assertEqual(101, maps:get(run_id, Effect)),
                {ok, 9001, 4}
            end),
            ?assertEqual(
                {error, denied_by_policy},
                agent_run_command:authorize_effect(conn(), 101, ctx(deny))
            ),
            ?assert(meck:called(?PG, insert_effect_tx, '_'))
        end},
        {"upstream deny: persistence failure surfaces (2e1a4f03 fail-closed regression)", fun() ->
            given_running_run(),
            meck:expect(?PG, insert_effect_tx, fun(_C, _Effect, none) ->
                {error, {effect_tx_failed, simulated_pg_down}}
            end),
            ?assertEqual(
                {error, {effect_tx_failed, simulated_pg_down}},
                agent_run_command:authorize_effect(conn(), 101, ctx(deny))
            )
        end}
    ].

grant_deny_tests(_) ->
    [
        {"grant revoked: denial persisted with grant_revoked reason", fun() ->
            given_running_run(),
            meck:expect(?PG, get_grant, fun(_C, 33) -> {ok, grant(revoked)} end),
            meck:expect(?PG, insert_effect_tx, fun(_C, Effect, RunOp) ->
                ?assertEqual(none, RunOp),
                ?assertEqual(denied, maps:get(decided_status, Effect)),
                ?assertEqual(grant_revoked, maps:get(denial_reason, Effect)),
                {ok, 9002, 5}
            end),
            ?assertEqual(
                {error, grant_revoked},
                agent_run_command:authorize_effect(conn(), 101, ctx(allow))
            ),
            ?assert(meck:called(?PG, insert_effect_tx, '_'))
        end},
        {"grant revoked: persistence failure surfaces (decide_deny fail closed)", fun() ->
            given_running_run(),
            meck:expect(?PG, get_grant, fun(_C, 33) -> {ok, grant(revoked)} end),
            meck:expect(?PG, insert_effect_tx, fun(_C, _Effect, none) ->
                {error, {effect_tx_failed, simulated_pg_down}}
            end),
            ?assertEqual(
                {error, {effect_tx_failed, simulated_pg_down}},
                agent_run_command:authorize_effect(conn(), 101, ctx(allow))
            )
        end}
    ].

%% ===================================================================
%% fixtures（连接与 id 均为任意项：meck 层零触达真库）
%% ===================================================================

given_running_run() ->
    meck:expect(?PG, get_run, fun(_C, 101) -> {ok, run()} end),
    meck:expect(?PG, get_grant, fun(_C, 33) -> {ok, grant(active)} end),
    %% maps:get/3 的默认值急切求值：effect_base 即使 Ctx 带 id 也会调 next_id/1
    meck:expect(?PG, next_id, fun
        (agent_effect) -> 9000;
        (agent_run_event) -> 9100
    end),
    ok.

run() ->
    #{
        id => 101,
        version => 3,
        status => running,
        grant_id => 33,
        idempotency_key => <<"idem-1">>,
        agent_id => 11,
        organization_id => 22,
        workspace_id => undefined,
        trigger_type => message
    }.

grant(Status) ->
    #{
        version => 2,
        status => Status,
        valid_from => {{2026, 1, 1}, {0, 0, 0}},
        expires_at => {{2027, 1, 1}, {0, 0, 0}}
    }.

ctx(Decision) ->
    #{
        decision => Decision,
        id => 9000,
        sequence => 1,
        tool_id => <<"tool.echo">>,
        capability => <<"echo">>,
        action => <<"invoke">>,
        resource_digest => <<"sha256:res">>,
        args_digest => <<"sha256:args">>,
        now => {{2026, 9, 17}, {12, 0, 0}}
    }.

%% 永不被用作真连接（agent_run_pg 全 mock）
conn() ->
    self().
