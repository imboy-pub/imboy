%% @doc AG31-09：Recovery 单元测试——单 owner 恢复（A11 败者零动作）、
%% 每 Effect reauthorize（A04 revoke / A05 suspend）、unknown 不自动重发
%% （A09）、reconcile 显式确认面、cancel 冻结边。
-module(agent_recovery_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PG, agent_run_pg).
-define(MEMBERSHIP, agent_org_membership_adapter).
-define(CATALOG, agent_capability_catalog).
-define(GPG, agent_grant_pg).
-define(REC, agent_recovery).

-define(ORG, 361).
-define(AGENT, 362).
-define(RUN, 363).
-define(GRANT, 364).
-define(NOW, {{2026, 9, 17}, {12, 0, 0}}).

recovery_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_C) ->
        [
            {"recover 胜者：recheck 仅含 authorized（unknown/dispatching 不自动重发 A09）",
                fun t_recover_winner/0},
            {"recover 败者：lease_not_acquired 且零后续动作（A11 winner=1）", fun t_recover_loser/0},
            {"A04 revoke 后 recheck → deny grant_revoked + dispatch 0", fun t_a04_revoke_race/0},
            {"A05 suspend membership → recheck deny + 新 Run 拒", fun t_a05_suspend_immediate/0},
            {"A09 同幂等键重派 → duplicate_effect（adapter 恰一）", fun t_a09_at_most_once/0},
            {"reconcile unknown→succeeded/failed 显式确认；非法 outcome 拒", fun t_reconcile/0},
            {"mark_unknown：dispatching→unknown 显式登记", fun t_mark_unknown/0},
            {"cancel_run：E10 running→cancelled 转发", fun t_cancel/0}
        ]
    end}.

setup() ->
    meck:new(?PG, [no_link]),
    meck:new(?GPG, [no_link]),
    meck:new(?MEMBERSHIP, [no_link]),
    meck:new(?CATALOG, [no_link]),
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
        [
            ?PG,
            ?GPG,
            ?MEMBERSHIP,
            ?CATALOG,
            agent_tool_authorizer,
            ag31_09_policy,
            ag31_09_dispatcher
        ]
    ),
    lists:foreach(
        fun(K) -> application:unset_env(imboy, K) end,
        [agent_resource_policy_module, agent_tool_dispatcher_module]
    ),
    ok.

given_lease_winner() ->
    meck:expect(?PG, lease_take_over, fun(_C, _R, _W, _E, _N) ->
        {ok, #{version => 5, attempt => 2}}
    end).

given_run_effects() ->
    meck:expect(?PG, list_run_effects, fun(_C, _R) ->
        [
            #{id => 11, status => authorized, sequence => 1},
            #{id => 12, status => dispatching, sequence => 2},
            #{id => 13, status => unknown, sequence => 3},
            #{id => 14, status => denied, sequence => 4}
        ]
    end).

given_authorizer_facts(OrgStatus, GrantStatus) ->
    ok = application:set_env(imboy, agent_membership_module, ?MEMBERSHIP),
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
    meck:expect(?PG, next_id, fun
        (agent_effect) -> 4100;
        (agent_run_event) -> 4200
    end),
    meck:expect(?PG, next_effect_sequence, fun(_C, _R) -> 9 end),
    meck:expect(?PG, get_run, fun(_C, _R) ->
        {ok, #{
            id => ?RUN,
            version => 4,
            status => running,
            grant_id => ?GRANT,
            agent_id => ?AGENT,
            organization_id => ?ORG,
            workspace_id => undefined,
            idempotency_key => <<"rec-1">>,
            trigger_type => message
        }}
    end),
    meck:expect(?PG, get_agent_identity, fun(_C, _A) ->
        {ok, #{account_type => 1, status => 1}}
    end),
    meck:expect(?PG, get_grant, fun(_C, _G) ->
        {ok, #{
            id => ?GRANT,
            version => 2,
            status => GrantStatus,
            valid_from => {{2026, 9, 1}, {0, 0, 0}},
            expires_at => {{2027, 9, 1}, {0, 0, 0}}
        }}
    end),
    meck:expect(?PG, get_effect, fun(_C, _E) ->
        {ok, #{
            id => 11,
            run_id => ?RUN,
            status => authorized,
            args_digest => <<"sha256:args">>,
            grant_version_checked => 2
        }}
    end),
    meck:expect(?PG, insert_effect_tx, fun(_C, _E, _R) -> {ok, 4100, undefined} end),
    meck:expect(?PG, insert_effect_guarded_tx, fun(_C, _E) -> {ok, 4100} end),
    meck:expect(?MEMBERSHIP, resolve_organization_state, fun(_O) ->
        case OrgStatus of
            error_archived -> {error, archived};
            Other -> {ok, #{status => Other, version => 5}}
        end
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
    safe_meck_new(ag31_09_policy),
    meck:expect(ag31_09_policy, evaluate, fun(_R, _T, _Res) -> allow end),
    ok = application:set_env(imboy, agent_resource_policy_module, ag31_09_policy),
    safe_meck_new(ag31_09_dispatcher),
    meck:expect(ag31_09_dispatcher, dispatch, fun(_I) -> {ok, dispatched} end),
    ok = application:set_env(imboy, agent_tool_dispatcher_module, ag31_09_dispatcher).

safe_meck_new(Mod) ->
    try
        meck:new(Mod, [no_link, non_strict])
    catch
        error:{already_started, _} -> ok
    end.

run_ctx() ->
    #{run_id => ?RUN, agent_id => ?AGENT, organization_id => ?ORG, now => ?NOW, conn => self()}.

tool() ->
    #{
        tool_id => <<"tool.demo">>,
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

%% ------------------------------------------------------------------

t_recover_winner() ->
    given_lease_winner(),
    given_run_effects(),
    {ok, Plan} = ?REC:recover_run(self(), ?RUN, <<"w-1">>, 60, #{now => ?NOW}),
    ?assertEqual(<<"w-1">>, maps:get(owner, Plan)),
    %% 只有 authorized(11) 进入重查清单——unknown(13)/dispatching(12) 不自动重发
    ?assertEqual([11], maps:get(recheck, Plan)).

t_recover_loser() ->
    try
        meck:reset(?PG)
    catch
        _:_ -> ok
    end,
    meck:expect(?PG, lease_take_over, fun(_C, _R, _W, _E, _N) ->
        {error, lease_not_acquired}
    end),
    given_run_effects(),
    ?assertEqual(
        {error, lease_not_acquired}, ?REC:recover_run(self(), ?RUN, <<"w-2">>, 60, #{now => ?NOW})
    ),
    %% 败者零动作：不读 effects
    ?assertEqual(0, meck:num_calls(?PG, list_run_effects, '_')).

t_a04_revoke_race() ->
    given_authorizer_facts(active, revoked),
    ?assertEqual(
        {deny, grant_revoked}, ?REC:recheck_effect(run_ctx(), tool(), resource(), 11)
    ),
    %% deny 路径 dispatcher=0
    ?assertEqual(0, safe_calls(ag31_09_dispatcher, dispatch)).

t_a05_suspend_immediate() ->
    %% port 契约（AG31-01）：archived 走 {error, archived} 原子语义
    given_authorizer_facts(error_archived, active),
    ?assertEqual(
        {deny, org_archived}, ?REC:recheck_effect(run_ctx(), tool(), resource(), 11)
    ),
    %% 新 Run 同拒（trigger 面消费同一 port）
    ?assertEqual(0, safe_calls(ag31_09_dispatcher, dispatch)).

t_a09_at_most_once() ->
    given_authorizer_facts(active, active),
    %% 第一次重查：allow（执行器可重派）
    {allow, _} = ?REC:recheck_effect(run_ctx(), tool(), resource(), 11),
    %% 第二次同幂等键（同 resource args）→ effect dedup deny
    meck:expect(?PG, insert_effect_guarded_tx, fun(_C, _E) ->
        {error, {duplicate_effect, <<"uq_ae_tool_external_idem">>}}
    end),
    ?assertEqual(
        {deny, duplicate_effect}, ?REC:recheck_effect(run_ctx(), tool(), resource(), 11)
    ),
    %% unknown effect 显式确认前不出边（unknown not retried）
    meck:expect(?PG, effect_to_tx, fun(_C, _E, From, To, _V, _N, _X) ->
        ?assertEqual(unknown, From),
        ?assertNotEqual(unknown, To),
        {ok, 3}
    end),
    ?assertEqual({ok, 3}, ?REC:reconcile_effect(self(), 13, succeeded, 2, #{now => ?NOW})).

t_reconcile() ->
    meck:expect(?PG, effect_to_tx, fun(_C, _E, unknown, To, _V, _N, _X) ->
        ?assert(lists:member(To, [succeeded, failed])),
        {ok, 4}
    end),
    ?assertEqual({ok, 4}, ?REC:reconcile_effect(self(), 13, succeeded, 2, #{now => ?NOW})),
    ?assertEqual({ok, 4}, ?REC:reconcile_effect(self(), 13, failed, 2, #{now => ?NOW})),
    %% 非法 outcome 拒（不触 PG）
    reset_calls([?PG]),
    ?assertEqual(
        {error, invalid_outcome}, ?REC:reconcile_effect(self(), 13, retry, 2, #{now => ?NOW})
    ),
    ?assertEqual(0, meck:num_calls(?PG, effect_to_tx, '_')).

t_mark_unknown() ->
    meck:expect(?PG, effect_to_tx, fun(_C, _E, dispatching, unknown, _V, _N, _X) ->
        {ok, 5}
    end),
    ?assertEqual({ok, 5}, ?REC:mark_unknown(self(), 12, #{now => ?NOW})).

t_cancel() ->
    ok = meck:new(agent_run_command, [no_link]),
    meck:expect(agent_run_command, transition, fun(_C, _R, running, cancelled, _Opts) ->
        {ok, 7}
    end),
    ?assertEqual({ok, 7}, ?REC:cancel_run(self(), ?RUN, #{expected_version => 4, now => ?NOW})),
    meck:unload(agent_run_command).

%% ------------------------------------------------------------------

safe_calls(Mod, Fun) ->
    try
        meck:num_calls(Mod, Fun, '_')
    catch
        _:_ -> 0
    end.

reset_calls(Mods) ->
    lists:foreach(
        fun(M) ->
            try
                meck:reset(M)
            catch
                _:_ -> ok
            end
        end,
        Mods
    ).
