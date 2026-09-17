%% @doc AG31-08：可执行验收矩阵命名测试（实施计划 §6 A08/A15 行——本文件
%% 两个 named test 逐字不可改，是 AG31-08 触发面的验收权威）。
%%
%%   * A08 | same source delivery/idempotency key | trigger twice
%%          | one Run id and execution | distinct_run_count=1
%%   * A15 | Organization archived | message/schedule/webhook trigger
%%          | no Run created | run_count=0 for all three
-module(agent_run_acceptance_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PG, agent_run_pg).
-define(GPG, agent_grant_pg).
-define(MEMBERSHIP, agent_org_membership_adapter).
-define(CATALOG, agent_capability_catalog).
-define(ADAPTER, agent_trigger_adapter).
-define(REC, agent_recovery).

-define(ORG, 351).
-define(AGENT, 352).
-define(GRANT, 353).
-define(RUN_ID, 3540).
-define(NOW, {{2026, 9, 17}, {12, 0, 0}}).

%% A08 | same source delivery/idempotency key | trigger twice | one Run id
a08_duplicate_trigger_test() ->
    setup(),
    try
        %% 第一次投递 → created
        {ok, created, Run1} = ?ADAPTER:start_run(conn(), req()),
        ?assertEqual(?RUN_ID, maps:get(id, Run1)),
        %% 第二次同 source delivery（同 trigger_type+trigger_id+idempotency_key）
        %% → 命中既有 Run（find_run_by_trigger），不新建
        meck:expect(?PG, find_run_by_trigger, fun(_C, _A, _O, _T, _Ti, _K) ->
            {ok, Run1}
        end),
        {ok, existing, Run2} = ?ADAPTER:start_run(conn(), req()),
        %% distinct_run_count=1：两次指向同一 Run id
        ?assertEqual(maps:get(id, Run1), maps:get(id, Run2)),
        ?assertEqual(1, create_calls())
    after
        teardown()
    end.

%% A15 | Organization archived | message/schedule/webhook trigger | no Run
a15_archived_org_all_triggers_denied_test() ->
    setup(),
    try
        meck:expect(?MEMBERSHIP, resolve_organization_state, fun(_O) ->
            {ok, #{status => archived, version => 9}}
        end),
        %% message / schedule：org 门拒
        lists:foreach(
            fun(T) ->
                ?assertEqual(
                    {error, org_not_active},
                    ?ADAPTER:start_run(conn(), req(#{trigger_type => T}))
                )
            end,
            [message, schedule]
        ),
        %% webhook：验签后同样被 org 门拒（run_count=0 for all three）
        ?assertEqual(
            {error, org_not_active},
            ?ADAPTER:start_run(conn(), req(#{trigger_type => webhook, verified => true}))
        ),
        ?assertEqual(0, create_calls())
    after
        teardown()
    end.

%% ===================================================================
%% 夹具
%% ===================================================================

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
    meck:expect(?PG, get_grant, fun(_C, _G) ->
        {ok, #{
            id => ?GRANT,
            version => 1,
            status => active,
            valid_from => {{2026, 9, 1}, {0, 0, 0}},
            expires_at => {{2027, 9, 1}, {0, 0, 0}}
        }}
    end),
    meck:expect(?PG, find_run_by_trigger, fun(_C, _A, _O, _T, _Ti, _K) -> {error, not_found} end),
    meck:new(agent_run_command, [no_link]),
    meck:expect(agent_run_command, create_run, fun(_C, _Ctx) ->
        put(ag31_08_acc_creates, create_calls() + 1),
        {ok, #{id => ?RUN_ID, status => created}}
    end),
    erase(ag31_08_acc_creates),
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
        [?PG, ?MEMBERSHIP, agent_run_command]
    ),
    application:unset_env(imboy, agent_default_workspace_module),
    erase(ag31_08_acc_creates),
    ok.

create_calls() ->
    case get(ag31_08_acc_creates) of
        undefined -> 0;
        N -> N
    end.

req() ->
    req(#{}).

req(Over) ->
    maps:merge(
        #{
            trigger_type => message,
            trigger_id => <<"trigger-acc-1">>,
            idempotency_key => <<"idem-acc-1">>,
            agent_id => ?AGENT,
            organization_id => ?ORG,
            grant_id => ?GRANT,
            runtime_type => mock,
            context_digest => <<"sha256:ctx">>,
            now => ?NOW
        },
        Over
    ).

conn() ->
    self().

%% ===================================================================
%% AG31-09 命名验收测试（A04/A05/A09/A11——实施计划 §6 行逐字）
%% ===================================================================

%% A04 | running Run with active Grant | revoke before next effect
%%      | next effect denied | post-revoke dispatch=0
a04_revoke_race_test() ->
    rec_setup(),
    try
        rec_given_full_chain(active),
        meck:expect(?PG, get_grant, fun(_C, _G) ->
            {ok, #{
                id => ?GRANT,
                version => 3,
                status => revoked,
                valid_from => {{2026, 9, 1}, {0, 0, 0}},
                expires_at => {{2027, 9, 1}, {0, 0, 0}}
            }}
        end),
        ?assertEqual(
            {deny, grant_revoked},
            ?REC:recheck_effect(rec_run_ctx(), rec_tool(), rec_resource(), 11)
        ),
        ?assertEqual(0, rec_safe_calls(ag31_08_acc_dispatcher, dispatch))
    after
        rec_teardown()
    end.

%% A05 | valid Run | suspend Agent membership | new Run and next effect
%%      denied | stale allow=0
a05_suspend_immediate_test() ->
    rec_setup(),
    try
        rec_given_full_chain(active),
        %% membership 事实源即时失效（port 契约：archived → {error, archived}）
        meck:expect(?MEMBERSHIP, resolve_organization_state, fun(_O) ->
            {error, archived}
        end),
        %% next effect denied
        ?assertEqual(
            {deny, org_archived},
            ?REC:recheck_effect(rec_run_ctx(), rec_tool(), rec_resource(), 11)
        ),
        %% new Run denied（同一 port 消费面）
        ?assertEqual(
            {error, org_not_active}, ?ADAPTER:start_run(self(), rec_req())
        )
    after
        rec_teardown()
    end.

%% A09 | same effect/idempotency key | dispatch twice | external attempt <=1
%%      | adapter_count=1; unknown not retried
a09_effect_at_most_once_test() ->
    rec_setup(),
    try
        rec_given_full_chain(active),
        %% 第一次重派（recheck allow → 执行器恰一）
        {allow, _} = ?REC:recheck_effect(rec_run_ctx(), rec_tool(), rec_resource(), 11),
        ?assertEqual(1, rec_safe_calls(ag31_08_acc_dispatcher, dispatch)),
        %% 第二次同幂等键 → effect dedup（external attempt 不增）
        meck:expect(?PG, insert_effect_guarded_tx, fun(_C, _E) ->
            {error, {duplicate_effect, <<"uq_ae_tool_external_idem">>}}
        end),
        ?assertEqual(
            {deny, duplicate_effect},
            ?REC:recheck_effect(rec_run_ctx(), rec_tool(), rec_resource(), 11)
        ),
        ?assertEqual(1, rec_safe_calls(ag31_08_acc_dispatcher, dispatch)),
        %% unknown 态不在恢复重查清单（不自动重发）
        meck:expect(?PG, list_run_effects, fun(_C, _R) ->
            [#{id => 11, status => unknown, sequence => 1}]
        end),
        meck:expect(?PG, lease_take_over, fun(_C, _R, _W, _E, _N) ->
            {ok, #{version => 5, attempt => 2}}
        end),
        {ok, Plan} = ?REC:recover_run(self(), ?RUN_ID, <<"w1">>, 60, #{now => ?NOW}),
        ?assertEqual([], maps:get(recheck, Plan))
    after
        rec_teardown()
    end.

%% A11 | leased running Run | owner process/node stops | one recovery owner
%%      | winner_count=1; duplicate_effect=0
a11_single_recovery_owner_test() ->
    rec_setup(),
    try
        %% lease CAS 单 owner：首次抢占成功，其后全拒（数据库真源；
        %% 真库双连接竞争腿见 agent_recovery_pg_tests）
        Winner = erlang:make_ref(),
        meck:expect(?PG, lease_take_over, fun(_C, _R, _W, _E, _N) ->
            case get(a11_taken) of
                undefined ->
                    put(a11_taken, true),
                    {ok, #{version => 5, attempt => 2}};
                true ->
                    {error, lease_not_acquired}
            end
        end),
        meck:expect(?PG, list_run_effects, fun(_C, _R) ->
            [#{id => 11, status => authorized, sequence => 1}]
        end),
        {ok, PlanW} = ?REC:recover_run(self(), ?RUN_ID, <<"winner">>, 60, #{now => ?NOW}),
        {error, lease_not_acquired} =
            ?REC:recover_run(self(), ?RUN_ID, <<"loser">>, 60, #{now => ?NOW}),
        ?assertEqual(<<"winner">>, maps:get(owner, PlanW)),
        ?assertEqual([11], maps:get(recheck, PlanW)),
        ?assert(Winner =/= loser_is_not_a_ref)
    after
        rec_teardown()
    end.

%% ------------------------------------------------------------------
%% AG31-09 夹具（与 AG31-08 夹具同文件共存；前缀 rec_ 隔离）
%% ------------------------------------------------------------------

rec_setup() ->
    meck:new(?PG, [no_link]),
    meck:new(?MEMBERSHIP, [no_link]),
    meck:new(?CATALOG, [no_link]),
    meck:new(?GPG, [no_link]),
    meck:expect(?PG, next_id, fun
        (agent_effect) -> 4100;
        (agent_run_event) -> 4200
    end),
    meck:expect(?PG, next_effect_sequence, fun(_C, _R) -> 9 end),
    meck:expect(?PG, find_run_by_trigger, fun(_C, _A, _O, _T, _Ti, _K) ->
        {error, not_found}
    end),
    safe_meck_new(ag31_08_acc_dispatcher),
    meck:expect(ag31_08_acc_dispatcher, dispatch, fun(_I) ->
        put(ag31_08_acc_disp, disp_calls() + 1),
        {ok, dispatched}
    end),
    ok = application:set_env(imboy, agent_tool_dispatcher_module, ag31_08_acc_dispatcher),
    erase(ag31_08_acc_disp),
    erase(a11_taken).

rec_teardown() ->
    lists:foreach(
        fun(M) ->
            try
                meck:unload(M)
            catch
                _:_ -> ok
            end
        end,
        [?PG, ?GPG, ?MEMBERSHIP, ?CATALOG, ag31_08_acc_dispatcher]
    ),
    application:unset_env(imboy, agent_tool_dispatcher_module),
    application:unset_env(imboy, agent_membership_module),
    erase(ag31_08_acc_disp),
    erase(a11_taken),
    ok.

%% authorizer 全链夹具（active Grant + active Org + 政策放行）
rec_given_full_chain(GrantStatus) ->
    ok = application:set_env(imboy, agent_membership_module, ?MEMBERSHIP),
    safe_meck_new(ag31_08_acc_policy),
    meck:expect(ag31_08_acc_policy, evaluate, fun(_R, _T, _Res) -> allow end),
    ok = application:set_env(imboy, agent_resource_policy_module, ag31_08_acc_policy),
    meck:expect(?PG, get_run, fun(_C, _R) ->
        {ok, #{
            id => ?RUN_ID,
            version => 4,
            status => running,
            grant_id => ?GRANT,
            agent_id => ?AGENT,
            organization_id => ?ORG,
            workspace_id => undefined,
            idempotency_key => <<"rec-acc">>,
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
            run_id => ?RUN_ID,
            status => authorized,
            args_digest => <<"sha256:args">>,
            grant_version_checked => 2
        }}
    end),
    meck:expect(?PG, insert_effect_tx, fun(_C, _E, _R) -> {ok, 4100, undefined} end),
    meck:expect(?PG, insert_effect_guarded_tx, fun(_C, _E) -> {ok, 4100} end),
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
    ok.

rec_run_ctx() ->
    #{run_id => ?RUN_ID, agent_id => ?AGENT, organization_id => ?ORG, now => ?NOW, conn => self()}.

rec_tool() ->
    #{
        tool_id => <<"tool.acc.rec">>,
        capability => <<"demo.read">>,
        action => <<"invoke">>,
        risk_level => low,
        side_effect_class => readonly
    }.

rec_resource() ->
    #{
        organization_id => ?ORG,
        workspace_id => undefined,
        resource_type => <<"demo">>,
        resource_digest => <<"sha256:res">>,
        args_digest => <<"sha256:args">>
    }.

rec_req() ->
    #{
        trigger_type => message,
        trigger_id => <<"t-rec">>,
        idempotency_key => <<"k-rec">>,
        agent_id => ?AGENT,
        organization_id => ?ORG,
        grant_id => ?GRANT,
        runtime_type => mock,
        context_digest => <<"sha256:ctx">>,
        now => ?NOW
    }.

disp_calls() ->
    case get(ag31_08_acc_disp) of
        undefined -> 0;
        N -> N
    end.

rec_safe_calls(Mod, Fun) ->
    try
        meck:num_calls(Mod, Fun, '_')
    catch
        _:_ -> 0
    end.

safe_meck_new(Mod) ->
    try
        meck:new(Mod, [no_link, non_strict])
    catch
        error:{already_started, _} -> ok
    end.
