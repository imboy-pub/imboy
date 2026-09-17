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
-define(MEMBERSHIP, agent_org_membership_adapter).
-define(ADAPTER, agent_trigger_adapter).

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
