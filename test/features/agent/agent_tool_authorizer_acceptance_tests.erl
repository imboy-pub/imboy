%% @doc AG31-05：可执行验收矩阵命名测试（实施计划 §6 Executable Acceptance
%% Matrix A01/A02/A03/A06/A07/A10 行——本文件六个 named test 逐字不可改，
%% 是 AG31-05 的验收权威）。
%%
%% 验收口径（实施计划表为唯一权威）：
%%   * A01 active Agent Org membership, no Grant → deny before dispatch；
%%     dispatcher_count=0；deny audit=1；决策行（denied/grant_missing）落账。
%%   * A02 Grant belongs Org A, request Org B resource → cross-org deny 且审计；
%%     reason=cross_org；dispatch=0。
%%   * A03 Grant scopes Workspace A, request Workspace B → deny before
%%     adapter；ws membership 门生效；dispatch adapter count=0。
%%   * A06 workspace Tool allowed, remove workspace membership → next
%%     workspace effect denied；dispatch=0。
%%   * A07 Profile/Prompt claims extra scope → untrusted input cannot
%%     expand（server ResourceContext 之外的 scope 一律拒）；dispatch=0。
%%   * A10 approved args digest/version, mutate args or revoke Grant →
%%     old approval rejected；dispatch=0。
%%
%% 环境：meck pg/grant_pg/membership/catalog（零真库），env 注入 permissive
%% policy（A0 裁决 R2 默认恒 deny，放行场景须注入）+ 计数 dispatcher
%% （R5）。审计经真 logger handler 捕获计数（logger sticky 不可 meck）。
-module(agent_tool_authorizer_acceptance_tests).

-include_lib("eunit/include/eunit.hrl").

-export([init/1, log/2]).

-define(PG, agent_run_pg).
-define(GPG, agent_grant_pg).
-define(MEMBERSHIP, agent_org_membership_adapter).
-define(CATALOG, agent_capability_catalog).
-define(AUTHORIZER, agent_tool_authorizer).
-define(DISPATCHER, ag31_05_acc_dispatcher).

-define(ORG_A, 22).
-define(ORG_B, 99).
-define(AGENT, 11).
-define(RUN, 101).
-define(GRANT, 33).
-define(WS_A, 55).
-define(WS_B, 66).
-define(NOW, {{2026, 9, 17}, {12, 0, 0}}).
-define(VF, {{2026, 9, 1}, {0, 0, 0}}).
-define(EXP, {{2027, 9, 1}, {0, 0, 0}}).
-define(EFFECT_ID, 9001).

%% ===================================================================
%% 六个命名验收测试（逐字；实现计划 A01-A07/A10 行）
%% ===================================================================

%% A01 | active Agent Org membership, no Grant | authorize Tool effect
%%      | deny before dispatch | dispatcher_count=0; deny audit=1; decision row
a01_membership_without_grant_denied_test() ->
    setup(),
    try
        given_membership_only(),
        Result = ?AUTHORIZER:authorize(run_ctx(), readonly_tool(), resource()),
        %% deny before dispatch，稳定 reason=grant_missing
        ?assertEqual({deny, grant_missing}, Result),
        %% dispatcher_count=0
        ?assertEqual(0, dispatcher_count()),
        %% 决策行：denied effect 带 grant_missing 落账（decision row）
        Effect = meck:capture(first, ?PG, insert_effect_tx, ['_', '_', '_'], 2),
        ?assertEqual(denied, maps:get(decided_status, Effect)),
        ?assertEqual(grant_missing, maps:get(denial_reason, Effect)),
        %% deny audit=1
        with_audit_capture(fun() ->
            reset_calls([?PG]),
            ?assertEqual(
                {deny, grant_missing}, ?AUTHORIZER:authorize(run_ctx(), readonly_tool(), resource())
            ),
            Report = recv_audit(),
            ?assertEqual(agent_tool_decision, maps:get(what, Report)),
            ?assertEqual(deny, maps:get(outcome, Report)),
            ?assertEqual(grant_missing, maps:get(reason, Report)),
            ?assertEqual(0, drain_audit_count())
        end)
    after
        teardown()
    end.

%% A02 | Grant belongs Org A | request Org B resource | cross-org deny
%%      | dispatch=0; reason=cross_org; audit assertion
a02_cross_org_denied_test() ->
    setup(),
    try
        given_all_gates_pass(),
        reset_calls([?MEMBERSHIP, ?CATALOG, ?DISPATCHER]),
        ResB = resource(#{organization_id => ?ORG_B}),
        ?assertEqual({deny, cross_org}, ?AUTHORIZER:authorize(run_ctx(), readonly_tool(), ResB)),
        ?assertEqual(0, dispatcher_count()),
        %% 步序读短路：步骤 1 即拒，membership/catalog 供给器未被调用
        ?assertEqual(0, meck:num_calls(?MEMBERSHIP, resolve_organization_membership, '_')),
        ?assertEqual(0, meck:num_calls(?CATALOG, lookup, '_')),
        with_audit_capture(fun() ->
            ?assertEqual(
                {deny, cross_org}, ?AUTHORIZER:authorize(run_ctx(), readonly_tool(), ResB)
            ),
            Report = recv_audit(),
            ?assertEqual(deny, maps:get(outcome, Report)),
            ?assertEqual(cross_org, maps:get(reason, Report))
        end)
    after
        teardown()
    end.

%% A03 | Grant scopes Workspace A | request Workspace B | deny before adapter
%%      | ws membership 门（scope facts + counter）；dispatch adapter count=0
a03_workspace_membership_required_test() ->
    setup(),
    try
        given_all_gates_pass(),
        given_grant_scope([?WS_A]),
        %% Agent 在 Workspace B 无 member 行（port not_found）——workspace 门拒绝
        meck:expect(?MEMBERSHIP, resolve_workspace_membership, fun(_O, Ws, _A) when Ws =:= ?WS_B ->
            {error, not_found}
        end),
        ResB = resource(#{workspace_id => ?WS_B}),
        ?assertEqual(
            {deny, workspace_membership_denied},
            ?AUTHORIZER:authorize(run_ctx(), readonly_tool(), ResB)
        ),
        %% ws membership 门确实被查询（scope facts）
        ?assertEqual(1, meck:num_calls(?MEMBERSHIP, resolve_workspace_membership, '_')),
        %% dispatch adapter count=0
        ?assertEqual(0, dispatcher_count())
    after
        teardown()
    end.

%% A06 | workspace Tool allowed | remove workspace membership | next workspace
%%      effect denied | dispatch=0；membership read + decision
a06_workspace_remove_test() ->
    setup(),
    try
        given_all_gates_pass(),
        given_grant_scope([?WS_A]),
        given_ws_member_ok(?WS_A),
        {allow, _} = ?AUTHORIZER:authorize(
            run_ctx(), readonly_tool(), resource(#{workspace_id => ?WS_A})
        ),
        ?assertEqual(1, dispatcher_count()),
        %% 移除 workspace membership（事实源即时生效——零缓存）
        meck:expect(
            ?MEMBERSHIP, resolve_workspace_membership, fun(_O, Ws, _A) when Ws =:= ?WS_A ->
                {error, not_found}
            end
        ),
        ?assertEqual(
            {deny, workspace_membership_denied},
            ?AUTHORIZER:authorize(run_ctx(), readonly_tool(), resource(#{workspace_id => ?WS_A}))
        ),
        ?assertEqual(1, dispatcher_count())
    after
        teardown()
    end.

%% A07 | Profile/Prompt claims extra scope | submit outside-Grant args
%%      | untrusted input cannot expand | deny; dispatch=0
a07_untrusted_scope_cannot_expand_test() ->
    setup(),
    try
        given_all_gates_pass(),
        given_grant_scope([?WS_A]),
        given_ws_member_ok(?WS_B),
        %% 不可信输入（Profile/Prompt 注入的 scope 声明）挂在 RunCtx 上——authorizer
        %% 只信服务端 adapter 解析的 ResourceContext，声明键被完全忽略
        ClaimedCtx = run_ctx(#{
            claimed_workspace_ids => [?WS_B, 9999],
            claimed_scope => <<"org:root">>,
            prompt_grant_override => <<"allow-all">>
        }),
        %% 服务端 ResourceContext：Workspace B（Grant 只覆盖 A）
        ResB = resource(#{workspace_id => ?WS_B}),
        ?assertEqual(
            {deny, grant_scope_mismatch},
            ?AUTHORIZER:authorize(ClaimedCtx, readonly_tool(), ResB)
        ),
        ?assertEqual(0, dispatcher_count()),
        %% 声明键对决策零影响：同参同拒绝（same inputs same decision）
        ?assertEqual(
            {deny, grant_scope_mismatch}, ?AUTHORIZER:authorize(run_ctx(), readonly_tool(), ResB)
        )
    after
        teardown()
    end.

%% A10 | approved args digest/version | mutate args or revoke Grant
%%      | old approval rejected | dispatch=0；approval/decision rows
a10_stale_approval_denied_test() ->
    setup(),
    try
        given_all_gates_pass(),
        Tool = write_tool(),
        %% 1) write 工具 → approval_required（决策行 waiting_approval + Run E07）
        {approval_required, _AC} = ?AUTHORIZER:authorize(run_ctx(), Tool, resource()),
        ?assertEqual(0, dispatcher_count()),
        %% 2) 批准摄取：args 变更 → 旧批准拒（四元组绑定：args_digest 不匹配）
        meck:expect(?PG, get_effect, fun(_C, _E) -> {ok, effect_row(#{})} end),
        meck:expect(agent_run_command, approve_effect, fun(_C, _R, _E, _Ctx) ->
            {error, approval_digest_mismatch}
        end),
        ?assertEqual(
            {error, approval_digest_mismatch},
            ?AUTHORIZER:approve_effect(
                conn(), ?RUN, ?EFFECT_ID, approve_ctx(#{args_digest => <<"sha256:args-MUTATED">>})
            )
        ),
        %% 3) Grant 撤销后再批准 → 实时重检拒绝（批准不能覆盖已撤销 Grant；
        %%    agent_run_command:approve_effect 的 Grant 重检行为由 04B 套件钉死）
        meck:expect(agent_run_command, approve_effect, fun(_C, _R, _E, _Ctx) ->
            {error, grant_revoked}
        end),
        ?assertEqual(
            {error, grant_revoked},
            ?AUTHORIZER:approve_effect(conn(), ?RUN, ?EFFECT_ID, approve_ctx(#{}))
        ),
        %% 4) dispatch 前再次授权=全链重走：批准携带的绑定 args 已变 → stale 拒
        meck:expect(?PG, get_effect, fun(_C, _E) -> {ok, effect_row(#{})} end),
        Ctx = run_ctx(#{approval_effect_id => ?EFFECT_ID}),
        ResMutated = resource(#{args_digest => <<"sha256:args-MUTATED">>}),
        ?assertEqual({deny, stale_approval_args}, ?AUTHORIZER:authorize(Ctx, Tool, ResMutated)),
        %% 5) Grant 版本变化（撤销 → version+1）→ 绑定版本失配拒
        meck:expect(?PG, get_grant, fun(_C, _G) -> {ok, grant(#{version => 3})} end),
        ?assertEqual(
            {deny, stale_approval_grant_version},
            ?AUTHORIZER:authorize(Ctx, Tool, resource())
        ),
        ?assertEqual(0, dispatcher_count())
    after
        teardown()
    end.

%% ===================================================================
%% 套件夹具（每个命名测试自包含：setup/teardown 独立 meck 实例 + env 自清理）
%% ===================================================================

setup() ->
    meck:new(?PG, [no_link]),
    meck:new(?GPG, [no_link]),
    meck:new(?MEMBERSHIP, [no_link]),
    meck:new(?CATALOG, [no_link]),
    meck:new(agent_run_command, [no_link]),
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
        [
            ?PG,
            ?GPG,
            ?MEMBERSHIP,
            ?CATALOG,
            logger,
            agent_run_command,
            ?DISPATCHER,
            ag31_05_acc_policy
        ]
    ),
    lists:foreach(
        fun(K) -> application:unset_env(imboy, K) end,
        [agent_resource_policy_module, agent_tool_dispatcher_module]
    ),
    erase(ag31_05_acc_disp),
    ok.

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

%% ===================================================================
%% 审计捕获（真 logger handler）
%% ===================================================================

init(Config) ->
    {ok, maps:get(config, Config, self())}.

log(Event, HandlerConfig) ->
    TestPid = maps:get(config, HandlerConfig, undefined),
    case {is_pid(TestPid), maps:get(msg, Event, undefined)} of
        {true, {report, Report}} when is_map(Report) ->
            forward_audit(TestPid, Report);
        {true, {report, Report, _Format}} when is_map(Report) ->
            forward_audit(TestPid, Report);
        _NotOurs ->
            ok
    end.

forward_audit(TestPid, Report) ->
    case maps:get(what, Report, undefined) of
        agent_tool_decision ->
            TestPid ! {ag31_05_acc_audit, Report},
            ok;
        _ ->
            ok
    end.

with_audit_capture(F) ->
    Id = ag31_05_acc_audit_capture,
    case logger:add_handler(Id, ?MODULE, #{config => self()}) of
        ok ->
            ok;
        {error, {already_exist, _}} ->
            ok = logger:remove_handler(Id),
            ok = logger:add_handler(Id, ?MODULE, #{config => self()})
    end,
    try
        F()
    after
        _ = logger:remove_handler(Id)
    end,
    ok.

recv_audit() ->
    receive
        {ag31_05_acc_audit, Report} -> Report
    after 2000 -> erlang:error(audit_line_not_emitted)
    end.

drain_audit_count() ->
    drain_audit_count(0).

drain_audit_count(N) ->
    receive
        {ag31_05_acc_audit, _Report} -> drain_audit_count(N + 1)
    after 300 -> N
    end.

%% ===================================================================
%% 给定条件与 fixtures
%% ===================================================================

%% Agent 有 active Org membership；Grant 未发行（A01 Given）
given_membership_only() ->
    given_id_generators(),
    given_run_ok(),
    given_agent_enabled(),
    given_org_facts_ok(),
    given_policy_allow(),
    given_counting_dispatcher(),
    meck:expect(?PG, get_grant, fun(_C, _G) -> {error, not_found} end),
    meck:expect(?PG, insert_effect_tx, fun(_C, _E, _R) -> {ok, ?EFFECT_ID, undefined} end).

%% 除 Grant scope 特化外全关放行（readonly 工具可走通 allow）
given_all_gates_pass() ->
    given_id_generators(),
    given_run_ok(),
    given_agent_enabled(),
    given_org_facts_ok(),
    given_grant_active(),
    given_catalog_entry(),
    given_policy_allow(),
    given_counting_dispatcher(),
    meck:expect(?PG, insert_effect_tx, fun(_C, _E, _R) -> {ok, ?EFFECT_ID, undefined} end),
    %% allow 决策走 MEDIUM-2 守卫事务（A2 review）：默认成功期望与 tx 同型
    meck:expect(?PG, insert_effect_guarded_tx, fun(_C, _E) -> {ok, ?EFFECT_ID} end).

given_id_generators() ->
    meck:expect(?PG, next_id, fun
        (agent_effect) -> ?EFFECT_ID;
        (agent_run_event) -> 9100
    end),
    meck:expect(?PG, next_effect_sequence, fun(_C, _R) -> 7 end).

given_run_ok() ->
    meck:expect(?PG, get_run, fun(_C, _R) ->
        {ok, #{
            id => ?RUN,
            version => 3,
            status => running,
            grant_id => ?GRANT,
            agent_id => ?AGENT,
            organization_id => ?ORG_A,
            workspace_id => undefined,
            idempotency_key => <<"acc-idem-1">>,
            trigger_type => message
        }}
    end).

given_agent_enabled() ->
    meck:expect(?PG, get_agent_identity, fun(_C, _A) ->
        {ok, #{account_type => 1, status => 1}}
    end).

given_org_facts_ok() ->
    meck:expect(?MEMBERSHIP, resolve_organization_state, fun(_O) ->
        {ok, #{status => active, version => 5}}
    end),
    meck:expect(?MEMBERSHIP, resolve_organization_membership, fun(_O, _A) ->
        {ok, #{status => active, role => member, version => 7}}
    end).

given_grant_active() ->
    meck:expect(?PG, get_grant, fun(_C, _G) -> {ok, grant(#{})} end),
    meck:expect(?GPG, list_workspace_ids, fun(_C, _O, _G) -> [] end),
    meck:expect(?GPG, list_capabilities, fun(_C, _O, _G) ->
        [cap_row(#{})]
    end).

given_grant_scope(WsIds) ->
    meck:expect(?GPG, list_workspace_ids, fun(_C, _O, _G) -> WsIds end).

given_catalog_entry() ->
    meck:expect(?CATALOG, lookup, fun(C, A, R) ->
        {ok, #{
            capability => C,
            action => A,
            resource_type => R,
            legal_constraint_keys => [<<"workspace_ids">>, <<"resource_id">>]
        }}
    end).

cap_row(Constraint) ->
    #{
        capability => <<"demo.read">>,
        action => <<"invoke">>,
        resource_type => <<"demo">>,
        constraint => Constraint
    }.

given_ws_member_ok(WsId) ->
    meck:expect(?MEMBERSHIP, resolve_workspace_membership, fun(_O, W, _A) when W =:= WsId ->
        {ok, #{status => active, role => member, version => 9}}
    end).

given_policy_allow() ->
    safe_meck_new(ag31_05_acc_policy),
    meck:expect(ag31_05_acc_policy, evaluate, fun(_R, _T, _Res) -> allow end),
    ok = application:set_env(imboy, agent_resource_policy_module, ag31_05_acc_policy).

given_counting_dispatcher() ->
    safe_meck_new(?DISPATCHER),
    meck:expect(?DISPATCHER, dispatch, fun(_Input) ->
        put(ag31_05_acc_disp, disp_count() + 1),
        {ok, dispatched}
    end),
    ok = application:set_env(imboy, agent_tool_dispatcher_module, ?DISPATCHER),
    erase(ag31_05_acc_disp).

disp_count() ->
    case get(ag31_05_acc_disp) of
        undefined -> 0;
        N -> N
    end.

dispatcher_count() ->
    disp_count().

safe_meck_new(Mod) ->
    try
        meck:new(Mod, [no_link, non_strict]),
        ok
    catch
        error:{already_started, _} -> ok
    end.

grant(Over) ->
    maps:merge(
        #{
            id => ?GRANT,
            version => 2,
            status => active,
            valid_from => ?VF,
            expires_at => ?EXP
        },
        Over
    ).

effect_row(Over) ->
    maps:merge(
        #{
            id => ?EFFECT_ID,
            run_id => ?RUN,
            status => authorized,
            args_digest => <<"sha256:args">>,
            grant_version_checked => 2
        },
        Over
    ).

run_ctx() -> run_ctx(#{}).

run_ctx(Over) ->
    maps:merge(
        #{
            run_id => ?RUN,
            agent_id => ?AGENT,
            organization_id => ?ORG_A,
            now => ?NOW,
            conn => conn()
        },
        Over
    ).

readonly_tool() ->
    #{
        tool_id => <<"tool.demo.v1">>,
        capability => <<"demo.read">>,
        action => <<"invoke">>,
        risk_level => low,
        side_effect_class => readonly
    }.

write_tool() ->
    #{
        tool_id => <<"tool.demo.v1">>,
        capability => <<"demo.read">>,
        action => <<"invoke">>,
        risk_level => medium,
        side_effect_class => write
    }.

resource() -> resource(#{}).

resource(Over) ->
    maps:merge(
        #{
            organization_id => ?ORG_A,
            workspace_id => undefined,
            resource_type => <<"demo">>,
            resource_digest => <<"sha256:res">>,
            args_digest => <<"sha256:args">>
        },
        Over
    ).

approve_ctx(Over) ->
    maps:merge(
        #{
            args_digest => <<"sha256:args">>,
            approval_ref => <<"appr-acc-1">>,
            actor_id => <<"human-1">>,
            now => ?NOW
        },
        Over
    ).

conn() ->
    self().
