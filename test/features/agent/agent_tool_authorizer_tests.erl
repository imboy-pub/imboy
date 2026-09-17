%% @doc AG31-05：中央工具授权器套件（架构合同 §10 Interface 5 + §T 十步决策
%% 序 + §R A0 裁决 R1-R7）。
%%
%% 生成器：
%%   * `decision_domain_test_`——纯域断言（零 mock，铁律 4）：入参形状、
%%     R3 hitl_verdict、R4 constraint_verdict 保守求值；
%%   * `authorizer_test_`——meck {foreach, setup, cleanup, [groups]} 范式
%%     （镜像 agent_grant_tests / agent_run_command_deny_tests）：mock
%%     agent_run_pg / agent_grant_pg / agent_org_membership_adapter /
%%     agent_capability_catalog，env 注入 policy/dispatcher 假模块，逐条覆盖
%%     十步每关至少一正一反 + truth table + dispatcher 计数 + catalog 空集
%%     默认拒 + R2/R3 默认政策 + CS 拒 + 决策持久化失败 fail closed +
%%     审计（真 logger handler 捕获）+ env seam。
%%
%% 时钟注入：Now 由用例显式给出（{{2026,9,17},{12,0,0}} 锚点）。
-module(agent_tool_authorizer_tests).

-include_lib("eunit/include/eunit.hrl").

-export([init/1, log/2]).

-define(PG, agent_run_pg).
-define(GPG, agent_grant_pg).
-define(MEMBERSHIP, agent_org_membership_adapter).
-define(CATALOG, agent_capability_catalog).
-define(AUTHORIZER, agent_tool_authorizer).

-define(ORG, 22).
-define(AGENT, 11).
-define(RUN, 101).
-define(GRANT, 33).
-define(WS, 55).
-define(NOW, {{2026, 9, 17}, {12, 0, 0}}).
-define(VF, {{2026, 9, 1}, {0, 0, 0}}).
-define(EXP, {{2027, 9, 1}, {0, 0, 0}}).
-define(EFFECT_ID, 9001).

%% ===================================================================
%% 生成器 1：纯域（零 mock）
%% ===================================================================

decision_domain_test_() ->
    [
        {"d1: tool descriptor shape (five keys + known enums + domain key)", fun t_descriptor/0},
        {"d2: resource context shape (org/ws/type/digests)", fun t_resource/0},
        {"d3: R3 hitl verdict readonly/pass-through/unknown", fun t_hitl/0},
        {"d4: R4 constraint conservative evaluation", fun t_constraint/0}
    ].

t_descriptor() ->
    ?assertEqual(ok, agent_tool_decision:validate_tool_descriptor(tool())),
    %% 缺键 / 空串 / 未知 risk / 未知 class 一律 fail closed（§10.2 未知 risk deny）
    ?assertEqual(
        {error, invalid_tool_descriptor},
        agent_tool_decision:validate_tool_descriptor(maps:without([risk_level], tool()))
    ),
    ?assertEqual(
        {error, invalid_tool_descriptor},
        agent_tool_decision:validate_tool_descriptor(tool(#{tool_id => <<>>}))
    ),
    ?assertEqual(
        {error, invalid_tool_descriptor},
        agent_tool_decision:validate_tool_descriptor(tool(#{risk_level => extreme}))
    ),
    ?assertEqual(
        {error, invalid_tool_descriptor},
        agent_tool_decision:validate_tool_descriptor(tool(#{side_effect_class => teleport}))
    ),
    %% domain 键：native 放行；cs 是已知域值（CS gate 在步骤 9 deny，形状合法）
    ?assertEqual(ok, agent_tool_decision:validate_tool_descriptor(tool(#{domain => cs}))),
    ?assertEqual(
        {error, invalid_tool_descriptor},
        agent_tool_decision:validate_tool_descriptor(tool(#{domain => 99}))
    ),
    ok.

t_resource() ->
    ?assertEqual(ok, agent_tool_decision:validate_resource_context(resource())),
    ?assertEqual(
        {error, invalid_resource_context},
        agent_tool_decision:validate_resource_context(
            maps:without([args_digest], resource())
        )
    ),
    ?assertEqual(
        {error, invalid_resource_context},
        agent_tool_decision:validate_resource_context(resource(#{args_digest => <<>>}))
    ),
    ?assertEqual(
        {error, invalid_resource_context},
        agent_tool_decision:validate_resource_context(resource(#{organization_id => 0}))
    ),
    ?assertEqual(
        {error, invalid_resource_context},
        agent_tool_decision:validate_resource_context(resource(#{workspace_id => <<"x">>}))
    ),
    ok.

t_hitl() ->
    %% R3：readonly → 不需审批；已知 write → approval_required；未知 class/risk deny
    ?assertEqual(no_approval, agent_tool_decision:hitl_verdict(low, readonly)),
    ?assertEqual(no_approval, agent_tool_decision:hitl_verdict(critical, readonly)),
    ?assertEqual(approval_required, agent_tool_decision:hitl_verdict(low, write)),
    ?assertEqual(approval_required, agent_tool_decision:hitl_verdict(high, write)),
    ?assertEqual(
        {deny, unknown_side_effect_class}, agent_tool_decision:hitl_verdict(low, teleport)
    ),
    ?assertEqual({deny, unknown_risk_level}, agent_tool_decision:hitl_verdict(extreme, readonly)),
    %% 默认政策模块直接委托 domain 纯分类（R3 冻结绑定）
    ?assertEqual(no_approval, agent_hitl_policy:evaluate(low, readonly)),
    ?assertEqual(approval_required, agent_hitl_policy:evaluate(medium, write)),
    ?assertEqual({deny, unknown_risk_level}, agent_hitl_policy:evaluate(no, write)),
    ok.

t_constraint() ->
    Resource = resource(#{workspace_id => ?WS, resource_id => <<"res-1">>}),
    Legal = [<<"workspace_ids">>, <<"resource_id">>],
    %% 空约束 / 白名单内且满足 → ok
    ?assertEqual(ok, agent_tool_decision:constraint_verdict(#{}, Resource, Legal)),
    ?assertEqual(
        ok,
        agent_tool_decision:constraint_verdict(#{<<"workspace_ids">> => [1, ?WS]}, Resource, Legal)
    ),
    ?assertEqual(
        ok,
        agent_tool_decision:constraint_verdict(#{<<"resource_id">> => <<"res-1">>}, Resource, Legal)
    ),
    %% 不满足 → scope_mismatch（宁拒勿扩）
    ?assertEqual(
        {error, scope_mismatch},
        agent_tool_decision:constraint_verdict(#{<<"workspace_ids">> => [66]}, Resource, Legal)
    ),
    ?assertEqual(
        {error, scope_mismatch},
        agent_tool_decision:constraint_verdict(#{<<"resource_id">> => <<"res-2">>}, Resource, Legal)
    ),
    %% resource 缺 workspace/resource_id 而约束要求 → 无法保守判定 → deny
    Bare = resource(),
    ?assertEqual(
        {error, scope_mismatch},
        agent_tool_decision:constraint_verdict(#{<<"workspace_ids">> => [?WS]}, Bare, Legal)
    ),
    %% 键不在目录白名单 → invalid_constraint（D7 §3 授权侧复检）
    ?assertEqual(
        {error, {invalid_constraint, <<"role">>}},
        agent_tool_decision:constraint_verdict(#{<<"role">> => <<"admin">>}, Resource, Legal)
    ),
    %% 白名单合法但执行器不支持的键 → unsupportable_constraint（R4 宁拒勿扩）
    ?assertEqual(
        {error, unsupportable_constraint},
        agent_tool_decision:constraint_verdict(#{<<"region">> => <<"cn">>}, Resource, [
            <<"region">>
        ])
    ),
    %% 值形状非法（嵌套 map / 非列表）→ invalid_constraint
    ?assertEqual(
        {error, invalid_constraint},
        agent_tool_decision:constraint_verdict(#{<<"workspace_ids">> => ?WS}, Resource, Legal)
    ),
    ?assertEqual(
        {error, invalid_constraint},
        agent_tool_decision:constraint_verdict(#{<<"resource_id">> => #{}}, Resource, Legal)
    ),
    ok.

%% ===================================================================
%% 生成器 2：authorize/3 编排（meck + env 注入）
%% ===================================================================

authorizer_test_() ->
    {foreach,
        fun() ->
            meck:new(?PG, [no_link]),
            meck:new(?GPG, [no_link]),
            meck:new(?MEMBERSHIP, [no_link]),
            meck:new(?CATALOG, [no_link]),
            %% 默认期望 = D7 空集目录语义（未知 capability）；allow 场景再覆盖
            meck:expect(?CATALOG, lookup, fun(_C, _A, _R) -> {error, not_found} end),
            ok
        end,
        fun(_) ->
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
                    ag31_05_policy_allow,
                    ag31_05_dispatcher_mock,
                    ag31_05_hitl_mock,
                    ag31_05_membership_mock,
                    ag31_05_test_catalog
                ]
            ),
            lists:foreach(
                fun(K) -> application:unset_env(imboy, K) end,
                [
                    agent_membership_module,
                    agent_capability_catalog_module,
                    agent_resource_policy_module,
                    agent_hitl_policy_module,
                    agent_tool_dispatcher_module
                ]
            ),
            ok
        end,
        [
            fun step1_run_tests/1,
            fun step2_agent_tests/1,
            fun step3_org_tests/1,
            fun step4_ws_tests/1,
            fun step5_grant_tests/1,
            fun step6_match_tests/1,
            fun step7_policy_tests/1,
            fun step8_hitl_tests/1,
            fun step9_cs_tests/1,
            fun step10_persist_tests/1,
            fun truth_table_tests/1,
            fun dispatcher_tests/1,
            fun audit_tests/1,
            fun approve_tests/1,
            fun env_seam_tests/1
        ]}.

%% ------------------------------------------------------------------
%% 步骤 1：Run 非终态 + 上下文不可变
%% ------------------------------------------------------------------

step1_run_tests(_) ->
    [
        {"run missing -> run_not_found", fun() ->
            meck:expect(?PG, get_run, fun(_C, _R) -> {error, not_found} end),
            ?assertEqual({deny, run_not_found}, auth0())
        end},
        {"run read crash -> run_read_failed (fail closed)", fun() ->
            meck:expect(?PG, get_run, fun(_C, _R) -> erlang:error(pg_down) end),
            ?assertEqual({deny, run_read_failed}, auth0())
        end},
        {"terminal run (succeeded) -> run_terminal", fun() ->
            given_run_status(succeeded),
            ?assertEqual({deny, run_terminal}, auth0())
        end},
        {"unknown run -> run_unknown (禁止新 Effect)", fun() ->
            given_run_status(unknown),
            ?assertEqual({deny, run_unknown}, auth0())
        end},
        {"queued run -> run_not_running", fun() ->
            given_run_status(queued),
            ?assertEqual({deny, run_not_running}, auth0())
        end},
        {"run row agent mismatch vs context -> context_mismatch", fun() ->
            meck:expect(?PG, get_run, fun(_C, _R) -> {ok, run(#{agent_id => 99})} end),
            ?assertEqual({deny, context_mismatch}, auth0())
        end},
        {"resource org B vs run org A -> cross_org", fun() ->
            given_run_ok(),
            ?assertEqual(
                {deny, cross_org}, auth_res(resource(#{organization_id => 99}))
            )
        end}
    ].

%% ------------------------------------------------------------------
%% 步骤 2：Agent enabled（R1：行存在 ∧ account_type=1 ∧ status=1）
%% ------------------------------------------------------------------

step2_agent_tests(_) ->
    [
        {"agent row missing -> agent_not_found", fun() ->
            given_run_ok(),
            meck:expect(?PG, get_agent_identity, fun(_C, _A) -> {error, not_found} end),
            ?assertEqual({deny, agent_not_found}, auth0())
        end},
        {"agent account_type=0 (human) -> agent_not_agent", fun() ->
            given_run_ok(),
            given_agent_identity(0, 1),
            ?assertEqual({deny, agent_not_agent}, auth0())
        end},
        {"agent account_type=1 but status=0 (禁用) -> agent_disabled (R1 加核)", fun() ->
            given_run_ok(),
            given_agent_identity(1, 0),
            ?assertEqual({deny, agent_disabled}, auth0())
        end},
        {"agent enabled (account_type=1 ∧ status=1) passes to membership gate", fun() ->
            given_run_ok(),
            given_agent_identity(1, 1),
            meck:expect(?MEMBERSHIP, resolve_organization_state, fun(_O) ->
                {error, unavailable}
            end),
            ?assertEqual({deny, org_state_unavailable}, auth0())
        end}
    ].

%% ------------------------------------------------------------------
%% 步骤 3：Organization state + Membership
%% ------------------------------------------------------------------

step3_org_tests(_) ->
    [
        {"org archived -> org_archived", fun() ->
            given_run_ok(),
            given_agent_identity(1, 1),
            meck:expect(?MEMBERSHIP, resolve_organization_state, fun(_O) ->
                {error, archived}
            end),
            ?assertEqual({deny, org_archived}, auth0())
        end},
        {"org state unavailable -> org_state_unavailable (fail closed)", fun() ->
            given_run_ok(),
            given_agent_identity(1, 1),
            meck:expect(?MEMBERSHIP, resolve_organization_state, fun(_O) ->
                {error, unavailable}
            end),
            ?assertEqual({deny, org_state_unavailable}, auth0())
        end},
        {"org state port crash -> org_state_unavailable", fun() ->
            given_run_ok(),
            given_agent_identity(1, 1),
            meck:expect(?MEMBERSHIP, resolve_organization_state, fun(_O) ->
                erlang:error(facts_down)
            end),
            ?assertEqual({deny, org_state_unavailable}, auth0())
        end},
        {"membership suspended -> membership_denied", fun() ->
            given_run_ok(),
            given_agent_identity(1, 1),
            given_org_state_ok(),
            meck:expect(?MEMBERSHIP, resolve_organization_membership, fun(_O, _A) ->
                {error, inactive}
            end),
            ?assertEqual({deny, membership_denied}, auth0())
        end},
        {"membership port crash -> membership_unavailable", fun() ->
            given_run_ok(),
            given_agent_identity(1, 1),
            given_org_state_ok(),
            meck:expect(?MEMBERSHIP, resolve_organization_membership, fun(_O, _A) ->
                erlang:error(facts_down)
            end),
            ?assertEqual({deny, membership_unavailable}, auth0())
        end}
    ].

%% ------------------------------------------------------------------
%% 步骤 4：Workspace membership（需要时）
%% ------------------------------------------------------------------

step4_ws_tests(_) ->
    [
        {"ws required but member row missing -> workspace_membership_denied", fun() ->
            given_pipeline_upto_ws(),
            meck:expect(?MEMBERSHIP, resolve_workspace_membership, fun(_O, _W, _A) ->
                {error, not_found}
            end),
            ?assertEqual(
                {deny, workspace_membership_denied},
                auth_res(resource(#{workspace_id => ?WS}))
            )
        end},
        {"ws in another org -> cross_org (port cross_organization)", fun() ->
            given_pipeline_upto_ws(),
            meck:expect(?MEMBERSHIP, resolve_workspace_membership, fun(_O, _W, _A) ->
                {error, cross_organization}
            end),
            ?assertEqual(
                {deny, cross_org}, auth_res(resource(#{workspace_id => ?WS}))
            )
        end},
        {"ws suspended -> workspace_membership_denied", fun() ->
            given_pipeline_upto_ws(),
            meck:expect(?MEMBERSHIP, resolve_workspace_membership, fun(_O, _W, _A) ->
                {error, inactive}
            end),
            ?assertEqual(
                {deny, workspace_membership_denied},
                auth_res(resource(#{workspace_id => ?WS}))
            )
        end},
        {"ws not required: ws port never called when resource has no workspace", fun() ->
            safe_meck_new(ag31_05_policy_allow),
            given_all_gates_pass(),
            reset_calls([?MEMBERSHIP]),
            {allow, _} = auth0(),
            ?assertEqual(0, meck:num_calls(?MEMBERSHIP, resolve_workspace_membership, '_'))
        end}
    ].

%% ------------------------------------------------------------------
%% 步骤 5：Grant 实时重读
%% ------------------------------------------------------------------

step5_grant_tests(_) ->
    [
        {"grant missing -> grant_missing (A01 机制面)", fun() ->
            given_pipeline_upto_grant(),
            meck:expect(?PG, get_grant, fun(_C, _G) -> {error, not_found} end),
            reset_calls([?CATALOG]),
            ?assertEqual({deny, grant_missing}, auth0()),
            %% 步序读短路：grant 未过 → 目录/政策/审批供给器未被调用
            ?assertEqual(0, meck:num_calls(?CATALOG, lookup, '_'))
        end},
        {"grant revoked (实时) -> grant_revoked", fun() ->
            given_pipeline_upto_grant(),
            meck:expect(?PG, get_grant, fun(_C, _G) -> {ok, grant(#{status => revoked})} end),
            ?assertEqual({deny, grant_revoked}, auth0())
        end},
        {"grant expired (注入时钟实时算，不改存储) -> grant_expired", fun() ->
            given_pipeline_upto_grant(),
            meck:expect(?PG, get_grant, fun(_C, _G) -> {ok, grant(#{})} end),
            Ctx = run_ctx(#{now => {{2028, 1, 1}, {0, 0, 0}}}),
            ?assertEqual({deny, grant_expired}, authorize(Ctx, tool(), resource()))
        end},
        {"grant pending (未生效) -> grant_pending", fun() ->
            given_pipeline_upto_grant(),
            meck:expect(?PG, get_grant, fun(_C, _G) -> {ok, grant(#{})} end),
            Ctx = run_ctx(#{now => {{2026, 8, 1}, {0, 0, 0}}}),
            ?assertEqual({deny, grant_pending}, authorize(Ctx, tool(), resource()))
        end},
        {"grant read crash -> grant_read_failed", fun() ->
            given_pipeline_upto_grant(),
            meck:expect(?PG, get_grant, fun(_C, _G) -> erlang:error(pg_down) end),
            ?assertEqual({deny, grant_read_failed}, auth0())
        end}
    ].

%% ------------------------------------------------------------------
%% 步骤 6：capability/action/resource 匹配（目录 D7 + Grant 三元组 + 约束）
%% ------------------------------------------------------------------

step6_match_tests(_) ->
    [
        {"default D7 empty catalog: any capability -> unknown_capability", fun() ->
            given_pipeline_upto_grant(),
            given_grant_active(),
            %% setup 已期望 lookup → not_found（真实默认目录语义同）
            ?assertEqual({deny, unknown_capability}, auth0())
        end},
        {"catalog crash -> catalog_unavailable (fail closed)", fun() ->
            given_pipeline_upto_grant(),
            given_grant_active(),
            meck:expect(?CATALOG, lookup, fun(_C, _A, _R) -> erlang:error(cat_down) end),
            ?assertEqual({deny, catalog_unavailable}, auth0())
        end},
        {"catalog hit but grant has no such capability row -> not_granted", fun() ->
            given_pipeline_upto_grant(),
            given_grant_active(),
            given_catalog_entry([<<"workspace_ids">>]),
            meck:expect(?GPG, list_capabilities, fun(_C, _O, _G) -> [] end),
            ?assertEqual({deny, not_granted}, auth0())
        end},
        {"grant ws scope excludes resource workspace -> grant_scope_mismatch", fun() ->
            given_pipeline_upto_grant(),
            given_grant_active(),
            given_catalog_entry([<<"workspace_ids">>]),
            given_grant_caps(#{}),
            given_ws_member_ok(),
            meck:expect(?GPG, list_workspace_ids, fun(_C, _O, _G) -> [?WS] end),
            ?assertEqual(
                {deny, grant_scope_mismatch},
                auth_res(resource(#{workspace_id => 66}))
            )
        end},
        {"grant capability constraint narrows to other workspace -> scope_mismatch", fun() ->
            given_pipeline_upto_grant(),
            given_grant_active(),
            given_catalog_entry([<<"workspace_ids">>]),
            given_grant_caps(#{<<"workspace_ids">> => [66]}),
            given_ws_member_ok(),
            meck:expect(?GPG, list_workspace_ids, fun(_C, _O, _G) -> [?WS, 66] end),
            ?assertEqual(
                {deny, scope_mismatch}, auth_res(resource(#{workspace_id => ?WS}))
            )
        end},
        {"grant capability constraint matches workspace -> allow path continues to policy", fun() ->
                safe_meck_new(ag31_05_policy_allow),
                given_all_gates_pass(),
                given_ws_member_ok(),
                given_grant_caps(#{<<"workspace_ids">> => [?WS]}),
                meck:expect(?GPG, list_workspace_ids, fun(_C, _O, _G) -> [?WS] end),
                meck:expect(?PG, insert_effect_guarded_tx, fun(_C, _E) ->
                    {ok, ?EFFECT_ID}
                end),
                {allow, _} = auth_res(resource(#{workspace_id => ?WS})),
                ?assert(meck:called(?PG, insert_effect_guarded_tx, '_'))
            end},
        {"grant ws 范围读取崩溃 -> deny grant_scope_read_failed（MEDIUM-1，不伪装空集）", fun() ->
            given_pipeline_upto_grant(),
            given_grant_active(),
            given_catalog_entry([<<"workspace_ids">>]),
            given_grant_caps(#{}),
            meck:expect(?GPG, list_workspace_ids, fun(_C, _O, _G) ->
                erlang:error(ws_ids_db_boom)
            end),
            ?assertEqual({deny, grant_scope_read_failed}, auth0())
        end}
    ].

%% ------------------------------------------------------------------
%% 步骤 7：Resource Policy（R2 默认恒 deny；env 可覆盖）
%% ------------------------------------------------------------------

step7_policy_tests(_) ->
    [
        {"default policy module = deny everything (R2 未配置=拒绝)", fun() ->
            clear_env_overrides(),
            given_pipeline_upto_policy(),
            given_catalog_entry([<<"workspace_ids">>]),
            given_grant_active(),
            given_grant_caps(#{}),
            meck:expect(?GPG, list_workspace_ids, fun(_C, _O, _G) -> [] end),
            %% 不注入 policy env → 真默认 agent_resource_policy 恒 deny
            ?assertEqual({deny, resource_policy_denied}, auth0())
        end},
        {"env override permissive policy -> allow path proceeds", fun() ->
            safe_meck_new(ag31_05_policy_allow),
            given_all_gates_pass(),
            reset_calls([?PG]),
            {allow, _} = auth0(),
            ?assertEqual(1, meck:num_calls(?PG, insert_effect_guarded_tx, '_'))
        end},
        {"policy port crash -> resource_policy_unavailable (fail closed)", fun() ->
            given_pipeline_upto_policy(),
            given_catalog_entry([<<"workspace_ids">>]),
            given_grant_active(),
            given_grant_caps(#{}),
            meck:expect(?GPG, list_workspace_ids, fun(_C, _O, _G) -> [] end),
            Fake = ag31_05_policy_allow,
            safe_meck_new(Fake),
            meck:expect(Fake, evaluate, fun(_R, _T, _Res) -> erlang:error(policy_down) end),
            ok = application:set_env(imboy, agent_resource_policy_module, Fake),
            ?assertEqual({deny, resource_policy_unavailable}, auth0())
        end}
    ].

%% ------------------------------------------------------------------
%% 步骤 8：HITL（R3 默认政策 + 批准绑定校验）
%% ------------------------------------------------------------------

step8_hitl_tests(_) ->
    [
        {"write tool with default policy -> approval_required + binding context", fun() ->
            given_all_gates_pass(),
            {approval_required, AC} = auth_tool(#{side_effect_class => write}),
            ?assertEqual(?EFFECT_ID, maps:get(effect_id, AC)),
            ?assertEqual(?RUN, maps:get(run_id, AC)),
            ?assertEqual(<<"sha256:args">>, maps:get(args_digest, AC)),
            ?assertEqual(2, maps:get(grant_version, AC))
        end},
        {"re-auth with valid approval binding -> allow (dispatch 前全链重走)", fun() ->
            given_all_gates_pass(),
            Ctx = run_ctx(#{approval_effect_id => ?EFFECT_ID}),
            given_effect_authorized(),
            {allow, _} = authorize(Ctx, tool(#{side_effect_class => write}), resource())
        end},
        {"stale approval: args mutated -> stale_approval_args (A10 机制面)", fun() ->
            given_all_gates_pass(),
            given_effect_authorized(),
            Ctx = run_ctx(#{approval_effect_id => ?EFFECT_ID}),
            Res = resource(#{args_digest => <<"sha256:args-NEW">>}),
            ?assertEqual(
                {deny, stale_approval_args},
                authorize(Ctx, tool(#{side_effect_class => write}), Res)
            )
        end},
        {"stale approval: grant version changed -> stale_approval_grant_version", fun() ->
            given_all_gates_pass(),
            given_effect_authorized(),
            Ctx = run_ctx(#{approval_effect_id => ?EFFECT_ID}),
            meck:expect(?PG, get_grant, fun(_C, _G) -> {ok, grant(#{version => 3})} end),
            ?assertEqual(
                {deny, stale_approval_grant_version},
                authorize(Ctx, tool(#{side_effect_class => write}), resource())
            )
        end},
        {"approval binding on waiting effect (not yet approved) -> approval_not_authorized",
            fun() ->
                given_all_gates_pass(),
                given_effect_authorized(#{status => waiting_approval}),
                Ctx = run_ctx(#{approval_effect_id => ?EFFECT_ID}),
                ?assertEqual(
                    {deny, approval_not_authorized},
                    authorize(Ctx, tool(#{side_effect_class => write}), resource())
                )
            end},
        {"approval binding run mismatch -> approval_run_mismatch", fun() ->
            given_all_gates_pass(),
            given_effect_authorized(#{run_id => 999}),
            Ctx = run_ctx(#{approval_effect_id => ?EFFECT_ID}),
            ?assertEqual(
                {deny, approval_run_mismatch},
                authorize(Ctx, tool(#{side_effect_class => write}), resource())
            )
        end},
        {"approval binding effect missing -> approval_not_found", fun() ->
            given_all_gates_pass(),
            meck:expect(?PG, get_effect, fun(_C, _E) -> {error, not_found} end),
            Ctx = run_ctx(#{approval_effect_id => ?EFFECT_ID}),
            ?assertEqual(
                {deny, approval_not_found},
                authorize(Ctx, tool(#{side_effect_class => write}), resource())
            )
        end},
        {"hitl port crash -> hitl_policy_unavailable (fail closed)", fun() ->
            given_pipeline_upto_policy(),
            given_catalog_entry([<<"workspace_ids">>]),
            given_grant_active(),
            given_grant_caps(#{}),
            meck:expect(?GPG, list_workspace_ids, fun(_C, _O, _G) -> [] end),
            Fake = ag31_05_hitl_mock,
            safe_meck_new(Fake),
            ok = application:set_env(imboy, agent_hitl_policy_module, Fake),
            ?assertEqual({deny, hitl_policy_unavailable}, auth0())
        end}
    ].

%% ------------------------------------------------------------------
%% 步骤 9：CS gate（未 PASS → 凡 CS 域工具一律 deny，冻结约束）
%% ------------------------------------------------------------------

step9_cs_tests(_) ->
    [
        {"CS domain tool -> cs_gate_blocked even with everything else passing", fun() ->
            safe_meck_new(ag31_05_policy_allow),
            given_all_gates_pass(),
            ?assertEqual({deny, cs_gate_blocked}, auth_tool(#{domain => cs}))
        end},
        {"CS write tool: deny 优先于 approval_required（永不进入审批流）", fun() ->
            safe_meck_new(ag31_05_policy_allow),
            given_all_gates_pass(),
            reset_calls([?PG, ag31_05_dispatcher_mock]),
            ?assertEqual(
                {deny, cs_gate_blocked},
                auth_tool(#{domain => cs, side_effect_class => write})
            ),
            %% deny 记账：denied 行携带 cs_gate_blocked；dispatcher 恒 0
            Effect = meck:capture(first, ?PG, insert_effect_tx, ['_', '_', '_'], 2),
            ?assertEqual(denied, maps:get(decided_status, Effect)),
            ?assertEqual(cs_gate_blocked, maps:get(denial_reason, Effect)),
            ?assertEqual(0, safe_num_calls(ag31_05_dispatcher_mock, dispatch, '_'))
        end},
        {"binary domain marker cs-binary equally blocked", fun() ->
            safe_meck_new(ag31_05_policy_allow),
            given_all_gates_pass(),
            ?assertEqual(
                {deny, cs_gate_blocked}, auth_tool(#{domain => <<"cs">>})
            )
        end}
    ].

%% ------------------------------------------------------------------
%% 步骤 10：决策持久化（含 deny 记账；持久化失败绝不 allow）
%% ------------------------------------------------------------------

step10_persist_tests(_) ->
    [
        {"allow: effect persisted authorized with grant_version_checked", fun() ->
            given_all_gates_pass(),
            meck:expect(?PG, insert_effect_guarded_tx, fun(_C, Effect) ->
                ?assertEqual(authorized, maps:get(decided_status, Effect)),
                ?assertEqual(2, maps:get(grant_version_checked, Effect)),
                ?assertEqual(7, maps:get(sequence, Effect)),
                ?assertEqual(?RUN, maps:get(run_id, Effect)),
                {ok, ?EFFECT_ID}
            end),
            {allow, DC} = auth0(),
            ?assertEqual(?EFFECT_ID, maps:get(effect_id, DC)),
            ?assertEqual(2, maps:get(grant_version, DC))
        end},
        {"allow + persistence failure -> deny decision_persist_failed (绝不 allow)", fun() ->
            given_all_gates_pass(),
            meck:expect(?PG, insert_effect_guarded_tx, fun(_C, _E) ->
                {error, {effect_tx_failed, simulated_pg_down}}
            end),
            ?assertEqual({deny, decision_persist_failed}, auth0())
        end},
        {"allow + duplicate effect (同幂等键) -> deny duplicate_effect", fun() ->
            given_all_gates_pass(),
            meck:expect(?PG, insert_effect_guarded_tx, fun(_C, _E) ->
                {error, {duplicate_effect, <<"uq_ae_tool_external_idem">>}}
            end),
            ?assertEqual({deny, duplicate_effect}, auth0())
        end},
        {"allow + 守卫事务观测并发终态 -> deny run_not_running (MEDIUM-2)", fun() ->
            given_all_gates_pass(),
            meck:expect(?PG, insert_effect_guarded_tx, fun(_C, _E) ->
                {error, {run_not_active, <<"cancelled">>}}
            end),
            ?assertEqual({deny, run_not_running}, auth0()),
            ?assertEqual(0, safe_num_calls(ag31_05_dispatcher_mock, dispatch, '_'))
        end},
        {"approval_required + persistence failure -> deny decision_persist_failed", fun() ->
            given_all_gates_pass(),
            meck:expect(?PG, insert_effect_tx, fun(_C, _E, _R) ->
                {error, {effect_tx_failed, simulated_pg_down}}
            end),
            ?assertEqual(
                {deny, decision_persist_failed}, auth_tool(#{side_effect_class => write})
            )
        end},
        {"approval_required: E07 run CAS 同事务（waiting_approval + 事件）", fun() ->
            given_all_gates_pass(),
            meck:expect(?PG, insert_effect_tx, fun(_C, Effect, RunOp) ->
                ?assertEqual(waiting_approval, maps:get(decided_status, Effect)),
                {cas_run, ?RUN, running, waiting_approval, 3, undefined, _Event} = RunOp,
                {ok, ?EFFECT_ID, 4}
            end),
            {approval_required, _} = auth_tool(#{side_effect_class => write})
        end},
        {"deny: denied effect persisted with stable reason; deny 不受持久化失败影响", fun() ->
            clear_env_overrides(),
            given_pipeline_upto_policy(),
            given_catalog_entry([<<"workspace_ids">>]),
            given_grant_active(),
            given_grant_caps(#{}),
            meck:expect(?GPG, list_workspace_ids, fun(_C, _O, _G) -> [] end),
            meck:expect(?PG, insert_effect_tx, fun(_C, Effect, RunOp) ->
                ?assertEqual(none, RunOp),
                ?assertEqual(denied, maps:get(decided_status, Effect)),
                ?assertEqual(resource_policy_denied, maps:get(denial_reason, Effect)),
                erlang:error(tx_boom)
            end),
            ?assertEqual({deny, resource_policy_denied}, auth0())
        end},
        {"deny on non-running run: no effect row attempted (结构门一致 04B)", fun() ->
            given_run_status(queued),
            given_agent_identity(1, 1),
            reset_calls([?PG]),
            ?assertEqual({deny, run_not_running}, auth0()),
            ?assertEqual(0, meck:num_calls(?PG, insert_effect_tx, '_'))
        end}
    ].

%% ------------------------------------------------------------------
%% truth table：readonly/write × policy × CS × 批准绑定
%% ------------------------------------------------------------------

truth_table_tests(_) ->
    [
        {"readonly + policy allow + native -> allow", fun() ->
            safe_meck_new(ag31_05_policy_allow),
            given_all_gates_pass(),
            ?assertMatch({allow, _}, auth_tool(#{side_effect_class => readonly}))
        end},
        {"write + policy allow + native + no binding -> approval_required", fun() ->
            safe_meck_new(ag31_05_policy_allow),
            given_all_gates_pass(),
            ?assertMatch(
                {approval_required, _}, auth_tool(#{side_effect_class => write})
            )
        end},
        {"write + policy deny -> deny (policy 优先于 HITL，步骤 7 先于 8)", fun() ->
            clear_env_overrides(),
            given_pipeline_upto_policy(),
            given_catalog_entry([<<"workspace_ids">>]),
            given_grant_active(),
            given_grant_caps(#{}),
            meck:expect(?GPG, list_workspace_ids, fun(_C, _O, _G) -> [] end),
            ?assertEqual(
                {deny, resource_policy_denied},
                auth_tool(#{side_effect_class => write})
            )
        end},
        {"write + valid approval binding + policy allow -> allow", fun() ->
            safe_meck_new(ag31_05_policy_allow),
            given_all_gates_pass(),
            given_effect_authorized(),
            Ctx = run_ctx(#{approval_effect_id => ?EFFECT_ID}),
            ?assertMatch(
                {allow, _}, authorize(Ctx, tool(#{side_effect_class => write}), resource())
            )
        end},
        {"any + cs domain -> cs_gate_blocked", fun() ->
            safe_meck_new(ag31_05_policy_allow),
            given_all_gates_pass(),
            ?assertEqual(
                {deny, cs_gate_blocked},
                auth_tool(#{side_effect_class => readonly, domain => cs})
            )
        end}
    ].

%% ------------------------------------------------------------------
%% R5 dispatcher 计数（deny/approval 路径恒 0）
%% ------------------------------------------------------------------

dispatcher_tests(_) ->
    [
        {"allow: dispatcher invoked exactly once (默认无操作被 env mock 取代)", fun() ->
            safe_meck_new(ag31_05_policy_allow),
            given_all_gates_pass(),
            Fake = given_counting_dispatcher(),
            reset_calls([Fake]),
            {allow, DC} = auth0(),
            ?assertEqual({ok, dispatched}, maps:get(dispatch_result, DC)),
            ?assertEqual(1, meck:num_calls(Fake, dispatch, '_'))
        end},
        {"deny: dispatcher count = 0", fun() ->
            safe_meck_new(ag31_05_policy_allow),
            given_all_gates_pass(),
            _ = given_counting_dispatcher(),
            reset_calls([ag31_05_dispatcher_mock, ?PG]),
            meck:expect(?PG, get_grant, fun(_C, _G) -> {error, not_found} end),
            ?assertEqual({deny, grant_missing}, auth0()),
            ?assertEqual(0, meck:num_calls(ag31_05_dispatcher_mock, dispatch, '_'))
        end},
        {"approval_required: dispatcher count = 0 (不得 dispatch)", fun() ->
            safe_meck_new(ag31_05_policy_allow),
            given_all_gates_pass(),
            _ = given_counting_dispatcher(),
            reset_calls([ag31_05_dispatcher_mock, ?PG]),
            {approval_required, _} = auth_tool(#{side_effect_class => write}),
            ?assertEqual(0, meck:num_calls(ag31_05_dispatcher_mock, dispatch, '_'))
        end},
        {"dispatcher crash after authorized persist -> allow with crash surfaced", fun() ->
            safe_meck_new(ag31_05_policy_allow),
            given_all_gates_pass(),
            Fake = ag31_05_dispatcher_mock,
            safe_meck_new(Fake),
            meck:expect(Fake, dispatch, fun(_I) -> erlang:error(adapter_boom) end),
            ok = application:set_env(imboy, agent_tool_dispatcher_module, Fake),
            {allow, DC} = auth0(),
            ?assertEqual({error, dispatcher_crashed}, maps:get(dispatch_result, DC))
        end}
    ].

%% ------------------------------------------------------------------
%% 强制决策审计（真 logger handler 捕获；每 authorize 恰一条）
%% ------------------------------------------------------------------

audit_tests(_) ->
    [
        {"allow emits exactly one decision audit line", fun() ->
            safe_meck_new(ag31_05_policy_allow),
            given_all_gates_pass(),
            with_audit_capture(fun() ->
                {allow, _} = auth0(),
                ?assertEqual(1, drain_audit_count())
            end)
        end},
        {"deny line carries stable reason (zero PII: ids/digests/enums only)", fun() ->
            clear_env_overrides(),
            given_all_gates_pass(),
            with_audit_capture(fun() ->
                meck:expect(?PG, get_grant, fun(_C, _G) -> {error, not_found} end),
                {deny, grant_missing} = auth0(),
                Report = recv_audit(),
                ?assertEqual(agent_tool_decision, maps:get(what, Report)),
                ?assertEqual(deny, maps:get(outcome, Report)),
                ?assertEqual(grant_missing, maps:get(reason, Report)),
                %% recv_audit has consumed the only audit line → no extra lines
                ?assertEqual(0, drain_audit_count())
            end)
        end},
        {"approval_required line outcome=approval_required", fun() ->
            safe_meck_new(ag31_05_policy_allow),
            given_all_gates_pass(),
            with_audit_capture(fun() ->
                {approval_required, _} = auth_tool(#{side_effect_class => write}),
                Report = recv_audit(),
                ?assertEqual(approval_required, maps:get(outcome, Report))
            end)
        end}
    ].

%% ------------------------------------------------------------------
%% R6 approve_effect/4（审批摄取附加命令）
%% ------------------------------------------------------------------

approve_tests(_) ->
    [
        {"approve: effect belongs to another run -> approval_run_mismatch (四元组绑定)", fun() ->
            meck:expect(?PG, get_effect, fun(_C, _E) ->
                {ok, effect_row(#{run_id => 999})}
            end),
            ?assertEqual(
                {error, approval_run_mismatch},
                ?AUTHORIZER:approve_effect(conn(), ?RUN, ?EFFECT_ID, approve_ctx())
            )
        end},
        {"approve: effect missing -> effect_not_found", fun() ->
            meck:expect(?PG, get_effect, fun(_C, _E) -> {error, not_found} end),
            ?assertEqual(
                {error, effect_not_found},
                ?AUTHORIZER:approve_effect(conn(), ?RUN, ?EFFECT_ID, approve_ctx())
            )
        end},
        {"approve: delegates to agent_run_command (digest mismatch passthrough)", fun() ->
            meck:expect(?PG, get_effect, fun(_C, _E) ->
                {ok, effect_row(#{})}
            end),
            meck:expect(agent_run_command, approve_effect, fun(_C, _R, _E, _Ctx) ->
                {error, approval_digest_mismatch}
            end),
            ?assertEqual(
                {error, approval_digest_mismatch},
                ?AUTHORIZER:approve_effect(conn(), ?RUN, ?EFFECT_ID, approve_ctx())
            ),
            ?assert(meck:called(agent_run_command, approve_effect, '_'))
        end}
    ].

%% ------------------------------------------------------------------
%% env seam（五个全部可覆盖）
%% ------------------------------------------------------------------

env_seam_tests(_) ->
    [
        {"agent_membership_module env overrides default adapter", fun() ->
            given_run_ok(),
            given_agent_identity(1, 1),
            Fake = ag31_05_membership_mock,
            meck:new(Fake, [no_link, non_strict]),
            meck:expect(Fake, resolve_organization_state, fun(_O) -> {error, unavailable} end),
            ok = application:set_env(imboy, agent_membership_module, Fake),
            ?assertEqual({deny, org_state_unavailable}, auth0()),
            ?assertEqual(1, meck:num_calls(Fake, resolve_organization_state, '_')),
            ?assertEqual(0, meck:num_calls(?MEMBERSHIP, resolve_organization_state, '_')),
            meck:unload(Fake),
            application:unset_env(imboy, agent_membership_module)
        end},
        {"agent_capability_catalog_module env overrides default catalog", fun() ->
            given_pipeline_upto_grant(),
            given_grant_active(),
            given_grant_caps(#{}),
            Fake = ag31_05_test_catalog,
            meck:new(Fake, [no_link, non_strict]),
            meck:expect(Fake, lookup, fun(_C, _A, _R) -> {error, not_found} end),
            ok = application:set_env(imboy, agent_capability_catalog_module, Fake),
            ?assertEqual({deny, unknown_capability}, auth0()),
            ?assertEqual(1, meck:num_calls(Fake, lookup, '_')),
            meck:unload(Fake),
            application:unset_env(imboy, agent_capability_catalog_module)
        end},
        {"agent_hitl_policy_module env overrides default policy", fun() ->
            safe_meck_new(ag31_05_policy_allow),
            given_all_gates_pass(),
            Fake = ag31_05_hitl_mock,
            safe_meck_new(Fake),
            meck:expect(Fake, evaluate, fun(_R, _C) -> {deny, custom_hitl_deny} end),
            ok = application:set_env(imboy, agent_hitl_policy_module, Fake),
            ?assertEqual({deny, custom_hitl_deny}, auth0())
        end}
    ].

%% ===================================================================
%% 审计捕获（logger handler 回调；logger 是 sticky 内核模块不 meck）
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
            TestPid ! {ag31_05_audit, Report},
            ok;
        _ ->
            ok
    end.

with_audit_capture(F) ->
    case logger:add_handler(ag31_05_test_capture, ?MODULE, #{config => self()}) of
        ok ->
            ok;
        {error, {already_exist, _}} ->
            ok = logger:remove_handler(ag31_05_test_capture),
            ok = logger:add_handler(ag31_05_test_capture, ?MODULE, #{config => self()})
    end,
    try
        F()
    after
        _ = logger:remove_handler(ag31_05_test_capture)
    end,
    ok.

recv_audit() ->
    receive
        {ag31_05_audit, Report} -> Report
    after 2000 -> erlang:error(audit_line_not_emitted)
    end.

drain_audit_count() ->
    drain_audit_count(0).

drain_audit_count(N) ->
    receive
        {ag31_05_audit, _Report} -> drain_audit_count(N + 1)
    after 300 -> N
    end.

%% ===================================================================
%% fixtures
%% ===================================================================

%% {foreach} 组内共享 meck 实例：同组后测安全复用已 new 的假模块。
safe_meck_new(Mod) ->
    try
        meck:new(Mod, [no_link, non_strict]),
        ok
    catch
        error:{already_started, _} -> ok
    end.

%% 断言调用数前清零历史（组内共享实例的历史不随用例自动清零）；
%% 未被 mock 的模块安全跳过。
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

%% 未 mock 的模块计数按 0 处理。
safe_num_calls(Mod, Fun, Args) ->
    try
        meck:num_calls(Mod, Fun, Args)
    catch
        error:{not_mocked, _} -> 0
    end.

%% 显式回到默认政策（组内前测可能留下 env 覆盖）。
clear_env_overrides() ->
    lists:foreach(
        fun(K) -> application:unset_env(imboy, K) end,
        [
            agent_resource_policy_module,
            agent_hitl_policy_module,
            agent_tool_dispatcher_module
        ]
    ).

auth0() ->
    ?AUTHORIZER:authorize(run_ctx(), tool(), resource()).

auth_tool(ToolOver) ->
    ?AUTHORIZER:authorize(run_ctx(), tool(ToolOver), resource()).

auth_res(ResourceOver) ->
    ?AUTHORIZER:authorize(run_ctx(), tool(), resource(ResourceOver)).

authorize(RunCtx, Tool, Resource) ->
    ?AUTHORIZER:authorize(RunCtx, Tool, Resource).

run_ctx() -> run_ctx(#{}).

run_ctx(Over) ->
    maps:merge(
        #{
            run_id => ?RUN,
            agent_id => ?AGENT,
            organization_id => ?ORG,
            now => ?NOW,
            conn => conn()
        },
        Over
    ).

tool() -> tool(#{}).

tool(Over) ->
    maps:merge(
        #{
            tool_id => <<"tool.demo.v1">>,
            capability => <<"demo.write">>,
            action => <<"create">>,
            risk_level => low,
            side_effect_class => readonly
        },
        Over
    ).

resource() -> resource(#{}).

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

approve_ctx() ->
    #{
        args_digest => <<"sha256:args">>,
        approval_ref => <<"appr-1">>,
        actor_id => <<"human-1">>,
        now => ?NOW
    }.

run(Over) ->
    maps:merge(
        #{
            id => ?RUN,
            version => 3,
            status => running,
            grant_id => ?GRANT,
            agent_id => ?AGENT,
            organization_id => ?ORG,
            workspace_id => undefined,
            idempotency_key => <<"idem-1">>,
            trigger_type => message
        },
        Over
    ).

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

conn() ->
    self().

%% ---- 给定条件桩 ----

given_run_ok() ->
    meck:expect(?PG, get_run, fun(_C, _R) -> {ok, run(#{})} end).

given_run_status(Status) ->
    meck:expect(?PG, get_run, fun(_C, _R) -> {ok, run(#{status => Status})} end).

given_agent_identity(AccountType, Status) ->
    meck:expect(?PG, get_agent_identity, fun(_C, _A) ->
        {ok, #{account_type => AccountType, status => Status}}
    end).

given_ws_member_ok() ->
    meck:expect(?MEMBERSHIP, resolve_workspace_membership, fun(_O, _W, _A) ->
        {ok, #{status => active, role => member, version => 9}}
    end).

given_org_state_ok() ->
    meck:expect(?MEMBERSHIP, resolve_organization_state, fun(_O) ->
        {ok, #{status => active, version => 5}}
    end).

%% 步骤 1 全过（run gate 通过；后续按需）
given_pipeline_upto_ws() ->
    given_run_ok(),
    given_agent_identity(1, 1),
    given_org_state_ok(),
    meck:expect(?MEMBERSHIP, resolve_organization_membership, fun(_O, _A) ->
        {ok, #{status => active, role => member, version => 7}}
    end).

given_pipeline_upto_grant() ->
    given_pipeline_upto_ws(),
    %% deny 决策也落 agent_effect 行（步骤 10 记账）；专门用例自行覆盖此期望
    meck:expect(?PG, insert_effect_tx, fun(_C, _E, _R) -> {ok, 9002, undefined} end),
    %% allow 决策走 MEDIUM-2 守卫事务（A2 review）：默认成功期望与 tx 同型
    meck:expect(?PG, insert_effect_guarded_tx, fun(_C, _E) -> {ok, 9002} end),
    given_id_generators().

given_id_generators() ->
    meck:expect(?PG, next_id, fun
        (agent_effect) -> ?EFFECT_ID;
        (agent_run_event) -> 9100
    end),
    meck:expect(?PG, next_effect_sequence, fun(_C, _R) -> 7 end),
    ok.

given_pipeline_upto_policy() ->
    given_pipeline_upto_grant(),
    given_grant_active().

given_grant_active() ->
    meck:expect(?PG, get_grant, fun(_C, _G) -> {ok, grant(#{})} end),
    meck:expect(?GPG, list_workspace_ids, fun(_C, _O, _G) -> [] end),
    meck:expect(?GPG, list_capabilities, fun(_C, _O, _G) -> [cap_row(#{})] end).

given_grant_caps(Constraint) ->
    meck:expect(?GPG, list_capabilities, fun(_C, _O, _G) -> [cap_row(Constraint)] end).

cap_row(Constraint) ->
    #{
        capability => <<"demo.write">>,
        action => <<"create">>,
        resource_type => <<"demo">>,
        constraint => Constraint
    }.

given_catalog_entry(LegalKeys) ->
    meck:expect(?CATALOG, lookup, fun(C, A, R) ->
        {ok, #{
            capability => C, action => A, resource_type => R, legal_constraint_keys => LegalKeys
        }}
    end).

given_effect_authorized() ->
    given_effect_authorized(#{}).

given_effect_authorized(Over) ->
    meck:expect(?PG, get_effect, fun(_C, _E) -> {ok, effect_row(Over)} end).

%% 步骤 7 前全通 + 政策放行 + dispatcher 计数（步骤 8 之前路径完整）
given_all_gates_pass() ->
    given_pipeline_upto_policy(),
    given_catalog_entry([<<"workspace_ids">>]),
    given_grant_caps(#{}),
    meck:expect(?GPG, list_workspace_ids, fun(_C, _O, _G) -> [] end),
    PolicyFake = ag31_05_policy_allow,
    safe_meck_new(PolicyFake),
    meck:expect(PolicyFake, evaluate, fun(_R, _T, _Res) -> allow end),
    ok = application:set_env(imboy, agent_resource_policy_module, PolicyFake),
    %% 默认持久化成功（专门用例在其后覆盖此期望）
    meck:expect(?PG, insert_effect_tx, fun(_C, _E, _R) -> {ok, ?EFFECT_ID, undefined} end),
    %% allow 路径守卫事务默认成功（MEDIUM-2；专门用例自行覆盖）
    meck:expect(?PG, insert_effect_guarded_tx, fun(_C, _E) -> {ok, ?EFFECT_ID} end),
    given_id_generators().

given_counting_dispatcher() ->
    Fake = ag31_05_dispatcher_mock,
    safe_meck_new(Fake),
    meck:expect(Fake, dispatch, fun(_Input) -> {ok, dispatched} end),
    ok = application:set_env(imboy, agent_tool_dispatcher_module, Fake),
    Fake.
