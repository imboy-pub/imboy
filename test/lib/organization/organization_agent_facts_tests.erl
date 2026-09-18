-module(organization_agent_facts_tests).

%% ORG-06 Agent Organization Boundary 公共事实适配器 contract tests（真 PG）。
%%
%% 覆盖（Agent Organization Contract §8 + 计划 ORG-06 TESTS + 派工卡）：
%%   * resolve organization state：active/archived 如实上报 + allowed fail closed
%%     （AG-ORG-A10 Organization 侧事实）；
%%   * multi-org：Agent 属多 Org，membership 状态彼此独立；
%%   * member role restriction：Agent admin 行 allowed=false（fail closed）；
%%     Agent owner 行被 ORG-01 DB invariant（126 唯一索引）兜底拒绝；
%%   * suspend/remove 即时失效：UPDATE 后下一次 resolve 立即 allowed=false；
%%   * cross-workspace：validate workspace ownership 跨 Org = false；
%%     Org member 但无 Workspace member → 404 拒（AG-ORG-A03 Organization 侧事实）；
%%   * consume default workspace：只读消费 ORG-05 显式关系（指路事实非权限，
%%     不回落 min-ID）；
%%   * 非 Agent 主体 403 拒（边界仅服务 account_type=1）；
%%   * 重复 fact read 零副作用（版本不变 + 零审计事件）。
%%
%% 运行：IMBOYENV=local make eunit-local t=organization_agent_facts_tests

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% ===================================================================
%% resolve organization state（active/archived fail closed）
%% ===================================================================

resolve_organization_fail_closed_on_archive_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        OwnerUid = new_uid(),
        OrgId = new_uid(),
        try
            create_human(OwnerUid),
            create_org_with_owner(OrgId, OwnerUid),
            %% active → allowed=true，fact_version 非负整数
            {ok, Fact1} = organization_agent_facts_app:resolve_organization(OrgId),
            ?assertEqual(organization_state, maps:get(kind, Fact1)),
            ?assertEqual(<<"active">>, maps:get(status, Fact1)),
            ?assert(maps:get(allowed, Fact1)),
            ?assert(is_integer(maps:get(fact_version, Fact1))),
            ?assert(is_integer(maps:get(observed_at, Fact1))),
            %% archive（C16 既有 command）→ 如实上报 archived + allowed=false
            {ok, _} = organization_lifecycle:archive(OwnerUid, OrgId),
            {ok, Fact2} = organization_agent_facts_app:resolve_organization(OrgId),
            ?assertEqual(<<"archived">>, maps:get(status, Fact2)),
            ?assertNot(maps:get(allowed, Fact2)),
            %% restore → 放行恢复
            {ok, _} = organization_lifecycle:restore(OwnerUid, OrgId),
            {ok, Fact3} = organization_agent_facts_app:resolve_organization(OrgId),
            ?assertEqual(<<"active">>, maps:get(status, Fact3)),
            ?assert(maps:get(allowed, Fact3)),
            %% 未知 org 404 / 非法 id 400
            ?assertMatch(
                {error, {404, _}}, organization_agent_facts_app:resolve_organization(new_uid())
            ),
            ?assertMatch({error, {400, _}}, organization_agent_facts_app:resolve_organization(0))
        after
            cleanup([OrgId], [], [OwnerUid])
        end
    end).

%% ===================================================================
%% multi-org 独立性 + suspend/remove 即时失效
%% ===================================================================

multi_org_independent_and_suspend_remove_immediate_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        OwnerUid = new_uid(),
        OrgA = new_uid(),
        OrgB = new_uid(),
        AgentId = new_uid(),
        try
            create_human(OwnerUid),
            create_agent(AgentId),
            create_org_with_owner(OrgA, OwnerUid),
            create_org_with_owner(OrgB, OwnerUid),
            insert_member(OrgA, AgentId, <<"member">>, <<"active">>),
            insert_member(OrgB, AgentId, <<"member">>, <<"active">>),
            %% 两 Org 均 allowed，role=member
            {ok, FactA1} = organization_agent_facts_app:resolve_membership(OrgA, AgentId),
            {ok, FactB1} = organization_agent_facts_app:resolve_membership(OrgB, AgentId),
            ?assert(maps:get(allowed, FactA1)),
            ?assert(maps:get(allowed, FactB1)),
            ?assertEqual(<<"member">>, maps:get(role, FactA1)),
            ?assertEqual(organization_membership, maps:get(kind, FactA1)),
            %% OrgA suspend（直接改事实行 = lifecycle 命令的最终效果）
            {ok, _} = elib_pg:query(
                <<
                    "UPDATE public.organization_member SET status = 'suspended',"
                    " updated_at = CURRENT_TIMESTAMP WHERE organization_id = $1 AND user_id = $2"
                >>,
                [OrgA, AgentId]
            ),
            %% 即时失效：下一次 resolve 立即 allowed=false（版本化事实如实上报）
            {ok, FactA2} = organization_agent_facts_app:resolve_membership(OrgA, AgentId),
            ?assertEqual(<<"suspended">>, maps:get(status, FactA2)),
            ?assertNot(maps:get(allowed, FactA2)),
            %% multi-org 独立：OrgB 不受影响
            {ok, FactB2} = organization_agent_facts_app:resolve_membership(OrgB, AgentId),
            ?assertEqual(<<"active">>, maps:get(status, FactB2)),
            ?assert(maps:get(allowed, FactB2)),
            %% remove 即时失效同理
            {ok, _} = elib_pg:query(
                <<
                    "UPDATE public.organization_member SET status = 'removed',"
                    " updated_at = CURRENT_TIMESTAMP WHERE organization_id = $1 AND user_id = $2"
                >>,
                [OrgB, AgentId]
            ),
            {ok, FactB3} = organization_agent_facts_app:resolve_membership(OrgB, AgentId),
            ?assertEqual(<<"removed">>, maps:get(status, FactB3)),
            ?assertNot(maps:get(allowed, FactB3)),
            %% org_status 一并携带（消费方可判定 archived 来源的 fail closed）
            ?assertEqual(<<"active">>, maps:get(org_status, FactA2))
        after
            cleanup([OrgA, OrgB], [], [OwnerUid, AgentId])
        end
    end).

%% ===================================================================
%% member role restriction（Agent 最高 role=member）
%% ===================================================================

member_role_restriction_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        OwnerUid = new_uid(),
        OrgId = new_uid(),
        AdminAgent = new_uid(),
        OwnerAgent = new_uid(),
        try
            create_human(OwnerUid),
            create_agent(AdminAgent),
            create_agent(OwnerAgent),
            create_org_with_owner(OrgId, OwnerUid),
            %% 反例：Agent 持 admin 行（127 invariant 只兜 owner，admin 无 DB
            %% guard——边界必须在 fact 侧 fail closed）
            insert_member(OrgId, AdminAgent, <<"admin">>, <<"active">>),
            {ok, Fact} = organization_agent_facts_app:resolve_membership(OrgId, AdminAgent),
            ?assertEqual(<<"admin">>, maps:get(role, Fact)),
            ?assertNot(maps:get(allowed, Fact)),
            %% 反例：Agent 作 owner 行被 126 唯一索引兜底拒绝
            %% （org 已有 Human owner 的 active owner 行，第二行撞
            %% uq_organization_member_single_active_owner）
            InsertResult = elib_pg:query(
                <<
                    "INSERT INTO public.organization_member"
                    " (organization_id, user_id, role, joined_at, status)"
                    " VALUES ($1, $2, 'owner', CURRENT_TIMESTAMP, 'active')"
                >>,
                [OrgId, OwnerAgent]
            ),
            ?assertMatch({error, _}, InsertResult),
            %% 审计兜底：owner 行不存在
            {ok, CountRows} = elib_pg:query(
                <<
                    "SELECT COUNT(*) AS c FROM public.organization_member"
                    " WHERE organization_id = $1 AND role = 'owner'"
                >>,
                [OrgId]
            ),
            ?assertEqual(1, elib_cnv:safe_to_integer(maps:get(<<"c">>, hd(CountRows))))
        after
            cleanup([OrgId], [], [OwnerUid, AdminAgent, OwnerAgent])
        end
    end).

%% ===================================================================
%% 非 Agent 主体 / 非成员 / 未知 Org
%% ===================================================================

resolve_membership_boundary_negative_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        OwnerUid = new_uid(),
        Human = new_uid(),
        AgentId = new_uid(),
        OtherAgent = new_uid(),
        OrgId = new_uid(),
        try
            create_human(OwnerUid),
            create_human(Human),
            create_agent(AgentId),
            create_agent(OtherAgent),
            create_org_with_owner(OrgId, OwnerUid),
            insert_member(OrgId, AgentId, <<"member">>, <<"active">>),
            %% 非 Agent 主体（Human）走 Agent 边界 → 403 fail closed
            ?assertMatch(
                {error, {403, _}}, organization_agent_facts_app:resolve_membership(OrgId, Human)
            ),
            %% Agent 非该 Org 成员 → 404
            ?assertMatch(
                {error, {404, _}},
                organization_agent_facts_app:resolve_membership(OrgId, OtherAgent)
            ),
            ?assertMatch(
                {error, {404, _}},
                organization_agent_facts_app:resolve_membership(new_uid(), AgentId)
            ),
            %% 非法 id → 400
            ?assertMatch(
                {error, {400, _}}, organization_agent_facts_app:resolve_membership(OrgId, 0)
            )
        after
            cleanup([OrgId], [], [OwnerUid, Human, AgentId, OtherAgent])
        end
    end).

%% ===================================================================
%% workspace membership facts + cross-workspace ownership
%% ===================================================================

workspace_membership_and_cross_org_validation_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        OwnerUid = new_uid(),
        OrgA = new_uid(),
        OrgB = new_uid(),
        WsA = new_uid(),
        WsB = new_uid(),
        AgentId = new_uid(),
        try
            create_human(OwnerUid),
            create_agent(AgentId),
            create_org_with_owner(OrgA, OwnerUid),
            create_org_with_owner(OrgB, OwnerUid),
            create_workspace(WsA, OwnerUid, OrgA),
            create_workspace(WsB, OwnerUid, OrgB),
            insert_member(OrgA, AgentId, <<"member">>, <<"active">>),
            insert_ws_member(WsA, AgentId, <<"member">>, <<"active">>),
            %% Agent 是 OrgA member 但**无 WsB membership** → 404（AG-ORG-A03：
            %% Org + Grant 之外的 Workspace 事实缺失 = deny 的确定性输入）
            ?assertMatch(
                {error, {404, _}},
                organization_agent_facts_app:resolve_workspace_membership(WsB, AgentId)
            ),
            %% WsA 成员 fact：allowed
            {ok, WsFact} = organization_agent_facts_app:resolve_workspace_membership(WsA, AgentId),
            ?assertEqual(workspace_membership, maps:get(kind, WsFact)),
            ?assert(maps:get(allowed, WsFact)),
            ?assertEqual(OrgA, maps:get(workspace_organization_id, WsFact)),
            %% ws membership removed → 即时 allowed=false
            {ok, _} = elib_pg:query(
                <<
                    "UPDATE public.workspace_member SET status = 'removed',"
                    " updated_at = CURRENT_TIMESTAMP WHERE workspace_id = $1 AND user_id = $2"
                >>,
                [WsA, AgentId]
            ),
            {ok, WsFact2} = organization_agent_facts_app:resolve_workspace_membership(WsA, AgentId),
            ?assertEqual(<<"removed">>, maps:get(status, WsFact2)),
            ?assertNot(maps:get(allowed, WsFact2)),
            %% cross-workspace ownership：WsB 不属于 OrgA → same_org=false
            {ok, CrossFact} = organization_agent_facts_app:validate_workspace_in_org(OrgA, WsB),
            ?assertEqual(workspace_ownership, maps:get(kind, CrossFact)),
            ?assertNot(maps:get(same_org, CrossFact)),
            ?assertNot(maps:get(allowed, CrossFact)),
            %% 同 Org 校验放行
            {ok, OwnFact} = organization_agent_facts_app:validate_workspace_in_org(OrgA, WsA),
            ?assert(maps:get(same_org, OwnFact)),
            ?assert(maps:get(allowed, OwnFact)),
            %% ws archived → membership fact fail closed（WsB 归档）
            {ok, _} = elib_pg:query(
                <<
                    "UPDATE public.workspace SET status = 'archived',"
                    " updated_at = CURRENT_TIMESTAMP WHERE id = $1"
                >>,
                [WsB]
            ),
            {ok, CrossFact2} = organization_agent_facts_app:validate_workspace_in_org(OrgB, WsB),
            ?assert(maps:get(same_org, CrossFact2)),
            ?assertNot(maps:get(allowed, CrossFact2))
        after
            cleanup([OrgA, OrgB], [WsA, WsB], [OwnerUid, AgentId])
        end
    end).

%% ===================================================================
%% consume default workspace（只读消费 ORG-05；指路事实非权限）
%% ===================================================================

consume_default_workspace_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        OwnerUid = new_uid(),
        OrgId = new_uid(),
        WsA = new_uid(),
        WsB = new_uid(),
        try
            create_human(OwnerUid),
            create_org_with_owner(OrgId, OwnerUid),
            create_workspace(WsA, OwnerUid, OrgId),
            create_workspace(WsB, OwnerUid, OrgId),
            %% 未设默认 → null（不回落 min-ID 推导，C05 TRANSITION 完成态）
            {ok, Fact0} = organization_agent_facts_app:consume_default_workspace(OrgId),
            ?assertEqual(default_workspace, maps:get(kind, Fact0)),
            ?assertEqual(null, maps:get(workspace_id, Fact0)),
            ?assertEqual(run_creation_candidate_only, maps:get(usage, Fact0)),
            %% 只读消费 ORG-05 显式关系：set 后读到；指路事实不带 allowed
            {ok, changed} = organization_default_workspace_app:set(OwnerUid, OrgId, WsB),
            {ok, Fact1} = organization_agent_facts_app:consume_default_workspace(OrgId),
            ?assertEqual(WsB, maps:get(workspace_id, Fact1)),
            ?assertEqual(false, maps:is_key(allowed, Fact1)),
            %% clear 幂等回收 → null
            {ok, cleared} = organization_default_workspace_app:clear(OwnerUid, OrgId),
            {ok, Fact2} = organization_agent_facts_app:consume_default_workspace(OrgId),
            ?assertEqual(null, maps:get(workspace_id, Fact2)),
            %% 未知 org 404
            ?assertMatch(
                {error, {404, _}}, organization_agent_facts_app:consume_default_workspace(new_uid())
            )
        after
            cleanup([OrgId], [WsA, WsB], [OwnerUid])
        end
    end).

%% ===================================================================
%% 重复 fact read 零副作用（版本不变 + 零审计事件）
%% ===================================================================

repeated_read_zero_side_effect_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        OwnerUid = new_uid(),
        OrgId = new_uid(),
        AgentId = new_uid(),
        Self = self(),
        ok = meck:new(elib_log, [passthrough, no_link]),
        ok = meck:expect(elib_log, internal_log, fun(Level, Msg, M, L) ->
            Self ! {audit, Msg},
            meck:passthrough([Level, Msg, M, L])
        end),
        try
            create_human(OwnerUid),
            create_agent(AgentId),
            create_org_with_owner(OrgId, OwnerUid),
            insert_member(OrgId, AgentId, <<"member">>, <<"active">>),
            drain(),
            %% 连续两次 resolve：版本稳定（DB 无写）+ 零审计事件
            {ok, Fact1} = organization_agent_facts_app:resolve_membership(OrgId, AgentId),
            {ok, Fact2} = organization_agent_facts_app:resolve_membership(OrgId, AgentId),
            ?assertEqual(
                maps:get(fact_version, Fact1), maps:get(fact_version, Fact2)
            ),
            ?assertEqual([], drain())
        after
            _ = (catch meck:unload(elib_log)),
            cleanup([OrgId], [], [OwnerUid, AgentId])
        end
    end).

%% ===================================================================
%% Fixture / cleanup
%% ===================================================================

new_uid() ->
    erlang:system_time(millisecond) * 1000 + rand:uniform(999).

create_human(Uid) ->
    create_user(Uid, 0).

create_agent(Uid) ->
    create_user(Uid, 1).

create_user(Uid, AccountType) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.\"user\" (id, account, password, reg_ip, reg_cosv, account_type)"
            " VALUES ($1, $2, 'x', '127.0.0.1', '', $3)"
        >>,
        [Uid, <<"org06", (integer_to_binary(Uid))/binary>>, AccountType]
    ),
    ok.

create_org_with_owner(OrgId, OwnerUid) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.organization (id, name, owner_id)"
            " VALUES ($1, 'org06-agent-facts-probe', $2)"
        >>,
        [OrgId, OwnerUid]
    ),
    ok.

create_workspace(WsId, OwnerUid, OrgId) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.workspace (id, name, owner_id, organization_id)"
            " VALUES ($1, $2, $3, $4)"
        >>,
        [WsId, <<"org06-ws-probe">>, OwnerUid, OrgId]
    ),
    ok.

insert_member(OrgId, Uid, Role, Status) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.organization_member"
            " (organization_id, user_id, role, joined_at, status)"
            " VALUES ($1, $2, $3, CURRENT_TIMESTAMP, $4)"
        >>,
        [OrgId, Uid, Role, Status]
    ),
    ok.

insert_ws_member(WsId, Uid, Role, Status) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.workspace_member"
            " (workspace_id, user_id, role, joined_at, status)"
            " VALUES ($1, $2, $3, CURRENT_TIMESTAMP, $4)"
        >>,
        [WsId, Uid, Role, Status]
    ),
    ok.

%% 清理顺序（FK RESTRICT 链）：
%% organization_default_workspace(RESTRICT) → workspace(RESTRICT org) →
%% organization（成员行 CASCADE；owner 用户受 126 RESTRICT 须先删 org）→ user。
cleanup(OrgIds, WsIds, Uids) ->
    lists:foreach(
        fun(OrgId) ->
            _ = elib_pg:query(
                <<"DELETE FROM public.organization_default_workspace WHERE organization_id = $1">>,
                [OrgId]
            )
        end,
        OrgIds
    ),
    lists:foreach(
        fun(WsId) ->
            _ = elib_pg:query(<<"DELETE FROM public.workspace WHERE id = $1">>, [WsId])
        end,
        WsIds
    ),
    lists:foreach(
        fun(OrgId) ->
            _ = elib_pg:query(<<"DELETE FROM public.organization WHERE id = $1">>, [OrgId])
        end,
        OrgIds
    ),
    lists:foreach(
        fun(Uid) ->
            _ = elib_pg:query(<<"DELETE FROM public.\"user\" WHERE id = $1">>, [Uid])
        end,
        Uids
    ),
    ok.

drain() ->
    drain([]).

drain(Acc) ->
    receive
        {audit, Msg} -> drain([Msg | Acc])
    after 200 ->
        lists:reverse(Acc)
    end.
