-module(organization_agent_membership_tests).

%% ORG-06 Agent 成员生命周期受控命令 contract tests（真 PG）。
%%
%% 覆盖（Agent Organization Contract §3 + 计划 ORG-06 TESTS + 派工卡）：
%%   * attach 恒以 role='member' 落库（member-only；幂等 unchanged 不重复审计）；
%%   * 操作人必须是 active Human owner/admin：member/Agent/非成员 403、
%%     未知 org 404、目标非 Agent 403、目标不存在 404；
%%   * suspend/restore/remove 状态机 + 幂等（重复命令稳定结果、不重复审计）；
%%     suspend 后 facts 即时 denied（suspend/remove 即时失效）；
%%   * stale version 拒：ExpectedVersion 与当前 fact_version 不符 → 409；
%%     行不存在带版本创建预期 → 409；
%%   * archived Org 一律 409（C16 fail closed）；
%%   * attach 对 admin 行 409 role_conflict（不静默改写治理角色）。
%%
%% 运行：IMBOYENV=local make eunit-local t=organization_agent_membership_tests

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(AGENT_EVENTS, [
    organization_agent_member_attached,
    organization_agent_member_suspended,
    organization_agent_member_restored,
    organization_agent_member_removed
]).

%% ===================================================================
%% attach：member-only 落库 + 幂等 + 审计一次
%% ===================================================================

attach_member_only_idempotent_with_audit_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        OwnerUid = new_uid(),
        OrgId = new_uid(),
        AgentId = new_uid(),
        Self = self(),
        with_audit_capture(Self, fun() ->
            try
                create_human(OwnerUid),
                create_agent(AgentId),
                create_org_with_owner(OrgId, OwnerUid),
                drain(),
                %% ① attach → changed，角色恒 member
                {ok, R1} =
                    organization_agent_membership_app:attach(
                        OwnerUid, OrgId, AgentId, undefined, <<"idem-attach-1">>
                    ),
                ?assertEqual(<<"active">>, maps:get(status, R1)),
                ?assertEqual(<<"member">>, maps:get(role, R1)),
                V1 = maps:get(fact_version, R1),
                ?assert(is_integer(V1)),
                member_row_is(OrgId, AgentId, <<"member">>, <<"active">>),
                %% ② 审计恰一次，且幂等键入审计
                Events1 = drain(),
                ?assertEqual(1, count_events(organization_agent_member_attached, Events1)),
                ?assert(lists:any(fun(E) -> lists:member(<<"idem-attach-1">>, E) end, Events1)),
                %% ③ 重复 attach → unchanged，无第二次审计
                {ok, R2} =
                    organization_agent_membership_app:attach(
                        OwnerUid, OrgId, AgentId, undefined, <<"idem-attach-2">>
                    ),
                ?assertEqual(unchanged, maps:get(status_tag, R2)),
                ?assertEqual(V1, maps:get(fact_version, R2)),
                ?assertEqual([], drain()),
                %% ④ attach 后 facts 即时 allowed
                {ok, Fact} = organization_agent_facts_app:resolve_membership(OrgId, AgentId),
                ?assert(maps:get(allowed, Fact)),
                %% ⑤ 参数形状：非法 id / 非法幂等键 → 400
                ?assertMatch(
                    {error, {400, _}},
                    organization_agent_membership_app:attach(OwnerUid, 0, AgentId, undefined, <<>>)
                ),
                ?assertMatch(
                    {error, {400, _}},
                    organization_agent_membership_app:attach(
                        OwnerUid, OrgId, AgentId, undefined, bad_key
                    )
                )
            after
                cleanup([OrgId], [], [OwnerUid, AgentId])
            end
        end)
    end).

%% ===================================================================
%% 操作人权限：Human owner/admin only（合同 §3）
%% ===================================================================

operator_authorization_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        OwnerUid = new_uid(),
        AdminUid = new_uid(),
        MemberUid = new_uid(),
        OperatorAgent = new_uid(),
        TargetAgent = new_uid(),
        Outsider = new_uid(),
        OrgId = new_uid(),
        try
            create_human(OwnerUid),
            create_human(AdminUid),
            create_human(MemberUid),
            create_agent(OperatorAgent),
            create_agent(TargetAgent),
            create_org_with_owner(OrgId, OwnerUid),
            insert_member(OrgId, AdminUid, <<"admin">>, <<"active">>),
            insert_member(OrgId, MemberUid, <<"member">>, <<"active">>),
            insert_member(OrgId, OperatorAgent, <<"member">>, <<"active">>),
            %% ① admin 可 attach（Human owner/admin 都可管理）
            {ok, _} =
                organization_agent_membership_app:attach(
                    AdminUid, OrgId, TargetAgent, undefined, <<"idem-admin">>
                ),
            %% ② Human member 操作人 → 403
            ?assertMatch(
                {error, {403, _}},
                organization_agent_membership_app:attach(
                    MemberUid, OrgId, TargetAgent, undefined, <<>>
                )
            ),
            %% ③ Agent 操作人（即使角色 member）→ 403（必须 Human）
            ?assertMatch(
                {error, {403, _}},
                organization_agent_membership_app:attach(
                    OperatorAgent, OrgId, TargetAgent, undefined, <<>>
                )
            ),
            %% ④ 非成员操作人 → 403
            create_human(Outsider),
            ?assertMatch(
                {error, {403, _}},
                organization_agent_membership_app:attach(
                    Outsider, OrgId, TargetAgent, undefined, <<>>
                )
            ),
            %% ⑤ 未知 org → 404
            ?assertMatch(
                {error, {404, _}},
                organization_agent_membership_app:attach(
                    OwnerUid, new_uid(), TargetAgent, undefined, <<>>
                )
            ),
            %% ⑥ 目标非 Agent（Human）→ 403；目标用户不存在 → 404
            ?assertMatch(
                {error, {403, _}},
                organization_agent_membership_app:attach(
                    OwnerUid, OrgId, MemberUid, undefined, <<>>
                )
            ),
            ?assertMatch(
                {error, {404, _}},
                organization_agent_membership_app:attach(
                    OwnerUid, OrgId, new_uid(), undefined, <<>>
                )
            ),
            %% ⑦ 未知操作人 → 403
            ?assertMatch(
                {error, {403, _}},
                organization_agent_membership_app:attach(
                    new_uid(), OrgId, TargetAgent, undefined, <<>>
                )
            )
        after
            cleanup([OrgId], [], [
                OwnerUid, AdminUid, MemberUid, OperatorAgent, TargetAgent, Outsider
            ])
        end
    end).

%% ===================================================================
%% suspend/restore/remove 状态机 + 即时失效 + 幂等
%% ===================================================================

suspend_restore_remove_lifecycle_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        OwnerUid = new_uid(),
        OrgId = new_uid(),
        AgentId = new_uid(),
        Self = self(),
        with_audit_capture(Self, fun() ->
            try
                create_human(OwnerUid),
                create_agent(AgentId),
                create_org_with_owner(OrgId, OwnerUid),
                {ok, _} =
                    organization_agent_membership_app:attach(
                        OwnerUid, OrgId, AgentId, undefined, <<"idem-l0">>
                    ),
                drain(),
                %% ① suspend → changed + facts 即时 denied
                {ok, R1} =
                    organization_agent_membership_app:suspend(
                        OwnerUid, OrgId, AgentId, undefined, <<"idem-s1">>
                    ),
                ?assertEqual(<<"suspended">>, maps:get(status, R1)),
                ?assertEqual(1, count_events(organization_agent_member_suspended, drain())),
                {ok, Fact} = organization_agent_facts_app:resolve_membership(OrgId, AgentId),
                ?assertNot(maps:get(allowed, Fact)),
                member_row_is(OrgId, AgentId, <<"member">>, <<"suspended">>),
                %% ② 重复 suspend → unchanged 不重复审计
                {ok, R2} =
                    organization_agent_membership_app:suspend(
                        OwnerUid, OrgId, AgentId, undefined, <<"idem-s2">>
                    ),
                ?assertEqual(unchanged, maps:get(status_tag, R2)),
                ?assertEqual([], drain()),
                %% ③ restore → changed active；再 restore 幂等
                {ok, R3} =
                    organization_agent_membership_app:restore(
                        OwnerUid, OrgId, AgentId, undefined, <<"idem-r1">>
                    ),
                ?assertEqual(<<"active">>, maps:get(status, R3)),
                ?assertEqual(1, count_events(organization_agent_member_restored, drain())),
                {ok, _} =
                    organization_agent_membership_app:restore(
                        OwnerUid, OrgId, AgentId, undefined, <<"idem-r2">>
                    ),
                ?assertEqual([], drain()),
                %% ④ remove（active → removed）→ changed；facts 即时 denied
                {ok, R4} =
                    organization_agent_membership_app:remove(
                        OwnerUid, OrgId, AgentId, undefined, <<"idem-x1">>
                    ),
                ?assertEqual(<<"removed">>, maps:get(status, R4)),
                ?assertEqual(1, count_events(organization_agent_member_removed, drain())),
                {ok, Fact2} = organization_agent_facts_app:resolve_membership(OrgId, AgentId),
                ?assertNot(maps:get(allowed, Fact2)),
                %% ⑤ removed 再 restore → 409；重复 remove 幂等 unchanged
                ?assertMatch(
                    {error, {409, _}},
                    organization_agent_membership_app:restore(
                        OwnerUid, OrgId, AgentId, undefined, <<>>
                    )
                ),
                {ok, R5} =
                    organization_agent_membership_app:remove(
                        OwnerUid, OrgId, AgentId, undefined, <<"idem-x2">>
                    ),
                ?assertEqual(unchanged, maps:get(status_tag, R5)),
                ?assertEqual([], drain()),
                %% ⑥ removed 后 attach → 显式重投为 active member
                {ok, R6} =
                    organization_agent_membership_app:attach(
                        OwnerUid, OrgId, AgentId, undefined, <<"idem-a2">>
                    ),
                ?assertEqual(<<"active">>, maps:get(status, R6)),
                ?assertEqual(<<"member">>, maps:get(role, R6)),
                member_row_is(OrgId, AgentId, <<"member">>, <<"active">>)
            after
                cleanup([OrgId], [], [OwnerUid, AgentId])
            end
        end)
    end).

%% ===================================================================
%% stale version 拒（乐观并发）
%% ===================================================================

stale_version_rejected_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        OwnerUid = new_uid(),
        OrgId = new_uid(),
        AgentId = new_uid(),
        try
            create_human(OwnerUid),
            create_agent(AgentId),
            create_org_with_owner(OrgId, OwnerUid),
            %% ① 行不存在带版本创建预期 → stale 409（当前版本按 0 语义）
            ?assertMatch(
                {error, {409, _}},
                organization_agent_membership_app:attach(OwnerUid, OrgId, AgentId, 1, <<>>)
            ),
            {ok, R1} =
                organization_agent_membership_app:attach(
                    OwnerUid, OrgId, AgentId, undefined, <<"idem-v0">>
                ),
            V1 = maps:get(fact_version, R1),
            %% ② 用当前版本 suspend → changed，版本推进
            {ok, R2} =
                organization_agent_membership_app:suspend(
                    OwnerUid, OrgId, AgentId, V1, <<"idem-v1">>
                ),
            V2 = maps:get(fact_version, R2),
            ?assert(is_integer(V2)),
            %% ③ 用旧版本 V1 再操作 → stale 409（即使状态机本可幂等命中）
            ?assertMatch(
                {error, {409, _}},
                organization_agent_membership_app:suspend(OwnerUid, OrgId, AgentId, V1, <<>>)
            ),
            %% ④ 用当前版本 V2 → fresh（幂等 unchanged）
            {ok, R3} =
                organization_agent_membership_app:suspend(
                    OwnerUid, OrgId, AgentId, V2, <<"idem-v2">>
                ),
            ?assertEqual(unchanged, maps:get(status_tag, R3)),
            %% ⑤ stale 拒同样适用于 facts 版本来源（resolve 的 fact_version）
            {ok, Fact} = organization_agent_facts_app:resolve_membership(OrgId, AgentId),
            ?assertMatch(
                {error, {409, _}},
                organization_agent_membership_app:restore(
                    OwnerUid,
                    OrgId,
                    AgentId,
                    maps:get(fact_version, Fact) + 999999,
                    <<>>
                )
            ),
            {ok, _} =
                organization_agent_membership_app:restore(
                    OwnerUid, OrgId, AgentId, maps:get(fact_version, Fact), <<>>
                )
        after
            cleanup([OrgId], [], [OwnerUid, AgentId])
        end
    end).

%% ===================================================================
%% archived Org fail closed（C16）
%% ===================================================================

archived_org_rejects_all_commands_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        OwnerUid = new_uid(),
        OrgId = new_uid(),
        AgentId = new_uid(),
        try
            create_human(OwnerUid),
            create_agent(AgentId),
            create_org_with_owner(OrgId, OwnerUid),
            {ok, _} =
                organization_agent_membership_app:attach(
                    OwnerUid, OrgId, AgentId, undefined, <<"idem-arch0">>
                ),
            {ok, _} = organization_lifecycle:archive(OwnerUid, OrgId),
            %% archived：四类命令一律 409
            ?assertMatch(
                {error, {409, _}},
                organization_agent_membership_app:attach(
                    OwnerUid, OrgId, AgentId, undefined, <<>>
                )
            ),
            ?assertMatch(
                {error, {409, _}},
                organization_agent_membership_app:suspend(
                    OwnerUid, OrgId, AgentId, undefined, <<>>
                )
            ),
            ?assertMatch(
                {error, {409, _}},
                organization_agent_membership_app:restore(
                    OwnerUid, OrgId, AgentId, undefined, <<>>
                )
            ),
            ?assertMatch(
                {error, {409, _}},
                organization_agent_membership_app:remove(
                    OwnerUid, OrgId, AgentId, undefined, <<>>
                )
            ),
            %% restore org 后命令恢复可用
            {ok, _} = organization_lifecycle:restore(OwnerUid, OrgId),
            {ok, R} =
                organization_agent_membership_app:suspend(
                    OwnerUid, OrgId, AgentId, undefined, <<"idem-arch1">>
                ),
            ?assertEqual(<<"suspended">>, maps:get(status, R))
        after
            cleanup([OrgId], [], [OwnerUid, AgentId])
        end
    end).

%% ===================================================================
%% attach 对治理角色行 409 role_conflict（不静默改写）
%% ===================================================================

attach_on_governance_role_rejected_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        OwnerUid = new_uid(),
        OrgId = new_uid(),
        AdminAgent = new_uid(),
        try
            create_human(OwnerUid),
            create_agent(AdminAgent),
            create_org_with_owner(OrgId, OwnerUid),
            insert_member(OrgId, AdminAgent, <<"admin">>, <<"active">>),
            ?assertMatch(
                {error, {409, _}},
                organization_agent_membership_app:attach(
                    OwnerUid, OrgId, AdminAgent, undefined, <<>>
                )
            ),
            member_row_is(OrgId, AdminAgent, <<"admin">>, <<"active">>)
        after
            cleanup([OrgId], [], [OwnerUid, AdminAgent])
        end
    end).

%% ===================================================================
%% Fixture / 审计捕获 / cleanup
%% ===================================================================

with_audit_capture(Self, TestFun) ->
    ok = meck:new(elib_log, [passthrough, no_link]),
    ok = meck:expect(elib_log, internal_log, fun(Level, Msg, M, L) ->
        Self ! {audit, Msg},
        meck:passthrough([Level, Msg, M, L])
    end),
    try
        TestFun()
    after
        _ = (catch meck:unload(elib_log))
    end.

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
        [Uid, <<"org06m", (integer_to_binary(Uid))/binary>>, AccountType]
    ),
    ok.

create_org_with_owner(OrgId, OwnerUid) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.organization (id, name, owner_id)"
            " VALUES ($1, 'org06-agent-membership-probe', $2)"
        >>,
        [OrgId, OwnerUid]
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

member_row_is(OrgId, Uid, Role, Status) ->
    {ok, Rows} = elib_pg:query(
        <<
            "SELECT role, status FROM public.organization_member"
            " WHERE organization_id = $1 AND user_id = $2"
        >>,
        [OrgId, Uid]
    ),
    [#{<<"role">> := Role, <<"status">> := Status}] = Rows,
    ok.

count_events(Tag, Events) ->
    length([1 || [T | _] <- Events, T =:= Tag]).

drain() ->
    drain([]).

drain(Acc) ->
    receive
        {audit, Msg} -> drain([Msg | Acc])
    after 200 ->
        lists:reverse(Acc)
    end.

%% 清理顺序同 facts 套件（RESTRICT 链）。
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
