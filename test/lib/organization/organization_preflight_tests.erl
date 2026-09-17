-module(organization_preflight_tests).

%% ORG-02 DeletionPreflightFacts 编排器 + org/workspace provider 测试。
%%
%% 覆盖（计划 §1.6 + ORG-02 卡 ORG-A05）：
%%   * 编排器 fail-closed：缺域（EB/CS/Agent 未注册）→ 拒；
%%     provider unavailable/timeout/inconsistent(malformed/crash) → 拒；
%%     稳定原因恒为 DEPENDENCY_FACTS_UNAVAILABLE。
%%   * 五域全注册 + 全无 blocker → {ok, blockers = []}；
%%     blocker 按冻结四字段聚合返回。
%%   * 真实 provider（真 PG）：owner/member/workspace owner 三态 blocker；
%%     无关用户空 blocker；默认注册表（organization+workspace 两域）
%%     下整体 preflight 因缺 EB/CS/Agent 被拒（合同明文，非回归）。
%%
%% 运行：make eunit-local t=organization_preflight_tests

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(CODE, <<"DEPENDENCY_FACTS_UNAVAILABLE">>).

%% ===================================================================
%% 编排器（stub registry；无 DB 依赖）
%% ===================================================================

missing_domains_rejected_test_() ->
    {setup, fun() -> ok end, fun(_) -> ok end, fun() ->
        Registry = [{organization, organization_preflight_stub_providers, facts_organization}],
        ok = application:set_env(imboy, deletion_preflight_providers, Registry),
        try
            %% 计划 §1.6 冻结原文：「禁止给未实现域默认空 blocker
            %% （未注册域=facts 不可得=拒）」
            {error, #{
                code := ?CODE,
                reason := provider_unregistered,
                detail := Missing
            }} = organization_deletion_preflight:run(42),
            ?assertEqual(
                lists:sort([workspace, enterprise_business, customer_service, agent]),
                lists:sort(Missing)
            )
        after
            application:unset_env(imboy, deletion_preflight_providers)
        end
    end}.

all_clear_aggregates_empty_blockers_test_() ->
    {setup, fun() -> ok end, fun(_) -> ok end, fun() ->
        with_full_registry(ok_all, fun() ->
            {ok, #{blockers := Blockers, subject_user_id := 42}} =
                organization_deletion_preflight:run(42),
            ?assertEqual([], Blockers)
        end)
    end}.

blockers_aggregated_with_frozen_shape_test_() ->
    {setup, fun() -> ok end, fun(_) -> ok end, fun() ->
        with_full_registry(blockers, fun() ->
            {ok, #{blockers := [Blocker]}} = organization_deletion_preflight:run(42),
            %% 冻结四字段：code/resource_type/resource_id(opaque)/organization_id
            ?assertEqual(
                #{
                    code => <<"ORG_OWNER_ACTIVE">>,
                    resource_type => <<"organization">>,
                    resource_id => <<"111222333">>,
                    organization_id => 111222333
                },
                Blocker
            )
        end)
    end}.

provider_unavailable_rejected_test_() ->
    {setup, fun() -> ok end, fun(_) -> ok end, fun() ->
        with_full_registry(unavailable, fun() ->
            {error, #{
                code := ?CODE, reason := unavailable, domain := organization
            }} = organization_deletion_preflight:run(42)
        end)
    end}.

provider_inconsistent_rejected_test_() ->
    {setup, fun() -> ok end, fun(_) -> ok end, fun() ->
        with_full_registry(inconsistent, fun() ->
            {error, #{
                code := ?CODE, reason := inconsistent, domain := organization
            }} = organization_deletion_preflight:run(42)
        end)
    end}.

provider_malformed_fact_rejected_test_() ->
    {setup, fun() -> ok end, fun(_) -> ok end, fun() ->
        with_full_registry(malformed, fun() ->
            {error, #{
                code := ?CODE, reason := inconsistent, domain := organization
            }} = organization_deletion_preflight:run(42)
        end)
    end}.

provider_crash_rejected_test_() ->
    {setup, fun() -> ok end, fun(_) -> ok end, fun() ->
        with_full_registry(crash, fun() ->
            {error, #{
                code := ?CODE, reason := inconsistent, domain := organization
            }} = organization_deletion_preflight:run(42)
        end)
    end}.

provider_timeout_rejected_test_() ->
    {setup, fun() -> ok end, fun(_) -> ok end, fun() ->
        ok = application:set_env(imboy, deletion_preflight_timeout_ms, 200),
        with_full_registry(hang, fun() ->
            {error, #{
                code := ?CODE, reason := timeout, domain := organization
            }} = organization_deletion_preflight:run(42)
        end)
    end}.

bad_subject_rejected_test_() ->
    {setup, fun() -> ok end, fun(_) -> ok end, fun() ->
        {error, #{code := ?CODE, reason := bad_subject}} =
            organization_deletion_preflight:run(bad)
    end}.

default_registry_is_org_plus_workspace_test_() ->
    {setup, fun() -> ok end, fun(_) -> ok end, fun() ->
        ?assertEqual(
            [
                {organization, organization_preflight_facts_pg, facts_organization},
                {workspace, organization_preflight_facts_pg, facts_workspace}
            ],
            organization_deletion_preflight:registry()
        ),
        %% 冻结五域缺一不可（C17）：EB/CS/Agent 交付前默认注册表整体拒
        ?assertEqual(
            [organization, workspace, enterprise_business, customer_service, agent],
            organization_deletion_preflight:required_domains()
        )
    end}.

default_registry_preflight_rejected_until_all_domains_registered_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        %% 真实 provider + 默认两域注册表：缺 EB/CS/Agent → 整体拒
        %% （计划 §1.6：未注册域=facts 不可得=拒；control/ruling-
        %% ORG02-plan1-orchestrator-tests.md 同口径）
        {error, #{code := ?CODE, reason := provider_unregistered}} =
            organization_deletion_preflight:run(1)
    end).

%% ===================================================================
%% 真实 provider（真 PG）
%% ===================================================================

provider_facts_owner_member_workspace_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Uid = new_uid(),
        OwnerUid = new_uid(),
        MemberUid = new_uid(),
        OrgId = new_uid(),
        Org2Id = new_uid(),
        WsId = new_uid(),
        try
            lists:foreach(fun create_user/1, [Uid, OwnerUid, MemberUid]),
            %% OwnerUid 是 OrgId 的 active owner；MemberUid 是 active member
            create_org_with_owner(OrgId, OwnerUid),
            {ok, _} = elib_pg:query(
                <<
                    "INSERT INTO public.organization_member"
                    " (organization_id, user_id, role, joined_at, status)"
                    " VALUES ($1, $2, 'member', CURRENT_TIMESTAMP, 'active')"
                >>,
                [OrgId, MemberUid]
            ),
            %% OwnerUid 在另一 org 也是 active member（非 owner）
            create_org_with_owner(Org2Id, Uid),
            {ok, _} = elib_pg:query(
                <<
                    "INSERT INTO public.organization_member"
                    " (organization_id, user_id, role, joined_at, status)"
                    " VALUES ($1, $2, 'member', CURRENT_TIMESTAMP, 'active')"
                >>,
                [Org2Id, OwnerUid]
            ),
            %% OwnerUid 名下还有 workspace（org 归属可空）
            {ok, _} = elib_pg:query(
                <<
                    "INSERT INTO public.workspace (id, name, owner_id)"
                    " VALUES ($1, 'ws-probe', $2)"
                >>,
                [WsId, OwnerUid]
            ),
            %% --- organization facts：owner + member 两类 blocker ---
            {ok, OrgFact} = organization_preflight_facts_pg:facts_organization(OwnerUid),
            ?assertEqual(OwnerUid, maps:get(subject_user_id, OrgFact)),
            ?assertEqual(organization, maps:get(domain, OrgFact)),
            ?assert(is_integer(maps:get(observed_at, OrgFact))),
            ?assertEqual(1, maps:get(fact_version, OrgFact)),
            Codes = [maps:get(code, B) || B <- maps:get(blockers, OrgFact)],
            ?assert(lists:member(<<"ORG_OWNER_ACTIVE">>, Codes)),
            ?assert(lists:member(<<"ORG_MEMBERSHIP_ACTIVE">>, Codes)),
            %% blocker 冻结四字段 + opaque resource_id
            lists:foreach(
                fun(B) ->
                    #{code := _, resource_type := _, resource_id := Rid, organization_id := Oid} =
                        B,
                    ?assert(is_binary(Rid)),
                    ?assert(Oid =:= null orelse is_integer(Oid))
                end,
                maps:get(blockers, OrgFact)
            ),
            %% --- workspace facts：WORKSPACE_OWNER_ACTIVE ---
            {ok, WsFact} = organization_preflight_facts_pg:facts_workspace(OwnerUid),
            ?assertEqual(workspace, maps:get(domain, WsFact)),
            [WsBlocker] = maps:get(blockers, WsFact),
            ?assertEqual(<<"WORKSPACE_OWNER_ACTIVE">>, maps:get(code, WsBlocker)),
            ?assertEqual(<<"workspace">>, maps:get(resource_type, WsBlocker)),
            ?assertEqual(integer_to_binary(WsId), maps:get(resource_id, WsBlocker)),
            %% --- member-only 用户：无 owner blocker ---
            {ok, MemberFact} = organization_preflight_facts_pg:facts_organization(MemberUid),
            MemberCodes = [maps:get(code, B) || B <- maps:get(blockers, MemberFact)],
            ?assertEqual([<<"ORG_MEMBERSHIP_ACTIVE">>], MemberCodes),
            %% --- 无关用户：空 blocker（用户行不存在同样成立：无 membership 行） ---
            CleanUid = Uid + 1000000,
            {ok, CleanFact} = organization_preflight_facts_pg:facts_organization(CleanUid),
            ?assertEqual([], maps:get(blockers, CleanFact)),
            {ok, CleanWs} = organization_preflight_facts_pg:facts_workspace(CleanUid),
            ?assertEqual([], maps:get(blockers, CleanWs)),
            %% --- removed 成员不再产生 blocker ---
            {ok, _} = elib_pg:query(
                <<
                    "UPDATE public.organization_member SET status = 'removed'"
                    " WHERE organization_id = $1 AND user_id = $2"
                >>,
                [OrgId, MemberUid]
            ),
            {ok, RemovedFact} = organization_preflight_facts_pg:facts_organization(MemberUid),
            ?assertEqual([], maps:get(blockers, RemovedFact))
        after
            cleanup(Uid, Org2Id),
            cleanup(OwnerUid, OrgId),
            cleanup(MemberUid, undefined),
            _ = elib_pg:query(<<"DELETE FROM public.workspace WHERE id = $1">>, [WsId])
        end
    end).

%% ===================================================================
%% Internal
%% ===================================================================

with_full_registry(Behavior, Fun) ->
    ok = application:set_env(imboy, deletion_preflight_providers, full_registry()),
    ok = application:set_env(imboy, preflight_stub_behavior, Behavior),
    try
        Fun()
    after
        application:unset_env(imboy, deletion_preflight_providers),
        application:unset_env(imboy, preflight_stub_behavior)
    end.

full_registry() ->
    organization_preflight_stub_providers:full_registry().

new_uid() ->
    erlang:system_time(millisecond) * 1000 + rand:uniform(999).

create_user(Uid) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.\"user\" (id, account, password, reg_ip, reg_cosv)"
            " VALUES ($1, $2, 'x', '127.0.0.1', '')"
        >>,
        [Uid, <<"u", (integer_to_binary(Uid))/binary>>]
    ),
    ok.

create_org_with_owner(OrgId, OwnerUid) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.organization (id, name, owner_id)"
            " VALUES ($1, 'preflight-probe', $2)"
        >>,
        [OrgId, OwnerUid]
    ),
    ok.

cleanup(Uid, MaybeOrgId) ->
    case MaybeOrgId of
        undefined ->
            ok;
        OrgId ->
            _ = elib_pg:query(
                <<"DELETE FROM public.organization WHERE id = $1">>, [OrgId]
            )
    end,
    lists:foreach(fun(Sql) -> _ = elib_pg:query(Sql, [Uid]) end, [
        <<"DELETE FROM public.organization_member WHERE user_id = $1">>,
        <<"DELETE FROM public.\"user\" WHERE id = $1">>
    ]).
