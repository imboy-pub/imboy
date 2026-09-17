%%% @doc AG31-01：agent_org_membership_adapter 行为套件（meck ORG facts，零 DB）。
%%%
%%% 上游 `organization_agent_facts_app` 在本分支不存在（TRACK-ORG 侧交付，形状以
%%% AG31-01-org-facts-shape.md 快照为唯一事实源），故 mock 必须 non_strict
%%% （允许 mock 未加载模块）。本套件只验证 adapter 的**映射表**与**fail closed
%%% 不缓存**语义，不触达 ORG worktree / 真库：
%%%   * 错误翻译：{404}→not_found、{403}→invalid_agent_role、{400}→not_found、
%%%     {500}/crash/未知形状→unavailable；
%%%   * §6.2 ok 形状锁死 status:=active：org/member/ws 任一非 active → inactive
%%%     （membership 上下文），archived 原子只归 resolve_organization_state；
%%%   * workspace 先验归属：same_org=false → cross_organization 且不再查 ws-member；
%%%   * 零缓存：连续两次 unavailable → facts 调用计数=2（meck:num_calls 断言）。
%%% 套件结构沿用 agent_run_command_deny_tests 的 {foreach, setup, cleanup, [groups]}。
-module(agent_org_membership_adapter_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FACTS, organization_agent_facts_app).
-define(ADAPTER, agent_org_membership_adapter).

-define(ORG_ID, 22).
-define(AGENT_ID, 11).
-define(WS_ID, 33).
-define(OBSERVED_AT, 1726500000000).

%% ===================================================================
%% 套件夹具
%% ===================================================================

membership_adapter_test_() ->
    {foreach,
        fun() ->
            %% 上游模块本分支不存在：non_strict 允许 mock 未加载模块
            meck:new(?FACTS, [no_link, non_strict]),
            ok
        end,
        fun(_) ->
            meck:unload(?FACTS),
            ok
        end,
        [
            fun org_membership_tests/1,
            fun org_state_tests/1,
            fun workspace_membership_tests/1,
            fun no_cache_and_multi_org_tests/1
        ]}.

org_membership_tests(_) ->
    [
        {"happy path: all active member -> {ok, active, member, version}", fun() ->
            expect_org(active),
            expect_membership(active, member),
            ?assertEqual(
                {ok, #{status => active, role => member, version => 7}},
                ?ADAPTER:resolve_organization_membership(?ORG_ID, ?AGENT_ID)
            ),
            ?assert(meck:called(?FACTS, resolve_membership, [?ORG_ID, ?AGENT_ID]))
        end},
        {"member row 404 -> not_found", fun() ->
            expect_org(active),
            meck:expect(?FACTS, resolve_membership, fun(_O, _A) ->
                {error, {404, <<"member row missing">>}}
            end),
            ?assertEqual(
                {error, not_found},
                ?ADAPTER:resolve_organization_membership(?ORG_ID, ?AGENT_ID)
            )
        end},
        {"member suspended -> inactive", fun() ->
            expect_org(active),
            expect_membership(suspended, member),
            ?assertEqual(
                {error, inactive},
                ?ADAPTER:resolve_organization_membership(?ORG_ID, ?AGENT_ID)
            )
        end},
        {"member removed -> inactive", fun() ->
            expect_org(active),
            expect_membership(removed, member),
            ?assertEqual(
                {error, inactive},
                ?ADAPTER:resolve_organization_membership(?ORG_ID, ?AGENT_ID)
            )
        end},
        {"org archived in membership context -> inactive (not archived)", fun() ->
            %% foreach 粒度是组：断言调用计数前先清 meck 历史
            meck:reset(?FACTS),
            expect_org(archived),
            expect_membership(active, member),
            ?assertEqual(
                {error, inactive},
                ?ADAPTER:resolve_organization_membership(?ORG_ID, ?AGENT_ID)
            ),
            %% org 门短路：archived org 不再查 member 行
            ?assertEqual(0, meck:num_calls(?FACTS, resolve_membership, '_'))
        end},
        {"role not member (DB invariant blocked in theory) -> inactive fail closed", fun() ->
            expect_org(active),
            expect_membership(active, admin),
            ?assertEqual(
                {error, inactive},
                ?ADAPTER:resolve_organization_membership(?ORG_ID, ?AGENT_ID)
            )
        end},
        {"{403, not_agent} -> invalid_agent_role", fun() ->
            expect_org(active),
            meck:expect(?FACTS, resolve_membership, fun(_O, _A) ->
                {error, {403, <<"subject is not an agent account">>}}
            end),
            ?assertEqual(
                {error, invalid_agent_role},
                ?ADAPTER:resolve_organization_membership(?ORG_ID, ?AGENT_ID)
            )
        end},
        {"{500, _} -> unavailable", fun() ->
            expect_org(active),
            meck:expect(?FACTS, resolve_membership, fun(_O, _A) ->
                {error, {500, <<"facts pg down">>}}
            end),
            ?assertEqual(
                {error, unavailable},
                ?ADAPTER:resolve_organization_membership(?ORG_ID, ?AGENT_ID)
            )
        end},
        {"mock crash -> unavailable (try/catch fallback)", fun() ->
            expect_org(active),
            meck:expect(?FACTS, resolve_membership, fun(_O, _A) ->
                meck:exception(error, facts_pg_down)
            end),
            ?assertEqual(
                {error, unavailable},
                ?ADAPTER:resolve_organization_membership(?ORG_ID, ?AGENT_ID)
            )
        end},
        {"{400, _} -> not_found (fail closed, same as missing)", fun() ->
            expect_org(active),
            meck:expect(?FACTS, resolve_membership, fun(_O, _A) ->
                {error, {400, <<"id must be positive integer">>}}
            end),
            ?assertEqual(
                {error, not_found},
                ?ADAPTER:resolve_organization_membership(?ORG_ID, ?AGENT_ID)
            )
        end},
        {"invalid input (non-positive id) -> not_found, no facts call", fun() ->
            meck:reset(?FACTS),
            expect_org(active),
            expect_membership(active, member),
            ?assertEqual(
                {error, not_found},
                ?ADAPTER:resolve_organization_membership(0, ?AGENT_ID)
            ),
            ?assertEqual(
                {error, not_found},
                ?ADAPTER:resolve_organization_membership(?ORG_ID, -1)
            ),
            ?assertEqual(0, meck:num_calls(?FACTS, resolve_organization, '_')),
            ?assertEqual(0, meck:num_calls(?FACTS, resolve_membership, '_'))
        end}
    ].

org_state_tests(_) ->
    [
        {"state active -> {ok, active, version}", fun() ->
            expect_org(active),
            ?assertEqual(
                {ok, #{status => active, version => 5}},
                ?ADAPTER:resolve_organization_state(?ORG_ID)
            )
        end},
        {"state archived -> {error, archived}", fun() ->
            expect_org(archived),
            ?assertEqual({error, archived}, ?ADAPTER:resolve_organization_state(?ORG_ID))
        end},
        {"state org 404 -> not_found", fun() ->
            meck:expect(?FACTS, resolve_organization, fun(_O) ->
                {error, {404, <<"org missing">>}}
            end),
            ?assertEqual({error, not_found}, ?ADAPTER:resolve_organization_state(?ORG_ID))
        end},
        {"state {500, _} -> unavailable", fun() ->
            meck:expect(?FACTS, resolve_organization, fun(_O) ->
                {error, {500, <<"facts pg down">>}}
            end),
            ?assertEqual({error, unavailable}, ?ADAPTER:resolve_organization_state(?ORG_ID))
        end},
        {"state unknown status -> unavailable (fail closed)", fun() ->
            expect_org(pending),
            ?assertEqual({error, unavailable}, ?ADAPTER:resolve_organization_state(?ORG_ID))
        end},
        {"state invalid input -> not_found, no facts call", fun() ->
            meck:reset(?FACTS),
            expect_org(active),
            ?assertEqual({error, not_found}, ?ADAPTER:resolve_organization_state(<<"22">>)),
            ?assertEqual(0, meck:num_calls(?FACTS, resolve_organization, '_'))
        end}
    ].

workspace_membership_tests(_) ->
    [
        {"workspace happy path: same org + active ws + active member -> ok with real role", fun() ->
                expect_ws_ownership(true, active),
                expect_ws_membership(active, admin),
                ?assertEqual(
                    {ok, #{status => active, role => admin, version => 9}},
                    ?ADAPTER:resolve_workspace_membership(?ORG_ID, ?WS_ID, ?AGENT_ID)
                ),
                %% ORG facts 侧是二参 (WsId, AgentId)
                ?assert(meck:called(?FACTS, resolve_workspace_membership, [?WS_ID, ?AGENT_ID]))
            end},
        {"workspace same_org=false -> cross_organization, ws-member not queried", fun() ->
            meck:reset(?FACTS),
            expect_ws_ownership(false, active),
            expect_ws_membership(active, member),
            ?assertEqual(
                {error, cross_organization},
                ?ADAPTER:resolve_workspace_membership(?ORG_ID, ?WS_ID, ?AGENT_ID)
            ),
            ?assertEqual(0, meck:num_calls(?FACTS, resolve_workspace_membership, '_'))
        end},
        {"workspace ws-member row 404 -> not_found", fun() ->
            expect_ws_ownership(true, active),
            meck:expect(?FACTS, resolve_workspace_membership, fun(_W, _A) ->
                {error, {404, <<"ws member row missing">>}}
            end),
            ?assertEqual(
                {error, not_found},
                ?ADAPTER:resolve_workspace_membership(?ORG_ID, ?WS_ID, ?AGENT_ID)
            )
        end},
        {"workspace same_org=true but ws suspended -> inactive", fun() ->
            meck:reset(?FACTS),
            expect_ws_ownership(true, suspended),
            expect_ws_membership(active, member),
            ?assertEqual(
                {error, inactive},
                ?ADAPTER:resolve_workspace_membership(?ORG_ID, ?WS_ID, ?AGENT_ID)
            ),
            ?assertEqual(0, meck:num_calls(?FACTS, resolve_workspace_membership, '_'))
        end},
        {"workspace member suspended -> inactive", fun() ->
            expect_ws_ownership(true, active),
            expect_ws_membership(suspended, member),
            ?assertEqual(
                {error, inactive},
                ?ADAPTER:resolve_workspace_membership(?ORG_ID, ?WS_ID, ?AGENT_ID)
            )
        end},
        {"workspace validate {500, _} -> unavailable", fun() ->
            meck:expect(?FACTS, validate_workspace_in_org, fun(_O, _W) ->
                {error, {500, <<"facts pg down">>}}
            end),
            ?assertEqual(
                {error, unavailable},
                ?ADAPTER:resolve_workspace_membership(?ORG_ID, ?WS_ID, ?AGENT_ID)
            )
        end},
        {"workspace invalid input -> not_found, no facts call", fun() ->
            meck:reset(?FACTS),
            expect_ws_ownership(true, active),
            ?assertEqual(
                {error, not_found},
                ?ADAPTER:resolve_workspace_membership(?ORG_ID, 0, ?AGENT_ID)
            ),
            ?assertEqual(0, meck:num_calls(?FACTS, validate_workspace_in_org, '_'))
        end}
    ].

no_cache_and_multi_org_tests(_) ->
    [
        {"no cache: two consecutive unavailable -> facts called exactly twice", fun() ->
            meck:reset(?FACTS),
            meck:expect(?FACTS, resolve_organization, fun(_O) ->
                {error, {500, <<"facts pg down">>}}
            end),
            ?assertEqual({error, unavailable}, ?ADAPTER:resolve_organization_state(?ORG_ID)),
            ?assertEqual({error, unavailable}, ?ADAPTER:resolve_organization_state(?ORG_ID)),
            ?assertEqual(2, meck:num_calls(?FACTS, resolve_organization, [?ORG_ID]))
        end},
        {"no cache: membership unavailable also hits facts per call", fun() ->
            meck:reset(?FACTS),
            expect_org(active),
            meck:expect(?FACTS, resolve_membership, fun(_O, _A) ->
                {error, {500, <<"facts pg down">>}}
            end),
            ?assertEqual(
                {error, unavailable},
                ?ADAPTER:resolve_organization_membership(?ORG_ID, ?AGENT_ID)
            ),
            ?assertEqual(
                {error, unavailable},
                ?ADAPTER:resolve_organization_membership(?ORG_ID, ?AGENT_ID)
            ),
            ?assertEqual(
                2, meck:num_calls(?FACTS, resolve_membership, [?ORG_ID, ?AGENT_ID])
            )
        end},
        {"multi-org: same agent resolves independently per org", fun() ->
            meck:expect(?FACTS, resolve_organization, fun
                (?ORG_ID) -> {ok, org_fact(active)};
                (_OtherOrg) -> {error, {404, <<"org missing">>}}
            end),
            meck:expect(?FACTS, resolve_membership, fun(_O, _A) ->
                {ok, membership_fact(active, member)}
            end),
            ?assertEqual(
                {ok, #{status => active, role => member, version => 7}},
                ?ADAPTER:resolve_organization_membership(?ORG_ID, ?AGENT_ID)
            ),
            ?assertEqual(
                {error, not_found},
                ?ADAPTER:resolve_organization_membership(99, ?AGENT_ID)
            )
        end}
    ].

%% ===================================================================
%% fixtures（facts 形状快照逐字段；id 均为任意项：meck 层零触达真库）
%% ===================================================================

expect_org(Status) ->
    meck:expect(?FACTS, resolve_organization, fun(_OrgId) -> {ok, org_fact(Status)} end).

expect_membership(Status, Role) ->
    meck:expect(?FACTS, resolve_membership, fun(_OrgId, _AgentId) ->
        {ok, membership_fact(Status, Role)}
    end).

expect_ws_ownership(SameOrg, WsStatus) ->
    meck:expect(?FACTS, validate_workspace_in_org, fun(_OrgId, _WsId) ->
        {ok, ws_ownership_fact(SameOrg, WsStatus)}
    end).

expect_ws_membership(Status, Role) ->
    meck:expect(?FACTS, resolve_workspace_membership, fun(_WsId, _AgentId) ->
        {ok, ws_membership_fact(Status, Role)}
    end).

org_fact(Status) ->
    #{
        kind => organization_state,
        organization_id => ?ORG_ID,
        status => Status,
        allowed => Status =:= active,
        fact_version => 5,
        observed_at => ?OBSERVED_AT
    }.

membership_fact(Status, Role) ->
    #{
        kind => organization_membership,
        organization_id => ?ORG_ID,
        subject_user_id => ?AGENT_ID,
        status => Status,
        role => Role,
        org_status => active,
        allowed => (Status =:= active) andalso (Role =:= member),
        fact_version => 7,
        observed_at => ?OBSERVED_AT
    }.

ws_ownership_fact(SameOrg, WsStatus) ->
    #{
        kind => workspace_ownership,
        organization_id => ?ORG_ID,
        workspace_id => ?WS_ID,
        same_org => SameOrg,
        workspace_status => WsStatus,
        allowed => SameOrg andalso (WsStatus =:= active),
        fact_version => 8,
        observed_at => ?OBSERVED_AT
    }.

ws_membership_fact(Status, Role) ->
    #{
        kind => workspace_membership,
        workspace_id => ?WS_ID,
        subject_user_id => ?AGENT_ID,
        status => Status,
        role => Role,
        workspace_status => active,
        workspace_organization_id => ?ORG_ID,
        allowed => Status =:= active,
        fact_version => 9,
        observed_at => ?OBSERVED_AT
    }.
