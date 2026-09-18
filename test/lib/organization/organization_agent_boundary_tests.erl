-module(organization_agent_boundary_tests).

%% ORG-06 Agent Organization Boundary 纯决策（domain）测试（零 DB、零 mock）。
%%
%% 覆盖冻结语义：
%%   * Agent 身份仅 account_type=1；Human 仅 account_type=0；
%%   * membership allowed 三条件：Org active + 成员 active + role=member
%%     （admin/owner 行 fail closed——member-only）；
%%   * workspace membership / ownership 的 archived|removed fail closed；
%%   * 乐观并发 stale_version 裁决（undefined=none / 相等=fresh / 否则=stale）；
%%   * facts 合同版本稳定为 1（C18 versioned API）。

-include_lib("eunit/include/eunit.hrl").

fact_version_stable_test_() ->
    ?_assertEqual(1, organization_agent_boundary:fact_version()).

agent_identity_test_() ->
    [
        {"account_type=1 是 Agent",
            ?_assertEqual(ok, organization_agent_boundary:ensure_agent_identity(1))},
        {"account_type=0 不是 Agent",
            ?_assertMatch({error, not_agent}, organization_agent_boundary:ensure_agent_identity(0))},
        {"account_type=2 不是 Agent",
            ?_assertMatch({error, not_agent}, organization_agent_boundary:ensure_agent_identity(2))},
        {"account_type=3 不是 Agent",
            ?_assertMatch({error, not_agent}, organization_agent_boundary:ensure_agent_identity(3))}
    ].

human_identity_test_() ->
    [
        {"account_type=0 是 Human",
            ?_assertEqual(ok, organization_agent_boundary:ensure_human_identity(0))},
        {"account_type=1 不是 Human",
            ?_assertMatch({error, not_human}, organization_agent_boundary:ensure_human_identity(1))},
        {"account_type=2 不是 Human",
            ?_assertMatch({error, not_human}, organization_agent_boundary:ensure_human_identity(2))}
    ].

organization_allowed_test_() ->
    [
        {"active 放行", ?_assert(organization_agent_boundary:organization_allowed(<<"active">>))},
        {"archived fail closed",
            ?_assertNot(organization_agent_boundary:organization_allowed(<<"archived">>))}
    ].

membership_allowed_test_() ->
    Positive = organization_agent_boundary:membership_allowed(
        <<"active">>, <<"active">>, <<"member">>
    ),
    [
        {"active org + active member + member 角色放行", ?_assert(Positive)},
        {"org archived fail closed",
            ?_assertNot(
                organization_agent_boundary:membership_allowed(
                    <<"archived">>, <<"active">>, <<"member">>
                )
            )},
        {"成员 suspended fail closed",
            ?_assertNot(
                organization_agent_boundary:membership_allowed(
                    <<"active">>, <<"suspended">>, <<"member">>
                )
            )},
        {"成员 removed fail closed",
            ?_assertNot(
                organization_agent_boundary:membership_allowed(
                    <<"active">>, <<"removed">>, <<"member">>
                )
            )},
        {"Agent admin 角色 fail closed（member-only，DB 对 admin 无 guard，边界兜底）",
            ?_assertNot(
                organization_agent_boundary:membership_allowed(
                    <<"active">>, <<"active">>, <<"admin">>
                )
            )},
        {"Agent owner 角色 fail closed",
            ?_assertNot(
                organization_agent_boundary:membership_allowed(
                    <<"active">>, <<"active">>, <<"owner">>
                )
            )}
    ].

workspace_membership_allowed_test_() ->
    [
        {"ws active + 成员 active 放行",
            ?_assert(
                organization_agent_boundary:workspace_membership_allowed(<<"active">>, <<"active">>)
            )},
        {"ws archived fail closed",
            ?_assertNot(
                organization_agent_boundary:workspace_membership_allowed(
                    <<"archived">>, <<"active">>
                )
            )},
        {"成员 removed fail closed",
            ?_assertNot(
                organization_agent_boundary:workspace_membership_allowed(
                    <<"active">>, <<"removed">>
                )
            )}
    ].

workspace_ownership_allowed_test_() ->
    [
        {"同 Org + active 放行",
            ?_assert(organization_agent_boundary:workspace_ownership_allowed(true, <<"active">>))},
        {"跨 Org fail closed",
            ?_assertNot(
                organization_agent_boundary:workspace_ownership_allowed(false, <<"active">>)
            )},
        {"同 Org 但 archived fail closed",
            ?_assertNot(
                organization_agent_boundary:workspace_ownership_allowed(true, <<"archived">>)
            )}
    ].

stale_version_test_() ->
    [
        {"undefined = 不做乐观校验",
            ?_assertEqual(none, organization_agent_boundary:stale_version(undefined, 42))},
        {"版本一致 = fresh", ?_assertEqual(fresh, organization_agent_boundary:stale_version(7, 7))},
        {"版本落后 = stale", ?_assertEqual(stale, organization_agent_boundary:stale_version(6, 7))},
        {"版本超前（按行版本语义不可能，防御归为 stale）",
            ?_assertEqual(stale, organization_agent_boundary:stale_version(8, 7))},
        {"行不存在的当前版本按 0：带版本创建预期 = stale",
            ?_assertEqual(stale, organization_agent_boundary:stale_version(1, 0))},
        {"行不存在且不带版本 = none",
            ?_assertEqual(none, organization_agent_boundary:stale_version(undefined, 0))}
    ].
