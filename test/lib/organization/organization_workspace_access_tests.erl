-module(organization_workspace_access_tests).

%% GZAPP-02/G7：org owner/admin → 本 org 全部 ws 管理权 判定入口单元测试。
%%   * ensure_org_manager/2：owner/admin ok；member/非成员 403；DB 异常 503 fail-closed；
%%   * is_org_manager_tx/3：个人域 undefined 恒 false；owner/admin true；
%%     member/非成员/DB 异常 false（事务内 fail-closed）。

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(ORG_ID, 700001).
-define(ORG_OWNER, 900001).
-define(ORG_ADMIN, 900002).
-define(ORG_MEMBER, 900003).
-define(OUTSIDER, 900004).

with_member_repo(Expectation, TestFun) ->
    ?WITH_MECKS([{organization_member_repo, [Expectation]}], TestFun).

ensure_org_manager_test_() ->
    [
        {"org owner passes", fun() ->
            with_member_repo(
                {'find_active', 3, fun(?ORG_ID, ?ORG_OWNER, <<"role">>) ->
                    {ok, #{<<"role">> => <<"owner">>}}
                end},
                fun() ->
                    ?assertEqual(
                        ok, organization_workspace_access:ensure_org_manager(?ORG_ID, ?ORG_OWNER)
                    )
                end
            )
        end},
        {"org admin passes", fun() ->
            with_member_repo(
                {'find_active', 3, fun(?ORG_ID, ?ORG_ADMIN, <<"role">>) ->
                    {ok, #{<<"role">> => <<"admin">>}}
                end},
                fun() ->
                    ?assertEqual(
                        ok, organization_workspace_access:ensure_org_manager(?ORG_ID, ?ORG_ADMIN)
                    )
                end
            )
        end},
        {"org member rejected 403", fun() ->
            with_member_repo(
                {'find_active', 3, fun(?ORG_ID, ?ORG_MEMBER, <<"role">>) ->
                    {ok, #{<<"role">> => <<"member">>}}
                end},
                fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        organization_workspace_access:ensure_org_manager(?ORG_ID, ?ORG_MEMBER)
                    )
                end
            )
        end},
        {"non member rejected 403", fun() ->
            with_member_repo(
                {'find_active', 3, fun(?ORG_ID, ?OUTSIDER, <<"role">>) ->
                    {error, not_found}
                end},
                fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        organization_workspace_access:ensure_org_manager(?ORG_ID, ?OUTSIDER)
                    )
                end
            )
        end},
        {"db error fail-closed 503", fun() ->
            with_member_repo(
                {'find_active', 3, fun(_Org, _Uid, _Cols) -> {error, pool_down} end},
                fun() ->
                    ?assertMatch(
                        {error, {503, _}},
                        organization_workspace_access:ensure_org_manager(?ORG_ID, ?ORG_OWNER)
                    )
                end
            )
        end}
    ].

is_org_manager_tx_test_() ->
    [
        {"personal scope (undefined org) is always false", fun() ->
            ?assertEqual(
                false,
                organization_workspace_access:is_org_manager_tx(fake_conn, undefined, ?ORG_OWNER)
            )
        end},
        {"tx predicate true for org owner", fun() ->
            with_member_repo(
                {'find_active_tx', 4, fun(_Conn, ?ORG_ID, ?ORG_OWNER, <<"role">>) ->
                    {ok, #{<<"role">> => <<"owner">>}}
                end},
                fun() ->
                    ?assertEqual(
                        true,
                        organization_workspace_access:is_org_manager_tx(
                            fake_conn, ?ORG_ID, ?ORG_OWNER
                        )
                    )
                end
            )
        end},
        {"tx predicate true for org admin", fun() ->
            with_member_repo(
                {'find_active_tx', 4, fun(_Conn, ?ORG_ID, ?ORG_ADMIN, <<"role">>) ->
                    {ok, #{<<"role">> => <<"admin">>}}
                end},
                fun() ->
                    ?assertEqual(
                        true,
                        organization_workspace_access:is_org_manager_tx(
                            fake_conn, ?ORG_ID, ?ORG_ADMIN
                        )
                    )
                end
            )
        end},
        {"tx predicate false for member/not-found/db-error (fail-closed)", fun() ->
            with_member_repo(
                {'find_active_tx', 4, fun(_Conn, _Org, Uid, <<"role">>) ->
                    case Uid of
                        ?ORG_MEMBER -> {ok, #{<<"role">> => <<"member">>}};
                        ?OUTSIDER -> {error, not_found};
                        ?ORG_OWNER -> {error, db_down}
                    end
                end},
                fun() ->
                    ?assertEqual(
                        false,
                        organization_workspace_access:is_org_manager_tx(
                            fake_conn, ?ORG_ID, ?ORG_MEMBER
                        )
                    ),
                    ?assertEqual(
                        false,
                        organization_workspace_access:is_org_manager_tx(
                            fake_conn, ?ORG_ID, ?OUTSIDER
                        )
                    ),
                    ?assertEqual(
                        false,
                        organization_workspace_access:is_org_manager_tx(
                            fake_conn, ?ORG_ID, ?ORG_OWNER
                        )
                    )
                end
            )
        end}
    ].

%% ===================================================================
%% GZAPP-03：ensure_org_manager_for_ws/2（Workspace 级入口）
%% ===================================================================

-define(WS_ID, 800001).

with_ws_and_member(FindWsResult, FindMemberResult, TestFun) ->
    ?WITH_MECKS(
        [
            {workspace_repo, [{'find_by_id', 2, fun(_WsId, _Cols) -> FindWsResult end}]},
            {organization_member_repo, [
                {'find_active', 3, fun(_OrgId, _Uid, _Cols) -> FindMemberResult end}
            ]}
        ],
        TestFun
    ).

ensure_org_manager_for_ws_test_() ->
    [
        {"ws org owner passes", fun() ->
            with_ws_and_member(
                #{<<"organization_id">> => ?ORG_ID},
                {ok, #{<<"role">> => <<"owner">>}},
                fun() ->
                    ?assertEqual(
                        ok,
                        organization_workspace_access:ensure_org_manager_for_ws(?WS_ID, ?ORG_OWNER)
                    )
                end
            )
        end},
        {"ws org member rejected 403", fun() ->
            with_ws_and_member(
                #{<<"organization_id">> => ?ORG_ID},
                {ok, #{<<"role">> => <<"member">>}},
                fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        organization_workspace_access:ensure_org_manager_for_ws(?WS_ID, ?ORG_MEMBER)
                    )
                end
            )
        end},
        {"personal-domain ws (org null) rejected 403 without org lookup", fun() ->
            with_ws_and_member(
                #{<<"organization_id">> => null},
                {error, not_found},
                fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        organization_workspace_access:ensure_org_manager_for_ws(?WS_ID, ?ORG_OWNER)
                    ),
                    ?assertEqual(0, meck:num_calls(organization_member_repo, find_active, 3))
                end
            )
        end},
        {"missing ws rejected 403", fun() ->
            with_ws_and_member(
                #{},
                {error, not_found},
                fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        organization_workspace_access:ensure_org_manager_for_ws(?WS_ID, ?ORG_OWNER)
                    )
                end
            )
        end},
        {"ws lookup db error fail-closed 503", fun() ->
            with_ws_and_member(
                {error, connection_closed},
                {ok, #{<<"role">> => <<"owner">>}},
                fun() ->
                    ?assertMatch(
                        {error, {503, _}},
                        organization_workspace_access:ensure_org_manager_for_ws(?WS_ID, ?ORG_OWNER)
                    )
                end
            )
        end}
    ].
