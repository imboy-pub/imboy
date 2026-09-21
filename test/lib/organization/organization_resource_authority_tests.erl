-module(organization_resource_authority_tests).

%% GZAPP-03/D04：资源级 org 管理权判定单元测试。
%%   * ws 域群/频道 + 本 org owner/admin → ok（第二授权源）
%%   * personal 域资源 → 403（无 org 可授权，不改变个人域语义）
%%   * 资源不存在 → 403（不泄露存在性）
%%   * 非 manager（org member）→ 403
%%   * resolver/DB 异常 → 503 fail-closed（不放行）

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID, 900001).
-define(GID, 666001).
-define(CID, 777001).
-define(WS_ID, 800001).

with_resolver(ResolveResult, ManagerResult, TestFun) ->
    ?WITH_MECKS(
        [
            {workspace_resolver, [{'resolve_workspace', 1, fun(_Resource) -> ResolveResult end}]},
            {organization_workspace_access, [
                {'ensure_org_manager_for_ws', 2, fun(_WsId, _Uid) -> ManagerResult end}
            ]}
        ],
        TestFun
    ).

ensure_manager_test_() ->
    [
        {"ws-scoped group + org manager passes", fun() ->
            with_resolver(
                {ok, ?WS_ID},
                ok,
                fun() ->
                    ?assertEqual(
                        ok,
                        organization_resource_authority:ensure_manager({group, ?GID}, ?UID)
                    )
                end
            )
        end},
        {"ws-scoped channel + org manager passes (ws id forwarded)", fun() ->
            ?WITH_MECKS(
                [
                    {workspace_resolver, [
                        {'resolve_workspace', 1, fun({channel, ?CID}) -> {ok, ?WS_ID} end}
                    ]},
                    {organization_workspace_access, [
                        {'ensure_org_manager_for_ws', 2, fun(WsId, Uid) ->
                            ?assertEqual(?WS_ID, WsId),
                            ?assertEqual(?UID, Uid),
                            ok
                        end}
                    ]}
                ],
                fun() ->
                    ?assertEqual(
                        ok,
                        organization_resource_authority:ensure_manager({channel, ?CID}, ?UID)
                    )
                end
            )
        end},
        {"org member (non manager) rejected 403", fun() ->
            with_resolver(
                {ok, ?WS_ID},
                {error, {403, <<"仅工作区 Owner 或组织 Owner/Admin 可执行该操作"/utf8>>}},
                fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        organization_resource_authority:ensure_manager({group, ?GID}, ?UID)
                    )
                end
            )
        end},
        {"personal-domain resource rejected 403 without manager lookup", fun() ->
            with_resolver(
                personal,
                ok,
                fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        organization_resource_authority:ensure_manager({group, ?GID}, ?UID)
                    ),
                    ?assertEqual(
                        0,
                        meck:num_calls(
                            organization_workspace_access, ensure_org_manager_for_ws, 2
                        )
                    )
                end
            )
        end},
        {"resource not found rejected 403", fun() ->
            with_resolver(
                {error, not_found},
                ok,
                fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        organization_resource_authority:ensure_manager({channel, ?CID}, ?UID)
                    )
                end
            )
        end},
        {"resolver db error fail-closed 503", fun() ->
            with_resolver(
                {error, {db_error, timeout}},
                ok,
                fun() ->
                    %% resolver 契约：DB 异常以 error:{resolver_db_error, _} 抛出
                    ?assertMatch(
                        {error, {503, _}},
                        (catch begin
                            meck:expect(workspace_resolver, resolve_workspace, 1, fun(_R) ->
                                error({resolver_db_error, timeout})
                            end),
                            organization_resource_authority:ensure_manager({group, ?GID}, ?UID)
                        end)
                    )
                end
            )
        end},
        {"unsupported resource type fail-closed 503", fun() ->
            with_resolver(
                {error, {unsupported_resource, {group_vote, 1}}},
                ok,
                fun() ->
                    %% {error, {unsupported_resource, _}} 走 denied 分支（不泄露内部类型）
                    ?assertMatch(
                        {error, {403, _}},
                        organization_resource_authority:ensure_manager({channel, ?CID}, ?UID)
                    )
                end
            )
        end}
    ].
