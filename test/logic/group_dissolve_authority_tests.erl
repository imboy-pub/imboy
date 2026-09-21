-module(group_dissolve_authority_tests).

%% GZAPP-03/D04：企业群解散双授权源单元测试。
%%   * 群主路径零行为变化（仍走 dissolve_group/4 owner 校验）；
%%   * 非群主 + 本群所属 org owner/admin → 走 dissolve_by_org_manager/3；
%%   * 非群主 + 非 org 管理者 → 保持既有拒绝文案（不泄露群/组织存在性，
%%     且不触达 DS 写路径）；
%%   * 授权链 DB 异常 → 503 透传（fail-closed，不误放行）。

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(GID, 666001).
-define(OWNER, 900001).
-define(ORG_ADMIN, 900002).
-define(OUTSIDER, 900003).
-define(FORBIDDEN, <<"只有拥有者才能够解散该群，或者群已解散"/utf8>>).

-define(GROUP_ROW, #{
    <<"id">> => ?GID,
    <<"owner_uid">> => ?OWNER,
    <<"scope">> => <<"workspace">>
}).

with_group(DissolveCall, TestFun) ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'find_by_id', 2, fun(?GID, _Cols) -> ?GROUP_ROW end},
                {'dissolve_group', 4, fun(_U, _G, _O, _Row) -> DissolveCall end},
                {'dissolve_by_org_manager', 3, fun(_U, _G, _Row) -> DissolveCall end}
            ]},
            {organization_resource_authority, [
                {'ensure_manager', 2, fun(_Resource, _Uid) -> {error, {403, denied}} end}
            ]}
        ],
        TestFun
    ).

dissolve_authority_test_() ->
    [
        {"owner path unchanged (dissolve_group/4 + no org authority lookup)", fun() ->
            ?WITH_MECKS(
                [
                    {group_ds, [
                        {'find_by_id', 2, fun(?GID, _Cols) -> ?GROUP_ROW end},
                        {'dissolve_group', 4, fun(_U, _G, _O, _Row) -> ok end},
                        {'dissolve_by_org_manager', 3, fun(_U, _G, _Row) -> {error, not_used} end}
                    ]},
                    {organization_resource_authority, [
                        {'ensure_manager', 2, fun(_R, _U) -> {error, not_used} end}
                    ]}
                ],
                fun() ->
                    ?assertEqual(ok, group_logic:dissolve(?OWNER, ?GID)),
                    ?assertEqual(
                        0,
                        meck:num_calls(organization_resource_authority, ensure_manager, 2)
                    ),
                    ?assertEqual(0, meck:num_calls(group_ds, dissolve_by_org_manager, 3))
                end
            )
        end},
        {"org owner/admin can dissolve non-owned group via manager path", fun() ->
            ?WITH_MECKS(
                [
                    {group_ds, [
                        {'find_by_id', 2, fun(?GID, _Cols) -> ?GROUP_ROW end},
                        {'dissolve_group', 4, fun(_U, _G, _O, _Row) -> {error, not_used} end},
                        {'dissolve_by_org_manager', 3, fun(_U, _G, _Row) -> ok end}
                    ]},
                    {organization_resource_authority, [
                        {'ensure_manager', 2, fun(_R, _U) -> ok end}
                    ]}
                ],
                fun() ->
                    ?assertEqual(ok, group_logic:dissolve(?ORG_ADMIN, ?GID)),
                    ?assertEqual(
                        1, meck:num_calls(group_ds, dissolve_by_org_manager, 3)
                    ),
                    ?assertEqual(0, meck:num_calls(group_ds, dissolve_group, 4))
                end
            )
        end},
        {"non-manager rejected with legacy message and no DS write", fun() ->
            ?WITH_MECKS(
                [
                    {group_ds, [
                        {'find_by_id', 2, fun(?GID, _Cols) -> ?GROUP_ROW end},
                        {'dissolve_group', 4, fun(_U, _G, _O, _Row) -> ok end},
                        {'dissolve_by_org_manager', 3, fun(_U, _G, _Row) -> ok end}
                    ]},
                    {organization_resource_authority, [
                        {'ensure_manager', 2, fun(_R, _U) -> {error, {403, denied}} end}
                    ]}
                ],
                fun() ->
                    ?assertEqual({error, ?FORBIDDEN}, group_logic:dissolve(?OUTSIDER, ?GID)),
                    ?assertEqual(0, meck:num_calls(group_ds, dissolve_group, 4)),
                    ?assertEqual(0, meck:num_calls(group_ds, dissolve_by_org_manager, 3))
                end
            )
        end},
        {"authority db error propagates 503 fail-closed (no DS write)", fun() ->
            ?WITH_MECKS(
                [
                    {group_ds, [
                        {'find_by_id', 2, fun(?GID, _Cols) -> ?GROUP_ROW end},
                        {'dissolve_group', 4, fun(_U, _G, _O, _Row) -> ok end},
                        {'dissolve_by_org_manager', 3, fun(_U, _G, _Row) -> ok end}
                    ]},
                    {organization_resource_authority, [
                        {'ensure_manager', 2, fun(_R, _U) ->
                            {error, {503, <<"权限校验暂时不可用，请稍后重试"/utf8>>}}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {error, <<"权限校验暂时不可用", _/binary>>},
                        group_logic:dissolve(?OUTSIDER, ?GID)
                    ),
                    ?assertEqual(0, meck:num_calls(group_ds, dissolve_by_org_manager, 3))
                end
            )
        end},
        {"missing group returns not-found without authority lookup", fun() ->
            ?WITH_MECKS(
                [
                    {group_ds, [{'find_by_id', 2, fun(_G, _Cols) -> {error, not_found} end}]},
                    {organization_resource_authority, [
                        {'ensure_manager', 2, fun(_R, _U) -> ok end}
                    ]}
                ],
                fun() ->
                    ?assertEqual(
                        {error, <<"群组不存在"/utf8>>}, group_logic:dissolve(?OUTSIDER, ?GID)
                    ),
                    ?assertEqual(
                        0,
                        meck:num_calls(organization_resource_authority, ensure_manager, 2)
                    )
                end
            )
        end}
    ].
