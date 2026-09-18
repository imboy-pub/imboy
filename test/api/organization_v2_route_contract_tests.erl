-module(organization_v2_route_contract_tests).

%% ORG-10（API Contract and Router Integration）路由契约测试。
%%
%% 守护面（对照派工卡契约门）：
%%   * Organization V1 v2 面 19 条路由一次性集中注册且 handler/action 正确
%%     （16 条 ORG-10 原有 + ORG-BACKEND-GAP 补齐的成员生命周期
%%     suspend/restore/offboard 3 条）；
%%   * 固定路径（deletion-preflight / invitations/mine）必须注册在
%%     :organization_id 通配之前防遮蔽（同 channel/qrcode 先例）；
%%   * 全路由表零重复路径（一次性集中注册不产生双 OWNER）；
%%   * 既有 legacy 面零意外消失（direct-add adapter 仍在，C11 TRANSITION）；
%%   * 新路由全部不在 open 白名单（必须 JWT 认证）。

-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% v2 面路由注册（handler + action 逐条冻结）
%% ------------------------------------------------------------------

org_v2_routes_registered_test() ->
    Expected = org_v2_expected_routes(),
    Missing = lists:filter(fun(Route) -> not route_registered(Route) end, Expected),
    ?assertEqual([], Missing).

%% 固定路径必须先于 :organization_id 通配（cowboy 顺序匹配，反被通配遮蔽）
org_fixed_paths_before_wildcard_test() ->
    Routes = org_routes(),
    WildcardIndex = index_of(<<"/api/v1/organizations/:organization_id">>, Routes),
    ?assert(is_integer(WildcardIndex), "org detail 通配路由必须存在"),
    FixedPaths = [
        <<"/api/v1/organizations/deletion-preflight">>,
        <<"/api/v1/organizations/invitations/mine">>
    ],
    Shadowed = [
        P
     || P <- FixedPaths,
        begin
            I = index_of(P, Routes),
            not (is_integer(I) andalso I < WildcardIndex)
        end
    ],
    ?assertEqual([], Shadowed).

%% 一次性集中注册：全路由表零重复路径（无双 OWNER）
org_v2_no_duplicate_paths_test() ->
    Paths = [P || {P, _H, _S} <- all_routes()],
    Dup = Paths -- lists:usort(Paths),
    ?assertEqual([], Dup).

%% legacy direct-add adapter 仍在（C11 TRANSITION：观测归零后才移除）
org_legacy_direct_add_preserved_test() ->
    %% C11 legacy direct-add = POST .../members/:user_id（单用户路径，v1 既有）
    Legacy =
        {<<"/api/v1/organizations/:organization_id/members/:user_id">>, organization_member_handler,
            #{
                action => member
            }},
    ?assert(route_registered(Legacy)).

%% 新 v2 路由全部须认证（不在 open 白名单）
org_v2_routes_not_in_open_list_test() ->
    Open = imboy_router:open(),
    Leakers = [
        P
     || {P, _H, _S} <- org_routes_only_v2(),
        lists:member(P, Open)
    ],
    ?assertEqual([], Leakers).

%% handler 模块可加载（含新 organization_api_handler）
org_v2_handler_modules_exist_test() ->
    ?assertEqual(ok, assert_module_loaded(organization_api_handler)),
    ?assertEqual(ok, assert_module_loaded(organization_handler)).

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

org_v2_expected_routes() ->
    A = fun(Act) -> #{action => Act} end,
    [
        {<<"/api/v1/organizations/deletion-preflight">>, organization_handler,
            A(deletion_preflight)},
        {<<"/api/v1/organizations/invitations/mine">>, organization_api_handler,
            A(invitation_mine)},
        {<<"/api/v1/organizations/:organization_id/archive">>, organization_handler, A(archive)},
        {<<"/api/v1/organizations/:organization_id/restore">>, organization_handler, A(restore)},
        {<<"/api/v1/organizations/:organization_id/invitations">>, organization_api_handler,
            A(invitation_collection)},
        {<<"/api/v1/organizations/:organization_id/invitations/accept">>, organization_api_handler,
            A(invitation_accept)},
        {<<"/api/v1/organizations/:organization_id/invitations/:invitation_id/reject">>,
            organization_api_handler, A(invitation_reject)},
        {<<"/api/v1/organizations/:organization_id/invitations/:invitation_id/revoke">>,
            organization_api_handler, A(invitation_revoke)},
        {<<"/api/v1/organizations/:organization_id/departments">>, organization_api_handler,
            A(department_collection)},
        {<<"/api/v1/organizations/:organization_id/departments/:department_id">>,
            organization_api_handler, A(department_item)},
        {<<"/api/v1/organizations/:organization_id/departments/:department_id/move">>,
            organization_api_handler, A(department_move)},
        {<<"/api/v1/organizations/:organization_id/departments/:department_id/archive">>,
            organization_api_handler, A(department_archive)},
        {<<"/api/v1/organizations/:organization_id/departments/:department_id/members">>,
            organization_api_handler, A(department_member_collection)},
        {<<"/api/v1/organizations/:organization_id/departments/:department_id/members/:user_id">>,
            organization_api_handler, A(department_member_item)},
        {<<
                "/api/v1/organizations/:organization_id/departments/:department_id/members/:user_id/admin"
            >>,
            organization_api_handler, A(department_member_admin)},
        {<<"/api/v1/organizations/:organization_id/members/:user_id/suspend">>,
            organization_member_handler, A(member_suspend)},
        {<<"/api/v1/organizations/:organization_id/members/:user_id/restore">>,
            organization_member_handler, A(member_restore)},
        {<<"/api/v1/organizations/:organization_id/members/:user_id/offboard">>,
            organization_member_handler, A(member_offboard)},
        {<<"/api/v1/organizations/:organization_id/default-workspace">>, organization_api_handler,
            A(default_workspace)}
    ].

route_registered(Route) ->
    lists:member(Route, all_routes()).

org_routes() ->
    [P || P = {Path, _H, _S} <- all_routes(), is_org_path(Path)].

org_routes_only_v2() ->
    [
        {P, H, S}
     || {P, H, S} <- all_routes(),
        is_org_path(P),
        H =:= organization_api_handler orelse
            lists:member(S, [
                #{action => deletion_preflight},
                #{action => archive},
                #{action => restore},
                #{action => member_suspend},
                #{action => member_restore},
                #{action => member_offboard}
            ])
    ].

is_org_path(<<"/api/v1/organizations", _/binary>>) ->
    true;
is_org_path(_) ->
    false.

index_of(Path, Routes) ->
    index_of(Path, Routes, 1).

index_of(_Path, [], _I) ->
    not_found;
index_of(Path, [{Path, _H, _S} | _Rest], I) ->
    I;
index_of(Path, [_ | Rest], I) ->
    index_of(Path, Rest, I + 1).

all_routes() ->
    %% 路由表 path 是 string（char list），统一转 binary 比对（同 router_consistency_tests 口径）
    lists:flatmap(
        fun({_Host, Routes}) ->
            [
                {unicode:characters_to_binary(P), H, S}
             || {P, H, S} <- Routes
            ]
        end,
        imboy_router:get_routes()
    ).

assert_module_loaded(Mod) ->
    case code:which(Mod) of
        non_existing -> {error, {module_missing, Mod}};
        _ -> ok
    end.
