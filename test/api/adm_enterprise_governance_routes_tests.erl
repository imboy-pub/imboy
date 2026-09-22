%% adm_enterprise_governance_routes_tests
%% FULL-08 — Admin 企业应用治理面（A-01..A-14）**路由注册契约**机械断言。
%%
%% 为什么需要这一层（不是冗余）：治理面的路径字符串在前端被单测钉死
%% （imboyadmin 分支 run/full-candidate-admin-20260921T101806Z 的
%% src/modules/enterprise_apps/api/contracts.ts:ENDPOINTS），后端一旦漂移，
%% 前端会静默退化为「404 → notFound」而不是报错。本套件把 14 条路径、handler、
%% action 与硬边界钉在后端 beam 上，漂移即红灯。
%%
%% 硬边界断言（plan 产品硬边界 §1、§2）：
%%   ① 14 条路径各注册**恰好一次**（重复注册会让 cowboy 首次匹配胜出，
%%      第二条永远不生效，属静默死路由）
%%   ② handler 恒为 adm_enterprise_application_handler（不接受其他 handler）
%%   ③ 公开开放平台面 /api/open/v1/* 的生产路由数恒为 0
%%   ④ 治理面路径**不在** imboy_router:open() 里（不得匿名可达）
%%   ⑤ 三个前缀互不相交：/api/adm/enterprise/（治理）、
%%      /api/adm/enterprise-business/（enterprise_business 运营面）、
%%      /api/internal/v1/（OA Credential 面）——OA 凭据不能换 Admin 权限

-module(adm_enterprise_governance_routes_tests).

-include_lib("eunit/include/eunit.hrl").

%% 冻结路径（与前端 contracts.ts:ENDPOINTS 逐字对应；改这里必须同步改前端）
-define(GOVERNANCE_ROUTES, [
    %% A-01 GET 列表
    {<<"/api/adm/enterprise/organizations/:org_id/applications">>, applications},
    %% A-02 GET 详情
    {<<"/api/adm/enterprise/organizations/:org_id/applications/:application_id">>,
        application_detail},
    %% A-03 POST 生命周期（CAS）
    {<<"/api/adm/enterprise/organizations/:org_id/applications/:application_id/status">>,
        application_status},
    %% A-04 PUT scope（CAS）
    {<<"/api/adm/enterprise/organizations/:org_id/applications/:application_id/scopes">>,
        application_scopes},
    %% A-05 GET 列表 / A-06 POST 签发（同路径不同方法）
    {<<"/api/adm/enterprise/organizations/:org_id/applications/:application_id/credentials">>,
        credentials},
    %% A-07 POST 轮换
    {<<"/api/adm/enterprise/organizations/:org_id/applications/:application_id/credentials/:credential_id/rotate">>,
        credential_rotate},
    %% A-08 DELETE 撤销
    {<<"/api/adm/enterprise/organizations/:org_id/applications/:application_id/credentials/:credential_id">>,
        credential_revoke},
    %% A-09 GET 列表 / A-10 POST 新增（同路径不同方法）
    {<<"/api/adm/enterprise/organizations/:org_id/applications/:application_id/grants">>, grants},
    %% A-11 PATCH CAS 增删
    {<<"/api/adm/enterprise/organizations/:org_id/applications/:application_id/grants/:grant_id">>,
        grant},
    %% A-12 GET 投递统计
    {<<"/api/adm/enterprise/organizations/:org_id/applications/:application_id/delivery-stats">>,
        delivery_stats},
    %% A-13 GET 投递列表
    {<<"/api/adm/enterprise/organizations/:org_id/applications/:application_id/deliveries">>,
        deliveries},
    %% A-14 GET 审计
    {<<"/api/adm/enterprise/organizations/:org_id/applications/:application_id/audit-logs">>,
        audit_logs}
]).

all_test_() ->
    {inorder, [
        {"every_governance_route_registered_exactly_once", fun registered_exactly_once/0},
        {"governance_handler_and_actions_match", fun handler_and_actions/0},
        {"open_platform_surface_is_zero", fun open_platform_zero/0},
        {"governance_routes_never_anonymously_reachable", fun not_in_open_list/0},
        {"three_surfaces_are_prefix_disjoint", fun prefix_disjoint/0}
    ]}.

%%%===================================================================
%%% ① 每条路径恰好注册一次，且 handler/action 匹配
%%%===================================================================

registered_exactly_once() ->
    Routes = all_routes(),
    lists:foreach(
        fun({Path, _Action}) ->
            Matches = [R || {P, _H, _S} = R <- Routes, to_bin(P) =:= Path],
            ?assertEqual(
                1,
                length(Matches),
                "治理面路径应恰好注册一次: " ++ binary_to_list(Path)
            )
        end,
        ?GOVERNANCE_ROUTES
    ).

handler_and_actions() ->
    Routes = all_routes(),
    lists:foreach(
        fun({Path, Action}) ->
            Matches = [{H, S} || {P, H, S} <- Routes, to_bin(P) =:= Path],
            ?assertEqual(1, length(Matches)),
            [{Handler, Opts}] = Matches,
            ?assertEqual(
                adm_enterprise_application_handler,
                Handler,
                "handler 必须是 adm_enterprise_application_handler: " ++ binary_to_list(Path)
            ),
            ?assertEqual(Action, maps:get(action, Opts))
        end,
        ?GOVERNANCE_ROUTES
    ).

%%%===================================================================
%%% ③ 公开开放平台面恒为 0
%%%===================================================================

open_platform_zero() ->
    Paths = [P || {P, _H, _S} <- all_routes()],
    Open = [to_bin(P) || P <- Paths, is_open_platform(to_bin(P))],
    ?assertEqual([], Open, "近期不建设 Open Platform：/api/open/v1/* 生产路由必须为 0").

is_open_platform(<<"/api/open/v1", _/binary>>) -> true;
is_open_platform(_) -> false.

%%%===================================================================
%%% ④ 治理面不得匿名可达
%%%===================================================================

not_in_open_list() ->
    Open = [to_bin(P) || P <- imboy_router:open()],
    lists:foreach(
        fun({Path, _Action}) ->
            ?assertNot(
                lists:member(Path, Open),
                "治理面不得出现在 open()（匿名可达）: " ++ binary_to_list(Path)
            )
        end,
        ?GOVERNANCE_ROUTES
    ).

%%%===================================================================
%%% ⑤ 三个 surface 前缀互不相交
%%%===================================================================

prefix_disjoint() ->
    Paths = [to_bin(P) || {P, _H, _S} <- all_routes()],
    Gov = [P || P <- Paths, is_governance(P)],
    Eb = [P || P <- Paths, is_eb_platform(P)],
    Internal = [P || P <- Paths, is_internal(P)],
    ?assertEqual(12, length(Gov)),
    %% 治理面路径不落在 enterprise-business 运营面或 OA internal 面之下
    ?assertEqual([], [P || P <- Gov, is_eb_platform(P)]),
    ?assertEqual([], [P || P <- Gov, is_internal(P)]),
    ?assertEqual([], [P || P <- Internal, is_governance(P)]),
    ?assert(lists:all(fun(P) -> is_internal(P) end, Internal)),
    ?assert(lists:all(fun(P) -> is_eb_platform(P) end, Eb)).

is_governance(<<"/api/adm/enterprise/organizations/", _/binary>>) -> true;
is_governance(_) -> false.

is_eb_platform(<<"/api/adm/enterprise-business/", _/binary>>) -> true;
is_eb_platform(_) -> false.

is_internal(<<"/api/internal/v1/", _/binary>>) -> true;
is_internal(_) -> false.

%%%===================================================================
%%% helpers
%%%===================================================================

to_bin(P) when is_binary(P) -> P;
to_bin(P) when is_list(P) -> unicode:characters_to_binary(P).

all_routes() ->
    lists:flatmap(fun({_Host, Routes}) -> Routes end, imboy_router:get_routes()).
