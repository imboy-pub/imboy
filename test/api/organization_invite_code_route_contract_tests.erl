-module(organization_invite_code_route_contract_tests).

%% GZAPP-01（Organization Invite Code + Join Orchestrator）路由契约测试。
%%
%% 镜像 organization_v2_route_contract_tests 模式，守护面：
%%   * invite_code 两条路由（同路径 GET/POST/DELETE 分派 + join 子路径）
%%     注册且 handler/action 正确；
%%   * join 子路径不得被父路径遮蔽语义破坏（两者 action 不同，靠
%%     cowboy 精确段匹配共存——断言两条都在表中且 action 各自正确）；
%%   * 全路由表零重复路径（不产生双 OWNER）；
%%   * 新路由不在 open 白名单（必须 JWT 认证）。

-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% 路由注册（handler + action 逐条冻结）
%% ------------------------------------------------------------------

invite_code_routes_registered_test() ->
    Expected = invite_code_expected_routes(),
    Missing = lists:filter(fun(Route) -> not route_registered(Route) end, Expected),
    ?assertEqual([], Missing).

%% invite_code 面追加后全路由表仍零重复路径（无双 OWNER）
invite_code_no_duplicate_paths_test() ->
    Paths = [P || {P, _H, _S} <- all_routes()],
    Dup = Paths -- lists:usort(Paths),
    ?assertEqual([], Dup).

%% 新路由全部须认证（不在 open 白名单）
invite_code_routes_not_in_open_list_test() ->
    Open = imboy_router:open(),
    Leakers = [
        P
     || {P, _H, _S} <- invite_code_routes(),
        lists:member(P, Open)
    ],
    ?assertEqual([], Leakers).

%% handler 模块与 app 模块可加载（接线两侧都真实存在）
invite_code_modules_exist_test() ->
    ?assertEqual(ok, assert_module_loaded(organization_api_handler)),
    ?assertEqual(ok, assert_module_loaded(organization_invite_code_app)),
    ?assertEqual(ok, assert_module_loaded(organization_join_orchestrator)),
    ?assertEqual(ok, assert_module_loaded(organization_invite_code_pg)).

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

invite_code_expected_routes() ->
    A = fun(Act) -> #{action => Act} end,
    [
        {<<"/api/v1/organizations/:organization_id/invite_code">>, organization_api_handler,
            A(invite_code)},
        {<<"/api/v1/organizations/:organization_id/invite_code/join">>, organization_api_handler,
            A(invite_code_join)}
    ].

invite_code_routes() ->
    [R || R = {P, _H, _S} <- all_routes(), is_invite_code_path(P)].

is_invite_code_path(<<"/api/v1/organizations/:organization_id/invite_code", _/binary>>) ->
    true;
is_invite_code_path(_) ->
    false.

route_registered(Route) ->
    lists:member(Route, all_routes()).

all_routes() ->
    %% 路由表 path 是 string（char list），统一转 binary 比对
    %% （同 organization_v2_route_contract_tests 口径）
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
