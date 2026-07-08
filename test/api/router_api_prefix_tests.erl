-module(router_api_prefix_tests).

%% 回归测试：锁定 /api 统一前缀硬切换后的契约
%% 1) 所有 API 路由统一在 /api/* 下（/api/v1/* /api/adm/* /api/<v0 裸根> /api/ws），
%%    旧路径（/v1/* /adm/* v0 裸根 /ws）已删除，不再双路并存。
%% 2) 网站/静态白名单保留根路径，不加 /api。
%% 详见 .claude/PRPs/plans/completed/unify-api-prefix.plan.md（Task 9 硬切）

-include_lib("eunit/include/eunit.hrl").

%% ---- 旧路径已删除 ----
legacy_v1_path_removed_test() ->
    Paths = route_path_set(),
    ?assertNot(
        sets:is_element(<<"/v1/passport/login">>, Paths),
        "旧 /v1/passport/login 应已删除，迁移至 /api/v1/passport/login"
    ),
    ?assert(sets:is_element(<<"/api/v1/passport/login">>, Paths)).

legacy_adm_path_removed_test() ->
    Paths = route_path_set(),
    ?assertNot(
        sets:is_element(<<"/adm/setup/status">>, Paths),
        "旧 /adm/setup/status 应已删除，迁移至 /api/adm/setup/status"
    ),
    ?assert(sets:is_element(<<"/api/adm/setup/status">>, Paths)).

legacy_bare_root_removed_test() ->
    Paths = route_path_set(),
    ?assertNot(
        sets:is_element(<<"/passport/login">>, Paths),
        "旧裸根 /passport/login 应已删除，迁移至 /api/passport/login"
    ),
    ?assert(sets:is_element(<<"/api/passport/login">>, Paths)).

legacy_ws_removed_test() ->
    Paths = route_path_set(),
    ?assertNot(sets:is_element(<<"/ws">>, Paths), "旧 /ws 应已删除"),
    ?assert(sets:is_element(<<"/api/ws">>, Paths)),
    ?assert(sets:is_element(<<"/api/v1/ws">>, Paths)).

%% ---- 网站/静态白名单：不加 /api，保留根路径 ----
website_whitelist_kept_at_root_test() ->
    Paths = route_path_set(),
    lists:foreach(
        fun(P) ->
            ?assert(sets:is_element(P, Paths), "白名单路径应保留在根: " ++ binary_to_list(P))
        end,
        [
            <<"/help">>,
            <<"/brand">>,
            <<"/metrics">>,
            <<"/privacy-policy">>,
            <<"/account-deletion">>,
            <<"/">>,
            <<"/static/[...]">>,
            <<"/static/admin/[...]">>
        ]
    ),
    lists:foreach(
        fun(P) ->
            ?assertNot(sets:is_element(P, Paths), "白名单不得加 /api: " ++ binary_to_list(P))
        end,
        [
            <<"/api/help">>,
            <<"/api/brand">>,
            <<"/api/metrics">>,
            <<"/api/privacy-policy">>,
            <<"/api/account-deletion">>,
            <<"/api/">>,
            <<"/api/static/[...]">>
        ]
    ).

%% ---- open/0 与 option/0 已统一 /api，旧路径不再返回 ----
open_uses_api_only_test() ->
    Open = imboy_router:open(),
    ?assertNot(lists:member(<<"/v1/passport/login">>, Open), "open/0 不应再含旧 /v1 路径"),
    ?assert(lists:member(<<"/api/v1/passport/login">>, Open)),
    ?assert(lists:member(<<"/help">>, Open), "网站白名单仍应在 open/0"),
    ?assertNot(lists:member(<<"/api/help">>, Open)).

option_uses_api_only_test() ->
    Option = imboy_router:option(),
    ?assertNot(lists:member(<<"/v1/feedback/add">>, Option), "option/0 不应再含旧 /v1 路径"),
    ?assert(lists:member(<<"/api/v1/feedback/add">>, Option)).

%% ---- helpers（镜像 router_consistency_tests）----
-spec route_path_set() -> sets:set(binary()).
route_path_set() ->
    sets:from_list([unicode:characters_to_binary(P) || {P, _H, _S} <- all_routes()]).

-spec all_routes() -> [tuple()].
all_routes() ->
    lists:flatmap(fun({_Host, Routes}) -> Routes end, imboy_router:get_routes()).
