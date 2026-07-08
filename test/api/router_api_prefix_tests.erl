-module(router_api_prefix_tests).

%% 回归测试：锁定 /api 统一前缀硬切换后的契约
%% 1) 所有 API 路由统一在 /api/v1/* 或 /api/adm/* 下；
%%    更早的旧路径（/v1/* /adm/* v0 裸根 /ws）已删除，不再双路并存。
%% 2) 2026-07-08：/api/* 裸 v0 业务路由（无 v1 段，如 /api/passport/login、
%%    /api/ws）也已下架，三个客户端确认迁移完毕后统一收口到 /api/v1/*；
%%    adm 路由保持 /api/adm/*（无 v1 版本，设计如此）。
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
        "旧裸根 /passport/login 应已删除，迁移至 /api/v1/passport/login"
    ),
    ?assert(sets:is_element(<<"/api/v1/passport/login">>, Paths)).

legacy_ws_removed_test() ->
    Paths = route_path_set(),
    ?assertNot(sets:is_element(<<"/ws">>, Paths), "旧 /ws 应已删除"),
    ?assert(sets:is_element(<<"/api/v1/ws">>, Paths)).

%% ---- v0 裸 /api/* 业务路由已下架（2026-07-08）----
v0_bare_api_paths_removed_test() ->
    Paths = route_path_set(),
    lists:foreach(
        fun(P) ->
            ?assertNot(
                sets:is_element(P, Paths),
                "v0 裸路径应已下架: " ++ binary_to_list(P)
            )
        end,
        [
            <<"/api/init">>,
            <<"/api/refreshtoken">>,
            <<"/api/ws">>,
            <<"/api/passport/login">>,
            <<"/api/passport/quick_login">>,
            <<"/api/user/show">>,
            <<"/api/conversation/online">>,
            <<"/api/friend/add">>,
            <<"/api/group/page">>
        ]
    ),
    lists:foreach(
        fun(P) ->
            ?assert(
                sets:is_element(P, Paths),
                "对应 v1 路径应存在: " ++ binary_to_list(P)
            )
        end,
        [
            <<"/api/v1/init">>,
            <<"/api/v1/refreshtoken">>,
            <<"/api/v1/ws">>,
            <<"/api/v1/passport/login">>,
            <<"/api/v1/passport/quick_login">>,
            <<"/api/v1/user/show">>,
            <<"/api/v1/conversation/online">>,
            <<"/api/v1/friend/add">>,
            <<"/api/v1/group/page">>
        ]
    ).

%% adm 路由设计上没有 v1 版本，保持 /api/adm/*，不受本次下架影响
adm_paths_kept_without_v1_test() ->
    Paths = route_path_set(),
    ?assert(sets:is_element(<<"/api/adm/setup/status">>, Paths)),
    ?assertNot(sets:is_element(<<"/api/v1/adm/setup/status">>, Paths)).

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
