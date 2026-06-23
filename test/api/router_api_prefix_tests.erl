-module(router_api_prefix_tests).

%% 回归测试：锁定 /api 统一前缀迁移的两条契约
%% 1) 双路并存——旧路径（/v1/* /adm/* v0 裸根 /ws）与 /api 前缀别名同时存在，
%%    且别名与原路由指向同一 handler（老客户端不断线）。
%% 2) 网站/静态白名单不得生成 /api 别名（保留根路径，供反代流量分流）。
%% 详见 .claude/PRPs/plans/completed/unify-api-prefix.plan.md

-include_lib("eunit/include/eunit.hrl").

%% ---- 双路并存 ----
dual_path_v1_aliased_test() ->
    Paths = route_path_set(),
    ?assert(sets:is_element(<<"/v1/passport/login">>, Paths)),
    ?assert(sets:is_element(<<"/api/v1/passport/login">>, Paths)).

dual_path_adm_aliased_test() ->
    Paths = route_path_set(),
    ?assert(sets:is_element(<<"/adm/setup/status">>, Paths)),
    ?assert(sets:is_element(<<"/api/adm/setup/status">>, Paths)).

dual_path_bare_root_aliased_test() ->
    Paths = route_path_set(),
    ?assert(sets:is_element(<<"/passport/login">>, Paths)),
    ?assert(sets:is_element(<<"/api/passport/login">>, Paths)).

dual_path_ws_aliased_test() ->
    Paths = route_path_set(),
    ?assert(sets:is_element(<<"/ws">>, Paths)),
    ?assert(sets:is_element(<<"/api/ws">>, Paths)),
    ?assert(sets:is_element(<<"/api/v1/ws">>, Paths)).

%% 别名必须与原路由指向同一 handler
alias_same_handler_test() ->
    ?assertEqual(
        handler_of(<<"/v1/passport/login">>),
        handler_of(<<"/api/v1/passport/login">>)
    ),
    ?assertEqual(
        handler_of(<<"/adm/setup/status">>),
        handler_of(<<"/api/adm/setup/status">>)
    ).

%% ---- 网站/静态白名单：不得加 /api 前缀别名 ----
website_whitelist_not_aliased_test() ->
    Paths = route_path_set(),
    lists:foreach(
        fun(P) ->
            ?assertNot(sets:is_element(P, Paths))
        end,
        [
            <<"/api/help">>,
            <<"/api/brand">>,
            <<"/api/metrics">>,
            <<"/api/privacy-policy">>,
            <<"/api/account-deletion">>,
            <<"/api/">>,
            <<"/api/static/[...]">>,
            <<"/api/static/admin/[...]">>
        ]
    ),
    %% 原网站路径仍保留在根
    ?assert(sets:is_element(<<"/help">>, Paths)),
    ?assert(sets:is_element(<<"/metrics">>, Paths)),
    ?assert(sets:is_element(<<"/static/[...]">>, Paths)).

%% ---- open/0 与 option/0 含 /api 别名且保留旧路径，白名单除外 ----
open_has_api_alias_test() ->
    Open = imboy_router:open(),
    ?assert(lists:member(<<"/v1/passport/login">>, Open)),
    ?assert(lists:member(<<"/api/v1/passport/login">>, Open)),
    ?assertNot(lists:member(<<"/api/help">>, Open)),
    ?assertNot(lists:member(<<"/api/metrics">>, Open)).

option_has_api_alias_test() ->
    Option = imboy_router:option(),
    ?assert(lists:member(<<"/v1/feedback/add">>, Option)),
    ?assert(lists:member(<<"/api/v1/feedback/add">>, Option)).

%% ---- helpers（镜像 router_consistency_tests）----
-spec route_path_set() -> sets:set(binary()).
route_path_set() ->
    sets:from_list([unicode:characters_to_binary(P) || {P, _H, _S} <- all_routes()]).

handler_of(Bin) ->
    [H | _] = [
        H0
     || {P, H0, _S} <- all_routes(),
        unicode:characters_to_binary(P) =:= Bin
    ],
    H.

-spec all_routes() -> [tuple()].
all_routes() ->
    lists:flatmap(fun({_Host, Routes}) -> Routes end, imboy_router:get_routes()).
