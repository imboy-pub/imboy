-module(auth_middleware_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc auth_middleware:execute/2 的前缀分发表
%%%
%%% 本模块此前**没有任何测试**——而它是全站请求的第一道分叉，一处前缀写错
%%% 就会让整类路由走错认证链路。审计 Critical NEW-A/#7 正是这么来的：
%%% 2026-07-08 路由由 /v1/* 改名 /api/v1/*，此处分发前缀漏改，导致
%%% auth_middleware_api_v1 成为死代码、318 条路由全落兜底分支，
%%% 支付回调与频道 webhook 被 902 拦死、passport 等丢失签名门。
%%% 修复见 commit 0456887f。
%%%
%%% 这些用例把每条分发去向钉死，特别是"裸 /v1/ 不得被当成 API v1"——
%%% 那正是当年写错的形态。
%%%===================================================================

%% 分发只看 path，把所有下游都打桩成可辨识的哨兵值
dispatch_mocks() ->
    [
        {cowboy_req, [
            {'path', 1, fun(Req) -> maps:get(path, Req, <<"/">>) end},
            {'header', 2, fun(<<"authorization">>, _Req) -> undefined end}
        ]},
        {auth_ds, [
            {'remove_last_forward_slash', 1, fun(P) -> P end},
            {'verify_sign', 2, fun(Req, Env) -> {ok, Req, Env} end},
            {'condition', 5, fun(_InOpt, _InOpen, _Auth, Req, Env) ->
                {ok, Req#{went => fallback}, Env}
            end}
        ]},
        {adm_auth_middleware, [
            {'execute', 2, fun(Req, Env) -> {ok, Req#{went => adm}, Env} end}
        ]},
        {auth_middleware_api_v1, [
            {'execute', 2, fun(Req, Env) -> {ok, Req#{went => api_v1}, Env} end}
        ]},
        {imboy_router, [
            {'open', 0, fun() -> [] end},
            {'option', 0, fun() -> [] end}
        ]},
        {config_ds, [
            {'env', 2, fun(api_auth_switch, D) -> D end}
        ]}
    ].

%% 跑一次 execute/2，返回它走了哪条分支
route(Path) ->
    {ok, Req, _Env} = auth_middleware:execute(#{path => Path}, #{}),
    maps:get(went, Req, passthrough).

%% ===================================================================
%% 核心回归守卫：/api/v1/* 必须委托给 auth_middleware_api_v1
%% ===================================================================

api_v1_prefix_delegates_to_api_v1_middleware_test_() ->
    ?WITH_MECKS(dispatch_mocks(), fun() ->
        ?assertEqual(api_v1, route(<<"/api/v1/passport/login">>)),
        ?assertEqual(api_v1, route(<<"/api/v1/payment/callback/alipay">>)),
        ?assertEqual(api_v1, route(<<"/api/v1/webhook/channel/abc">>)),
        ?assertEqual(api_v1, route(<<"/api/v1/ws">>))
    end).

%% 反向守卫：裸 /v1/* 不是真实路由（全仓 0 条），不得被当成 API v1。
%% 当年就是把分发写成了 <<"/v1/", _>>，才让 api_v1 中间件整个失联。
bare_v1_prefix_must_not_be_treated_as_api_v1_test_() ->
    ?WITH_MECKS(dispatch_mocks(), fun() ->
        ?assertEqual(fallback, route(<<"/v1/passport/login">>))
    end).

%% ===================================================================
%% 其余分支
%% ===================================================================

adm_prefixes_delegate_to_adm_middleware_test_() ->
    ?WITH_MECKS(dispatch_mocks(), fun() ->
        ?assertEqual(adm, route(<<"/adm/user/list">>)),
        %% /api/adm/* 同样走 adm，避免落客户端默认分支误触 902 签名门
        ?assertEqual(adm, route(<<"/api/adm/user/list">>))
    end).

static_and_webrtc_pass_through_without_auth_test_() ->
    ?WITH_MECKS(dispatch_mocks(), fun() ->
        ?assertEqual(passthrough, route(<<"/static/img/a.png">>)),
        ?assertEqual(passthrough, route(<<"/static/admin/index.html">>)),
        ?assertEqual(passthrough, route(<<"/webrtc/signal">>))
    end).

unmatched_path_falls_back_to_open_list_and_condition_test_() ->
    ?WITH_MECKS(dispatch_mocks(), fun() ->
        ?assertEqual(fallback, route(<<"/">>)),
        ?assertEqual(fallback, route(<<"/help">>)),
        %% /api/* 但不带 v1 段：v0 业务路由已下架，落兜底
        ?assertEqual(fallback, route(<<"/api/passport/login">>))
    end).

%% 相似前缀不得误匹配：/api/v11/ 与 /api/v1x 都不是 /api/v1/
lookalike_prefixes_do_not_match_api_v1_test_() ->
    ?WITH_MECKS(dispatch_mocks(), fun() ->
        ?assertEqual(fallback, route(<<"/api/v11/x">>)),
        ?assertEqual(fallback, route(<<"/api/v1x">>)),
        %% 恰好等于 /api/v1（无尾斜杠）也不该匹配 <<"/api/v1/", _>>
        ?assertEqual(fallback, route(<<"/api/v1">>))
    end).
