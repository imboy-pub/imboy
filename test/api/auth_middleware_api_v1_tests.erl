-module(auth_middleware_api_v1_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc auth_middleware_api_v1 模块测试
%%%
%%% 回归 2026-07-08：43224c1f/4cc20e81 硬切换 /api 前缀后，本模块 execute/2
%%% 里判断是否需要 auth_ds:verify_sign/2（设备签名防篡改校验）的显式分支
%%% 一度仍匹配裸 /v1/ws、/v1/init、/v1/refreshtoken、/v1/passport/ 路径，
%%% 永远不命中真实的 /api/* 路径；这些端点虽在 open() 白名单里免 JWT，
%%% 但仍需 verify_sign，回归后被悄悄跳过。后又下架了 v0 裸 /api/* 路由，
%%% 只保留 /api/v1/* 形态，本文件只测 v1 路径。

verify_sign_called_for_ws_test_() ->
    verify_sign_called_case(<<"/api/v1/ws">>).

verify_sign_called_for_init_test_() ->
    verify_sign_called_case(<<"/api/v1/init">>).

verify_sign_called_for_refreshtoken_test_() ->
    verify_sign_called_case(<<"/api/v1/refreshtoken">>).

verify_sign_called_for_passport_test_() ->
    verify_sign_called_case(<<"/api/v1/passport/login">>).

verify_sign_called_case(Path) ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> Path end},
                {'header', 2, fun(<<"authorization">>, _Req) -> undefined end}
            ]},
            {config_ds, [
                {'env', 2, fun(api_auth_switch, _Def) -> <<"on">> end}
            ]},
            {auth_ds, [
                {'verify_sign', 2, fun(Req, Env) -> {ok, Req, Env} end},
                {'condition', 5, fun(_InOptionLi, _InOpenLi, _Auth, Req, Env) -> {ok, Req, Env} end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{},
            Result = auth_middleware_api_v1:execute(Req, Env),
            ?assertMatch({ok, _, _}, Result),
            ?assertEqual(1, meck:num_calls(auth_ds, verify_sign, 2))
        end
    ).

%% @doc 非 open 路径（普通受保护端点）也应触发 verify_sign（既有行为，防止误改）
verify_sign_called_for_protected_path_test_() ->
    verify_sign_called_case(<<"/api/v1/user/info">>).

%% @doc api_auth_switch=off 时任何路径都不应调用 verify_sign
verify_sign_skipped_when_switch_off_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/ws">> end},
                {'header', 2, fun(<<"authorization">>, _Req) -> undefined end}
            ]},
            {config_ds, [
                {'env', 2, fun(api_auth_switch, _Def) -> <<"off">> end}
            ]},
            {auth_ds, [
                {'verify_sign', 2, fun(Req, Env) -> {ok, Req, Env} end},
                {'condition', 5, fun(_InOptionLi, _InOpenLi, _Auth, Req, Env) -> {ok, Req, Env} end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{},
            Result = auth_middleware_api_v1:execute(Req, Env),
            ?assertMatch({ok, _, _}, Result),
            ?assertEqual(0, meck:num_calls(auth_ds, verify_sign, 2))
        end
    ).
