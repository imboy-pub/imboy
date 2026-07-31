-module(elib_req_client_ip_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc elib_req:get_client_ip/1 受信代理语义测试
%%%
%%% 背景：该函数是 throttle_middleware 的 passport_per_ip(10/min) 与
%%% api_per_ip(60/min) 两个限流桶的 key 来源。此前无条件采信
%%% x-forwarded-for 首段 —— 每个请求带一个随机 XFF 就能让桶 key 每次都新，
%%% 全站 IP 维度限流 100% 失效（登录爆破 / 验证码轰炸 / 注册刷量）。
%%%
%%% 本测试锁定：只有直连对端在白名单内时才采信 XFF。
%%% @end
%%%===================================================================

-define(TRUSTED, {127, 0, 0, 1}).
-define(UNTRUSTED, {203, 0, 113, 9}).

setup() ->
    meck:new(cowboy_req, [no_link, passthrough]),
    meck:new(config_ds, [no_link, passthrough]),
    meck:expect(config_ds, env, fun
        (trusted_proxy_ips, _Default) -> [<<"127.0.0.1">>, <<"::1">>];
        (_Key, Default) -> Default
    end),
    ok.

cleanup(_) ->
    meck:unload(config_ds),
    meck:unload(cowboy_req),
    ok.

%% Req 只是个占位 map，所有取值都被 meck 掉
req() -> #{}.

mock_peer(Ip) ->
    meck:expect(cowboy_req, peer, fun(_Req) -> {Ip, 54321} end).

mock_xff(Value) ->
    meck:expect(cowboy_req, header, fun
        (<<"x-forwarded-for">>, _Req, Default) ->
            case Value of
                undefined -> Default;
                V -> V
            end;
        (_H, _Req, Default) ->
            Default
    end).

get_client_ip_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun untrusted_peer_ignores_forged_xff/0,
        fun untrusted_peer_ignores_xff_even_with_many_hops/0,
        fun trusted_proxy_uses_leftmost_xff/0,
        fun trusted_proxy_without_xff_falls_back_to_peer/0,
        fun trusted_proxy_with_empty_xff_does_not_crash/0,
        fun trusted_proxy_trims_whitespace/0
    ]}.

%% 核心不变量：直连对端不可信时，伪造的 XFF 必须被完全忽略
untrusted_peer_ignores_forged_xff() ->
    mock_peer(?UNTRUSTED),
    mock_xff(<<"1.2.3.4">>),
    ?assertEqual(<<"203.0.113.9">>, elib_req:get_client_ip(req())).

%% 攻击者常用多跳伪造，同样不得采信
untrusted_peer_ignores_xff_even_with_many_hops() ->
    mock_peer(?UNTRUSTED),
    mock_xff(<<"1.1.1.1, 2.2.2.2, 3.3.3.3">>),
    ?assertEqual(<<"203.0.113.9">>, elib_req:get_client_ip(req())).

%% 受信代理转发时取最左段（最接近真实客户端）
trusted_proxy_uses_leftmost_xff() ->
    mock_peer(?TRUSTED),
    mock_xff(<<"198.51.100.7, 10.0.0.1">>),
    ?assertEqual(<<"198.51.100.7">>, elib_req:get_client_ip(req())).

trusted_proxy_without_xff_falls_back_to_peer() ->
    mock_peer(?TRUSTED),
    mock_xff(undefined),
    ?assertEqual(<<"127.0.0.1">>, elib_req:get_client_ip(req())).

%% 回归：原 passport_handler 版本用 hd/1，空值头会 badarg 崩掉请求
trusted_proxy_with_empty_xff_does_not_crash() ->
    mock_peer(?TRUSTED),
    mock_xff(<<>>),
    ?assertEqual(<<"127.0.0.1">>, elib_req:get_client_ip(req())),
    mock_xff(<<"  ,  ">>),
    ?assertEqual(<<"127.0.0.1">>, elib_req:get_client_ip(req())).

trusted_proxy_trims_whitespace() ->
    mock_peer(?TRUSTED),
    mock_xff(<<"  198.51.100.7 ,10.0.0.1">>),
    ?assertEqual(<<"198.51.100.7">>, elib_req:get_client_ip(req())).
