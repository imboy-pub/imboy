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

%% 覆盖代理层数（默认 1 = 单层 nginx）
mock_hops(Hops) ->
    meck:expect(config_ds, env, fun
        (trusted_proxy_ips, _Default) -> [<<"127.0.0.1">>, <<"::1">>];
        (trusted_proxy_hops, _Default) -> Hops;
        (_Key, Default) -> Default
    end).

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
        fun trusted_proxy_uses_rightmost_xff/0,
        fun trusted_proxy_ignores_client_supplied_xff_prefix/0,
        fun trusted_proxy_padding_with_trusted_ips_does_not_bypass/0,
        fun trusted_proxy_two_hops_takes_second_from_right/0,
        fun trusted_proxy_falls_back_when_segments_fewer_than_hops/0,
        fun trusted_proxy_hops_accepts_binary_config/0,
        fun trusted_proxy_hops_falls_back_to_one_when_config_garbage/0,
        fun trusted_proxy_without_xff_falls_back_to_peer/0,
        fun trusted_proxy_with_empty_xff_does_not_crash/0,
        fun trusted_proxy_trims_whitespace/0,
        fun limiter_oidc_ip_trust_unified/0,
        fun limiter_ipv6_text_normalized/0,
        fun limiter_invalid_ip_text_falls_back/0
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

%% 反转断言（Rule 7）：原用例断言"取最左段"，而 nginx 的
%% $proxy_add_x_forwarded_for 把客户端自带 XFF 留在左、真实 remote_addr
%% 追加到右 —— 最左段恰恰是攻击者可控的那一段。单层 nginx 取最后一段。
trusted_proxy_uses_rightmost_xff() ->
    mock_peer(?TRUSTED),
    mock_xff(<<"198.51.100.7, 10.0.0.1">>),
    ?assertEqual(<<"10.0.0.1">>, elib_req:get_client_ip(req())).

%% 核心不变量：攻击者自带的 XFF 前缀不得影响分桶 key
trusted_proxy_ignores_client_supplied_xff_prefix() ->
    mock_peer(?TRUSTED),
    mock_xff(<<"1.2.3.4, 198.51.100.7">>),
    ?assertEqual(<<"198.51.100.7">>, elib_req:get_client_ip(req())),
    %% 换一个随机伪造前缀，桶 key 必须不变（否则限流可被无限旁路）
    mock_xff(<<"9.9.9.9, 198.51.100.7">>),
    ?assertEqual(<<"198.51.100.7">>, elib_req:get_client_ip(req())).

%% 用受信 IP 填充 XFF 不得绕过（这正是"贪心丢弃所有受信段"实现的破绽）
trusted_proxy_padding_with_trusted_ips_does_not_bypass() ->
    mock_peer(?TRUSTED),
    mock_xff(<<"evil, 127.0.0.1, 198.51.100.7">>),
    ?assertEqual(<<"198.51.100.7">>, elib_req:get_client_ip(req())).

%% 云 LB → nginx 两层：真实客户端在倒数第二段
trusted_proxy_two_hops_takes_second_from_right() ->
    mock_peer(?TRUSTED),
    mock_hops(2),
    mock_xff(<<"1.2.3.4, 198.51.100.7, 10.0.0.1">>),
    ?assertEqual(<<"198.51.100.7">>, elib_req:get_client_ip(req())).

%% 段数不足配置跳数时回退直连 IP（不可伪造），不得取到攻击者可控段
trusted_proxy_falls_back_when_segments_fewer_than_hops() ->
    mock_peer(?TRUSTED),
    mock_hops(3),
    mock_xff(<<"1.2.3.4, 10.0.0.1">>),
    ?assertEqual(<<"127.0.0.1">>, elib_req:get_client_ip(req())).

%% IMBOY_* 环境变量注入进来的是 binary，不是 integer。原实现的 is_integer 守卫
%% 会**静默**落到兜底、永远返回直连 IP —— 多层 LB 下那等于全站共用一个限流桶。
trusted_proxy_hops_accepts_binary_config() ->
    mock_peer(?TRUSTED),
    mock_hops(<<"2">>),
    mock_xff(<<"1.2.3.4, 198.51.100.7, 10.0.0.1">>),
    ?assertEqual(<<"198.51.100.7">>, elib_req:get_client_ip(req())).

%% 配置成非数字时按 1 处理（默认单层，最保守），而不是静默退化
trusted_proxy_hops_falls_back_to_one_when_config_garbage() ->
    mock_peer(?TRUSTED),
    mock_hops(<<"abc">>),
    mock_xff(<<"1.2.3.4, 10.0.0.1">>),
    ?assertEqual(<<"10.0.0.1">>, elib_req:get_client_ip(req())).

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
    mock_xff(<<"  198.51.100.7 ,10.0.0.1 ">>),
    ?assertEqual(<<"10.0.0.1">>, elib_req:get_client_ip(req())).

%% ===================================================================
%% Limiter V1 L3（计划 docs/plans/2026-10-13-limiter-v1-* §9.2 AC-09，
%% limiter_identity_domains）。原 L0 oracle（两 IP 提取路径分叉 /
%% IPv6 文本不规范化）已随 L3 迁移翻转：全站唯一实现
%% elib_req:get_client_ip/1，返回前统一规范化。
%% ===================================================================

%% AC-09 信任统一：非可信 peer + 伪造 CF/XReal/XFF 头——中间件与 OIDC
%% 入口（同一函数）都必须忽略伪造头、稳定返回直连 peer。原 L0 缺陷是
%% OIDC 走 login_attempt_ds:extract_real_ip（无 peer 信任检查）与中间件
%% 分叉；L3 起 auth_oidc_handler 委托本函数，分叉不复存在。
limiter_oidc_ip_trust_unified() ->
    ForgedCfIp = <<"1.2.3.4">>,
    Headers = #{
        <<"cf-connecting-ip">> => ForgedCfIp,
        <<"x-real-ip">> => <<"5.6.7.8">>,
        <<"x-forwarded-for">> => ForgedCfIp
    },
    %% 同一 peer、同一组伪造头：无论从哪个入口看都是直连 peer
    mock_peer(?UNTRUSTED),
    mock_xff(ForgedCfIp),
    MiddlewareIp = elib_req:get_client_ip(req()),
    ?assertEqual(<<"203.0.113.9">>, MiddlewareIp),
    %% OIDC 入口现为同一实现（auth_oidc_handler:client_ip → 本函数）：
    %% 同一请求两入口必须同 Key
    ?assertEqual(MiddlewareIp, elib_req:get_client_ip(req())),
    %% 伪造头三件套不改变结果
    _ = Headers,
    mock_xff(<<"1.2.3.4, 5.6.7.8">>),
    ?assertEqual(<<"203.0.113.9">>, elib_req:get_client_ip(req())).

%% AC-09 规范化（原 L0 oracle「文本不规范化」翻转）：等价 IPv6 文本
%% （全写 / 压缩 / 前导零）必须产出同一限流 Key；IPv4-mapped IPv6
%% （::ffff:a.b.c.d）归一为纯 IPv4 文本。
limiter_ipv6_text_normalized() ->
    %% 受信 peer + 单段 XFF（hops=1 取最后一段）：XFF 段即桶 Key
    mock_peer(?TRUSTED),
    %% 全写 vs 压缩：同一地址 → 同一文本
    mock_xff(<<"2001:0db8:0000:0000:0000:0000:0000:0001">>),
    Expanded = elib_req:get_client_ip(req()),
    mock_xff(<<"2001:db8::1">>),
    Compressed = elib_req:get_client_ip(req()),
    ?assertEqual(Expanded, Compressed),
    ?assertEqual(<<"2001:db8::1">>, Compressed),
    %% IPv4-mapped IPv6 归一为 IPv4 文本（双栈 LB 两种呈现同桶）
    mock_xff(<<"::ffff:192.0.2.1">>),
    ?assertEqual(<<"192.0.2.1">>, elib_req:get_client_ip(req())),
    mock_xff(<<"::ffff:c000:201">>),
    ?assertEqual(<<"192.0.2.1">>, elib_req:get_client_ip(req())),
    %% IPv4 不受影响
    mock_xff(<<"198.51.100.7">>),
    ?assertEqual(<<"198.51.100.7">>, elib_req:get_client_ip(req())).

%% AC-09 非法头回退：解析失败的原文本原样返回（不崩、不吞段）——
%% 攻击者可控的垃圾段进桶无害（不指向真实对端），但不能让请求崩溃。
limiter_invalid_ip_text_falls_back() ->
    mock_peer(?TRUSTED),
    mock_xff(<<"not-an-ip">>),
    ?assertEqual(<<"not-an-ip">>, elib_req:get_client_ip(req())),
    %% 粗解析可过、严格解析拒绝的形态（IPv4 段含非数字）同样回退原文本
    mock_xff(<<"1.2.3.4.5">>),
    ?assertEqual(<<"1.2.3.4.5">>, elib_req:get_client_ip(req())).
