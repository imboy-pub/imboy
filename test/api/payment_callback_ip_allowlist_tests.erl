-module(payment_callback_ip_allowlist_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc 支付回调 IP 白名单（A-04）的门控语义
%%%
%%% 回调端点免 JWT，是全站唯一由第三方直接触达的入账入口。本白名单是
%%% **纵深防御**，不是新的强制门：
%%%   1. 未配置（默认 []）→ 行为与加白名单前完全一致，验签照常执行
%%%   2. 配置且命中 → 照常进入验签
%%%   3. 配置且未命中 → 403，且**根本不进入 payment_callback_logic**
%%%
%%% 第 3 条同时锁定"回 403 而不是 200"：回 200 会让部分网关按已成功接收
%%% 停止重推，白名单配错就成了静默丢单。
%%% @end
%%%===================================================================

-define(GATEWAY_IP, {110, 75, 1, 2}).
-define(STRANGER_IP, {203, 0, 113, 9}).

setup() ->
    meck:new(cowboy_req, [no_link, passthrough]),
    meck:new(config_ds, [no_link, passthrough]),
    meck:new(payment_gateway, [no_link, passthrough]),
    meck:new(payment_callback_logic, [no_link, passthrough]),

    %% 网关总开关必须开，否则先被 A-29 的 ?ERR_FEATURE_DISABLED 拦掉，
    %% 测不到白名单这一层。
    meck:expect(payment_gateway, enabled, fun() -> true end),
    meck:expect(payment_callback_logic, handle, fun(_Gw, _Notify, _Ctx) -> {ok, #{}} end),

    meck:expect(cowboy_req, binding, fun(gateway, _Req) -> <<"alipay">> end),
    meck:expect(cowboy_req, read_body, fun(Req, _Opts) -> {ok, <<"{}">>, Req} end),
    meck:expect(cowboy_req, headers, fun(_Req) -> #{} end),
    meck:expect(cowboy_req, path, fun(_Req) -> <<"/api/v1/payment/callback/alipay">> end),
    meck:expect(cowboy_req, header, fun(_H, _Req, Default) -> Default end),
    %% reply/4 记录状态码到 Req，便于断言
    meck:expect(cowboy_req, reply, fun(Status, _Headers, Body, Req) ->
        Req#{replied => {Status, Body}}
    end),
    ok.

cleanup(_) ->
    meck:unload(payment_callback_logic),
    meck:unload(payment_gateway),
    meck:unload(config_ds),
    meck:unload(cowboy_req),
    ok.

mock_peer(Ip) ->
    meck:expect(cowboy_req, peer, fun(_Req) -> {Ip, 54321} end).

mock_allowlist(List) ->
    meck:expect(config_ds, env, fun
        (payment_callback_ip_allowlist, _Default) -> List;
        %% 直连对端即客户端；不经代理，XFF 不参与
        (trusted_proxy_ips, _Default) -> [];
        (_Key, Default) -> Default
    end).

run_init() ->
    {ok, Req, _State} = payment_callback_handler:init(#{}, #{action => notify}),
    Req.

status_of(Req) ->
    element(1, maps:get(replied, Req)).

ip_allowlist_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun unconfigured_allowlist_does_not_gate/0,
        fun configured_and_matching_ip_reaches_verification/0,
        fun configured_and_matching_prefix_reaches_verification/0,
        fun non_matching_ip_gets_403/0,
        fun non_matching_ip_never_reaches_verification/0
    ]}.

%% 默认 [] —— 加了这道门之后，未配置时行为必须与从前逐字节相同
unconfigured_allowlist_does_not_gate() ->
    mock_allowlist([]),
    mock_peer(?STRANGER_IP),
    Req = run_init(),
    ?assertEqual(200, status_of(Req)),
    ?assertEqual(1, meck:num_calls(payment_callback_logic, handle, 3)).

configured_and_matching_ip_reaches_verification() ->
    mock_allowlist([<<"110.75.1.2">>]),
    mock_peer(?GATEWAY_IP),
    Req = run_init(),
    ?assertEqual(200, status_of(Req)),
    ?assertEqual(1, meck:num_calls(payment_callback_logic, handle, 3)).

%% 网关按段公布出口 IP 时的常见配置写法
configured_and_matching_prefix_reaches_verification() ->
    mock_allowlist(["110.75."]),
    mock_peer(?GATEWAY_IP),
    Req = run_init(),
    ?assertEqual(200, status_of(Req)),
    ?assertEqual(1, meck:num_calls(payment_callback_logic, handle, 3)).

%% 403 而非 200：200 会让网关停止重推，配错白名单即静默丢单
non_matching_ip_gets_403() ->
    mock_allowlist([<<"110.75.">>]),
    mock_peer(?STRANGER_IP),
    Req = run_init(),
    ?assertEqual(403, status_of(Req)).

%% 门必须在验签之前 —— 否则它就不是"纵深防御"而只是个多余的日志点
non_matching_ip_never_reaches_verification() ->
    mock_allowlist([<<"110.75.">>]),
    mock_peer(?STRANGER_IP),
    _ = run_init(),
    ?assertEqual(0, meck:num_calls(payment_callback_logic, handle, 3)).
