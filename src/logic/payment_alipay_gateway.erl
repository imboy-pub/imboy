-module(payment_alipay_gateway).
-behaviour(payment_gateway).

%%%===================================================================
%%% @doc 支付宝支付网关 —— App 支付 (alipay.trade.app.pay)
%%%
%%% 两种运行模式（由 sys.config {payment_mode, _} 决定）：
%%%   - sandbox（当前默认）：不发起真实请求，本地生成沙箱支付单与
%%%     模拟 form 参数，供上层联调跑通整条链路。
%%%   - live：真实接入骨架（RSA2 签名 + alipay.trade.app.pay）。
%%%     真实凭据为空时返回 {error, 未配置真实凭据}。真实 HTTP/签名
%%%     调用点以 TODO 注明，沙箱阶段不引入额外 HTTP/加密依赖。
%%%
%%% 凭据（IMBOY_* 注入，application:get_env/3 读取）：
%%%   alipay_app_id / alipay_private_key / alipay_public_key
%%%
%%% Amount: 订单金额（元；integer/float/binary/list）。本网关不扣钱包，
%%%         仅负责生成支付单/跳转参数（钱包支付走 payment_wallet_gateway）。
%%% @end
%%%===================================================================

-export([pay/3, refund/2]).

%% @doc 发起支付
-spec pay(binary(), term(), map()) -> {ok, binary()} | {error, binary()}.
pay(OrderNo, Amount, Opts) ->
    case payment_mode() of
        sandbox -> sandbox_pay(OrderNo, Amount, Opts);
        _ -> live_pay(OrderNo, Amount, Opts)
    end.

%% @doc 发起退款
-spec refund(binary(), term()) -> ok | {error, binary()}.
refund(PaymentNo, Amount) ->
    case payment_mode() of
        sandbox -> sandbox_refund(PaymentNo, Amount);
        _ -> live_refund(PaymentNo, Amount)
    end.

%%%===================================================================
%%% sandbox 模式 —— 本地受理，不发起真实请求
%%%===================================================================

-spec sandbox_pay(binary(), term(), map()) -> {ok, binary()}.
sandbox_pay(OrderNo, Amount, _Opts) ->
    PaymentNo = <<"SANDBOX_ALIPAY_", OrderNo/binary>>,
    %% 模拟支付参数：真实 live 时此处为客户端 SDK 唤起所需的 orderStr
    _OrderStr = build_sandbox_order_str(OrderNo, Amount),
    lager:info(
        "[payment][alipay][sandbox] 沙箱已受理 order=~ts amount=~ts payment_no=~ts",
        [OrderNo, elib_cnv:safe_to_binary(Amount), PaymentNo]
    ),
    {ok, PaymentNo}.

%% 模拟 alipay.trade.app.pay 返回的 orderStr 占位（form 字符串形态）
-spec build_sandbox_order_str(binary(), term()) -> binary().
build_sandbox_order_str(OrderNo, Amount) ->
    AmountBin = elib_cnv:safe_to_binary(Amount),
    iolist_to_binary([
        <<"app_id=SANDBOX_APP_ID">>,
        <<"&method=alipay.trade.app.pay">>,
        <<"&out_trade_no=">>,
        OrderNo,
        <<"&total_amount=">>,
        AmountBin,
        <<"&sign=SANDBOX_SIGN">>
    ]).

-spec sandbox_refund(binary(), term()) -> ok.
sandbox_refund(PaymentNo, Amount) ->
    lager:info(
        "[payment][alipay][sandbox] 沙箱退款受理 payment_no=~ts amount=~ts",
        [PaymentNo, elib_cnv:safe_to_binary(Amount)]
    ),
    ok.

%%%===================================================================
%%% live 模式 —— 真实接入骨架（沙箱阶段不实际发起请求）
%%%===================================================================

-spec live_pay(binary(), term(), map()) -> {ok, binary()} | {error, binary()}.
live_pay(OrderNo, Amount, _Opts) ->
    case credentials() of
        {ok, #{app_id := AppId, private_key := PriKey}} ->
            %% TODO[live]: 组装 alipay.trade.app.pay 业务参数 biz_content
            %%   #{out_trade_no => OrderNo, total_amount => Amount,
            %%     subject => ..., product_code => <<"QUICK_MSECURITY_PAY">>}
            %% TODO[live]: RSA2 签名 —— 用 PriKey 对待签名串做 SHA256withRSA
            %%   公钥参数 AppId 用于拼装请求公共参数。
            %% TODO[live]: 生成客户端唤起用 orderStr（已签名 query 串），
            %%   App 支付通常无需服务端 HTTP 请求，直接返回 orderStr 给客户端。
            _ = {AppId, PriKey, OrderNo, Amount},
            PaymentNo = <<"ALIPAY_", OrderNo/binary>>,
            {ok, PaymentNo};
        {error, Reason} ->
            {error, Reason}
    end.

-spec live_refund(binary(), term()) -> ok | {error, binary()}.
live_refund(PaymentNo, Amount) ->
    case credentials() of
        {ok, #{app_id := AppId, private_key := PriKey}} ->
            %% TODO[live]: 调用 alipay.trade.refund
            %%   biz_content = #{out_trade_no/trade_no => ..., refund_amount => Amount}
            %% TODO[live]: RSA2 签名后 HTTP POST 到 https://openapi.alipay.com/gateway.do
            %% TODO[live]: 验证响应 code=10000，否则返回 {error, SubMsg}
            _ = {AppId, PriKey, PaymentNo, Amount},
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% 配置读取
%%%===================================================================

-spec payment_mode() -> sandbox | live | term().
payment_mode() ->
    application:get_env(imboy, payment_mode, sandbox).

%% @doc 读取并校验真实凭据；任一为空则视为未配置
-spec credentials() -> {ok, map()} | {error, binary()}.
credentials() ->
    AppId = application:get_env(imboy, alipay_app_id, <<>>),
    PriKey = application:get_env(imboy, alipay_private_key, <<>>),
    PubKey = application:get_env(imboy, alipay_public_key, <<>>),
    case is_blank(AppId) orelse is_blank(PriKey) orelse is_blank(PubKey) of
        true ->
            {error, <<"支付网关未配置真实凭据"/utf8>>};
        false ->
            {ok, #{app_id => AppId, private_key => PriKey, public_key => PubKey}}
    end.

-spec is_blank(term()) -> boolean().
is_blank(<<>>) -> true;
is_blank("") -> true;
is_blank(undefined) -> true;
is_blank(_) -> false.
