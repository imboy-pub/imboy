-module(payment_stripe_gateway).
-behaviour(payment_gateway).

%%%===================================================================
%%% @doc Stripe 支付网关 —— PaymentIntent
%%%
%%% 两种运行模式（由 sys.config {payment_mode, _} 决定）：
%%%   - sandbox（当前默认）：不发起真实请求，本地生成沙箱支付单与
%%%     模拟 client_secret，供上层联调跑通整条链路。
%%%   - live：真实接入骨架（创建 PaymentIntent）。真实凭据为空时返回
%%%     {error, 未配置真实凭据}。真实 HTTP/验签调用点以 TODO 注明，
%%%     沙箱阶段不引入额外 HTTP/加密依赖。
%%%
%%% 凭据（IMBOY_* 注入，application:get_env/3 读取）：
%%%   stripe_secret_key / stripe_webhook_secret
%%%
%%% Amount: 订单金额（元/主币种单位；integer/float/binary/list）。
%%%         Stripe amount 以最小货币单位（cents）传输。本网关不扣钱包，
%%%         仅负责创建支付意图（钱包支付走 payment_wallet_gateway）。
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
    PaymentNo = <<"SANDBOX_STRIPE_", OrderNo/binary>>,
    %% 模拟 PaymentIntent 返回：客户端确认支付所需的 client_secret 占位
    _ClientSecret = <<"SANDBOX_pi_", OrderNo/binary, "_secret_test">>,
    lager:info(
        "[payment][stripe][sandbox] 沙箱已受理 order=~ts amount=~ts payment_no=~ts client_secret=~ts",
        [OrderNo, elib_cnv:safe_to_binary(Amount), PaymentNo, _ClientSecret]
    ),
    {ok, PaymentNo}.

-spec sandbox_refund(binary(), term()) -> ok.
sandbox_refund(PaymentNo, Amount) ->
    lager:info(
        "[payment][stripe][sandbox] 沙箱退款受理 payment_no=~ts amount=~ts",
        [PaymentNo, elib_cnv:safe_to_binary(Amount)]
    ),
    ok.

%%%===================================================================
%%% live 模式 —— 真实接入骨架（沙箱阶段不实际发起请求）
%%%===================================================================

-spec live_pay(binary(), term(), map()) -> {ok, binary()} | {error, binary()}.
live_pay(OrderNo, Amount, _Opts) ->
    case credentials() of
        {ok, #{secret_key := SecretKey}} ->
            %% 金额换算：Stripe amount 单位为最小货币单位（cents）
            AmountCents = yuan_to_cents(Amount),
            %% TODO[live]: 创建 PaymentIntent
            %%   POST https://api.stripe.com/v1/payment_intents
            %%   body(x-www-form-urlencoded): amount=AmountCents&currency=usd
            %%     &metadata[out_trade_no]=OrderNo&automatic_payment_methods[enabled]=true
            %% TODO[live]: Authorization: Bearer SecretKey（HTTP Basic 亦可）
            %% TODO[live]: 解析响应 id(pi_xxx) 与 client_secret，
            %%   PaymentNo 用 PaymentIntent id，client_secret 返回客户端确认。
            %% TODO[live]: Webhook 用 stripe_webhook_secret 校验签名（Stripe-Signature 头）。
            _ = {SecretKey, OrderNo, AmountCents},
            PaymentNo = <<"STRIPE_", OrderNo/binary>>,
            {ok, PaymentNo};
        {error, Reason} ->
            {error, Reason}
    end.

-spec live_refund(binary(), term()) -> ok | {error, binary()}.
live_refund(PaymentNo, Amount) ->
    case credentials() of
        {ok, #{secret_key := SecretKey}} ->
            AmountCents = yuan_to_cents(Amount),
            %% TODO[live]: 创建 Refund
            %%   POST https://api.stripe.com/v1/refunds
            %%   body: payment_intent=PaymentNo&amount=AmountCents
            %% TODO[live]: 校验响应 status=succeeded，否则返回 {error, Message}
            _ = {SecretKey, PaymentNo, AmountCents},
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

%% @doc 读取并校验真实凭据；secret_key 为空则视为未配置
%% （webhook_secret 仅回调验签用，下单不强制要求，故下单只校验 secret_key）
-spec credentials() -> {ok, map()} | {error, binary()}.
credentials() ->
    SecretKey = application:get_env(imboy, stripe_secret_key, <<>>),
    WebhookSecret = application:get_env(imboy, stripe_webhook_secret, <<>>),
    case is_blank(SecretKey) of
        true ->
            {error, <<"支付网关未配置真实凭据"/utf8>>};
        false ->
            {ok, #{secret_key => SecretKey, webhook_secret => WebhookSecret}}
    end.

-spec is_blank(term()) -> boolean().
is_blank(<<>>) -> true;
is_blank("") -> true;
is_blank(undefined) -> true;
is_blank(_) -> false.

%%%===================================================================
%%% 主币种单位 → 最小货币单位（cents）安全换算（避免浮点精度误差）
%%%===================================================================

-spec yuan_to_cents(term()) -> integer().
yuan_to_cents(V) when is_integer(V) -> V * 100;
yuan_to_cents(V) when is_float(V) -> round(V * 100);
yuan_to_cents(V) when is_binary(V) -> yuan_to_cents_bin(V);
yuan_to_cents(V) when is_list(V) -> yuan_to_cents_bin(list_to_binary(V));
yuan_to_cents(_) -> 0.

-spec yuan_to_cents_bin(binary()) -> integer().
yuan_to_cents_bin(Bin) ->
    case binary:split(Bin, <<".">>) of
        [IntPart] ->
            safe_int(IntPart) * 100;
        [IntPart, FracPart] ->
            safe_int(IntPart) * 100 + safe_int(normalize_frac(FracPart));
        _ ->
            0
    end.

-spec normalize_frac(binary()) -> binary().
normalize_frac(F) ->
    case byte_size(F) of
        0 -> <<"00">>;
        1 -> <<F/binary, "0">>;
        _ -> binary:part(F, 0, 2)
    end.

-spec safe_int(binary()) -> integer().
safe_int(B) ->
    case catch binary_to_integer(B) of
        I when is_integer(I) -> I;
        _ -> 0
    end.
