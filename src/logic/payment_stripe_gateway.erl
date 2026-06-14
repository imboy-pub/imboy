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

%% live 模式经 erlang_pay 库真实创建 PaymentIntent。
%% 金额：Amount 已是最小货币单位（分/cents，由 recharge_order 存储），
%% 直接作 amount_fen 传入，不再 *100（旧 yuan_to_cents 基于「Amount=元」的
%% 错误假设，已弃用）。currency 取订单币种（默认 cny），Stripe 要求小写。
-spec live_pay(binary(), term(), map()) ->
    {ok, binary()} | {ok, binary(), map()} | {error, binary()}.
live_pay(OrderNo, Amount, Opts) ->
    case credentials() of
        {ok, Cred} ->
            Order = #{
                out_trade_no => OrderNo,
                amount_fen => to_minor_int(Amount),
                currency => order_currency(Opts)
            },
            case erlang_pay:create_payment(stripe, epay_cfg(Cred), Order) of
                {ok, #{payment_no := PaymentNo, client_secret := Secret}} ->
                    {ok, PaymentNo, #{<<"client_secret">> => Secret}};
                {ok, #{payment_no := PaymentNo}} ->
                    {ok, PaymentNo};
                {error, Err} ->
                    {error, epay_err_msg(Err)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec live_refund(binary(), term()) -> ok | {error, binary()}.
live_refund(PaymentNo, Amount) ->
    case credentials() of
        {ok, Cred} ->
            RefundReq = #{
                payment_intent => PaymentNo,
                amount_fen => to_minor_int(Amount)
            },
            case erlang_pay:refund(stripe, epay_cfg(Cred), RefundReq) of
                {ok, _Resp} -> ok;
                {error, Err} -> {error, epay_err_msg(Err)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% imboy 凭据 map → erlang_pay Stripe Cfg。base_url 留空走库默认
%% （api.stripe.com）；sandbox 联调可经 IMBOY 配置覆盖（此处不读）。
-spec epay_cfg(map()) -> map().
epay_cfg(#{secret_key := SecretKey} = Cred) ->
    #{
        secret_key => SecretKey,
        webhook_secret => maps:get(webhook_secret, Cred, <<>>)
    }.

%% erlang_pay 统一错误 {Code::atom(), Msg::binary()} → 展示用 binary。
-spec epay_err_msg(term()) -> binary().
epay_err_msg({_Code, Msg}) when is_binary(Msg) -> Msg;
epay_err_msg(Msg) when is_binary(Msg) -> Msg;
epay_err_msg(Other) -> elib_cnv:safe_to_binary(Other).

%% 订单币种（Opts 透传），Stripe 要求小写 ISO 4217。
-spec order_currency(map()) -> binary().
order_currency(Opts) ->
    Cur = maps:get(currency, Opts, <<"cny">>),
    list_to_binary(string:lowercase(binary_to_list(elib_cnv:safe_to_binary(Cur)))).

%% Amount 已是最小货币单位（分）→ integer。兼容历史的 binary/float 入参。
-spec to_minor_int(term()) -> integer().
to_minor_int(V) when is_integer(V) -> V;
to_minor_int(V) when is_float(V) -> round(V);
to_minor_int(V) when is_binary(V) -> safe_int(V);
to_minor_int(V) when is_list(V) -> safe_int(list_to_binary(V));
to_minor_int(_) -> 0.

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
%%% 金额工具
%%%===================================================================

-spec safe_int(binary()) -> integer().
safe_int(B) ->
    case catch binary_to_integer(B) of
        I when is_integer(I) -> I;
        _ -> 0
    end.
