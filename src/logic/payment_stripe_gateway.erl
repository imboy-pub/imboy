-module(payment_stripe_gateway).
-compile([nowarn_deprecated_catch]).
-dialyzer({nowarn_function, [err_msg/1]}).
-behaviour(payment_gateway).

%%%===================================================================
%%% @doc Stripe 支付网关 —— erlang_pay 适配器（PaymentIntent）
%%%
%%% 纯适配层：读 imboy 凭据 → 组 Cfg/Order → 调 erlang_pay → 翻译返回。
%%% 真实 PaymentIntent/退款/Webhook 验签全在 erlang_pay(epay_stripe)；
%%% 本地联调统一用 payment_mock_gateway，本模块不含沙箱模拟。
%%%
%%% 凭据(IMBOY_* 注入)：stripe_secret_key / stripe_webhook_secret
%%% Amount：最小货币单位（分/cents），直接作 amount_fen 传入。
%%% Refund：refund/3 需 Opts.out_refund_no（业务退款唯一号）——erlang_pay
%%% 据此派生稳定幂等键；refund/2 缺号即拒绝，绝不无幂等键发退款。
%%% @end
%%%===================================================================

-export([pay/3, refund/2, refund/3]).

-spec pay(binary(), term(), map()) ->
    {ok, binary()} | {ok, binary(), map()} | {error, binary()}.
pay(OrderNo, Amount, Opts) ->
    case cfg() of
        {ok, Cfg} ->
            Order = #{
                out_trade_no => OrderNo,
                amount_fen => to_fen(Amount),
                currency => currency(Opts)
            },
            case erlang_pay:create_payment(stripe, Cfg, Order) of
                {ok, #{payment_no := PaymentNo, client_secret := Secret}} ->
                    {ok, PaymentNo, #{<<"client_secret">> => Secret}};
                {ok, #{payment_no := PaymentNo}} ->
                    {ok, PaymentNo};
                {error, Err} ->
                    {error, err_msg(Err)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 退款（兼容入口）。无退款上下文时委托 refund/3 —— Stripe 要求稳定
%% 退款唯一号派生幂等键，缺号即拒绝（fail-closed，不向网关发请求）。
-spec refund(binary(), term()) -> ok | {error, binary()}.
refund(PaymentNo, Amount) ->
    refund(PaymentNo, Amount, #{}).

%% @doc 退款（主入口）。Opts.out_refund_no 为业务退款唯一号
%% （payment_transaction.trade_no / channel_order.order_no）：
%% 同一笔业务退款重试 → 同一 out_refund_no → erlang_pay 派生同一
%% Idempotency-Key（rf_ 前缀）→ Stripe 同键 24h 内返回首次结果，
%% 不会重复退款。禁止用 payment_intent 派生（同一 PI 允许多次部分退款）。
-spec refund(binary(), term(), map()) -> ok | {error, binary()}.
refund(PaymentNo, Amount, Opts) ->
    case out_refund_no(Opts) of
        {ok, RefundNo} ->
            do_refund(PaymentNo, Amount, RefundNo);
        {error, _} = Err ->
            Err
    end.

-spec do_refund(binary(), term(), binary()) -> ok | {error, binary()}.
do_refund(PaymentNo, Amount, RefundNo) ->
    case cfg() of
        {ok, Cfg} ->
            Req = #{
                payment_intent => PaymentNo,
                amount_fen => to_fen(Amount),
                out_refund_no => RefundNo
            },
            case erlang_pay:refund(stripe, Cfg, Req) of
                {ok, _} -> ok;
                {error, Err} -> {error, err_msg(Err)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Opts 提取业务退款唯一号：必须为非空 binary，缺失/空值 fail-closed
%% （erlang_pay 幂等合同要求，绝不无键发退款 POST）。
-spec out_refund_no(map()) -> {ok, binary()} | {error, binary()}.
out_refund_no(Opts) ->
    case maps:get(out_refund_no, Opts, <<>>) of
        N when is_binary(N), N =/= <<>> ->
            {ok, N};
        _ ->
            {error, <<"Stripe 退款缺少业务退款唯一号(out_refund_no)，已拒绝"/utf8>>}
    end.

%% imboy 凭据 → erlang_pay Stripe Cfg；secret_key 为空视为未配置
%% （webhook_secret 仅回调验签用，下单不强制）
-spec cfg() -> {ok, map()} | {error, binary()}.
cfg() ->
    SecretKey = application:get_env(imboy, stripe_secret_key, <<>>),
    WebhookSecret = application:get_env(imboy, stripe_webhook_secret, <<>>),
    case SecretKey =/= <<>> of
        true ->
            {ok, #{secret_key => SecretKey, webhook_secret => WebhookSecret}};
        false ->
            {error, <<"支付网关未配置真实凭据"/utf8>>}
    end.

%% 订单币种（Opts 透传），Stripe 要求小写 ISO 4217
-spec currency(map()) -> binary().
currency(Opts) ->
    Cur = maps:get(currency, Opts, <<"cny">>),
    list_to_binary(string:lowercase(binary_to_list(elib_cnv:safe_to_binary(Cur)))).

%% Amount 已是分；确保 integer，不做 *100 换算
-spec to_fen(term()) -> integer().
to_fen(V) when is_integer(V) -> V;
to_fen(V) when is_float(V) -> round(V);
to_fen(V) when is_binary(V) -> safe_int(V);
to_fen(V) when is_list(V) -> safe_int(list_to_binary(V));
to_fen(_) -> 0.

-spec safe_int(binary()) -> integer().
safe_int(B) ->
    case catch binary_to_integer(B) of
        I when is_integer(I) -> I;
        _ -> 0
    end.

-spec err_msg({term(), binary()} | term()) -> binary().
err_msg({_Code, M}) when is_binary(M) -> M;
err_msg(O) -> elib_cnv:safe_to_binary(O).
