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
-spec pay(binary(), term(), map()) ->
    {ok, binary()} | {ok, binary(), map()} | {error, binary()}.
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

%% live 模式经 erlang_pay 库真实下单（alipay.trade.app.pay）。
%% 金额：Amount 已是最小货币单位(分，recharge_order 存储)，直接作 amount_fen
%% 传入，不再 *100（与 Stripe 网关口径一致；旧 yuan_to_fen 基于 Amount=元的
%% 错误假设会收 100 倍钱，已弃用）。返回 order_str 供客户端 SDK 唤起。
-spec live_pay(binary(), term(), map()) ->
    {ok, binary()} | {ok, binary(), map()} | {error, binary()}.
live_pay(OrderNo, Amount, _Opts) ->
    case credentials() of
        {ok, Cred} ->
            Order = #{
                out_trade_no => OrderNo,
                amount_fen => to_minor_int(Amount),
                subject => <<"充值"/utf8>>
            },
            case erlang_pay:create_payment(alipay, epay_cfg(Cred), Order) of
                {ok, #{order_str := OrderStr}} ->
                    {ok, <<"ALIPAY_", OrderNo/binary>>, #{<<"order_str">> => OrderStr}};
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
                out_trade_no => strip_alipay_prefix(PaymentNo),
                refund_amount_fen => to_minor_int(Amount),
                out_request_no => <<"R_", PaymentNo/binary>>
            },
            case erlang_pay:refund(alipay, epay_cfg(Cred), RefundReq) of
                {ok, _Resp} -> ok;
                {error, Err} -> {error, epay_err_msg(Err)}
            end;
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

%%%===================================================================
%%% erlang_pay 适配
%%%===================================================================

%% imboy 凭据 map → erlang_pay 支付宝 Cfg
-spec epay_cfg(map()) -> map().
epay_cfg(#{app_id := AppId, private_key := PriKey, public_key := PubKey}) ->
    #{app_id => AppId, private_key => PriKey, public_key => PubKey}.

%% erlang_pay 统一错误 {Code::atom(), Msg::binary()} → 展示用 binary
-spec epay_err_msg(term()) -> binary().
epay_err_msg({_Code, Msg}) when is_binary(Msg) -> Msg;
epay_err_msg(Msg) when is_binary(Msg) -> Msg;
epay_err_msg(Other) -> elib_cnv:safe_to_binary(Other).

%% 去 ALIPAY_ 前缀还原原订单号（退款 out_trade_no 用）
-spec strip_alipay_prefix(binary()) -> binary().
strip_alipay_prefix(<<"ALIPAY_", Rest/binary>>) -> Rest;
strip_alipay_prefix(Other) -> Other.

%% Amount 已是最小货币单位(分)；兼容历史 binary/float/list 入参
-spec to_minor_int(term()) -> integer().
to_minor_int(V) when is_integer(V) -> V;
to_minor_int(V) when is_float(V) -> round(V);
to_minor_int(V) when is_binary(V) -> safe_int(V);
to_minor_int(V) when is_list(V) -> safe_int(list_to_binary(V));
to_minor_int(_) -> 0.

-spec safe_int(binary()) -> integer().
safe_int(B) ->
    case catch binary_to_integer(B) of
        I when is_integer(I) -> I;
        _ -> 0
    end.
