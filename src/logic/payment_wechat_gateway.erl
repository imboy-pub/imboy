-module(payment_wechat_gateway).
-behaviour(payment_gateway).

%%%===================================================================
%%% @doc 微信支付网关 —— APIv3（JSAPI / Native 下单）
%%%
%%% 两种运行模式（由 sys.config {payment_mode, _} 决定）：
%%%   - sandbox（当前默认）：不发起真实请求，本地生成沙箱支付单与
%%%     模拟 prepay_id，供上层联调跑通整条链路。
%%%   - live：真实接入骨架（APIv3 证书 + JSAPI/Native 下单）。
%%%     真实凭据为空时返回 {error, 未配置真实凭据}。真实 HTTP/验签
%%%     调用点以 TODO 注明，沙箱阶段不引入额外 HTTP/加密依赖。
%%%
%%% 凭据（IMBOY_* 注入，application:get_env/3 读取）：
%%%   wechat_mch_id / wechat_app_id / wechat_api_v3_key / wechat_cert_serial
%%%
%%% Amount: 订单金额（元；integer/float/binary/list）。本网关不扣钱包，
%%%         仅负责生成支付单/下单参数（钱包支付走 payment_wallet_gateway）。
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
    PaymentNo = <<"SANDBOX_WECHAT_", OrderNo/binary>>,
    %% 模拟下单返回：JSAPI 返回 prepay_id；此处给出 prepay_id 占位
    _PrepayId = <<"SANDBOX_PREPAY_", OrderNo/binary>>,
    lager:info(
        "[payment][wechat][sandbox] 沙箱已受理 order=~ts amount=~ts payment_no=~ts prepay_id=~ts",
        [OrderNo, elib_cnv:safe_to_binary(Amount), PaymentNo, _PrepayId]
    ),
    {ok, PaymentNo}.

-spec sandbox_refund(binary(), term()) -> ok.
sandbox_refund(PaymentNo, Amount) ->
    lager:info(
        "[payment][wechat][sandbox] 沙箱退款受理 payment_no=~ts amount=~ts",
        [PaymentNo, elib_cnv:safe_to_binary(Amount)]
    ),
    ok.

%%%===================================================================
%%% live 模式 —— 真实接入骨架（沙箱阶段不实际发起请求）
%%%===================================================================

-spec live_pay(binary(), term(), map()) -> {ok, binary()} | {error, binary()}.
live_pay(OrderNo, Amount, _Opts) ->
    case credentials() of
        {ok, #{
            mch_id := MchId,
            app_id := AppId,
            api_v3_key := ApiKey,
            cert_serial := Serial
        }} ->
            %% 金额换算：微信下单 total 单位为「分」(integer)
            TotalFen = yuan_to_fen(Amount),
            %% TODO[live]: 组装 JSAPI/Native 下单请求体
            %%   #{appid => AppId, mchid => MchId, out_trade_no => OrderNo,
            %%     amount => #{total => TotalFen, currency => <<"CNY">>},
            %%     description => ..., notify_url => ...}
            %% TODO[live]: APIv3 请求签名 —— 用商户私钥对
            %%   "METHOD\nURL\nTIMESTAMP\nNONCE\nBODY\n" 做 SHA256withRSA，
            %%   Authorization 头携带 mchid/serial_no(Serial)/nonce/timestamp/signature。
            %% TODO[live]: HTTP POST https://api.mch.weixin.qq.com/v3/pay/transactions/jsapi
            %% TODO[live]: 解析响应 prepay_id；再对客户端调起参数二次签名后返回。
            %% TODO[live]: 回调通知用 ApiKey 做 AES-256-GCM 解密并验签。
            _ = {MchId, AppId, ApiKey, Serial, OrderNo, TotalFen},
            PaymentNo = <<"WECHAT_", OrderNo/binary>>,
            {ok, PaymentNo};
        {error, Reason} ->
            {error, Reason}
    end.

-spec live_refund(binary(), term()) -> ok | {error, binary()}.
live_refund(PaymentNo, Amount) ->
    case credentials() of
        {ok, #{mch_id := MchId, api_v3_key := ApiKey}} ->
            RefundFen = yuan_to_fen(Amount),
            %% TODO[live]: 组装退款请求体
            %%   #{out_trade_no => ..., out_refund_no => ...,
            %%     amount => #{refund => RefundFen, total => ..., currency => <<"CNY">>}}
            %% TODO[live]: APIv3 签名后 HTTP POST
            %%   https://api.mch.weixin.qq.com/v3/refund/domestic/refunds
            %% TODO[live]: 校验响应 status，异常返回 {error, Message}
            _ = {MchId, ApiKey, PaymentNo, RefundFen},
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
    MchId = application:get_env(imboy, wechat_mch_id, <<>>),
    AppId = application:get_env(imboy, wechat_app_id, <<>>),
    ApiKey = application:get_env(imboy, wechat_api_v3_key, <<>>),
    Serial = application:get_env(imboy, wechat_cert_serial, <<>>),
    case
        is_blank(MchId) orelse is_blank(AppId) orelse
            is_blank(ApiKey) orelse is_blank(Serial)
    of
        true ->
            {error, <<"支付网关未配置真实凭据"/utf8>>};
        false ->
            {ok, #{
                mch_id => MchId,
                app_id => AppId,
                api_v3_key => ApiKey,
                cert_serial => Serial
            }}
    end.

-spec is_blank(term()) -> boolean().
is_blank(<<>>) -> true;
is_blank("") -> true;
is_blank(undefined) -> true;
is_blank(_) -> false.

%%%===================================================================
%%% 元 → 分 安全换算（微信金额单位为分；避免浮点精度误差）
%%%===================================================================

-spec yuan_to_fen(term()) -> integer().
yuan_to_fen(V) when is_integer(V) -> V * 100;
yuan_to_fen(V) when is_float(V) -> round(V * 100);
yuan_to_fen(V) when is_binary(V) -> yuan_to_fen_bin(V);
yuan_to_fen(V) when is_list(V) -> yuan_to_fen_bin(list_to_binary(V));
yuan_to_fen(_) -> 0.

-spec yuan_to_fen_bin(binary()) -> integer().
yuan_to_fen_bin(Bin) ->
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
