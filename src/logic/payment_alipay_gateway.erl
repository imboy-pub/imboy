-module(payment_alipay_gateway).
-compile([nowarn_deprecated_catch]).
-dialyzer({nowarn_function, [err_msg/1]}).
-behaviour(payment_gateway).

%%%===================================================================
%%% @doc 支付宝网关 —— erlang_pay 适配器（App 支付 alipay.trade.app.pay）
%%%
%%% 纯适配层：读 imboy 凭据 → 组 Cfg/Order → 调 erlang_pay → 翻译返回。
%%% 真实签名/HTTP/对账全在 erlang_pay(epay_alipay/epay_crypto/epay_http)；
%%% 本地联调统一用 payment_mock_gateway，本模块不含沙箱模拟。
%%%
%%% 凭据(IMBOY_* 注入)：alipay_app_id / alipay_private_key / alipay_public_key
%%% Amount：最小货币单位（分，recharge_order 存储），直接作 amount_fen 传入。
%%% @end
%%%===================================================================

-export([pay/3, refund/2]).

-spec pay(binary(), term(), map()) ->
    {ok, binary()} | {ok, binary(), map()} | {error, binary()}.
pay(OrderNo, Amount, _Opts) ->
    case cfg() of
        {ok, Cfg} ->
            Order = #{
                out_trade_no => OrderNo,
                amount_fen => to_fen(Amount),
                subject => <<"充值"/utf8>>
            },
            case erlang_pay:create_payment(alipay, Cfg, Order) of
                {ok, #{order_str := OrderStr}} ->
                    {ok, <<"ALIPAY_", OrderNo/binary>>, #{<<"order_str">> => OrderStr}};
                {error, Err} ->
                    {error, err_msg(Err)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec refund(binary(), term()) -> ok | {error, binary()}.
refund(PaymentNo, Amount) ->
    case cfg() of
        {ok, Cfg} ->
            Req = #{
                out_trade_no => strip_prefix(PaymentNo),
                refund_amount_fen => to_fen(Amount),
                out_request_no => <<"R_", PaymentNo/binary>>
            },
            case erlang_pay:refund(alipay, Cfg, Req) of
                {ok, _} -> ok;
                {error, Err} -> {error, err_msg(Err)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% imboy 凭据 → erlang_pay 支付宝 Cfg；任一为空视为未配置
-spec cfg() -> {ok, map()} | {error, binary()}.
cfg() ->
    AppId = application:get_env(imboy, alipay_app_id, <<>>),
    PriKey = application:get_env(imboy, alipay_private_key, <<>>),
    PubKey = application:get_env(imboy, alipay_public_key, <<>>),
    %% 异步回调地址（可选）：erlang_pay 经 maybe_put 处理，空值不下发 notify_url。
    NotifyUrl = application:get_env(imboy, alipay_notify_url, <<>>),
    case AppId =/= <<>> andalso PriKey =/= <<>> andalso PubKey =/= <<>> of
        true ->
            {ok, #{
                app_id => AppId,
                private_key => PriKey,
                public_key => PubKey,
                notify_url => NotifyUrl,
                %% 证书模式应用（本平台即证书模式）下单/退款公共参数必带证书 SN，
                %% 否则收银台报「商家订单参数异常」；公钥模式留空即不下发。
                app_cert_sn => application:get_env(imboy, alipay_app_cert_sn, <<>>),
                alipay_root_cert_sn => application:get_env(imboy, alipay_root_cert_sn, <<>>)
            }};
        false ->
            {error, <<"支付网关未配置真实凭据"/utf8>>}
    end.

-spec strip_prefix(binary()) -> binary().
strip_prefix(<<"ALIPAY_", Rest/binary>>) -> Rest;
strip_prefix(Other) -> Other.

%% Amount 已是分；确保 integer（兼容历史 binary/float），不做 *100 换算
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
