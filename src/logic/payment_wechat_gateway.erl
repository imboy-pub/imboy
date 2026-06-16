-module(payment_wechat_gateway).
-behaviour(payment_gateway).

%%%===================================================================
%%% @doc 微信支付网关 —— erlang_pay 适配器（APIv3 Native 下单）
%%%
%%% 纯适配层：读 imboy 凭据 → 组 Cfg/Order → 调 erlang_pay → 翻译返回。
%%% 真实签名(SHA256withRSA)/HTTP/回调 AES-GCM 解密全在 erlang_pay(epay_wechat)；
%%% 本地联调统一用 payment_mock_gateway，本模块不含沙箱模拟。
%%%
%%% 凭据(IMBOY_* 注入)：
%%%   wechat_mch_id / wechat_app_id / wechat_api_v3_key / wechat_cert_serial
%%%   wechat_private_key(商户 API 证书私钥) / wechat_platform_public_key(平台公钥)
%%%   ⚠️ private_key 与 platform_public_key 为新增凭据项，上线需人工配置。
%%%
%%% Amount：最小货币单位（分），直接作 amount_fen 传入。
%%% @end
%%%===================================================================

-export([pay/3, refund/2]).

-spec pay(binary(), term(), map()) ->
    {ok, binary()} | {ok, binary(), map()} | {error, binary()}.
pay(OrderNo, Amount, _Opts) ->
    case cfg() of
        {ok, Cfg} ->
            Order = #{
                pay_type => native,
                out_trade_no => OrderNo,
                amount_fen => to_fen(Amount),
                description => <<"充值"/utf8>>
            },
            case erlang_pay:create_payment(wechat, Cfg, Order) of
                {ok, #{code_url := CodeUrl}} ->
                    {ok, <<"WECHAT_", OrderNo/binary>>, #{<<"code_url">> => CodeUrl}};
                {ok, #{prepay_id := PrepayId}} ->
                    {ok, <<"WECHAT_", OrderNo/binary>>, #{<<"prepay_id">> => PrepayId}};
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
            Fen = to_fen(Amount),
            Req = #{
                out_trade_no => strip_prefix(PaymentNo),
                out_refund_no => <<"R_", PaymentNo/binary>>,
                refund_fen => Fen,
                total_fen => Fen
            },
            case erlang_pay:refund(wechat, Cfg, Req) of
                {ok, _} -> ok;
                {error, Err} -> {error, err_msg(Err)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% imboy 凭据 → erlang_pay 微信 Cfg；任一必填项为空视为未配置
-spec cfg() -> {ok, map()} | {error, binary()}.
cfg() ->
    MchId = application:get_env(imboy, wechat_mch_id, <<>>),
    AppId = application:get_env(imboy, wechat_app_id, <<>>),
    ApiV3 = application:get_env(imboy, wechat_api_v3_key, <<>>),
    Serial = application:get_env(imboy, wechat_cert_serial, <<>>),
    PriKey = application:get_env(imboy, wechat_private_key, <<>>),
    PlatPub = application:get_env(imboy, wechat_platform_public_key, <<>>),
    %% 异步回调地址（APIv3 Native 下单必填项之一，由 erlang_pay 组进请求体）。
    NotifyUrl = application:get_env(imboy, wechat_notify_url, <<>>),
    case lists:member(<<>>, [MchId, AppId, ApiV3, Serial, PriKey]) of
        true ->
            {error, <<"支付网关未配置真实凭据"/utf8>>};
        false ->
            {ok, #{
                mch_id => MchId,
                app_id => AppId,
                api_v3 => ApiV3,
                mch_serial_no => Serial,
                private_key => PriKey,
                platform_public_key => PlatPub,
                notify_url => NotifyUrl
            }}
    end.

-spec strip_prefix(binary()) -> binary().
strip_prefix(<<"WECHAT_", Rest/binary>>) -> Rest;
strip_prefix(Other) -> Other.

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

-spec err_msg(term()) -> binary().
err_msg({_Code, M}) when is_binary(M) -> M;
err_msg(M) when is_binary(M) -> M;
err_msg(O) -> elib_cnv:safe_to_binary(O).
