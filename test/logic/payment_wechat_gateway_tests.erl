-module(payment_wechat_gateway_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc payment_wechat_gateway 经 erlang_pay 接入的契约测试。
%%%
%%% 目标：验证 imboy 网关适配器
%%%   ① cfg/0 把 wechat_notify_url 读入 Cfg（key 为 atom notify_url）；
%%%   ② pay/3 把 erlang_pay 返回的 code_url / prepay_id 经 3 元组 Extra
%%%      透传为统一信封 pay_params。
%%% 手法：meck erlang_pay（绝不发真实请求），set 六项凭据 + notify_url。
%%% @end
%%%===================================================================

setup() ->
    application:set_env(imboy, wechat_mch_id, <<"1900000000">>),
    application:set_env(imboy, wechat_app_id, <<"wxappid">>),
    application:set_env(imboy, wechat_api_v3_key, <<"testtesttesttesttesttesttesttest">>),
    application:set_env(imboy, wechat_cert_serial, <<"SERIAL123">>),
    application:set_env(imboy, wechat_private_key, <<"test-priv-stub">>),
    application:set_env(imboy, wechat_platform_public_key, <<"test-pub-stub">>),
    application:set_env(
        imboy, wechat_notify_url, <<"https://im.example.com/v1/pay/wechat/notify">>
    ),
    meck:new(erlang_pay, [non_strict]),
    ok.

cleanup(_) ->
    meck:unload(erlang_pay),
    application:set_env(imboy, wechat_mch_id, <<>>),
    application:set_env(imboy, wechat_app_id, <<>>),
    application:set_env(imboy, wechat_api_v3_key, <<>>),
    application:set_env(imboy, wechat_cert_serial, <<>>),
    application:set_env(imboy, wechat_private_key, <<>>),
    application:set_env(imboy, wechat_platform_public_key, <<>>),
    application:set_env(imboy, wechat_notify_url, <<>>),
    ok.

wechat_gateway_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun cfg_carries_notify_url/0,
        fun cfg_carries_api_v3_key/0,
        fun pay_carries_code_url/0,
        fun pay_jsapi_carries_full_pay_sign/0,
        fun pay_jsapi_sign_failure_is_fail_closed/0,
        fun pay_amount_is_minor_unit/0,
        fun pay_maps_gateway_error_to_binary/0,
        fun missing_credential_rejected/0
    ]}.

%% cfg/0 把 notify_url 读入 Cfg（atom key，erlang_pay 组进 APIv3 请求体）
cfg_carries_notify_url() ->
    meck:expect(erlang_pay, create_payment, fun(wechat, Cfg, _Order) ->
        ?assertEqual(
            <<"https://im.example.com/v1/pay/wechat/notify">>,
            maps:get(notify_url, Cfg)
        ),
        {ok, #{code_url => <<"weixin://wxpay/bizpayurl?pr=abc">>}}
    end),
    _ = payment_wechat_gateway:pay(<<"ORD_W1">>, 100, #{}).

%% Native 下单：code_url 经 3 元组 Extra 透传为信封 pay_params
pay_carries_code_url() ->
    meck:expect(erlang_pay, create_payment, fun(wechat, _Cfg, _Order) ->
        {ok, #{code_url => <<"weixin://wxpay/bizpayurl?pr=xyz">>}}
    end),
    Result = payment_wechat_gateway:pay(<<"ORD_W2">>, 1999, #{}),
    ?assertEqual(
        {ok, <<"WECHAT_ORD_W2">>, #{<<"code_url">> => <<"weixin://wxpay/bizpayurl?pr=xyz">>}},
        Result
    ).

%% cfg/0 把 api_v3_key 读入 Cfg（atom key api_v3_key，与 erlang_pay epay_wechat 契约一致）
cfg_carries_api_v3_key() ->
    meck:expect(erlang_pay, create_payment, fun(wechat, Cfg, _Order) ->
        ?assertEqual(
            <<"testtesttesttesttesttesttesttest">>,
            maps:get(api_v3_key, Cfg)
        ),
        {ok, #{code_url => <<"u">>}}
    end),
    _ = payment_wechat_gateway:pay(<<"ORD_W7">>, 100, #{}).

%% JSAPI 下单：拿到 prepay_id 后调 build_pay_sign 生成完整二次签名，
%% 组装成前端 payment_launcher 期望的 snake_case pay_params。
pay_jsapi_carries_full_pay_sign() ->
    meck:expect(erlang_pay, create_payment, fun(wechat, _Cfg, _Order) ->
        {ok, #{prepay_id => <<"wx20210000">>}}
    end),
    %% erlang_pay build_pay_sign 返回 v3 camelCase 标准字段
    meck:expect(erlang_pay, build_pay_sign, fun(wechat, Cfg, Args) ->
        %% 适配器必须把 prepay_id 传入 build_pay_sign
        ?assertEqual(<<"wx20210000">>, maps:get(prepay_id, Args)),
        %% Cfg 仍是含商户私钥等的下单同款 Cfg
        ?assertEqual(<<"wxappid">>, maps:get(app_id, Cfg)),
        {ok, #{
            <<"appId">> => <<"wxappid">>,
            <<"timeStamp">> => <<"1700000000">>,
            <<"nonceStr">> => <<"abc123nonce">>,
            <<"package">> => <<"prepay_id=wx20210000">>,
            <<"signType">> => <<"RSA">>,
            <<"paySign">> => <<"BASE64SIGN==">>
        }}
    end),
    {ok, PayNo, Params} = payment_wechat_gateway:pay(<<"ORD_W3">>, 1999, #{}),
    ?assertEqual(<<"WECHAT_ORD_W3">>, PayNo),
    %% 前端 payment_launcher.parseWechatParams 读取的 snake_case key 全部齐备
    ?assertEqual(<<"wxappid">>, maps:get(<<"appid">>, Params)),
    %% partnerid = 商户号 mch_id（build_pay_sign 返回不含，由适配器补）
    ?assertEqual(<<"1900000000">>, maps:get(<<"partnerid">>, Params)),
    %% 裸 prepay_id（前端读 prepay_id/prepayid）
    ?assertEqual(<<"wx20210000">>, maps:get(<<"prepay_id">>, Params)),
    %% package = prepay_id=xxx（JSAPI 唤起约定，非默认 Sign=WXPay）
    ?assertEqual(<<"prepay_id=wx20210000">>, maps:get(<<"package">>, Params)),
    ?assertEqual(<<"abc123nonce">>, maps:get(<<"noncestr">>, Params)),
    ?assertEqual(<<"1700000000">>, maps:get(<<"timestamp">>, Params)),
    %% sign = paySign（前端读 sign）
    ?assertEqual(<<"BASE64SIGN==">>, maps:get(<<"sign">>, Params)),
    ?assertEqual(<<"RSA">>, maps:get(<<"signtype">>, Params)).

%% build_pay_sign 失败（能力不可用/凭据缺失/签名失败）→ fail-closed，
%% 整体返回 {error, _}，绝不输出半成品签名 pay_params。
pay_jsapi_sign_failure_is_fail_closed() ->
    meck:expect(erlang_pay, create_payment, fun(wechat, _Cfg, _Order) ->
        {ok, #{prepay_id => <<"wx20210000">>}}
    end),
    meck:expect(erlang_pay, build_pay_sign, fun(wechat, _Cfg, _Args) ->
        {error, {sign_failed, <<"微信 paySign 签名失败"/utf8>>}}
    end),
    ?assertEqual(
        {error, <<"微信 paySign 签名失败"/utf8>>},
        payment_wechat_gateway:pay(<<"ORD_W8">>, 1999, #{})
    ).

%% 金额已是最小货币单位（分），原样作 amount_fen
pay_amount_is_minor_unit() ->
    meck:expect(erlang_pay, create_payment, fun(wechat, _Cfg, Order) ->
        ?assertEqual(1999, maps:get(amount_fen, Order)),
        {ok, #{code_url => <<"u">>}}
    end),
    _ = payment_wechat_gateway:pay(<<"ORD_W4">>, 1999, #{}).

%% 库业务错误 → 展示用 binary
pay_maps_gateway_error_to_binary() ->
    meck:expect(erlang_pay, create_payment, fun(wechat, _Cfg, _Order) ->
        {error, {gateway_error, <<"商户号不存在"/utf8>>}}
    end),
    ?assertEqual(
        {error, <<"商户号不存在"/utf8>>},
        payment_wechat_gateway:pay(<<"ORD_W5">>, 100, #{})
    ).

%% 缺真实凭据 → 拒绝，不调用库
missing_credential_rejected() ->
    application:set_env(imboy, wechat_mch_id, <<>>),
    ?assertMatch({error, _}, payment_wechat_gateway:pay(<<"ORD_W6">>, 100, #{})).
