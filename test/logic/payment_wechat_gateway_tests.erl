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
        fun pay_carries_code_url/0,
        fun pay_carries_prepay_id/0,
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

%% JSAPI 下单：prepay_id 经 3 元组 Extra 透传（二次签名为遗留 TODO，库直返 prepay_id）
pay_carries_prepay_id() ->
    meck:expect(erlang_pay, create_payment, fun(wechat, _Cfg, _Order) ->
        {ok, #{prepay_id => <<"wx20210000">>}}
    end),
    Result = payment_wechat_gateway:pay(<<"ORD_W3">>, 1999, #{}),
    ?assertEqual(
        {ok, <<"WECHAT_ORD_W3">>, #{<<"prepay_id">> => <<"wx20210000">>}},
        Result
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
