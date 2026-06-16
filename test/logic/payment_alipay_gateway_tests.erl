-module(payment_alipay_gateway_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc payment_alipay_gateway 经 erlang_pay 接入的契约测试。
%%%
%%% 目标：验证 imboy 网关适配器
%%%   ① cfg/0 把 alipay_notify_url 读入 Cfg（key 为 atom notify_url，
%%%      erlang_pay 期望值）；
%%%   ② pay/3 把 erlang_pay 返回的 order_str 经 3 元组 Extra 透传
%%%      为统一信封 pay_params 的 #{<<"order_str">>}。
%%% 手法：meck erlang_pay（绝不发真实请求），set 凭据 + notify_url。
%%%      每用例 try...after 卸载 mock 与还原 env。
%%% @end
%%%===================================================================

setup() ->
    application:set_env(imboy, alipay_app_id, <<"2021000000000000">>),
    application:set_env(imboy, alipay_private_key, <<"test-priv-stub">>),
    application:set_env(imboy, alipay_public_key, <<"test-pub-stub">>),
    application:set_env(
        imboy, alipay_notify_url, <<"https://im.example.com/v1/pay/alipay/notify">>
    ),
    meck:new(erlang_pay, [non_strict]),
    ok.

cleanup(_) ->
    meck:unload(erlang_pay),
    application:set_env(imboy, alipay_app_id, <<>>),
    application:set_env(imboy, alipay_private_key, <<>>),
    application:set_env(imboy, alipay_public_key, <<>>),
    application:set_env(imboy, alipay_notify_url, <<>>),
    ok.

alipay_gateway_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun cfg_carries_notify_url/0,
        fun pay_carries_order_str/0,
        fun pay_amount_is_minor_unit/0,
        fun pay_maps_gateway_error_to_binary/0,
        fun missing_credential_rejected/0,
        fun empty_notify_url_still_passed/0
    ]}.

%% cfg/0 把 notify_url 读入 Cfg（atom key，erlang_pay maybe_put 消费）
cfg_carries_notify_url() ->
    meck:expect(erlang_pay, create_payment, fun(alipay, Cfg, _Order) ->
        ?assertEqual(
            <<"https://im.example.com/v1/pay/alipay/notify">>,
            maps:get(notify_url, Cfg)
        ),
        {ok, #{order_str => <<"app_id=2021&..."/utf8>>}}
    end),
    _ = payment_alipay_gateway:pay(<<"ORD_A1">>, 100, #{}).

%% 下单成功：order_str 经 3 元组 Extra 透传为信封 pay_params 字段
pay_carries_order_str() ->
    meck:expect(erlang_pay, create_payment, fun(alipay, _Cfg, _Order) ->
        {ok, #{order_str => <<"orderstr_abc">>}}
    end),
    Result = payment_alipay_gateway:pay(<<"ORD_A2">>, 1999, #{}),
    ?assertEqual(
        {ok, <<"ALIPAY_ORD_A2">>, #{<<"order_str">> => <<"orderstr_abc">>}},
        Result
    ).

%% 金额已是最小货币单位（分），原样作 amount_fen，不再 *100
pay_amount_is_minor_unit() ->
    meck:expect(erlang_pay, create_payment, fun(alipay, _Cfg, Order) ->
        ?assertEqual(1999, maps:get(amount_fen, Order)),
        {ok, #{order_str => <<"s">>}}
    end),
    _ = payment_alipay_gateway:pay(<<"ORD_A3">>, 1999, #{}).

%% 库业务错误 {Code, Msg} → 展示用 binary
pay_maps_gateway_error_to_binary() ->
    meck:expect(erlang_pay, create_payment, fun(alipay, _Cfg, _Order) ->
        {error, {gateway_error, <<"参数错误"/utf8>>}}
    end),
    ?assertEqual(
        {error, <<"参数错误"/utf8>>},
        payment_alipay_gateway:pay(<<"ORD_A4">>, 100, #{})
    ).

%% 缺真实凭据 → 拒绝，不调用库
missing_credential_rejected() ->
    application:set_env(imboy, alipay_app_id, <<>>),
    ?assertMatch({error, _}, payment_alipay_gateway:pay(<<"ORD_A5">>, 100, #{})).

%% notify_url 缺省（空）仍以空 binary 放入 Cfg（erlang_pay 经 maybe_put 不下发）
empty_notify_url_still_passed() ->
    application:set_env(imboy, alipay_notify_url, <<>>),
    meck:expect(erlang_pay, create_payment, fun(alipay, Cfg, _Order) ->
        ?assertEqual(<<>>, maps:get(notify_url, Cfg)),
        {ok, #{order_str => <<"s">>}}
    end),
    _ = payment_alipay_gateway:pay(<<"ORD_A6">>, 100, #{}).
