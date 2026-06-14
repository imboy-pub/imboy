-module(payment_stripe_gateway_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc payment_stripe_gateway live 模式经 erlang_pay 接入的契约测试。
%%%
%%% 目标：验证 imboy 网关骨架正确委托 erlang_pay，并把库返回适配为
%%%   imboy 契约（client_secret 透传、错误归一为 binary）。
%%% 手法：meck erlang_pay（绝不发真实请求），set payment_mode=live +
%%%   stripe_secret_key。每用例 try...after 卸载 mock 与还原 env。
%%% @end
%%%===================================================================

setup() ->
    application:set_env(imboy, payment_mode, live),
    application:set_env(imboy, stripe_secret_key, <<"sk_test_x">>),
    application:set_env(imboy, stripe_webhook_secret, <<"whsec_x">>),
    meck:new(erlang_pay, [non_strict]),
    ok.

cleanup(_) ->
    meck:unload(erlang_pay),
    application:set_env(imboy, payment_mode, sandbox),
    application:set_env(imboy, stripe_secret_key, <<>>),
    application:set_env(imboy, stripe_webhook_secret, <<>>),
    ok.

stripe_gateway_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun pay_carries_client_secret/0,
        fun pay_amount_is_minor_unit/0,
        fun pay_maps_gateway_error_to_binary/0,
        fun refund_ok/0,
        fun refund_maps_error/0,
        fun missing_credential_rejected/0
    ]}.

%% 下单成功：client_secret 经 3 元组 Extra 透传
pay_carries_client_secret() ->
    meck:expect(erlang_pay, create_payment, fun(stripe, _Cfg, _Order) ->
        {ok, #{
            type => stripe_payment_intent,
            payment_no => <<"pi_123">>,
            client_secret => <<"pi_123_secret_abc">>
        }}
    end),
    Result = payment_stripe_gateway:pay(<<"order_1">>, 1999, #{currency => <<"USD">>}),
    ?assertEqual({ok, <<"pi_123">>, #{<<"client_secret">> => <<"pi_123_secret_abc">>}}, Result).

%% 金额已是最小货币单位（分/cents），原样作 amount_fen，不再 *100
pay_amount_is_minor_unit() ->
    meck:expect(erlang_pay, create_payment, fun(stripe, _Cfg, Order) ->
        %% 断言传入库的金额未被放大
        ?assertEqual(1999, maps:get(amount_fen, Order)),
        ?assertEqual(<<"cny">>, maps:get(currency, Order)),
        {ok, #{payment_no => <<"pi_1">>, client_secret => <<"s">>}}
    end),
    _ = payment_stripe_gateway:pay(<<"order_2">>, 1999, #{}).

%% 库业务错误 {Code, Msg} → 展示用 binary
pay_maps_gateway_error_to_binary() ->
    meck:expect(erlang_pay, create_payment, fun(stripe, _Cfg, _Order) ->
        {error, {gateway_error, <<"card declined"/utf8>>}}
    end),
    ?assertEqual(
        {error, <<"card declined"/utf8>>},
        payment_stripe_gateway:pay(<<"order_3">>, 100, #{})
    ).

refund_ok() ->
    meck:expect(erlang_pay, refund, fun(stripe, _Cfg, _Req) ->
        {ok, #{type => stripe_refund}}
    end),
    ?assertEqual(ok, payment_stripe_gateway:refund(<<"pi_123">>, 1999)).

refund_maps_error() ->
    meck:expect(erlang_pay, refund, fun(stripe, _Cfg, _Req) ->
        {error, {http_error, <<"timeout"/utf8>>}}
    end),
    ?assertEqual(
        {error, <<"timeout"/utf8>>},
        payment_stripe_gateway:refund(<<"pi_x">>, 100)
    ).

%% 缺真实凭据（live 模式）→ 拒绝，不调用库
missing_credential_rejected() ->
    application:set_env(imboy, stripe_secret_key, <<>>),
    ?assertMatch({error, _}, payment_stripe_gateway:pay(<<"order_4">>, 100, #{})).
