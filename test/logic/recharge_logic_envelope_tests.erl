-module(recharge_logic_envelope_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc recharge_logic:pay/2 统一支付信封契约测试。
%%%
%%% 验证 S4 缺口修复：钱包充值 pay 把网关返回的支付参数透传为统一信封
%%%   #{<<"payment_method">>, <<"payment_no">>, <<"pay_params">>}（binary key）
%%%   并保留 recharge 业务字段 order_no/amount/status 供前端轮询。
%%%   真实网关路径 status=pending（等第三方回调入账），不触入账事务。
%%%
%%% 手法：meck recharge_order_ds + payment_gateway，绝不触真实 PG。
%%% @end
%%%===================================================================

-define(UID, 3003).

setup() ->
    application:set_env(imboy, env, test),
    meck:new(recharge_order_ds, [no_link, passthrough]),
    meck:new(payment_gateway, [no_link, passthrough]),
    %% 订单：归属当前用户、待支付(status=0)、未过期、金额（分）
    meck:expect(recharge_order_ds, find_by_order_no, fun(OrderNo) ->
        {ok, #{
            <<"order_no">> => OrderNo,
            <<"user_id">> => ?UID,
            <<"status">> => 0,
            <<"payment_method">> => <<"alipay">>,
            <<"amount">> => 1999,
            <<"currency">> => <<"CNY">>,
            <<"expires_at">> => null
        }}
    end),
    ok.

cleanup(_) ->
    meck:unload(payment_gateway),
    meck:unload(recharge_order_ds),
    ok.

recharge_pay_envelope_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun thirdparty_pay_returns_envelope_pending/0,
        fun gateway_error_propagated/0
    ]}.

%% 真实网关：信封含 pay_params + status=pending + 保留 order_no/amount
thirdparty_pay_returns_envelope_pending() ->
    meck:expect(payment_gateway, pay, fun(<<"alipay">>, OrderNo, Opts) ->
        %% recharge 金额单位全程「分」，直接透传，不换算
        ?assertEqual(1999, maps:get(amount, Opts)),
        {ok, <<"ALIPAY_", OrderNo/binary>>, #{<<"order_str">> => <<"orderstr_r">>}}
    end),
    {ok, Result} = recharge_logic:pay(?UID, <<"RCH_R1">>),
    ?assertEqual(<<"alipay">>, maps:get(<<"payment_method">>, Result)),
    ?assertEqual(<<"ALIPAY_RCH_R1">>, maps:get(<<"payment_no">>, Result)),
    ?assertEqual(#{<<"order_str">> => <<"orderstr_r">>}, maps:get(<<"pay_params">>, Result)),
    ?assertEqual(<<"RCH_R1">>, maps:get(<<"order_no">>, Result)),
    ?assertEqual(1999, maps:get(<<"amount">>, Result)),
    %% 真实网关下单阶段不入账，待第三方回调
    ?assertEqual(0, maps:get(<<"status">>, Result)).

%% 网关失败 → 原样透传 error
gateway_error_propagated() ->
    meck:expect(payment_gateway, pay, fun(<<"alipay">>, _OrderNo, _Opts) ->
        {error, <<"支付网关未配置真实凭据"/utf8>>}
    end),
    ?assertEqual(
        {error, <<"支付网关未配置真实凭据"/utf8>>},
        recharge_logic:pay(?UID, <<"RCH_R2">>)
    ).
