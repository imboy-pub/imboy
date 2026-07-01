-module(recharge_logic_pay_access_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc recharge_logic:pay/2 访问控制边界测试。
%%%
%%% 对应路由 POST /v1/wallet/recharge/pay（P0 支付）。
%%% happy path（owner + pending + 网关成功信封）已由
%%%   recharge_logic_envelope_tests 覆盖；本文件补两个非法输入：
%%%   ① 订单不属于当前用户 → IDOR 拒绝；② 订单非待支付 → 拒绝。
%%%
%%% 手法：meck recharge_order_ds，绝不触真实 PG（拒绝路径不触网关）。
%%% @end
%%%===================================================================

-define(UID, 4201).
-define(OTHER, 4202).

setup() ->
    application:set_env(imboy, env, test),
    meck:new(recharge_order_ds, [no_link, passthrough]),
    ok.

cleanup(_) ->
    meck:unload(recharge_order_ds),
    ok.

recharge_pay_access_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun pay_non_owner_rejected/0,
        fun pay_not_pending_rejected/0
    ]}.

order(Uid, Status) ->
    #{
        <<"order_no">> => <<"RCH_P1">>,
        <<"user_id">> => Uid,
        <<"status">> => Status,
        <<"payment_method">> => <<"alipay">>,
        <<"amount">> => 1999,
        <<"currency">> => <<"CNY">>
    }.

%% 非法输入：订单不属于当前用户 → 拒绝（IDOR 防护）
pay_non_owner_rejected() ->
    meck:expect(recharge_order_ds, find_by_order_no, fun(<<"RCH_P1">>) ->
        {ok, order(?OTHER, 0)}
    end),
    ?assertEqual(
        {error, <<"无权操作此订单"/utf8>>},
        recharge_logic:pay(?UID, <<"RCH_P1">>)
    ).

%% 非法输入：订单非待支付（已支付 status=1）→ 拒绝
pay_not_pending_rejected() ->
    meck:expect(recharge_order_ds, find_by_order_no, fun(<<"RCH_P1">>) ->
        {ok, order(?UID, 1)}
    end),
    ?assertEqual(
        {error, <<"订单状态不允许支付"/utf8>>},
        recharge_logic:pay(?UID, <<"RCH_P1">>)
    ).
