-module(recharge_logic_confirm_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc recharge_logic:confirm/2 主动查单确认测试。
%%%
%%% 场景：客户端支付回跳后轮询订单，但异步回调（notify）丢失/不可达时
%%% 订单永远 pending —— confirm 由服务端主动向网关查单，TRADE_SUCCESS
%%% 则幂等入账。对应路由 POST /v1/wallet/recharge/confirm。
%%%
%%% 手法：meck recharge_order_ds / wallet_ds / payment_gateway，
%%% 绝不触真实 PG 与真实支付宝。
%%% @end
%%%===================================================================

-define(UID, 5101).
-define(OTHER, 5102).
-define(ORDER_NO, <<"RCH_C1">>).

setup() ->
    application:set_env(imboy, env, test),
    meck:new(recharge_order_ds, [no_link, passthrough]),
    meck:new(wallet_ds, [no_link, passthrough]),
    meck:new(payment_gateway, [no_link, passthrough]),
    ok = meck:expect(wallet_ds, ensure_wallet, fun(_) -> #{<<"id">> => 9001} end).

cleanup(_) ->
    meck:unload(recharge_order_ds),
    meck:unload(wallet_ds),
    meck:unload(payment_gateway),
    ok.

recharge_confirm_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun confirm_non_owner_rejected/0,
        fun confirm_already_paid_idempotent/0,
        fun confirm_gateway_success_credits/0,
        fun confirm_gateway_pending_keeps_pending/0,
        fun confirm_gateway_not_exist_keeps_pending/0,
        fun confirm_gateway_unsupported_keeps_pending/0
    ]}.

order(Uid, Status) ->
    #{
        <<"order_no">> => ?ORDER_NO,
        <<"user_id">> => Uid,
        <<"status">> => Status,
        <<"payment_method">> => <<"alipay">>,
        <<"amount">> => 100,
        <<"currency">> => <<"CNY">>
    }.

%% 非本人订单 → 拒绝（IDOR 防护），且不触网关
confirm_non_owner_rejected() ->
    meck:expect(recharge_order_ds, find_by_order_no, fun(?ORDER_NO) ->
        {ok, order(?OTHER, 0)}
    end),
    ?assertEqual(
        {error, <<"无权操作此订单"/utf8>>},
        recharge_logic:confirm(?UID, ?ORDER_NO)
    ),
    ?assertEqual(0, meck:num_calls(payment_gateway, query_order, 2)).

%% 订单已支付 → 幂等返回已付，不再查网关、不重复入账
confirm_already_paid_idempotent() ->
    meck:expect(recharge_order_ds, find_by_order_no, fun(?ORDER_NO) ->
        {ok, order(?UID, 1)}
    end),
    {ok, Result} = recharge_logic:confirm(?UID, ?ORDER_NO),
    ?assertEqual(1, maps:get(<<"status">>, Result)),
    ?assertEqual(0, meck:num_calls(payment_gateway, query_order, 2)),
    ?assertEqual(0, meck:num_calls(recharge_order_ds, credit_in_tx, 4)).

%% 网关查单 TRADE_SUCCESS → 幂等入账并返回新余额
confirm_gateway_success_credits() ->
    meck:expect(recharge_order_ds, find_by_order_no, fun(?ORDER_NO) ->
        {ok, order(?UID, 0)}
    end),
    meck:expect(payment_gateway, query_order, fun(<<"alipay">>, ?ORDER_NO) ->
        {ok, #{trade_state => success, trade_no => <<"2026082122001">>}}
    end),
    meck:expect(
        recharge_order_ds,
        credit_in_tx,
        fun(?ORDER_NO, <<"2026082122001">>, ?UID, 100) -> {ok, 100} end
    ),
    {ok, Result} = recharge_logic:confirm(?UID, ?ORDER_NO),
    ?assertEqual(1, maps:get(<<"status">>, Result)),
    ?assertEqual(100, maps:get(<<"balance">>, Result)).

%% 网关查单未付款（WAIT_BUYER_PAY）→ 保持 pending，不入账
confirm_gateway_pending_keeps_pending() ->
    meck:expect(recharge_order_ds, find_by_order_no, fun(?ORDER_NO) ->
        {ok, order(?UID, 0)}
    end),
    meck:expect(payment_gateway, query_order, fun(<<"alipay">>, ?ORDER_NO) ->
        {ok, #{trade_state => pending, trade_no => <<>>}}
    end),
    {ok, Result} = recharge_logic:confirm(?UID, ?ORDER_NO),
    ?assertEqual(0, maps:get(<<"status">>, Result)),
    ?assertEqual(0, meck:num_calls(recharge_order_ds, credit_in_tx, 4)).

%% 网关查单交易不存在（买家从未付款）→ 保持 pending
confirm_gateway_not_exist_keeps_pending() ->
    meck:expect(recharge_order_ds, find_by_order_no, fun(?ORDER_NO) ->
        {ok, order(?UID, 0)}
    end),
    meck:expect(payment_gateway, query_order, fun(<<"alipay">>, ?ORDER_NO) ->
        {ok, #{trade_state => not_exist, trade_no => <<>>}}
    end),
    {ok, Result} = recharge_logic:confirm(?UID, ?ORDER_NO),
    ?assertEqual(0, maps:get(<<"status">>, Result)).

%% 网关不支持查单（mock 等即时入账网关）→ 如实返回当前 pending
confirm_gateway_unsupported_keeps_pending() ->
    meck:expect(recharge_order_ds, find_by_order_no, fun(?ORDER_NO) ->
        {ok, order(?UID, 0)}
    end),
    meck:expect(payment_gateway, query_order, fun(<<"alipay">>, ?ORDER_NO) ->
        unsupported
    end),
    {ok, Result} = recharge_logic:confirm(?UID, ?ORDER_NO),
    ?assertEqual(0, maps:get(<<"status">>, Result)).
