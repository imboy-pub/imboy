%%% @doc 管理端代发退款 channel_logic_order:admin_refund_order/2 的 EUnit 测试。
%%% 全程 meck mock，不触碰真实 DB / 支付网关。
-module(channel_logic_order_admin_refund_tests).

-include_lib("eunit/include/eunit.hrl").

%% 构造一个指定状态的订单 map（字段对齐 channel_order 表）
order(Status) ->
    #{
        <<"user_id">> => 52278,
        <<"channel_id">> => 1001,
        <<"status">> => Status,
        <<"payment_method">> => <<"wallet">>,
        <<"payment_no">> => <<"PAY-1">>,
        <<"amount">> => 9
    }.

setup() ->
    application:set_env(imboy, env, test),
    meck:new(channel_order_ds, [no_link, passthrough]),
    meck:new(payment_gateway, [no_link, passthrough]),
    meck:new(channel_ds, [no_link, passthrough]),
    meck:expect(channel_ds, unsubscribe, fun(_ChannelId, _Uid) -> ok end),
    ok.

cleanup(_) ->
    meck:unload(channel_order_ds),
    meck:unload(payment_gateway),
    meck:unload(channel_ds),
    ok.

admin_refund_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun t_admin_refund_ok/0,
        fun t_admin_refund_idempotent/0,
        fun t_admin_refund_bad_status/0,
        fun t_admin_refund_not_found/0,
        fun t_admin_refund_gateway_fail/0
    ]}.

%% 已支付(status=1) → 退款成功，网关与 ds:refund 各调用一次
t_admin_refund_ok() ->
    meck:expect(channel_order_ds, find_by_order_no, fun(_) -> {ok, order(1)} end),
    meck:expect(payment_gateway, refund, fun(_M, _P, _A) -> ok end),
    meck:expect(channel_order_ds, refund, fun(_O, _U, _R) -> ok end),
    ?assertEqual(ok, channel_logic_order:admin_refund_order(<<"ORD-1">>, <<"申诉退款"/utf8>>)),
    ?assertEqual(1, meck:num_calls(payment_gateway, refund, '_')),
    ?assertEqual(1, meck:num_calls(channel_order_ds, refund, '_')).

%% 已退款(status=2) → 幂等，返回提示且不调用网关
t_admin_refund_idempotent() ->
    meck:expect(channel_order_ds, find_by_order_no, fun(_) -> {ok, order(2)} end),
    meck:expect(payment_gateway, refund, fun(_M, _P, _A) -> ok end),
    ?assertEqual(
        {error, <<"订单已退款"/utf8>>},
        channel_logic_order:admin_refund_order(<<"ORD-1">>, <<"x"/utf8>>)
    ),
    ?assertEqual(0, meck:num_calls(payment_gateway, refund, '_')).

%% 非可退状态(status=3 已取消) → 拒绝
t_admin_refund_bad_status() ->
    meck:expect(channel_order_ds, find_by_order_no, fun(_) -> {ok, order(3)} end),
    ?assertEqual(
        {error, <<"订单状态不允许退款"/utf8>>},
        channel_logic_order:admin_refund_order(<<"ORD-1">>, <<"x"/utf8>>)
    ).

%% 订单不存在 → 拒绝
t_admin_refund_not_found() ->
    meck:expect(channel_order_ds, find_by_order_no, fun(_) -> {error, not_found} end),
    ?assertEqual(
        {error, <<"订单不存在"/utf8>>},
        channel_logic_order:admin_refund_order(<<"ORD-X">>, <<"x"/utf8>>)
    ).

%% 网关退款失败 → 不更新订单状态（ds:refund 不被调用）
t_admin_refund_gateway_fail() ->
    meck:expect(channel_order_ds, find_by_order_no, fun(_) -> {ok, order(1)} end),
    meck:expect(payment_gateway, refund, fun(_M, _P, _A) -> {error, <<"网关失败"/utf8>>} end),
    meck:expect(channel_order_ds, refund, fun(_O, _U, _R) -> ok end),
    ?assertMatch({error, _}, channel_logic_order:admin_refund_order(<<"ORD-1">>, <<"x"/utf8>>)),
    ?assertEqual(0, meck:num_calls(channel_order_ds, refund, '_')).
