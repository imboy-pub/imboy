-module(channel_logic_order_refund_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc channel_logic_order 退款状态机 B-09 契约测试。
%%%
%%% 目标：验证退款流程已升级为 CAS 模式：
%%%   paid(1) → mark_refunding(5) → 网关退款 → finalize_refund(2)
%%%   并发退款最多一次有效；网关失败可重试；payment_transaction 同步。
%%%
%%% 手法：meck channel_order_ds / payment_gateway / payment_transaction_ds，
%%%   不触真实 DB/网关。
%%% @end
%%%===================================================================

-define(UID, 2002).
-define(CID, 11).
-define(ORDER_NO, <<"ORD_REF_1">>).

setup() ->
    meck:new(payment_gateway, [no_link, passthrough]),
    meck:new(channel_order_ds, [no_link, passthrough]),
    meck:new(payment_transaction_ds, [no_link, passthrough]),
    meck:new(channel_ds, [no_link, passthrough]),
    %% 默认：mark_refunding 成功（CAS 抢占成功）
    meck:expect(channel_order_ds, mark_refunding, fun(_OrderNo) -> {ok, 1} end),
    meck:expect(channel_order_ds, finalize_refund, fun(_, _, _) -> ok end),
    meck:expect(channel_order_ds, release_refunding, fun(_) -> {ok, 1} end),
    meck:expect(channel_order_ds, find_by_order_no, fun(OrderNo) ->
        {ok, #{
            <<"order_no">> => OrderNo,
            <<"channel_id">> => ?CID,
            <<"user_id">> => ?UID,
            <<"amount">> => 9.90,
            <<"status">> => 1,
            <<"payment_method">> => <<"wallet">>,
            <<"payment_no">> => <<"WPY_ORD_REF">>
        }}
    end),
    meck:expect(payment_gateway, refund, fun(<<"wallet">>, _No, _Amt) -> ok end),
    %% payment_transaction_ds: 默认找不到对应流水 → 静默跳过
    meck:expect(payment_transaction_ds, find_by_biz_order_no, fun(_, _) -> #{} end),
    meck:expect(payment_transaction_ds, mark_refunding, fun(_) -> {ok, 1} end),
    meck:expect(payment_transaction_ds, release_refunding, fun(_) -> {ok, 1} end),
    meck:expect(payment_transaction_ds, mark_refunded, fun(_) -> {ok, 1} end),
    meck:expect(channel_ds, unsubscribe, fun(_, _) -> ok end),
    ok.

cleanup(_) ->
    catch meck:unload(payment_gateway),
    catch meck:unload(channel_order_ds),
    catch meck:unload(payment_transaction_ds),
    catch meck:unload(channel_ds),
    ok.

refund_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun concurrent_refund_only_one_succeeds/0,
        fun gateway_failure_releases_refunding/0,
        fun gateway_failure_allows_retry/0,
        fun refund_notifies_payment_transaction/0,
        fun refund_already_refunded_returns_error/0,
        fun refund_already_refunding_returns_error/0,
        fun admin_refund_uses_same_primitive/0
    ]}.

%% B-09 核心：并发退款，CAS 抢占只让一个通过
concurrent_refund_only_one_succeeds() ->
    %% 第一个请求：mark_refunding 返回 {ok, 1}（抢占成功）
    meck:expect(channel_order_ds, mark_refunding, fun(_OrderNo) -> {ok, 1} end),
    Result1 = channel_logic_order:refund_order(?UID, ?ORDER_NO),
    ?assertEqual(ok, Result1),
    %% 第二个请求：mark_refunding 返回 {ok, 0}（已被抢占）
    meck:expect(channel_order_ds, mark_refunding, fun(_OrderNo) -> {ok, 0} end),
    Result2 = channel_logic_order:refund_order(?UID, ?ORDER_NO),
    ?assertMatch({error, _}, Result2),
    %% 第一个请求调了 gateway:refund，第二个没调
    ?assertEqual(1, meck:num_calls(payment_gateway, refund, '_')).

%% 网关失败 → 释放占位(5→1)，下次可重试
gateway_failure_releases_refunding() ->
    meck:expect(payment_gateway, refund, fun(<<"wallet">>, _No, _Amt) ->
        {error, <<"余额不足"/utf8>>}
    end),
    Result = channel_logic_order:refund_order(?UID, ?ORDER_NO),
    ?assertMatch({error, _}, Result),
    %% 释放占位被调用
    ?assert(meck:called(channel_order_ds, release_refunding, [?ORDER_NO])).

%% 网关失败释放后，重试可以再次抢占
gateway_failure_allows_retry() ->
    %% 第一次：网关失败
    meck:expect(payment_gateway, refund, fun(<<"wallet">>, _No, _Amt) ->
        {error, <<"余额不足"/utf8>>}
    end),
    _ = channel_logic_order:refund_order(?UID, ?ORDER_NO),
    %% 第二次：mark_refunding 再次抢占成功（因为释放了）
    meck:expect(payment_gateway, refund, fun(<<"wallet">>, _No, _Amt) -> ok end),
    Result2 = channel_logic_order:refund_order(?UID, ?ORDER_NO),
    ?assertEqual(ok, Result2),
    %% 两次网关调用，mark_refunding 被调了两次
    ?assertEqual(2, meck:num_calls(payment_gateway, refund, '_')).

%% 退款成功后 payment_transaction 被标记为已退款
refund_notifies_payment_transaction() ->
    %% 模拟存在支付流水
    meck:expect(payment_transaction_ds, find_by_biz_order_no, fun(2, ?ORDER_NO) ->
        #{<<"trade_no">> => <<"TX_REF_1">>, <<"status">> => 1}
    end),
    Result = channel_logic_order:refund_order(?UID, ?ORDER_NO),
    ?assertEqual(ok, Result),
    %% mark_refunding 和 mark_refunded 都被调用了
    ?assert(meck:called(payment_transaction_ds, mark_refunding, [<<"TX_REF_1">>])),
    ?assert(meck:called(payment_transaction_ds, mark_refunded, [<<"TX_REF_1">>])).

%% 已退款订单拒绝重复退款
refund_already_refunded_returns_error() ->
    meck:expect(channel_order_ds, find_by_order_no, fun(OrderNo) ->
        {ok, #{
            <<"order_no">> => OrderNo,
            <<"channel_id">> => ?CID,
            <<"user_id">> => ?UID,
            <<"amount">> => 9.90,
            <<"status">> => 2,
            <<"payment_method">> => <<"wallet">>,
            <<"payment_no">> => <<"WPY_ORD_REF">>
        }}
    end),
    ?assertMatch(
        {error, <<"订单已退款"/utf8>>},
        channel_logic_order:refund_order(?UID, ?ORDER_NO)
    ).

%% 退款中订单拒绝重复退款
refund_already_refunding_returns_error() ->
    meck:expect(channel_order_ds, find_by_order_no, fun(OrderNo) ->
        {ok, #{
            <<"order_no">> => OrderNo,
            <<"channel_id">> => ?CID,
            <<"user_id">> => ?UID,
            <<"amount">> => 9.90,
            <<"status">> => 5,
            <<"payment_method">> => <<"wallet">>,
            <<"payment_no">> => <<"WPY_ORD_REF">>
        }}
    end),
    ?assertMatch(
        {error, <<"订单退款中，请稍后重试"/utf8>>},
        channel_logic_order:refund_order(?UID, ?ORDER_NO)
    ).

%% 管理员退款复用同一原语（mark_refunding + finalize_refund）
admin_refund_uses_same_primitive() ->
    meck:expect(channel_order_ds, find_by_order_no, fun(OrderNo) ->
        {ok, #{
            <<"order_no">> => OrderNo,
            <<"channel_id">> => ?CID,
            <<"user_id">> => ?UID,
            <<"amount">> => 9.90,
            <<"status">> => 1,
            <<"payment_method">> => <<"wallet">>,
            <<"payment_no">> => <<"WPY_ORD_ADM">>
        }}
    end),
    Result = channel_logic_order:admin_refund_order(?ORDER_NO, <<"管理员退款"/utf8>>),
    ?assertEqual(ok, Result),
    %% 管理员退款也走了 mark_refunding → finalize_refund 链路
    ?assert(meck:called(channel_order_ds, mark_refunding, [?ORDER_NO])),
    ?assert(meck:called(channel_order_ds, finalize_refund, [?ORDER_NO, ?UID, <<"管理员退款"/utf8>>])).
