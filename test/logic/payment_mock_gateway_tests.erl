-module(payment_mock_gateway_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% payment_mock_gateway 模块的 EUnit 测试
%%%
%%% 目标：验证 Mock 支付网关的行为
%%% 覆盖：
%%%   - pay/3 返回 {ok, <<"MOCK_", OrderNo/binary>>}
%%%   - pay/3 对不同金额行为一致（金额被忽略）
%%%   - refund/2 返回 ok
%%%   - refund/2 对不同参数行为一致（参数被忽略）
%%%===================================================================

%% ===================================================================
%% pay/3 测试
%% ===================================================================

pay_returns_ok_tuple_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = payment_mock_gateway:pay(<<"order_001">>, 100, #{}),
        ?assertMatch({ok, _}, Result)
    end).

pay_returns_mock_prefixed_payment_no_test_() ->
    ?TEST_SIMPLE(fun() ->
        {ok, PayNo} = payment_mock_gateway:pay(<<"order_001">>, 100, #{}),
        ?assertMatch(<<"MOCK_", _/binary>>, PayNo)
    end).

pay_embeds_order_no_in_payment_no_test_() ->
    ?TEST_SIMPLE(fun() ->
        OrderNo = <<"order_abc123">>,
        {ok, PayNo} = payment_mock_gateway:pay(OrderNo, 500, #{}),
        ?assertEqual(<<"MOCK_order_abc123">>, PayNo)
    end).

pay_ignores_amount_test_() ->
    ?TEST_SIMPLE(fun() ->
        OrderNo = <<"order_same">>,
        {ok, PayNo1} = payment_mock_gateway:pay(OrderNo, 1, #{}),
        {ok, PayNo2} = payment_mock_gateway:pay(OrderNo, 99999, #{}),
        ?assertEqual(PayNo1, PayNo2)
    end).

pay_ignores_opts_test_() ->
    ?TEST_SIMPLE(fun() ->
        OrderNo = <<"order_opts">>,
        {ok, PayNo1} = payment_mock_gateway:pay(OrderNo, 100, #{}),
        {ok, PayNo2} = payment_mock_gateway:pay(OrderNo, 100, #{extra => <<"data">>}),
        ?assertEqual(PayNo1, PayNo2)
    end).

pay_with_empty_order_no_test_() ->
    ?TEST_SIMPLE(fun() ->
        {ok, PayNo} = payment_mock_gateway:pay(<<>>, 100, #{}),
        ?assertEqual(<<"MOCK_">>, PayNo)
    end).

%% ===================================================================
%% refund/2 测试
%% ===================================================================

refund_returns_ok_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = payment_mock_gateway:refund(<<"MOCK_order_001">>, 100),
        ?assertEqual(ok, Result)
    end).

refund_ignores_payment_no_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(ok, payment_mock_gateway:refund(<<"any_payment">>, 0)),
        ?assertEqual(ok, payment_mock_gateway:refund(<<>>, 999))
    end).

refund_ignores_amount_test_() ->
    ?TEST_SIMPLE(fun() ->
        PayNo = <<"MOCK_order_x">>,
        ?assertEqual(ok, payment_mock_gateway:refund(PayNo, 0)),
        ?assertEqual(ok, payment_mock_gateway:refund(PayNo, 99999))
    end).

%% ===================================================================
%% behaviour 契约测试
%% ===================================================================

pay_result_is_ok_binary_tuple_test_() ->
    ?TEST_SIMPLE(fun() ->
        {ok, PayNo} = payment_mock_gateway:pay(<<"order_x">>, 1, #{}),
        ?assert(is_binary(PayNo))
    end).
