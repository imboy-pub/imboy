-module(payment_gateway_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% payment_gateway 模块的 EUnit 测试
%%%
%%% 目标：验证支付网关路由和错误处理逻辑
%%% 覆盖：
%%%   - method_module/1 返回值格式
%%%   - pay/3 路由到 mock 网关
%%%   - pay/3 路由到 wallet 网关
%%%   - pay/3 对未知支付方式返回 {error, _}（不崩溃）
%%%   - refund/2 对未知支付方式返回 {error, _}
%%%===================================================================

%% ===================================================================
%% method_module/1 测试
%% ===================================================================

method_module_mock_returns_ok_tuple_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = payment_gateway:method_module(<<"mock">>),
        ?assertEqual({ok, payment_mock_gateway}, Result)
    end).

method_module_wallet_returns_ok_tuple_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = payment_gateway:method_module(<<"wallet">>),
        ?assertEqual({ok, payment_wallet_gateway}, Result)
    end).

method_module_unknown_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = payment_gateway:method_module(<<"unknown">>),
        ?assertMatch({error, _}, Result)
    end).

method_module_empty_string_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = payment_gateway:method_module(<<>>),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% pay/3 测试 — mock 网关路由
%% ===================================================================

pay_with_mock_gateway_succeeds_test_() ->
    ?TEST_SIMPLE(fun() ->
        OrderNo = <<"order_123">>,
        Opts = #{amount => 100},
        Result = payment_gateway:pay(<<"mock">>, OrderNo, Opts),
        ?assertMatch({ok, <<"MOCK_", _/binary>>}, Result)
    end).

pay_with_mock_gateway_returns_payment_no_test_() ->
    ?TEST_SIMPLE(fun() ->
        OrderNo = <<"order_456">>,
        Opts = #{amount => 500},
        {ok, PayNo} = payment_gateway:pay(<<"mock">>, OrderNo, Opts),
        ?assertEqual(<<"MOCK_order_456">>, PayNo)
    end).

%% ===================================================================
%% pay/3 测试 — 未知支付方式（不应崩溃）
%% ===================================================================

pay_with_unknown_method_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = payment_gateway:pay(<<"bitcoin">>, <<"order_789">>, #{amount => 100}),
        ?assertMatch({error, _}, Result)
    end).

pay_with_null_method_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = payment_gateway:pay(<<>>, <<"order_000">>, #{amount => 0}),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% refund/2 测试 — mock 网关路由
%% ===================================================================

refund_with_mock_gateway_returns_ok_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = payment_gateway:refund(<<"mock">>, <<"MOCK_order_123">>),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% refund/2 测试 — 未知支付方式（不应崩溃）
%% ===================================================================

refund_with_unknown_method_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = payment_gateway:refund(<<"bitcoin">>, <<"PAY_123">>),
        ?assertMatch({error, _}, Result)
    end).
