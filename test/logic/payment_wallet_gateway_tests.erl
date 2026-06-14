-module(payment_wallet_gateway_tests).

%%%===================================================================
%%% @doc payment_wallet_gateway 元→分换算单元测试
%%%
%%% 金额换算是支付的金融核心，必须精确无浮点误差。
%%% 覆盖：整数 / 浮点 / binary 字符串（多种小数位）/ 边界 / 非法输入。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

%% 整数元 → 分
yuan_to_fen_integer_test() ->
    ?assertEqual(900, payment_wallet_gateway:yuan_to_fen(9)),
    ?assertEqual(0, payment_wallet_gateway:yuan_to_fen(0)),
    ?assertEqual(1000000, payment_wallet_gateway:yuan_to_fen(10000)).

%% 浮点元 → 分（round 处理）
yuan_to_fen_float_test() ->
    ?assertEqual(990, payment_wallet_gateway:yuan_to_fen(9.9)),
    ?assertEqual(1, payment_wallet_gateway:yuan_to_fen(0.01)),
    ?assertEqual(12345, payment_wallet_gateway:yuan_to_fen(123.45)).

%% binary 字符串（numeric 从 epgsql 常以 binary 返回）—— 整数精确换算
yuan_to_fen_binary_test() ->
    ?assertEqual(990, payment_wallet_gateway:yuan_to_fen(<<"9.90">>)),
    ?assertEqual(990, payment_wallet_gateway:yuan_to_fen(<<"9.9">>)),
    ?assertEqual(900, payment_wallet_gateway:yuan_to_fen(<<"9">>)),
    ?assertEqual(1, payment_wallet_gateway:yuan_to_fen(<<"0.01">>)),
    ?assertEqual(10000, payment_wallet_gateway:yuan_to_fen(<<"100">>)),
    ?assertEqual(0, payment_wallet_gateway:yuan_to_fen(<<"0">>)),
    ?assertEqual(10001, payment_wallet_gateway:yuan_to_fen(<<"100.01">>)).

%% 小数超过 2 位 —— 截断到 2 位（建表已约束 numeric(10,2)）
yuan_to_fen_truncate_test() ->
    ?assertEqual(999, payment_wallet_gateway:yuan_to_fen(<<"9.999">>)),
    ?assertEqual(990, payment_wallet_gateway:yuan_to_fen(<<"9.901">>)).

%% list 字符串
yuan_to_fen_list_test() ->
    ?assertEqual(990, payment_wallet_gateway:yuan_to_fen("9.90")),
    ?assertEqual(1000, payment_wallet_gateway:yuan_to_fen("10")).

%% 非法输入 —— 安全返回 0（不崩溃）
yuan_to_fen_invalid_test() ->
    ?assertEqual(0, payment_wallet_gateway:yuan_to_fen(undefined)),
    ?assertEqual(0, payment_wallet_gateway:yuan_to_fen(<<"abc">>)),
    ?assertEqual(0, payment_wallet_gateway:yuan_to_fen(#{})).
