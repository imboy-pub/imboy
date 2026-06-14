-module(channel_logic_order_tests).

%%%===================================================================
%%% @doc channel_logic_order 金额单位适配单测
%%%
%%% 验证 HIGH bug 修复：channel_order.amount 为元，按目标网关期望单位适配
%%% （wallet 期望元 / 第三方期望分），防止"元误当分少 100 倍"回归。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

%% wallet 网关期望「元」—— 原样透传（payment_wallet_gateway 内部再换分）
to_gateway_amount_wallet_test() ->
    ?assertEqual(990, channel_logic_order:to_gateway_amount(<<"wallet">>, 990)),
    ?assertEqual(<<"9.90">>, channel_logic_order:to_gateway_amount(<<"wallet">>, <<"9.90">>)),
    ?assertEqual(9.9, channel_logic_order:to_gateway_amount(<<"wallet">>, 9.9)).

%% 第三方网关期望「分」—— 元×100
to_gateway_amount_thirdparty_test() ->
    ?assertEqual(990, channel_logic_order:to_gateway_amount(<<"alipay">>, <<"9.90">>)),
    ?assertEqual(10000, channel_logic_order:to_gateway_amount(<<"stripe">>, 100)),
    ?assertEqual(1, channel_logic_order:to_gateway_amount(<<"wechat">>, <<"0.01">>)),
    ?assertEqual(990, channel_logic_order:to_gateway_amount(<<"alipay">>, 9.9)).

%% 元 → 分 换算：整数/浮点/binary(多小数位)/截断/非法
yuan_to_fen_test() ->
    ?assertEqual(900, channel_logic_order:yuan_to_fen(9)),
    ?assertEqual(990, channel_logic_order:yuan_to_fen(9.9)),
    ?assertEqual(990, channel_logic_order:yuan_to_fen(<<"9.90">>)),
    ?assertEqual(990, channel_logic_order:yuan_to_fen(<<"9.9">>)),
    ?assertEqual(900, channel_logic_order:yuan_to_fen(<<"9">>)),
    ?assertEqual(1, channel_logic_order:yuan_to_fen(<<"0.01">>)),
    ?assertEqual(10001, channel_logic_order:yuan_to_fen(<<"100.01">>)),
    %% 超 2 位小数截断（建表约束 numeric(10,2)）
    ?assertEqual(999, channel_logic_order:yuan_to_fen(<<"9.999">>)),
    %% 非法输入安全返回 0
    ?assertEqual(0, channel_logic_order:yuan_to_fen(<<"abc">>)).
