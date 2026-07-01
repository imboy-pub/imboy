-module(recharge_logic_query_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc recharge_logic:query/2 充值订单查询的安全契约测试。
%%%
%%% 验证 IDOR 防护：query(Uid, OrderNo) 仅当 order.user_id =:= Uid 才返回
%%%   订单，非本人返回 {error, <<"无权查看此订单">>}；订单不存在返回
%%%   {error, not_found}。对应路由 GET /v1/wallet/recharge/:order_no（P0 支付）。
%%%
%%% 手法：meck recharge_order_ds，绝不触真实 PG。
%%% @end
%%%===================================================================

-define(UID, 4001).
-define(OTHER_UID, 4002).

setup() ->
    application:set_env(imboy, env, test),
    meck:new(recharge_order_ds, [no_link, passthrough]),
    ok.

cleanup(_) ->
    meck:unload(recharge_order_ds),
    ok.

recharge_query_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun query_owner_returns_order/0,
        fun query_non_owner_rejected/0,
        fun query_not_found/0
    ]}.

order_for(Uid) ->
    #{
        <<"id">> => 9001,
        <<"order_no">> => <<"RCH_Q1">>,
        <<"user_id">> => Uid,
        <<"amount">> => 1999,
        <<"currency">> => <<"CNY">>,
        <<"payment_method">> => <<"alipay">>,
        <<"status">> => 0
    }.

%% 本人查询 → 返回订单（正常路径）
query_owner_returns_order() ->
    meck:expect(recharge_order_ds, find_by_order_no, fun(<<"RCH_Q1">>) ->
        {ok, order_for(?UID)}
    end),
    {ok, Order} = recharge_logic:query(?UID, <<"RCH_Q1">>),
    ?assertEqual(?UID, maps:get(<<"user_id">>, Order)),
    ?assertEqual(<<"RCH_Q1">>, maps:get(<<"order_no">>, Order)).

%% 非本人查询 → 拒绝（IDOR 防护 / 非法输入）
query_non_owner_rejected() ->
    meck:expect(recharge_order_ds, find_by_order_no, fun(<<"RCH_Q2">>) ->
        {ok, order_for(?OTHER_UID)}
    end),
    ?assertEqual(
        {error, <<"无权查看此订单"/utf8>>},
        recharge_logic:query(?UID, <<"RCH_Q2">>)
    ).

%% 订单不存在 → 用户态错误消息（load_order_raw 将 not_found 转中文）
query_not_found() ->
    meck:expect(recharge_order_ds, find_by_order_no, fun(<<"RCH_MISSING">>) ->
        {error, not_found}
    end),
    ?assertEqual(
        {error, <<"订单不存在"/utf8>>},
        recharge_logic:query(?UID, <<"RCH_MISSING">>)
    ).
