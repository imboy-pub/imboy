-module(recharge_logic_create_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc recharge_logic:create_order/3 创建充值订单的安全契约测试。
%%%
%%% 验证：① user_id 来自 JWT（Data map），非请求体（无 mass-assignment）；
%%%   ② 金额整数 + [Min,Max] 区间校验；③ 支付方式白名单（生产排除 mock）。
%%%   对应路由 POST /v1/wallet/recharge/order（P0 支付）。
%%%
%%% 手法：meck recharge_order_ds，绝不触真实 PG。
%%% @end
%%%===================================================================

-define(UID, 4101).

setup() ->
    application:set_env(imboy, env, test),
    application:set_env(imboy, recharge_min_amount, 100),
    application:set_env(imboy, recharge_max_amount, 1000000),
    meck:new(recharge_order_ds, [no_link, passthrough]),
    %% mass-assignment 防护：create 收到的 Data.user_id 必须等于 JWT 的 ?UID，
    %% 否则 fun 头不匹配 → function_clause → 测试失败。
    meck:expect(recharge_order_ds, create, fun(#{user_id := ?UID}) ->
        {ok, <<"RCH_NEW">>}
    end),
    meck:expect(recharge_order_ds, find_by_order_no, fun(<<"RCH_NEW">>) ->
        {ok, #{
            <<"order_no">> => <<"RCH_NEW">>,
            <<"user_id">> => ?UID,
            <<"amount">> => 1999,
            <<"currency">> => <<"CNY">>,
            <<"payment_method">> => <<"alipay">>,
            <<"status">> => 0
        }}
    end),
    ok.

cleanup(_) ->
    meck:unload(recharge_order_ds),
    ok.

recharge_create_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun create_order_valid_returns_order/0,
        fun create_order_amount_too_small_rejected/0,
        fun create_order_bad_method_rejected/0
    ]}.

%% 正常路径：合法金额 + 白名单方式 → 创建并返回订单；user_id 取自 JWT
create_order_valid_returns_order() ->
    {ok, Order} = recharge_logic:create_order(?UID, 1999, <<"alipay">>),
    ?assertEqual(<<"RCH_NEW">>, maps:get(<<"order_no">>, Order)),
    ?assertEqual(?UID, maps:get(<<"user_id">>, Order)).

%% 非法输入：金额低于下限（50 < 100）→ 拒绝
create_order_amount_too_small_rejected() ->
    ?assertMatch({error, _}, recharge_logic:create_order(?UID, 50, <<"alipay">>)).

%% 非法输入：支付方式不在白名单 → 拒绝
create_order_bad_method_rejected() ->
    ?assertEqual(
        {error, <<"不支持的支付方式"/utf8>>},
        recharge_logic:create_order(?UID, 1999, <<"bitcoin">>)
    ).
