-module(payment_callback_logic_tests).
-compile([nowarn_deprecated_catch]).

%%%===================================================================
%%% @doc payment_callback_logic 回调字段提取/业务类型推导单测
%%%
%%% 验证 biz_type 由订单号前缀推导、各网关字段名兼容(pick)、频道金额元→分。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

%% biz_type 由订单号前缀推导：RCH=充值(1) / CH=频道(2)；显式值作兜底
biz_type_of_test() ->
    ?assertEqual(1, payment_callback_logic:biz_type_of(<<"RCH1781400000123">>, 0)),
    ?assertEqual(2, payment_callback_logic:biz_type_of(<<"CH1781400000456">>, 0)),
    %% 无法识别前缀时用回调显式 biz_type 兜底
    ?assertEqual(2, payment_callback_logic:biz_type_of(<<"UNKNOWN">>, 2)),
    ?assertEqual(0, payment_callback_logic:biz_type_of(<<"UNKNOWN">>, 0)),
    %% RCH 优先于 CH（不被 CH 子句误匹配）
    ?assertEqual(1, payment_callback_logic:biz_type_of(<<"RCH999">>, 0)).

%% pick：按候选键列表取第一个非空值（适配各网关字段名）
pick_test() ->
    %% 命中第二候选键（微信 transaction_id）
    ?assertEqual(
        <<"4200001">>,
        payment_callback_logic:pick(
            #{<<"transaction_id">> => <<"4200001">>},
            [<<"gateway_payment_no">>, <<"transaction_id">>, <<"trade_no">>]
        )
    ),
    %% 命中第一候选键（统一字段优先）
    ?assertEqual(
        <<"UNI">>,
        payment_callback_logic:pick(
            #{<<"gateway_payment_no">> => <<"UNI">>, <<"trade_no">> => <<"TN">>},
            [<<"gateway_payment_no">>, <<"trade_no">>]
        )
    ),
    %% 全部缺失返回空
    ?assertEqual(<<>>, payment_callback_logic:pick(#{}, [<<"a">>, <<"b">>])),
    %% 空值跳过继续找
    ?assertEqual(
        <<"X">>,
        payment_callback_logic:pick(
            #{<<"a">> => <<>>, <<"b">> => <<"X">>}, [<<"a">>, <<"b">>]
        )
    ).

%% 支付回调结果指标必须在唯一出入口产出（C0-OPS-01）。
%% deploy/prometheus/rules/imboy-alerts.yml 的 imboy.payment 告警组依赖这两个
%% 计数器；若未来重构把 increment 丢了，告警会变成永远不触发的死规则。
callback_result_metric_test_() ->
    {setup, fun metric_setup/0, fun metric_cleanup/1, [
        fun sign_failure_emits_both_counters/0
    ]}.

metric_setup() ->
    meck:new(elib_metric, [passthrough, non_strict]),
    meck:expect(elib_metric, increment, fun(_N, _D, _L) -> ok end),
    meck:new(payment_sign, [passthrough, non_strict]),
    meck:expect(payment_sign, verify, fun(_G, _R, _H) -> {error, bad_sign} end),
    ok.

metric_cleanup(_) ->
    catch meck:unload(payment_sign),
    catch meck:unload(elib_metric),
    ok.

sign_failure_emits_both_counters() ->
    ?assertEqual(
        {error, <<"验签失败"/utf8>>},
        payment_callback_logic:handle(<<"alipay">>, #{}, #{raw => <<>>, headers => #{}})
    ),
    Calls = [Args || {_Pid, {elib_metric, increment, Args}, _Ret} <- meck:history(elib_metric)],
    Names = [N || [N | _] <- Calls],
    %% 验签失败要同时计入「安全信号」与「总结果」两个维度
    ?assert(lists:member(payment_callback_sign_failed_total, Names)),
    ?assert(lists:member(payment_callback_total, Names)),
    %% 总结果维度必须带 result="error" 标签，否则告警的 result 过滤失效
    ?assert(
        lists:any(
            fun
                ([payment_callback_total, _D, #{result := <<"error">>}]) -> true;
                (_) -> false
            end,
            Calls
        )
    ).

%%%===================================================================
%%% B-02：biz_type=2（频道订单）回调分支端到端。
%%%
%%% B-01 把第三方支付的发货动作全部移到回调侧后，这条分支成了付费频道**唯一**
%%% 的发货路径 —— 而它此前只有 helper 单测，主流程零行为覆盖。
%%% 这里走公开入口 handle/3，mock 所有 DS 边界，不触真实 PG。
%%%
%%% 真实沙箱 e2e（网关沙箱付款 → 回调到达 → 频道可见）需要运行中的后端 +
%%% PG + 网关沙箱凭据，不在单测范围内；本组测试钉死的是回调到达之后的行为。
%%%===================================================================

-define(CB_CID, 77).
-define(CB_UID, 9001).
-define(CB_ORDER, <<"CH1781400000456">>).

channel_callback_test_() ->
    {foreach, fun cb_setup/0, fun cb_cleanup/1, [
        fun callback_marks_paid_and_subscribes/0,
        fun repeat_callback_is_idempotent/0,
        fun subscribe_failure_does_not_report_paid/0,
        fun amount_comes_from_order_not_callback/0
    ]}.

cb_setup() ->
    meck:new(payment_sign, [passthrough, non_strict]),
    meck:expect(payment_sign, verify, fun(_G, _R, _H) -> {ok, #{}} end),
    meck:new(elib_metric, [passthrough, non_strict]),
    meck:expect(elib_metric, increment, fun(_N, _D, _L) -> ok end),
    meck:new(channel_order_ds, [no_link, passthrough]),
    meck:new(channel_ds, [no_link, passthrough]),
    meck:new(payment_transaction_ds, [no_link, passthrough]),
    %% 订单存在且待支付（B-01 之后第三方订单在回调前就是这个状态）
    meck:expect(channel_order_ds, find_by_order_no, fun(OrderNo) ->
        {ok, #{
            <<"order_no">> => OrderNo,
            <<"channel_id">> => ?CB_CID,
            <<"user_id">> => ?CB_UID,
            <<"amount">> => <<"9.90">>,
            <<"currency">> => <<"CNY">>,
            <<"status">> => 0
        }}
    end),
    meck:expect(channel_order_ds, pay, fun(_OrderNo, _PaymentData) -> ok end),
    meck:expect(channel_ds, subscribe, fun(_ChannelId, _Uid) -> ok end),
    %% 无既有流水 → 走首次回调路径
    meck:expect(payment_transaction_ds, find_by_gateway_no, fun(_G, _No) -> #{} end),
    meck:expect(payment_transaction_ds, create, fun(_Data) -> {ok, 1} end),
    meck:expect(payment_transaction_ds, mark_success, fun(_TradeNo, _Extra) -> {ok, 1} end),
    ok.

cb_cleanup(_) ->
    catch meck:unload(payment_transaction_ds),
    catch meck:unload(channel_ds),
    catch meck:unload(channel_order_ds),
    catch meck:unload(elib_metric),
    catch meck:unload(payment_sign),
    ok.

cb_notify() ->
    #{<<"gateway_payment_no">> => <<"WX_4200001">>, <<"out_trade_no">> => ?CB_ORDER}.

cb_handle() ->
    payment_callback_logic:handle(<<"wechat">>, cb_notify(), #{
        raw => <<"{\"x\":1}">>, headers => #{}
    }).

%% 回调到达 → 订单标记已支付 + 频道订阅生效 + 流水置成功
callback_marks_paid_and_subscribes() ->
    ?assertEqual({ok, paid}, cb_handle()),
    ?assert(meck:called(channel_order_ds, pay, '_')),
    ?assert(meck:called(channel_ds, subscribe, [?CB_CID, ?CB_UID])),
    ?assert(meck:called(payment_transaction_ds, mark_success, '_')).

%% 网关重推：已成功流水命中 → 直接 already，不得二次发货
repeat_callback_is_idempotent() ->
    meck:expect(payment_transaction_ds, find_by_gateway_no, fun(_G, _No) ->
        #{<<"status">> => 1, <<"trade_no">> => <<"PT_wechat_WX_4200001">>}
    end),
    ?assertEqual({ok, already}, cb_handle()),
    ?assertNot(meck:called(channel_order_ds, pay, '_')),
    ?assertNot(meck:called(channel_ds, subscribe, '_')).

%% 订阅失败不得回报 paid —— 否则网关停推，用户付了钱永远看不到频道
subscribe_failure_does_not_report_paid() ->
    meck:expect(channel_ds, subscribe, fun(_C, _U) -> {error, db_down} end),
    ?assertEqual({error, <<"频道订阅失败"/utf8>>}, cb_handle()),
    ?assertNot(meck:called(payment_transaction_ds, mark_success, '_')).

%% 金额以订单为准，不信回调金额（回调可伪造）：9.90 元 → 990 分入流水
amount_comes_from_order_not_callback() ->
    Notify = maps:put(<<"amount">>, 1, cb_notify()),
    ?assertEqual(
        {ok, paid},
        payment_callback_logic:handle(<<"wechat">>, Notify, #{raw => <<>>, headers => #{}})
    ),
    [Data] = [
        D
     || {_P, {payment_transaction_ds, create, [D]}, _R} <- meck:history(
            payment_transaction_ds
        )
    ],
    ?assertEqual(990, maps:get(<<"amount">>, Data)),
    ?assertEqual(?CB_UID, maps:get(<<"user_id">>, Data)).

%% 频道订单金额 元 → 分
yuan_to_fen_test() ->
    ?assertEqual(900, payment_callback_logic:yuan_to_fen(9)),
    ?assertEqual(990, payment_callback_logic:yuan_to_fen(<<"9.90">>)),
    ?assertEqual(10001, payment_callback_logic:yuan_to_fen(<<"100.01">>)),
    ?assertEqual(0, payment_callback_logic:yuan_to_fen(<<"bad">>)).
