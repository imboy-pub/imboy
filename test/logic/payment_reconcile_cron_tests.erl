-module(payment_reconcile_cron_tests).
-compile([nowarn_deprecated_catch]).

%%%===================================================================
%%% @doc B-06：对账 job 的 ecron 入口 —— 发现漏单要能自动补单并推指标。
%%%
%%% 判据「制造漏单 → 对账 job 自动补单并告警」：这里覆盖前两段（发现 + 补单 +
%%% 指标）；最后一段"告警响过一次"属 B-07 的 Prometheus 规则 + 真实触发演练。
%%%
%%% 手法：meck DS 边界 + payment_callback_logic:credit/1，不触真实 PG。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

setup() ->
    meck:new(payment_transaction_ds, [no_link, passthrough]),
    meck:new(payment_callback_logic, [no_link, passthrough]),
    meck:new(elib_metric, [passthrough, non_strict]),
    meck:expect(elib_metric, increment, fun(_N, _V, _L) -> ok end),
    meck:expect(payment_callback_logic, credit, fun(_Fields) -> ok end),
    ok.

cleanup(_) ->
    catch meck:unload(elib_metric),
    catch meck:unload(payment_callback_logic),
    catch meck:unload(payment_transaction_ds),
    ok.

%% 一条「已收款但频道订单还没落地」的流水 = 漏单
leaked_tx() ->
    #{
        <<"trade_no">> => <<"PT_wechat_WX_1">>,
        <<"biz_type">> => 2,
        <<"biz_order_no">> => <<"CH_LEAK_1">>,
        <<"user_id">> => 9001,
        <<"gateway">> => <<"wechat">>,
        <<"gateway_payment_no">> => <<"WX_1">>,
        <<"amount">> => 990,
        <<"status">> => 1
    }.

stub_rows(Rows) ->
    meck:expect(payment_transaction_ds, reconcile_list, fun(_F, _T, _S) -> {ok, Rows} end).

%% 业务订单状态查询走 elib_pg:query（reconcile 内部直查），按需打桩
stub_order_status(Result) ->
    meck:expect(elib_pg, query, fun(_Sql, _Args) -> Result end).

reconcile_cron_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun leaked_order_is_repaired/0,
        fun consistent_order_is_not_touched/0,
        fun missing_order_is_not_repaired/0,
        fun repair_failure_does_not_crash_the_job/0,
        fun lookback_window_has_floor/0
    ]}.

with_pg(Fun) ->
    meck:new(elib_pg, [no_link, passthrough]),
    try
        Fun()
    after
        catch meck:unload(elib_pg)
    end.

metric_names() ->
    [N || {_P, {elib_metric, increment, [N | _]}, _R} <- meck:history(elib_metric)].

%% 漏单（流水成功、订单仍未支付）→ 调 credit/1 补单
leaked_order_is_repaired() ->
    with_pg(fun() ->
        stub_rows([leaked_tx()]),
        %% 订单存在但 status=0 → status_mismatch
        stub_order_status({ok, [#{<<"status">> => 0}]}),
        ?assertEqual(ok, payment_reconcile_logic:run_hourly(3)),
        ?assert(meck:called(payment_callback_logic, credit, '_')),
        [Fields] = [
            F
         || {_P, {payment_callback_logic, credit, [F]}, _R} <- meck:history(
                payment_callback_logic
            )
        ],
        %% 补单必须带齐 credit/1 需要的字段，否则补了个空
        ?assertEqual(2, maps:get(biz_type, Fields)),
        ?assertEqual(<<"CH_LEAK_1">>, maps:get(biz_order_no, Fields)),
        ?assertEqual(9001, maps:get(user_id, Fields)),
        ?assertEqual(990, maps:get(amount, Fields)),
        ?assertEqual(<<"WX_1">>, maps:get(gateway_payment_no, Fields)),
        ?assert(lists:member(payment_reconcile_mismatch_total, metric_names())),
        ?assert(lists:member(payment_reconcile_repair_total, metric_names()))
    end).

%% 一致（订单已支付）→ 不得触发补单
consistent_order_is_not_touched() ->
    with_pg(fun() ->
        stub_rows([leaked_tx()]),
        stub_order_status({ok, [#{<<"status">> => 1}]}),
        ?assertEqual(ok, payment_reconcile_logic:run_hourly(3)),
        ?assertNot(meck:called(payment_callback_logic, credit, '_'))
    end).

%% 业务订单根本不存在 → 补不了，单独计数交人工，不得盲目调 credit
missing_order_is_not_repaired() ->
    with_pg(fun() ->
        stub_rows([leaked_tx()]),
        stub_order_status({ok, []}),
        ?assertEqual(ok, payment_reconcile_logic:run_hourly(3)),
        ?assertNot(meck:called(payment_callback_logic, credit, '_')),
        ?assert(lists:member(payment_reconcile_unrepairable_total, metric_names()))
    end).

%% 单条补单失败不得掀掉整个 job（否则一条脏数据让对账永久停摆）
repair_failure_does_not_crash_the_job() ->
    with_pg(fun() ->
        stub_rows([leaked_tx(), leaked_tx()]),
        stub_order_status({ok, [#{<<"status">> => 0}]}),
        meck:expect(payment_callback_logic, credit, fun(_F) -> erlang:error(boom) end),
        ?assertEqual(ok, payment_reconcile_logic:run_hourly(3)),
        %% 两条都尝试过，没有在第一条就中断。
        %% ⚠️ 不能用 history 的三元组解构数：抛异常的调用在 history 里是**五元组**
        %%    {Pid, MFA, Class, Reason, Stacktrace}，三元组模式会一条都匹配不上。
        ?assertEqual(2, meck:num_calls(payment_callback_logic, credit, '_'))
    end).

%% 回看窗口有下限：配 0 也不能退化成无重叠窗口
lookback_window_has_floor() ->
    with_pg(fun() ->
        stub_rows([]),
        stub_order_status({ok, []}),
        ?assertEqual(ok, payment_reconcile_logic:run_hourly(0)),
        [{_P, {payment_transaction_ds, reconcile_list, [From, To, _S]}, _R} | _] =
            meck:history(payment_transaction_ds),
        FromMs = elib_dt:rfc3339_to(From),
        ToMs = elib_dt:rfc3339_to(To),
        %% 下限 2 小时 → 窗口至少 2 小时
        ?assert(ToMs - FromMs >= 2 * 3600 * 1000)
    end).
