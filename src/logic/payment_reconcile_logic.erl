-module(payment_reconcile_logic).
-compile([nowarn_deprecated_catch]).
%%%===================================================================
%%% @doc 支付对账逻辑 / Payment reconciliation logic
%%%
%%% 以 payment_transaction 为权威账，核对其与业务订单
%%% （recharge_order / channel_order）的状态一致性，输出不一致清单供人工/告警处理。
%%%
%%% 一致性定义（成功流水 status=1）：
%%%   - biz_type=1 充值：recharge_order.status 应为 1（已支付）。
%%%   - biz_type=2 频道订单：channel_order.status 应为 1（已支付）。
%%%   不一致 = 业务订单缺失，或状态非 1（疑似入账后业务侧未落地）。
%%%
%%% 用法：
%%%   payment_reconcile_logic:check(FromTs, ToTs)            %% 全部成功流水
%%%   payment_reconcile_logic:check(FromTs, ToTs, Status)    %% 指定流水状态
%%% FromTs/ToTs 为 RFC3339 binary（如 elib_dt:now()/minus 产出）。
%%% @end
%%%===================================================================

-export([check/2, check/3]).
%% ecron 入口（B-06）
-export([run_hourly/0, run_hourly/1]).

-include("log.hrl").

-define(TX_SUCCESS, 1).
-define(BIZ_RECHARGE, 1).
-define(BIZ_CHANNEL_ORDER, 2).

%% 回看窗口（小时）。每小时跑一次却回看更久是**故意重叠**：
%% 补单原语幂等，重复核对无副作用，而窗口不重叠会让恰好跨边界的漏单永远漏掉。
%% 下限 2 小时，防止配成 0/1 退化成无重叠。
-define(MIN_LOOKBACK_HOURS, 2).
-define(DEFAULT_LOOKBACK_HOURS, 25).

%% @doc 核对成功流水的业务一致性
-spec check(binary(), binary()) -> {ok, map()} | {error, term()}.
check(FromTs, ToTs) ->
    check(FromTs, ToTs, ?TX_SUCCESS).

%% @doc 核对指定状态流水
%% @return {ok, #{total, consistent, mismatched, mismatches => [Detail]}} | {error, term()}
-spec check(binary(), binary(), integer() | all) -> {ok, map()} | {error, term()}.
check(FromTs, ToTs, Status) ->
    case payment_transaction_ds:reconcile_list(FromTs, ToTs, Status) of
        {ok, Rows} ->
            {Consistent, Mismatches} =
                lists:foldl(fun reconcile_row/2, {0, []}, Rows),
            Result = #{
                total => length(Rows),
                consistent => Consistent,
                mismatched => length(Mismatches),
                mismatches => lists:reverse(Mismatches)
            },
            {ok, Result};
        {error, Reason} ->
            ?ERROR_LOG([payment_reconcile, list_failed, Reason]),
            {error, Reason}
    end.

%% ===================================================================
%% B-06 ecron 入口：定时对账 → 自动补单 → 推指标供告警消费
%% ===================================================================

%% @doc ecron 入口（每小时）。恒 ok，任何异常记日志后跳过，绝不抛给调度器。
-spec run_hourly() -> ok.
run_hourly() ->
    Hours = config_ds:env(payment_reconcile_lookback_hours, ?DEFAULT_LOOKBACK_HOURS),
    run_hourly(Hours).

-spec run_hourly(integer()) -> ok.
run_hourly(Hours0) ->
    Hours = max(?MIN_LOOKBACK_HOURS, to_int(Hours0)),
    try
        ToTs = elib_dt:now(),
        FromTs = elib_dt:minus(ToTs, {Hours * 60, minute}),
        do_run(FromTs, ToTs)
    catch
        Class:Reason:St ->
            ?ERROR_LOG([payment_reconcile, run_failed, Class, Reason, St]),
            _ = elib_metric:increment(payment_reconcile_error_total, 1, #{}),
            ok
    end.

-spec do_run(binary(), binary()) -> ok.
do_run(FromTs, ToTs) ->
    case check(FromTs, ToTs) of
        {ok, #{total := Total, mismatched := N, mismatches := Mismatches}} ->
            _ = elib_metric:increment(payment_reconcile_checked_total, Total, #{}),
            _ = elib_metric:increment(payment_reconcile_mismatch_total, N, #{}),
            case N of
                0 -> ok;
                _ -> repair_all(Mismatches)
            end;
        {error, Reason} ->
            ?ERROR_LOG([payment_reconcile, check_failed, Reason]),
            _ = elib_metric:increment(payment_reconcile_error_total, 1, #{}),
            ok
    end.

%% @doc 逐条补单。补单本身幂等（复用回调侧的 credit/1），失败只计数不中断后续条目。
-spec repair_all([map()]) -> ok.
repair_all(Mismatches) ->
    lists:foreach(fun repair_one/1, Mismatches),
    ok.

-spec repair_one(map()) -> ok.
repair_one(#{reason := <<"order_missing">>} = M) ->
    %% 业务订单根本不存在 —— 补不了，只能人工介入（可能是脏流水或订单被误删）。
    ?ERROR_LOG([payment_reconcile, unrepairable, maps:get(trade_no, M, <<>>)]),
    _ = elib_metric:increment(payment_reconcile_unrepairable_total, 1, #{}),
    ok;
repair_one(M) ->
    BizType = to_int(maps:get(biz_type, M, 0)),
    Fields = #{
        biz_type => BizType,
        biz_order_no => maps:get(biz_order_no, M, <<>>),
        gateway => maps:get(gateway, M, <<>>),
        gateway_payment_no => maps:get(gateway_payment_no, M, <<>>),
        user_id => to_int(maps:get(user_id, M, 0)),
        amount => to_int(maps:get(amount, M, 0))
    },
    Outcome =
        try payment_callback_logic:credit(Fields) of
            ok -> <<"repaired">>;
            {error, Why} -> log_repair_failed(M, Why)
        catch
            Class:Why:St -> log_repair_failed(M, {Class, Why, St})
        end,
    _ = elib_metric:increment(
        payment_reconcile_repair_total, 1, #{outcome => Outcome, biz_type => BizType}
    ),
    ok.

-spec log_repair_failed(map(), term()) -> binary().
log_repair_failed(M, Why) ->
    ?ERROR_LOG([payment_reconcile, repair_failed, maps:get(trade_no, M, <<>>), Why]),
    <<"failed">>.

%% ===================================================================
%% Internal
%% ===================================================================

-spec reconcile_row(map(), {non_neg_integer(), [map()]}) ->
    {non_neg_integer(), [map()]}.
reconcile_row(Tx, {OkCnt, Acc}) ->
    Status = to_int(maps:get(<<"status">>, Tx, 0)),
    case Status of
        ?TX_SUCCESS ->
            check_business_consistency(Tx, OkCnt, Acc);
        _ ->
            %% 非成功流水不参与业务一致性核对，记为「一致」
            {OkCnt + 1, Acc}
    end.

-spec check_business_consistency(map(), non_neg_integer(), [map()]) ->
    {non_neg_integer(), [map()]}.
check_business_consistency(Tx, OkCnt, Acc) ->
    BizType = to_int(maps:get(<<"biz_type">>, Tx, 0)),
    BizOrderNo = maps:get(<<"biz_order_no">>, Tx, <<>>),
    case business_order_status(BizType, BizOrderNo) of
        {ok, 1} ->
            {OkCnt + 1, Acc};
        {ok, OtherStatus} ->
            {OkCnt, [mismatch(Tx, {status_mismatch, OtherStatus}) | Acc]};
        {error, not_found} ->
            {OkCnt, [mismatch(Tx, order_missing) | Acc]};
        {error, Reason} ->
            {OkCnt, [mismatch(Tx, {query_error, Reason}) | Acc]}
    end.

%% @doc 查询业务订单当前状态
-spec business_order_status(integer(), binary()) ->
    {ok, integer()} | {error, not_found | term()}.
business_order_status(?BIZ_RECHARGE, OrderNo) ->
    Tb = elib_pg_sql:public_tablename(<<"recharge_order">>),
    query_status(Tb, OrderNo);
business_order_status(?BIZ_CHANNEL_ORDER, OrderNo) ->
    query_status(<<"channel_order">>, OrderNo);
business_order_status(_, _) ->
    {error, not_found}.

-spec query_status(binary(), binary()) -> {ok, integer()} | {error, not_found | term()}.
query_status(Table, OrderNo) ->
    Sql = <<"SELECT status FROM ", Table/binary, " WHERE order_no = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [OrderNo]) of
        {ok, [#{<<"status">> := S} | _]} -> {ok, to_int(S)};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% user_id / gateway_payment_no 是 B-06 补单所必需（credit_recharge 要 user_id+amount，
%% credit_channel_order 要 gateway+gateway_payment_no 落 payment_no），不是给人看的字段。
-spec mismatch(map(), term()) -> map().
mismatch(Tx, Reason) ->
    #{
        trade_no => maps:get(<<"trade_no">>, Tx, <<>>),
        biz_type => maps:get(<<"biz_type">>, Tx, 0),
        biz_order_no => maps:get(<<"biz_order_no">>, Tx, <<>>),
        gateway => maps:get(<<"gateway">>, Tx, <<>>),
        gateway_payment_no => maps:get(<<"gateway_payment_no">>, Tx, <<>>),
        user_id => maps:get(<<"user_id">>, Tx, 0),
        amount => maps:get(<<"amount">>, Tx, 0),
        reason => elib_cnv:safe_to_binary(Reason)
    }.

-spec to_int(term()) -> integer().
to_int(V) when is_integer(V) -> V;
to_int(V) when is_binary(V) ->
    case catch binary_to_integer(V) of
        I when is_integer(I) -> I;
        _ -> 0
    end;
to_int(_) ->
    0.
