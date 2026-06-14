-module(payment_reconcile_logic).
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

-include("log.hrl").

-define(TX_SUCCESS, 1).
-define(BIZ_RECHARGE, 1).
-define(BIZ_CHANNEL_ORDER, 2).

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

-spec mismatch(map(), term()) -> map().
mismatch(Tx, Reason) ->
    #{
        trade_no => maps:get(<<"trade_no">>, Tx, <<>>),
        biz_type => maps:get(<<"biz_type">>, Tx, 0),
        biz_order_no => maps:get(<<"biz_order_no">>, Tx, <<>>),
        gateway => maps:get(<<"gateway">>, Tx, <<>>),
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
