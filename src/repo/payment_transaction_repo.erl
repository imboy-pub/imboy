-module(payment_transaction_repo).
%%%
% payment_transaction 数据仓库层 / Unified payment transaction repository
%
% 职责：封装统一支付流水表 payment_transaction 的 PostgreSQL 操作。
%   - 每一笔第三方支付的权威记录，用于「对账」与「回调幂等」。
%   - 幂等强约束：UNIQUE(trade_no) + UNIQUE(gateway, gateway_payment_no) partial。
%
% biz_type: 1=充值 recharge / 2=频道订单 channel_order / 3=SaaS 账单 billing
% status:   0=待支付 / 1=成功 / 2=失败 / 3=已退款 / 4=部分退款
%
% 所有 SQL 经 elib_pg 参数化；TSID 经 elib_tsid:generate(payment_transaction)。
%%%

-export([tablename/0]).
-export([create/1]).
-export([find_by_trade_no/1]).
-export([find_by_gateway_no/2]).
-export([update_status/3]).
-export([page/5]).
-export([reconcile_list/3]).

-include("log.hrl").

%% ===================================================================
%% Constants
%% ===================================================================

-define(STATUS_PENDING, 0).
-define(STATUS_SUCCESS, 1).
-define(STATUS_FAILED, 2).
-define(STATUS_REFUNDED, 3).
-define(STATUS_PART_REFUNDED, 4).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"payment_transaction">>).

%% @doc 创建支付流水
%% @param Data map，须含 trade_no, biz_type, biz_order_no, user_id, gateway, amount；
%%        可选 gateway_payment_no, currency, status, notify_data, paid_at
%% @return {ok, Id} | {error, term()}
%%   gateway_payment_no 命中唯一约束（23505）时返回 {error, duplicate}（回调幂等信号）
-spec create(map()) -> {ok, integer()} | {error, duplicate | term()}.
create(Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(payment_transaction),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Sql, Params) of
        {ok, _Count} ->
            {ok, Id};
        {error, {pgsql_error, #{code := <<"23505">>}}} ->
            {error, duplicate};
        {error, {error, error, _, unique_violation, _, _}} ->
            {error, duplicate};
        {error, Reason} ->
            ?WARN_LOG([payment_tx_create, failed, Reason]),
            {error, Reason}
    end.

%% @doc 按 trade_no（内部交易号）查询流水
-spec find_by_trade_no(binary()) -> map().
find_by_trade_no(TradeNo) ->
    Tb = tablename(),
    Sql =
        <<"SELECT id, trade_no, biz_type, biz_order_no, user_id, gateway, ",
            "gateway_payment_no, amount, currency, status, paid_at, created_at FROM ", Tb/binary,
            " WHERE trade_no = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [TradeNo]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc 按 (gateway, gateway_payment_no) 查询流水 —— 回调幂等的主路径
-spec find_by_gateway_no(binary(), binary()) -> map().
find_by_gateway_no(Gateway, GatewayPaymentNo) ->
    Tb = tablename(),
    Sql =
        <<"SELECT id, trade_no, biz_type, biz_order_no, user_id, gateway, ",
            "gateway_payment_no, amount, currency, status, paid_at, created_at FROM ", Tb/binary,
            " WHERE gateway = $1 AND gateway_payment_no = $2 LIMIT 1">>,
    case elib_pg:query(Sql, [Gateway, GatewayPaymentNo]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc 更新流水状态（成功时落 gateway_payment_no / notify_data / paid_at）
%% @param TradeNo 内部交易号
%% @param Status 目标状态（0..4）
%% @param Extra map，可选 gateway_payment_no, notify_data, paid_at
%% @return {ok, Count} | {error, term()}
-spec update_status(binary(), integer(), map()) ->
    {ok, non_neg_integer()} | {error, term()}.
update_status(TradeNo, Status, Extra) ->
    Tb = tablename(),
    GwNo = maps:get(<<"gateway_payment_no">>, Extra, undefined),
    NotifyData = maps:get(<<"notify_data">>, Extra, undefined),
    PaidNow = Status =:= ?STATUS_SUCCESS,
    %% COALESCE 保证 gateway_payment_no/notify_data 已有值时不被空覆盖
    Sql =
        <<"UPDATE ", Tb/binary, " SET status = $1,",
            " gateway_payment_no = COALESCE($2, gateway_payment_no),",
            " notify_data = COALESCE($3::jsonb, notify_data),",
            " paid_at = CASE WHEN $4 THEN NOW() ELSE paid_at END,", " updated_at = NOW()",
            " WHERE trade_no = $5">>,
    case elib_pg:execute(Sql, [Status, GwNo, NotifyData, PaidNow, TradeNo]) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 分页查询（对账/后台用），WhereMap 由调用方组装
-spec page(binary(), map(), iodata(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(Column, WhereMap, Order, Page, Size) ->
    Tb = tablename(),
    elib_pg:page_with_total(Tb, Column, WhereMap, Order, Page, Size).

%% @doc 对账查询：按时间段 + 可选状态拉取流水（用于与业务订单核对）
%% @param FromTs/ToTs RFC3339 binary 时间边界（created_at 闭区间）
%% @param Status 整数状态 | all（不限状态）
%% @return {ok, [map()]} | {error, term()}
-spec reconcile_list(binary(), binary(), integer() | all) ->
    {ok, [map()]} | {error, term()}.
reconcile_list(FromTs, ToTs, Status) ->
    Tb = tablename(),
    Column =
        <<"id, trade_no, biz_type, biz_order_no, user_id, gateway, ",
            "gateway_payment_no, amount, currency, status, paid_at, created_at">>,
    case Status of
        all ->
            Sql =
                <<"SELECT ", Column/binary, " FROM ", Tb/binary,
                    " WHERE created_at >= $1 AND created_at <= $2", " ORDER BY id ASC">>,
            run_reconcile(Sql, [FromTs, ToTs]);
        S when is_integer(S) ->
            Sql =
                <<"SELECT ", Column/binary, " FROM ", Tb/binary,
                    " WHERE created_at >= $1 AND created_at <= $2 AND status = $3",
                    " ORDER BY id ASC">>,
            run_reconcile(Sql, [FromTs, ToTs, S])
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec run_reconcile(binary(), [term()]) -> {ok, [map()]} | {error, term()}.
run_reconcile(Sql, Params) ->
    case elib_pg:query(Sql, Params) of
        {ok, Rows} when is_list(Rows) -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.
