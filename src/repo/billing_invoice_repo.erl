-module(billing_invoice_repo).
%%%
% billing_invoice 账单数据仓库层 / SaaS invoice repository
%
% 职责：封装账单表 billing_invoice 的 PostgreSQL 操作。
%   - invoice_no UNIQUE；(subscription_id, period_start, period_end) UNIQUE 保证
%     同订阅同周期只生成一张账单（账单生成幂等）。
%   - 金额 amount 单位「分」(bigint)。账单支付复用统一支付通道（biz_type=3）。
%
% status: 0=待支付 / 1=已付 / 2=逾期
%
% 所有 SQL 经 elib_pg 参数化；TSID 经 elib_tsid:generate(billing_invoice)。
%%%

-export([tablename/0]).
-export([create/1]).
-export([find_by_id/1]).
-export([find_by_invoice_no/1]).
-export([mark_paid/2]).
-export([mark_overdue/1]).
-export([list_by_subscription/1]).
-export([page/4]).

-include("log.hrl").

-define(STATUS_UNPAID, 0).
-define(STATUS_PAID, 1).
-define(STATUS_OVERDUE, 2).

-define(COLUMNS,
    <<"id, invoice_no, subscription_id, amount, currency, status, payment_no, ",
        "period_start, period_end, paid_at, created_at, updated_at">>
).

%% @doc 表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"billing_invoice">>).

%% @doc 创建账单
%% @param Data map，须含 invoice_no, subscription_id, amount；
%%        period_start/period_end 为毫秒时间戳整数（经 to_timestamp 落库）
%% @return {ok, Id} | {error, duplicate | term()}
%%   duplicate：invoice_no 或 (sub, period) 命中唯一约束（账单已存在 → 幂等信号）
-spec create(map()) -> {ok, integer()} | {error, duplicate | term()}.
create(Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(billing_invoice),
    InvoiceNo = maps:get(invoice_no, Data),
    SubId = maps:get(subscription_id, Data),
    Amount = maps:get(amount, Data),
    Currency = maps:get(currency, Data, <<"CNY">>),
    PeriodStart = maps:get(period_start, Data),
    PeriodEnd = maps:get(period_end, Data),
    Sql =
        <<"INSERT INTO ", Tb/binary, " (id, invoice_no, subscription_id, amount, currency, status,",
            " period_start, period_end, created_at)", " VALUES ($1, $2, $3, $4, $5, $6,",
            " to_timestamp($7::bigint/1000), to_timestamp($8::bigint/1000), NOW())",
            " RETURNING id">>,
    case
        elib_pg:execute(Sql, [
            Id, InvoiceNo, SubId, Amount, Currency, ?STATUS_UNPAID, PeriodStart, PeriodEnd
        ])
    of
        {ok, 1, [{Id}]} ->
            {ok, Id};
        {error, {pgsql_error, #{code := <<"23505">>}}} ->
            {error, duplicate};
        {error, {error, error, _, unique_violation, _, _}} ->
            {error, duplicate};
        {error, Reason} ->
            ?WARN_LOG([billing_invoice_create, failed, Reason]),
            {error, Reason}
    end.

%% @doc 按 id 查询账单
-spec find_by_id(integer()) -> map().
find_by_id(Id) ->
    Tb = tablename(),
    Sql = <<"SELECT ", ?COLUMNS/binary, " FROM ", Tb/binary, " WHERE id = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [Id]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc 按 invoice_no 查询账单
-spec find_by_invoice_no(binary()) -> map().
find_by_invoice_no(InvoiceNo) ->
    Tb = tablename(),
    Sql = <<"SELECT ", ?COLUMNS/binary, " FROM ", Tb/binary, " WHERE invoice_no = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [InvoiceNo]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc 标记账单已支付（条件更新：仅待支付/逾期 → 已付；幂等保护）
%% @param InvoiceNo 账单号
%% @param PaymentNo 支付单号（统一支付 trade_no 或网关单号）
%% @return ok | {error, not_found_or_paid | term()}
-spec mark_paid(binary(), binary()) -> ok | {error, not_found_or_paid | term()}.
mark_paid(InvoiceNo, PaymentNo) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = $1, payment_no = $2, paid_at = NOW(), updated_at = NOW()",
            " WHERE invoice_no = $3 AND status IN ($4, $5)">>,
    case
        elib_pg:execute(Sql, [
            ?STATUS_PAID, PaymentNo, InvoiceNo, ?STATUS_UNPAID, ?STATUS_OVERDUE
        ])
    of
        {ok, 0} -> {error, not_found_or_paid};
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 标记账单逾期（仅待支付 → 逾期）
-spec mark_overdue(binary()) -> ok | {error, term()}.
mark_overdue(InvoiceNo) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary, " SET status = $1, updated_at = NOW()",
            " WHERE invoice_no = $2 AND status = $3">>,
    case elib_pg:execute(Sql, [?STATUS_OVERDUE, InvoiceNo, ?STATUS_UNPAID]) of
        {ok, 0} -> {error, not_found_or_paid};
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 列出某订阅的全部账单（倒序）
-spec list_by_subscription(integer()) -> [map()].
list_by_subscription(SubId) ->
    Tb = tablename(),
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", Tb/binary,
            " WHERE subscription_id = $1 ORDER BY id DESC">>,
    case elib_pg:query(Sql, [SubId]) of
        {ok, Rows} when is_list(Rows) -> Rows;
        _ -> []
    end.

%% @doc 分页查询（后台/租户账单列表）
-spec page(map(), iodata(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(WhereMap, Order, Page, Size) ->
    Tb = tablename(),
    elib_pg:page_with_total(Tb, ?COLUMNS, WhereMap, Order, Page, Size).
