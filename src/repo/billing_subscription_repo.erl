-module(billing_subscription_repo).
%%%
% billing_subscription 订阅数据仓库层 / SaaS subscription repository
%
% 职责：封装订阅表 billing_subscription 的 PostgreSQL 操作。
%   - tenant_id 为逻辑字段（单租户=0 或站点 id），无多租户实体表。
%   - 唯一约束 uniq_billing_sub_active：同租户同时只允许一条「试用/生效」订阅。
%
% status: 0=试用 / 1=生效 / 2=过期 / 3=取消
%
% 所有 SQL 经 elib_pg 参数化；TSID 经 elib_tsid:generate(billing_subscription)。
%%%

-export([tablename/0]).
-export([create/1]).
-export([find_by_id/1]).
-export([find_active_by_tenant/1]).
-export([update/2]).
-export([renew/3]).
-export([list_expiring/1]).
-export([page/4]).

-include("log.hrl").

-define(STATUS_TRIAL, 0).
-define(STATUS_ACTIVE, 1).
-define(STATUS_EXPIRED, 2).
-define(STATUS_CANCELLED, 3).

-define(COLUMNS,
    <<"id, tenant_id, plan_id, status, current_period_start, current_period_end, ",
        "auto_renew, created_at, updated_at">>
).

%% @doc 表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"billing_subscription">>).

%% @doc 创建订阅
%% @param Data map，须含 tenant_id, plan_id；可选 status, current_period_start/end, auto_renew
%% @return {ok, Id} | {error, duplicate | term()}
%%   duplicate 表示该租户已有生效/试用订阅（命中 uniq_billing_sub_active）
-spec create(map()) -> {ok, integer()} | {error, duplicate | term()}.
create(Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(billing_subscription),
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
            ?WARN_LOG([billing_sub_create, failed, Reason]),
            {error, Reason}
    end.

%% @doc 按 id 查询订阅
-spec find_by_id(integer()) -> map().
find_by_id(Id) ->
    Tb = tablename(),
    Sql = <<"SELECT ", ?COLUMNS/binary, " FROM ", Tb/binary, " WHERE id = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [Id]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc 查询租户当前生效/试用的订阅（最多一条）
-spec find_active_by_tenant(integer()) -> map().
find_active_by_tenant(TenantId) ->
    Tb = tablename(),
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", Tb/binary,
            " WHERE tenant_id = $1 AND status IN ($2, $3) ", " ORDER BY id DESC LIMIT 1">>,
    case elib_pg:query(Sql, [TenantId, ?STATUS_TRIAL, ?STATUS_ACTIVE]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc 更新订阅（部分字段）
-spec update(integer(), map()) -> {ok, non_neg_integer()} | {error, term()}.
update(Id, ChangeMap) when map_size(ChangeMap) > 0 ->
    Tb = tablename(),
    ChangeMap2 = ChangeMap#{<<"updated_at">> => elib_dt:now()},
    case elib_pg:update(Tb, ChangeMap2, <<"id = $1">>, [Id]) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end;
update(_Id, _ChangeMap) ->
    {ok, 0}.

%% @doc 续费：推进计费周期并置为生效
%% @param Id 订阅 id
%% @param PeriodStart 新周期开始（毫秒时间戳，整数）
%% @param PeriodEnd 新周期结束（毫秒时间戳，整数）
%% @return {ok, Count} | {error, term()}
-spec renew(integer(), integer(), integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
renew(Id, PeriodStart, PeriodEnd) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary, " SET status = $1,",
            " current_period_start = to_timestamp($2::bigint/1000),",
            " current_period_end = to_timestamp($3::bigint/1000),", " updated_at = NOW()",
            " WHERE id = $4">>,
    case elib_pg:execute(Sql, [?STATUS_ACTIVE, PeriodStart, PeriodEnd, Id]) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 列出 BeforeTs（毫秒时间戳）前到期、且仍生效/试用的订阅（账单生成/过期扫描用）
-spec list_expiring(integer()) -> [map()].
list_expiring(BeforeTs) ->
    Tb = tablename(),
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", Tb/binary,
            " WHERE status IN ($1, $2) AND current_period_end IS NOT NULL",
            " AND current_period_end <= to_timestamp($3::bigint/1000)",
            " ORDER BY current_period_end ASC">>,
    case elib_pg:query(Sql, [?STATUS_TRIAL, ?STATUS_ACTIVE, BeforeTs]) of
        {ok, Rows} when is_list(Rows) -> Rows;
        _ -> []
    end.

%% @doc 分页查询（后台/租户列表用）
-spec page(map(), iodata(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(WhereMap, Order, Page, Size) ->
    Tb = tablename(),
    elib_pg:page_with_total(Tb, ?COLUMNS, WhereMap, Order, Page, Size).
