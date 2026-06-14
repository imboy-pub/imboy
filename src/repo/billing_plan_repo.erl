-module(billing_plan_repo).
%%%
% billing_plan 套餐数据仓库层 / SaaS billing plan repository
%
% 职责：封装套餐表 billing_plan 的 PostgreSQL 操作（CRUD + 查询）。
%   - code UNIQUE 防重复套餐编码；quota_config 为 jsonb 各项配额上限。
%   - 金额（price）单位「分」(bigint)。
%
% status: 0=下架 / 1=上架
%
% 所有 SQL 经 elib_pg 参数化；TSID 经 elib_tsid:generate(billing_plan)。
%%%

-export([tablename/0]).
-export([create/1]).
-export([find_by_id/1]).
-export([find_by_code/1]).
-export([update/2]).
-export([list_active/0]).
-export([page/4]).

-include("log.hrl").

-define(STATUS_OFFLINE, 0).
-define(STATUS_ONLINE, 1).

-define(COLUMNS,
    <<"id, code, name, price, billing_period, quota_config, description, status, created_at, updated_at">>
).

%% @doc 表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"billing_plan">>).

%% @doc 创建套餐
%% @param Data map，须含 code, name, price, billing_period；可选 quota_config, description, status
%% @return {ok, Id} | {error, duplicate | term()}（code 命中唯一约束返回 duplicate）
-spec create(map()) -> {ok, integer()} | {error, duplicate | term()}.
create(Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(billing_plan),
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
            ?WARN_LOG([billing_plan_create, failed, Reason]),
            {error, Reason}
    end.

%% @doc 按 id 查询套餐
-spec find_by_id(integer()) -> map().
find_by_id(Id) ->
    Tb = tablename(),
    Sql = <<"SELECT ", ?COLUMNS/binary, " FROM ", Tb/binary, " WHERE id = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [Id]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc 按 code 查询套餐
-spec find_by_code(binary()) -> map().
find_by_code(Code) ->
    Tb = tablename(),
    Sql = <<"SELECT ", ?COLUMNS/binary, " FROM ", Tb/binary, " WHERE code = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [Code]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc 更新套餐（部分字段），ChangeMap 为待更新列 -> 值
%% @return {ok, Count} | {error, term()}
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

%% @doc 列出全部上架套餐
-spec list_active() -> [map()].
list_active() ->
    Tb = tablename(),
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", Tb/binary,
            " WHERE status = $1 ORDER BY price ASC">>,
    case elib_pg:query(Sql, [?STATUS_ONLINE]) of
        {ok, Rows} when is_list(Rows) -> Rows;
        _ -> []
    end.

%% @doc 分页查询（后台管理用）
-spec page(map(), iodata(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(WhereMap, Order, Page, Size) ->
    Tb = tablename(),
    elib_pg:page_with_total(Tb, ?COLUMNS, WhereMap, Order, Page, Size).
