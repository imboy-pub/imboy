-module(billing_usage_repo).
%%%
% billing_usage 用量数据仓库层 / SaaS usage repository
%
% 职责：封装用量表 billing_usage 的 PostgreSQL 操作。
%   - 用量按 (subscription_id, metric, period) 累加，依赖唯一约束 upsert。
%   - metric 为自由文本指标键（message/storage/dau...）；used 单位由调用方定义。
%
% 所有 SQL 经 elib_pg 参数化；TSID 经 elib_tsid:generate(billing_usage)。
%%%

-export([tablename/0]).
-export([incr/4]).
-export([get_used/3]).
-export([list_by_period/2]).

-include("log.hrl").

-define(COLUMNS,
    <<"id, subscription_id, metric, used, period, recorded_at, updated_at">>
).

%% @doc 表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"billing_usage">>).

%% @doc 累加用量（upsert）：同 (subscription_id, metric, period) 已存在则 used += Delta
%% @param SubId 订阅 id
%% @param Metric 指标键 binary
%% @param Period 计费周期标识 binary（如 <<"2026-06">>）
%% @param Delta 增量（>=0 整数）
%% @return {ok, Used} 累加后的总用量 | {error, term()}
-spec incr(integer(), binary(), binary(), non_neg_integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
incr(SubId, Metric, Period, Delta) when is_integer(Delta), Delta >= 0 ->
    Tb = tablename(),
    Id = elib_tsid:generate(billing_usage),
    Sql =
        <<"INSERT INTO ", Tb/binary, " (id, subscription_id, metric, used, period, recorded_at)",
            " VALUES ($1, $2, $3, $4, $5, NOW())", " ON CONFLICT (subscription_id, metric, period)",
            " DO UPDATE SET used = billing_usage.used + EXCLUDED.used,", " updated_at = NOW()",
            " RETURNING used">>,
    case elib_pg:execute(Sql, [Id, SubId, Metric, Delta, Period]) of
        {ok, _Count, [{Used}]} ->
            {ok, Used};
        {ok, _Count} ->
            {ok, Delta};
        {error, Reason} ->
            ?WARN_LOG([billing_usage_incr, failed, Reason]),
            {error, Reason}
    end.

%% @doc 查询某订阅某指标在某周期的已用量，不存在则 0
-spec get_used(integer(), binary(), binary()) -> non_neg_integer().
get_used(SubId, Metric, Period) ->
    Tb = tablename(),
    Sql =
        <<"SELECT used FROM ", Tb/binary,
            " WHERE subscription_id = $1 AND metric = $2 AND period = $3 LIMIT 1">>,
    case elib_pg:query(Sql, [SubId, Metric, Period]) of
        {ok, [#{<<"used">> := Used} | _]} -> Used;
        _ -> 0
    end.

%% @doc 列出某订阅某周期的所有指标用量
-spec list_by_period(integer(), binary()) -> [map()].
list_by_period(SubId, Period) ->
    Tb = tablename(),
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", Tb/binary,
            " WHERE subscription_id = $1 AND period = $2 ORDER BY metric ASC">>,
    case elib_pg:query(Sql, [SubId, Period]) of
        {ok, Rows} when is_list(Rows) -> Rows;
        _ -> []
    end.
