-module(ops_report_repo).

%%%
%%% 运营周报数据仓库 / Ops weekly-report repository（P0-3 A3-4）
%%%
%%% 提供「上周」运营统计的只读查询：新增用户、活跃用户、消息总量、举报数、
%%% 举报 Top 原因。全部参数化 SQL，按 created_at 时间范围聚合。
%%%
%%% 设计约束：
%%%   - 纯读，无副作用；查询失败返回 0/[]（不阻断周报生成）。
%%%   - msg_store 是 TimescaleDB hypertable，时间范围查询高效。
%%%   - 不碰 E2EE 密文：仅统计行数/聚合 reason，不解码 payload。
%%%

-export([
    count_new_users/2,
    count_active_users/2,
    count_messages/2,
    count_reports/2,
    top_report_reasons/3
]).

-include("log.hrl").

%% @doc 指定时间范围内新增的用户数（status >= 0，排除已注销/禁用）
-spec count_new_users(term(), term()) -> integer().
count_new_users(Since, Until) ->
    UserTb = elib_pg_sql:public_tablename(<<"user">>),
    Sql =
        <<"SELECT count(*) AS n FROM ", UserTb/binary,
            " WHERE created_at >= $1 AND created_at < $2 AND status >= 0">>,
    count_query(Sql, [Since, Until]).

%% @doc 指定时间范围内的活跃用户数（按设备 last_active_at 去重）
-spec count_active_users(term(), term()) -> integer().
count_active_users(Since, Until) ->
    Sql =
        <<"SELECT count(DISTINCT user_id) AS n FROM user_device ",
            "WHERE last_active_at >= $1 AND last_active_at < $2">>,
    count_query(Sql, [Since, Until]).

%% @doc 指定时间范围内的消息总量（c2c + c2g，hypertable 范围查）
-spec count_messages(term(), term()) -> integer().
count_messages(Since, Until) ->
    Sql =
        <<"SELECT count(*) AS n FROM msg_store WHERE created_at >= $1 AND created_at < $2">>,
    count_query(Sql, [Since, Until]).

%% @doc 指定时间范围内的举报工单数
-spec count_reports(term(), term()) -> integer().
count_reports(Since, Until) ->
    Sql =
        <<"SELECT count(*) AS n FROM report_ticket WHERE created_at >= $1 AND created_at < $2">>,
    count_query(Sql, [Since, Until]).

%% @doc 指定时间范围内举报原因 Top N（reason, count），仅非空 reason
-spec top_report_reasons(
    term(), term(), pos_integer()
) -> [{binary(), integer()}].
top_report_reasons(Since, Until, Limit) ->
    Sql =
        <<"SELECT reason, count(*) AS n FROM report_ticket ",
            "WHERE created_at >= $1 AND created_at < $2 AND reason <> '' ",
            "GROUP BY reason ORDER BY n DESC LIMIT $3">>,
    case elib_pg:query(Sql, [Since, Until, Limit]) of
        {ok, Rows} when is_list(Rows) ->
            [
                {maps:get(<<"reason">>, R, <<>>), maps:get(<<"n">>, R, 0)}
             || R <- Rows
            ];
        _ ->
            []
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec count_query(binary(), [term()]) -> integer().
count_query(Sql, Args) ->
    case elib_pg:query(Sql, Args) of
        {ok, [#{<<"n">> := N}]} when is_integer(N) ->
            N;
        {ok, []} ->
            0;
        {error, Reason} ->
            ?WARN_LOG("[OPS_REPORT] count query failed ~p sql=~p~n", [Reason, Sql]),
            0
    end.
