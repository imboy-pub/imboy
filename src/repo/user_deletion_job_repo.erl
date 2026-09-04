-module(user_deletion_job_repo).
%%%
% user_deletion_job_repo —— 账号注销删除任务/墓碑仓库层
% （迁移 00000086，Implementation Plan Task D-03）
%
% 状态机：pending → running → completed
%              ↖ (attempts < max) ↙↘ (attempts ≥ max → failed 终态)
% 认领：FOR UPDATE SKIP LOCKED —— 多 worker 并发认领互不重复。
% 墓碑：job 行无 user FK，用户主行删除后幸存（account 快照留证）。
%%%

-export([tablename/0]).
-export([ensure_job_tx/3]).
-export([claim_pending_expired/2]).
-export([mark_completed/1]).
-export([mark_failed/3]).
-export([find_by_user/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"user_deletion_job">>).

%% @doc 事务内确保存在 pending 任务（幂等：已有任务则 no-op）
%% @param Conn 数据库连接
%% @param Uid 用户ID
%% @param Account 注销时的账号快照（墓碑用）
%% @return {ok, created | exists} | {error, term()}
-spec ensure_job_tx(pid(), integer(), binary()) -> {ok, created | exists} | {error, term()}.
ensure_job_tx(Conn, Uid, Account) ->
    Tb = tablename(),
    Id = elib_tsid:generate(user_deletion_job),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, user_id, account, status)"
            " VALUES ($1, $2, $3, 'pending')"
            " ON CONFLICT (user_id) DO NOTHING"
            " RETURNING id">>,
    case elib_pg:query(Conn, Sql, [Id, Uid, Account]) of
        {ok, [_]} -> {ok, created};
        {ok, []} -> {ok, exists};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 原子认领一个"宽限期已满的 pending 任务"
%% FOR UPDATE SKIP LOCKED：并发 worker 各得其一，互不重复执行。
%% @param GraceDays 宽限期天数
%% @param ClaimedBy 认领者标识（node/worker）
%% @return {ok, map()} | {ok, none} | {error, term()}
-spec claim_pending_expired(pos_integer(), binary()) -> {ok, map() | none} | {error, term()}.
claim_pending_expired(GraceDays, ClaimedBy) ->
    Tb = tablename(),
    ReqTb = user_deletion_request_repo:tablename(),
    UserTb = user_repo:tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " j"
            " SET status = 'running',"
            "     claimed_by = $1,"
            "     claimed_at = CURRENT_TIMESTAMP,"
            "     attempts = j.attempts + 1,"
            "     updated_at = CURRENT_TIMESTAMP"
            " WHERE j.id = ("
            "   SELECT j2.id FROM ", Tb/binary,
            " j2"
            "   JOIN ", ReqTb/binary,
            " r ON r.user_id = j2.user_id"
            "   JOIN ", UserTb/binary,
            " u ON u.id = j2.user_id"
            "   WHERE j2.status = 'pending'"
            "     AND r.status = 'requested'"
            "     AND u.status = 2"
            "     AND r.requested_at <= NOW() - ($2 || ' days')::INTERVAL"
            "     AND NOT EXISTS (SELECT 1 FROM public.wallet w"
            "          WHERE w.user_id = j2.user_id AND w.balance <> 0)"
            "   ORDER BY r.requested_at ASC"
            "   FOR UPDATE SKIP LOCKED"
            "   LIMIT 1)"
            " RETURNING j.id, j.user_id, j.attempts">>,
    case elib_pg:query(Sql, [ClaimedBy, GraceDays]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {ok, none};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 标记完成（终态）
%% @return {ok, non_neg_integer()} | {error, term()}
-spec mark_completed(integer()) -> ok.
mark_completed(JobId) ->
    ok = update_status(JobId, <<"completed">>).

%% @doc 标记失败：attempts < MaxAttempts 时回 pending 供重试，否则 failed 终态
%% @return {ok, pending | failed} | {error, term()}
-spec mark_failed(integer(), binary(), pos_integer()) -> {ok, pending | failed} | {error, term()}.
mark_failed(JobId, Error, MaxAttempts) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = CASE WHEN attempts >= $2 THEN 'failed' ELSE 'pending' END,"
            "     last_error = $3,"
            "     finished_at = CASE WHEN attempts >= $2 THEN CURRENT_TIMESTAMP END,"
            "     updated_at = CURRENT_TIMESTAMP"
            " WHERE id = $1"
            " RETURNING status">>,
    case elib_pg:query(Sql, [JobId, MaxAttempts, Error]) of
        {ok, [#{<<"status">> := Status}]} -> {ok, binary_to_atom(Status, utf8)};
        {ok, []} -> {error, job_not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 读某用户的任务行（墓碑查询）
%% @return {ok, map()} | {ok, undefined} | {error, term()}
-spec find_by_user(integer()) -> {ok, map() | undefined} | {error, term()}.
find_by_user(Uid) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT status, attempts, last_error, account, finished_at"
            " FROM ",
            Tb/binary,
            " WHERE user_id = $1 LIMIT 1"
        >>,
    case elib_pg:query(Sql, [Uid]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

update_status(JobId, Status) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = $2,"
            "     finished_at = CURRENT_TIMESTAMP,"
            "     updated_at = CURRENT_TIMESTAMP"
            " WHERE id = $1">>,
    %% 查询/写入均 fire-and-forget：调用方只关心已完成标记是否落库
    _ = elib_pg:query(Sql, [JobId, Status]),
    ok.
