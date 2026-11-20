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
-export([mark_completed_tx/3]).
-export([lock_execution_tx/5]).
-export([release_ineligible/2]).
-export([mark_failed/3]).
-export([find_by_user/1]).
-export([read_status/1]).

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
        <<
            "WITH candidate AS ("
            " SELECT j.id, r.updated_at AS request_version FROM ",
            Tb/binary,
            " j JOIN ",
            ReqTb/binary,
            " r ON r.user_id = j.user_id"
            " JOIN ",
            UserTb/binary,
            " u ON u.id = j.user_id"
            " WHERE j.status = 'pending'"
            " AND ((r.status = 'requested' AND u.status = 2)"
            "      OR (r.status = 'approved' AND u.status = -1))"
            " AND r.requested_at <= NOW() - ($2 || ' days')::INTERVAL"
            " AND NOT EXISTS (SELECT 1 FROM public.wallet w"
            "      WHERE w.user_id = j.user_id AND w.balance <> 0)"
            " ORDER BY r.requested_at ASC"
            " FOR UPDATE OF j SKIP LOCKED LIMIT 1)"
            " UPDATE ",
            Tb/binary,
            " j SET status = 'running', claimed_by = $1,"
            " claimed_at = CURRENT_TIMESTAMP, attempts = j.attempts + 1,"
            " updated_at = CURRENT_TIMESTAMP FROM candidate c WHERE j.id = c.id"
            " RETURNING j.id, j.user_id, j.attempts, c.request_version::text AS request_version"
        >>,
    case elib_pg:query(Sql, [ClaimedBy, GraceDays]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {ok, none};
        {error, Reason} -> {error, Reason}
    end.

%% 审批与撤销也先锁 user，再访问 request 和 job。
-spec lock_execution_tx(pid(), integer(), integer(), map(), pos_integer()) ->
    {ok, map()} | {error, term()}.
lock_execution_tx(Conn, Uid, JobId, Job, GraceDays) ->
    User = user_repo:tablename(),
    UserSql = <<"SELECT status FROM ", User/binary, " WHERE id = $1 FOR UPDATE">>,
    case elib_pg:query(Conn, UserSql, [Uid]) of
        {ok, [#{<<"status">> := UserStatus}]} when UserStatus =:= 2; UserStatus =:= -1 ->
            lock_request_tx(Conn, Uid, JobId, Job, GraceDays, UserStatus);
        {ok, _} ->
            {error, request_no_longer_eligible};
        {error, Reason} ->
            {error, Reason}
    end.

lock_request_tx(Conn, Uid, JobId, Job, GraceDays, UserStatus) ->
    Request = user_deletion_request_repo:tablename(),
    RequestSql =
        <<"SELECT status, approved_at::text AS approved_at FROM ", Request/binary,
            " WHERE user_id = $1 AND status = $2"
            " AND updated_at = $3::text::timestamptz"
            " AND requested_at <= NOW() - ($4 || ' days')::INTERVAL FOR UPDATE">>,
    RequestStatus =
        case UserStatus of
            2 -> <<"requested">>;
            -1 -> <<"approved">>
        end,
    Version = maps:get(<<"request_version">>, Job),
    case elib_pg:query(Conn, RequestSql, [Uid, RequestStatus, Version, GraceDays]) of
        {ok, [Snapshot]} ->
            case lock_claim_tx(Conn, Uid, JobId, maps:get(<<"attempts">>, Job)) of
                ok -> {ok, Snapshot};
                {error, Reason} -> {error, Reason}
            end;
        {ok, []} ->
            {error, request_no_longer_eligible};
        {error, Reason} ->
            {error, Reason}
    end.

lock_claim_tx(Conn, Uid, JobId, Attempts) ->
    Tb = tablename(),
    Sql =
        <<"SELECT id FROM ", Tb/binary,
            " WHERE id = $1 AND user_id = $2 AND attempts = $3 AND status = 'running' FOR UPDATE">>,
    case elib_pg:query(Conn, Sql, [JobId, Uid, Attempts]) of
        {ok, [_]} -> ok;
        {ok, []} -> {error, job_not_running};
        {error, Reason} -> {error, Reason}
    end.

-spec release_ineligible(integer(), pos_integer()) -> ok | {error, term()}.
release_ineligible(JobId, Attempts) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = 'pending', attempts = attempts - 1, claimed_by = NULL, claimed_at = NULL,"
            " updated_at = CURRENT_TIMESTAMP"
            " WHERE id = $1 AND attempts = $2 AND status = 'running'">>,
    case elib_pg:execute_count(Sql, [JobId, Attempts]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 在注销事务中将已认领任务标为完成。
-spec mark_completed_tx(pid(), integer(), map()) -> ok | {error, term()}.
mark_completed_tx(Conn, JobId, #{<<"status">> := RequestStatus, <<"approved_at">> := ApprovedAt}) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = 'completed',"
            "     last_error = NULL,"
            "     request_status_at_execution = $2,"
            "     request_approved_at = $3::text::timestamptz,"
            "     finished_at = CURRENT_TIMESTAMP,"
            "     updated_at = CURRENT_TIMESTAMP"
            " WHERE id = $1 AND status = 'running'"
            " RETURNING id">>,
    case elib_pg:query(Conn, Sql, [JobId, RequestStatus, ApprovedAt]) of
        {ok, [_]} -> ok;
        {ok, []} -> {error, job_not_running};
        {error, Reason} -> {error, Reason}
    end.

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
            " WHERE id = $1 AND status = 'running'"
            " RETURNING status">>,
    case elib_pg:query(Sql, [JobId, MaxAttempts, Error]) of
        {ok, [#{<<"status">> := <<"pending">>}]} -> {ok, pending};
        {ok, [#{<<"status">> := <<"failed">>}]} -> {ok, failed};
        {ok, []} -> {error, job_not_running};
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

%% @doc 读取同一数据库语句快照中的申请与执行状态。
-spec read_status(integer()) -> {ok, map()} | {error, term()}.
read_status(Uid) ->
    Request = user_deletion_request_repo:tablename(),
    Job = tablename(),
    Sql =
        <<
            "SELECT COALESCE(r.status, 'not_requested') AS status, r.requested_at,"
            " COALESCE(j.status, 'not_started') AS execution_status,"
            " j.request_status_at_execution, j.request_approved_at,"
            " CASE WHEN j.status IN ('completed', 'failed') THEN j.finished_at END AS execution_finished_at"
            " FROM (VALUES ($1::bigint)) AS target(uid)"
            " LEFT JOIN ",
            Request/binary,
            " r ON r.user_id = target.uid"
            " LEFT JOIN ",
            Job/binary,
            " j ON j.user_id = target.uid"
        >>,
    case elib_pg:query(Sql, [Uid]) of
        {ok, [Row]} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.
