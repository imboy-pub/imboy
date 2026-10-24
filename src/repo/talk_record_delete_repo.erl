-module(talk_record_delete_repo).
-moduledoc "talk_record 删除/清理主表仓储（T20：软删+撤权+清理状态推进；迁移 00000215）。".
%%%
% 删除闭环（BACKEND-API §4 DELETE /:id、§10.3 删除行映射、§10.4 治理与
% 删除状态、§11.3 删除后 owner 白名单）的仓储层。全部核心写路径为事务内
% 形态（Conn 直连），行锁序列化与 T17 授权域共用同一 talk_record 行的
% FOR UPDATE——PUT/DELETE grants、DELETE 记录、移交三者天然互斥。
%
%   * lock_for_delete_tx/3  —— 删除行锁：FOR UPDATE + record.revision CAS
%                              复核（§11.2：删除属 record.revision 域）。
%                              与 talk_record_repo:lock_for_acl_tx 同款
%                              "锁后复核 deleted_at/acl_revision" 模式。
%   * soft_delete_tx/2      —— deleted_at=now + cleanup_status=pending +
%                              revision/acl_revision 推进（202 响应载荷源）。
%   * revoke_all_grants_tx/2 —— 全量活跃授权撤销（立即撤权；acl_revision
%                              推进使在途票据 acl_stale，§11.3）。
%   * fence_active_capture_tx/2 —— 活跃采集 fence（interrupted + lease_epoch+1
%                              + active_capture_id 清空；旧设备后续写 409）。
%   * enqueue_purge_tx/2    —— purge job 投递（dedupe 幂等）。
%   * find_deleted_for_owner/2 —— 删除后 owner 白名单读取（仅清理状态字段，
%                              无标题/正文/参与者，§11.3）。
%   * update_cleanup_status_tx/3 —— 清理状态推进（单向：blocked_*/failed/
%                              complete 只在 pending/failed 上推进，不回退）。
%   * purged_attachment_paths_tx/2 —— 曾绑定附件键回读（物理状态投影 join 源）。
%%%

-export([
    lock_for_delete_tx/3,
    soft_delete_tx/2,
    revoke_all_grants_tx/2,
    fence_active_capture_tx/2,
    enqueue_purge_tx/3,
    find_deleted_for_owner/2,
    update_cleanup_status_tx/3,
    count_purge_jobs_tx/2,
    purged_attachment_paths_tx/2,
    write_blocker_tx/2,
    mark_write_pending_tx/2,
    cleanup_status_tx/2
]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(CLEANUP_COLUMNS,
    <<"tr.id, tr.owner_user_id, tr.scope, tr.deleted_at, tr.cleanup_status, tr.cleanup_generation,",
        " tr.retention_until, tr.revision, tr.acl_revision, tr.updated_at">>
).

%% ===================================================================
%% 删除事务（行锁 + 软删 + 撤权 + fence + purge 投递）
%% ===================================================================

%% @doc 删除行锁（§4 DELETE /:id：owner + If-Match 对 record.revision）。
%% FOR UPDATE 序列化并发 DELETE/PUT grants（同一行锁）；锁后复核：
%%   deleted_at 已置位 → {ok, deleted, undefined}（幂等重放分支，调用方返回
%%     首次结果形状——§3 幂等动作重放先返回首次结果再校验版本）；
%%   revision 不匹配 → precondition_failed（412）；
%%   行不存在 → not_found；非 owner 由调用方在锁前/锁后裁决。
-spec lock_for_delete_tx(any(), integer(), pos_integer()) ->
    {ok, live, map()}
    | {ok, deleted, undefined}
    | {error, not_found | precondition_failed | term()}.
lock_for_delete_tx(Conn, Id, ExpectedRevision) ->
    Sql =
        <<"SELECT ", ?CLEANUP_COLUMNS/binary, " FROM talk_record tr",
            " WHERE tr.id = $1 FOR UPDATE">>,
    case elib_pg:query(Conn, Sql, [Id]) of
        {ok, [#{<<"deleted_at">> := null, <<"revision">> := ExpectedRevision} = Row]} ->
            {ok, live, Row};
        {ok, [#{<<"deleted_at">> := null}]} ->
            {error, precondition_failed};
        {ok, [_Deleted]} ->
            %% 已软删：行存在；调用方重新读取并复核 owner 后返回幂等结果
            {ok, deleted, undefined};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 软删（§4：202 logical_deleted=true + cleanup_status=pending）。
%% revision+1（删除属 record.revision 域）+ acl_revision+1（撤权使在途
%% 票据/Range 校验立即失效，§11.3）。调用方须先 lock_for_delete_tx 持锁。
-spec soft_delete_tx(any(), integer()) -> {ok, map()} | {error, term()}.
soft_delete_tx(Conn, Id) ->
    Sql =
        <<"UPDATE talk_record tr SET deleted_at = now(), cleanup_status = 'pending',",
            " revision = revision + 1, acl_revision = acl_revision + 1, updated_at = now()",
            " WHERE tr.id = $1 AND tr.deleted_at IS NULL", " RETURNING ", ?CLEANUP_COLUMNS/binary>>,
    case elib_pg:query(Conn, Sql, [Id]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 全量撤销活跃授权（§4 DELETE 行"立即撤权"；§11.3 撤销版本推进）。
%% 已撤销行不动（幂等）。返回撤销行数。
-spec revoke_all_grants_tx(any(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
revoke_all_grants_tx(Conn, RecordId) ->
    Sql =
        <<"UPDATE talk_record_grant SET revoked_at = now(), updated_at = now()",
            " WHERE record_id = $1 AND revoked_at IS NULL RETURNING id">>,
    case elib_pg:query(Conn, Sql, [RecordId]) of
        {ok, Rows} -> {ok, length(Rows)};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 活跃采集 fence（§10.3 删除行"立即撤销业务读写"）：未 ended 的
%% capture 置 interrupted + lease_epoch+1（旧设备后续写 409 capture_fenced，
%% §11.1）；active_capture_id 投影清空。
-spec fence_active_capture_tx(any(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
fence_active_capture_tx(Conn, RecordId) ->
    case
        elib_pg:query(
            Conn,
            <<
                "UPDATE talk_record_capture SET status='interrupted',lease_epoch=lease_epoch+1,updated_at=now()"
                " WHERE record_id=$1 AND status IN ('draft','recording','paused')"
            >>,
            [RecordId]
        )
    of
        {ok, _} ->
            case
                elib_pg:query(
                    Conn,
                    <<"UPDATE talk_record SET active_capture_id=NULL,updated_at=now() WHERE id=$1">>,
                    [RecordId]
                )
            of
                {ok, _} -> {ok, 0};
                E -> E
            end;
        E ->
            E
    end.

%% @doc purge job 投递（dedupe_key 稳定：purge:<record_id>:<attempt_n>；
%% 同 attempt 重投幂等，§2 job 表 UNIQUE(dedupe_key)）。
-spec enqueue_purge_tx(any(), integer(), non_neg_integer()) ->
    {ok, created | existing, integer()} | {error, term()}.
enqueue_purge_tx(Conn, RecordId, Attempt) ->
    talk_record_job_repo:enqueue_tx(Conn, #{
        record_id => RecordId,
        kind => <<"purge">>,
        dedupe_key => purge_dedupe_key(RecordId, Attempt)
    }).

-spec purge_dedupe_key(integer(), non_neg_integer()) -> binary().
purge_dedupe_key(RecordId, Attempt) ->
    <<"purge:", (integer_to_binary(RecordId))/binary, ":", (integer_to_binary(Attempt))/binary>>.

%% @doc 该记录已存在的 purge job 计数（重试编号推导：新 attempt = 已有数）。
-spec count_purge_jobs_tx(any(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
count_purge_jobs_tx(Conn, RecordId) ->
    case
        elib_pg:query(
            Conn,
            <<"SELECT count(*) AS n FROM talk_record_job",
                " WHERE record_id = $1 AND kind = 'purge'">>,
            [RecordId]
        )
    of
        {ok, [#{<<"n">> := N}]} -> {ok, N};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% 删除后 owner 白名单（§11.3）
%% ===================================================================

%% @doc 删除后 owner 读取白名单行：仅 id/清理状态域/版本/删除时间——
%% 无标题、正文、参与者、授权名单（§11.3"GET 元数据仅 owner 可读清除
%% 状态、不能读正文"）。非 owner / 未删除 / 不存在 → not_found。
-spec find_deleted_for_owner(integer(), integer()) -> {ok, map()} | {error, not_found | term()}.
find_deleted_for_owner(Uid, Id) ->
    Sql =
        <<"SELECT ", ?CLEANUP_COLUMNS/binary, " FROM talk_record tr",
            " WHERE tr.id = $2 AND tr.deleted_at IS NOT NULL AND tr.owner_user_id = $1">>,
    case elib_pg:one(Sql, [Uid, Id]) of
        {ok, Row} when map_size(Row) > 0 -> {ok, Row};
        {ok, _} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% 清理状态推进（§10.4 状态拆分：cleanup_status 单向推进）
%% ===================================================================

%% @doc 清理状态推进：仅 pending → {blocked_*, retained_*, failed, complete}
%% retained 可因上传能力到期/其他引用释放重新 pending；blocked/complete 保持。
%% 所有推进在记录锁下协调；刷新时间用于有界轮询公平调度。返回状态（未变化时返回
%% 当前值 + unchanged 标记）。
-spec update_cleanup_status_tx(any(), integer(), binary()) ->
    {ok, binary()} | {error, term()}.
update_cleanup_status_tx(Conn, RecordId, NewStatus) ->
    Sql =
        <<"UPDATE talk_record SET cleanup_status = $2, updated_at = now()",
            " WHERE id = $1 AND deleted_at IS NOT NULL",
            " AND ((cleanup_status = 'pending')"
            "   OR (cleanup_status = 'failed' AND $2 IN ('failed','complete','pending'))"
            "   OR (cleanup_status = 'retained_by_other_reference' AND $2 IN"
            " ('pending','failed','complete','retained_by_other_reference','blocked_hold','blocked_retention')))",
            " RETURNING cleanup_status">>,
    case elib_pg:query(Conn, Sql, [RecordId, NewStatus]) of
        {ok, [#{<<"cleanup_status">> := S}]} ->
            {ok, S};
        {ok, []} ->
            %% 当前状态不接受该推进（如 blocked 保持）——读回现值
            cleanup_status_tx(Conn, RecordId);
        {error, Reason} ->
            {error, Reason}
    end.

-spec cleanup_status_tx(any(), integer()) -> {ok, binary()} | {error, term()}.
cleanup_status_tx(Conn, RecordId) ->
    case
        elib_pg:query(
            Conn, <<"SELECT cleanup_status FROM talk_record WHERE id = $1">>, [RecordId]
        )
    of
        {ok, [#{<<"cleanup_status">> := S}]} -> {ok, S};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% 物理状态投影支撑（§10.4：附件物理状态从既有队列回读）
%% ===================================================================

%% @doc 该记录全部曾绑定附件的键与资产/队列状态回读（released 引用行
%% join attachment + attachment_object_deletion 队列）。purge 协调完成后
%% 的 complete/failed 判定数据源——本域不执行存储 DELETE，物理进度
%% 全部以既有队列表为准（§10.4）。
-spec purged_attachment_paths_tx(any(), integer()) -> {ok, [map()]} | {error, term()}.
purged_attachment_paths_tx(Conn, RecordId) ->
    Sql =
        <<"SELECT DISTINCT ON (a.path) a.path, a.lifecycle_state,",
            " a.gc_verified, w.state AS derived_write_state, q.job_state AS queue_state,",
            " (q.completed_at IS NOT NULL) AS queue_done,",
            " (q.attempts-q.attempt_base >= 8) AS queue_exhausted",
            " FROM attachment a LEFT JOIN attachment_derived_write w ON w.attachment_id=a.id",
            " LEFT JOIN attachment_object_deletion q", "   ON q.path = a.path",
            "  AND q.storage_driver IS NOT DISTINCT FROM a.storage_driver",
            "  AND q.storage_namespace IS NOT DISTINCT FROM a.storage_namespace",
            "  AND q.storage_container IS NOT DISTINCT FROM a.storage_container",
            "  AND q.storage_locator_sha256 IS NOT DISTINCT FROM a.storage_locator_sha256",
            " WHERE a.id IN (SELECT r.attachment_id FROM talk_record_attachment_ref r WHERE r.record_id=$1",
            " UNION SELECT dw.attachment_id FROM attachment_derived_write dw JOIN talk_record_job j",
            " ON j.id=dw.source_job_id WHERE j.record_id=$1)",
            " ORDER BY a.path, q.created_at DESC NULLS LAST">>,
    case elib_pg:query(Conn, Sql, [RecordId]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% Includes pre-publication artifacts with no business reference.
write_blocker_tx(Conn, Rid) ->
    Sql = <<
        "SELECT CASE WHEN EXISTS(SELECT 1 FROM talk_record_processor_run r JOIN talk_record_job j"
        " ON j.id=r.job_id WHERE j.record_id=$1 AND r.state='unknown') THEN 'processor_stop_unconfirmed'"
        " WHEN EXISTS(SELECT 1 FROM talk_record_processor_run r JOIN talk_record_job j ON j.id=r.job_id"
        " WHERE j.record_id=$1 AND r.state IN ('prepared','running')) THEN 'processor_cleanup_pending'"
        " WHEN EXISTS(SELECT 1 FROM attachment_derived_write w JOIN talk_record_job j ON j.id=w.source_job_id"
        " WHERE j.record_id=$1 AND w.state IN ('reserved','writing','unknown')) THEN 'write_outcome_unknown'"
        " ELSE NULL END AS reason"
    >>,
    case elib_pg:query(Conn, Sql, [Rid]) of
        {ok, [#{<<"reason">> := R}]} -> {ok, R};
        E -> E
    end.

mark_write_pending_tx(Conn, Rid) ->
    case
        elib_pg:query(
            Conn,
            <<
                "UPDATE talk_record SET cleanup_status='pending',updated_at=now()"
                " WHERE id=$1 AND deleted_at IS NOT NULL AND cleanup_status IN ('complete','failed','pending')"
                " RETURNING cleanup_status"
            >>,
            [Rid]
        )
    of
        {ok, [#{<<"cleanup_status">> := S}]} -> {ok, S};
        {ok, []} -> cleanup_status_tx(Conn, Rid);
        Error -> Error
    end.
