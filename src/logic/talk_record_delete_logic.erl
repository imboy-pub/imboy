-module(talk_record_delete_logic).
-moduledoc "talk_record 删除闭环业务逻辑（T20：DELETE /:id + 白名单 + purge 重试 + 注销预检投影）。".
%%%
% 删除/留存/注销预检闭环（BACKEND-API §4 DELETE /:id、§10.3、§10.4、
% §11.3；DESIGN 删除状态页契约 T21）：
%
%   * delete/3 —— owner + If-Match（record.revision 域，§11.2）：
%       事务（lock_for_delete 行锁——与 PUT/DELETE grants 同一行 FOR
%       UPDATE，互斥天然成立）→ 幂等（已删 owner 重放返回首次结果形状）
%       → 软删（deleted_at + cleanup_status=pending + revision/acl_revision
%       推进）→ 全量撤权（活跃 grant revoked；acl_revision 推进使在途
%       票据/Range 立即 acl_stale/not_found——复用 T17 verify_ticket
%       校验点的服务端重验）→ 活跃采集 fence（interrupted + lease_epoch+1）
%       → 取消全部非终态 job（T12 cancel_record_jobs——迟到模型结果
%       不发布）→ purge job 投递 → 审计。HTTP 202 语义承载在 handler。
%       【不在 HTTP 内物理删】：本函数只写软删行 + 投递 purge job；
%       字节删除归既有附件队列（§10.3 删除行映射）。
%   * deleted_detail/2 —— 删除后 owner 白名单 GET（§11.3）：仅
%       id/logical_deleted/cleanup_status/physical_delete_state/revision/
%       deleted_at——无标题、正文、参与者、授权名单；非 owner 404。
%   * retry_purge/1 —— 清理失败可恢复入口（§4 retry 行"purge 由专用
%       运维能力重试"）：内部函数 + admin 骨架注释，不暴露普通用户
%       端点（handler 无路由）。仅 failed/pending 态可触发；blocked_*/
%       retained_*（治理保护）拒绝——需先解除留存。
%   * project_departure_custody_tx/2 —— 注销/离企投影（§9）：owner 失去
%       归属访问的企业记录 custody_status → handover_required（T19 已建
%       字段）；不删除、不级联（注销执行器接线点，user_deletion_executor
%       不在本卡写权限——报告登记集成项）。
%%%

-export([
    delete/3,
    deleted_detail/2,
    retry_purge/1,
    retry_purge_tx/2,
    project_departure_custody_tx/2,
    project_departure_custody_tx/3,
    ensure_departure_responsibility_tx/3
]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% DELETE /:id
%% ===================================================================

%% @doc owner 删除（202 语义在 handler 映射）。If-Match 必填（428）、
%% 非法形态 422、旧版本 412；无权/不存在统一 not_found；非 owner 可见
%% forbidden、不可见 not_found。
-spec delete(integer(), integer(), binary() | undefined) ->
    {ok, deleted | replay, map()} | {error, term()}.
delete(Uid, RecordId, IfMatch) ->
    case talk_record_logic:parse_if_match(IfMatch) of
        {ok, ExpectedRevision} ->
            delete_tx(Uid, RecordId, ExpectedRevision);
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete_tx(integer(), integer(), pos_integer()) ->
    {ok, deleted | replay, map()} | {error, term()}.
delete_tx(Uid, RecordId, ExpectedRevision) ->
    case
        elib_pg:with_tx(
            fun(Conn) -> delete_in_tx(Conn, Uid, RecordId, ExpectedRevision) end,
            [{reraise, false}]
        )
    of
        {ok, Mark, Payload} ->
            ok = talk_record_job_worker:kick(),
            {ok, Mark, Payload};
        {rollback, {talk_record_rollback, {error, Reason}}} ->
            {error, Reason};
        {rollback, Reason} ->
            ?ERROR_LOG([talk_record_delete_rolled_back, RecordId, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

delete_in_tx(Conn, Uid, RecordId, ExpectedRevision) ->
    case talk_record_delete_repo:lock_for_delete_tx(Conn, RecordId, ExpectedRevision) of
        {ok, live, Row} ->
            delete_live_tx(Conn, Uid, RecordId, Row);
        {ok, deleted, undefined} ->
            delete_replay_tx(Conn, Uid, RecordId);
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.

%% @private owner 门禁（锁后复核）：非 owner 可见（grant 未撤）→
%% forbidden；不可见 → not_found（§11.3 统一 404）。
delete_live_tx(Conn, Uid, RecordId, #{<<"owner_user_id">> := Uid}) ->
    do_delete_tx(Conn, Uid, RecordId);
delete_live_tx(_Conn, Uid, RecordId, _Row) ->
    case talk_record_repo:find_active_grant(RecordId, Uid) of
        {ok, _} -> throw({talk_record_rollback, {error, forbidden}});
        _ -> throw({talk_record_rollback, {error, not_found}})
    end.

do_delete_tx(Conn, Uid, RecordId) ->
    {ok, DeletedRow} = talk_record_delete_repo:soft_delete_tx(Conn, RecordId),
    {ok, _Revoked} = talk_record_delete_repo:revoke_all_grants_tx(Conn, RecordId),
    {ok, _} = talk_record_delete_repo:fence_active_capture_tx(Conn, RecordId),
    %% 取消全部非终态 job + 释放 assemble processing lease（T12 已备；
    %% running 行 finish CAS 落空 → 迟到模型结果不发布）
    cancel_jobs_in_tx(Conn, RecordId),
    {ok, _Mark, _PurgeJobId} = talk_record_delete_repo:enqueue_purge_tx(Conn, RecordId, 0),
    case
        talk_record_repo:insert_audit_tx(
            Conn,
            RecordId,
            Uid,
            <<"delete">>,
            RecordId,
            #{<<"revision">> => maps:get(<<"revision">>, DeletedRow)}
        )
    of
        {ok, _} -> ok;
        {error, _} -> throw({talk_record_rollback, {error, audit_failed}})
    end,
    {ok, deleted, delete_payload(Conn, DeletedRow)}.

%% cancel_record_jobs 自起事务（elib_pg:with_tx）——嵌套事务不可行，
%% 在删除事务内直接执行其 tx 形态主体。
cancel_jobs_in_tx(Conn, RecordId) ->
    case talk_record_job_repo:cancel_record_jobs_tx(Conn, RecordId) of
        {ok, _N} ->
            ok;
        {error, Reason} ->
            ?WARN_LOG([talk_record_delete_cancel_jobs_failed, RecordId, Reason]),
            throw({talk_record_rollback, {error, cancel_jobs_failed}})
    end.

%% @private 已删 owner 幂等重放（§3：重放先返回首次结果形状，不再校验
%% 版本）；非 owner 404。
delete_replay_tx(_Conn, Uid, RecordId) ->
    case talk_record_delete_repo:find_deleted_for_owner(Uid, RecordId) of
        {ok, Row} ->
            {ok, replay, delete_replay_payload(Row)};
        {error, not_found} ->
            %% 行可能不存在或属他人——统一 404（§11.3 不披露存在性）
            throw({talk_record_rollback, {error, not_found}});
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.

%% @private 重放载荷（只读投影，不再触发附件回读事务）。
delete_replay_payload(Row) ->
    #{
        <<"id">> => maps:get(<<"id">>, Row),
        <<"logical_deleted">> => true,
        <<"cleanup_status">> => maps:get(<<"cleanup_status">>, Row, <<"pending">>),
        <<"cleanup_generation">> => maps:get(<<"cleanup_generation">>, Row, 0),
        <<"physical_delete_state">> => <<"unknown">>,
        <<"revision">> => maps:get(<<"revision">>, Row),
        <<"acl_revision">> => maps:get(<<"acl_revision">>, Row),
        <<"deleted_at">> => maps:get(<<"deleted_at">>, Row)
    }.

%% @private 202 响应载荷（§4 DELETE 行 + §10.4 拆分）：
%% logical_deleted / cleanup_status / physical_delete_state 三段拆分；
%% revision/acl_revision 为删除推进后的新值（客户端刷新列表用）。
delete_payload(Conn, Row) ->
    RecordId = maps:get(<<"id">>, Row),
    #{
        <<"id">> => RecordId,
        <<"logical_deleted">> => true,
        <<"cleanup_status">> => maps:get(<<"cleanup_status">>, Row, <<"pending">>),
        <<"cleanup_generation">> => maps:get(<<"cleanup_generation">>, Row, 0),
        <<"physical_delete_state">> =>
            talk_record_cleanup_logic:physical_delete_state_tx(Conn, RecordId),
        <<"cleanup_reason">> => talk_record_cleanup_logic:cleanup_reason_tx(Conn, RecordId),
        <<"revision">> => maps:get(<<"revision">>, Row),
        <<"acl_revision">> => maps:get(<<"acl_revision">>, Row),
        <<"deleted_at">> => maps:get(<<"deleted_at">>, Row)
    }.

%% ===================================================================
%% 删除后 owner 白名单 GET（§11.3）
%% ===================================================================

%% @doc 仅 owner 可读清理状态白名单：无正文/标题/参与者/授权名单。
%% 非 owner / 未删除 / 不存在 → not_found（统一 404）。
%% cleanup_status 为 blocked_*/retained_* 时附 protected=true（T21
%% 删除状态页"受治理保护≠异常"语义）；failed 附 retryable=true。
-spec deleted_detail(integer(), integer()) -> {ok, map()} | {error, not_found | term()}.
deleted_detail(Uid, RecordId) ->
    case talk_record_delete_repo:find_deleted_for_owner(Uid, RecordId) of
        {ok, Row} ->
            deleted_status_payload(Row, RecordId, Uid);
        {error, Reason} ->
            {error, Reason}
    end.

deleted_status_payload(Row, Rid, Uid) ->
    Result = elib_pg:with_conn(fun(C) ->
        {
            talk_record_cleanup_logic:physical_delete_state_tx(C, Rid),
            talk_record_cleanup_logic:cleanup_reason_tx(C, Rid),
            talk_record_cleanup_retry_repo:capability_tx(C, Uid, Rid)
        }
    end),
    case Result of
        {Physical, Reason, Retry} when
            is_binary(Physical), (is_binary(Reason) orelse Reason =:= null)
        ->
            P = whitelist_payload(Row, Physical),
            Status =
                case {maps:get(<<"cleanup_status">>, P), Reason} of
                    {<<"complete">>, R} when R =/= null -> <<"pending">>;
                    {S, _} -> S
                end,
            {ok, P#{
                <<"cleanup_reason">> => Reason,
                <<"cleanup_status">> => Status,
                <<"cleanup_retry">> => Retry
            }};
        _ ->
            {error, db_error}
    end.

whitelist_payload(Row, Physical) ->
    Status = maps:get(<<"cleanup_status">>, Row, <<"pending">>),
    Base = #{
        <<"id">> => maps:get(<<"id">>, Row),
        <<"logical_deleted">> => true,
        <<"cleanup_status">> => Status,
        <<"cleanup_generation">> => maps:get(<<"cleanup_generation">>, Row, null),
        <<"physical_delete_state">> => Physical,
        <<"revision">> => maps:get(<<"revision">>, Row),
        <<"deleted_at">> => maps:get(<<"deleted_at">>, Row),
        <<"capabilities_for_actor">> => #{<<"read_content">> => false}
    },
    Base1 = maybe_flag(
        protected,
        Status,
        [
            <<"blocked_retention">>,
            <<"blocked_hold">>,
            <<"retained_by_other_reference">>
        ],
        Base
    ),
    maybe_flag(retryable, Status, [<<"failed">>], Base1).

maybe_flag(Key, Status, Values, Map) ->
    case lists:member(Status, Values) of
        true -> Map#{Key => true};
        false -> Map
    end.

%% ===================================================================
%% purge 重试（运维专用；§4 retry 行"purge 由专用运维能力重试"）
%% ===================================================================

%% @doc 清理失败可恢复：failed/pending → 复位 pending + 投递新 purge
%% job（attempt = 已有 purge job 数，dedupe 稳定）+ 立即刷新物理状态
%% 投影（队列可能已完成——refresh 先行可免一轮 worker）。
%% blocked_*/retained_*/complete → {error, cleanup_not_retryable}
%% （治理保护态需先解除留存/引用，不做绕过）。
%% 【admin 骨架】运维端点（如 POST /api/v1/admin/talk-records/:id/
%% cleanup-retry，需实例管理员能力门禁 + 审计）为集成项——本卡只提供
%% 此内部函数，不暴露普通用户端点（handler/路由无条目）。
-spec retry_purge(integer()) -> {ok, map()} | {error, term()}.
retry_purge(_RecordId) -> {error, cleanup_actor_required}.

%% Legacy callers cannot create an unauthenticated retry or bypass generation CAS.
-spec retry_purge_tx(any(), integer()) -> {error, cleanup_actor_required}.
retry_purge_tx(_Conn, _RecordId) -> {error, cleanup_actor_required}.

%% ===================================================================
%% 注销/离企投影（§9 custody；注销执行器接线点）
%% ===================================================================

%% Normal voluntary exit requires explicit ownership transfer first. Caller
%% already holds organization/member locks; lock owned records only afterwards.
ensure_departure_responsibility_tx(Conn, Uid, OrgId) ->
    Sql =
        <<"SELECT id FROM talk_record WHERE owner_user_id=$1",
            " AND organization_id=$2 AND scope='organization' AND deleted_at IS NULL",
            " ORDER BY id FOR UPDATE">>,
    case elib_pg:query(Conn, Sql, [Uid, OrgId]) of
        {ok, []} -> ok;
        {ok, [_ | _]} -> throw({abort_tx, {409, <<"请先移交面聊记录的管理责任，再退出企业"/utf8>>}});
        {error, Reason} -> throw({abort_tx, {internal, Reason}})
    end.

%% Forced suspension/removal preserves records and denies stale tickets. This
%% never grants administrators content access or implements custody reassignment.
project_departure_custody_tx(Conn, Uid) ->
    project_departure_custody_tx(Conn, Uid, 0).

project_departure_custody_tx(Conn, Uid, OrgId) ->
    Sql =
        <<"UPDATE talk_record tr SET custody_status='handover_required',",
            " revision=revision+1, acl_revision=acl_revision+1, updated_at=now()",
            " WHERE owner_user_id=$1 AND scope='organization' AND deleted_at IS NULL",
            " AND custody_status='active' AND ($2::bigint=0 OR organization_id=$2)",
            " AND NOT EXISTS (SELECT 1 FROM organization_member om",
            " WHERE om.organization_id=tr.organization_id AND om.user_id=$1",
            " AND om.status='active') RETURNING id">>,
    case elib_pg:query(Conn, Sql, [Uid, OrgId]) of
        {ok, Rows} -> {ok, length(Rows)};
        {error, Reason} -> {error, Reason}
    end.
