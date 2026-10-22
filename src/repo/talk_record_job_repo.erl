-module(talk_record_job_repo).
-moduledoc "talk_record 持久处理任务仓储（T12：迁移 00000211+00000214+00000222）。".
%%%
% talk_record_job 数据仓库层。
%
% 队列协议（仿 moya_ai_worker claim 分事务 + attachment_delete_worker SKIP
% LOCKED/租约/退避/attempts 上限，T03 findings A4 两范例合并）：
%   * enqueue_tx/2    —— dedupe_key UNIQUE 幂等投递（HTTP 超时/离线补传
%                        重试不产生重复任务与重复付费调用，§4）；
%   * claim_next_tx/4 —— 原子 UPDATE ... FOR UPDATE SKIP LOCKED：候选 =
%                        state IN (queued,retry_wait) AND NOT suspended AND
%                        (retry_at IS NULL OR retry_at <= now())，按
%                        created_at,id 稳定序取最老一条；租约 = lease_token
%                        (uuid) + lease_until；attempt 自增（毒行经上限
%                        终止，worker 侧裁决）；
%   * finish_*_tx     —— claim 与 finish 分开事务（处理期不持锁）；
%                        CAS 判据 = id + state='running' + lease_token，
%                        租约丢失（reaper 回收/取消/他者接管）一律
%                        {error, lease_lost}，迟到结果不发布（§3/§11.2）；
%   * reap_expired_tx/1 —— 零副作用回收过期租约：running 且 lease_until
%                        已过 → 回 queued（attempt 保留，防毒行永动；租约
%                        清空）。只用 lease_until 判据，不做年龄猜测。
%   * cancel_*_tx     —— cancelled 终态（非终态才可取消；running 行取消后
%                        worker 的 finish CAS 自然拒绝）。
%
% execution_mode（00000222）：只有 server_local 可进入普通 worker、finish、
% renew、reaper、人工 retry 与 unsuspend。client_byok 由专用执行协议管理，
% 本仓储不提供把它降级为 server_local 的通道。取消仍覆盖两模式以支持撤权/删除。
%
% suspended（00000214）：处理器未选型的诚实挂起位——waiting_processor 行
% state=retry_wait 且 suspended=true，claim 候选谓词排除；选型后（T13/T14）
% unsuspend_kind_tx 置 false 恢复调度，不伪造成功。
%
% 终态一致性：succeeded/failed/cancelled 必须 finished_at（209 CHECK
% ck_trj_finished）；retry_failed_tx 复位时清 finished_at。
%%%

-export([tablename/0]).
-export([enqueue_tx/2]).
-export([claim_next_tx/4, renew_lease_tx/3]).
-export([finish_success_tx/3]).
-export([finish_failed_tx/4]).
-export([finish_retry_tx/5]).
-export([finish_suspended_tx/4]).
-export([cancel_job_tx/2]).
-export([cancel_record_jobs_tx/2]).
-export([retry_failed_tx/2]).
-export([unsuspend_kind_tx/2]).
-export([reap_expired_tx/1]).
-export([find_tx/2, find/1]).
-export([find_by_dedupe_tx/2]).
-export([list_by_record_tx/3, list_by_record/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(JOB_COLUMNS,
    <<"j.id,j.record_id,j.processor_input,j.kind,j.execution_mode,j.input_version,j.input_manifest_hash,",
        "j.input_transcript_version,j.input_content_revision,j.dedupe_key,",
        "j.state,j.attempt,j.lease_owner,j.lease_token,j.lease_until,j.retry_at,",
        "j.error_code,j.provider_request_id,j.suspended,j.finished_at,",
        "j.created_at,j.updated_at">>
).
-define(ACTIVE_STATES, <<"('queued','retry_wait','running')">>).

%% ===================================================================
%% 表名
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"talk_record_job">>).

%% ===================================================================
%% 投递（dedupe 幂等）
%% ===================================================================

%% Job :: #{
%%   id => integer(),                       %% 缺省 elib_tsid:generate(attachment)
%%   record_id := integer(), kind := binary(),
%%   execution_mode => <<"server_local">> | <<"client_byok">>,
%%   input_version => integer() | null,
%%   input_manifest_hash => binary() | null,
%%   input_transcript_version => integer() | null,
%%   input_content_revision => integer() | null,
%%   dedupe_key := binary()
%% }
%% 返回 {ok, created, JobId} | {ok, existing, Row}（dedupe 撞库转重放）。
-spec enqueue_tx(any(), map()) ->
    {ok, created, integer()} | {ok, existing, map()} | {error, term()}.
enqueue_tx(
    Conn,
    #{
        record_id := RecordId,
        kind := Kind,
        dedupe_key := DedupeKey
    } = Job
) when
    is_integer(RecordId), RecordId > 0, is_binary(Kind), is_binary(DedupeKey)
->
    case execution_mode(Kind, maps:get(execution_mode, Job, <<"server_local">>)) of
        {ok, Mode} -> enqueue_mode_tx(Conn, Job, Mode);
        error -> {error, invalid_job}
    end;
enqueue_tx(_Conn, _Job) ->
    {error, invalid_job}.

execution_mode(Kind, <<"server_local">>) ->
    case
        lists:member(Kind, [
            <<"assemble">>, <<"transcribe">>, <<"summarize">>, <<"export">>, <<"purge">>
        ])
    of
        true -> {ok, <<"server_local">>};
        false -> error
    end;
execution_mode(Kind, <<"client_byok">>) when Kind =:= <<"transcribe">>; Kind =:= <<"summarize">> ->
    {ok, <<"client_byok">>};
execution_mode(_, _) ->
    error.

enqueue_mode_tx(Conn, #{record_id := RecordId, kind := Kind, dedupe_key := DedupeKey} = Job, Mode) ->
    Id = maps:get(id, Job, elib_tsid:generate(attachment)),
    Now = elib_dt:now(),
    Data = #{
        <<"id">> => Id,
        <<"record_id">> => RecordId,
        <<"kind">> => Kind,
        <<"execution_mode">> => Mode,
        <<"input_version">> => maps:get(input_version, Job, null),
        <<"processor_input">> =>
            case maps:get(processor_input, Job, null) of
                null -> null;
                P -> jsone:encode(P)
            end,
        <<"input_manifest_hash">> => maps:get(input_manifest_hash, Job, null),
        <<"input_transcript_version">> => maps:get(input_transcript_version, Job, null),
        <<"input_content_revision">> => maps:get(input_content_revision, Job, null),
        <<"dedupe_key">> => DedupeKey,
        <<"state">> => <<"queued">>,
        <<"attempt">> => 0,
        <<"created_at">> => Now,
        <<"updated_at">> => Now
    },
    {Sql0, Params} = elib_pg_sql:insert(tablename(), Data),
    Sql = <<Sql0/binary, " ON CONFLICT (dedupe_key) DO NOTHING RETURNING id">>,
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [#{<<"id">> := NewId}]} ->
            {ok, created, NewId};
        {ok, []} ->
            replay_mode_tx(Conn, DedupeKey, Mode);
        {error, Reason} ->
            {error, Reason}
    end.

replay_mode_tx(Conn, Key, Mode) ->
    case find_by_dedupe_tx(Conn, Key) of
        {ok, existing, #{<<"execution_mode">> := Mode}} = Existing -> Existing;
        {ok, existing, _} -> {error, dedupe_execution_mode_conflict};
        Error -> Error
    end.

%% ===================================================================
%% claim（SKIP LOCKED + 租约）
%% ===================================================================

%% 原子抢占最老一条可执行任务；LeaseToken 为 uuid 文本（worker 生成）。
%% 返回 {ok, Row} | {error, empty | term()}。
-spec claim_next_tx(any(), binary(), binary(), pos_integer()) ->
    {ok, map()} | {error, empty | term()}.
claim_next_tx(Conn, LeaseOwner, LeaseToken, LeaseSeconds) when
    is_binary(LeaseOwner), is_binary(LeaseToken), is_integer(LeaseSeconds), LeaseSeconds > 0
->
    Sql =
        <<"UPDATE ", (tablename())/binary, " j SET", " state='running', attempt = j.attempt + 1,",
            " lease_owner=$1, lease_token=$2,", " lease_until = now() + make_interval(secs => $3),",
            " retry_at = NULL, error_code = NULL, updated_at = now()", " WHERE j.id IN (",
            "   SELECT id FROM ", (tablename())/binary,
            "   WHERE execution_mode='server_local' AND state IN ('queued','retry_wait')",
            "     AND suspended = false", "     AND (retry_at IS NULL OR retry_at <= now())",
            "   ORDER BY created_at ASC, id ASC", "   LIMIT 1 FOR UPDATE SKIP LOCKED)",
            " RETURNING ", ?JOB_COLUMNS/binary>>,
    case elib_pg:query(Conn, Sql, [LeaseOwner, LeaseToken, LeaseSeconds]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, empty};
        {error, Reason} -> {error, Reason}
    end.

%% Never revive expired leases or renew a replacement attempt/token.
renew_lease_tx(C, J, Seconds) when is_integer(Seconds), Seconds > 0, Seconds =< 900 ->
    case
        elib_pg:query(
            C,
            <<
                "UPDATE talk_record_job SET lease_until=clock_timestamp()+make_interval(secs=>$4),"
                "updated_at=now() WHERE id=$1 AND state='running' AND lease_token=$2 AND attempt=$3"
                " AND execution_mode='server_local' AND lease_until>clock_timestamp() RETURNING id"
            >>,
            [
                maps:get(<<"id">>, J),
                maps:get(<<"lease_token">>, J),
                maps:get(<<"attempt">>, J),
                Seconds
            ]
        )
    of
        {ok, [_]} -> ok;
        {ok, []} -> {error, lease_lost};
        E -> E
    end.

%% ===================================================================
%% finish（CAS：state='running' + lease_token）
%% ===================================================================

%% 成功终态。CAS 未命中（取消/回收/他者接管）→ {error, lease_lost}，
%% 迟到结果由调用方放弃发布。
-spec finish_success_tx(any(), integer(), binary()) -> ok | {error, lease_lost | not_found}.
finish_success_tx(Conn, JobId, LeaseToken) ->
    Sql =
        <<"UPDATE ", (tablename())/binary, " SET state='succeeded', finished_at=now(),",
            " lease_owner=NULL, lease_token=NULL, lease_until=NULL,",
            " retry_at=NULL, suspended=false, updated_at=now()",
            " WHERE id=$1 AND state='running' AND lease_token=$2 AND execution_mode='server_local'",
            " RETURNING id">>,
    case elib_pg:query(Conn, Sql, [JobId, LeaseToken]) of
        {ok, [_]} -> ok;
        {ok, []} -> {error, lease_lost};
        {error, Reason} -> {error, Reason}
    end.

%% 终态失败（不可重试错误/attempts 耗尽）。ErrorCode 为归一化短码。
-spec finish_failed_tx(any(), integer(), binary(), binary()) ->
    ok | {error, lease_lost | not_found | term()}.
finish_failed_tx(Conn, JobId, LeaseToken, ErrorCode) ->
    Sql =
        <<"UPDATE ", (tablename())/binary, " SET state='failed', error_code=$3, finished_at=now(),",
            " lease_owner=NULL, lease_token=NULL, lease_until=NULL,",
            " retry_at=NULL, suspended=false, updated_at=now()",
            " WHERE id=$1 AND state='running' AND lease_token=$2 AND execution_mode='server_local'",
            " RETURNING id">>,
    case elib_pg:query(Conn, Sql, [JobId, LeaseToken, ErrorCode]) of
        {ok, [_]} -> ok;
        {ok, []} -> {error, lease_lost};
        {error, Reason} -> {error, Reason}
    end.

%% 退避重试：running → retry_wait，retry_at = now()+RetryDelaySeconds；
%% attempt 保留（毒行经 worker 上限裁决终止）。
-spec finish_retry_tx(any(), integer(), binary(), binary(), pos_integer()) ->
    {ok, retry_wait, binary()} | {error, lease_lost | not_found | term()}.
finish_retry_tx(Conn, JobId, LeaseToken, ErrorCode, RetryDelaySeconds) ->
    Sql =
        <<"UPDATE ", (tablename())/binary, " SET state='retry_wait', error_code=$3,",
            " retry_at = now() + make_interval(secs => $4),",
            " lease_owner=NULL, lease_token=NULL, lease_until=NULL,",
            " suspended=false, updated_at=now()",
            " WHERE id=$1 AND state='running' AND lease_token=$2 AND execution_mode='server_local'",
            " RETURNING retry_at">>,
    case elib_pg:query(Conn, Sql, [JobId, LeaseToken, ErrorCode, RetryDelaySeconds]) of
        {ok, [#{<<"retry_at">> := RetryAt}]} -> {ok, retry_wait, RetryAt};
        {ok, []} -> {error, lease_lost};
        {error, Reason} -> {error, Reason}
    end.

%% 诚实挂起（处理器未选型/kind 未接线）：retry_wait + suspended=true +
%% error_code=ErrorCode（waiting_processor / kind_not_wired）。无 retry_at
%% ——claim 谓词按 suspended 排除，选型/接线后 unsuspend 恢复，不靠猜时间。
-spec finish_suspended_tx(any(), integer(), binary(), binary()) ->
    ok | {error, lease_lost | not_found | term()}.
finish_suspended_tx(Conn, JobId, LeaseToken, ErrorCode) ->
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET state='retry_wait', error_code=$3, suspended=true,",
            " retry_at=NULL, lease_owner=NULL, lease_token=NULL,",
            " lease_until=NULL, updated_at=now()",
            " WHERE id=$1 AND state='running' AND lease_token=$2 AND execution_mode='server_local'",
            " RETURNING id">>,
    case elib_pg:query(Conn, Sql, [JobId, LeaseToken, ErrorCode]) of
        {ok, [_]} -> ok;
        {ok, []} -> {error, lease_lost};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% 取消 / 人工重试 / unsuspend
%% ===================================================================

%% 取消单任务：仅非终态可取消（queued/retry_wait/running → cancelled）。
%% running 行被取消后，worker 的 finish CAS（state='running'）必然落空——
%% 迟到结果不发布（§3 删除后迟到结果 CAS 拒绝同机制）。
-spec cancel_job_tx(any(), integer()) ->
    {ok, cancelled} | {ok, already_terminal} | {error, not_found | term()}.
cancel_job_tx(Conn, JobId) ->
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET state='cancelled', finished_at=now(), suspended=false,",
            " lease_owner=NULL, lease_token=NULL, lease_until=NULL,",
            " retry_at=NULL, updated_at=now()", " WHERE id=$1 AND state IN ", ?ACTIVE_STATES/binary,
            " RETURNING id">>,
    case elib_pg:query(Conn, Sql, [JobId]) of
        {ok, [_]} ->
            {ok, cancelled};
        {ok, []} ->
            case find_tx(Conn, JobId) of
                {ok, _} -> {ok, already_terminal};
                {error, _} = E -> E
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% 记录级取消（删除/治理流程入口，T20 接线）：全部非终态任务 → cancelled。
%% 返回取消行数。
-spec cancel_record_jobs_tx(any(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
cancel_record_jobs_tx(Conn, RecordId) ->
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET state='cancelled', finished_at=now(), suspended=false,",
            " lease_owner=NULL, lease_token=NULL, lease_until=NULL,",
            " retry_at=NULL, updated_at=now()", " WHERE record_id=$1 AND state IN ",
            ?ACTIVE_STATES/binary, " RETURNING id">>,
    case elib_pg:query(Conn, Sql, [RecordId]) of
        {ok, Rows} -> {ok, length(Rows)};
        {error, Reason} -> {error, Reason}
    end.

%% 人工重试（§4 POST /:id/jobs/:job_id/retry）：仅 failed → queued 全复位。
%% 可重试性分类（错误码白名单）在 logic 层先行裁决；此处只做状态机守卫。
-spec retry_failed_tx(any(), integer()) -> {ok, map()} | {error, not_failed | not_found | term()}.
retry_failed_tx(Conn, JobId) ->
    Sql =
        <<"UPDATE ", (tablename())/binary, " j SET", " state='queued', attempt=0, error_code=NULL,",
            " finished_at=NULL, retry_at=NULL, suspended=false,",
            " lease_owner=NULL, lease_token=NULL, lease_until=NULL,", " updated_at=now()",
            " WHERE j.id=$1 AND j.state='failed' AND j.execution_mode='server_local'",
            " RETURNING ", ?JOB_COLUMNS/binary>>,
    case elib_pg:query(Conn, Sql, [JobId]) of
        {ok, [Row]} ->
            {ok, Row};
        {ok, []} ->
            case find_tx(Conn, JobId) of
                {ok, _} -> {error, not_failed};
                {error, _} = E -> E
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% 挂起恢复（T13/T14 处理器选型后调用；T12 不调用——预置入口）。
-spec unsuspend_kind_tx(any(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
unsuspend_kind_tx(Conn, Kind) ->
    Sql =
        <<"UPDATE ", (tablename())/binary, " SET suspended=false, updated_at=now()",
            " WHERE kind=$1 AND execution_mode='server_local' AND suspended=true AND state='retry_wait'",
            " RETURNING id">>,
    case elib_pg:query(Conn, Sql, [Kind]) of
        {ok, Rows} -> {ok, length(Rows)};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% reaper（零副作用过期租约回收）
%% ===================================================================

%% 仅 lease_until 判据：running 且租约已过 → 回 queued（attempt 保留、
%% 租约清空）。不删数据、不重置错误、不做年龄猜测——正在处理的行只要
%% 租约有效绝不被回收。
-spec reap_expired_tx(any()) -> {ok, non_neg_integer()} | {error, term()}.
reap_expired_tx(Conn) ->
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET state='queued', lease_owner=NULL, lease_token=NULL,",
            " lease_until=NULL, updated_at=now()",
            " WHERE execution_mode='server_local' AND state='running'",
            "   AND lease_until IS NOT NULL AND lease_until <= clock_timestamp()",
            " RETURNING id">>,
    case elib_pg:query(Conn, Sql, []) of
        {ok, Rows} -> {ok, length(Rows)};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% 查询
%% ===================================================================

-spec find_tx(any(), integer()) -> {ok, map()} | {error, not_found | term()}.
find_tx(Conn, JobId) ->
    Sql = <<"SELECT ", ?JOB_COLUMNS/binary, " FROM ", (tablename())/binary, " j WHERE j.id=$1">>,
    case elib_pg:query(Conn, Sql, [JobId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec find(integer()) -> {ok, map()} | {error, not_found | term()}.
find(JobId) ->
    elib_pg:with_conn(fun(Conn) -> find_tx(Conn, JobId) end).

-spec find_by_dedupe_tx(any(), binary()) -> {ok, existing, map()} | {error, not_found | term()}.
find_by_dedupe_tx(Conn, DedupeKey) ->
    Sql =
        <<"SELECT ", ?JOB_COLUMNS/binary, " FROM ", (tablename())/binary,
            " j WHERE j.dedupe_key=$1">>,
    case elib_pg:query(Conn, Sql, [DedupeKey]) of
        {ok, [Row]} -> {ok, existing, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec list_by_record_tx(any(), integer(), pos_integer()) -> {ok, [map()]} | {error, term()}.
list_by_record_tx(Conn, RecordId, Limit) ->
    Sql =
        <<"SELECT ", ?JOB_COLUMNS/binary, " FROM ", (tablename())/binary,
            " j WHERE j.record_id=$1 ORDER BY j.created_at DESC, j.id DESC LIMIT $2">>,
    case elib_pg:query(Conn, Sql, [RecordId, Limit]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

-spec list_by_record(integer()) -> {ok, [map()]} | {error, term()}.
list_by_record(RecordId) ->
    elib_pg:with_conn(fun(Conn) -> list_by_record_tx(Conn, RecordId, 100) end).
