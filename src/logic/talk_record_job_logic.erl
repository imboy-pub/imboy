-module(talk_record_job_logic).
-moduledoc "面聊 job 生命周期编排（T12）：complete 202 链路 / jobs 列表 / retry / processing-requests。".
%%%
% Handler→Logic→DS→Repo 的第二层：talk_record complete 与 job 生命周期
% 端点的业务编排。HTTP 语义承载在返回 reason 上，由 handler 映射——
% 本卡 handler/路由接线为集成项（约束：src/api/ 不在本卡写权限内，
% 映射表见模块尾注释与 T12 报告）。
%
% complete（§4 POST /:id/complete，全 202 不等模型）：
%   幂等键事务（登记→owner 门禁→If-Match→capture ended→缺段校验→
%   manifest 固化（§3"manifest 固化在 complete 事务中"）→media=assembling
%   →enqueue assemble job（dedupe 幂等）→mode≠record_only 记同意事件+
%   enqueue 转写 job→快照回填）。缺段 → {error, {missing_sequences, Detail}}
%   （409）；客户端 manifest_hash 与服务端重算不一致 → manifest_hash_conflict
%   （409）。record_only 不建模型任务；不在 HTTP 内等待任何模型（§4/§6）。
%
% jobs 列表（§4 GET /:id/jobs）：有记录读权即可查——状态/attempt/归一化
% error_code/可重试标记（failed 且错误码白名单且 kind 允许）；不回传
% lease 内部与外部服务原始异常。
%
% retry（§4 POST /:id/jobs/:job_id/retry）：owner=assemble/transcribe/
% summarize；editor 仅 transcribe/summarize；export 归请求者、purge 归运维
% （T19/T20 接线前对用户面一律 forbidden）。只允许可重试失败（state=
% failed 且错误码白名单）；幂等键 + If-Match；成功 202。
%
% processing-requests（§4 POST /:id/processing-requests，record_only 会后
% 升级）：owner/editor 显式请求 + 处理同意 + 配置版本；能力未配置 →
% processing_unavailable（503 处理器未选型如实返回，不伪装排队）；media
% 未 available → media_not_ready（409）。成功：同意事件 + processing_mode
% 推进（record.revision+1——一次性归属内设置，非 worker 心跳，§11.2）+
% enqueue 目标 kind job（V1 内 job 对未选型处理器挂起 waiting_processor，
% 不伪造成功）。
%%%

-export([
    complete/5,
    parse_complete/1,
    list_jobs/2,
    retry_job/5,
    processing_request/5,
    parse_processing_request/1,
    cancel_record_jobs/1,
    %% 挂起分类/骨架（worker 分派 + T13/T14/T19/T20 接线点）
    retryable_error/1,
    kind_allowed_for_role/2,
    processor_available/1,
    execute_export_tx/2,
    execute_purge_tx/2
]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(MAX_JOBS_LIST, 100).
%% 可重试失败错误码白名单：环境性/瞬时失败。input_stale/record_deleted/
%% missing_sequences/manifest_hash_conflict/part_attachment_missing/
%% processor_not_implemented/attempts_exhausted 为证据性/结构性失败，
%% 同输入重试不会变化，不开放人工重试。
-define(RETRYABLE_ERRORS, [
    <<"db_error">>,
    <<"lease_grant_failed">>,
    <<"bind_failed">>,
    <<"timeout">>,
    <<"provider_error">>
]).

%% ===================================================================
%% complete（§4；manifest 固化 + job 投递，202 即返）
%% ===================================================================

-spec complete(integer(), integer(), binary() | undefined, binary() | undefined, map()) ->
    {ok, created | replay, map()} | {error, term()}.
complete(Uid, RecordId, IfMatch, IdemKey, Body) ->
    case talk_record_logic:valid_idempotency_key(IdemKey) of
        false ->
            {error, idempotency_key_required};
        true ->
            complete_keyed(Uid, RecordId, IfMatch, IdemKey, Body)
    end.

complete_keyed(Uid, RecordId, IfMatch, IdemKey, Body) ->
    case talk_record_logic:parse_if_match(IfMatch) of
        {ok, ExpectedRevision} ->
            complete_revision(Uid, RecordId, ExpectedRevision, IdemKey, Body);
        {error, Reason} ->
            {error, Reason}
    end.

complete_revision(Uid, RecordId, ExpectedRevision, IdemKey, Body) ->
    case parse_complete(Body) of
        {ok, Parsed} ->
            Resource = complete_resource(RecordId),
            case talk_record_logic:request_digest(<<"POST">>, Resource, Body) of
                {ok, Digest} ->
                    complete_tx(Uid, RecordId, ExpectedRevision, IdemKey, Parsed, Resource, Digest);
                {error, non_canonical} ->
                    {error, invalid_body}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

complete_tx(Uid, RecordId, ExpectedRevision, IdemKey, Parsed, Resource, Digest) ->
    ExpiresAt = elib_dt:add(elib_dt:now(), {
        talk_record_job_support:idempotency_ttl_seconds(), second
    }),
    case
        elib_pg:with_tx(
            fun(Conn) ->
                complete_in_tx(
                    Conn,
                    Uid,
                    RecordId,
                    ExpectedRevision,
                    IdemKey,
                    Parsed,
                    Resource,
                    Digest,
                    ExpiresAt
                )
            end,
            [{reraise, false}]
        )
    of
        {ok, Mark, Payload} ->
            ok = talk_record_job_worker:kick(),
            {ok, Mark, Payload};
        {rollback, {talk_record_rollback, {error, Reason}}} ->
            {error, Reason};
        {rollback, Reason} ->
            ?ERROR_LOG([talk_record_complete_rolled_back, RecordId, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

complete_in_tx(Conn, Uid, RecordId, ExpectedRevision, IdemKey, Parsed, Resource, Digest, ExpiresAt) ->
    %% Idempotency snapshots never replace current access or capture identity.
    ok = complete_replay_access_tx(Conn, Uid, RecordId, Parsed),
    case
        talk_record_idempotency_repo:record_tx(
            Conn, Uid, <<"POST">>, Resource, IdemKey, Digest, ExpiresAt
        )
    of
        {ok, inserted} ->
            complete_fresh_tx(Conn, Uid, RecordId, ExpectedRevision, IdemKey, Parsed, Resource);
        {ok, existing, Row} ->
            talk_record_job_support:replay_idem(Row, Digest);
        {error, Reason} ->
            ?ERROR_LOG([talk_record_complete_idem_failed, Uid, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

complete_replay_access_tx(Conn, Uid, Rid, #{captures := [#{capture_id := Cid}]}) ->
    case
        elib_pg:query(
            Conn, <<"SELECT id FROM talk_record WHERE id=$1 AND deleted_at IS NULL FOR UPDATE">>, [
                Rid
            ]
        )
    of
        {ok, [_]} -> complete_current_owner_tx(Conn, Uid, Rid, Cid);
        {ok, []} -> throw({talk_record_rollback, {error, not_found}});
        {error, _} -> throw({talk_record_rollback, {error, db_error}})
    end;
complete_replay_access_tx(_, _, _, _) ->
    throw({talk_record_rollback, {error, invalid_captures}}).

complete_current_owner_tx(Conn, Uid, Rid, Cid) ->
    case talk_record_repo:find_visible(Uid, Rid) of
        {ok, #{<<"owner_user_id">> := Uid}} ->
            case
                elib_pg:query(
                    Conn,
                    <<
                        "SELECT id FROM talk_record_capture WHERE record_id=$1"
                        " AND id=$2 AND started_by=$3"
                    >>,
                    [Rid, Cid, Uid]
                )
            of
                {ok, [_]} -> ok;
                {ok, []} -> throw({talk_record_rollback, {error, capture_not_found}});
                {error, _} -> throw({talk_record_rollback, {error, db_error}})
            end;
        {ok, _} ->
            throw({talk_record_rollback, {error, forbidden}});
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.

complete_fresh_tx(Conn, Uid, RecordId, ExpectedRevision, IdemKey, Parsed, Resource) ->
    case owner_gate_tx(Conn, Uid, RecordId, ExpectedRevision) of
        {ok, RecordRow} ->
            case maps:get(<<"media_status">>, RecordRow, <<"uploading">>) of
                S when
                    S =:= <<"assembling">>;
                    S =:= <<"available">>;
                    S =:= <<"retryable_failed">>
                ->
                    throw({talk_record_rollback, {error, completion_already_submitted}});
                _ ->
                    complete_capture_tx(Conn, Uid, RecordId, RecordRow, IdemKey, Parsed, Resource)
            end;
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.

%% @private complete 门禁：软删 404；仅 owner（可见非 owner 403，不可见
%% 404）；If-Match 对 record.revision（§3 complete 必须 If-Match）。
owner_gate_tx(Conn, Uid, RecordId, ExpectedRevision) ->
    Columns =
        <<"tr.id, tr.owner_user_id, tr.revision, tr.deleted_at,",
            " tr.processing_mode, tr.content_revision, tr.media_status">>,
    case talk_record_repo:find_by_id_tx(Conn, RecordId, Columns) of
        {ok,
            #{<<"deleted_at">> := null, <<"owner_user_id">> := Uid, <<"revision">> := Revision} =
                Row} ->
            case Revision =:= ExpectedRevision of
                true -> {ok, Row};
                false -> {error, precondition_failed}
            end;
        {ok, #{<<"deleted_at">> := null}} ->
            case talk_record_repo:find_active_grant(RecordId, Uid) of
                {ok, _} -> {error, forbidden};
                _ -> {error, not_found}
            end;
        {ok, _} ->
            {error, not_found};
        {error, not_found} ->
            {error, not_found};
        {error, Reason} ->
            ?ERROR_LOG([talk_record_complete_gate_failed, RecordId, Reason]),
            {error, db_error}
    end.

complete_capture_tx(
    Conn, Uid, RecordId, RecordRow, IdemKey, #{captures := [CapDecl]} = Parsed, Resource
) ->
    CaptureId = maps:get(capture_id, CapDecl),
    case load_ended_capture_tx(Conn, RecordId, CaptureId) of
        {ok, Capture} ->
            complete_parts_tx(
                Conn, Uid, RecordId, RecordRow, Capture, CapDecl, Parsed, IdemKey, Resource
            );
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end;
complete_capture_tx(_Conn, _Uid, _RecordId, _RecordRow, _IdemKey, _Parsed, _Resource) ->
    %% V1 单 capture（§8：追加采集用明确追加操作+新聚合 manifest，不双 API）
    throw({talk_record_rollback, {error, invalid_captures}}).

load_ended_capture_tx(Conn, RecordId, CaptureId) ->
    case
        elib_pg:query(
            Conn,
            <<"SELECT c.id, c.record_id, c.status FROM talk_record_capture c",
                " WHERE c.record_id=$1 AND c.id=$2">>,
            [RecordId, CaptureId]
        )
    of
        {ok, [#{<<"status">> := <<"ended">>} = Row]} ->
            {ok, Row};
        {ok, [_]} ->
            {error, capture_not_ended};
        {ok, []} ->
            {error, capture_not_found};
        {error, Reason} ->
            ?ERROR_LOG([talk_record_complete_capture_load_failed, CaptureId, Reason]),
            {error, db_error}
    end.

complete_parts_tx(Conn, Uid, RecordId, RecordRow, Capture, CapDecl, Parsed, IdemKey, Resource) ->
    CaptureId = maps:get(<<"id">>, Capture),
    LastSeq = maps:get(last_seq, CapDecl),
    {ok, Parts} = talk_record_assembly_logic:confirmed_parts_tx(Conn, RecordId),
    case talk_record_assembly_logic:missing_sequences(Parts, LastSeq) of
        [] ->
            complete_manifest_tx(
                Conn, Uid, RecordId, RecordRow, CaptureId, Parts, CapDecl, Parsed, IdemKey, Resource
            );
        Missing ->
            throw(
                {talk_record_rollback,
                    {error,
                        {missing_sequences, #{
                            <<"capture_id">> => CaptureId,
                            <<"missing">> => Missing
                        }}}}
            )
    end.

complete_manifest_tx(
    Conn, Uid, RecordId, RecordRow, CaptureId, Parts, CapDecl, Parsed, IdemKey, Resource
) ->
    Manifest = talk_record_assembly_logic:build_manifest(CaptureId, Parts),
    ManifestHash = maps:get(<<"manifest_hash">>, Manifest),
    case maps:get(manifest_hash, CapDecl, undefined) of
        undefined -> ok;
        ClientHash when ClientHash =:= ManifestHash -> ok;
        _Other -> throw({talk_record_rollback, {error, manifest_hash_conflict}})
    end,
    {ok, _AsmId, _} = talk_record_assembly_logic:persist_tx(Conn, RecordId, CaptureId, Manifest),
    complete_jobs_tx(Conn, Uid, RecordId, RecordRow, ManifestHash, Parsed, IdemKey, Resource).

complete_jobs_tx(Conn, Uid, RecordId, RecordRow, ManifestHash, Parsed, IdemKey, Resource) ->
    Mode = maps:get(<<"processing_mode">>, RecordRow),
    ContentRevision = maps:get(<<"content_revision">>, RecordRow),
    {ok, AssembleMark, AssembleJobId} =
        talk_record_job_support:enqueue_job_tx(Conn, #{
            record_id => RecordId,
            kind => <<"assemble">>,
            input_manifest_hash => ManifestHash,
            input_content_revision => ContentRevision,
            dedupe_key => talk_record_job_support:dedupe_key(<<"assemble">>, RecordId, ManifestHash)
        }),
    %% Q-L1：回填固化行的产出 job（manifest 先落、job 后投递——同一事务；
    %% 幂等重放/dedupe 命中时写同值，i_tra_job 反查索引因此有真实数据）
    _ = elib_pg:query(
        Conn,
        <<"UPDATE talk_record_assembly SET job_id=$2, updated_at=now() WHERE record_id=$1">>,
        [RecordId, AssembleJobId]
    ),
    %% record_only 不创建模型任务（§4）；transcribe/summarize 建转写任务
    %% （summary 必须绑定已成功转写版本——§3；V1 转写 job 对未选型处理器
    %% 诚实挂起 waiting_processor，不伪造成功）
    ModelJob =
        case Mode of
            M when M =:= <<"transcribe">>; M =:= <<"summarize">> ->
                Consent = talk_record_job_support:require_consent(
                    maps:get(consent, Parsed, undefined)
                ),
                {ok, ConsentId} = talk_record_job_support:record_consent_tx(
                    Conn, Uid, RecordId, Consent, M
                ),
                Pin = talk_record_job_support:processor_pin(ConsentId, Consent, M),
                {ok, _Mark2, JobId2} =
                    talk_record_job_support:enqueue_job_tx(Conn, #{
                        record_id => RecordId,
                        kind => <<"transcribe">>,
                        processor_input => Pin,
                        input_manifest_hash => ManifestHash,
                        input_content_revision => ContentRevision,
                        dedupe_key => talk_record_job_support:model_dedupe_key(
                            <<"transcribe">>,
                            RecordId,
                            <<ManifestHash/binary, ":", M/binary, ":",
                                (maps:get(processor_config_version, Consent))/binary, ":",
                                (integer_to_binary(ContentRevision))/binary>>
                        )
                    }),
                #{<<"transcribe_job_id">> => JobId2};
            <<"record_only">> ->
                #{}
        end,
    _ = elib_pg:query(
        Conn,
        <<"UPDATE talk_record SET media_status='assembling',",
            " ended_at = COALESCE(ended_at, now()), updated_at=now()",
            " WHERE id=$1 AND deleted_at IS NULL">>,
        [RecordId]
    ),
    Payload0 = #{
        <<"record_id">> => RecordId,
        <<"job_id">> => AssembleJobId,
        <<"media_status">> => <<"assembling">>,
        <<"manifest_hash">> => ManifestHash
    },
    Payload = maps:merge(Payload0, ModelJob),
    Snapshot = jsone:encode(#{<<"payload">> => Payload}),
    case
        talk_record_idempotency_repo:complete_tx(Conn, Uid, <<"POST">>, Resource, IdemKey, Snapshot)
    of
        ok ->
            {ok, AssembleMark, Payload};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc complete 请求体解析：V1 单 capture 清单 + 可选处理同意。
%% 返回 #{captures := [#{capture_id,last_seq,manifest_hash?}], consent => map()|undefined}。
-spec parse_complete(map()) -> {ok, map()} | {error, atom()}.
parse_complete(Body) when is_map(Body) ->
    case maps:get(<<"captures">>, Body, undefined) of
        [CapDecl] when is_map(CapDecl) ->
            parse_capture_decl(Body, CapDecl);
        [_ | _] ->
            {error, invalid_captures};
        _ ->
            {error, invalid_captures}
    end;
parse_complete(_) ->
    {error, invalid_body}.

parse_capture_decl(Body, CapDecl) ->
    CaptureId = maps:get(<<"capture_id">>, CapDecl, 0),
    LastSeq = maps:get(<<"last_seq">>, CapDecl, -1),
    ClientHash = maps:get(<<"manifest_hash">>, CapDecl, undefined),
    case positive_int(CaptureId) of
        {ok, Cid} when is_integer(LastSeq), LastSeq >= 0 ->
            parse_decl_hash(Body, Cid, LastSeq, ClientHash);
        {ok, _} ->
            {error, invalid_last_seq};
        error ->
            {error, invalid_capture_id}
    end.

parse_decl_hash(Body, Cid, LastSeq, ClientHash) ->
    HashOk =
        ClientHash =:= undefined orelse (is_binary(ClientHash) andalso is_hex64(ClientHash)),
    case HashOk of
        true ->
            Consent =
                case maps:get(<<"processing_consent">>, Body, undefined) of
                    undefined -> undefined;
                    C when is_map(C) -> C;
                    _ -> invalid
                end,
            case Consent of
                invalid ->
                    {error, invalid_processing_consent};
                _ ->
                    Decl = #{capture_id => Cid, last_seq => LastSeq},
                    Decl1 =
                        case ClientHash of
                            undefined -> Decl;
                            _ -> Decl#{manifest_hash => ClientHash}
                        end,
                    {ok, #{captures => [Decl1], consent => Consent}}
            end;
        false ->
            {error, invalid_manifest_hash}
    end.

%% ===================================================================
%% 取消（删除/治理流程入口，T20 接线点）
%% ===================================================================

%% 记录删除/移交时取消全部非终态任务（running 行被取消后 worker 的
%% finish CAS 必然落空——迟到结果不发布）；assemble 任务同时释放其
%% processing lease（§10.3 取消释放；到期清扫兜底残余）。
-spec cancel_record_jobs(integer()) -> {ok, non_neg_integer()} | {error, term()}.
cancel_record_jobs(RecordId) ->
    elib_pg:with_tx(
        fun(Conn) ->
            case talk_record_job_repo:cancel_record_jobs_tx(Conn, RecordId) of
                {ok, N} when N > 0 ->
                    ok = cancel_release_leases_tx(Conn, RecordId),
                    {ok, N};
                Other ->
                    Other
            end
        end,
        [{reraise, false}]
    ).

cancel_release_leases_tx(Conn, RecordId) ->
    case
        elib_pg:query(
            Conn,
            <<"SELECT j.id FROM talk_record_job j",
                " WHERE j.record_id=$1 AND j.kind='assemble' AND j.state='cancelled'">>,
            [RecordId]
        )
    of
        {ok, JobRows} ->
            {ok, Parts} = talk_record_assembly_logic:confirmed_parts_tx(Conn, RecordId),
            lists:foreach(
                fun(#{<<"id">> := JobId}) ->
                    talk_record_assembly_logic:release_job_leases_tx(Conn, JobId, Parts)
                end,
                JobRows
            ),
            ok;
        {error, _} ->
            ok
    end.

%% ===================================================================
%% 分类与骨架
%% ===================================================================

%% @doc 可重试错误码白名单（binary 形态与 DB error_code 列一致）。
-spec retryable_error(binary() | null) -> boolean().
retryable_error(ErrorCode) when is_binary(ErrorCode) ->
    lists:member(ErrorCode, ?RETRYABLE_ERRORS);
retryable_error(_) ->
    false.

%% @doc 角色 × kind 白名单（§4 retry 行）：owner=assemble/transcribe/
%% summarize；editor=transcribe/summarize；export（T19 请求者限定）与
%% purge（T20 运维专用）不在用户 retry 面。
-spec kind_allowed_for_role(binary(), owner | editor) -> boolean().
kind_allowed_for_role(Kind, owner) ->
    lists:member(Kind, [<<"assemble">>, <<"transcribe">>, <<"summarize">>]);
kind_allowed_for_role(Kind, editor) ->
    lists:member(Kind, [<<"transcribe">>, <<"summarize">>]);
kind_allowed_for_role(_, _) ->
    false.

%% @doc 处理器能力（V1 如实）：transcribe 看 asr_enabled，summarize 看
%% summary_enabled；capabilities 可经部署配置覆写（T13/T14 接入后置 true）。
-spec processor_available(binary()) -> boolean().
processor_available(<<"transcribe">>) ->
    maps:get(<<"asr_enabled">>, talk_record_ds:capabilities(), false) =:= true;
processor_available(<<"summarize">>) ->
    maps:get(<<"summary_enabled">>, talk_record_ds:capabilities(), false) =:= true;
processor_available(_) ->
    false.

%% -------------------------------------------------------------------
%% export 骨架（T19 接线点）/ purge 执行体（T20 已接线）
%% -------------------------------------------------------------------

%% @doc export job 执行骨架。T19 接线内容：talk_record_export 落行、
%% 文本产物经 talk_record_attachment_adapter:bind_artifact_tx(role=export)
%% 登记、expires_at 到期清理、requester 再验。
-spec execute_export_tx(any(), map()) -> {ok, map()} | {error, term()}.
execute_export_tx(Conn, Job) ->
    talk_record_export_job:execute_tx(Conn, Job).

%% @doc purge job 执行体（T20 接线，T12 骨架替换）：委托
%% talk_record_cleanup_logic:execute_purge_tx/2 —— 协调引用释放（经
%% T06 adapter，治理门先行）、删除意图登记、cleanup_status 推进；
%% 不执行存储 DELETE（附件物理状态从既有队列回读，§10.4）。
%% 未删除记录的 purge 行 → {error, record_not_deleted}（防御拒绝）。
-spec execute_purge_tx(any(), map()) -> {ok, map()} | {error, term()}.
execute_purge_tx(Conn, Job) ->
    talk_record_cleanup_logic:execute_purge_tx(Conn, Job).

-spec complete_resource(integer()) -> binary().
complete_resource(RecordId) ->
    <<"/api/v1/talk-records/", (integer_to_binary(RecordId))/binary, "/complete">>.

-spec positive_int(term()) -> {ok, integer()} | error.
positive_int(N) when is_integer(N), N > 0, N =< 9223372036854775807 -> {ok, N};
positive_int(_) -> error.

-spec is_hex64(binary()) -> boolean().
is_hex64(Bin) when is_binary(Bin), byte_size(Bin) =:= 64 ->
    lists:all(
        fun(C) -> (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f) end,
        binary_to_list(Bin)
    );
is_hex64(_) ->
    false.

%% ===================================================================
%% HTTP 语义映射（handler 接线用；5543x 为本卡建议值——5520..5542 已被
%% T05/T07 预留段占用，本卡从 5543 续段，登记入 error_code.hrl 时以
%% 分配器为准）
%% -------------------------------------------------------------------
%% not_found → 404/ERR_NOT_FOUND                    forbidden → 403/ERR_FORBIDDEN
%% precondition_required → 428/5525                 precondition_failed → 412/5526
%% idempotency_key_required → 422/ERR_IDEMPOTENCY_KEY_REQUIRED
%% idempotency_conflict / idempotency_pending → 409/ERR_IDEMPOTENCY_CONFLICT
%% invalid_body / invalid_captures / invalid_capture_id / invalid_last_seq /
%% invalid_manifest_hash / invalid_processing_consent /
%% processing_consent_required / invalid_processing_mode → 422
%% {missing_sequences, Detail} → 409/5543（payload 附 missing 清单）
%% manifest_hash_conflict → 409/5544
%% capture_not_found → 404                           capture_not_ended → 409/5551
%% job_not_retryable → 409/5546
%% processing_unavailable → 503/5548
%% media_not_ready → 409/5550
%% db_error → 500
%% ===================================================================

-spec list_jobs(integer(), integer()) -> {ok, map()} | {error, term()}.
list_jobs(A1, A2) ->
    talk_record_job_requests:list_jobs(A1, A2).

-spec retry_job(integer(), integer(), integer(), binary() | undefined, binary() | undefined) ->
    {ok, created | replay, map()} | {error, term()}.
retry_job(A1, A2, A3, A4, A5) ->
    talk_record_job_requests:retry_job(A1, A2, A3, A4, A5).

-spec processing_request(integer(), integer(), binary() | undefined, binary() | undefined, map()) ->
    {ok, created | replay, map()} | {error, term()}.
processing_request(A1, A2, A3, A4, A5) ->
    talk_record_job_requests:processing_request(A1, A2, A3, A4, A5).

-spec parse_processing_request(map()) -> {ok, map()} | {error, atom()}.
parse_processing_request(A1) ->
    talk_record_job_requests:parse_processing_request(A1).
