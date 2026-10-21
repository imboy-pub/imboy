-module(talk_record_job_requests).
-moduledoc "面聊记录职责拆分；保留原端点、权限与事务语义。".
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").
-define(MAX_JOBS_LIST, 100).
-export([list_jobs/2, parse_processing_request/1, processing_request/5, retry_job/5]).

%% ===================================================================
%% jobs 列表（§4 GET /:id/jobs）
%% ===================================================================

%% 有记录读权（owner/editor/viewer）即可查任务状态面。
-spec list_jobs(integer(), integer()) -> {ok, map()} | {error, term()}.
list_jobs(Uid, RecordId) ->
    case talk_record_repo:find_visible(Uid, RecordId) of
        {ok, _Row} ->
            case talk_record_job_repo:list_by_record(RecordId) of
                {ok, Rows} ->
                    {ok, #{<<"jobs">> => [job_payload(R) || R <- Rows]}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 出站 DTO（§4：状态/可重试标记/归一化错误码；不回传 lease
%% 内部、外部服务原始异常与秘钥）。
-spec job_payload(map()) -> map().
job_payload(Job) ->
    #{
        <<"job_id">> => maps:get(<<"id">>, Job),
        <<"kind">> => maps:get(<<"kind">>, Job),
        <<"state">> => maps:get(<<"state">>, Job),
        <<"attempt">> => maps:get(<<"attempt">>, Job),
        <<"error_code">> => maps:get(<<"error_code">>, Job, null),
        <<"retryable">> => retryable(Job),
        <<"retry_at">> => maps:get(<<"retry_at">>, Job, null),
        <<"suspended">> => maps:get(<<"suspended">>, Job, false),
        <<"created_at">> => maps:get(<<"created_at">>, Job),
        <<"finished_at">> => maps:get(<<"finished_at">>, Job, null)
    }.

%% @doc 可重试标记：failed 且错误码白名单且 kind 属用户可重试集合
%% （export/purge 走各自专用入口，恒 false）。
-spec retryable(map()) -> boolean().
retryable(#{<<"state">> := <<"failed">>, <<"kind">> := Kind, <<"error_code">> := ErrorCode}) when
    ErrorCode =/= null
->
    lists:member(Kind, [<<"assemble">>, <<"transcribe">>, <<"summarize">>]) andalso
        talk_record_job_logic:retryable_error(ErrorCode);
retryable(_) ->
    false.

%% ===================================================================
%% retry（§4 POST /:id/jobs/:job_id/retry）
%% ===================================================================

-spec retry_job(integer(), integer(), integer(), binary() | undefined, binary() | undefined) ->
    {ok, created | replay, map()} | {error, term()}.
retry_job(Uid, RecordId, JobId, IfMatch, IdemKey) ->
    case talk_record_logic:valid_idempotency_key(IdemKey) of
        false ->
            {error, idempotency_key_required};
        true ->
            retry_keyed(Uid, RecordId, JobId, IfMatch, IdemKey)
    end.

retry_keyed(Uid, RecordId, JobId, IfMatch, IdemKey) ->
    case talk_record_logic:parse_if_match(IfMatch) of
        {ok, ExpectedRevision} ->
            case talk_record_job_support:role_gate(Uid, RecordId, ExpectedRevision) of
                {ok, Role} ->
                    retry_dispatch(Uid, RecordId, JobId, IdemKey, Role);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

retry_dispatch(Uid, RecordId, JobId, IdemKey, Role) ->
    Resource = retry_resource(RecordId, JobId),
    case
        elib_pg:with_tx(
            fun(Conn) -> retry_tx(Conn, Uid, RecordId, JobId, IdemKey, Resource, Role) end,
            [{reraise, false}]
        )
    of
        {ok, Mark, Payload} ->
            ok = talk_record_job_worker:kick(),
            {ok, Mark, Payload};
        {rollback, {talk_record_rollback, {error, Reason}}} ->
            {error, Reason};
        {rollback, Reason} ->
            ?ERROR_LOG([talk_record_retry_rolled_back, JobId, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

retry_tx(Conn, Uid, RecordId, JobId, IdemKey, Resource, Role) ->
    ExpiresAt = elib_dt:add(elib_dt:now(), {
        talk_record_job_support:idempotency_ttl_seconds(), second
    }),
    %% retry 无请求体：digest 取资源指纹 SHA-256（满足 payload_hash 64 位
    %% 十六进制 CHECK；同资源同键重放可比对）
    Digest = binary:encode_hex(crypto:hash(sha256, Resource), lowercase),
    case
        talk_record_idempotency_repo:record_tx(
            Conn, Uid, <<"POST">>, Resource, IdemKey, Digest, ExpiresAt
        )
    of
        {ok, inserted} ->
            retry_fresh_tx(Conn, Uid, RecordId, JobId, IdemKey, Resource, Role);
        {ok, existing, Row} ->
            talk_record_job_support:replay_idem(Row, Digest);
        {error, Reason} ->
            ?ERROR_LOG([talk_record_retry_idem_failed, Uid, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

retry_fresh_tx(Conn, Uid, RecordId, JobId, IdemKey, Resource, Role) ->
    case talk_record_job_repo:find_tx(Conn, JobId) of
        {ok, #{<<"record_id">> := RecordId} = Job} ->
            case talk_record_job_logic:kind_allowed_for_role(maps:get(<<"kind">>, Job), Role) of
                true ->
                    retry_state_tx(Conn, Uid, Job, IdemKey, Resource);
                false ->
                    %% editor 限 transcribe/summarize；export/purge 非用户面（§4）
                    throw({talk_record_rollback, {error, forbidden}})
            end;
        {ok, _} ->
            %% 任务不属于该记录：统一 not_found（不泄露存在性，§11.3）
            throw({talk_record_rollback, {error, not_found}});
        {error, not_found} ->
            throw({talk_record_rollback, {error, not_found}});
        {error, Reason} ->
            ?ERROR_LOG([talk_record_retry_load_failed, JobId, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

retry_state_tx(
    Conn,
    Uid,
    #{<<"state">> := <<"failed">>, <<"error_code">> := ErrorCode} = Job,
    IdemKey,
    Resource
) when
    ErrorCode =/= null
->
    case talk_record_job_logic:retryable_error(ErrorCode) of
        true ->
            do_retry_tx(Conn, Uid, Job, IdemKey, Resource);
        false ->
            throw({talk_record_rollback, {error, job_not_retryable}})
    end;
retry_state_tx(_Conn, _Uid, #{<<"state">> := <<"failed">>}, _IdemKey, _Resource) ->
    throw({talk_record_rollback, {error, job_not_retryable}});
retry_state_tx(_Conn, _Uid, #{<<"state">> := _}, _IdemKey, _Resource) ->
    %% 非失败态（queued/running/retry_wait/succeeded/cancelled）不可重试
    throw({talk_record_rollback, {error, job_not_retryable}}).

do_retry_tx(Conn, Uid, Job, IdemKey, Resource) ->
    JobId = maps:get(<<"id">>, Job),
    case talk_record_job_repo:retry_failed_tx(Conn, JobId) of
        {ok, ResetRow} ->
            Payload = job_payload(ResetRow),
            Snapshot = jsone:encode(#{<<"payload">> => Payload}),
            case
                talk_record_idempotency_repo:complete_tx(
                    Conn, Uid, <<"POST">>, Resource, IdemKey, Snapshot
                )
            of
                ok -> {ok, created, Payload};
                {error, Reason} -> {error, Reason}
            end;
        {error, not_failed} ->
            throw({talk_record_rollback, {error, job_not_retryable}});
        {error, Reason} ->
            ?ERROR_LOG([talk_record_retry_reset_failed, JobId, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

%% ===================================================================
%% processing-requests（§4：record_only 会后升级）
%% ===================================================================

-spec processing_request(integer(), integer(), binary() | undefined, binary() | undefined, map()) ->
    {ok, created | replay, map()} | {error, term()}.
processing_request(Uid, RecordId, IfMatch, IdemKey, Body) ->
    case talk_record_logic:valid_idempotency_key(IdemKey) of
        false ->
            {error, idempotency_key_required};
        true ->
            processing_keyed(Uid, RecordId, IfMatch, IdemKey, Body)
    end.

processing_keyed(Uid, RecordId, IfMatch, IdemKey, Body) ->
    case parse_processing_request(Body) of
        {ok, Parsed} ->
            processing_parsed(Uid, RecordId, IfMatch, IdemKey, Body, Parsed);
        {error, Reason} ->
            {error, Reason}
    end.

processing_parsed(Uid, RecordId, IfMatch, IdemKey, Body, Parsed) ->
    case talk_record_logic:parse_if_match(IfMatch) of
        {ok, ExpectedRevision} ->
            case talk_record_job_support:role_gate(Uid, RecordId, ExpectedRevision) of
                {ok, _Role} ->
                    processing_capability(Uid, RecordId, IdemKey, Body, Parsed);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% 能力检查在状态检查之前：处理器未选型 → processing_unavailable（503
%% 如实返回，§4"202 或能力未配置503"；不借排队伪装可用）。
processing_capability(Uid, RecordId, IdemKey, Body, Parsed) ->
    case talk_record_job_logic:processor_available(maps:get(mode, Parsed)) of
        true ->
            processing_tx(Uid, RecordId, IdemKey, Body, Parsed);
        false ->
            {error, processing_unavailable}
    end.

processing_tx(Uid, RecordId, IdemKey, Body, Parsed) ->
    Resource = processing_resource(RecordId),
    case talk_record_logic:request_digest(<<"POST">>, Resource, Body) of
        {ok, Digest} ->
            processing_digest(Uid, RecordId, IdemKey, Parsed, Resource, Digest);
        {error, non_canonical} ->
            {error, invalid_body}
    end.

processing_digest(Uid, RecordId, IdemKey, Parsed, Resource, Digest) ->
    ExpiresAt = elib_dt:add(elib_dt:now(), {
        talk_record_job_support:idempotency_ttl_seconds(), second
    }),
    case
        elib_pg:with_tx(
            fun(Conn) ->
                processing_in_tx(Conn, Uid, RecordId, IdemKey, Parsed, Resource, Digest, ExpiresAt)
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
            ?ERROR_LOG([talk_record_processing_rolled_back, RecordId, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

processing_in_tx(Conn, Uid, RecordId, IdemKey, Parsed, Resource, Digest, ExpiresAt) ->
    case
        talk_record_idempotency_repo:record_tx(
            Conn, Uid, <<"POST">>, Resource, IdemKey, Digest, ExpiresAt
        )
    of
        {ok, inserted} ->
            processing_fresh_tx(Conn, Uid, RecordId, IdemKey, Parsed, Resource);
        {ok, existing, Row} ->
            talk_record_job_support:replay_idem(Row, Digest);
        {error, Reason} ->
            ?ERROR_LOG([talk_record_processing_idem_failed, Uid, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

processing_fresh_tx(Conn, Uid, RecordId, IdemKey, Parsed, Resource) ->
    case
        elib_pg:query(
            Conn,
            <<"SELECT id, deleted_at, media_status, processing_mode, content_revision",
                " FROM talk_record WHERE id=$1 FOR UPDATE">>,
            [RecordId]
        )
    of
        {ok, [#{<<"deleted_at">> := null, <<"media_status">> := <<"available">>} = Row]} ->
            processing_ready_tx(Conn, Uid, RecordId, Row, IdemKey, Parsed, Resource);
        {ok, [#{<<"deleted_at">> := null}]} ->
            throw({talk_record_rollback, {error, media_not_ready}});
        {ok, [_]} ->
            throw({talk_record_rollback, {error, not_found}});
        {ok, []} ->
            throw({talk_record_rollback, {error, not_found}});
        {error, Reason} ->
            ?ERROR_LOG([talk_record_processing_load_failed, RecordId, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

processing_ready_tx(Conn, Uid, RecordId, Row, IdemKey, Parsed, Resource) ->
    case talk_record_assembly_logic:find_tx(Conn, RecordId) of
        {ok, #{<<"manifest_hash">> := ManifestHash, <<"capture_id">> := CaptureId}} ->
            Mode = maps:get(mode, Parsed),
            ContentRevision = maps:get(<<"content_revision">>, Row),
            Consent = maps:get(consent, Parsed),
            {ok, ConsentId} = talk_record_job_support:record_consent_tx(
                Conn, Uid, RecordId, Consent, Mode
            ),
            Pin = talk_record_job_support:processor_pin(ConsentId, Consent, Mode),
            {Kind, TV} = processing_kind(Conn, RecordId, Mode),
            %% processing_mode 推进（一次性归属内设置，record.revision+1）
            {ok, [Updated]} =
                elib_pg:query(
                    Conn,
                    <<"UPDATE talk_record SET processing_mode=$2, revision=revision+1,",
                        " updated_at=now() WHERE id=$1 AND deleted_at IS NULL",
                        " RETURNING revision">>,
                    [RecordId, Mode]
                ),
            {ok, _Mark, JobId} =
                talk_record_job_support:enqueue_job_tx(Conn, #{
                    record_id => RecordId,
                    kind => Kind,
                    input_transcript_version => TV,
                    processor_input => Pin,
                    input_manifest_hash => ManifestHash,
                    input_content_revision => ContentRevision,
                    dedupe_key => talk_record_job_support:model_dedupe_key(
                        Kind,
                        RecordId,
                        <<ManifestHash/binary, ":", Mode/binary, ":",
                            (maps:get(processor_config_version, Consent))/binary, ":",
                            (integer_to_binary(ContentRevision))/binary, ":",
                            (binary:encode_hex(crypto:hash(sha256, IdemKey), lowercase))/binary>>
                    )
                }),
            Payload = #{
                <<"record_id">> => RecordId,
                <<"job_id">> => JobId,
                <<"processing_mode">> => Mode,
                <<"revision">> => maps:get(<<"revision">>, Updated),
                <<"capture_id">> => CaptureId
            },
            Snapshot = jsone:encode(#{<<"payload">> => Payload}),
            case
                talk_record_idempotency_repo:complete_tx(
                    Conn, Uid, <<"POST">>, Resource, IdemKey, Snapshot
                )
            of
                ok -> {ok, created, Payload};
                {error, Reason} -> {error, Reason}
            end;
        {error, not_found} ->
            %% media=available 但 manifest 缺失（数据残缺）：如实 409
            throw({talk_record_rollback, {error, media_not_ready}});
        {error, Reason} ->
            ?ERROR_LOG([talk_record_processing_assembly_failed, RecordId, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

%% @doc processing-requests 请求体解析：mode ∈ transcribe|summarize +
%% 处理同意（policy_version + processor_config_version 必填）。
processing_kind(C, Rid, <<"summarize">>) ->
    case talk_record_transcript_repo:find_current_tx(C, Rid, <<"t.version,t.status">>) of
        {ok, #{<<"status">> := <<"no_speech">>}} ->
            throw({talk_record_rollback, {error, media_not_ready}});
        {ok, T} ->
            {<<"summarize">>, maps:get(<<"version">>, T)};
        {error, not_found} ->
            {<<"transcribe">>, null};
        _ ->
            throw({talk_record_rollback, {error, db_error}})
    end;
processing_kind(_, _, Mode) ->
    {Mode, null}.

-spec parse_processing_request(map()) -> {ok, map()} | {error, atom()}.
parse_processing_request(Body) when is_map(Body) ->
    case maps:get(<<"mode">>, Body, <<>>) of
        M when M =:= <<"transcribe">>; M =:= <<"summarize">> ->
            parse_pr_consent(Body, M);
        _ ->
            {error, invalid_processing_mode}
    end;
parse_processing_request(_) ->
    {error, invalid_body}.

parse_pr_consent(Body, Mode) ->
    case maps:get(<<"processing_consent">>, Body, undefined) of
        #{<<"policy_version">> := PV, <<"processor_config_version">> := PCV} when
            is_binary(PV), PV =/= <<>>, is_binary(PCV), PCV =/= <<>>
        ->
            {ok, #{
                mode => Mode,
                consent => #{policy_version => PV, processor_config_version => PCV}
            }};
        #{} ->
            {error, processing_consent_required};
        _ ->
            {error, processing_consent_required}
    end.

-spec retry_resource(integer(), integer()) -> binary().
retry_resource(RecordId, JobId) ->
    <<"/api/v1/talk-records/", (integer_to_binary(RecordId))/binary, "/jobs/",
        (integer_to_binary(JobId))/binary, "/retry">>.

-spec processing_resource(integer()) -> binary().
processing_resource(RecordId) ->
    <<"/api/v1/talk-records/", (integer_to_binary(RecordId))/binary, "/processing-requests">>.
