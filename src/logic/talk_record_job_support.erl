-module(talk_record_job_support).
-moduledoc "面聊记录职责拆分；保留原端点、权限与事务语义。".
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").
-define(MAX_JOBS_LIST, 100).
-export([
    dedupe_key/3,
    enqueue_job_tx/2,
    idempotency_ttl_seconds/0,
    record_consent_tx/5,
    replay_idem/2,
    require_consent/1,
    processor_pin/3,
    summary_snapshot_tx/2,
    model_dedupe_key/3,
    role_gate/3
]).

%% @private 模型同意事件（§2 consent_event：记录操作事实，不伪造现场同意）。
%% Consent 必须已归一化（policy_version 非空）——record_only 之外缺同意在
%% 入口拒绝。
record_consent_tx(Conn, Uid, RecordId, Consent, Mode) ->
    #{policy_version := PV, processor_config_version := PCV} = Consent,
    Cap = talk_record_ds:capabilities(),
    case
        PV =:= maps:get(<<"policy_version">>, Cap) andalso
            PCV =:= talk_record_processor_config:version() andalso
            talk_record_job_logic:processor_available(Mode)
    of
        true -> ok;
        false -> throw({talk_record_rollback, {error, processing_unavailable}})
    end,
    Id = elib_tsid:generate(attachment),
    case
        elib_pg:execute(
            Conn,
            <<
                "INSERT INTO talk_record_consent_event"
                " (id,record_id,actor_id,kind,policy_version,processing_mode,processor_config_version,captured_at)"
                " VALUES($1,$2,$3,'processing_opt_in',$4,$5,$6,now())"
            >>,
            [Id, RecordId, Uid, PV, Mode, PCV]
        )
    of
        {ok, 1} -> {ok, Id};
        _ -> throw({talk_record_rollback, {error, db_error}})
    end.

processor_pin(Id, Consent, Mode) ->
    #{
        <<"consent_event_id">> => Id,
        <<"processor_config_version">> => maps:get(processor_config_version, Consent),
        <<"requested_mode">> => Mode,
        <<"processor_config_digest">> => talk_record_processor_config:fingerprint()
    }.

-spec enqueue_job_tx(any(), map()) -> {ok, created | existing, integer()} | no_return().
enqueue_job_tx(Conn, Job0) ->
    Job =
        case Job0 of
            #{kind := <<"summarize">>, record_id := Rid, processor_input := Pin} ->
                Job0#{
                    processor_input => Pin#{
                        <<"summary_snapshot">> => summary_snapshot_tx(Conn, Rid)
                    }
                };
            _ ->
                Job0
        end,
    case talk_record_job_repo:enqueue_tx(Conn, Job) of
        {ok, created, JobId} ->
            {ok, created, JobId};
        {ok, existing, Row} ->
            {ok, existing, maps:get(<<"id">>, Row)};
        {error, Reason} ->
            ?ERROR_LOG([talk_record_job_enqueue_failed, maps:get(record_id, Job), Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

%% @private 同意归一化：policy_version 必填非空（processor_config_version
%% 可选透传——V1 无本地配置真源可比对，登记事实）。
normalize_consent(#{<<"policy_version">> := PV, <<"processor_config_version">> := PCV}) when
    is_binary(PV), PV =/= <<>>, is_binary(PCV), PCV =/= <<>>
->
    #{policy_version => PV, processor_config_version => PCV}.

%% @private 模型模式缺同意的入口拒绝（§1 仅录音不要求转写同意；触发模型
%% 的 complete 必须带处理同意）。
-spec require_consent(map() | undefined) -> map() | no_return().
require_consent(undefined) ->
    throw({talk_record_rollback, {error, processing_consent_required}});
require_consent(Raw) ->
    try
        normalize_consent(Raw)
    catch
        _:_ ->
            throw({talk_record_rollback, {error, processing_consent_required}})
    end.

%% @private 幂等重放（快照形态回放，不回读后续状态；已成功 complete 的
%% 重试不再走版本校验——§3 幂等动作重放先返回首次结果再校验版本）。
replay_idem(Row, Digest) ->
    case maps:get(<<"payload_hash">>, Row, <<>>) of
        Digest ->
            case maps:get(<<"result_ref">>, Row, null) of
                SnapBin when is_binary(SnapBin) ->
                    #{<<"payload">> := P} = jsone:decode(SnapBin),
                    {ok, replay, P};
                _ ->
                    {error, idempotency_pending}
            end;
        _ ->
            {error, idempotency_conflict}
    end.

%% ===================================================================
%% 内部
%% ===================================================================

%% @private 读者角色 + If-Match 门禁：owner/editor 放行返回角色；viewer
%% forbidden；不可见 not_found；revision 不匹配 precondition_failed。
%% （processing-requests 与 retry 共用；complete 用更严的 owner_gate_tx。）
role_gate(Uid, RecordId, ExpectedRevision) ->
    case talk_record_repo:find_visible(Uid, RecordId) of
        {ok, Row} ->
            case maps:get(<<"revision">>, Row) =:= ExpectedRevision of
                false ->
                    {error, precondition_failed};
                true ->
                    case maps:get(<<"owner_user_id">>, Row) of
                        Uid ->
                            {ok, owner};
                        _ ->
                            case talk_record_repo:find_active_grant(RecordId, Uid) of
                                {ok, #{<<"role">> := <<"editor">>}} -> {ok, editor};
                                {ok, _} -> {error, forbidden};
                                _ -> {error, not_found}
                            end
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec dedupe_key(binary(), integer(), binary()) -> binary().
dedupe_key(Kind, RecordId, ManifestHash) ->
    <<Kind/binary, ":", (integer_to_binary(RecordId))/binary, ":", ManifestHash/binary>>.

summary_snapshot_tx(C, Rid) ->
    case talk_record_summary_repo:find_by_record_tx(C, Rid) of
        {ok, R} ->
            #{<<"id">> => maps:get(<<"id">>, R), <<"revision">> => maps:get(<<"revision">>, R)};
        {error, not_found} ->
            null;
        _ ->
            throw({talk_record_rollback, {error, db_error}})
    end.

model_dedupe_key(Kind, Rid, Input) ->
    dedupe_key(Kind, Rid, binary:encode_hex(crypto:hash(sha256, Input), lowercase)).

-spec idempotency_ttl_seconds() -> pos_integer().
idempotency_ttl_seconds() ->
    case config_ds:env(imboy, talk_record_idempotency_ttl_seconds, 604800) of
        N when is_integer(N), N > 0 -> N;
        _ -> 604800
    end.
