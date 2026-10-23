-module(talk_record_client_audio_repo).
-moduledoc "Read-only exact assembly source metadata; never accepts a URL, path or storage location.".
-export([run_identity_tx/4, assembly_tx/2, artifact_tx/3, consent_tx/4, source_authority_tx/2]).

run_identity_tx(C, Rid, Jid, RunId) ->
    one(
        C,
        <<
            "SELECT actor_user_id,device_session_id FROM talk_record_client_run"
            " WHERE record_id=$1 AND job_id=$2 AND id=$3"
        >>,
        [Rid, Jid, RunId]
    ).

assembly_tx(C, Rid) ->
    one(
        C,
        <<
            "SELECT manifest_hash,segments,total_duration_ms,audio_attachment_id"
            " FROM talk_record_assembly WHERE record_id=$1"
        >>,
        [Rid]
    ).

artifact_tx(C, Rid, Aid) ->
    one(
        C,
        <<
            "SELECT a.id,a.size,a.file_hash256,a.mime_type,a.cipher,a.lifecycle_state,"
            " w.state AS write_state,r.state AS ref_state,r.role AS ref_role,"
            " (r.lease_until IS NULL OR r.lease_until>clock_timestamp()) AS reference_valid,"
            " EXISTS(SELECT 1 FROM enterprise_attachment_retention e WHERE e.attachment_id=a.id"
            " AND e.purge_state='purged') AS governance_purged"
            " FROM attachment a JOIN attachment_derived_write w ON w.attachment_id=a.id"
            " JOIN talk_record_job j ON j.id=w.source_job_id AND j.record_id=$1"
            " AND j.kind='assemble' AND j.state='succeeded' AND j.attempt=w.source_attempt"
            " JOIN talk_record_attachment_ref r ON r.attachment_id=a.id AND r.record_id=$1"
            " AND r.role='transcoded_audio' AND r.business_key='assembly:transcoded_audio:'||a.id::text"
            " WHERE a.id=$2 AND a.scope='talk_record' AND a.scope_ref=$1::text AND a.status=1"
        >>,
        [Rid, Aid]
    ).

consent_tx(C, Uid, Run, Kind) ->
    one(
        C,
        <<
            "SELECT id FROM talk_record_consent_event WHERE id=$1 AND record_id=$2 AND actor_id=$3"
            " AND kind='processing_opt_in' AND policy_version=$4 AND processor_config_version=$5"
            " AND processing_mode=$6 FOR SHARE"
        >>,
        [
            maps:get(<<"consent_event_id">>, Run),
            maps:get(<<"record_id">>, Run),
            Uid,
            maps:get(<<"policy_version">>, Run),
            maps:get(<<"config_version">>, Run),
            Kind
        ]
    ).

one(C, Sql, Args) ->
    case elib_pg:query(C, Sql, Args) of
        {ok, [R]} -> {ok, R};
        {ok, []} -> {error, audio_source_denied};
        _ -> {error, audio_source_denied}
    end.

source_authority_tx(C, #{rid := Rid, jid := Jid, run_id := RunId, seq := 0}) ->
    case talk_record_provider_repo:origin_tx(C, Rid, Jid, RunId) of
        {ok, #{<<"cancelled_at">> := null} = T} ->
            A = talk_record_client_gate:json(maps:get(<<"assembly">>, T)),
            Profile = talk_record_client_gate:json(maps:get(<<"request_profile">>, T)),
            Request = maps:get(<<"request">>, Profile),
            {ok, (maps:merge(T, Request))#{
                <<"run_id">> => maps:get(<<"source_run_id">>, T),
                <<"attachment_id">> => maps:get(<<"id">>, A),
                <<"attachment_hash">> => maps:get(<<"sha256">>, A),
                <<"manifest_hash">> => maps:get(<<"input_manifest_hash">>, T),
                <<"content_revision">> => maps:get(<<"input_content_revision">>, T),
                <<"transcript_version">> => maps:get(<<"input_transcript_version">>, T)
            }};
        _ ->
            {error, audio_source_denied}
    end;
source_authority_tx(_, _) ->
    {error, audio_source_denied}.
