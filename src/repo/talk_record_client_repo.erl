-module(talk_record_client_repo).
-moduledoc "Transaction-only device execution storage; never dispatches a provider.".
-export([
    lock_record_tx/2,
    lock_job_tx/3,
    lock_run_tx/4,
    latest_tx/2,
    parts_tx/2,
    insert_run_tx/2,
    insert_part_tx/2,
    freeze_plan_tx/3,
    heartbeat_tx/2,
    dispatch_tx/5,
    result_tx/5,
    settle_tx/4,
    claim_job_tx/3,
    finish_job_tx/3,
    expire_tx/2,
    sources_tx/3,
    consent_tx/5,
    touch_tx/2,
    reuse_tx/3,
    previous_tx/3,
    complete_tx/2,
    expired_candidates/0,
    same_input_tx/6,
    active_count_tx/2
]).

lock_record_tx(C, Rid) -> one(C, <<"SELECT * FROM talk_record WHERE id=$1 FOR UPDATE">>, [Rid]).
lock_job_tx(C, Rid, Jid) ->
    one(
        C,
        <<"SELECT * FROM talk_record_job WHERE record_id=$1 AND id=$2 AND execution_mode='client_byok' FOR UPDATE">>,
        [Rid, Jid]
    ).
lock_run_tx(C, Rid, Jid, Id) ->
    one(
        C,
        <<
            "SELECT *,lease_until>clock_timestamp() AS lease_valid,floor(extract(epoch from lease_until))::bigint AS lease_expires_at_unix FROM talk_record_client_run"
            " WHERE record_id=$1 AND job_id=$2 AND id=$3 FOR UPDATE"
        >>,
        [Rid, Jid, Id]
    ).
latest_tx(C, Jid) ->
    one(
        C,
        <<
            "SELECT *,lease_until>clock_timestamp() AS lease_valid,floor(extract(epoch from lease_until))::bigint AS lease_expires_at_unix FROM talk_record_client_run"
            " WHERE job_id=$1 ORDER BY attempt DESC LIMIT 1 FOR UPDATE"
        >>,
        [Jid]
    ).
parts_tx(C, RunId) ->
    elib_pg:query(
        C,
        <<"SELECT * FROM talk_record_client_run_part WHERE run_id=$1 ORDER BY seq FOR UPDATE">>,
        [RunId]
    ).

insert_run_tx(C, Data) -> insert(C, <<"talk_record_client_run">>, Data).
insert_part_tx(C, Data) -> insert(C, <<"talk_record_client_run_part">>, Data).
freeze_plan_tx(C, Id, Hash) ->
    elib_pg:execute(
        C,
        <<
            "UPDATE talk_record_client_run SET chunk_plan_hash=$2,revision=revision+1,updated_at=now()"
            " WHERE id=$1 AND state='active' AND chunk_plan_hash IS NULL"
        >>,
        [Id, Hash]
    ).
touch_tx(C, Id) ->
    elib_pg:execute(
        C,
        <<"UPDATE talk_record_client_run SET revision=revision+1,updated_at=now() WHERE id=$1 AND state='active'">>,
        [Id]
    ).
heartbeat_tx(C, Id) ->
    elib_pg:execute(
        C,
        <<
            "UPDATE talk_record_client_run SET lease_until=clock_timestamp()+interval '5 minutes',"
            "revision=revision+1,updated_at=now() WHERE id=$1 AND state='active' AND lease_until>clock_timestamp()"
        >>,
        [Id]
    ).

claim_job_tx(C, Id, Attempt) ->
    elib_pg:execute(
        C,
        <<
            "UPDATE talk_record_job SET state='running',attempt=$2,suspended=false,finished_at=NULL,"
            "lease_owner=NULL,lease_token=NULL,lease_until=NULL,error_code=NULL,updated_at=now()"
            " WHERE id=$1 AND execution_mode='client_byok' AND state IN ('queued','retry_wait','failed')"
        >>,
        [Id, Attempt]
    ).
finish_job_tx(C, Id, State) ->
    elib_pg:execute(
        C,
        <<
            "UPDATE talk_record_job SET state=$2,finished_at=now(),suspended=false,updated_at=now()"
            " WHERE id=$1 AND execution_mode='client_byok' AND state='running'"
        >>,
        [Id, State]
    ).

dispatch_tx(C, RunId, Seq, Intent, Hash) ->
    elib_pg:execute(
        C,
        <<
            "UPDATE talk_record_client_run_part SET dispatch_intent_id=$3,request_hash=$4,"
            "provider_state='dispatching',revision=revision+1 WHERE run_id=$1 AND seq=$2"
            " AND provider_state='not_dispatched'"
        >>,
        [RunId, Seq, Intent, Hash]
    ).
result_tx(C, RunId, Seq, Hash, {Result, Trace}) ->
    elib_pg:execute(
        C,
        <<
            "UPDATE talk_record_client_run_part SET provider_state='confirmed',result_hash=$3,"
            "result=$4::jsonb,provider_request_id=$5,revision=revision+1 WHERE run_id=$1 AND seq=$2"
            " AND provider_state='dispatching'"
        >>,
        [RunId, Seq, Hash, jsone:encode(Result), Trace]
    ).

settle_tx(C, Id, State, Code) ->
    {ok, _} = elib_pg:execute(
        C,
        <<
            "UPDATE talk_record_client_run_part SET provider_state='provider_outcome_unknown',revision=revision+1"
            " WHERE run_id=$1 AND provider_state='dispatching'"
        >>,
        [Id]
    ),
    {ok, 1} = elib_pg:execute(
        C,
        <<
            "UPDATE talk_record_client_run SET state=$2,revision=revision+1,updated_at=now()"
            " WHERE id=$1 AND state='active'"
        >>,
        [Id, State]
    ),
    {ok, N} = elib_pg:execute(
        C,
        <<
            "UPDATE talk_record_job SET state='failed',finished_at=now(),suspended=true,error_code=$2,updated_at=now()"
            " WHERE id=(SELECT job_id FROM talk_record_client_run WHERE id=$1)"
            " AND execution_mode='client_byok' AND state='running'"
        >>,
        [Id, Code]
    ),
    case N of
        1 ->
            {ok, 1};
        0 ->
            {ok, [#{<<"state">> := <<"cancelled">>}]} = elib_pg:query(
                C,
                <<"SELECT state FROM talk_record_job WHERE id=(SELECT job_id FROM talk_record_client_run WHERE id=$1)">>,
                [Id]
            ),
            {ok, 1}
    end.

complete_tx(C, Id) ->
    elib_pg:execute(
        C,
        <<
            "UPDATE talk_record_client_run SET state='complete',revision=revision+1,updated_at=now()"
            " WHERE id=$1 AND state='active' AND lease_until>clock_timestamp()"
        >>,
        [Id]
    ).
previous_tx(C, Jid, Id) ->
    one(
        C,
        <<
            "SELECT * FROM talk_record_client_run WHERE job_id=$1"
            " AND attempt<(SELECT attempt FROM talk_record_client_run WHERE job_id=$1 AND id=$2)"
            " ORDER BY attempt DESC LIMIT 1 FOR UPDATE"
        >>,
        [Jid, Id]
    ).

expire_tx(C, #{<<"state">> := <<"active">>, <<"lease_valid">> := false, <<"id">> := Id}) ->
    {ok, Parts} = parts_tx(C, Id),
    Unknown = lists:any(
        fun(P) ->
            lists:member(
                maps:get(<<"provider_state">>, P),
                [<<"dispatching">>, <<"provider_outcome_unknown">>]
            )
        end,
        Parts
    ),
    State =
        case Unknown of
            true -> <<"provider_outcome_unknown">>;
            false -> <<"expired">>
        end,
    Code =
        case Unknown of
            true -> <<"provider_outcome_unknown">>;
            false -> <<"client_lease_expired">>
        end,
    {ok, 1} = settle_tx(C, Id, State, Code),
    {expired, State};
expire_tx(_, _) ->
    current.

sources_tx(C, Rid, <<"transcribe">>) ->
    case
        elib_pg:query(
            C,
            <<"SELECT segments AS parts,total_duration_ms FROM talk_record_assembly WHERE record_id=$1">>,
            [Rid]
        )
    of
        {ok, [Row]} -> {ok, Row#{<<"parts">> => json(maps:get(<<"parts">>, Row))}};
        _ -> {error, media_not_ready}
    end;
sources_tx(C, Rid, <<"summarize">>) ->
    case
        elib_pg:query(
            C,
            <<
                "SELECT s.id,s.start_ms,s.end_ms,s.text FROM talk_record_segment s JOIN talk_record r"
                " ON r.id=s.record_id JOIN talk_record_transcript t ON t.id=r.current_transcript_id"
                " WHERE r.id=$1 AND s.transcript_version=t.version ORDER BY s.seq LIMIT 20001"
            >>,
            [Rid]
        )
    of
        {ok, Segs} when Segs =/= [], length(Segs) =< 20000 -> {ok, #{<<"segments">> => Segs}};
        _ -> {error, media_not_ready}
    end.

consent_tx(C, Rid, Uid, Kind, {PV, CV}) ->
    Id = elib_tsid:generate(attachment),
    {ok, 1} = elib_pg:execute(
        C,
        <<
            "INSERT INTO talk_record_consent_event(id,record_id,actor_id,kind,policy_version,processing_mode,"
            "processor_config_version,captured_at) VALUES($1,$2,$3,'processing_opt_in',$4,$5,$6,now())"
        >>,
        [Id, Rid, Uid, PV, Kind, CV]
    ),
    {ok, Id}.

reuse_tx(C, NewId, OldId) ->
    elib_pg:execute(
        C,
        <<
            "UPDATE talk_record_client_run_part n SET provider_state='confirmed',dispatch_intent_id=o.dispatch_intent_id,"
            "request_hash=o.request_hash,result_hash=o.result_hash,result=o.result,provider_request_id=o.provider_request_id,"
            "reused_from_run_id=o.run_id,reused_from_seq=o.seq,revision=n.revision+1"
            " FROM talk_record_client_run_part o WHERE n.run_id=$1 AND o.run_id=$2 AND n.seq=o.seq"
            " AND n.provider_state='not_dispatched' AND o.provider_state='confirmed'"
            " AND n.source_part_manifest_hash=o.source_part_manifest_hash AND n.source_ranges=o.source_ranges"
            " AND n.offset_ms=o.offset_ms AND n.duration_ms=o.duration_ms AND n.byte_size=o.byte_size"
        >>,
        [NewId, OldId]
    ).

insert(C, Table, Data) ->
    {Sql, Params} = elib_pg_sql:insert(Table, Data),
    elib_pg:execute(C, Sql, Params).
one(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, [R]} -> {ok, R};
        {ok, []} -> {error, not_found};
        E -> E
    end.
json(B) when is_binary(B) -> jsone:decode(B);
json(M) -> M.

expired_candidates() ->
    elib_pg:query(
        <<
            "SELECT record_id,job_id,id FROM talk_record_client_run"
            " WHERE state='active' AND lease_until<=clock_timestamp() ORDER BY lease_until LIMIT 20"
        >>,
        []
    ).

%% The caller holds the record row lock before either admission query.
same_input_tx(C, Rid, Kind, Manifest, Revision, TV) ->
    elib_pg:query(
        C,
        <<
            "SELECT * FROM talk_record_job WHERE record_id=$1 AND kind=$2"
            " AND execution_mode='client_byok' AND input_manifest_hash=$3"
            " AND input_content_revision=$4 AND input_transcript_version IS NOT DISTINCT FROM $5"
            " ORDER BY id FOR UPDATE"
        >>,
        [Rid, Kind, Manifest, Revision, TV]
    ).

active_count_tx(C, Rid) ->
    one(
        C,
        <<
            "SELECT count(*)::bigint AS count FROM talk_record_job WHERE record_id=$1"
            " AND execution_mode='client_byok' AND state IN ('queued','retry_wait','running')"
        >>,
        [Rid]
    ).
