-module(talk_record_provider_repo).
-moduledoc "Durable supplier facts; transaction caller holds record lock before job/run/task.".
-export([
    snapshot_tx/2,
    task_tx/3,
    origin_tx/4,
    insert_tx/2,
    update_tx/3,
    query_intent_tx/2,
    cancel_tx/2,
    paid_tasks_tx/2,
    latest_task_tx/2
]).

snapshot_tx(C, Rid) ->
    case talk_record_client_audio_repo:assembly_tx(C, Rid) of
        {ok, A} ->
            Aid = maps:get(<<"audio_attachment_id">>, A),
            case talk_record_client_audio_repo:artifact_tx(C, Rid, Aid) of
                {ok, F} -> snapshot(A, F);
                E -> E
            end;
        E ->
            E
    end.
snapshot(A, F) ->
    try
        <<"live">> = maps:get(<<"lifecycle_state">>, F),
        <<"complete">> = maps:get(<<"write_state">>, F),
        <<"bound">> = maps:get(<<"ref_state">>, F),
        true = maps:get(<<"reference_valid">>, F),
        false = maps:get(<<"governance_purged">>, F),
        null = maps:get(<<"cipher">>, F),
        <<"audio/aac">> = maps:get(<<"mime_type">>, F),
        N = maps:get(<<"size">>, F),
        true = N > 0 andalso N < 134217728,
        D = maps:get(<<"total_duration_ms">>, A),
        true = D > 0 andalso D =< 7200000,
        {ok, #{
            <<"source_kind">> => <<"assembly">>,
            <<"parts">> => talk_record_client_gate:json(maps:get(<<"segments">>, A)),
            <<"assembly">> => #{
                <<"id">> => maps:get(<<"id">>, F),
                <<"sha256">> => maps:get(<<"file_hash256">>, F),
                <<"byte_size">> => N,
                <<"duration_ms">> => D,
                <<"mime_type">> => <<"audio/aac">>,
                <<"manifest_hash">> => maps:get(<<"manifest_hash">>, A)
            }
        }}
    catch
        _:_ -> {error, media_not_ready}
    end.

task_tx(C, Rid, Tid) ->
    one(C, <<"SELECT * FROM talk_record_provider_task WHERE record_id=$1 AND id=$2 FOR UPDATE">>, [
        Rid, Tid
    ]).
origin_tx(C, Rid, Jid, Run) ->
    one(
        C,
        <<"SELECT * FROM talk_record_provider_task WHERE record_id=$1 AND job_id=$2 AND source_run_id=$3 AND seq=0 FOR UPDATE">>,
        [Rid, Jid, Run]
    ).
insert_tx(C, D) ->
    Keys = lists:sort(maps:keys(D)),
    Sql = iolist_to_binary([
        "INSERT INTO talk_record_provider_task (",
        join(Keys),
        ") VALUES (",
        join([<<"$", (integer_to_binary(N))/binary>> || N <- lists:seq(1, length(Keys))]),
        ")"
    ]),
    elib_pg:execute(C, Sql, [stored(K, maps:get(K, D)) || K <- Keys]).
update_tx(C, Id, D) ->
    Allowed = [
        <<"provider_task_id">>,
        <<"state">>,
        <<"source_issued_at">>,
        <<"source_expires_at">>,
        <<"intent_id">>,
        <<"request_hash">>,
        <<"result">>,
        <<"result_hash">>
    ],
    true = maps:without(Allowed, D) =:= #{},
    Keys = lists:sort(maps:keys(D)),
    Sets = [
        <<K/binary, "=$", (integer_to_binary(N))/binary>>
     || {K, N} <- lists:zip(Keys, lists:seq(2, length(Keys) + 1))
    ],
    Sql = iolist_to_binary([
        "UPDATE talk_record_provider_task SET ",
        join(Sets),
        ",outcome_unknown_at=CASE WHEN state='prepared' THEN coalesce(outcome_unknown_at,now()) ELSE outcome_unknown_at END,",
        "revision=revision+1,updated_at=now() WHERE id=$1 AND cancelled_at IS NULL"
    ]),
    elib_pg:execute(C, Sql, [Id | [stored(K, maps:get(K, D)) || K <- Keys]]).
query_intent_tx(C, Id) ->
    elib_pg:execute(
        C,
        <<
            "UPDATE talk_record_provider_task SET query_count=query_count+1,next_query_at=clock_timestamp()+interval '2 seconds',"
            "revision=revision+1,updated_at=now() WHERE id=$1 AND cancelled_at IS NULL"
            " AND (next_query_at IS NULL OR next_query_at<=clock_timestamp()) AND query_count<10000"
        >>,
        [Id]
    ).
cancel_tx(C, Id) ->
    elib_pg:execute(
        C,
        <<"UPDATE talk_record_provider_task SET cancelled_at=coalesce(cancelled_at,now()),revision=revision+1,updated_at=now() WHERE id=$1">>,
        [Id]
    ).
stored(<<"assembly">>, V) -> jsone:encode(V);
stored(<<"request_profile">>, V) -> jsone:encode(V);
stored(<<"result">>, V) -> jsone:encode(V);
stored(_, V) -> V.
join(L) -> lists:join(<<",">>, L).
one(C, S, A) ->
    case elib_pg:query(C, S, A) of
        {ok, [R]} -> {ok, R};
        {ok, []} -> {error, not_found};
        _ -> {error, db_error}
    end.

paid_tasks_tx(C, Jid) ->
    elib_pg:query(
        C,
        <<"SELECT id FROM talk_record_provider_task WHERE job_id=$1 AND intent_id IS NOT NULL ORDER BY id FOR UPDATE">>,
        [Jid]
    ).

latest_task_tx(C, Jid) ->
    one(
        C,
        <<"SELECT * FROM talk_record_provider_task WHERE job_id=$1 ORDER BY id DESC LIMIT 1 FOR UPDATE">>,
        [Jid]
    ).
