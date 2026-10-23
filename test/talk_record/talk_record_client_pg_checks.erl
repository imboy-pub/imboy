-module(talk_record_client_pg_checks).
-moduledoc "Controller-only real PG checks; never connects, migrates or clears tables.".
-export([run/3, fixture/3, ds_flow/3, rollback_lock/4, retained_down/4]).
-include_lib("eunit/include/eunit.hrl").

%% Caller owns a dedicated formal-schema connection, synthetic live record/user,
%% and transaction. Caller must roll back all fixture writes after this function.
run(C, Rid, Uid) ->
    {Job, Run} = fixture(C, Rid, Uid),
    Id = maps:get(<<"id">>, Run),
    H = hash(),
    constraints(C, Job, Run),
    {ok, 1} = talk_record_client_repo:freeze_plan_tx(C, Id, H),
    {ok, 1} = talk_record_client_repo:insert_part_tx(C, chunk(Id)),
    reject(C, <<"UPDATE talk_record_client_run SET chunk_plan_hash=$2 WHERE id=$1">>, [
        Id, binary:copy(<<"b">>, 64)
    ]),
    reject(C, <<"UPDATE talk_record_client_run SET device_session_id='other' WHERE id=$1">>, [Id]),
    reject(C, <<"UPDATE talk_record_client_run_part SET duration_ms=999 WHERE run_id=$1">>, [Id]),
    {ok, 1} = talk_record_client_repo:dispatch_tx(C, Id, 0, elib_tsid:generate(attachment), H),
    reject(
        C,
        <<"UPDATE talk_record_client_run_part SET dispatch_intent_id=dispatch_intent_id+1 WHERE run_id=$1">>,
        [Id]
    ),
    reject(
        C,
        <<"UPDATE talk_record_client_run_part SET provider_state='not_dispatched',dispatch_intent_id=NULL,request_hash=NULL WHERE run_id=$1">>,
        [Id]
    ),
    {ok, 1} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_client_run SET lease_until=clock_timestamp()-interval '1 second' WHERE id=$1">>,
        [Id]
    ),
    {ok, Expired} = talk_record_client_repo:lock_run_tx(C, Rid, Job, Id),
    ?assertEqual(
        {expired, <<"provider_outcome_unknown">>}, talk_record_client_repo:expire_tx(C, Expired)
    ),
    {ok, [P]} = talk_record_client_repo:parts_tx(C, Id),
    ?assertEqual(<<"provider_outcome_unknown">>, maps:get(<<"provider_state">>, P)),
    reject(C, <<"UPDATE talk_record_client_run SET state='active' WHERE id=$1">>, [Id]),
    ?assertEqual({ok, 0}, talk_record_client_repo:result_tx(C, Id, 0, H, {#{}, <<>>})),
    cancellation(C, Rid, Uid),
    confirmed_terminal(C, Rid, Uid),
    historical_reuse(C, Rid, Uid),
    {ok, #{
        checks => [
            foreign_keys,
            active_unique,
            immutable_pins,
            immutable_plan,
            durable_intent,
            expiry_unknown,
            terminal_unknown,
            late_result,
            cancel_trigger,
            confirmed_terminal,
            historical_reuse
        ]
    }}.

cancellation(C, Rid, Uid) ->
    {Job, Run} = fixture(C, Rid, Uid),
    Id = maps:get(<<"id">>, Run),
    {ok, 1} = talk_record_client_repo:freeze_plan_tx(C, Id, hash()),
    {ok, 1} = talk_record_client_repo:insert_part_tx(C, chunk(Id)),
    {ok, 1} = talk_record_client_repo:dispatch_tx(C, Id, 0, elib_tsid:generate(attachment), hash()),
    {ok, cancelled} = talk_record_job_repo:cancel_job_tx(C, Job),
    {ok, After} = talk_record_client_repo:lock_run_tx(C, Rid, Job, Id),
    ?assertEqual(<<"provider_outcome_unknown">>, maps:get(<<"state">>, After)),
    {ok, [Part]} = talk_record_client_repo:parts_tx(C, Id),
    ?assertEqual(<<"provider_outcome_unknown">>, maps:get(<<"provider_state">>, Part)).

fixture(C, Rid, Uid) ->
    Job = elib_tsid:generate(attachment),
    Id = elib_tsid:generate(attachment),
    {ok, created, Job} = talk_record_job_repo:enqueue_tx(C, #{
        id => Job,
        record_id => Rid,
        kind => <<"transcribe">>,
        execution_mode => <<"client_byok">>,
        dedupe_key => integer_to_binary(Job),
        input_manifest_hash => input_hash(Job),
        input_content_revision => 1,
        processor_input => #{
            <<"processing_mode">> => <<"transcribe">>,
            <<"provider">> => <<"minimax">>,
            <<"model">> => <<"asr-1.0">>,
            <<"policy_version">> => <<"pg-check">>,
            <<"config_version">> => hash()
        }
    }),
    {ok, 1} = talk_record_client_repo:claim_job_tx(C, Job, 1),
    {ok, Consent} = talk_record_client_repo:consent_tx(
        C, Rid, Uid, <<"transcribe">>, {<<"pg-check">>, hash()}
    ),
    Data = #{
        <<"id">> => Id,
        <<"record_id">> => Rid,
        <<"job_id">> => Job,
        <<"attempt">> => 1,
        <<"actor_user_id">> => Uid,
        <<"device_session_id">> => <<"synthetic-pg-check">>,
        <<"lease_token_hash">> => hash(),
        <<"lease_until">> => elib_dt:add(elib_dt:now(), {300, second}),
        <<"provider">> => <<"minimax">>,
        <<"model">> => <<"asr-1.0">>,
        <<"policy_version">> => <<"pg-check">>,
        <<"config_version">> => hash(),
        <<"consent_event_id">> => Consent,
        <<"input_manifest_hash">> => input_hash(Job),
        <<"input_content_revision">> => 1,
        <<"input_snapshot">> => jsone:encode(snapshot())
    },
    {ok, 1} = talk_record_client_repo:insert_run_tx(C, Data),
    {ok, Run} = talk_record_client_repo:lock_run_tx(C, Rid, Job, Id),
    {Job, Run}.

chunk(Id) ->
    #{
        <<"run_id">> => Id,
        <<"seq">> => 0,
        <<"source_part_manifest_hash">> => hash(),
        <<"source_ranges">> => <<"[]">>,
        <<"offset_ms">> => 0,
        <<"duration_ms">> => 1000,
        <<"byte_size">> => 100
    }.
hash() -> binary:copy(<<"a">>, 64).
reject(C, Sql, Args) ->
    {ok, []} = elib_pg:query(C, <<"SAVEPOINT client_contract_rejection">>, []),
    Result = elib_pg:query(C, Sql, Args),
    {ok, []} = elib_pg:query(C, <<"ROLLBACK TO SAVEPOINT client_contract_rejection">>, []),
    {ok, []} = elib_pg:query(C, <<"RELEASE SAVEPOINT client_contract_rejection">>, []),
    ?assertMatch({error, {error, _, <<"23514">>, _, _, _}}, Result).

input_hash(Job) -> talk_record_client_contract:hash(#{<<"job">> => Job}).

constraints(C, Job, Run) ->
    Id = maps:get(<<"id">>, Run),
    reject_code(
        C,
        <<"23503">>,
        <<
            "INSERT INTO talk_record_client_run_part"
            "(run_id,seq,source_part_manifest_hash,source_ranges,offset_ms,duration_ms,byte_size)"
            " VALUES(-223,0,$1,'[]',0,1,1)"
        >>,
        [hash()]
    ),
    reject_code(
        C,
        <<"23503">>,
        <<
            "INSERT INTO talk_record_client_run_part"
            "(run_id,seq,source_part_manifest_hash,source_ranges,offset_ms,duration_ms,byte_size,reused_from_run_id,reused_from_seq)"
            " VALUES($1,1,$2,'[]',0,1,1,-223,0)"
        >>,
        [Id, hash()]
    ),
    %% PostgreSQL raises 23001 restrict_violation when a DELETE hits rows still
    %% referenced through an ON DELETE RESTRICT foreign key; 23503
    %% foreign_key_violation is only returned for INSERT/UPDATE pointing at
    %% missing rows. Verified on the sealed-template chain under PG 18.1.
    reject_code(C, <<"23001">>, <<"DELETE FROM talk_record_consent_event WHERE id=$1">>, [
        maps:get(<<"consent_event_id">>, Run)
    ]),
    reject_code(C, <<"23001">>, <<"DELETE FROM talk_record_job WHERE id=$1">>, [Job]),
    {ok, Rows} = elib_pg:query(
        C,
        <<
            "SELECT pg_get_constraintdef(oid) AS definition FROM pg_constraint"
            " WHERE conrelid IN ('talk_record_client_run'::regclass,'talk_record_client_run_part'::regclass)"
            " AND contype='f'"
        >>,
        []
    ),
    Definitions = iolist_to_binary([maps:get(<<"definition">>, Row) || Row <- Rows]),
    lists:foreach(
        fun(Reference) -> ?assertNotEqual(nomatch, binary:match(Definitions, Reference)) end,
        [
            <<"REFERENCES talk_record_job(record_id, id)">>,
            <<"REFERENCES talk_record_consent_event(id)">>,
            <<"REFERENCES talk_record_client_run_part(run_id, seq)">>,
            <<"REFERENCES talk_record_client_run(id)">>,
            <<"REFERENCES \"user\"(id)">>
        ]
    ),
    {ok, 1} = elib_pg:execute(C, <<"UPDATE talk_record_job SET attempt=2 WHERE id=$1">>, [Job]),
    Result = reject_code(C, <<"23505">>, duplicate_run_sql(), [
        elib_tsid:generate(attachment), 2, Id
    ]),
    ?assertNotEqual(nomatch, binary:match(term_to_binary(Result), <<"uq_trcr_active">>)),
    {ok, 1} = elib_pg:execute(C, <<"UPDATE talk_record_job SET attempt=1 WHERE id=$1">>, [Job]).

duplicate_run_sql() ->
    <<
        "INSERT INTO talk_record_client_run(id,record_id,job_id,attempt,actor_user_id,device_session_id,"
        "lease_token_hash,lease_until,provider,model,policy_version,config_version,consent_event_id,"
        "input_manifest_hash,input_content_revision,input_transcript_version,input_snapshot)"
        " SELECT $1,record_id,job_id,$2,actor_user_id,device_session_id,lease_token_hash,"
        "clock_timestamp()+interval '5 minutes',provider,model,policy_version,config_version,consent_event_id,"
        "input_manifest_hash,input_content_revision,input_transcript_version,input_snapshot"
        " FROM talk_record_client_run WHERE id=$3"
    >>.

reject_code(C, Code, Sql, Args) ->
    {ok, []} = elib_pg:query(C, <<"SAVEPOINT client_expected_error">>, []),
    Result = elib_pg:query(C, Sql, Args),
    {ok, []} = elib_pg:query(C, <<"ROLLBACK TO SAVEPOINT client_expected_error">>, []),
    {ok, []} = elib_pg:query(C, <<"RELEASE SAVEPOINT client_expected_error">>, []),
    ?assertMatch({error, {error, _, Code, _, _, _}}, Result),
    Result.

confirmed_terminal(C, Rid, Uid) ->
    {Job, Run} = fixture(C, Rid, Uid),
    Id = maps:get(<<"id">>, Run),
    {ok, 1} = talk_record_client_repo:insert_part_tx(C, chunk(Id)),
    {ok, 1} = talk_record_client_repo:dispatch_tx(C, Id, 0, elib_tsid:generate(attachment), hash()),
    {ok, 1} = talk_record_client_repo:result_tx(C, Id, 0, hash(), {#{}, <<"synthetic">>}),
    reject(
        C,
        <<"UPDATE talk_record_client_run_part SET result='{}',result_hash=$2 WHERE run_id=$1">>,
        [Id, binary:copy(<<"b">>, 64)]
    ),
    reject(
        C,
        <<"UPDATE talk_record_client_run_part SET dispatch_intent_id=dispatch_intent_id+1 WHERE run_id=$1">>,
        [Id]
    ),
    {ok, 1} = talk_record_client_repo:complete_tx(C, Id),
    reject(C, <<"UPDATE talk_record_client_run SET revision=revision+1 WHERE id=$1">>, [Id]),
    {ok, 1} = talk_record_client_repo:finish_job_tx(C, Job, <<"succeeded">>).

snapshot() ->
    #{
        <<"parts">> => [
            #{
                <<"part_id">> => 223,
                <<"sha256">> => hash(),
                <<"offset_ms">> => 0,
                <<"duration_ms">> => 2000,
                <<"byte_size">> => 200
            }
        ]
    }.

plan() ->
    [Part] = maps:get(<<"parts">>, snapshot()),
    #{
        <<"overlap_rule">> => <<"none">>,
        <<"chunks">> => [
            #{
                <<"seq">> => Seq,
                <<"source_part_manifest_hash">> => talk_record_client_contract:hash(Part),
                <<"source_ranges">> => [
                    #{
                        <<"part_id">> => 223,
                        <<"sha256">> => hash(),
                        <<"start_ms">> => Seq * 1000,
                        <<"end_ms">> => (Seq + 1) * 1000
                    }
                ],
                <<"offset_ms">> => Seq * 1000,
                <<"duration_ms">> => 1000,
                <<"byte_size">> => 100
            }
         || Seq <- [0, 1]
        ]
    }.

wire_result() ->
    #{
        <<"model_version">> => <<"asr-1.0">>,
        <<"status">> => <<"ready">>,
        <<"speakers">> => [#{<<"label">> => <<"speaker">>}],
        <<"segments">> => [
            #{
                <<"start_ms">> => 0,
                <<"end_ms">> => 500,
                <<"speaker_label">> => <<"speaker">>,
                <<"text">> => <<"synthetic">>
            }
        ]
    }.

result_body() ->
    Wire = wire_result(),
    #{
        <<"result">> => Wire,
        <<"result_hash">> => talk_record_client_contract:hash(Wire),
        <<"provider_request_id">> => <<"synthetic-223">>
    }.

operation(C, Action, Record, Job, Id, Seq, Body) ->
    {ok, Run} = talk_record_client_repo:lock_run_tx(
        C, maps:get(<<"id">>, Record), maps:get(<<"id">>, Job), Id
    ),
    talk_record_client_operations:perform(C, Action, Record, Job, Run, Seq, Body).

dispatch_body(C, Id, Seq) ->
    {ok, Run} = talk_record_client_repo:latest_tx(C, run_job(C, Id)),
    {ok, Parts} = talk_record_client_repo:parts_tx(C, Id),
    [Part] = [Part || Part <- Parts, maps:get(<<"seq">>, Part) =:= Seq],
    #{
        <<"chunk_plan_hash">> => maps:get(<<"chunk_plan_hash">>, Run),
        <<"request_hash">> => talk_record_client_operations:request_hash(Run, Part, Seq)
    }.

run_job(C, Id) ->
    {ok, [#{<<"job_id">> := Job}]} = elib_pg:query(
        C, <<"SELECT job_id FROM talk_record_client_run WHERE id=$1">>, [Id]
    ),
    Job.

expire(C, Rid, Job, Id) ->
    {ok, 1} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_client_run SET lease_until=clock_timestamp()-interval '1 second' WHERE id=$1">>,
        [Id]
    ),
    {ok, Run} = talk_record_client_repo:lock_run_tx(C, Rid, Job, Id),
    ?assertEqual({expired, <<"expired">>}, talk_record_client_repo:expire_tx(C, Run)).

successor(C, Job, OldId, Attempt) ->
    Id = elib_tsid:generate(attachment),
    {ok, 1} = talk_record_client_repo:claim_job_tx(C, Job, Attempt),
    {ok, 1} = elib_pg:execute(C, duplicate_run_sql(), [Id, Attempt, OldId]),
    Id.

historical_reuse(C, Rid, Uid) ->
    {JobId, First} = fixture(C, Rid, Uid),
    FirstId = maps:get(<<"id">>, First),
    {ok, Record} = talk_record_client_repo:lock_record_tx(C, Rid),
    {ok, Job} = talk_record_client_repo:lock_job_tx(C, Rid, JobId),
    {ok, created, _} = operation(C, plan, Record, Job, FirstId, 0, plan()),
    {ok, created, #{<<"may_dispatch">> := true}} = operation(
        C, dispatch, Record, Job, FirstId, 0, dispatch_body(C, FirstId, 0)
    ),
    {ok, created, _} = operation(C, result, Record, Job, FirstId, 0, result_body()),
    expire(C, Rid, JobId, FirstId),
    EmptyId = successor(C, JobId, FirstId, 2),
    expire(C, Rid, JobId, EmptyId),
    ThirdId = successor(C, JobId, FirstId, 3),
    {ok, created, _} = operation(C, plan, Record, Job, ThirdId, 0, plan()),
    {ok, [Reused, _]} = talk_record_client_repo:parts_tx(C, ThirdId),
    ?assertEqual(<<"confirmed">>, maps:get(<<"provider_state">>, Reused)),
    ?assertEqual(FirstId, maps:get(<<"reused_from_run_id">>, Reused)),
    {ok, created, #{<<"may_dispatch">> := false}} = operation(
        C, dispatch, Record, Job, ThirdId, 0, dispatch_body(C, ThirdId, 0)
    ),
    expire(C, Rid, JobId, ThirdId).

ds_flow(C, _ParentRid, Uid) ->
    Previous = application:get_env(imboy, talk_record_client_processing),
    application:set_env(imboy, talk_record_client_processing, #{
        enabled => true,
        policy_version => <<"pg-check">>,
        signing_key => binary:copy(<<"synthetic-offline-pg-key">>, 2)
    }),
    try
        ds_request_flow(C, Uid)
    after
        restore_policy(Previous)
    end.

restore_policy({ok, Value}) -> application:set_env(imboy, talk_record_client_processing, Value);
restore_policy(undefined) -> application:unset_env(imboy, talk_record_client_processing).

%% The full talk_record schema carries UNIQUE (created_by, client_recording_id)
%% (uq_tr_creator_client) and the caller-seeded parent row already occupies the
%% fixed synthetic uuid for this user, so this flow derives its recording id
%% from the fresh record TSID: the (created_by, client_recording_id) pair is
%% then guaranteed distinct without changing what the ds request flow verifies.
%% Hex text because epgsql's uuid codec encodes from hex digits.
ds_recording_id(Rid) ->
    binary:encode_hex(<<0:32, 223:32, Rid:64/big-unsigned-integer>>, lowercase).

ds_request_flow(C, Uid) ->
    Rid = elib_tsid:generate(attachment),
    Capture = elib_tsid:generate(attachment),
    {ok, 1} = elib_pg:execute(
        C,
        <<
            "INSERT INTO talk_record(id,owner_user_id,created_by,client_recording_id,scope,media_status)"
            " VALUES($1,$2,$2,$3,'personal','available')"
        >>,
        [Rid, Uid, ds_recording_id(Rid)]
    ),
    {ok, 1} = elib_pg:execute(
        C,
        <<
            "INSERT INTO talk_record_capture(id,record_id,device_session_id,device_id,started_by,status)"
            " VALUES($1,$2,'00000000-0000-0000-0000-000000000223','pg-check',$3,'ended')"
        >>,
        [Capture, Rid, Uid]
    ),
    {ok, 1} = elib_pg:execute(
        C,
        <<
            "INSERT INTO talk_record_assembly(id,record_id,capture_id,manifest_hash,segments,"
            "part_count,total_duration_ms,total_bytes) VALUES($1,$2,$3,$4,$5::jsonb,1,2000,200)"
        >>,
        [
            elib_tsid:generate(attachment),
            Rid,
            Capture,
            hash(),
            jsone:encode(maps:get(<<"parts">>, snapshot()))
        ]
    ),
    Context = #{uid => Uid, did => <<"synthetic-pg-check">>},
    Public = talk_record_client_policy:public(),
    Request = maps:with([<<"policy_version">>, <<"config_version">>], Public),
    Pin = Request#{
        <<"execution_mode">> => <<"client_byok">>,
        <<"provider">> => <<"minimax">>,
        <<"model">> => <<"asr-1.0">>
    },
    Consent = Pin#{<<"confirmed">> => true},
    Body = Pin#{<<"mode">> => <<"transcribe">>, <<"processing_consent">> => Consent},
    Headers = #{if_match => <<"1">>, idempotency_key => <<"synthetic-client-request-223">>},
    {ok, created, Created} = talk_record_client_requests:create_tx(
        C, Context, Rid, Headers, {Body, unused}
    ),
    {ok, replay, Created} = talk_record_client_requests:create_tx(
        C, Context, Rid, Headers, {Body, unused}
    ),
    Jid = maps:get(<<"job_id">>, Created),
    Record = talk_record_client_gate:record_tx(C, Context, Rid, write),
    {Job, _} = talk_record_client_gate:job_tx(C, Record, Jid),
    {ok, created, Claim} = talk_record_client_claim:create_tx(C, Context, Record, Job, #{}, #{
        <<"processing_consent">> => Consent
    }),
    Id = maps:get(<<"id">>, Claim),
    {ok, CurrentJob} = talk_record_client_repo:lock_job_tx(C, Rid, Jid),
    {ok, Run} = talk_record_client_repo:lock_run_tx(C, Rid, Jid, Id),
    {ok, Config} = talk_record_client_policy:get(),
    Token = talk_record_client_policy:token(
        maps:get(signing_key, Config), Id, 1, Uid, maps:get(did, Context)
    ),
    ok = talk_record_client_gate:run_tx(C, Context, CurrentJob, Run, Token),
    ?assertThrow(
        {talk_client_abort, forbidden},
        talk_record_client_gate:run_tx(C, Context#{did => <<"other">>}, CurrentJob, Run, Token)
    ),
    ?assertThrow(
        {talk_client_abort, lease_lost},
        talk_record_client_gate:run_tx(C, Context, CurrentJob, Run, binary:copy(<<"x">>, 32))
    ),
    {ok, created, _} = operation(C, heartbeat, Record, CurrentJob, Id, 0, #{}),
    {ok, created, _} = operation(C, plan, Record, CurrentJob, Id, 0, plan()),
    lists:foreach(
        fun(Seq) ->
            {ok, created, #{<<"may_dispatch">> := true}} = operation(
                C, dispatch, Record, CurrentJob, Id, Seq, dispatch_body(C, Id, Seq)
            ),
            {ok, created, _} = operation(C, result, Record, CurrentJob, Id, Seq, result_body())
        end,
        [0, 1]
    ),
    {ok, created, #{<<"state">> := <<"complete">>}} = operation(
        C, complete, Record, CurrentJob, Id, 0, #{}
    ),
    {ok, Finished} = talk_record_client_repo:lock_job_tx(C, Rid, Jid),
    ?assertEqual(<<"succeeded">>, maps:get(<<"state">>, Finished)),
    {ok, Segments} = elib_pg:query(
        C, <<"SELECT start_ms,end_ms FROM talk_record_segment WHERE record_id=$1 ORDER BY seq">>, [
            Rid
        ]
    ),
    ?assertEqual(
        [
            #{<<"start_ms">> => 0, <<"end_ms">> => 500},
            #{<<"start_ms">> => 1000, <<"end_ms">> => 1500}
        ],
        Segments
    ),
    {ok, #{
        checks => [
            request_idempotency,
            live_personal_acl,
            claim,
            device_token_fence,
            heartbeat,
            plan,
            dispatch,
            result,
            complete,
            publication
        ]
    }}.

rollback_lock(MigrationC, OtherC, RunId, DownPath) ->
    {ok, Sql} = file:read_file(DownPath),
    [LockSql, _] = binary:split(Sql, <<";">>),
    ?assertNotEqual(nomatch, binary:match(LockSql, <<"IN ACCESS EXCLUSIVE MODE">>)),
    {ok, []} = elib_pg:query(MigrationC, LockSql, []),
    {ok, [#{<<"held">> := 2}]} = elib_pg:query(
        MigrationC,
        <<
            "SELECT count(*) AS held FROM pg_locks"
            " WHERE pid=pg_backend_pid() AND granted AND mode='AccessExclusiveLock'"
            " AND relation IN ('talk_record_client_run'::regclass,'talk_record_client_run_part'::regclass)"
        >>,
        []
    ),
    {ok, []} = elib_pg:query(OtherC, <<"SET LOCAL lock_timeout='200ms'">>, []),
    reject_code(
        OtherC,
        <<"55P03">>,
        <<
            "INSERT INTO talk_record_client_run_part"
            "(run_id,seq,source_part_manifest_hash,source_ranges,offset_ms,duration_ms,byte_size)"
            " VALUES($1,0,$2,'[]',0,1,1)"
        >>,
        [RunId, hash()]
    ),
    {ok, concurrent_client_insert_blocked}.

retained_down(C, Rid, Uid, DownPath) ->
    _ = fixture(C, Rid, Uid),
    {ok, Sql} = file:read_file(DownPath),
    Results = epgsql:squery(C, Sql),
    ?assertMatch({error, {error, _, <<"P0001">>, _, _, _}}, lists:last(Results)),
    {ok, retention_refused}.
