-module(talk_record_client_admission_tests).
-include_lib("eunit/include/eunit.hrl").

new_idem_and_device_cannot_bypass_unknown_test() ->
    Modules = [
        talk_record_client_gate,
        talk_record_client_policy,
        talk_record_client_repo,
        talk_record_job_repo,
        talk_record_assembly_logic,
        talk_record_job_support,
        talk_record_idempotency_repo,
        elib_pg,
        elib_tsid,
        talk_record_ds
    ],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    try
        setup_request(),
        {Body, Parsed} = request(),
        Context = #{uid => 10, did => <<"device-a">>},
        First = talk_record_client_requests:create_tx(
            conn, Context, 7, headers(1, <<"first">>), {Body, Parsed}
        ),
        ?assertMatch({ok, created, #{<<"job_id">> := 99}}, First),
        Job = get(client_admission_job),
        put(client_admission_job, Job#{<<"state">> => <<"failed">>}),
        Second = talk_record_client_requests:create_tx(
            conn,
            Context#{uid => 11, did => <<"device-b">>},
            7,
            headers(2, <<"second">>),
            {Body, Parsed}
        ),
        ?assertMatch({ok, created, #{<<"job_id">> := 99}}, Second),
        ?assertEqual(1, meck:num_calls(talk_record_job_repo, enqueue_tx, '_')),
        ?assertEqual(1, meck:num_calls(talk_record_client_repo, consent_tx, '_')),
        meck:expect(talk_record_client_repo, latest_tx, fun(_, 99) ->
            {ok, #{
                <<"id">> => 88, <<"state">> => <<"provider_outcome_unknown">>, <<"revision">> => 4
            }}
        end),
        ?assertThrow(
            {talk_client_abort, async_protocol_required},
            talk_record_client_claim:create_tx(
                conn,
                Context#{did => <<"device-b">>},
                get(client_admission_record),
                get(client_admission_job),
                #{},
                #{}
            )
        ),
        ?assertEqual(0, meck:num_calls(talk_record_client_repo, claim_job_tx, '_'))
    after
        lists:foreach(fun meck:unload/1, Modules),
        erase(client_admission_record),
        erase(client_admission_job)
    end.

setup_request() ->
    R = #{
        <<"id">> => 7,
        <<"scope">> => <<"personal">>,
        <<"revision">> => 1,
        <<"content_revision">> => 5,
        <<"media_status">> => <<"available">>,
        <<"processing_mode">> => <<"record_only">>
    },
    put(client_admission_record, R),
    erase(client_admission_job),
    meck:expect(talk_record_client_gate, record_tx, fun(_, _, 7, write) ->
        get(client_admission_record)
    end),
    meck:expect(talk_record_client_policy, get, fun() -> {ok, #{public => policy_public()}} end),
    meck:expect(talk_record_assembly_logic, find_tx, fun(_, 7) ->
        {ok, #{<<"manifest_hash">> => hash()}}
    end),
    meck:expect(talk_record_job_support, summary_snapshot_tx, fun(_, 7) -> null end),
    meck:expect(talk_record_ds, capabilities, fun() -> #{<<"queue_limit">> => 2} end),
    setup_storage().

setup_storage() ->
    meck:expect(talk_record_client_repo, same_input_tx, fun(_, 7, <<"transcribe">>, H, 5, null) ->
        ?assertEqual(hash(), H),
        case get(client_admission_job) of
            undefined -> {ok, []};
            J -> {ok, [J]}
        end
    end),
    meck:expect(talk_record_client_repo, active_count_tx, fun(_, 7) -> {ok, #{<<"count">> => 0}} end),
    meck:expect(talk_record_client_repo, consent_tx, fun(_, 7, 10, <<"transcribe">>, _) ->
        {ok, 77}
    end),
    meck:expect(elib_tsid, generate, fun(attachment) -> 99 end),
    meck:expect(talk_record_job_repo, enqueue_tx, fun(_, J) ->
        put(client_admission_job, #{
            <<"id">> => maps:get(id, J),
            <<"state">> => <<"queued">>,
            <<"processor_input">> => maps:get(processor_input, J)
        }),
        {ok, created, maps:get(id, J)}
    end),
    meck:expect(elib_pg, execute, fun(_, Sql, [7, Mode]) ->
        ?assertNotEqual(nomatch, binary:match(Sql, <<"UPDATE talk_record SET processing_mode">>)),
        R = get(client_admission_record),
        put(client_admission_record, R#{
            <<"revision">> => maps:get(<<"revision">>, R) + 1,
            <<"processing_mode">> => Mode
        }),
        {ok, 1}
    end),
    meck:expect(talk_record_idempotency_repo, record_tx, fun(_, _, _, _, _, _, _) ->
        {ok, inserted}
    end),
    meck:expect(talk_record_idempotency_repo, complete_tx, fun(_, _, _, _, _, _) -> ok end).

same_input_policy_change_cannot_mint_job_test() ->
    with_admission(fun() ->
        {_, P} = request(),
        J = #{
            <<"id">> => 99,
            <<"state">> => <<"failed">>,
            <<"processor_input">> => #{
                <<"provider">> => <<"doubao">>,
                <<"model">> => <<"volc.seedasr.auc">>,
                <<"policy_version">> => <<"old">>,
                <<"config_version">> => hash(),
                <<"processing_mode">> => <<"transcribe">>
            }
        },
        meck:expect(talk_record_client_repo, same_input_tx, fun(_, _, _, _, _, _) -> {ok, [J]} end),
        ?assertThrow({talk_client_abort, client_execution_conflict}, choose(P)),
        ?assertEqual(0, meck:num_calls(talk_record_client_repo, active_count_tx, '_'))
    end).

queue_full_blocks_new_job_but_not_reuse_test() ->
    with_admission(fun() ->
        {_, P} = request(),
        meck:expect(talk_record_client_repo, same_input_tx, fun(_, _, _, _, _, _) -> {ok, []} end),
        meck:expect(talk_record_client_repo, active_count_tx, fun(_, 7) ->
            {ok, #{<<"count">> => 2}}
        end),
        ?assertThrow({talk_client_abort, client_queue_full}, choose(P)),
        J = #{<<"id">> => 99, <<"state">> => <<"running">>, <<"processor_input">> => pin()},
        meck:expect(talk_record_client_repo, same_input_tx, fun(_, _, _, _, _, _) -> {ok, [J]} end),
        ?assertEqual({existing, J}, choose(P)),
        ?assertEqual(1, meck:num_calls(talk_record_client_repo, active_count_tx, '_')),
        ?assertThrow(
            {talk_client_abort, client_queue_full},
            talk_record_client_admission:claim_tx(
                conn,
                #{<<"id">> => 7},
                #{<<"state">> => <<"failed">>}
            )
        )
    end).

multiple_preexisting_jobs_fail_closed_test() ->
    with_admission(fun() ->
        {_, P} = request(),
        meck:expect(talk_record_client_repo, same_input_tx, fun(_, _, _, _, _, _) ->
            {ok, [#{}, #{}]}
        end),
        ?assertThrow({talk_client_abort, client_execution_conflict}, choose(P))
    end).

input_dedupe_independent_of_request_identity_test() ->
    I = input(),
    K = talk_record_client_admission:dedupe_key(7, <<"transcribe">>, I),
    ?assertEqual(K, talk_record_client_admission:dedupe_key(7, <<"transcribe">>, I)),
    ?assertNotEqual(
        K, talk_record_client_admission:dedupe_key(7, <<"transcribe">>, I#{revision => 6})
    ),
    ?assertNotEqual(K, talk_record_client_admission:dedupe_key(8, <<"transcribe">>, I)),
    ?assertNotEqual(K, talk_record_client_admission:dedupe_key(7, <<"summarize">>, I)).

doubao_successor_requires_async_protocol_test() ->
    Modules = [
        talk_record_client_repo,
        talk_record_provider_repo,
        talk_record_client_policy,
        talk_record_ds,
        elib_tsid
    ],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    try
        Old = #{<<"id">> => 88, <<"state">> => <<"provider_outcome_unknown">>, <<"revision">> => 4},
        meck:expect(talk_record_client_repo, latest_tx, fun(_, 99) -> {ok, Old} end),
        meck:expect(talk_record_ds, capabilities, fun() -> #{<<"queue_limit">> => 2} end),
        meck:expect(talk_record_client_repo, active_count_tx, fun(_, 7) ->
            {ok, #{<<"count">> => 0}}
        end),
        meck:expect(talk_record_client_policy, get, fun() ->
            {ok, #{public => policy_public(), signing_key => binary:copy(<<"k">>, 32)}}
        end),
        meck:expect(talk_record_provider_repo, snapshot_tx, fun(_, 7) ->
            {ok, #{<<"parts">> => []}}
        end),
        meck:expect(talk_record_client_repo, consent_tx, fun(_, 7, 11, <<"transcribe">>, _) ->
            {ok, 77}
        end),
        meck:expect(elib_tsid, generate, fun(attachment) -> 100 end),
        meck:expect(talk_record_client_repo, claim_job_tx, fun(_, 99, 2) -> {ok, 1} end),
        meck:expect(talk_record_client_repo, insert_run_tx, fun(_, Run) ->
            ?assertEqual(88, maps:get(<<"replaces_run_id">>, Run)),
            ?assertEqual(true, maps:get(<<"ack_possible_duplicate_charge">>, Run)),
            ?assertEqual(<<"device-b">>, maps:get(<<"device_session_id">>, Run)),
            put(client_successor, Run#{<<"revision">> => 1}),
            {ok, 1}
        end),
        meck:expect(talk_record_client_repo, lock_run_tx, fun(_, 7, 99, 100) ->
            {ok, get(client_successor)}
        end),
        {Body, _} = request(),
        Claim = #{
            <<"processing_consent">> => maps:get(<<"processing_consent">>, Body),
            <<"previous_run_id">> => 88,
            <<"ack_possible_duplicate_charge">> => true
        },
        Job = #{
            <<"id">> => 99,
            <<"kind">> => <<"transcribe">>,
            <<"state">> => <<"failed">>,
            <<"attempt">> => 1,
            <<"processor_input">> => pin(),
            <<"input_manifest_hash">> => hash(),
            <<"input_content_revision">> => 5,
            <<"input_transcript_version">> => null
        },
        ?assertThrow(
            {talk_client_abort, async_protocol_required},
            talk_record_client_claim:create_tx(
                conn,
                #{uid => 11, did => <<"device-b">>},
                #{<<"id">> => 7},
                Job,
                #{if_match => <<"\"client-run:88:4\"">>},
                Claim
            )
        ),
        ?assertEqual(0, meck:num_calls(talk_record_client_repo, settle_tx, '_')),
        ?assertEqual(0, meck:num_calls(talk_record_client_repo, insert_run_tx, '_'))
    after
        lists:foreach(fun meck:unload/1, Modules),
        erase(client_successor)
    end.

already_admitted_claim_does_not_consume_another_slot_test() ->
    with_admission(fun() ->
        lists:foreach(
            fun(State) ->
                ?assertEqual(
                    ok,
                    talk_record_client_admission:claim_tx(
                        conn,
                        #{<<"id">> => 7},
                        #{<<"state">> => State}
                    )
                )
            end,
            [<<"queued">>, <<"retry_wait">>]
        ),
        ?assertEqual(0, meck:num_calls(talk_record_client_repo, active_count_tx, '_'))
    end).

with_admission(F) ->
    meck:new(talk_record_client_repo, [passthrough, no_link]),
    meck:new(talk_record_ds, [passthrough, no_link]),
    try
        meck:expect(talk_record_ds, capabilities, fun() -> #{<<"queue_limit">> => 2} end),
        F()
    after
        meck:unload(talk_record_client_repo),
        meck:unload(talk_record_ds)
    end.
choose(P) -> talk_record_client_admission:choose_tx(conn, #{<<"id">> => 7}, P, input()).
input() -> #{manifest => hash(), revision => 5, transcript_version => null}.
headers(Rev, Key) -> #{if_match => integer_to_binary(Rev), idempotency_key => Key}.
public() -> #{<<"policy_version">> => <<"p1">>, <<"config_version">> => hash()}.
pin() ->
    (public())#{
        <<"provider">> => <<"doubao">>,
        <<"model">> => <<"volc.seedasr.auc">>,
        <<"processing_mode">> => <<"transcribe">>
    }.
request() ->
    Pin = (public())#{
        <<"execution_mode">> => <<"client_byok">>,
        <<"provider">> => <<"doubao">>,
        <<"model">> => <<"volc.seedasr.auc">>
    },
    Body = Pin#{
        <<"mode">> => <<"transcribe">>, <<"processing_consent">> => Pin#{<<"confirmed">> => true}
    },
    {ok, P} = talk_record_client_contract:request(Body, policy_public()),
    {Body, P}.
hash() -> binary:copy(<<"a">>, 64).

policy_public() ->
    (public())#{
        <<"providers">> => #{<<"transcribe">> => maps:with([<<"provider">>, <<"model">>], pin())}
    }.
