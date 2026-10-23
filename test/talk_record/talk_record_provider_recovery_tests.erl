-module(talk_record_provider_recovery_tests).
-include_lib("eunit/include/eunit.hrl").

same_device_source_run_is_reused_without_renewal_test() ->
    with_recovery(fun() ->
        {ok, created, P} = recover(task()),
        ?assertEqual(30, maps:get(<<"id">>, maps:get(<<"run">>, P))),
        ?assertEqual(false, maps:get(<<"may_submit">>, P)),
        ?assertEqual(500, maps:get(<<"lease_expires_at_unix">>, maps:get(<<"run">>, P))),
        ?assertEqual(
            400,
            maps:get(
                <<"expires_at">>, maps:get(<<"source_authorization">>, maps:get(<<"task">>, P))
            )
        ),
        ?assertEqual(0, meck:num_calls(talk_record_client_repo, heartbeat_tx, '_')),
        ?assertEqual(0, meck:num_calls(talk_record_client_repo, insert_run_tx, '_')),
        ?assertEqual(0, meck:num_calls(talk_record_client_repo, expire_tx, '_')),
        ?assertEqual(0, meck:num_calls(talk_record_client_repo, settle_tx, '_'))
    end).
foreign_actor_device_or_other_task_cannot_reuse_test() ->
    lists:foreach(
        fun(Change) ->
            with_recovery(fun() ->
                put(recovery_run, maps:merge(get(recovery_run), Change)),
                ?assertThrow({talk_client_abort, client_execution_conflict}, recover(task())),
                ?assertEqual(0, meck:num_calls(talk_record_client_repo, settle_tx, '_')),
                ?assertEqual(0, meck:num_calls(talk_record_client_repo, insert_run_tx, '_'))
            end)
        end,
        [
            #{<<"actor_user_id">> => 11},
            #{<<"device_session_id">> => <<"other">>},
            #{<<"execution_purpose">> => <<"query_only">>, <<"provider_task_id">> => 999},
            #{<<"id">> => 999}
        ]
    ).
query_only_run_for_exact_task_is_reused_test() ->
    with_recovery(fun() ->
        put(recovery_run, (get(recovery_run))#{
            <<"execution_purpose">> => <<"query_only">>, <<"provider_task_id">> => 50
        }),
        ?assertMatch({ok, created, _}, recover(task()))
    end).
result_ready_reuses_confirmed_part_for_completion_test() ->
    with_recovery(fun() ->
        T = (task())#{<<"state">> => <<"result_ready">>, <<"result_hash">> => hash()},
        put(recovery_parts, [
            #{<<"seq">> => 0, <<"provider_state">> => <<"confirmed">>, <<"result_hash">> => hash()}
        ]),
        {ok, created, P} = recover(T),
        ?assert(
            lists:member(<<"complete">>, maps:get(<<"allowed_actions">>, maps:get(<<"task">>, P)))
        ),
        ?assertEqual(0, meck:num_calls(talk_record_client_repo, result_tx, '_')),
        put(recovery_parts, [#{<<"seq">> => 0, <<"provider_state">> => <<"dispatching">>}]),
        ?assertThrow({talk_client_abort, client_result_conflict}, recover(T))
    end).
expired_run_enters_new_query_lease_path_test() ->
    with_recovery(fun() ->
        put(recovery_run, (get(recovery_run))#{<<"lease_valid">> => false}),
        meck:expect(talk_record_client_repo, expire_tx, fun(conn, #{<<"id">> := 30}) ->
            {expired, <<"provider_outcome_unknown">>}
        end),
        meck:expect(talk_record_client_repo, lock_job_tx, fun(conn, 7, 20) -> {ok, job()} end),
        %% Stop before any fixture insert; this oracle proves the expired path
        %% does not return the old lease or wait for another 300 seconds.
        meck:expect(talk_record_client_repo, consent_tx, fun(_, _, _, _, _) ->
            {error, synthetic_new_lease_boundary}
        end),
        ?assertThrow({talk_client_abort, synthetic_new_lease_boundary}, recover(task())),
        ?assertEqual(1, meck:num_calls(talk_record_client_repo, expire_tx, '_'))
    end).
explicit_own_unknown_handoff_precedes_new_submit_test() ->
    with_recovery(fun() ->
        T = (task())#{<<"state">> => <<"submit_outcome_unknown">>, <<"provider_task_id">> => null},
        meck:expect(talk_record_client_repo, settle_tx, fun(
            conn, 30, <<"provider_outcome_unknown">>, <<"network_unknown">>
        ) ->
            put(handed_off, true),
            {ok, 1}
        end),
        meck:expect(talk_record_client_repo, lock_job_tx, fun(conn, 7, 20) ->
            ?assertEqual(true, get(handed_off)),
            {ok, job()}
        end),
        meck:expect(talk_record_client_repo, consent_tx, fun(_, _, _, _, _) ->
            {error, synthetic_new_lease_boundary}
        end),
        ?assertThrow({talk_client_abort, synthetic_new_lease_boundary}, create(resubmission, T)),
        ?assertEqual(1, meck:num_calls(talk_record_client_repo, settle_tx, '_')),
        ?assertEqual(0, meck:num_calls(talk_record_client_repo, expire_tx, '_'))
    end).
recover(T) -> create(query_execution, T).
create(A, T) ->
    talk_record_provider_recovery:create(
        conn,
        A,
        #{uid => 10, did => <<"device">>},
        #{<<"id">> => 7},
        job(),
        T,
        #{},
        #{<<"processing_consent">> => consent(), <<"ack_possible_duplicate_charge">> => true},
        #{}
    ).
with_recovery(F) ->
    Modules = [
        talk_record_client_repo,
        talk_record_provider_repo,
        talk_record_client_policy,
        elib_pg,
        elib_tsid
    ],
    lists:foreach(
        fun(M) ->
            O =
                case M of
                    elib_pg -> [no_link];
                    _ -> [passthrough, no_link]
                end,
            meck:new(M, O)
        end,
        Modules
    ),
    try
        Config = #{
            signing_key => binary:copy(<<"k">>, 32),
            public => #{
                <<"policy_version">> => <<"p1">>,
                <<"config_version">> => hash(),
                <<"providers">> => #{
                    <<"transcribe">> => #{
                        <<"provider">> => <<"doubao">>, <<"model">> => <<"volc.seedasr.auc">>
                    }
                }
            }
        },
        Token = talk_record_client_policy:token(
            maps:get(signing_key, Config), 30, 1, 10, <<"device">>
        ),
        put(recovery_run, #{
            <<"id">> => 30,
            <<"record_id">> => 7,
            <<"job_id">> => 20,
            <<"attempt">> => 1,
            <<"actor_user_id">> => 10,
            <<"device_session_id">> => <<"device">>,
            <<"state">> => <<"active">>,
            <<"revision">> => 1,
            <<"lease_valid">> => true,
            <<"lease_expires_at_unix">> => 500,
            <<"execution_purpose">> => <<"submit">>,
            <<"lease_token_hash">> => binary:encode_hex(crypto:hash(sha256, Token), lowercase),
            <<"consent_event_id">> => 31,
            <<"policy_version">> => <<"p1">>,
            <<"config_version">> => hash()
        }),
        put(recovery_parts, []),
        meck:expect(talk_record_provider_repo, latest_task_tx, fun(conn, 20) -> {ok, task()} end),
        meck:expect(talk_record_client_policy, get, fun() -> {ok, Config} end),
        meck:expect(talk_record_client_repo, latest_tx, fun(conn, 20) -> {ok, get(recovery_run)} end),
        meck:expect(talk_record_client_repo, lock_run_tx, fun(conn, 7, 20, 30) ->
            {ok, get(recovery_run)}
        end),
        meck:expect(talk_record_client_repo, parts_tx, fun(conn, 30) ->
            {ok, get(recovery_parts)}
        end),
        meck:expect(elib_pg, query, fun(conn, SQL, [31, 7, 10, <<"p1">>, _, <<"transcribe">>]) ->
            ?assertNotEqual(nomatch, binary:match(SQL, <<"talk_record_consent_event">>)),
            {ok, [#{<<"id">> => 31}]}
        end),
        meck:expect(elib_tsid, generate, fun(attachment) -> 100 end),
        F()
    after
        lists:foreach(fun meck:unload/1, Modules),
        lists:foreach(fun erase/1, [recovery_run, recovery_parts, handed_off])
    end.
job() ->
    #{
        <<"id">> => 20,
        <<"attempt">> => 1,
        <<"kind">> => <<"transcribe">>,
        <<"state">> => <<"running">>,
        <<"processor_input">> => maps:remove(<<"confirmed">>, consent())
    }.
consent() ->
    #{
        <<"provider">> => <<"doubao">>,
        <<"model">> => <<"volc.seedasr.auc">>,
        <<"execution_mode">> => <<"client_byok">>,
        <<"policy_version">> => <<"p1">>,
        <<"config_version">> => hash(),
        <<"confirmed">> => true
    }.
task() ->
    #{
        <<"id">> => 50,
        <<"source_run_id">> => 30,
        <<"job_id">> => 20,
        <<"state">> => <<"accepted">>,
        <<"provider_task_id">> => <<"returned-id">>,
        <<"revision">> => 1,
        <<"request_profile">> => talk_record_provider_contract:profile(),
        <<"assembly">> => #{},
        <<"source_authorization_id">> => 60,
        <<"source_issued_at">> => 100,
        <<"source_expires_at">> => 400,
        <<"policy_version">> => <<"p1">>,
        <<"config_version">> => hash()
    }.
hash() -> binary:copy(<<"a">>, 64).

legacy_claim_rejects_every_doubao_history_test() ->
    with_recovery(fun() ->
        lists:foreach(
            fun(State) ->
                put(recovery_run, (get(recovery_run))#{<<"state">> => State}),
                ?assertThrow(
                    {talk_client_abort, async_protocol_required},
                    talk_record_client_claim:create_tx(
                        conn,
                        #{uid => 10, did => <<"device">>},
                        #{<<"id">> => 7},
                        (job())#{<<"state">> => <<"failed">>},
                        #{},
                        #{<<"processing_consent">> => consent()}
                    )
                )
            end,
            [
                <<"active">>,
                <<"expired">>,
                <<"failed">>,
                <<"complete">>,
                <<"cancelled">>,
                <<"provider_outcome_unknown">>
            ]
        ),
        ?assertEqual(0, meck:num_calls(talk_record_client_repo, insert_run_tx, '_'))
    end).

first_doubao_generic_claim_remains_available_test() ->
    with_recovery(fun() ->
        meck:expect(talk_record_client_repo, latest_tx, fun(conn, 20) -> {error, not_found} end),
        Snapshot = #{
            <<"source_kind">> => <<"assembly">>,
            <<"parts">> => [
                #{
                    <<"part_id">> => 71,
                    <<"duration_ms">> => 1000,
                    <<"sha256">> => hash()
                }
            ],
            <<"assembly">> => #{
                <<"id">> => 80,
                <<"sha256">> => hash(),
                <<"byte_size">> => 100,
                <<"duration_ms">> => 1000,
                <<"manifest_hash">> => hash()
            }
        },
        meck:expect(talk_record_provider_repo, snapshot_tx, fun(conn, 7) -> {ok, Snapshot} end),
        meck:expect(talk_record_client_repo, consent_tx, fun(conn, 7, 10, <<"transcribe">>, _) ->
            {ok, 31}
        end),
        meck:expect(talk_record_client_repo, claim_job_tx, fun(conn, 20, 1) -> {ok, 1} end),
        meck:expect(talk_record_client_repo, insert_run_tx, fun(conn, D) ->
            ?assertEqual(null, maps:get(<<"replaces_run_id">>, D)),
            ?assertEqual(false, maps:get(<<"ack_possible_duplicate_charge">>, D)),
            ?assertEqual(Snapshot, jsone:decode(maps:get(<<"input_snapshot">>, D))),
            put(inserted_initial_run, D#{<<"revision">> => 1}),
            {ok, 1}
        end),
        meck:expect(talk_record_client_repo, lock_run_tx, fun(conn, 7, 20, 100) ->
            {ok, get(inserted_initial_run)}
        end),
        J = (job())#{
            <<"attempt">> => 0,
            <<"state">> => <<"queued">>,
            <<"input_manifest_hash">> => hash(),
            <<"input_content_revision">> => 1,
            <<"input_transcript_version">> => null
        },
        ?assertMatch(
            {ok, created, #{<<"id">> := 100}},
            talk_record_client_claim:create_tx(
                conn,
                #{uid => 10, did => <<"device">>},
                #{<<"id">> => 7},
                J,
                #{},
                #{<<"processing_consent">> => consent()}
            )
        ),
        ?assertEqual(1, meck:num_calls(talk_record_client_repo, insert_run_tx, '_')),
        erase(inserted_initial_run)
    end).
foreign_device_cannot_handoff_unknown_task_test() ->
    with_recovery(fun() ->
        put(recovery_run, (get(recovery_run))#{<<"device_session_id">> => <<"other-device">>}),
        T = (task())#{<<"state">> => <<"submit_outcome_unknown">>, <<"provider_task_id">> => null},
        ?assertThrow({talk_client_abort, client_execution_conflict}, create(resubmission, T)),
        ?assertEqual(0, meck:num_calls(talk_record_client_repo, settle_tx, '_'))
    end).

old_rejected_task_cannot_bypass_newer_task_test() ->
    with_recovery(fun() ->
        meck:expect(talk_record_provider_repo, latest_task_tx, fun(conn, 20) ->
            {ok, #{<<"id">> => 999}}
        end),
        ?assertThrow(
            {talk_client_abort, client_execution_conflict},
            create(resubmission, (task())#{<<"state">> => <<"rejected">>})
        ),
        ?assertEqual(0, meck:num_calls(talk_record_client_repo, settle_tx, '_'))
    end).

unstarted_preparation_creates_new_unacknowledged_task_test() ->
    lists:foreach(
        fun(Active) ->
            with_recovery(fun() ->
                T = unstarted_task(),
                Snap = preparation_snapshot(),
                put(recovery_run, (get(recovery_run))#{
                    <<"lease_valid">> => Active,
                    <<"provider">> => <<"doubao">>,
                    <<"model">> => <<"volc.seedasr.auc">>,
                    <<"input_manifest_hash">> => hash(),
                    <<"input_content_revision">> => 1,
                    <<"input_transcript_version">> => null,
                    <<"input_snapshot">> => Snap,
                    <<"chunk_plan_hash">> => hash()
                }),
                put(recovery_parts, [
                    #{
                        <<"seq">> => 0,
                        <<"provider_state">> => <<"not_dispatched">>,
                        <<"dispatch_intent_id">> => null,
                        <<"request_hash">> => null
                    }
                ]),
                setup_preparation_rows(T, Snap),
                {ok, created, P} = talk_record_provider_recovery:create(
                    conn,
                    resubmission,
                    #{uid => 10, did => <<"device">>},
                    #{<<"id">> => 7},
                    job(),
                    T,
                    #{},
                    #{
                        <<"processing_consent">> => consent(),
                        <<"ack_possible_duplicate_charge">> => false
                    },
                    #{}
                ),
                ?assertEqual(false, maps:get(<<"may_submit">>, P)),
                New = maps:get(<<"task">>, P),
                ?assertEqual(<<"prepared">>, maps:get(<<"state">>, New)),
                ?assert(
                    lists:member(<<"resume_preparation">>, maps:get(<<"allowed_actions">>, New))
                ),
                ?assertEqual(
                    case Active of
                        true -> 1;
                        false -> 0
                    end,
                    meck:num_calls(talk_record_client_repo, settle_tx, '_')
                ),
                ?assertEqual(
                    case Active of
                        true -> 0;
                        false -> 1
                    end,
                    meck:num_calls(talk_record_client_repo, expire_tx, '_')
                ),
                erase(new_preparation_run),
                erase(new_preparation_task)
            end)
        end,
        [false, true]
    ).
setup_preparation_rows(T, Snap) ->
    meck:expect(talk_record_provider_repo, latest_task_tx, fun(conn, 20) -> {ok, T} end),
    meck:expect(talk_record_client_repo, expire_tx, fun(conn, #{<<"id">> := 30}) ->
        put(recovery_run, (get(recovery_run))#{
            <<"state">> => <<"expired">>, <<"lease_valid">> => false
        }),
        {expired, <<"expired">>}
    end),
    meck:expect(talk_record_client_repo, settle_tx, fun(
        conn, 30, <<"expired">>, <<"device_stopped">>
    ) ->
        put(recovery_run, (get(recovery_run))#{
            <<"state">> => <<"expired">>, <<"lease_valid">> => false
        }),
        {ok, 1}
    end),
    meck:expect(talk_record_client_repo, lock_job_tx, fun(conn, 7, 20) -> {ok, job()} end),
    meck:expect(talk_record_client_repo, consent_tx, fun(_, 7, 10, <<"transcribe">>, _) ->
        {ok, 31}
    end),
    meck:expect(talk_record_client_repo, claim_job_tx, fun(conn, 20, 2) -> {ok, 1} end),
    meck:expect(talk_record_client_repo, insert_run_tx, fun(conn, D) ->
        ?assertEqual(30, maps:get(<<"replaces_run_id">>, D)),
        ?assertEqual(false, maps:get(<<"ack_possible_duplicate_charge">>, D)),
        put(new_preparation_run, D#{
            <<"revision">> => 1,
            <<"state">> => <<"active">>,
            <<"lease_valid">> => true,
            <<"chunk_plan_hash">> => null
        }),
        {ok, 1}
    end),
    meck:expect(talk_record_client_repo, lock_run_tx, fun
        (conn, 7, 20, 30) -> {ok, get(recovery_run)};
        (conn, 7, 20, 100) -> {ok, get(new_preparation_run)}
    end),
    meck:expect(talk_record_provider_repo, origin_tx, fun
        (conn, 7, 20, 30) -> {ok, T};
        (conn, 7, 20, 100) -> {error, not_found}
    end),
    meck:expect(talk_record_provider_repo, snapshot_tx, fun(conn, 7) -> {ok, Snap} end),
    meck:expect(talk_record_client_repo, freeze_plan_tx, fun(conn, 100, H) ->
        put(new_preparation_run, (get(new_preparation_run))#{<<"chunk_plan_hash">> => H}),
        {ok, 1}
    end),
    meck:expect(talk_record_client_repo, insert_part_tx, fun(
        conn, #{<<"run_id">> := 100, <<"seq">> := 0}
    ) ->
        {ok, 1}
    end),
    meck:expect(talk_record_client_repo, parts_tx, fun
        (conn, 30) -> {ok, get(recovery_parts)};
        (conn, 100) -> {ok, []}
    end),
    meck:expect(talk_record_provider_repo, insert_tx, fun(conn, D) ->
        ?assertEqual(50, maps:get(<<"previous_task_id">>, D)),
        ?assertEqual(false, maps:get(<<"ack_possible_duplicate_charge">>, D)),
        put(new_preparation_task, maps:merge(unstarted_task(), D#{<<"revision">> => 1})),
        {ok, 1}
    end),
    meck:expect(talk_record_provider_repo, task_tx, fun(conn, 7, 100) ->
        {ok, get(new_preparation_task)}
    end).
unstarted_task() ->
    (task())#{
        <<"record_id">> => 7,
        <<"state">> => <<"prepared">>,
        <<"provider_task_id">> => null,
        <<"intent_id">> => null,
        <<"request_hash">> => null,
        <<"source_issued_at">> => null,
        <<"source_expires_at">> => null,
        <<"result">> => null,
        <<"result_hash">> => null,
        <<"outcome_unknown_at">> => null,
        <<"cancelled_at">> => null
    }.
preparation_snapshot() ->
    #{
        <<"source_kind">> => <<"assembly">>,
        <<"parts">> => [
            #{
                <<"part_id">> => 71,
                <<"sha256">> => hash(),
                <<"duration_ms">> => 1000
            }
        ],
        <<"assembly">> => #{
            <<"id">> => 80,
            <<"sha256">> => hash(),
            <<"byte_size">> => 100,
            <<"duration_ms">> => 1000,
            <<"mime_type">> => <<"audio/aac">>,
            <<"manifest_hash">> => hash()
        }
    }.

false_ack_never_restarts_a_paid_or_dispatched_fact_test() ->
    with_recovery(fun() ->
        T = (task())#{<<"state">> => <<"submit_outcome_unknown">>, <<"provider_task_id">> => null},
        B = #{<<"processing_consent">> => consent(), <<"ack_possible_duplicate_charge">> => false},
        ?assertThrow(
            {talk_client_abort, client_execution_conflict},
            talk_record_provider_recovery:create(
                conn,
                resubmission,
                #{uid => 10, did => <<"device">>},
                #{<<"id">> => 7},
                job(),
                T,
                #{},
                B,
                #{}
            )
        ),
        U = unstarted_task(),
        meck:expect(talk_record_provider_repo, latest_task_tx, fun(conn, 20) -> {ok, U} end),
        put(recovery_parts, [
            #{
                <<"seq">> => 0,
                <<"provider_state">> => <<"dispatching">>,
                <<"dispatch_intent_id">> => 9
            }
        ]),
        ?assertThrow(
            {talk_client_abort, client_intent_conflict},
            talk_record_provider_recovery:create(
                conn,
                resubmission,
                #{uid => 10, did => <<"device">>},
                #{<<"id">> => 7},
                job(),
                U,
                #{},
                B,
                #{}
            )
        ),
        ?assertEqual(0, meck:num_calls(talk_record_client_repo, settle_tx, '_'))
    end).
