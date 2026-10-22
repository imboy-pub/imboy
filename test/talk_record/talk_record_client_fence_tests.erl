-module(talk_record_client_fence_tests).
-include_lib("eunit/include/eunit.hrl").

old_attempt_rejected_before_consent_or_result_test() ->
    {Ctx, Run, Token} = identity(),
    ?assertThrow(
        {talk_client_abort, lease_lost},
        talk_record_client_gate:run_tx(
            no_db,
            Ctx,
            #{<<"attempt">> => 2, <<"state">> => <<"running">>},
            Run,
            Token
        )
    ).

wrong_device_or_actor_rejected_test() ->
    {Ctx, Run, Token} = identity(),
    Job = #{<<"attempt">> => 1, <<"state">> => <<"running">>},
    ?assertThrow(
        {talk_client_abort, forbidden},
        talk_record_client_gate:run_tx(no_db, Ctx#{did => <<"other">>}, Job, Run, Token)
    ),
    ?assertThrow(
        {talk_client_abort, forbidden},
        talk_record_client_gate:run_tx(no_db, Ctx#{uid => 11}, Job, Run, Token)
    ).

cancelled_job_rejects_late_result_test() ->
    {Ctx, Run, Token} = identity(),
    ?assertThrow(
        {talk_client_abort, lease_lost},
        talk_record_client_gate:run_tx(
            no_db,
            Ctx,
            #{<<"attempt">> => 1, <<"state">> => <<"cancelled">>},
            Run,
            Token
        )
    ).

wrong_token_rejected_test() ->
    {Ctx, Run, _} = identity(),
    ?assertThrow(
        {talk_client_abort, lease_lost},
        talk_record_client_gate:run_tx(
            no_db,
            Ctx,
            #{<<"attempt">> => 1, <<"state">> => <<"running">>},
            Run,
            binary:copy(<<"y">>, 44)
        )
    ).

unknown_requires_explicit_charge_ack_test() ->
    unknown_claim(#{}, client_execution_conflict),
    unknown_claim(
        #{<<"ack_possible_duplicate_charge">> => true, <<"previous_run_id">> => 66},
        client_execution_conflict
    ),
    unknown_claim(
        #{<<"ack_possible_duplicate_charge">> => true, <<"previous_run_id">> => 77},
        precondition_required
    ),
    unknown_claim(#{<<"device_id">> => <<"pretend">>}, client_schema_invalid).

unknown_claim(Body, Expected) ->
    meck:new(talk_record_client_repo, [passthrough, no_link]),
    try
        meck:expect(talk_record_client_repo, latest_tx, fun(_, _) ->
            {ok, #{
                <<"id">> => 77, <<"state">> => <<"provider_outcome_unknown">>, <<"revision">> => 4
            }}
        end),
        ?assertThrow(
            {talk_client_abort, Expected},
            talk_record_client_claim:create_tx(
                conn,
                #{},
                #{},
                #{<<"id">> => 9, <<"state">> => <<"failed">>},
                #{},
                Body
            )
        ),
        ?assertEqual(0, meck:num_calls(talk_record_client_repo, insert_run_tx, '_'))
    after
        meck:unload(talk_record_client_repo)
    end.

run_etag_cannot_target_another_run_test() ->
    ?assertThrow(
        {talk_client_abort, invalid_if_match},
        talk_record_client_ds:revision(
            #{<<"id">> => 7, <<"revision">> => 2}, <<"\"client-run:8:2\"">>
        )
    ),
    ?assertThrow(
        {talk_client_abort, precondition_failed},
        talk_record_client_ds:revision(
            #{<<"id">> => 7, <<"revision">> => 2}, <<"\"client-run:7:1\"">>
        )
    ).

incomplete_result_cannot_publish_test() ->
    meck:new(talk_record_client_repo, [passthrough, no_link]),
    meck:new(talk_record_client_publish, [passthrough, no_link]),
    try
        P = #{
            <<"part_id">> => 1,
            <<"sha256">> => binary:copy(<<"a">>, 64),
            <<"offset_ms">> => 0,
            <<"duration_ms">> => 1000,
            <<"byte_size">> => 100
        },
        %% Empty result set cannot be treated as complete just because all present rows are confirmed.
        meck:expect(talk_record_client_repo, parts_tx, fun(_, _) -> {ok, []} end),
        ?assertThrow(
            {talk_client_abort, client_schema_invalid},
            talk_record_client_operations:perform(
                conn,
                complete,
                #{},
                #{<<"kind">> => <<"transcribe">>},
                #{
                    <<"id">> => 8,
                    <<"input_snapshot">> => #{<<"parts">> => [P]},
                    <<"chunk_plan_hash">> => binary:copy(<<"a">>, 64)
                },
                0,
                #{}
            )
        ),
        ?assertEqual(0, meck:num_calls(talk_record_client_publish, publish_tx, '_'))
    after
        meck:unload(talk_record_client_repo),
        meck:unload(talk_record_client_publish)
    end.

identity() ->
    T = binary:copy(<<"x">>, 44),
    {
        #{uid => 10, did => <<"device">>},
        #{
            <<"actor_user_id">> => 10,
            <<"device_session_id">> => <<"device">>,
            <<"attempt">> => 1,
            <<"lease_token_hash">> => binary:encode_hex(crypto:hash(sha256, T), lowercase)
        },
        T
    }.

record_and_frozen_processing_mode_must_match_test() ->
    J = #{
        <<"kind">> => <<"transcribe">>,
        <<"processor_input">> => #{<<"processing_mode">> => <<"transcribe">>}
    },
    ?assertEqual(ok, talk_record_client_gate:mode(#{<<"processing_mode">> => <<"transcribe">>}, J)),
    ?assertThrow(
        {talk_client_abort, processing_consent_required},
        talk_record_client_gate:mode(
            #{<<"processing_mode">> => <<"record_only">>}, J
        )
    ),
    ?assertThrow(
        {talk_client_abort, processing_consent_required},
        talk_record_client_gate:mode(
            #{<<"processing_mode">> => <<"transcribe">>}, J#{
                <<"processor_input">> => #{<<"processing_mode">> => <<"summarize">>}
            }
        )
    ).

consent_event_processing_mode_is_checked_test() ->
    meck:new(elib_pg, [passthrough, no_link]),
    try
        meck:expect(elib_pg, query, fun(_, Sql, Args) ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"processing_mode=$6">>)),
            ?assertEqual(<<"transcribe">>, lists:nth(6, Args)),
            {ok, []}
        end),
        {Ctx, R, T} = identity(),
        Run = R#{
            <<"consent_event_id">> => 5,
            <<"record_id">> => 7,
            <<"policy_version">> => <<"p1">>,
            <<"config_version">> => binary:copy(<<"a">>, 64)
        },
        ?assertThrow(
            {talk_client_abort, processing_consent_required},
            talk_record_client_gate:run_tx(
                conn,
                Ctx,
                #{<<"attempt">> => 1, <<"state">> => <<"running">>, <<"kind">> => <<"transcribe">>},
                Run,
                T
            )
        )
    after
        meck:unload(elib_pg)
    end.

mode_change_blocks_all_mutations_before_idempotent_replay_test() ->
    Modules = [
        elib_pg,
        talk_record_client_gate,
        talk_record_client_repo,
        talk_record_idempotency_repo,
        talk_record_client_operations
    ],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    try
        meck:expect(elib_pg, with_tx, fun(F, _) ->
            try
                F(conn)
            catch
                throw:E -> {rollback, E}
            end
        end),
        meck:expect(talk_record_client_gate, record_tx, fun(_, _, _, write) ->
            #{<<"id">> => 7, <<"processing_mode">> => <<"record_only">>}
        end),
        meck:expect(talk_record_client_repo, lock_job_tx, fun(_, 7, 8) ->
            {ok, #{
                <<"id">> => 8,
                <<"kind">> => <<"transcribe">>,
                <<"processor_input">> => #{<<"processing_mode">> => <<"transcribe">>}
            }}
        end),
        meck:expect(talk_record_client_repo, latest_tx, fun(_, 8) -> {error, not_found} end),
        meck:expect(talk_record_client_repo, lock_run_tx, fun(_, 7, 8, 9) -> {ok, #{}} end),
        lists:foreach(
            fun(Action) ->
                ?assertEqual(
                    {error, processing_consent_required},
                    talk_record_client_ds:execute(
                        Action,
                        #{uid => 10, did => <<"device">>},
                        7,
                        8,
                        9,
                        0,
                        {#{}, #{}}
                    )
                )
            end,
            [claim, plan, heartbeat, dispatch, result, complete, settle]
        ),
        ?assertEqual(0, meck:num_calls(talk_record_idempotency_repo, record_tx, '_')),
        ?assertEqual(0, meck:num_calls(talk_record_client_operations, perform, '_'))
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.

queue_full_http_endpoint_test() ->
    Modules = [elib_param, cowboy_req, talk_record_http, elib_response],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Modules),
    meck:new(talk_record_client_logic, [passthrough, no_link]),
    try
        Body = #{<<"execution_mode">> => <<"client_byok">>},
        meck:expect(elib_param, post, fun(request) -> Body end),
        meck:expect(talk_record_http, path_id, fun(request) -> {ok, 7} end),
        meck:expect(cowboy_req, header, fun(_, request) -> undefined end),
        meck:expect(talk_record_client_logic, processing_request, fun(Context, 7, _, Actual) ->
            ?assertEqual(#{uid => 10, did => <<"authenticated-device">>}, Context),
            ?assertEqual(Body, Actual),
            {error, client_queue_full}
        end),
        meck:expect(elib_response, error_with_status, fun(request, Status, Message, _) ->
            {Status, Message}
        end),
        ?assertEqual(
            {409, <<"client_queue_full">>},
            talk_record_client_handler:processing_request(
                request,
                #{current_uid => 10, current_did => <<"authenticated-device">>}
            )
        )
    after
        lists:foreach(fun meck:unload/1, [talk_record_client_logic | Modules])
    end.

confirmed_ancestor_lineage_test_() ->
    [
        {atom_to_list(Scenario), fun() -> ancestor_probe(Scenario, Expected) end}
     || {Scenario, Expected} <- [
            {empty_expired, reused},
            {empty_failed, reused},
            {two_confirmed_ancestors, reused},
            {first_attempt, not_reused},
            {multiple_empty_attempts, reused},
            {newer_confirmed_wins, reused},
            {future_attempt, reused},
            {acknowledged_unknown, reused},
            {unknown_without_ack, client_execution_conflict},
            {unknown_wrong_parent, client_execution_conflict},
            {unknown_without_flag, client_execution_conflict},
            {hidden_unknown_part, client_execution_conflict},
            {hidden_dispatching_part, client_execution_conflict},
            {incompatible_unknown, client_execution_conflict},
            {incompatible_plan, client_plan_conflict},
            {incompatible_pins, client_execution_conflict},
            {missing_attempt, client_execution_conflict},
            {foreign_job, client_execution_conflict},
            {foreign_record, client_execution_conflict},
            {active_ancestor, client_execution_conflict},
            {empty_plan_with_parts, client_plan_conflict}
        ]
    ].

ancestor_probe(Scenario, Expected) ->
    {Initial, Plan, Part} = ancestor_fixture(),
    Hash = talk_record_client_contract:hash(Plan),
    Run =
        case Scenario of
            first_attempt -> Initial#{<<"attempt">> => 1};
            multiple_empty_attempts -> Initial#{<<"attempt">> => 4};
            _ -> Initial
        end,
    First = Run#{
        <<"id">> => 101,
        <<"attempt">> => 1,
        <<"state">> => <<"expired">>,
        <<"chunk_plan_hash">> => Hash
    },
    Second = Run#{<<"id">> => 102, <<"attempt">> => 2, <<"state">> => <<"expired">>},
    Confirmed = Part#{
        <<"source_ranges">> => jsone:encode(maps:get(<<"source_ranges">>, Part)),
        <<"provider_state">> => <<"confirmed">>,
        <<"dispatch_intent_id">> => 501,
        <<"request_hash">> => talk_record_client_operations:request_hash(First, Part, 0),
        <<"result_hash">> => Hash,
        <<"result">> => #{<<"text">> => <<"paid result">>}
    },
    {History, OldParts} = ancestor_variant(Scenario, First, Second, Confirmed),
    Modules = [talk_record_client_repo, elib_pg, elib_tsid],
    lists:foreach(fun(Module) -> meck:new(Module, [passthrough, no_link]) end, Modules),
    put(client_fix_run, Run),
    put(client_fix_parts, []),
    put(client_fix_sources, []),
    try
        ancestor_storage(Run, History, OldParts, Hash),
        Result =
            try
                talk_record_client_operations:perform(
                    conn,
                    plan,
                    #{<<"id">> => 7},
                    #{<<"id">> => 31, <<"kind">> => <<"transcribe">>},
                    Run,
                    0,
                    Plan
                )
            catch
                throw:{talk_client_abort, Reason} -> {error, Reason}
            end,
        case Expected of
            Failure when Failure =:= client_execution_conflict; Failure =:= client_plan_conflict ->
                ?assertEqual({error, Failure}, Result),
                ?assertEqual(0, meck:num_calls(talk_record_client_repo, dispatch_tx, '_'));
            _ ->
                ?assertMatch({ok, created, _}, Result),
                Current = get(client_fix_run),
                [CurrentPart | _] = get(client_fix_parts),
                Body = #{
                    <<"request_hash">> => talk_record_client_operations:request_hash(
                        Current, CurrentPart, 0
                    ),
                    <<"chunk_plan_hash">> => Hash
                },
                {ok, created, View} = talk_record_client_operations:perform(
                    conn,
                    dispatch,
                    #{<<"id">> => 7},
                    #{<<"id">> => 31},
                    Current,
                    0,
                    Body
                ),
                ?assertEqual(Expected =:= not_reused, maps:get(<<"may_dispatch">>, View)),
                case Expected of
                    reused ->
                        [Reused | _] = get(client_fix_parts),
                        ExpectedSource =
                            case Scenario of
                                newer_confirmed_wins -> 102;
                                _ -> 101
                            end,
                        ExpectedIntent =
                            case Scenario of
                                newer_confirmed_wins -> 601;
                                _ -> 501
                            end,
                        ?assertEqual(ExpectedSource, maps:get(<<"reused_from_run_id">>, Reused)),
                        ?assertEqual(ExpectedIntent, maps:get(<<"dispatch_intent_id">>, Reused)),
                        ?assertEqual(
                            maps:get(<<"result">>, Confirmed), maps:get(<<"result">>, Reused)
                        ),
                        ?assertEqual(0, meck:num_calls(talk_record_client_repo, dispatch_tx, '_'));
                    not_reused ->
                        ?assertEqual([], get(client_fix_sources))
                end,
                case Scenario of
                    two_confirmed_ancestors ->
                        ?assertEqual([101, 102], get(client_fix_sources));
                    acknowledged_unknown ->
                        [_, Pending] = get(client_fix_parts),
                        ?assertEqual(<<"not_dispatched">>, maps:get(<<"provider_state">>, Pending));
                    _ ->
                        ok
                end
        end,
        ?assert(lists:all(fun meck:validate/1, Modules))
    after
        lists:foreach(fun meck:unload/1, Modules),
        lists:foreach(fun erase/1, [client_fix_run, client_fix_parts, client_fix_sources])
    end.

ancestor_variant(first_attempt, _First, _Second, _Part) ->
    {[], #{}};
ancestor_variant(multiple_empty_attempts, First, Second, Part) ->
    {[First, Second, Second#{<<"id">> => 104, <<"attempt">> => 3}], #{
        101 => [Part], 102 => [], 104 => []
    }};
ancestor_variant(empty_expired, First, Second, Part) ->
    {[First, Second], #{101 => [Part], 102 => []}};
ancestor_variant(empty_failed, First, Second, Part) ->
    {[First, Second#{<<"state">> => <<"failed">>}], #{101 => [Part], 102 => []}};
ancestor_variant(two_confirmed_ancestors, First, Second, Part) ->
    [Range] = jsone:decode(maps:get(<<"source_ranges">>, Part)),
    Next = Part#{
        <<"seq">> => 1,
        <<"offset_ms">> => 1000,
        <<"source_ranges">> => jsone:encode([Range#{<<"start_ms">> => 1000, <<"end_ms">> => 2000}])
    },
    {[First, Second#{<<"chunk_plan_hash">> => maps:get(<<"chunk_plan_hash">>, First)}], #{
        101 => [Part],
        102 => [
            Next#{<<"request_hash">> => talk_record_client_operations:request_hash(First, Next, 1)}
        ]
    }};
ancestor_variant(newer_confirmed_wins, First, Second, Part) ->
    {[First, Second#{<<"chunk_plan_hash">> => maps:get(<<"chunk_plan_hash">>, First)}], #{
        101 => [Part], 102 => [Part#{<<"dispatch_intent_id">> => 601}]
    }};
ancestor_variant(future_attempt, First, Second, Part) ->
    {[First, Second, First#{<<"id">> => 104, <<"attempt">> => 4}], #{
        101 => [Part], 102 => [], 104 => [Part]
    }};
ancestor_variant(acknowledged_unknown, First, Second, Part) ->
    {
        [
            First#{<<"state">> => <<"provider_outcome_unknown">>},
            Second#{<<"replaces_run_id">> => 101, <<"ack_possible_duplicate_charge">> => true}
        ],
        #{
            101 => [
                Part, Part#{<<"seq">> => 1, <<"provider_state">> => <<"provider_outcome_unknown">>}
            ],
            102 => []
        }
    };
ancestor_variant(unknown_without_ack, First, Second, Part) ->
    {[First#{<<"state">> => <<"provider_outcome_unknown">>}, Second], #{101 => [Part], 102 => []}};
ancestor_variant(unknown_wrong_parent, First, Second, Part) ->
    {
        [
            First#{<<"state">> => <<"provider_outcome_unknown">>},
            Second#{<<"replaces_run_id">> => 999, <<"ack_possible_duplicate_charge">> => true}
        ],
        #{101 => [Part], 102 => []}
    };
ancestor_variant(unknown_without_flag, First, Second, Part) ->
    {
        [
            First#{<<"state">> => <<"provider_outcome_unknown">>},
            Second#{<<"replaces_run_id">> => 101}
        ],
        #{101 => [Part], 102 => []}
    };
ancestor_variant(hidden_unknown_part, First, Second, Part) ->
    {[First, Second], #{
        101 => [Part], 102 => [Part#{<<"provider_state">> => <<"provider_outcome_unknown">>}]
    }};
ancestor_variant(hidden_dispatching_part, First, Second, Part) ->
    {[First, Second], #{101 => [Part], 102 => [Part#{<<"provider_state">> => <<"dispatching">>}]}};
ancestor_variant(incompatible_unknown, First, Second, Part) ->
    {
        [
            First,
            Second#{
                <<"state">> => <<"provider_outcome_unknown">>,
                <<"chunk_plan_hash">> => binary:copy(<<"c">>, 64)
            }
        ],
        #{101 => [Part], 102 => []}
    };
ancestor_variant(incompatible_plan, First, Second, Part) ->
    {[First, Second#{<<"chunk_plan_hash">> => binary:copy(<<"c">>, 64)}], #{
        101 => [Part], 102 => []
    }};
ancestor_variant(incompatible_pins, First, Second, Part) ->
    {[First, Second#{<<"policy_version">> => <<"other">>}], #{101 => [Part], 102 => []}};
ancestor_variant(missing_attempt, First, _Second, Part) ->
    {[First], #{101 => [Part]}};
ancestor_variant(foreign_job, First, Second, Part) ->
    {[First, Second#{<<"job_id">> => 99}], #{101 => [Part], 102 => []}};
ancestor_variant(foreign_record, First, Second, Part) ->
    {[First, Second#{<<"record_id">> => 99}], #{101 => [Part], 102 => []}};
ancestor_variant(active_ancestor, First, Second, Part) ->
    {[First, Second#{<<"state">> => <<"active">>}], #{101 => [Part], 102 => []}};
ancestor_variant(empty_plan_with_parts, First, Second, Part) ->
    {[First, Second], #{101 => [Part], 102 => [Part]}}.

ancestor_storage(Run, History, OldParts, Hash) ->
    meck:expect(elib_pg, query, fun(conn, Sql, [31, ChildId]) ->
        [Child] = [Row || Row <- [Run | History], maps:get(<<"id">>, Row) =:= ChildId],
        Earlier = binary:match(Sql, <<"AND attempt<">>) =/= nomatch,
        Rows = [
            Row
         || Row <- [Run | History],
            maps:get(<<"job_id">>, Row) =:= 31,
            maps:get(<<"id">>, Row) =/= ChildId,
            not Earlier orelse maps:get(<<"attempt">>, Row) < maps:get(<<"attempt">>, Child)
        ],
        Sorted = lists:sort(
            fun(Left, Right) -> maps:get(<<"attempt">>, Left) > maps:get(<<"attempt">>, Right) end,
            Rows
        ),
        {ok, lists:sublist(Sorted, 1)}
    end),
    meck:expect(talk_record_client_repo, freeze_plan_tx, fun(conn, 103, HashValue) ->
        ?assertEqual(Hash, HashValue),
        put(client_fix_run, Run#{<<"chunk_plan_hash">> => Hash}),
        {ok, 1}
    end),
    meck:expect(talk_record_client_repo, insert_part_tx, fun(conn, Part) ->
        put(
            client_fix_parts,
            get(client_fix_parts) ++
                [
                    Part#{
                        <<"provider_state">> => <<"not_dispatched">>,
                        <<"dispatch_intent_id">> => null
                    }
                ]
        ),
        {ok, 1}
    end),
    meck:expect(talk_record_client_repo, parts_tx, fun(conn, Id) ->
        {ok,
            case Id of
                103 -> get(client_fix_parts);
                _ -> maps:get(Id, OldParts)
            end}
    end),
    meck:expect(talk_record_client_repo, lock_run_tx, fun(conn, 7, 31, 103) ->
        {ok, get(client_fix_run)}
    end),
    meck:expect(talk_record_client_repo, reuse_tx, fun(conn, 103, OldId) ->
        put(client_fix_sources, [OldId | get(client_fix_sources)]),
        PlanKeys = [
            <<"source_part_manifest_hash">>,
            <<"source_ranges">>,
            <<"offset_ms">>,
            <<"duration_ms">>,
            <<"byte_size">>
        ],
        Updated = [
            case
                [
                    Old
                 || Old <- maps:get(OldId, OldParts),
                    maps:get(<<"seq">>, Old) =:= maps:get(<<"seq">>, Part),
                    maps:with(PlanKeys, Old) =:= maps:with(PlanKeys, Part),
                    maps:get(<<"provider_state">>, Old) =:= <<"confirmed">>,
                    maps:get(<<"provider_state">>, Part) =:= <<"not_dispatched">>
                ]
            of
                [Old] -> maps:merge(Part, Old#{<<"reused_from_run_id">> => OldId});
                [] -> Part
            end
         || Part <- get(client_fix_parts)
        ],
        put(client_fix_parts, Updated),
        {ok, 1}
    end),
    meck:expect(elib_tsid, generate, fun(attachment) -> 502 end),
    meck:expect(talk_record_client_repo, dispatch_tx, fun(conn, 103, 0, 502, RequestHash) ->
        [First | Rest] = get(client_fix_parts),
        put(client_fix_parts, [
            First#{
                <<"provider_state">> => <<"dispatching">>,
                <<"dispatch_intent_id">> => 502,
                <<"request_hash">> => RequestHash
            }
            | Rest
        ]),
        {ok, 1}
    end),
    meck:expect(talk_record_client_repo, touch_tx, fun(conn, 103) -> {ok, 1} end).

ancestor_fixture() ->
    Source = #{
        <<"part_id">> => 901,
        <<"sha256">> => binary:copy(<<"a">>, 64),
        <<"offset_ms">> => 0,
        <<"duration_ms">> => 2000,
        <<"byte_size">> => 2000
    },
    SourceHash = talk_record_client_contract:hash(Source),
    Chunk = fun(Sequence, Start, End) ->
        #{
            <<"seq">> => Sequence,
            <<"source_part_manifest_hash">> => SourceHash,
            <<"source_ranges">> => [
                #{
                    <<"part_id">> => 901,
                    <<"sha256">> => maps:get(<<"sha256">>, Source),
                    <<"start_ms">> => Start,
                    <<"end_ms">> => End
                }
            ],
            <<"offset_ms">> => Start,
            <<"duration_ms">> => End - Start,
            <<"byte_size">> => 1000
        }
    end,
    Plan = #{
        <<"overlap_rule">> => <<"none">>, <<"chunks">> => [Chunk(0, 0, 1000), Chunk(1, 1000, 2000)]
    },
    Run = #{
        <<"id">> => 103,
        <<"record_id">> => 7,
        <<"job_id">> => 31,
        <<"attempt">> => 3,
        <<"revision">> => 1,
        <<"state">> => <<"active">>,
        <<"provider">> => <<"minimax">>,
        <<"model">> => <<"asr-1.0">>,
        <<"policy_version">> => <<"p1">>,
        <<"config_version">> => binary:copy(<<"b">>, 64),
        <<"input_manifest_hash">> => SourceHash,
        <<"input_content_revision">> => 1,
        <<"input_transcript_version">> => null,
        <<"input_snapshot">> => #{<<"parts">> => [Source]},
        <<"chunk_plan_hash">> => null
    },
    {Run, Plan, Chunk(0, 0, 1000)}.

context_before_record_and_locked_recheck_test_() ->
    [
        {atom_to_list(Scenario), fun() -> context_probe(Scenario, Expected) end}
     || {Scenario, Expected} <- [
            {organization, ok},
            {personal, ok},
            {changed_scope, context_changed},
            {changed_org, context_changed},
            {changed_workspace, context_changed},
            {lost_visibility, not_found},
            {lost_owner, forbidden},
            {new_owner_with_editor, ok},
            {deleted, not_found},
            {inactive_custody, context_changed},
            {inactive_user, forbidden},
            {inactive_org, context_changed}
        ]
    ].

context_probe(Scenario, Expected) ->
    Modules = [talk_record_repo, talk_record_client_repo, elib_pg],
    lists:foreach(fun(Module) -> meck:new(Module, [passthrough, no_link]) end, Modules),
    Record = context_record(Scenario),
    Locked = locked_record(Scenario, Record),
    put(client_fix_context, false),
    put(client_fix_locked, false),
    put(client_fix_visible, 0),
    try
        meck:expect(talk_record_repo, find_visible_tx, fun(conn, 10, 7) ->
            Count = get(client_fix_visible),
            put(client_fix_visible, Count + 1),
            case Count of
                0 ->
                    ?assertEqual(false, get(client_fix_locked)),
                    {ok, Record};
                1 ->
                    ?assertEqual(true, get(client_fix_locked)),
                    case Scenario of
                        lost_visibility -> {error, not_found};
                        _ -> {ok, Locked}
                    end
            end
        end),
        meck:expect(talk_record_client_repo, lock_record_tx, fun(conn, 7) ->
            ?assert(Scenario =:= personal orelse get(client_fix_context)),
            put(client_fix_locked, true),
            {ok, Locked}
        end),
        meck:expect(elib_pg, query, fun(conn, Sql, Args) -> context_query(Scenario, Sql, Args) end),
        case Expected of
            ok ->
                ?assertEqual(
                    Locked,
                    talk_record_client_gate:record_tx(
                        conn, #{uid => 10, did => <<"device">>}, 7, write
                    )
                ),
                ?assertEqual(2, get(client_fix_visible));
            _ ->
                ?assertThrow(
                    {talk_client_abort, Expected},
                    talk_record_client_gate:record_tx(
                        conn,
                        #{uid => 10, did => <<"device">>},
                        7,
                        write
                    )
                )
        end,
        ?assert(lists:all(fun meck:validate/1, Modules))
    after
        lists:foreach(fun meck:unload/1, Modules),
        lists:foreach(fun erase/1, [client_fix_context, client_fix_locked, client_fix_visible])
    end.

context_query(Scenario, Sql, Args) ->
    case
        {
            binary:match(Sql, <<"FROM public.\"user\"">>),
            binary:match(Sql, <<"FROM talk_record_grant">>)
        }
    of
        {{_, _}, _} ->
            ?assertEqual(true, get(client_fix_locked)),
            {ok,
                case Scenario of
                    inactive_user -> [];
                    _ -> [#{<<"id">> => 10}]
                end};
        {_, {_, _}} ->
            ?assertEqual(true, get(client_fix_locked)),
            {ok,
                case Scenario of
                    new_owner_with_editor -> [#{<<"id">> => 1}];
                    _ -> []
                end};
        _ ->
            ?assertEqual(false, get(client_fix_locked)),
            case {binary:match(Sql, <<"FROM organization o">>), Args} of
                {{_, _}, [17]} when Scenario =:= inactive_org -> {ok, []};
                _ ->
                    put(client_fix_context, true),
                    {ok, [
                        #{
                            <<"user_id">> => 10,
                            <<"role">> => <<"owner">>,
                            <<"status">> => <<"active">>
                        }
                    ]}
            end
    end.

context_record(personal) ->
    (context_record(organization))#{
        <<"scope">> => <<"personal">>,
        <<"organization_id">> => null,
        <<"workspace_id">> => null
    };
context_record(_) ->
    #{
        <<"id">> => 7,
        <<"scope">> => <<"organization">>,
        <<"organization_id">> => 17,
        <<"workspace_id">> => 27,
        <<"owner_user_id">> => 10,
        <<"deleted_at">> => null,
        <<"custody_status">> => <<"active">>
    }.

locked_record(changed_scope, Record) ->
    Record#{<<"scope">> => <<"personal">>};
locked_record(changed_org, Record) ->
    Record#{<<"organization_id">> => 18};
locked_record(changed_workspace, Record) ->
    Record#{<<"workspace_id">> => 28};
locked_record(Scenario, Record) when Scenario =:= lost_owner; Scenario =:= new_owner_with_editor ->
    Record#{<<"owner_user_id">> => 11};
locked_record(deleted, Record) ->
    Record#{<<"deleted_at">> => 1};
locked_record(inactive_custody, Record) ->
    Record#{<<"custody_status">> => <<"pending_transfer">>};
locked_record(_, Record) ->
    Record.
