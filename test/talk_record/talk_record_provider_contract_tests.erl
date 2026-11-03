-module(talk_record_provider_contract_tests).
-include_lib("eunit/include/eunit.hrl").
profile_frozen_hash_test() ->
    ?assertEqual(
        <<"d723bf03c21bf27f6d35a38d6134c2cdf33ebe4a6700b35c8bd98ec27fc4cfab">>,
        talk_record_client_contract:hash(talk_record_provider_contract:profile(16000))
    ).
submit_once_test() ->
    T = task(),
    B = #{<<"profile_hash">> => maps:get(<<"profile_hash">>, T)},
    {ok, D, true} = talk_record_provider_contract:transition(submit_intent, T, B, 100),
    ?assertEqual(400, maps:get(<<"source_expires_at">>, D)),
    ?assertEqual(
        {ok, #{}, false},
        talk_record_provider_contract:transition(submit_intent, maps:merge(T, D), B, 200)
    ).
query_after_source_expiry_test() ->
    T = (task())#{
        <<"state">> => <<"accepted">>,
        <<"provider_task_id">> => <<"returned-task">>,
        <<"source_expires_at">> => 400
    },
    ?assertEqual(
        {ok, #{}, false}, talk_record_provider_contract:transition(query_intent, T, #{}, 10000)
    ),
    ?assertEqual(
        {ok, #{<<"state">> => <<"processing">>}, false},
        talk_record_provider_contract:transition(
            query_receipt,
            T,
            #{<<"provider_task_id">> => <<"returned-task">>, <<"state">> => <<"processing">>},
            10000
        )
    ).
no_taskid_no_query_test() ->
    T = (task())#{<<"state">> => <<"submit_outcome_unknown">>, <<"intent_id">> => 9},
    ?assertMatch({error, _}, talk_record_provider_contract:transition(query_intent, T, #{}, 10000)).
returned_id_is_not_client_uuid_test() ->
    T = (task())#{<<"state">> => <<"submit_outcome_unknown">>, <<"intent_id">> => 9},
    B = #{
        <<"client_request_id">> => uuid(),
        <<"provider_task_id">> => <<"supplier-returned-id">>,
        <<"state">> => <<"accepted">>
    },
    {ok, D, false} = talk_record_provider_contract:transition(reconcile_receipt, T, B, 1000),
    ?assertEqual(<<"supplier-returned-id">>, maps:get(<<"provider_task_id">>, D)),
    ?assertMatch(
        {error, _},
        talk_record_provider_contract:transition(
            reconcile_receipt,
            maps:merge(T, D),
            B#{<<"provider_task_id">> => <<"replacement">>},
            1001
        )
    ).
foreign_receipt_uuid_test() ->
    T = (task())#{<<"state">> => <<"submit_outcome_unknown">>, <<"intent_id">> => 9},
    ?assertMatch(
        {error, _},
        talk_record_provider_contract:transition(
            reconcile_receipt,
            T,
            #{
                <<"client_request_id">> => <<"00000000-0000-4000-8000-000000000002">>,
                <<"provider_task_id">> => <<"task">>,
                <<"state">> => <<"accepted">>
            },
            1000
        )
    ).
no_provider_urls_or_keys_test() ->
    lists:foreach(
        fun(K) ->
            ?assertMatch(
                {error, _},
                talk_record_provider_contract:validate(
                    submit_intent,
                    #{<<"profile_hash">> => maps:get(<<"profile_hash">>, task()), K => <<"secret">>}
                )
            )
        end,
        [<<"api_key">>, <<"url">>, <<"path">>, <<"error">>, <<"device_id">>]
    ).
cancelled_cannot_query_test() ->
    T = (task())#{
        <<"state">> => <<"accepted">>, <<"provider_task_id">> => <<"task">>, <<"cancelled_at">> => 1
    },
    ?assertMatch({error, _}, talk_record_provider_contract:transition(query_intent, T, #{}, 500)).
query_result_wrong_task_test() ->
    T = (task())#{<<"state">> => <<"processing">>, <<"provider_task_id">> => <<"task">>},
    ?assertMatch(
        {error, _},
        talk_record_provider_contract:transition(
            query_receipt,
            T,
            #{<<"provider_task_id">> => <<"other">>, <<"state">> => <<"processing">>},
            500
        )
    ).
source_query_strict_test() ->
    ?assertEqual(
        <<"a.b.c">>, talk_record_client_audio_handler:ticket([{<<"ticket">>, <<"a.b.c">>}])
    ),
    lists:foreach(
        fun(Q) -> ?assertEqual(undefined, talk_record_client_audio_handler:ticket(Q)) end,
        [
            [],
            [{<<"ticket">>, <<>>}],
            [{<<"ticket">>, true}],
            [{<<"ticket">>, <<"x">>}, {<<"ticket">>, <<"x">>}],
            [{<<"ticket">>, <<"x">>}, {<<"other">>, <<"y">>}]
        ]
    ).
range_no_multiple_test() ->
    ?assertEqual(
        {error, range_not_satisfiable},
        talk_record_client_audio_handler:byte_range(<<"bytes=0-2,4-5">>, 100)
    ).
task() ->
    #{
        <<"state">> => <<"prepared">>,
        <<"cancelled_at">> => null,
        <<"provider_task_id">> => null,
        <<"intent_id">> => null,
        <<"profile_hash">> => talk_record_client_contract:hash(
            talk_record_provider_contract:profile(16000)
        ),
        <<"client_request_id">> => uuid()
    }.
uuid() -> <<"00000000-0000-4000-8000-000000000001">>.

resubmission_matrix_test() ->
    lists:foreach(
        fun(State) ->
            Known = #{<<"state">> => State, <<"provider_task_id">> => <<"returned-id">>},
            ?assertNot(talk_record_provider_contract:can_resubmit(Known))
        end,
        [
            <<"prepared">>,
            <<"accepted">>,
            <<"processing">>,
            <<"submit_outcome_unknown">>,
            <<"result_ready">>,
            <<"published">>,
            <<"failed">>
        ]
    ),
    ?assert(
        talk_record_provider_contract:can_resubmit(#{
            <<"state">> => <<"submit_outcome_unknown">>, <<"provider_task_id">> => null
        })
    ),
    lists:foreach(
        fun(State) ->
            ?assert(
                talk_record_provider_contract:can_resubmit(
                    #{<<"state">> => State, <<"provider_task_id">> => <<"returned-id">>}
                )
            )
        end,
        [<<"rejected">>]
    ).

pg_matrix_covers_all_mixed_nulls_test() ->
    Matrix = talk_record_provider_constraints_pg_checks:matrix(),
    Mixed = [W || {reject, _, W} <- Matrix, lists:member(null, W), W =/= [null, null, null, null]],
    ?assertEqual(14, length(Mixed)),
    ?assertEqual(3, length([ok || {allow, _, _} <- Matrix])).

known_pending_view_and_recovery_block_resubmit_test() ->
    Base = #{
        <<"id">> => 1,
        <<"revision">> => 1,
        <<"request_profile">> => talk_record_provider_contract:profile(16000),
        <<"assembly">> => #{},
        <<"source_authorization_id">> => 1,
        <<"cancelled_at">> => null,
        <<"provider_task_id">> => <<"returned-id">>
    },
    lists:foreach(
        fun(State) ->
            T = Base#{<<"state">> => State},
            Actions = maps:get(<<"allowed_actions">>, talk_record_provider_contract:view(T)),
            ?assertNot(lists:member(<<"resubmission">>, Actions)),
            ?assert(lists:member(<<"query_execution">>, Actions)),
            ?assertThrow(
                {talk_client_abort, client_execution_conflict},
                talk_record_provider_recovery:create(
                    no_db,
                    resubmission,
                    #{},
                    #{},
                    #{},
                    T,
                    #{},
                    #{<<"ack_possible_duplicate_charge">> => true},
                    #{}
                )
            )
        end,
        [<<"accepted">>, <<"processing">>, <<"submit_outcome_unknown">>]
    ).

prepared_resume_requires_exact_no_intent_facts_test() ->
    T = unstarted(),
    ?assert(talk_record_provider_contract:can_resume_preparation(T)),
    lists:foreach(
        fun(K) ->
            ?assertNot(talk_record_provider_contract:can_resume_preparation(T#{K => 1})),
            ?assertNot(talk_record_provider_contract:can_resume_preparation(maps:remove(K, T)))
        end,
        [
            <<"intent_id">>,
            <<"request_hash">>,
            <<"provider_task_id">>,
            <<"source_issued_at">>,
            <<"source_expires_at">>,
            <<"result">>,
            <<"result_hash">>,
            <<"outcome_unknown_at">>,
            <<"cancelled_at">>
        ]
    ),
    ?assertNot(talk_record_provider_contract:can_resubmit(T)),
    ?assertEqual(
        ok,
        talk_record_provider_contract:validate(
            resubmission,
            #{<<"processing_consent">> => #{}, <<"ack_possible_duplicate_charge">> => false}
        )
    ).
query_http_failure_cannot_be_reported_as_task_failed_test() ->
    ?assertMatch(
        {error, client_schema_invalid},
        talk_record_provider_contract:validate(
            query_receipt,
            #{<<"provider_task_id">> => <<"returned-id">>, <<"state">> => <<"failed">>}
        )
    ),
    ?assertEqual(
        ok,
        talk_record_provider_contract:validate(
            query_receipt,
            #{<<"provider_task_id">> => <<"returned-id">>, <<"state">> => <<"unknown">>}
        )
    ).
unstarted() ->
    maps:from_list([
        {<<"state">>, <<"prepared">>}
        | [
            {K, null}
         || K <- [
                <<"intent_id">>,
                <<"request_hash">>,
                <<"provider_task_id">>,
                <<"source_issued_at">>,
                <<"source_expires_at">>,
                <<"result">>,
                <<"result_hash">>,
                <<"outcome_unknown_at">>,
                <<"cancelled_at">>
            ]
        ]
    ]).

%% no-backend-ffmpeg V1：非 16k 黄金 hash（迁移 00000242 白名单同源值）。
profile_non_16k_golden_hash_test() ->
    ?assertEqual(
        <<"cab55b1c02a75033e4b7e396abb14ba29d4ca2d3ebfffdc4de4c885ea76bbb16">>,
        talk_record_client_contract:hash(talk_record_provider_contract:profile(44100))
    ).

%% 冻结 profile 白名单校验：固定策略精确 + ADTS 采样率合法 + 漂移拒绝。
profile_rate_whitelist_test() ->
    {ok, 44100} = talk_record_provider_contract:profile_rate(
        talk_record_provider_contract:profile(44100)
    ),
    {ok, 16000} = talk_record_provider_contract:profile_rate(
        talk_record_provider_contract:profile(16000)
    ),
    lists:foreach(
        fun(Rate) ->
            {ok, Rate} = talk_record_provider_contract:profile_rate(
                talk_record_provider_contract:profile(Rate)
            )
        end,
        talk_record_provider_contract:rates()
    ),
    %% 未知采样率：白名单外拒绝。
    {error, profile_rate_invalid} = talk_record_provider_contract:profile_rate(
        talk_record_provider_contract:profile(12345)
    ),
    %% request 策略漂移拒绝（固定策略不放宽）。
    Drifted = (talk_record_provider_contract:profile(16000))#{
        <<"request">> =>
            (maps:get(<<"request">>, talk_record_provider_contract:profile(16000)))#{
                <<"enable_punc">> => false
            }
    },
    {error, profile_policy_invalid} = talk_record_provider_contract:profile_rate(Drifted),
    %% audio 形态漂移拒绝（声道/格式/位深固定）。
    {error, profile_policy_invalid} = talk_record_provider_contract:profile_rate(
        (talk_record_provider_contract:profile(16000))#{
            <<"audio">> =>
                (maps:get(<<"audio">>, talk_record_provider_contract:profile(16000)))#{
                    <<"channel">> => 2
                }
        }
    ),
    %% 坏形态拒绝。
    {error, profile_policy_invalid} = talk_record_provider_contract:profile_rate(<<"junk">>).
