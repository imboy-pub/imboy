-module(talk_record_client_contract_tests).
-include_lib("eunit/include/eunit.hrl").

request_exact_consent_test() ->
    {P, B} = request(),
    ?assertMatch({ok, #{provider := <<"doubao">>}}, talk_record_client_contract:request(B, P)),
    ?assertEqual(
        {error, client_schema_invalid},
        talk_record_client_contract:request(B#{<<"api_key">> => <<"secret">>}, P)
    ),
    C = maps:get(<<"processing_consent">>, B),
    ?assertEqual(
        {error, client_schema_invalid},
        talk_record_client_contract:request(
            B#{<<"processing_consent">> => C#{<<"provider">> => <<"other">>}}, P
        )
    ),
    ?assertEqual(
        {error, client_schema_invalid},
        talk_record_client_contract:request(
            B#{<<"processing_consent">> => C#{<<"confirmed">> => false}}, P
        )
    ).

exact_audio_coverage_test() ->
    P = source(),
    Body = plan(P),
    Snapshot = #{<<"parts">> => [P]},
    ?assertMatch({ok, _}, talk_record_client_contract:plan(<<"transcribe">>, Body, Snapshot)),
    [Chunk] = maps:get(<<"chunks">>, Body),
    lists:foreach(
        fun(Bad) ->
            ?assertEqual(
                {error, client_schema_invalid},
                talk_record_client_contract:plan(<<"transcribe">>, Bad, Snapshot)
            )
        end,
        [
            Body#{<<"chunks">> => []},
            Body#{<<"chunks">> => [Chunk, Chunk]},
            Body#{<<"chunks">> => [Chunk#{<<"seq">> => 1}]},
            Body#{<<"chunks">> => [Chunk#{<<"duration_ms">> => 999}]},
            Body#{<<"overlap_rule">> => <<"guess">>},
            Body#{<<"url">> => <<"https://evil">>}
        ]
    ).

provider_limits_test() ->
    lists:foreach(
        fun(P) ->
            ?assertEqual(
                {error, client_schema_invalid},
                talk_record_client_contract:plan(
                    <<"transcribe">>, plan(P), #{<<"parts">> => [P]}
                )
            )
        end,
        [(source())#{<<"duration_ms">> => 500001}, (source())#{<<"byte_size">> => 50000001}]
    ),
    Oversized = (source())#{<<"byte_size">> => 50000001},
    ?assertEqual(
        {error, client_schema_invalid},
        talk_record_client_contract:plan(
            <<"transcribe">>,
            audio_plan([slice(Oversized, 0, 0, 1000, 100)]),
            #{<<"parts">> => [Oversized]}
        )
    ).

oversized_source_can_be_sliced_test() ->
    lists:foreach(
        fun({Duration, Bytes, Boundary}) ->
            P = (source())#{<<"duration_ms">> => Duration, <<"byte_size">> => Bytes},
            Body = audio_plan([
                slice(P, 0, 0, Boundary, 30000000), slice(P, 1, Boundary, Duration, 30000001)
            ]),
            ?assertMatch(
                {ok, _},
                talk_record_client_contract:plan(<<"transcribe">>, Body, #{<<"parts">> => [P]})
            )
        end,
        [{600000, 60000001, 300000}, {1000, 60000001, 500}]
    ).

provider_limit_boundaries_test() ->
    P = (source())#{<<"duration_ms">> => 500000, <<"byte_size">> => 50000000},
    ?assertMatch(
        {ok, _}, talk_record_client_contract:plan(<<"transcribe">>, plan(P), #{<<"parts">> => [P]})
    ).

invalid_slice_plans_test_() ->
    P = (source())#{<<"duration_ms">> => 600000, <<"byte_size">> => 60000001},
    First = slice(P, 0, 0, 300000, 30000000),
    Second = slice(P, 1, 300000, 600000, 30000001),
    [Range] = maps:get(<<"source_ranges">>, Second),
    Cases = [
        {"gap", [First, slice(P, 1, 300001, 600000, 30000001)]},
        {"overlap", [First, slice(P, 1, 299999, 600000, 30000001)]},
        {"reordered", [Second, First]},
        {"missing_tail", [First]},
        {"missing_head", [Second]},
        {"duplicate", [First, First, Second]},
        {"seq_gap", [First, Second#{<<"seq">> => 2}]},
        {"foreign_source", [First, Second#{<<"source_ranges">> => [Range#{<<"part_id">> => 10}]}]},
        {"changed_hash", [
            First, Second#{<<"source_part_manifest_hash">> => binary:copy(<<"b">>, 64)}
        ]},
        {"offset", [First, Second#{<<"offset_ms">> => 300001}]},
        {"duration", [First, Second#{<<"duration_ms">> => 299999}]},
        {"zero_duration", [First, slice(P, 1, 300000, 300000, 1), Second]},
        {"past_end", [First, slice(P, 1, 300000, 600001, 30000001)]},
        {"duration_limit", [
            slice(P, 0, 0, 500001, 30000000), slice(P, 1, 500001, 600000, 30000001)
        ]},
        {"size_limit", [First, Second#{<<"byte_size">> => 50000001}]},
        {"zero_size", [First, Second#{<<"byte_size">> => 0}]},
        {"unknown_field", [First, Second#{<<"url">> => <<"file:///tmp/audio">>}]},
        {"false_whole_size", [slice(P, 0, 0, 600000, 100)]}
    ],
    [
        {Name,
            ?_assertEqual(
                {error, client_schema_invalid},
                talk_record_client_contract:plan(
                    <<"transcribe">>, audio_plan(Chunks), #{<<"parts">> => [P]}
                )
            )}
     || {Name, Chunks} <- Cases
    ].

split_plan_reconstruction_and_publish_offsets_test() ->
    P = (source())#{<<"duration_ms">> => 600000, <<"byte_size">> => 60000001},
    Next = (source())#{<<"part_id">> => 10, <<"offset_ms">> => 600000},
    Chunks = [
        slice(P, 0, 0, 300000, 30000000),
        slice(P, 1, 300000, 600000, 30000001),
        slice(Next, 2, 0, 1000, 100)
    ],
    Body = audio_plan(Chunks),
    Snapshot = #{<<"parts">> => [P, Next]},
    {ok, #{hash := Hash}} = talk_record_client_contract:plan(<<"transcribe">>, Body, Snapshot),
    Stored = [
        C#{
            <<"source_ranges">> => jsone:encode(maps:get(<<"source_ranges">>, C)),
            <<"result">> => #{
                <<"speakers">> => [#{<<"label">> => <<"S1">>}],
                <<"segments">> => [
                    #{
                        <<"speaker_label">> => <<"S1">>,
                        <<"start_ms">> => 0,
                        <<"end_ms">> => 100,
                        <<"text">> => <<"hello">>
                    }
                ]
            }
        }
     || C <- Chunks
    ],
    Rebuilt = talk_record_client_operations:reconstructed_plan(<<"transcribe">>, Stored),
    ?assertEqual(Body, Rebuilt),
    ?assertMatch(
        {ok, #{hash := Hash}}, talk_record_client_contract:plan(<<"transcribe">>, Rebuilt, Snapshot)
    ),
    #{segments := Segments} = talk_record_client_publish:transcript(Stored, <<"volc.seedasr.auc">>),
    ?assertEqual([0, 300000, 600000], [maps:get(<<"start_ms">>, S) || S <- Segments]),
    ?assertEqual([<<"0:S1">>, <<"1:S1">>, <<"2:S1">>], [
        maps:get(<<"speaker_label">>, S)
     || S <- Segments
    ]).

slice(P, Seq, Start, End, Bytes) ->
    #{
        <<"seq">> => Seq,
        <<"source_part_manifest_hash">> => talk_record_client_contract:hash(P),
        <<"source_ranges">> => [
            #{
                <<"part_id">> => maps:get(<<"part_id">>, P),
                <<"sha256">> => maps:get(<<"sha256">>, P),
                <<"start_ms">> => Start,
                <<"end_ms">> => End
            }
        ],
        <<"offset_ms">> => maps:get(<<"offset_ms">>, P) + Start,
        <<"duration_ms">> => End - Start,
        <<"byte_size">> => Bytes
    }.
audio_plan(Chunks) -> #{<<"overlap_rule">> => <<"none">>, <<"chunks">> => Chunks}.

summary_exact_sources_test() ->
    Segs = [#{<<"id">> => 15, <<"text">> => <<"hello">>}],
    C = #{
        <<"seq">> => 0,
        <<"source_segment_ids">> => [15],
        <<"source_hash">> => talk_record_client_contract:hash(Segs)
    },
    Body = #{<<"overlap_rule">> => <<"none">>, <<"chunks">> => [C]},
    ?assertMatch(
        {ok, _}, talk_record_client_contract:plan(<<"summarize">>, Body, #{<<"segments">> => Segs})
    ),
    ?assertEqual(
        {error, client_schema_invalid},
        talk_record_client_contract:plan(
            <<"summarize">>,
            Body#{<<"chunks">> => [C#{<<"source_segment_ids">> => [16]}]},
            #{<<"segments">> => Segs}
        )
    ).

secret_and_float_hash_rejected_test() ->
    ?assertEqual(
        false, talk_record_client_contract:allowed(#{<<"api_key">> => <<"x">>}, [<<"state">>])
    ),
    ?assertException(
        error, {badmatch, {error, _}}, talk_record_client_contract:hash(#{<<"seconds">> => 1.5})
    ).

constant_token_comparison_test() ->
    ?assert(talk_record_client_contract:constant_equal(<<"abcd">>, <<"abcd">>)),
    ?assertNot(talk_record_client_contract:constant_equal(<<"abcd">>, <<"abce">>)),
    ?assertNot(talk_record_client_contract:constant_equal(<<"abcd">>, <<"abc">>)),
    ?assertNot(talk_record_client_contract:constant_equal(undefined, <<"abc">>)).

token_binds_actor_device_attempt_test() ->
    Key = binary:copy(<<"x">>, 32),
    T = talk_record_client_policy:token(Key, 1, 1, 10, <<"a">>),
    ?assertEqual(T, talk_record_client_policy:token(Key, 1, 1, 10, <<"a">>)),
    lists:foreach(fun(Other) -> ?assertNotEqual(T, Other) end, [
        talk_record_client_policy:token(Key, 1, 2, 10, <<"a">>),
        talk_record_client_policy:token(Key, 1, 1, 11, <<"a">>),
        talk_record_client_policy:token(Key, 1, 1, 10, <<"b">>)
    ]).

enterprise_policy_default_deny_test() ->
    R = #{<<"scope">> => <<"organization">>, <<"organization_id">> => 7},
    ?assertNot(talk_record_client_policy:allowed(R, #{})),
    ?assert(talk_record_client_policy:allowed(R, #{enterprise_organizations => [7]})),
    ?assertNot(talk_record_client_policy:allowed(R, #{enterprise_organizations => [8]})).

public_view_omits_secrets_test() ->
    Run = #{
        <<"id">> => 1,
        <<"revision">> => 2,
        <<"lease_token_hash">> => <<"secret">>,
        <<"device_session_id">> => <<"device">>
    },
    V = talk_record_client_ds:view(Run, []),
    ?assertEqual(false, maps:is_key(<<"lease_token_hash">>, V)),
    ?assertEqual(false, maps:is_key(<<"device_session_id">>, V)),
    ?assertEqual(<<"client-run:1:2">>, maps:get(<<"etag">>, V)).

request() ->
    P = #{
        <<"policy_version">> => <<"p1">>,
        <<"config_version">> => <<"c1">>,
        <<"providers">> => #{
            <<"transcribe">> => #{
                <<"provider">> => <<"doubao">>,
                <<"model">> => <<"volc.seedasr.auc">>
            }
        }
    },
    Pin = (maps:without([<<"providers">>], P))#{
        <<"execution_mode">> => <<"client_byok">>,
        <<"provider">> => <<"doubao">>,
        <<"model">> => <<"volc.seedasr.auc">>
    },
    {P, Pin#{
        <<"mode">> => <<"transcribe">>, <<"processing_consent">> => Pin#{<<"confirmed">> => true}
    }}.
source() ->
    #{
        <<"part_id">> => 9,
        <<"sha256">> => binary:copy(<<"a">>, 64),
        <<"duration_ms">> => 1000,
        <<"offset_ms">> => 0,
        <<"byte_size">> => 100
    }.
plan(P) ->
    #{
        <<"overlap_rule">> => <<"none">>,
        <<"chunks">> => [
            #{
                <<"seq">> => 0,
                <<"source_part_manifest_hash">> => talk_record_client_contract:hash(P),
                <<"source_ranges">> => [
                    #{
                        <<"part_id">> => maps:get(<<"part_id">>, P),
                        <<"sha256">> => maps:get(<<"sha256">>, P),
                        <<"start_ms">> => 0,
                        <<"end_ms">> => maps:get(<<"duration_ms">>, P)
                    }
                ],
                <<"offset_ms">> => maps:get(<<"offset_ms">>, P),
                <<"duration_ms">> => maps:get(<<"duration_ms">>, P),
                <<"byte_size">> => maps:get(<<"byte_size">>, P)
            }
        ]
    }.

result_schema_and_hash_fences_test() ->
    R = #{
        <<"model_version">> => <<"volc.seedasr.auc">>,
        <<"status">> => <<"ready">>,
        <<"speakers">> => [#{<<"label">> => <<"S1">>}],
        <<"segments">> => [
            #{
                <<"start_ms">> => 0,
                <<"end_ms">> => 1000,
                <<"speaker_label">> => <<"S1">>,
                <<"text">> => <<"hello">>
            }
        ]
    },
    F = fun(Result) ->
        talk_record_client_contract:result(
            <<"transcribe">>,
            #{
                <<"result">> => Result,
                <<"result_hash">> => talk_record_client_contract:hash(Result)
            },
            #{<<"duration_ms">> => 1000},
            #{<<"source_kind">> => <<"assembly">>}
        )
    end,
    ?assertMatch({ok, _}, F(R)),
    [S] = maps:get(<<"segments">>, R),
    lists:foreach(fun(Bad) -> ?assertEqual({error, client_schema_invalid}, F(Bad)) end, [
        R#{<<"segments">> => [S#{<<"end_ms">> => 1001}]},
        R#{<<"segments">> => [S#{<<"speaker_label">> => <<"S2">>}]},
        R#{<<"language">> => #{<<"api_key">> => <<"never stored">>}},
        R#{<<"api_key">> => <<"never stored">>}
    ]),
    ?assertEqual(
        {error, client_schema_invalid},
        talk_record_client_contract:result(
            <<"transcribe">>,
            #{<<"result">> => R, <<"result_hash">> => binary:copy(<<"0">>, 64)},
            #{<<"duration_ms">> => 1000},
            #{<<"source_kind">> => <<"assembly">>}
        )
    ).

summary_foreign_references_rejected_test() ->
    Snapshot = #{<<"segments">> => [#{<<"id">> => 1}]},
    R = #{
        <<"model_version">> => <<"glm-5.3-flash">>,
        <<"overview">> => <<"draft">>,
        <<"decisions">> => [],
        <<"actions">> => [#{<<"text">> => <<"act">>, <<"segment_ids">> => [2]}]
    },
    Body = #{<<"result">> => R, <<"result_hash">> => talk_record_client_contract:hash(R)},
    ?assertEqual(
        {error, client_schema_invalid},
        talk_record_client_contract:result(
            <<"summarize">>, Body, #{}, Snapshot
        )
    ).
