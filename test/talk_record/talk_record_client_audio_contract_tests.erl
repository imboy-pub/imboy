-module(talk_record_client_audio_contract_tests).
-include_lib("eunit/include/eunit.hrl").
-export([authority/0]).

durable_authority_and_expired_device_lease_test() ->
    A = authority(),
    Now = erlang:system_time(second),
    {ok, B} = talk_record_client_audio_contract:authorize(A, Now),
    ?assertEqual(<<"bigmodel">>, maps:get(<<"model_name">>, B)),
    ?assertEqual(<<"volc.seedasr.auc">>, maps:get(<<"resource_id">>, B)),
    ?assert(talk_record_client_audio_contract:validate_binding(B)),
    %% Provider task/source authorization, not a 300s device lease, bounds reading.
    Run = maps:get(run, A),
    J = maps:get(job, A),
    [Part] = maps:get(parts, A),
    ?assertMatch(
        {ok, _},
        check(A#{
            run => Run#{
                <<"state">> => <<"provider_outcome_unknown">>,
                <<"lease_expires_at_unix">> => Now - 100
            },
            job => J#{<<"attempt">> => 9, <<"state">> => <<"failed">>},
            parts => [Part#{<<"provider_state">> => <<"provider_outcome_unknown">>}]
        })
    ).

missing_durable_task_cannot_be_enabled_by_flags_test() ->
    A = authority(),
    ?assert(talk_record_client_audio_contract:enabled(maps:get(config, A))),
    ?assertEqual(denied(), check(maps:remove(provider_task, A))),
    ?assertEqual(
        {error, audio_source_denied}, talk_record_client_audio_repo:source_authority_tx(no_db, #{})
    ).

retained_or_held_bytes_do_not_restore_deleted_access_test() ->
    A = authority(),
    R = maps:get(record, A),
    Artifact = maps:get(artifact, A),
    lists:foreach(fun(Changed) -> ?assertEqual(denied(), check(Changed)) end, [
        A#{
            record => R#{<<"deleted_at">> => <<"synthetic-deleted">>},
            artifact => Artifact#{held => true}
        },
        A#{record => R#{<<"custody_status">> => <<"locked">>}},
        A#{artifact => Artifact#{<<"governance_purged">> => true}},
        A#{artifact => Artifact#{<<"lifecycle_state">> => <<"deleting">>}},
        A#{artifact => Artifact#{<<"reference_valid">> => false}},
        A#{artifact => Artifact#{<<"ref_state">> => <<"released">>}},
        A#{artifact => Artifact#{<<"cipher">> => #{<<"scheme">> => <<"aes">>}}}
    ]).

provider_parameters_and_input_versions_are_exact_test() ->
    A = authority(),
    J = maps:get(job, A),
    Pin = maps:get(<<"processor_input">>, J),
    lists:foreach(
        fun({K, V}) ->
            ?assertEqual(denied(), check(A#{job => J#{<<"processor_input">> => Pin#{K => V}}}))
        end,
        [
            {<<"resource_id">>, <<"other">>},
            {<<"model_name">>, <<"other">>},
            {<<"ssd_version">>, <<"301">>},
            {<<"enable_speaker_info">>, false},
            {<<"source_delivery">>, <<"arbitrary_url">>}
        ]
    ),
    R = maps:get(record, A),
    ?assertEqual(denied(), check(A#{record => R#{<<"content_revision">> => 4}})).

only_full_exact_published_assembly_is_authorized_test() ->
    A = authority(),
    [P] = maps:get(parts, A),
    Artifact = maps:get(artifact, A),
    Assembly = maps:get(assembly, A),
    lists:foreach(fun(Changed) -> ?assertEqual(denied(), check(Changed)) end, [
        A#{parts => []},
        A#{parts => [P, P]},
        A#{parts => [P#{<<"seq">> => 1}]},
        A#{parts => [P#{<<"offset_ms">> => 1}]},
        A#{parts => [P#{<<"source_ranges">> => []}]},
        A#{artifact => Artifact#{<<"id">> => 999}},
        A#{artifact => Artifact#{<<"file_hash256">> => hash_b()}},
        A#{artifact => Artifact#{<<"size">> => 134217728}},
        A#{assembly => Assembly#{<<"total_duration_ms">> => 7200001}},
        A#{parts => [P#{<<"run_id">> => 999}]}
    ]).

provider_task_identity_and_fixed_window_test() ->
    A = authority(),
    T = maps:get(provider_task, A),
    Now = erlang:system_time(second),
    lists:foreach(
        fun(Changed) -> ?assertEqual(denied(), check(A#{provider_task => Changed})) end, [
            T#{<<"actor_user_id">> => 999},
            T#{<<"intent_id">> => 999},
            T#{<<"request_hash">> => hash_b()},
            T#{<<"resource_id">> => <<"other">>},
            T#{<<"attachment_id">> => 999},
            T#{<<"state">> => <<"cancelled">>},
            T#{<<"source_expires_at">> => Now},
            T#{<<"source_expires_at">> => Now + 301}
        ]
    ).

source_binding_revokes_on_acl_context_or_epoch_change_test() ->
    A = authority(),
    {ok, B} = check(A),
    R = maps:get(record, A),
    lists:foreach(
        fun(Changed) ->
            {ok, Other} = check(Changed),
            ?assertNotEqual(B, Other)
        end,
        [
            A#{auth_epoch => 2},
            A#{record => R#{<<"acl_revision">> => 2}},
            A#{record => R#{<<"owner_user_id">> => 11}}
        ]
    ).

range_and_url_are_bounded_test() ->
    ?assertEqual({ok, {0, 100}}, talk_record_client_audio_contract:range({0, 100}, 100)),
    lists:foreach(
        fun(Range) ->
            ?assertEqual(
                {error, audio_source_range_invalid},
                talk_record_client_audio_contract:range(Range, 100)
            )
        end,
        [{-1, 1}, {0, 0}, {99, 2}, <<"bytes=0-1,4-8">>]
    ),
    {ok, U} = talk_record_client_audio_contract:build_url(
        <<"https://EXAMPLE.com:443/">>, <<"abc.def.sig">>
    ),
    ?assertEqual(
        <<"https://example.com/api/v1/talk-records/client-audio-source?ticket=abc.def.sig">>, U
    ),
    lists:foreach(
        fun(Origin) ->
            ?assertEqual(
                denied(),
                talk_record_client_audio_contract:build_url(
                    Origin, <<"abc.def.sig">>
                )
            )
        end,
        [
            <<"http://example.com">>,
            <<"https://user@example.com">>,
            <<"https://example.com/path">>,
            <<"https://example.com?ticket=other">>
        ]
    ),
    ?assertEqual(
        denied(),
        talk_record_client_audio_contract:build_url(<<"https://example.com">>, <<"x&ticket=y">>)
    ).

check(A) -> talk_record_client_audio_contract:authorize(A, erlang:system_time(second)).
denied() -> {error, audio_source_denied}.
hash() -> binary:copy(<<"a">>, 64).
hash_b() -> binary:copy(<<"b">>, 64).

authority() ->
    Now = erlang:system_time(second),
    Pin = pin(),
    #{
        record => #{
            <<"id">> => 7,
            <<"owner_user_id">> => 10,
            <<"scope">> => <<"personal">>,
            <<"organization_id">> => null,
            <<"workspace_id">> => null,
            <<"deleted_at">> => null,
            <<"custody_status">> => <<"active">>,
            <<"media_status">> => <<"available">>,
            <<"processing_mode">> => <<"transcribe">>,
            <<"content_revision">> => 3,
            <<"acl_revision">> => 1
        },
        job => #{
            <<"id">> => 20,
            <<"record_id">> => 7,
            <<"kind">> => <<"transcribe">>,
            <<"execution_mode">> => <<"client_byok">>,
            <<"state">> => <<"running">>,
            <<"attempt">> => 1,
            <<"processor_input">> => Pin,
            <<"input_manifest_hash">> => hash(),
            <<"input_content_revision">> => 3,
            <<"input_transcript_version">> => null
        },
        run => run(Now),
        parts => [part()],
        assembly => assembly(),
        artifact => artifact(),
        provider_task => task(Now),
        config => #{
            audio_source_enabled => true,
            deployment_id => hash(),
            public => #{
                <<"policy_version">> => <<"p1">>,
                <<"config_version">> => hash(),
                <<"providers">> => #{<<"transcribe">> => Pin}
            }
        },
        auth_epoch => 1
    }.

pin() ->
    #{
        <<"processing_mode">> => <<"transcribe">>,
        <<"provider">> => <<"doubao">>,
        <<"model">> => <<"volc.seedasr.auc">>,
        <<"resource_id">> => <<"volc.seedasr.auc">>,
        <<"model_name">> => <<"bigmodel">>,
        <<"enable_speaker_info">> => true,
        <<"show_utterances">> => true,
        <<"ssd_version">> => <<"300">>,
        <<"enable_itn">> => true,
        <<"enable_punc">> => true,
        <<"enable_ddc">> => false,
        <<"protocol">> => <<"doubao_auc_v1">>,
        <<"source_delivery">> => <<"assembly_url_v1">>,
        <<"policy_version">> => <<"p1">>,
        <<"config_version">> => hash()
    }.
run(Now) ->
    #{
        <<"id">> => 30,
        <<"record_id">> => 7,
        <<"job_id">> => 20,
        <<"actor_user_id">> => 10,
        <<"attempt">> => 1,
        <<"state">> => <<"active">>,
        <<"lease_expires_at_unix">> => Now - 100,
        <<"provider">> => <<"doubao">>,
        <<"model">> => <<"volc.seedasr.auc">>,
        <<"policy_version">> => <<"p1">>,
        <<"config_version">> => hash(),
        <<"input_manifest_hash">> => hash(),
        <<"input_content_revision">> => 3,
        <<"input_transcript_version">> => null,
        <<"chunk_plan_hash">> => hash(),
        <<"consent_event_id">> => 31
    }.
part() ->
    #{
        <<"run_id">> => 30,
        <<"seq">> => 0,
        <<"offset_ms">> => 0,
        <<"duration_ms">> => 1000,
        <<"byte_size">> => 100,
        <<"provider_state">> => <<"dispatching">>,
        <<"dispatch_intent_id">> => 40,
        <<"request_hash">> => hash(),
        <<"source_part_manifest_hash">> => hash(),
        <<"source_ranges">> => [
            #{
                <<"part_id">> => 71,
                <<"sha256">> => hash(),
                <<"start_ms">> => 0,
                <<"end_ms">> => 1000
            }
        ]
    }.
assembly() ->
    #{
        <<"audio_attachment_id">> => 80,
        <<"manifest_hash">> => hash(),
        <<"total_duration_ms">> => 1000,
        <<"segments">> => [
            #{<<"part_id">> => 71, <<"duration_ms">> => 1000, <<"sha256">> => hash()}
        ]
    }.
artifact() ->
    #{
        <<"id">> => 80,
        <<"size">> => 100,
        <<"file_hash256">> => hash(),
        <<"mime_type">> => <<"audio/aac">>,
        <<"cipher">> => null,
        <<"lifecycle_state">> => <<"live">>,
        <<"write_state">> => <<"complete">>,
        <<"ref_state">> => <<"bound">>,
        <<"ref_role">> => <<"transcoded_audio">>,
        <<"reference_valid">> => true,
        <<"governance_purged">> => false
    }.
task(Now) ->
    #{
        <<"id">> => 50,
        <<"record_id">> => 7,
        <<"job_id">> => 20,
        <<"run_id">> => 30,
        <<"attempt">> => 1,
        <<"seq">> => 0,
        <<"actor_user_id">> => 10,
        <<"attachment_id">> => 80,
        <<"attachment_hash">> => hash(),
        <<"manifest_hash">> => hash(),
        <<"content_revision">> => 3,
        <<"consent_event_id">> => 31,
        <<"policy_version">> => <<"p1">>,
        <<"config_version">> => hash(),
        <<"model">> => <<"volc.seedasr.auc">>,
        <<"transcript_version">> => null,
        <<"provider">> => <<"doubao">>,
        <<"resource_id">> => <<"volc.seedasr.auc">>,
        <<"model_name">> => <<"bigmodel">>,
        <<"enable_speaker_info">> => true,
        <<"show_utterances">> => true,
        <<"ssd_version">> => <<"300">>,
        <<"enable_itn">> => true,
        <<"enable_punc">> => true,
        <<"enable_ddc">> => false,
        <<"intent_id">> => 40,
        <<"request_hash">> => hash(),
        <<"profile_hash">> => talk_record_client_contract:hash(
            talk_record_provider_contract:profile()
        ),
        <<"state">> => <<"accepted">>,
        <<"source_authorization_id">> => 60,
        <<"source_issued_at">> => Now - 5,
        <<"source_expires_at">> => Now + 120
    }.
