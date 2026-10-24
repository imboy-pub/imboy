-module(talk_record_job_abort_tests).
-include_lib("eunit/include/eunit.hrl").

exported_abort_test_() ->
    [?_test(assembly_paths()), ?_test(export_paths()), ?_test(model_paths())].

assembly_paths() ->
    with_mocks([talk_record_export_repo, talk_record_assembly_logic], fun() ->
        J = job(<<"assemble">>),
        R = #{<<"content_revision">> => 3},
        meck:expect(talk_record_export_repo, lock_record_tx, fun(_, _) -> {error, closed} end),
        ?assertThrow({talk_assembly_abort, closed}, talk_record_assembly_job:prepare_tx(c, J)),
        ?assertNot(meck:called(talk_record_assembly_logic, prepare_tx, '_')),
        meck:expect(talk_record_export_repo, lock_record_tx, fun(_, _) -> {ok, R} end),
        meck:expect(talk_record_export_repo, lock_job_tx, fun(_, _) -> {error, not_found} end),
        ?assertThrow({talk_assembly_abort, lease_lost}, talk_record_assembly_job:prepare_tx(c, J)),
        meck:expect(talk_record_export_repo, lock_job_tx, fun(_, _) -> {ok, J} end),
        meck:expect(talk_record_assembly_logic, find_tx, fun(_, _) ->
            {ok, #{<<"manifest_hash">> => <<"h">>}}
        end),
        meck:expect(talk_record_assembly_logic, prepare_tx, fun(_, _) ->
            {ok, #{parts => [part]}}
        end),
        ?assertEqual({ok, #{parts => [part]}}, talk_record_assembly_job:prepare_tx(c, J))
    end).

export_paths() ->
    with_mocks(
        [talk_record_export_repo, talk_record_export_logic, talk_record_export_artifact], fun() ->
            J = job(<<"export">>),
            Ex = #{<<"requester_id">> => 8},
            meck:expect(talk_record_export_repo, lock_record_tx, fun(_, _) -> {ok, #{}} end),
            meck:expect(talk_record_export_repo, lock_job_tx, fun(_, _) -> {error, sock_closed} end),
            ?assertThrow({talk_export_abort, sock_closed}, talk_record_export_job:prepare_tx(c, J)),
            ?assertNot(meck:called(talk_record_export_repo, find_by_job_tx, '_')),
            meck:expect(talk_record_export_repo, lock_job_tx, fun(_, _) -> {error, not_found} end),
            ?assertThrow({talk_export_abort, lease_lost}, talk_record_export_job:prepare_tx(c, J)),
            meck:expect(talk_record_export_repo, lock_job_tx, fun(_, _) -> {ok, J} end),
            meck:expect(talk_record_export_repo, find_by_job_tx, fun(_, _) -> {ok, Ex} end),
            meck:expect(talk_record_export_logic, authorize_tx, fun(_, _, _) ->
                {ok, #{<<"content_revision">> => 3}}
            end),
            meck:expect(talk_record_export_logic, snapshot_tx, fun(_, _) -> {ok, generated} end),
            meck:expect(talk_record_export_artifact, prepare_tx, fun(_, _, _, _) ->
                {ok, artifact}
            end),
            ?assertEqual(
                {ok, #{export => Ex, generated => generated, artifact => artifact}},
                talk_record_export_job:prepare_tx(c, J)
            )
        end
    ).

model_paths() ->
    with_mocks(
        [
            talk_record_export_repo,
            talk_record_processor_run_repo,
            talk_record_assembly_logic,
            talk_record_transcript_repo
        ],
        fun() ->
            J = job(<<"transcribe">>),
            meck:expect(talk_record_export_repo, lock_record_tx, fun(_, _) -> {error, not_found} end),
            ?assertThrow(
                {talk_model_abort, record_deleted}, talk_record_model_job:prepare_tx(c, J, #{})
            ),
            meck:expect(talk_record_export_repo, lock_record_tx, fun(_, _) -> {ok, #{}} end),
            meck:expect(talk_record_export_repo, lock_job_tx, fun(_, _) -> {error, not_found} end),
            ?assertThrow(
                {talk_model_abort, lease_lost}, talk_record_model_job:prepare_tx(c, J, #{})
            ),
            ?assertNot(meck:called(talk_record_processor_run_repo, reserve_tx, '_')),
            S = job(<<"summarize">>),
            meck:expect(talk_record_export_repo, lock_record_tx, fun(_, _) ->
                {ok, #{<<"media_status">> => <<"available">>, <<"content_revision">> => 3}}
            end),
            meck:expect(talk_record_export_repo, lock_job_tx, fun(_, _) -> {ok, S} end),
            meck:expect(talk_record_assembly_logic, find_tx, fun(_, _) ->
                {ok, #{<<"manifest_hash">> => <<"h">>}}
            end),
            meck:expect(talk_record_transcript_repo, find_current_tx, fun(_, _, _) ->
                {error, sock_error}
            end),
            ?assertThrow(
                {talk_model_abort, sock_error}, talk_record_model_job:prepare_tx(c, S, #{})
            ),
            ?assertNot(meck:called(talk_record_processor_run_repo, reserve_tx, '_')),
            ?assertEqual(
                ok,
                talk_record_model_job:release_confirmed(J, #{}, {error, processor_stop_unconfirmed})
            )
        end
    ).

job(Kind) ->
    #{
        <<"record_id">> => 1,
        <<"id">> => 2,
        <<"kind">> => Kind,
        <<"input_content_revision">> => 3,
        <<"input_manifest_hash">> => <<"h">>,
        <<"lease_token">> => <<"l">>
    }.
with_mocks(Modules, F) ->
    [meck:new(M, [non_strict, no_link]) || M <- Modules],
    try
        F()
    after
        [meck:unload(M) || M <- Modules]
    end.

%% Public transaction entry points must reject malformed successful consent reads.
model_consent_shape_test_() ->
    [
        ?_test(consent_shape(Entry, Case))
     || Entry <- [prepare, publish],
        Case <- [empty, missing_actor, multiple, scalar, database_error, owner, editor, revoked]
    ].

consent_shape(Entry, Case) ->
    Modules = [
        talk_record_export_repo,
        talk_record_assembly_logic,
        talk_record_processor_config,
        talk_record_repo,
        elib_pg,
        talk_record_processor_run_repo,
        talk_record_attachment_adapter,
        talk_record_model_result_repo,
        talk_record_job_repo
    ],
    with_mocks(Modules, fun() ->
        Config = #{config_version => <<"v1">>, model_version => <<"model">>},
        Pin = #{
            <<"processor_config_version">> => <<"v1">>,
            <<"processor_config_digest">> => <<"digest">>,
            <<"consent_event_id">> => 9,
            <<"requested_mode">> => <<"transcribe">>
        },
        J = (job(<<"transcribe">>))#{<<"attempt">> => 1, <<"processor_input">> => Pin},
        Record = #{
            <<"id">> => 1,
            <<"media_status">> => <<"available">>,
            <<"content_revision">> => 3,
            <<"deleted_at">> => null,
            <<"current_transcript_id">> => null
        },
        A = #{
            <<"manifest_hash">> => <<"h">>,
            <<"audio_attachment_id">> => 44,
            <<"total_duration_ms">> => 1000
        },
        meck:expect(talk_record_export_repo, lock_record_tx, fun(_, _) -> {ok, Record} end),
        meck:expect(talk_record_export_repo, lock_job_tx, fun(_, _) -> {ok, J} end),
        meck:expect(talk_record_assembly_logic, find_tx, fun(_, _) -> {ok, A} end),
        meck:expect(talk_record_processor_config, fingerprint, fun() -> <<"digest">> end),
        meck:expect(talk_record_processor_config, get, fun(_) -> {ok, Config} end),
        meck:expect(talk_record_repo, find_visible_tx, fun(_, 8, 1) ->
            case Case of
                owner -> {ok, #{<<"owner_user_id">> => 8}};
                editor -> {ok, #{<<"owner_user_id">> => 99}};
                _ -> {error, not_found}
            end
        end),
        Payload =
            case Case of
                empty -> {ok, []};
                missing_actor -> {ok, [#{}]};
                multiple -> {ok, [#{<<"actor_id">> => 8}, #{<<"actor_id">> => 9}]};
                scalar -> {ok, 17};
                database_error -> {error, closed};
                _ -> {ok, [#{<<"actor_id">> => 8}]}
            end,
        meck:expect(elib_pg, query, fun(_, Sql, _) ->
            case binary:match(Sql, <<"talk_record_consent_event">>) of
                nomatch -> {ok, [#{<<"id">> => 7}]};
                _ -> Payload
            end
        end),
        meck:expect(elib_pg, execute, fun(_, _, _) -> {ok, 1} end),
        meck:expect(talk_record_processor_run_repo, reserve_tx, fun(_, _) -> ok end),
        meck:expect(
            talk_record_attachment_adapter,
            grant_processing_lease_tx,
            fun(_, _, _, _, _, _) -> {ok, 1, true} end
        ),
        meck:expect(
            talk_record_model_result_repo,
            publish_transcript_tx,
            fun(_, _, _, _) -> {ok, 1, 4} end
        ),
        meck:expect(talk_record_job_repo, finish_success_tx, fun(_, _, _) -> ok end),
        Call = fun() ->
            case Entry of
                prepare ->
                    talk_record_model_job:prepare_tx(c, J, Config);
                publish ->
                    talk_record_model_job:publish_tx(c, J, Config, #{status => <<"no_speech">>})
            end
        end,
        case Case of
            owner -> ?assertMatch({ok, _}, Call());
            editor -> ?assertMatch({ok, _}, Call());
            database_error -> ?assertThrow({talk_model_abort, closed}, Call());
            _ -> ?assertThrow({talk_model_abort, processing_consent_required}, Call())
        end,
        case lists:member(Case, [owner, editor]) of
            true ->
                ?assert(meck:called(talk_record_repo, find_visible_tx, [c, 8, 1]));
            false ->
                ?assertNot(meck:called(talk_record_processor_run_repo, reserve_tx, '_')),
                ?assertNot(meck:called(talk_record_model_result_repo, publish_transcript_tx, '_')),
                ?assertNot(meck:called(elib_pg, execute, '_')),
                ?assertNot(
                    meck:called(talk_record_attachment_adapter, grant_processing_lease_tx, '_')
                )
        end
    end).
