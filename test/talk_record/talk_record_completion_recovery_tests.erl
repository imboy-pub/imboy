-module(talk_record_completion_recovery_tests).
-include_lib("eunit/include/eunit.hrl").

fresh_terminal_completion_test_() ->
    [
        {binary_to_list(Status), fun() ->
            with_mocks(Status, fun() ->
                ?assertEqual({error, completion_already_submitted}, complete()),
                ?assertEqual(0, meck:num_calls(talk_record_job_worker, kick, '_')),
                ?assertEqual(0, meck:num_calls(elib_pg, execute, '_')),
                ?assertEqual(1, meck:num_calls(talk_record_idempotency_repo, record_tx, '_'))
            end)
        end}
     || Status <- [<<"assembling">>, <<"available">>, <<"retryable_failed">>]
    ].

same_key_replay_remains_authorized_test() ->
    with_mocks(<<"assembling">>, fun() ->
        {ok, Digest} = talk_record_logic:request_digest(
            <<"POST">>, <<"/api/v1/talk-records/1/complete">>, body()
        ),
        Snapshot = jsone:encode(#{<<"payload">> => #{<<"media_status">> => <<"assembling">>}}),
        meck:expect(talk_record_idempotency_repo, record_tx, fun(_, _, _, _, _, _, _) ->
            {ok, existing, #{<<"payload_hash">> => Digest, <<"result_ref">> => Snapshot}}
        end),
        ?assertMatch({ok, replay, _}, complete()),
        ?assertEqual(1, meck:num_calls(talk_record_repo, find_visible, [7, 1])),
        ?assertEqual(0, meck:num_calls(talk_record_repo, find_by_id_tx, '_'))
    end).

same_final_upload_dedupe_remains_valid_test() ->
    Final = maps:get(<<"final_audio">>, body()),
    with_existing_final_job(Final, fun() ->
        ?assertMatch({ok, existing, #{<<"job_id">> := 9}}, complete()),
        ?assertEqual(1, meck:num_calls(talk_record_job_repo, find_tx, [conn, 9])),
        ?assertEqual(1, meck:num_calls(talk_record_idempotency_repo, complete_tx, '_')),
        ?assertEqual(1, meck:num_calls(talk_record_job_worker, kick, '_'))
    end).

different_final_upload_identity_is_conflict_test_() ->
    Final = maps:get(<<"final_audio">>, body()),
    [
        {binary_to_list(Key), fun() ->
            with_existing_final_job(Final#{Key => Other}, fun() ->
                ?assertEqual({error, final_identity_conflict}, complete()),
                ?assertEqual(0, meck:num_calls(talk_record_idempotency_repo, complete_tx, '_')),
                ?assertEqual(0, meck:num_calls(talk_record_job_worker, kick, '_'))
            end)
        end}
     || {Key, Other} <- [
            {<<"object_key">>, <<"talk-record/final/other.aac">>},
            {<<"client_upload_id">>, <<"00000000-0000-0000-0000-000000000002">>}
        ]
    ].

revoked_actor_cannot_reconcile_or_replay_test() ->
    with_mocks(<<"assembling">>, fun() ->
        meck:expect(talk_record_repo, find_visible, fun(7, 1) -> {error, not_found} end),
        ?assertEqual({error, not_found}, complete()),
        ?assertEqual(0, meck:num_calls(talk_record_idempotency_repo, record_tx, '_'))
    end).

completion_hash_query_bound_to_same_record_capture_test() ->
    meck:new(elib_pg, [non_strict, no_link]),
    try
        meck:expect(elib_pg, query, fun(conn, Sql, [1]) ->
            ?assertNotEqual(
                nomatch, binary:match(Sql, <<"a.record_id=c.record_id AND a.capture_id=c.id">>)
            ),
            ?assertNotEqual(
                nomatch, binary:match(Sql, <<"a.manifest_hash AS completion_manifest_hash">>)
            ),
            {ok, []}
        end),
        ?assertEqual({ok, []}, talk_record_capture_repo:find_recent_by_record_tx(conn, 1))
    after
        meck:unload(elib_pg)
    end.

conflict_has_explicit_http_status_test() ->
    meck:new(elib_response, [non_strict, no_link]),
    try
        meck:expect(elib_response, error_with_status, fun(req, S, _, Code) -> {S, Code} end),
        ?assertEqual({409, 409}, talk_record_http:respond_error(req, completion_already_submitted))
    after
        meck:unload(elib_response)
    end.

body() ->
    #{
        <<"captures">> => [
            #{
                <<"capture_id">> => 2,
                <<"last_seq">> => 0,
                <<"manifest_hash">> => binary:copy(<<"a">>, 64)
            }
        ],
        <<"final_audio">> => #{
            <<"object_key">> => <<"talk-record/final/synthetic.aac">>,
            <<"client_upload_id">> => <<"00000000-0000-0000-0000-000000000001">>,
            %% f67fbe677 起 attachment_id 必填。
            <<"attachment_id">> => 190002,
            <<"sha256">> => binary:copy(<<"b">>, 64),
            <<"byte_size">> => 39,
            <<"sample_rate">> => 16000,
            <<"channels">> => 1,
            <<"sample_count">> => 1024,
            <<"duration_ms">> => 64
        },
        <<"processing_consent">> => #{
            <<"policy_version">> => <<"v2">>, <<"processor_config_version">> => <<"pcv2">>
        }
    }.
complete() -> talk_record_job_logic:complete(7, 1, <<"\"1\"">>, <<"new-key">>, body()).
with_mocks(Status, F) ->
    Mods = [elib_pg, talk_record_repo, talk_record_idempotency_repo, talk_record_job_worker],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Mods),
    try
        meck:expect(elib_pg, with_tx, fun(Fn, _) ->
            try
                Fn(conn)
            catch
                throw:R -> {rollback, R}
            end
        end),
        meck:expect(elib_pg, query, fun(conn, Sql, Args) ->
            case Args of
                [1] ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"FOR UPDATE">>)),
                    {ok, [#{<<"id">> => 1}]};
                [1, 2, 7] ->
                    {ok, [#{<<"id">> => 2}]}
            end
        end),
        meck:expect(talk_record_repo, find_visible, fun(7, 1) ->
            {ok, #{<<"owner_user_id">> => 7}}
        end),
        meck:expect(talk_record_repo, find_by_id_tx, fun(conn, 1, Cols) ->
            ?assertNotEqual(nomatch, binary:match(Cols, <<"tr.media_status">>)),
            {ok, #{
                <<"id">> => 1,
                <<"owner_user_id">> => 7,
                <<"deleted_at">> => null,
                <<"revision">> => 1,
                <<"media_status">> => Status,
                <<"processing_mode">> => <<"transcribe">>
            }}
        end),
        meck:expect(talk_record_idempotency_repo, record_tx, fun(_, _, _, _, _, _, _) ->
            {ok, inserted}
        end),
        meck:expect(talk_record_job_worker, kick, fun() -> ok end),
        F()
    after
        lists:foreach(fun meck:unload/1, Mods)
    end.

with_existing_final_job(PinnedFinal, F) ->
    with_mocks(<<"uploading">>, fun() ->
        Mods = [talk_record_assembly_logic, talk_record_job_repo],
        lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Mods),
        try
            Final = maps:get(<<"final_audio">>, body()),
            Key = maps:get(<<"object_key">>, Final),
            Cid = maps:get(<<"client_upload_id">>, Final),
            Hash = binary:copy(<<"a">>, 64),
            meck:expect(elib_pg, query, fun(conn, Sql, Params) ->
                case {Sql, Params} of
                    {<<"SELECT id FROM talk_record WHERE", _/binary>>, [1]} ->
                        {ok, [#{<<"id">> => 1}]};
                    {_, [1, 2, 7]} ->
                        {ok, [#{<<"id">> => 2}]};
                    {_, [1, 2]} ->
                        {ok, [#{<<"id">> => 2, <<"status">> => <<"ended">>}]};
                    {_, [Cid, 7, <<"1">>]} ->
                        {ok, [
                            #{
                                <<"attachment_id">> => 190002,
                                <<"file_hash256">> => maps:get(<<"sha256">>, Final),
                                <<"size">> => maps:get(<<"byte_size">>, Final),
                                <<"session_object_key">> => Key
                            }
                        ]};
                    {_, [Key]} ->
                        {ok, [#{<<"attempt_no">> => 1, <<"target_key">> => <<"physical/aac">>}]};
                    {<<"UPDATE talk_record_assembly SET", _/binary>>, [1, 9]} ->
                        {ok, 1};
                    {<<"UPDATE talk_record SET", _/binary>>, [1]} ->
                        {ok, 1}
                end
            end),
            meck:expect(talk_record_repo, find_by_id_tx, fun(conn, 1, _) ->
                {ok, #{
                    <<"owner_user_id">> => 7,
                    <<"deleted_at">> => null,
                    <<"revision">> => 1,
                    <<"content_revision">> => 1,
                    <<"media_status">> => <<"uploading">>,
                    <<"processing_mode">> => <<"record_only">>
                }}
            end),
            meck:expect(talk_record_assembly_logic, confirmed_parts_tx, fun(conn, 1) ->
                {ok, [part]}
            end),
            meck:expect(talk_record_assembly_logic, missing_sequences, fun([part], 0) -> [] end),
            meck:expect(talk_record_assembly_logic, build_manifest, fun(2, [part]) ->
                #{<<"manifest_hash">> => Hash}
            end),
            meck:expect(talk_record_assembly_logic, persist_tx, fun(conn, 1, 2, _) ->
                {ok, 3, created}
            end),
            meck:expect(talk_record_job_repo, enqueue_tx, fun(conn, Job) ->
                Frozen = maps:get(<<"final_audio">>, maps:get(processor_input, Job)),
                ?assertEqual(Key, maps:get(<<"object_key">>, Frozen)),
                ?assertEqual(Cid, maps:get(<<"client_upload_id">>, Frozen)),
                {ok, existing, #{<<"id">> => 9}}
            end),
            meck:expect(talk_record_job_repo, find_tx, fun(conn, 9) ->
                {ok, #{<<"processor_input">> => jsone:encode(#{<<"final_audio">> => PinnedFinal})}}
            end),
            meck:expect(talk_record_idempotency_repo, complete_tx, fun(_, _, _, _, _, _) -> ok end),
            F()
        after
            lists:foreach(fun meck:unload/1, Mods)
        end
    end).
