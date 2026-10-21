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
        <<"captures">> => [#{<<"capture_id">> => 2, <<"last_seq">> => 0}],
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
