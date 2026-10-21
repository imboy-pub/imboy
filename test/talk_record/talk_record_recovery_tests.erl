-module(talk_record_recovery_tests).
-include_lib("eunit/include/eunit.hrl").

lookup_projection_test() ->
    mock(fun() ->
        {ok, P} = talk_record_recovery_ds:lookup(7, context()),
        ?assertEqual(10, maps:get(<<"id">>, P)),
        ?assertNot(maps:is_key(<<"title">>, P)),
        ?assertNot(maps:is_key(<<"notes">>, P)),
        ?assertNot(maps:is_key(<<"policy_version">>, P))
    end).

exact_replay_rechecks_current_acl_test() ->
    mock(fun() ->
        ?assertMatch(
            {ok, replay, _, _},
            talk_record_recovery_ds:replay(7, context(), <<"key">>, <<"digest">>)
        ),
        meck:expect(talk_record_repo, find_recovery_tx, fun(conn, 7, _) -> {error, not_found} end),
        ?assertEqual(
            {error, not_found},
            talk_record_recovery_ds:replay(7, context(), <<"key">>, <<"digest">>)
        )
    end).

changed_payload_not_replayed_test() ->
    mock(fun() ->
        ?assertEqual(
            miss, talk_record_recovery_ds:replay(7, context(), <<"key">>, <<"different">>)
        ),
        ?assertEqual(0, meck:num_calls(talk_record_repo, find_recovery_tx, '_'))
    end).

lookup_none_is_not_permission_to_create_test() ->
    mock(fun() ->
        meck:expect(talk_record_repo, find_recovery_tx, fun(conn, 7, _) -> {error, not_found} end),
        ?assertEqual({error, not_found}, talk_record_recovery_ds:lookup(7, context())),
        ?assertEqual(0, meck:num_calls(talk_record_repo, insert_tx, '_'))
    end).

wrong_snapshot_id_rejected_test() ->
    mock(fun() ->
        meck:expect(talk_record_repo, find_recovery_tx, fun(conn, 7, _) -> {ok, row(11)} end),
        ?assertEqual(
            {error, not_found},
            talk_record_recovery_ds:replay(7, context(), <<"key">>, <<"digest">>)
        )
    end).

business_replay_identity_guard_test() ->
    mock(fun() ->
        ?assertMatch({ok, _}, talk_record_recovery_ds:authorize_tx(conn, 7, context(), 10)),
        ?assertEqual(
            {error, not_found}, talk_record_recovery_ds:authorize_tx(conn, 7, context(), 11)
        )
    end).

notice_is_real_current_submission_event_test() ->
    mock(fun() ->
        meck:expect(elib_tsid, generate, fun(talk_record) -> 123 end),
        meck:expect(elib_pg, execute, fun(conn, Sql, [123, 10, 7, <<"v2">>, <<"record_only">>]) ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"recording_notice_ack">>)),
            {ok, 1}
        end),
        ?assertEqual(
            ok,
            talk_record_recovery_ds:notice_tx(
                conn,
                7,
                10,
                #{policy_version => <<"v2">>, processing_mode => <<"record_only">>}
            )
        )
    end).

notice_failure_rolls_back_test() ->
    mock(fun() ->
        meck:expect(elib_tsid, generate, fun(_) -> 123 end),
        meck:expect(elib_pg, execute, fun(_, _, _) -> {error, failed} end),
        ?assertThrow(
            {talk_record_rollback, {error, db_error}},
            talk_record_recovery_ds:notice_tx(
                conn,
                7,
                10,
                #{policy_version => <<"v2">>, processing_mode => <<"record_only">>}
            )
        )
    end).

old_policy_exact_create_replay_precedes_current_policy_test() ->
    mock(fun() ->
        Body = #{
            <<"client_recording_id">> => maps:get(client_recording_id, context()),
            <<"scope">> => <<"personal">>,
            <<"notice_ack">> => true,
            <<"policy_version">> => <<"old-v1">>
        },
        {ok, Digest} = talk_record_logic:request_digest(Body),
        Snap = jsone:encode(#{<<"payload">> => row(10), <<"etag">> => <<"etag">>}),
        meck:expect(talk_record_idempotency_repo, find, fun(_, _, _, _) ->
            {ok, #{<<"payload_hash">> => Digest, <<"result_ref">> => Snap}}
        end),
        ?assertMatch({ok, replay, _, _}, talk_record_logic:create(7, Body, <<"key">>)),
        ?assertEqual(0, meck:num_calls(talk_record_repo, insert_tx, '_')),
        ?assertEqual(0, meck:num_calls(elib_pg, execute, '_'))
    end).

new_create_records_notice_exactly_once_test() ->
    creation_mock(fun() ->
        ?assertMatch(
            {ok, created, _, _}, talk_record_ds:create(7, valid(), <<"key">>, <<"digest">>)
        ),
        ?assertEqual(1, meck:num_calls(elib_pg, execute, '_')),
        ?assertEqual(1, meck:num_calls(talk_record_idempotency_repo, complete_tx, '_'))
    end).

new_notice_failure_rolls_back_create_before_snapshot_test() ->
    creation_mock(fun() ->
        meck:expect(elib_pg, execute, fun(_, _, _) -> {error, failed} end),
        ?assertEqual({error, db_error}, talk_record_ds:create(7, valid(), <<"key">>, <<"digest">>)),
        ?assertEqual(0, meck:num_calls(talk_record_idempotency_repo, complete_tx, '_'))
    end).

business_uuid_context_mismatch_rolls_back_new_key_test() ->
    creation_mock(fun() ->
        meck:expect(talk_record_repo, insert_tx, fun(conn, _) -> {error, unique_violation} end),
        meck:expect(talk_record_repo, find_by_creator_client_tx, fun(conn, 7, _) ->
            {ok, row(10)}
        end),
        meck:expect(talk_record_repo, find_recovery_tx, fun(conn, 7, _) -> {error, not_found} end),
        ?assertEqual(
            {error, not_found}, talk_record_ds:create(7, valid(), <<"key">>, <<"digest">>)
        ),
        ?assertEqual(0, meck:num_calls(talk_record_idempotency_repo, complete_tx, '_')),
        ?assertEqual(0, meck:num_calls(elib_pg, execute, '_'))
    end).

existing_create_does_not_fabricate_notice_test() ->
    creation_mock(fun() ->
        Snap = jsone:encode(#{<<"payload">> => row(10), <<"etag">> => <<"etag">>}),
        meck:expect(talk_record_idempotency_repo, record_tx, fun(_, _, _, _, _, _, _) ->
            {ok, existing, #{<<"payload_hash">> => <<"digest">>, <<"result_ref">> => Snap}}
        end),
        ?assertMatch(
            {ok, replay, _, _}, talk_record_ds:create(7, valid(), <<"key">>, <<"digest">>)
        ),
        ?assertEqual(0, meck:num_calls(elib_pg, execute, '_'))
    end).

valid() ->
    (context())#{
        policy_version => <<"v2">>,
        processing_mode => <<"record_only">>,
        title => <<>>,
        language => <<>>
    }.
creation_mock(F) ->
    Mods = [
        elib_pg,
        talk_record_repo,
        talk_record_idempotency_repo,
        elib_tsid,
        talk_record_context_ds,
        talk_record_view_ds
    ],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Mods),
    try
        meck:expect(elib_pg, with_tx, fun(Fn, _) ->
            try
                Fn(conn)
            catch
                throw:R -> {rollback, R}
            end
        end),
        meck:expect(elib_pg, execute, fun(conn, _, [123, 10, 7, <<"v2">>, <<"record_only">>]) ->
            {ok, 1}
        end),
        meck:expect(elib_tsid, generate, fun(_) -> 123 end),
        meck:expect(talk_record_context_ds, validate_context_tx, fun(conn, 7, _) -> ok end),
        meck:expect(talk_record_view_ds, idempotency_ttl_seconds, fun() -> 604800 end),
        meck:expect(talk_record_view_ds, capabilities_for_actor, fun(_, _) -> #{} end),
        meck:expect(talk_record_view_ds, etag, fun(_) -> <<"etag">> end),
        meck:expect(talk_record_view_ds, detail_payload, fun(_, R, _) -> R end),
        meck:expect(talk_record_repo, insert_tx, fun(conn, _) -> {ok, 10} end),
        meck:expect(talk_record_repo, find_by_id_tx, fun(conn, 10, _) -> {ok, row(10)} end),
        meck:expect(talk_record_repo, detail_columns, fun() -> <<"id">> end),
        meck:expect(talk_record_repo, find_recovery_tx, fun(conn, 7, _) -> {ok, row(10)} end),
        meck:expect(talk_record_idempotency_repo, record_tx, fun(_, _, _, _, _, _, _) ->
            {ok, inserted}
        end),
        meck:expect(talk_record_idempotency_repo, complete_tx, fun(_, _, _, _, _, _) -> ok end),
        F()
    after
        lists:foreach(fun meck:unload/1, Mods)
    end.

context_matrix_test() ->
    B = #{
        <<"client_recording_id">> => maps:get(client_recording_id, context()),
        <<"scope">> => <<"personal">>
    },
    ?assertMatch({ok, #{scope := <<"personal">>}}, talk_record_recovery_logic:context(B)),
    ?assertEqual(
        {error, personal_with_org},
        talk_record_recovery_logic:context(B#{<<"organization_id">> => 0})
    ),
    ?assertEqual(
        {error, invalid_client_recording_id},
        talk_record_recovery_logic:context(B#{<<"client_recording_id">> => <<"bad">>})
    ),
    Org = B#{
        <<"scope">> => <<"organization">>,
        <<"organization_id">> => <<"10">>,
        <<"workspace_id">> => <<"20">>
    },
    ?assertMatch(
        {ok, #{organization_id := 10, workspace_id := 20}}, talk_record_recovery_logic:context(Org)
    ),
    ?assertEqual(
        {error, invalid_organization_id},
        talk_record_recovery_logic:context(maps:remove(<<"organization_id">>, Org))
    ),
    ?assertEqual({error, not_found}, talk_record_recovery_logic:lookup(0, B)).

strict_repo_parameters_test() ->
    meck:new(elib_pg, [non_strict, no_link]),
    try
        meck:expect(elib_pg, query, fun(conn, Sql, [7, Uuid, <<"organization">>, 10, 20]) ->
            ?assertEqual(maps:get(client_recording_id, context()), Uuid),
            lists:foreach(
                fun(Needle) -> ?assertNotEqual(nomatch, binary:match(Sql, Needle)) end,
                [
                    <<"tr.created_by=$1">>,
                    <<"tr.owner_user_id=$1">>,
                    <<"tr.deleted_at IS NULL">>,
                    <<"organization_id IS NOT DISTINCT FROM $4">>,
                    <<"workspace_id IS NOT DISTINCT FROM $5">>
                ]
            ),
            {ok, []}
        end),
        ?assertEqual(
            {error, not_found},
            talk_record_repo:find_recovery_tx(
                conn,
                7,
                (context())#{scope => <<"organization">>, organization_id => 10, workspace_id => 20}
            )
        )
    after
        meck:unload(elib_pg)
    end.

context() ->
    #{client_recording_id => <<"00000000-0000-0000-0000-000000000001">>, scope => <<"personal">>}.
row(Id) ->
    #{
        <<"id">> => Id,
        <<"owner_user_id">> => 7,
        <<"client_recording_id">> => maps:get(client_recording_id, context()),
        <<"scope">> => <<"personal">>,
        <<"revision">> => 1,
        <<"title">> => <<"private">>,
        <<"notes">> => <<"secret">>
    }.
mock(F) ->
    Mods = [elib_pg, talk_record_repo, talk_record_idempotency_repo, elib_tsid],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Mods),
    try
        meck:expect(elib_pg, with_tx, fun(Fn, _) -> Fn(conn) end),
        meck:expect(talk_record_repo, find_recovery_tx, fun(conn, 7, _) -> {ok, row(10)} end),
        Snap = jsone:encode(#{<<"payload">> => row(10), <<"etag">> => <<"etag">>}),
        meck:expect(talk_record_idempotency_repo, find, fun(
            7, <<"POST">>, <<"/api/v1/talk-records">>, <<"key">>
        ) ->
            {ok, #{<<"payload_hash">> => <<"digest">>, <<"result_ref">> => Snap}}
        end),
        F()
    after
        lists:foreach(fun meck:unload/1, Mods)
    end.
