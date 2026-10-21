-module(enterprise_directory_member_pg_checks).
-export([exercise/2]).
-include_lib("eunit/include/eunit.hrl").

%% Own savepoints in the fully migrated disposable fixture; no shared DB writes.
exercise(C, Ctx0) ->
    {ok, _, _} = epgsql:squery(C, "BEGIN"),
    try
        App = maps:get(application_id, Ctx0),
        Ctx = Ctx0#{correlation_id => <<"synthetic-member-lifecycle">>},
        pending(C, Ctx),
        read_only(C, Ctx),
        lifecycle(C, Ctx),
        authority(C, Ctx),
        removal(C, Ctx),
        migration_guard(C)
    after
        {ok, _, _} = epgsql:squery(C, "ROLLBACK")
    end.

pending(C, Ctx) ->
    App = maps:get(application_id, Ctx),
    {ok, _} = enterprise_source_person_repo:sync_tx(
        C, App, <<"unclaimed-member">>, <<"Synthetic pending">>, <<"active">>, 1
    ),
    exec(
        C,
        <<"INSERT INTO enterprise_directory_membership_source(application_id,organization_id,external_user_id,department_external_ids,source_version,payload_digest,status) VALUES($1,977101,'unclaimed-member','[]',1,repeat('c',64),'active')">>,
        [App]
    ),
    {ok, R} = enterprise_directory_member_logic:read_tx(C, Ctx, (query())#{
        <<"external_user_id">> => <<"unclaimed-member">>
    }),
    ?assertEqual(null, maps:get(<<"member_version">>, R)),
    ?assertEqual(6, map_size(maps:get(<<"blocker_counts">>, R))),
    ?assertEqual([0], lists:usort(maps:values(maps:get(<<"blocker_counts">>, R)))),
    ?assertEqual(false, maps:get(<<"can_remove">>, R)).

read_only(C, Ctx) ->
    Before = snapshot(C),
    V = view(C, Ctx),
    ?assertEqual(true, maps:get(<<"can_suspend">>, V)),
    ?assertEqual(6, map_size(maps:get(<<"blocker_counts">>, V))),
    ?assertEqual(Before, snapshot(C)),
    error_unchanged(
        C,
        fun() ->
            enterprise_directory_member_logic:read_tx(C, Ctx, (query())#{
                <<"external_organization_id">> => <<"foreign">>
            })
        end,
        <<"resource_not_found">>
    ).

lifecycle(C, Ctx) ->
    V = maps:get(<<"member_version">>, view(C, Ctx)),
    PauseBody = body(<<"suspended">>, V),
    Pause = write(C, Ctx, <<"pause">>, PauseBody),
    ?assertMatch({ok, 200, _, []}, Pause),
    {ok, 200, Bytes, []} = Pause,
    ?assertEqual(<<"suspended">>, maps:get(<<"member_status">>, jsone:decode(Bytes))),
    Before = snapshot(C),
    ?assertMatch({ok, 200, Bytes, [_]}, write(C, Ctx, <<"pause">>, PauseBody)),
    ?assertEqual(Before, snapshot(C)),
    error_unchanged(
        C, fun() -> write(C, Ctx, <<"stale">>, body(<<"active">>, V)) end, <<"version_conflict">>
    ),
    Paused = view(C, Ctx),
    ?assertEqual(true, maps:get(<<"can_restore">>, Paused)),
    ?assertMatch(
        {ok, 200, _, []},
        write(C, Ctx, <<"restore">>, body(<<"active">>, maps:get(<<"member_version">>, Paused)))
    ),
    manual_pause(C, Ctx),
    audit_rollback(C, Ctx).

manual_pause(C, Ctx) ->
    {ok, _, _} = epgsql:squery(C, "SAVEPOINT manual"),
    try
        exec(
            C,
            <<"UPDATE organization_member SET status='suspended',updated_at=clock_timestamp() WHERE organization_id=977101 AND user_id=977002">>,
            []
        ),
        V = view(C, Ctx),
        ?assertEqual(false, maps:get(<<"can_restore">>, V)),
        error_unchanged(
            C,
            fun() ->
                write(
                    C,
                    Ctx,
                    <<"manual-restore">>,
                    body(<<"active">>, maps:get(<<"member_version">>, V))
                )
            end,
            <<"resource_conflict">>
        )
    after
        {ok, _, _} = epgsql:squery(C, "ROLLBACK TO SAVEPOINT manual")
    end.

audit_rollback(C, Ctx) ->
    {ok, _, _} = epgsql:squery(C, "SAVEPOINT reject_audit"),
    try
        exec(
            C,
            <<"ALTER TABLE enterprise_audit_event ADD CONSTRAINT member_audit_reject CHECK(false) NOT VALID">>,
            []
        ),
        V = maps:get(<<"member_version">>, view(C, Ctx)),
        error_unchanged(
            C,
            fun() -> write(C, Ctx, <<"audit-reject">>, body(<<"suspended">>, V)) end,
            <<"internal_error">>
        )
    after
        {ok, _, _} = epgsql:squery(C, "ROLLBACK TO SAVEPOINT reject_audit")
    end.

authority(C, Ctx) ->
    {ok, _, _} = epgsql:squery(C, "SAVEPOINT auth"),
    try
        %% Live credential revocation also rejects an idempotent replay.
        V = maps:get(<<"member_version">>, view(C, Ctx)),
        ?assertMatch(
            {ok, 200, _, []}, write(C, Ctx, <<"independent-write">>, body(<<"suspended">>, V))
        ),
        exec(
            C,
            <<"UPDATE enterprise_application_credential SET status='revoked',revoked_at=clock_timestamp() WHERE id=$1">>,
            [maps:get(credential_id, Ctx)]
        ),
        error_unchanged(
            C,
            fun() -> write(C, Ctx, <<"independent-write">>, body(<<"suspended">>, V)) end,
            <<"invalid_credential">>
        )
    after
        {ok, _, _} = epgsql:squery(C, "ROLLBACK TO SAVEPOINT auth")
    end.

removal(C, Ctx) ->
    V = maps:get(<<"member_version">>, view(C, Ctx)),
    ?assertMatch({ok, 200, _, []}, write(C, Ctx, <<"departure-pause">>, body(<<"suspended">>, V))),
    Person = #{
        <<"external_user_id">> => <<"pending">>,
        <<"display_name">> => <<"Pending">>,
        <<"source_status">> => <<"active">>,
        <<"source_version">> => 1,
        <<"membership">> => #{
            <<"version">> => 2, <<"status">> => <<"removed">>, <<"department_external_ids">> => []
        }
    },
    B = #{
        <<"organization">> => #{
            <<"external_id">> => <<"fixture">>, <<"name">> => <<"Synced">>, <<"version">> => 2
        },
        <<"departments">> => [],
        <<"persons">> => [Person]
    },
    blocked_departure(C, Ctx, B),
    ?assertMatch(
        {ok, applied, _}, enterprise_directory_sync_logic:sync_tx(C, Ctx, <<"paused-departure">>, B)
    ),
    {ok, Row} = enterprise_directory_member_repo:target_tx(
        C, maps:get(application_id, Ctx), 977101, <<"pending">>
    ),
    ?assertEqual(<<"removed">>, maps:get(<<"member_status">>, Row)),
    ?assertEqual(null, maps:get(<<"suspension_member_version">>, Row)),
    ?assertEqual(<<"active">>, maps:get(<<"source_status">>, Row)).

blocked_departure(C, Ctx, Body) ->
    {ok, _, _} = epgsql:squery(C, "SAVEPOINT assignment"),
    try
        exec(
            C,
            <<"INSERT INTO organization_business_identity(id,organization_id,function_key,display_name) VALUES(977800,977101,'sales','synthetic-offboarding')">>,
            []
        ),
        exec(
            C,
            <<"INSERT INTO organization_business_identity_assignment(id,organization_id,business_identity_id,function_key,user_id) VALUES(977801,977101,977800,'sales',977002)">>,
            []
        ),
        R = view(C, Ctx),
        ?assertEqual(1, maps:get(<<"active_assignments">>, maps:get(<<"blocker_counts">>, R))),
        ?assertEqual(false, maps:get(<<"can_remove">>, R)),
        Before = snapshot(C),
        {ok, _, _} = epgsql:squery(C, "SAVEPOINT blocked_departure"),
        Result =
            try
                enterprise_directory_sync_logic:sync_tx(C, Ctx, <<"blocked-departure">>, Body)
            catch
                throw:{abort_tx, Reason} -> {error, Reason}
            end,
        {ok, _, _} = epgsql:squery(C, "ROLLBACK TO SAVEPOINT blocked_departure"),
        ?assertMatch({error, {409, _}}, Result),
        ?assertEqual(Before, snapshot(C)),
        ?assertEqual(<<"suspended">>, maps:get(<<"member_status">>, view(C, Ctx)))
    after
        {ok, _, _} = epgsql:squery(C, "ROLLBACK TO SAVEPOINT assignment")
    end.

migration_guard(C) ->
    {ok, Sql} = file:read_file("priv/migrations/00000208_directory_member_suspension.down.sql"),
    {ok, _, _} = epgsql:squery(C, "SAVEPOINT migration"),
    try
        ?assertMatch({error, _}, epgsql:squery(C, Sql))
    after
        {ok, _, _} = epgsql:squery(C, "ROLLBACK TO SAVEPOINT migration")
    end.

error_unchanged(C, F, Code) ->
    Before = snapshot(C),
    {ok, _, _} = epgsql:squery(C, "SAVEPOINT expected_error"),
    Result =
        try
            F()
        catch
            throw:{rollback, {internal_error_code, E}} -> {error, E}
        end,
    {ok, _, _} = epgsql:squery(C, "ROLLBACK TO SAVEPOINT expected_error"),
    ?assertEqual({error, Code}, Result),
    ?assertEqual(Before, snapshot(C)).
query() ->
    #{<<"external_organization_id">> => <<"fixture">>, <<"external_user_id">> => <<"pending">>}.
body(Status, V) -> (query())#{<<"status">> => Status, <<"expected_member_version">> => V}.
write(C, Ctx, K, B) ->
    {ok, Digest} = enterprise_internal_idempotency:request_digest(
        <<"PATCH">>, <<"/api/internal/v1/directory/memberships">>, B
    ),
    enterprise_directory_member_logic:write_tx(C, Ctx, update, K, Digest, 0, B).
view(C, Ctx) ->
    {ok, V} = enterprise_directory_member_logic:read_tx(C, Ctx, query()),
    V.
snapshot(C) -> enterprise_directory_sync_pg_tests:all_rows_snapshot(C).
exec(C, Q, P) ->
    {ok, _} = elib_pg:execute(C, Q, P),
    ok.
