-module(enterprise_directory_source_read_pg_checks).
-export([exercise/2]).
-include_lib("eunit/include/eunit.hrl").

%% Runs against the fully migrated disposable directory fixture, and rolls back its own changes.
exercise(Conn, Ctx) ->
    {ok, _, _} = epgsql:squery(Conn, "BEGIN"),
    try
        setup(Conn, Ctx),
        query_checks(Conn, Ctx),
        actual_projection(Conn, Ctx),
        revocations(Conn, Ctx)
    after
        {ok, _, _} = epgsql:squery(Conn, "ROLLBACK")
    end.

setup(Conn, Ctx) ->
    App = maps:get(application_id, Ctx),
    {ok, _} = enterprise_source_person_repo:sync_tx(
        Conn,
        App,
        <<"read-unlinked">>,
        <<"Synthetic unlinked">>,
        <<"active">>,
        1
    ),
    exec(
        Conn,
        <<
            "INSERT INTO enterprise_directory_membership_source"
            " (application_id,organization_id,external_user_id,department_external_ids,source_version,payload_digest,status)"
            " VALUES ($1,977101,'read-unlinked','[]',1,repeat('a',64),'active')"
        >>,
        [App]
    ),
    application:set_env(
        imboy,
        enterprise_internal_cursor_signing_key,
        <<"directory_read_fixture_key_0123456789">>
    ).

query_checks(Conn, Ctx) ->
    Before = enterprise_directory_sync_pg_tests:all_rows_snapshot(Conn),
    Query = #{<<"external_organization_id">> => <<"fixture">>, <<"page_size">> => <<"1">>},
    {ok, Orgs} = page(Conn, Ctx, organizations, #{}),
    ?assert(
        lists:any(
            fun(#{<<"organization_id">> := Id}) -> Id =:= 977101 end,
            maps:get(<<"items">>, Orgs)
        )
    ),
    {ok, Departments} = page(Conn, Ctx, departments, Query),
    ?assertMatch(
        [
            #{
                <<"external_department_id">> := <<"claim-dept">>,
                <<"parent_external_id">> := null
            }
        ],
        maps:get(<<"items">>, Departments)
    ),
    {ok, First} = page(Conn, Ctx, persons, Query),
    ?assertEqual(true, maps:get(<<"has_more">>, First)),
    ?assertMatch(
        [
            #{
                <<"external_user_id">> := <<"pending">>,
                <<"projection_status">> := <<"projected">>
            }
        ],
        maps:get(<<"items">>, First)
    ),
    Cursor = maps:get(<<"next_cursor">>, First),
    {ok, Second} = page(Conn, Ctx, persons, Query#{<<"cursor">> => Cursor}),
    ?assertMatch(
        [
            #{
                <<"external_user_id">> := <<"read-unlinked">>,
                <<"user_id">> := null,
                <<"projection_status">> := <<"unlinked">>
            }
        ],
        maps:get(<<"items">>, Second)
    ),
    ?assertEqual(false, maps:get(<<"has_more">>, Second)),
    ?assertEqual(
        {error, resource_not_found}, page(Conn, Ctx#{application_id => 1}, persons, Query)
    ),
    ?assertEqual(
        {error, resource_not_found},
        page(
            Conn,
            Ctx,
            persons,
            Query#{<<"external_organization_id">> => <<"foreign-source">>}
        )
    ),
    ?assertEqual(Before, enterprise_directory_sync_pg_tests:all_rows_snapshot(Conn)).

actual_projection(Conn, Ctx) ->
    Q = #{<<"external_organization_id">> => <<"fixture">>, <<"external_user_id">> => <<"pending">>},
    exec(Conn, <<"UPDATE \"user\" SET status=0 WHERE id=977002">>, []),
    {ok, Disabled} = page(Conn, Ctx, persons, Q),
    ?assertMatch(
        [
            #{
                <<"binding_status">> := <<"linked">>,
                <<"user_id">> := null,
                <<"projection_status">> := <<"pending">>
            }
        ],
        maps:get(<<"items">>, Disabled)
    ),
    exec(Conn, <<"UPDATE \"user\" SET status=1 WHERE id=977002">>, []),
    exec(Conn, <<"DELETE FROM organization_department_member WHERE user_id=977002">>, []),
    {ok, MissingDepartment} = page(Conn, Ctx, persons, Q),
    ?assertMatch(
        [#{<<"projection_status">> := <<"pending">>}],
        maps:get(<<"items">>, MissingDepartment)
    ).

revocations(Conn, Ctx) ->
    Query = #{<<"external_organization_id">> => <<"fixture">>},
    exec(
        Conn,
        <<
            "UPDATE enterprise_directory_organization_source SET enabled=false"
            " WHERE external_organization_id='fixture'"
        >>,
        []
    ),
    ?assertEqual({error, resource_not_found}, page(Conn, Ctx, departments, Query)).

page(Conn, Ctx, Kind, Q) -> enterprise_directory_source_read_logic:page_tx(Conn, Ctx, Kind, Q).
exec(Conn, Sql, Args) ->
    {ok, _} = elib_pg:query(Conn, Sql, Args),
    ok.
