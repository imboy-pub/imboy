-module(enterprise_directory_user_delete_pg_checks).
-export([exercise/2]).
-include_lib("eunit/include/eunit.hrl").

exercise(State, Ctx) ->
    Conn = maps:get(conn, State),
    ?assertEqual(undefined, whereis(pgsql)),
    Providers = application:get_env(imboy, deletion_preflight_providers),
    Server = maps:get(server, State),
    Options = maps:merge(
        maps:with([host, port, username, password], Server),
        maps:get(connect_extra, Server, #{})
    ),
    {ok, _} = application:ensure_all_started(pooler),
    {ok, _} = pooler:new_pool(#{
        name => pgsql,
        init_count => 2,
        max_count => 4,
        start_mfa => {epgsql, connect, [Options#{database => maps:get(db, State)}]}
    }),
    try
        application:unset_env(imboy, deletion_preflight_providers),
        offboarded_user_deletion_tx(Conn, Ctx)
    after
        pooler:rm_pool(pgsql),
        case Providers of
            undefined -> application:unset_env(imboy, deletion_preflight_providers);
            {ok, Value} -> application:set_env(imboy, deletion_preflight_providers, Value)
        end
    end.

offboarded_user_deletion_tx(Conn, Ctx) ->
    {ok, _} = elib_pg:query(
        Conn,
        <<
            "INSERT INTO \"user\""
            " (id,password,account,account_type,status,reg_ip,reg_cosv)"
            " VALUES (977003,'x','directory-delete-person',0,1,'127.0.0.1','x')"
        >>,
        []
    ),
    DeleteBody = fun(Version, Status) ->
        Base = pending_body(Version, Status),
        [Person] = maps:get(<<"persons">>, Base),
        Base#{<<"persons">> => [Person#{<<"external_user_id">> => <<"delete-person">>}]}
    end,
    ?assertMatch(
        {ok, applied, _}, tx(Conn, Ctx, <<"delete-person-active">>, DeleteBody(1, <<"active">>))
    ),
    {ok, _} = enterprise_directory_sync_logic:claim_and_project_tx(
        Conn, Ctx, <<"delete-person">>, 977003
    ),
    ?assertMatch(
        {ok, applied, _}, tx(Conn, Ctx, <<"delete-person-offboard">>, DeleteBody(2, <<"removed">>))
    ),
    ?assertEqual(
        true,
        scalar(Conn, <<
            "SELECT offboarding_member_updated_at IS NOT NULL AS value"
            " FROM enterprise_directory_membership_source WHERE external_user_id='delete-person'"
        >>)
    ),
    {ok, #{blockers := [], facts := Facts}} = organization_deletion_preflight:run(977003),
    ?assertEqual(5, length(Facts)),
    History = deletion_history_snapshot(Conn),
    Before = enterprise_directory_sync_pg_tests:all_rows_snapshot(Conn),
    execute_delete_tx(Conn, rollback),
    ?assertEqual(Before, enterprise_directory_sync_pg_tests:all_rows_snapshot(Conn)),
    execute_delete_tx(Conn, commit),
    ?assertEqual(History, deletion_history_snapshot(Conn)),
    assert_retired_binding(Conn, Ctx, DeleteBody, History).

assert_retired_binding(Conn, Ctx, DeleteBody, History) ->
    ?assertMatch(
        {error, _},
        enterprise_source_person_repo:claim_tx(
            Conn, maps:get(application_id, Ctx), <<"delete-person">>, 977002
        )
    ),
    ?assertEqual(
        {error, identity_conflict},
        enterprise_source_person_repo:sync_tx(
            Conn, maps:get(application_id, Ctx), <<"delete-person">>, <<"Pending">>, <<"active">>, 2
        )
    ),
    ?assertEqual(
        {error, identity_conflict},
        tx(
            Conn,
            Ctx,
            <<"delete-person-restore-denied">>,
            DeleteBody(3, <<"active">>)
        )
    ),
    ?assertEqual(History, deletion_history_snapshot(Conn)).

execute_delete_tx(Conn, rollback) ->
    {ok, _} = elib_pg:query(Conn, <<"BEGIN">>, []),
    try
        ok = user_deletion_executor:execute_main_tx(Conn, 977003),
        assert_deleted_directory_person(Conn)
    after
        {ok, _} = elib_pg:query(Conn, <<"ROLLBACK">>, [])
    end;
execute_delete_tx(Conn, commit) ->
    {ok, _} = elib_pg:query(Conn, <<"BEGIN">>, []),
    try
        ok = user_deletion_executor:execute_main_tx(Conn, 977003),
        assert_deleted_directory_person(Conn),
        {ok, _} = elib_pg:query(Conn, <<"COMMIT">>, [])
    catch
        Class:Reason:Stack ->
            elib_pg:query(Conn, <<"ROLLBACK">>, []),
            erlang:raise(Class, Reason, Stack)
    end.

deletion_history_snapshot(Conn) ->
    {
        scalar(Conn, <<
            "SELECT (to_jsonb(s)-'projected_user_id')::text"
            " AS value FROM enterprise_directory_membership_source s WHERE external_user_id='delete-person'"
        >>),
        scalar(Conn, <<
            "SELECT COALESCE(jsonb_agg(to_jsonb(a) ORDER BY to_jsonb(a)::text),'[]'::jsonb)::text"
            " AS value FROM enterprise_audit_event a"
        >>)
    }.

assert_deleted_directory_person(Conn) ->
    ?assertEqual(0, scalar(Conn, <<"SELECT count(*) AS value FROM \"user\" WHERE id=977003">>)),
    ?assertEqual(
        true,
        scalar(Conn, <<
            "SELECT projected_user_id IS NULL"
            " AND offboarding_member_updated_at IS NOT NULL AND managed AND status='removed'"
            " AND source_version=2 AS value FROM enterprise_directory_membership_source"
            " WHERE external_user_id='delete-person'"
        >>)
    ),
    ?assertEqual(
        true,
        scalar(Conn, <<
            "SELECT user_id IS NULL AND binding_retired"
            " AND status='removed' AS value FROM enterprise_source_person WHERE external_user_id='delete-person'"
        >>)
    ).

pending_body(Version, Status) ->
    Person = #{
        <<"external_user_id">> => <<"pending">>,
        <<"display_name">> => <<"Pending">>,
        <<"source_status">> => <<"active">>,
        <<"source_version">> => 1,
        <<"membership">> => #{
            <<"version">> => Version,
            <<"status">> => Status,
            <<"department_external_ids">> =>
                case Status of
                    <<"active">> -> [<<"claim-dept">>];
                    <<"removed">> -> []
                end
        }
    },
    (body(2))#{<<"persons">> => [Person]}.

tx(Conn, Ctx, Key, Body) ->
    {ok, _, _} = epgsql:squery(Conn, "BEGIN"),
    try enterprise_directory_sync_logic:sync_tx(Conn, Ctx, Key, Body) of
        Result ->
            {ok, _, _} = epgsql:squery(Conn, "COMMIT"),
            Result
    catch
        throw:{abort_tx, Reason} ->
            {ok, _, _} = epgsql:squery(Conn, "ROLLBACK"),
            {error, Reason};
        Class:Reason:Stack ->
            _ = epgsql:squery(Conn, "ROLLBACK"),
            erlang:raise(Class, Reason, Stack)
    end.

scalar(Conn, Sql) ->
    {ok, [#{<<"value">> := Value}]} = elib_pg:query(Conn, Sql, []),
    Value.

body(Version) ->
    #{
        <<"organization">> => #{
            <<"external_id">> => <<"fixture">>,
            <<"name">> => <<"Synced">>,
            <<"version">> => Version
        },
        <<"departments">> => [],
        <<"persons">> => []
    }.
