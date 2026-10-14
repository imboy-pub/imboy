-module(enterprise_source_person_r20_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

r20_test_() ->
    {timeout, 900, fun run/0}.

metadata_down_test_() ->
    [{timeout, 900, fun() -> metadata_down(Restore) end} || Restore <- [false, true]].

metadata_down(Restore) ->
    elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    State = inttest_marker_db:provision(#{
        env_prefix => <<"R20_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    Conn = maps:get(conn, State),
    try
        seed(Conn),
        AppId = app(Conn),
        {ok, _} = enterprise_external_identity_repo:bind_tx(
            Conn, 976100, AppId, <<"metadata-down">>, 976002
        ),
        {ok, _} = query(
            Conn,
            <<
                "UPDATE enterprise_external_identity SET created_at='2020-01-01',"
                "updated_at='2020-01-01',status='removed'"
            >>,
            []
        ),
        {ok, _} = query(
            Conn,
            <<
                "UPDATE enterprise_source_person SET created_at='2020-01-01',"
                "updated_at='2020-01-01'"
            >>,
            []
        ),
        case Restore of
            true ->
                {ok, _} = enterprise_external_identity_repo:bind_tx(
                    Conn, 976100, AppId, <<"metadata-down">>, 976002
                );
            false ->
                ok
        end,
        check_metadata_down(Conn, Restore)
    after
        inttest_marker_db:release(State)
    end.

check_metadata_down(Conn, Restore) ->
    Before = metadata_snapshot(Conn),
    {ok, Down} = file:read_file("priv/migrations/00000176_enterprise_source_person.down.sql"),
    {ok, Up} = file:read_file("priv/migrations/00000176_enterprise_source_person.up.sql"),
    {ok, _} = query(Conn, <<"BEGIN">>, []),
    try
        Results = result_list(epgsql:squery(Conn, Down)),
        case Restore of
            true ->
                ?assert(
                    lists:any(
                        fun
                            ({error, #error{code = <<"23514">>}}) -> true;
                            (_) -> false
                        end,
                        Results
                    )
                );
            false ->
                ?assertEqual([], [Error || {error, _} = Error <- Results]),
                UpResults = result_list(epgsql:squery(Conn, Up)),
                ?assertEqual([], [Error || {error, _} = Error <- UpResults]),
                ?assertEqual(Before, metadata_snapshot(Conn))
        end
    after
        {ok, _} = query(Conn, <<"ROLLBACK">>, [])
    end,
    ?assertEqual(Before, metadata_snapshot(Conn)).

metadata_snapshot(Conn) ->
    {ok, Legacy} = query(
        Conn, <<"SELECT to_jsonb(e) AS row FROM enterprise_external_identity e ORDER BY id">>, []
    ),
    {ok, Source} = query(
        Conn,
        <<"SELECT to_jsonb(p) AS row FROM enterprise_source_person p ORDER BY application_id,external_user_id">>,
        []
    ),
    {Legacy, Source}.

run() ->
    elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    State = inttest_marker_db:provision(#{
        env_prefix => <<"R20_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    Conn = maps:get(conn, State),
    try
        seed(Conn),
        executor_deletion(Conn),
        executor_rollback(Conn),
        legacy_and_versions(Conn),
        retired(Conn),
        concurrent_claim(State),
        rollback_gate(Conn)
    after
        inttest_marker_db:release(State)
    end.

seed(Conn) ->
    seed_users(Conn),
    seed_organizations(Conn),
    {ok, _} = enterprise_application_repo:create_tx(
        Conn, 976100, <<"r20-source">>, <<"R20">>
    ).

seed_users(Conn) ->
    lists:foreach(
        fun(UserId) ->
            {ok, _} = query(
                Conn,
                <<
                    "INSERT INTO \"user\" "
                    "(id,password,account,account_type,status,reg_ip,reg_cosv) "
                    "VALUES ($1,'x',$2,0,1,'127.0.0.1','x')"
                >>,
                [UserId, <<"r20-", (integer_to_binary(UserId))/binary>>]
            )
        end,
        lists:seq(976001, 976009)
    ).

seed_organizations(Conn) ->
    {ok, _} = query(
        Conn,
        <<
            "INSERT INTO organization "
            "(id,name,owner_id,status,branding,settings) "
            "VALUES (976100,'r20',976001,'active','{}','{}')"
        >>,
        []
    ),
    lists:foreach(
        fun(UserId) ->
            {ok, _} = query(
                Conn,
                <<
                    "INSERT INTO organization_member "
                    "(organization_id,user_id,role,status) VALUES (976100,$1,'member','active')"
                >>,
                [UserId]
            )
        end,
        lists:seq(976002, 976006) ++ [976009]
    ),
    {ok, _} = query(
        Conn,
        <<
            "INSERT INTO organization "
            "(id,name,owner_id,status,branding,settings) "
            "VALUES (976101,'r20-other',976001,'active','{}','{}')"
        >>,
        []
    ),
    {ok, _} = query(
        Conn,
        <<
            "INSERT INTO organization_member "
            "(organization_id,user_id,role,status) VALUES "
            "(976101,976002,'member','active'),(976101,976003,'member','active'),"
            "(976101,976009,'member','active')"
        >>,
        []
    ).

app(Conn) ->
    {ok, [#{<<"id">> := AppId}]} = query(
        Conn,
        <<"SELECT id FROM enterprise_application WHERE application_key='r20-source'">>,
        []
    ),
    AppId.

executor_deletion(Conn) ->
    AppId = app(Conn),
    ExternalId = <<"executor-deletion">>,
    UserId = 976009,
    {ok, _} = enterprise_source_person_repo:claim_tx(Conn, AppId, ExternalId, UserId),
    {ok, _} = enterprise_external_identity_repo:bind_tx(Conn, 976100, AppId, ExternalId, UserId),
    {ok, _} = enterprise_external_identity_repo:bind_tx(Conn, 976101, AppId, ExternalId, UserId),
    {ok, [_, _]} = query(
        Conn,
        <<"SELECT id FROM enterprise_external_identity WHERE application_id=$1 AND external_user_id=$2">>,
        [AppId, ExternalId]
    ),
    executor_deletion_tx(Conn, AppId, ExternalId, UserId).

executor_deletion_tx(Conn, AppId, ExternalId, UserId) ->
    Providers = application:get_env(imboy, deletion_preflight_providers),
    Behavior = application:get_env(imboy, preflight_stub_behavior),
    try
        ok = application:set_env(
            imboy,
            deletion_preflight_providers,
            organization_preflight_stub_providers:full_registry()
        ),
        ok = application:set_env(imboy, preflight_stub_behavior, ok_all),
        {ok, _} = query(Conn, <<"BEGIN">>, []),
        try
            ok = user_deletion_executor:execute_main_tx(Conn, UserId),
            {ok, []} = query(Conn, <<"SELECT id FROM \"user\" WHERE id=$1">>, [UserId]),
            {ok, []} = query(
                Conn,
                <<"SELECT id FROM enterprise_external_identity WHERE application_id=$1 AND external_user_id=$2">>,
                [AppId, ExternalId]
            ),
            {ok, Person} = enterprise_source_person_repo:find_tx(Conn, AppId, ExternalId),
            ?assertEqual(true, maps:get(<<"binding_retired">>, Person)),
            ?assertEqual(null, maps:get(<<"user_id">>, Person)),
            ?assertEqual(
                {error, identity_conflict},
                enterprise_source_person_repo:claim_tx(Conn, AppId, ExternalId, 976008)
            ),
            ?assertEqual(
                {error, identity_conflict},
                enterprise_source_person_repo:sync_tx(
                    Conn, AppId, ExternalId, <<>>, <<"active">>, 1
                )
            )
        after
            {ok, _} = query(Conn, <<"ROLLBACK">>, [])
        end
    after
        restore_env(deletion_preflight_providers, Providers),
        restore_env(preflight_stub_behavior, Behavior)
    end.

restore_env(Key, undefined) -> application:unset_env(imboy, Key);
restore_env(Key, {ok, Value}) -> application:set_env(imboy, Key, Value).

executor_rollback(Conn) ->
    AppId = app(Conn),
    ExternalId = <<"executor-blocked-owner">>,
    UserId = 976001,
    {ok, _} = enterprise_external_identity_repo:bind_tx(Conn, 976100, AppId, ExternalId, UserId),
    {ok, Person} = enterprise_source_person_repo:find_tx(Conn, AppId, ExternalId),
    Providers = application:get_env(imboy, deletion_preflight_providers),
    Behavior = application:get_env(imboy, preflight_stub_behavior),
    try
        ok = application:set_env(
            imboy,
            deletion_preflight_providers,
            organization_preflight_stub_providers:full_registry()
        ),
        ok = application:set_env(imboy, preflight_stub_behavior, ok_all),
        {ok, _} = query(Conn, <<"BEGIN">>, []),
        try
            user_deletion_executor:execute_main_tx(Conn, UserId),
            error(owner_deletion_unexpectedly_succeeded)
        catch
            error:{delete_from_table_failed, Table, PgError} ->
                case PgError of
                    #error{code = <<"23001">>, codename = restrict_violation, extra = Extra} ->
                        ?assertEqual(<<"public.\"user\"">>, Table),
                        ?assertEqual(
                            <<"fk_organization_owner">>, proplists:get_value(constraint_name, Extra)
                        );
                    _ ->
                        error({unexpected_owner_deletion_error, Table, PgError})
                end
        after
            {ok, _} = query(Conn, <<"ROLLBACK">>, [])
        end,
        {ok, [_]} = query(Conn, <<"SELECT id FROM \"user\" WHERE id=$1">>, [UserId]),
        {ok, [_]} = query(
            Conn,
            <<"SELECT id FROM enterprise_external_identity WHERE application_id=$1 AND external_user_id=$2">>,
            [AppId, ExternalId]
        ),
        {ok, Person} = enterprise_source_person_repo:find_tx(Conn, AppId, ExternalId)
    after
        restore_env(deletion_preflight_providers, Providers),
        restore_env(preflight_stub_behavior, Behavior)
    end.

legacy_and_versions(Conn) ->
    AppId = app(Conn),
    {ok, _} = enterprise_source_person_repo:sync_tx(
        Conn, AppId, <<"pending">>, <<"Name">>, <<"active">>, 2
    ),
    {ok, _} = enterprise_external_identity_repo:bind_tx(Conn, 976100, AppId, <<"pending">>, 976002),
    {ok, Person} = enterprise_source_person_repo:find_tx(Conn, AppId, <<"pending">>),
    source_versions(Conn, AppId, Person),
    legacy_org_lifecycle(Conn, AppId),
    source_restore(Conn, AppId),
    invalid_claims(Conn, AppId).

source_versions(Conn, AppId, Person) ->
    ?assertEqual(976002, maps:get(<<"user_id">>, Person)),
    ?assertEqual(2, maps:get(<<"source_version">>, Person)),
    ?assertEqual(<<"Name">>, maps:get(<<"display_name">>, Person)),
    {ok, _} = enterprise_external_identity_repo:bind_tx(Conn, 976100, AppId, <<"pending">>, 976002),
    rejected(
        Conn,
        fun() ->
            enterprise_external_identity_repo:bind_tx(
                Conn, 976100, AppId, <<"pending">>, 976003
            )
        end,
        {error, identity_already_mapped}
    ),
    rejected(
        Conn,
        fun() ->
            enterprise_external_identity_repo:bind_tx(
                Conn, 976101, AppId, <<"pending">>, 976003
            )
        end,
        {error, identity_already_mapped}
    ),
    {ok, Person} = enterprise_source_person_repo:find_tx(Conn, AppId, <<"pending">>),
    ?assertEqual(
        {error, stale_source_version},
        enterprise_source_person_repo:sync_tx(
            Conn, AppId, <<"pending">>, <<"Name">>, <<"active">>, 1
        )
    ),
    ?assertEqual(
        {error, source_version_conflict},
        enterprise_source_person_repo:sync_tx(
            Conn, AppId, <<"pending">>, <<"Other">>, <<"active">>, 2
        )
    ),
    {ok, _} = enterprise_source_person_repo:sync_tx(
        Conn, AppId, <<"pending">>, <<"Name">>, <<"active">>, 2
    ),
    {ok, _} = enterprise_source_person_repo:sync_tx(
        Conn, AppId, <<"pending">>, <<"New">>, <<"active">>, 3
    ).

legacy_org_lifecycle(Conn, AppId) ->
    {ok, _} = enterprise_external_identity_repo:bind_tx(Conn, 976101, AppId, <<"pending">>, 976002),
    {ok, Person} = enterprise_source_person_repo:find_tx(Conn, AppId, <<"pending">>),
    ok = enterprise_external_identity_repo:unbind_tx(Conn, 976100, AppId, <<"pending">>),
    {ok, Person} = enterprise_source_person_repo:find_tx(Conn, AppId, <<"pending">>),
    {ok, #{<<"status">> := <<"active">>}} = enterprise_external_identity_repo:find_by_external_tx(
        Conn, 976101, AppId, <<"pending">>
    ),
    {ok, _} = enterprise_external_identity_repo:bind_tx(Conn, 976100, AppId, <<"pending">>, 976002),
    {ok, Person} = enterprise_source_person_repo:find_tx(Conn, AppId, <<"pending">>).

source_restore(Conn, AppId) ->
    {ok, Removed} = enterprise_source_person_repo:sync_tx(
        Conn, AppId, <<"pending">>, <<"New">>, <<"removed">>, 4
    ),
    {ok, Removed} = enterprise_source_person_repo:sync_tx(
        Conn, AppId, <<"pending">>, <<"New">>, <<"removed">>, 4
    ),
    ?assertEqual(
        {error, source_version_conflict},
        enterprise_source_person_repo:sync_tx(
            Conn, AppId, <<"pending">>, <<"New">>, <<"active">>, 4
        )
    ),
    ?assertEqual(
        {error, identity_conflict},
        enterprise_source_person_repo:claim_tx(
            Conn, AppId, <<"pending">>, 976002
        )
    ),
    rejected(
        Conn,
        fun() ->
            enterprise_external_identity_repo:bind_tx(
                Conn, 976100, AppId, <<"pending">>, 976002
            )
        end,
        {error, identity_already_mapped}
    ),
    {ok, Restored} = enterprise_source_person_repo:sync_tx(
        Conn, AppId, <<"pending">>, <<"Restored">>, <<"active">>, 5
    ),
    ?assertEqual(976002, maps:get(<<"user_id">>, Restored)),
    {ok, Restored} = enterprise_source_person_repo:sync_tx(
        Conn, AppId, <<"pending">>, <<"Restored">>, <<"active">>, 5
    ),
    ?assertEqual(
        {error, source_version_conflict},
        enterprise_source_person_repo:sync_tx(
            Conn, AppId, <<"pending">>, <<"Changed">>, <<"active">>, 5
        )
    ),
    {ok, _} = enterprise_external_identity_repo:bind_tx(Conn, 976100, AppId, <<"pending">>, 976002).

invalid_claims(Conn, AppId) ->
    {ok, _} = enterprise_source_person_repo:claim_tx(Conn, AppId, <<"pending">>, 976002),
    {ok, _} = enterprise_source_person_repo:sync_tx(
        Conn, AppId, <<"removed">>, <<>>, <<"removed">>, 1
    ),
    rejected(
        Conn,
        fun() ->
            enterprise_external_identity_repo:bind_tx(
                Conn, 976100, AppId, <<"removed">>, 976003
            )
        end,
        {error, identity_already_mapped}
    ),
    {ok, _} = enterprise_source_person_repo:sync_tx(
        Conn, AppId, <<"disabled">>, <<>>, <<"active">>, 1
    ),
    {ok, _} = query(Conn, <<"UPDATE \"user\" SET status=0 WHERE id=976006">>, []),
    rejected(
        Conn,
        fun() ->
            enterprise_external_identity_repo:bind_tx(
                Conn, 976100, AppId, <<"disabled">>, 976006
            )
        end,
        {error, invalid_member}
    ).

retired(Conn) ->
    AppId = app(Conn),
    {ok, _} = enterprise_source_person_repo:claim_tx(Conn, AppId, <<"retired">>, 976007),
    {ok, _} = query(Conn, <<"DELETE FROM \"user\" WHERE id=976007">>, []),
    {ok, Person} = enterprise_source_person_repo:find_tx(Conn, AppId, <<"retired">>),
    ?assertEqual(true, maps:get(<<"binding_retired">>, Person)),
    ?assertEqual(null, maps:get(<<"user_id">>, Person)),
    ?assertEqual(<<"removed">>, maps:get(<<"status">>, Person)),
    ?assertEqual(
        {error, identity_conflict},
        enterprise_source_person_repo:claim_tx(
            Conn, AppId, <<"retired">>, 976008
        )
    ),
    ?assertEqual(
        {error, identity_conflict},
        enterprise_source_person_repo:sync_tx(
            Conn, AppId, <<"retired">>, <<>>, <<"active">>, 1
        )
    ).

concurrent_claim(State) ->
    Conn = maps:get(conn, State),
    AppId = app(Conn),
    {ok, _} = enterprise_source_person_repo:sync_tx(Conn, AppId, <<"race">>, <<>>, <<"active">>, 1),
    Server = maps:get(server, State),
    Options = maps:merge(
        maps:with([host, port, username, password], Server),
        maps:get(connect_extra, Server, #{})
    ),
    {ok, Other} = inttest_marker_db:safe_connect(Options#{database => maps:get(db, State)}),
    {ok, [#{<<"pid">> := BackendPid}]} = query(Other, <<"SELECT pg_backend_pid() AS pid">>, []),
    Parent = self(),
    try
        {ok, _} = query(Conn, <<"BEGIN">>, []),
        {ok, _} = enterprise_external_identity_repo:bind_tx(
            Conn, 976100, AppId, <<"race">>, 976004
        ),
        Worker = spawn_monitor(fun() ->
            {ok, _} = query(Other, <<"SET lock_timeout='5s'">>, []),
            Parent ! {self(), started},
            Result = enterprise_source_person_repo:claim_tx(Other, AppId, <<"race">>, 976005),
            Parent ! {self(), Result}
        end),
        {WorkerPid, Monitor} = Worker,
        receive
            {WorkerPid, started} -> ok
        after 5000 -> error(worker_not_started)
        end,
        await_lock(Conn, BackendPid, 100),
        {ok, _} = query(Conn, <<"COMMIT">>, []),
        receive
            {WorkerPid, Result} -> ?assertEqual({error, identity_conflict}, Result)
        after 10000 -> error(claim_timeout)
        end,
        receive
            {'DOWN', Monitor, process, WorkerPid, Reason} -> ?assertEqual(normal, Reason)
        after 5000 -> error(worker_not_finished)
        end,
        {ok, Person} = enterprise_source_person_repo:find_tx(Conn, AppId, <<"race">>),
        ?assertEqual(976004, maps:get(<<"user_id">>, Person))
    after
        epgsql:close(Other)
    end.

await_lock(_, _, 0) ->
    error(concurrent_claim_did_not_wait);
await_lock(Conn, BackendPid, Attempts) ->
    {ok, [#{<<"waiting">> := Waiting}]} = query(
        Conn,
        <<"SELECT EXISTS(SELECT 1 FROM pg_stat_activity WHERE pid=$1 AND wait_event_type='Lock') AS waiting">>,
        [BackendPid]
    ),
    case Waiting of
        true -> ok;
        false -> receive
            after 20 -> await_lock(Conn, BackendPid, Attempts - 1)
            end
    end.

rollback_gate(Conn) ->
    {ok, Down} = file:read_file("priv/migrations/00000176_enterprise_source_person.down.sql"),
    {ok, _} = query(Conn, <<"BEGIN">>, []),
    try
        Results = epgsql:squery(Conn, Down),
        ?assert(
            lists:any(
                fun
                    ({error, #error{code = <<"23514">>}}) -> true;
                    (_) -> false
                end,
                result_list(Results)
            )
        )
    after
        {ok, _} = query(Conn, <<"ROLLBACK">>, [])
    end,
    {ok, _} = enterprise_source_person_repo:find_tx(Conn, app(Conn), <<"pending">>).

result_list(Results) when is_list(Results) -> Results;
result_list(Result) -> [Result].

rejected(Conn, Fun, Expected) ->
    {ok, _} = query(Conn, <<"BEGIN">>, []),
    try
        ?assertEqual(Expected, Fun())
    after
        {ok, _} = query(Conn, <<"ROLLBACK">>, [])
    end.

query(Conn, Sql, Params) -> elib_pg:query(Conn, Sql, Params).
