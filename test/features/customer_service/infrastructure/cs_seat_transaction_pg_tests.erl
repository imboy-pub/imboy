%%% Real PostgreSQL transaction oracle; use a newly created isolated database.
-module(cs_seat_transaction_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-export([run/1]).

run(Socket) ->
    Connect = fun() ->
        epgsql:connect(#{
            host => {local, Socket},
            port => 0,
            username => "departure_test",
            database => "postgres"
        })
    end,
    {ok, C} = Connect(),
    try
        sql(C, <<
            "CREATE TABLE customer_service_seat (organization_id bigint, business_identity_id bigint PRIMARY KEY,"
            " function_key text, enabled boolean, max_concurrent integer, created_by_user_id bigint,"
            " version integer DEFAULT 1, created_at timestamptz DEFAULT now(), updated_at timestamptz DEFAULT now());"
            "CREATE TABLE customer_service_seat_limit (organization_id bigint PRIMARY KEY, seat_limit integer);"
            "CREATE TABLE organization_business_identity(organization_id bigint,id bigint,function_key text,status text DEFAULT 'active');"
            "INSERT INTO organization_business_identity VALUES(1,1,'customer_service','active'),(1,2,'customer_service','active');"
            "CREATE TABLE organization(id bigint PRIMARY KEY,status text); INSERT INTO organization VALUES(1,'active'),(2,'active');"
            "CREATE TABLE workspace(id bigint,organization_id bigint,status text); INSERT INTO workspace VALUES(10,1,'active'),(20,2,'active');"
            "CREATE TABLE customer_service_event(id bigint PRIMARY KEY,organization_id bigint,workspace_id bigint NOT NULL,"
            "session_id bigint,business_identity_id bigint,actor_user_id bigint,actor_kind text,action text,detail jsonb);"
        >>),
        meck:new(config_ds, [non_strict, no_link]),
        meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
        meck:new(pooler, [non_strict, no_link]),
        meck:expect(pooler, take_member, fun(pgsql) ->
            undefined = get(seat_tx_connection),
            {ok, Conn} = Connect(),
            put(seat_tx_connection, Conn),
            Conn
        end),
        meck:expect(pooler, return_member, fun(pgsql, Conn) ->
            Conn = erase(seat_tx_connection),
            epgsql:close(Conn)
        end),
        meck:expect(pooler, return_member, fun(pgsql, Conn, _) ->
            Conn = erase(seat_tx_connection),
            epgsql:close(Conn)
        end),
        meck:new(cs_tsid, [non_strict, no_link]),
        meck:expect(cs_tsid, new_id, fun(_) -> erlang:unique_integer([positive, monotonic]) end),
        eunit:test(cases(C) ++ audit_cases(C) ++ governance_cases(C), [verbose])
    after
        meck:unload(),
        epgsql:close(C)
    end.

cases(C) ->
    [
        {"create uses the supplied connection and outer rollback", fun() ->
            seed(C, true),
            ?assertEqual(
                {rollback, audit_failure},
                elib_pg:with_tx(fun(Conn) ->
                    {ok, _} = cs_pg_seat:create_seat_limit_tx(Conn, 1, 2, true, 2, undefined),
                    throw({rollback, audit_failure})
                end)
            ),
            ?assertEqual(
                0,
                count(
                    C, <<"SELECT count(*) FROM customer_service_seat WHERE business_identity_id=2">>
                )
            )
        end},
        {"suspend rolls back with its caller", fun() ->
            seed(C, true),
            rollback_enabled(false),
            assert_enabled(C, true)
        end},
        {"resume rolls back with its caller", fun() ->
            seed(C, false),
            rollback_enabled(true),
            assert_enabled(C, false)
        end},
        {"store wrappers need only one connection", fun() ->
            seed(C, false),
            ?assertMatch({ok, _}, cs_pg_store:create_seat_limit_checked(1, 2, false, 1, undefined)),
            ?assertMatch({ok, _}, cs_pg_store:set_enabled_checked(1, 1, true, 1700000000)),
            ?assertMatch({ok, _}, cs_pg_store:set_enabled_checked(1, 1, false, 1700000001))
        end},
        {"limit rejection and foreign organization do not change seats", fun() ->
            seed(C, true),
            sql(C, <<"INSERT INTO customer_service_seat_limit VALUES(1,1)">>),
            ?assertEqual(
                {error, seat_limit_exceeded},
                cs_pg_store:create_seat_limit_checked(1, 2, true, 1, undefined)
            ),
            ?assertEqual(
                {error, not_found}, cs_pg_store:set_enabled_checked(2, 1, true, 1700000000)
            ),
            assert_enabled(C, true),
            ?assertEqual(1, count(C, <<"SELECT count(*) FROM customer_service_seat">>))
        end},
        {"concurrent creates retain the organization limit", fun() ->
            seed(C, false),
            sql(C, <<"INSERT INTO customer_service_seat_limit VALUES(1,2)">>),
            Parent = self(),
            Ref = make_ref(),
            [
                spawn(fun() ->
                    Parent ! {Ref, cs_pg_store:create_seat_limit_checked(1, Id, true, 1, undefined)}
                end)
             || Id <- [2, 3, 4, 5]
            ],
            Results = [
                receive
                    {Ref, R} -> R
                after 10000 -> error(timeout)
                end
             || _ <- [2, 3, 4, 5]
            ],
            ?assertEqual(2, length([ok || {ok, _} <- Results])),
            ?assertEqual(2, length([ok || {error, seat_limit_exceeded} <- Results])),
            ?assertEqual(
                2, count(C, <<"SELECT count(*) FROM customer_service_seat WHERE enabled=true">>)
            )
        end}
    ].

audit_cases(C) ->
    [
        {"audit rejection rolls back create and permits a clean retry", fun() ->
            seed(C, true),
            with_rejected_audit(C, fun() ->
                ?assertMatch(
                    {error, {audit_append_failed, _}},
                    cs_seat_app:create_seat(1, seat_params(2))
                ),
                ?assertEqual(
                    0,
                    count(
                        C,
                        <<"SELECT count(*) FROM customer_service_seat WHERE business_identity_id=2">>
                    )
                ),
                ?assertEqual(0, count(C, <<"SELECT count(*) FROM customer_service_event">>))
            end),
            ?assertMatch({ok, _}, cs_seat_app:create_seat(1, seat_params(2))),
            ?assertEqual(
                1,
                count(
                    C,
                    <<"SELECT count(*) FROM customer_service_event WHERE action='seat.created' AND workspace_id=10 AND business_identity_id=2">>
                )
            )
        end},
        {"audit rejection rolls back suspend", fun() ->
            seed(C, true),
            with_rejected_audit(C, fun() ->
                ?assertMatch(
                    {error, {audit_append_failed, _}},
                    cs_seat_app:suspend_seat(1, seat_params(1))
                ),
                assert_enabled(C, true),
                ?assertEqual(0, count(C, <<"SELECT count(*) FROM customer_service_event">>))
            end)
        end},
        {"audit rejection rolls back resume", fun() ->
            seed(C, false),
            with_rejected_audit(C, fun() ->
                ?assertMatch(
                    {error, {audit_append_failed, _}},
                    cs_seat_app:resume_seat(1, seat_params(1))
                ),
                assert_enabled(C, false),
                ?assertEqual(0, count(C, <<"SELECT count(*) FROM customer_service_event">>))
            end)
        end},
        {"successful operations commit one correctly scoped event each", fun() ->
            seed(C, false),
            ?assertMatch({ok, _}, cs_seat_app:create_seat(1, (seat_params(2))#{enabled => false})),
            ?assertMatch({ok, _}, cs_seat_app:resume_seat(1, seat_params(2))),
            ?assertMatch({ok, _}, cs_seat_app:suspend_seat(1, seat_params(2))),
            {ok, _, Rows} = epgsql:equery(
                C,
                <<"SELECT action,workspace_id,business_identity_id,actor_user_id FROM customer_service_event ORDER BY id">>,
                []
            ),
            ?assertEqual(
                [
                    {<<"seat.created">>, 10, 2, 99},
                    {<<"seat.resumed">>, 10, 2, 99},
                    {<<"seat.suspended">>, 10, 2, 99}
                ],
                Rows
            ),
            {ok, _, [{false, 3}]} = epgsql:equery(
                C,
                <<"SELECT enabled,version FROM customer_service_seat WHERE business_identity_id=2">>,
                []
            )
        end}
    ].

seat_params(IdentityId) ->
    #{
        workspace_id => 10,
        business_identity_id => IdentityId,
        at => 1700000000,
        actor_user_id => 99,
        created_by_user_id => 99,
        reason => <<"synthetic test">>
    }.

with_rejected_audit(C, Fun) ->
    sql(C, <<"ALTER TABLE customer_service_event ADD CONSTRAINT reject_audit CHECK(false)">>),
    try
        Fun()
    after
        sql(C, <<"ALTER TABLE customer_service_event DROP CONSTRAINT reject_audit">>)
    end.

governance_cases(C) ->
    [
        {"governance lists disabled seats with keyset paging", fun() ->
            seed(C, false),
            ?assertMatch({ok, _}, govern(1, create, (seat_params(2))#{enabled => false})),
            {ok, [First]} = govern(1, list, #{limit => 1}),
            ?assertEqual(1, maps:get(business_identity_id, First)),
            ?assertEqual(false, maps:get(enabled, First)),
            {ok, [Second]} = govern(1, list, #{limit => 1, after_id => 1}),
            ?assertEqual(2, maps:get(business_identity_id, Second)),
            ?assertMatch(
                {ok, #{business_identity_id := 2}}, govern(1, detail, #{business_identity_id => 2})
            ),
            ?assertEqual(
                {rollback, {error, not_found}}, govern(2, detail, #{business_identity_id => 2})
            ),
            {ok, []} = govern(2, list, #{})
        end},
        {"governance settings have optimistic version checks", fun() ->
            seed(C, false),
            ?assertMatch(
                {ok, #{enabled := true, max_concurrent := 4, version := 2}},
                govern(1, update, (seat_params(1))#{
                    expected_version => 1, enabled => true, max_concurrent => 4
                })
            ),
            ?assertEqual(
                {rollback, {error, conflict}},
                govern(1, update, (seat_params(1))#{expected_version => 1, enabled => false})
            ),
            ?assertMatch(
                {ok, #{enabled := true, max_concurrent := 4, version := 2}},
                govern(1, detail, #{business_identity_id => 1})
            ),
            ?assertEqual(1, count(C, <<"SELECT count(*) FROM customer_service_event">>)),
            {ok, _, [{<<"application">>, null, <<"77">>, <<"synthetic-governance-request">>}]} =
                epgsql:equery(
                    C,
                    <<"SELECT actor_kind,actor_user_id,detail->>'application_id',detail->>'correlation_id' FROM customer_service_event">>,
                    []
                )
        end},
        {"two governance updates cannot consume the same version", fun() ->
            seed(C, false),
            Parent = self(),
            Ref = make_ref(),
            [
                spawn(fun() ->
                    Parent !
                        {Ref,
                            govern(
                                1,
                                update,
                                (seat_params(1))#{expected_version => 1, max_concurrent => Max}
                            )}
                end)
             || Max <- [2, 3]
            ],
            Results = [
                receive
                    {Ref, R} -> R
                after 10000 -> error(timeout)
                end
             || _ <- [2, 3]
            ],
            ?assertEqual(1, length([ok || {ok, _} <- Results])),
            ?assertEqual(1, length([ok || {rollback, {error, conflict}} <- Results])),
            ?assertEqual(1, count(C, <<"SELECT count(*) FROM customer_service_event">>))
        end},
        {"governance audit and outer completion failures undo settings", fun() ->
            seed(C, false),
            with_rejected_audit(C, fun() ->
                ?assertMatch(
                    {rollback, {error, {audit_append_failed, _}}},
                    govern(1, update, (seat_params(1))#{
                        expected_version => 1, enabled => true, max_concurrent => 4
                    })
                ),
                assert_enabled(C, false)
            end),
            ?assertEqual(
                {rollback, completion_failure},
                elib_pg:with_tx(fun(Conn) ->
                    {ok, _} = customer_service_facade:govern_seat(
                        1, governance_params(Conn, create, seat_params(2))
                    ),
                    throw({rollback, completion_failure})
                end)
            ),
            ?assertEqual(
                0,
                count(
                    C, <<"SELECT count(*) FROM customer_service_seat WHERE business_identity_id=2">>
                )
            ),
            ?assertEqual(0, count(C, <<"SELECT count(*) FROM customer_service_event">>))
        end},
        {"governance enforces active parents and permits retiring a seat", fun() ->
            seed(C, true),
            ?assertEqual(
                {rollback, {error, {not_found, workspace}}},
                govern(1, create, (seat_params(2))#{workspace_id => 20})
            ),
            sql(C, <<"UPDATE organization_business_identity SET status='inactive' WHERE id=1">>),
            try
                ?assertEqual(
                    {rollback, {error, {not_found, identity}}},
                    govern(1, update, (seat_params(1))#{expected_version => 1, enabled => true})
                ),
                ?assertMatch(
                    {ok, #{enabled := false}},
                    govern(1, update, (seat_params(1))#{expected_version => 1, enabled => false})
                )
            after
                sql(C, <<"UPDATE organization_business_identity SET status='active' WHERE id=1">>)
            end,
            sql(C, <<"UPDATE organization SET status='inactive' WHERE id=1">>),
            try
                ?assertEqual({rollback, {error, {not_found, organization}}}, govern(1, list, #{}))
            after
                sql(C, <<"UPDATE organization SET status='active' WHERE id=1">>)
            end
        end},
        {"governance retains limits and rejects malformed settings", fun() ->
            seed(C, true),
            sql(C, <<"INSERT INTO customer_service_seat_limit VALUES(1,1)">>),
            ?assertEqual(
                {rollback, {error, seat_limit_exceeded}}, govern(1, create, seat_params(2))
            ),
            ?assertEqual(
                {error, {invalid_argument, govern_seat}},
                govern(1, update, (seat_params(1))#{expected_version => 1, max_concurrent => 0})
            ),
            ?assertEqual(
                {error, {invalid_argument, govern_seat}},
                govern(1, update, (seat_params(1))#{expected_version => 1})
            ),
            ?assertEqual(
                {error, {invalid_argument, govern_seat}},
                govern(1, detail, #{business_identity_id => 9223372036854775808})
            ),
            ?assertEqual(0, count(C, <<"SELECT count(*) FROM customer_service_event">>))
        end}
    ].

govern(OrgId, Operation, Params) ->
    elib_pg:with_tx(fun(Conn) ->
        customer_service_facade:govern_seat(OrgId, governance_params(Conn, Operation, Params))
    end).

governance_params(Conn, Operation, Params) ->
    Params#{
        connection => Conn,
        operation => Operation,
        application_id => 77,
        correlation_id => <<"synthetic-governance-request">>
    }.

rollback_enabled(Enabled) ->
    ?assertEqual(
        {rollback, audit_failure},
        elib_pg:with_tx(fun(Conn) ->
            {ok, _} = cs_pg_seat:set_enabled_limit_tx(Conn, 1, 1, Enabled, 1700000000),
            throw({rollback, audit_failure})
        end)
    ).

assert_enabled(C, Enabled) ->
    {ok, _, [{Enabled, 1}]} = epgsql:equery(
        C,
        <<"SELECT enabled,version FROM customer_service_seat WHERE organization_id=1 AND business_identity_id=1">>,
        []
    ).

seed(C, Enabled) ->
    sql(C, <<"TRUNCATE customer_service_event,customer_service_seat,customer_service_seat_limit">>),
    {ok, 1} = epgsql:equery(
        C,
        <<"INSERT INTO customer_service_seat(organization_id,business_identity_id,function_key,enabled,max_concurrent) VALUES(1,1,'customer_service',$1,1)">>,
        [Enabled]
    ).

count(C, Query) ->
    {ok, _, [{N}]} = epgsql:equery(C, Query, []),
    N.
sql(C, Query) ->
    Results = epgsql:squery(C, Query),
    lists:foreach(
        fun(R) -> ?assertNotMatch({error, _}, R) end,
        case is_list(Results) of
            true -> Results;
            false -> [Results]
        end
    ).
