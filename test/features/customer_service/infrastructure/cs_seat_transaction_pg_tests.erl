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
        eunit:test(cases(C), [verbose])
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
    sql(C, <<"TRUNCATE customer_service_seat,customer_service_seat_limit">>),
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
