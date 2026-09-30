%%% Focused real PostgreSQL oracle; run only against a new isolated database.
-module(cs_transfer_capacity_pg_tests).
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
    Counter = ets:new(cs_transfer_checkouts, [public]),
    ets:insert(Counter, {calls, 0}),
    try
        fixture(C),
        meck:new(config_ds, [non_strict, no_link]),
        meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
        meck:new(pooler, [non_strict, no_link]),
        meck:expect(pooler, take_member, fun(pgsql) ->
            ets:update_counter(Counter, calls, 1),
            {ok, Conn} = Connect(),
            Conn
        end),
        meck:expect(pooler, return_member, fun(pgsql, Conn) -> epgsql:close(Conn) end),
        meck:expect(pooler, return_member, fun(pgsql, Conn, _) -> epgsql:close(Conn) end),
        meck:new(cs_tsid, [non_strict, no_link]),
        meck:expect(cs_tsid, new_id, fun(_) -> erlang:unique_integer([positive, monotonic]) end),
        eunit:test(cases(C, Counter), [verbose])
    after
        meck:unload(),
        ets:delete(Counter),
        epgsql:close(C)
    end.

cases(C, Counter) ->
    [
        {"disabled target rejects without writes", fun() -> disabled_target(C) end},
        {"full target rejects without writes", fun() -> full_target(C) end},
        {"two transfers share target capacity", fun() -> two_transfers(C) end},
        {"claim and transfer share target capacity", fun() -> claim_and_transfer(C) end},
        {"cursor boundary uses one transaction checkout", fun() ->
            transaction_connection(C, Counter)
        end},
        {"audit failure rolls back assignment and cursor", fun() -> audit_rollback(C) end},
        {"stale version and foreign target reject", fun() -> invalid_target(C) end}
    ].

disabled_target(C) ->
    seed(C),
    sql(
        C, <<"UPDATE customer_service_seat SET enabled=false WHERE business_identity_id=3">>
    ),
    ?assertEqual({error, seat_disabled}, transfer(1, 3)),
    unchanged(C).

full_target(C) ->
    seed(C),
    sql(C, <<"UPDATE customer_service_session SET business_identity_id=3 WHERE id=2">>),
    ?assertEqual({error, seat_at_capacity}, transfer(1, 3)),
    unchanged(C).

two_transfers(C) ->
    seed(C),
    Results = race([fun() -> transfer(1, 3) end, fun() -> transfer(2, 3) end]),
    ?assertEqual(1, length([ok || {ok, _} <- Results])),
    ?assertEqual(1, length([ok || {error, seat_at_capacity} <- Results])),
    ?assertEqual(
        1,
        value(
            C,
            <<"SELECT count(*) FROM customer_service_session WHERE business_identity_id=3">>
        )
    ),
    ?assertEqual(1, value(C, <<"SELECT count(*) FROM customer_service_event">>)).

claim_and_transfer(C) ->
    seed(C),
    sql(
        C,
        <<"INSERT INTO customer_service_session(id,organization_id,workspace_id,conversation_id,status,version) VALUES(3,1,10,300,'queued',1)">>
    ),
    Results = race([
        fun() -> transfer(1, 3) end,
        fun() ->
            cs_pg_session:claim_session(1, 10, 3, 3, 1, 1700000000, (event(3, 3))#{
                action => <<"session.claimed">>
            })
        end
    ]),
    ?assertEqual(1, length([ok || {ok, _} <- Results])),
    ?assertEqual(1, length([ok || {error, seat_at_capacity} <- Results])),
    ?assertEqual(
        1,
        value(
            C,
            <<"SELECT count(*) FROM customer_service_session WHERE business_identity_id=3">>
        )
    ).

transaction_connection(C, Counter) ->
    seed(C),
    Before = ets:lookup_element(Counter, calls, 2),
    ?assertMatch({ok, _}, transfer(1, 3)),
    ?assertEqual(1, ets:lookup_element(Counter, calls, 2) - Before),
    ?assertEqual(
        90, value(C, <<"SELECT last_read_message_id FROM customer_service_read_cursor">>)
    ).

audit_rollback(C) ->
    seed(C),
    Event = (event(1, 3))#{action => <<"reject">>},
    ?assertMatch(
        {error, {event_append_failed, _}},
        cs_pg_session:transfer_session(1, 10, 1, 3, 1, 1700000000, Event)
    ),
    unchanged(C).

invalid_target(C) ->
    seed(C),
    ?assertEqual(
        {error, conflict},
        cs_pg_session:transfer_session(1, 10, 1, 3, 99, 1700000000, event(1, 3))
    ),
    ?assertEqual({error, seat_not_found}, transfer(1, 4)),
    unchanged(C).

transfer(Session, Target) ->
    cs_pg_session:transfer_session(1, 10, Session, Target, 1, 1700000000, event(Session, Target)).

event(Session, Target) ->
    #{
        workspace_id => 10,
        session_id => Session,
        business_identity_id => Target,
        actor_kind => <<"seat">>,
        action => <<"session.transferred">>
    }.

unchanged(C) ->
    ?assertEqual(
        1, value(C, <<"SELECT business_identity_id FROM customer_service_session WHERE id=1">>)
    ),
    ?assertEqual(1, value(C, <<"SELECT version FROM customer_service_session WHERE id=1">>)),
    ?assertEqual(0, value(C, <<"SELECT count(*) FROM customer_service_event">>)),
    ?assertEqual(0, value(C, <<"SELECT count(*) FROM customer_service_read_cursor">>)).

race(Funs) ->
    Parent = self(),
    Ref = make_ref(),
    Pids = [
        spawn_monitor(fun() ->
            receive
                Ref -> Parent ! {Ref, F()}
            end
        end)
     || F <- Funs
    ],
    [Pid ! Ref || {Pid, _} <- Pids],
    Results = [
        receive
            {Ref, Result} -> Result
        after 10000 -> error(race_timeout)
        end
     || _ <- Pids
    ],
    [
        receive
            {'DOWN', M, process, _, normal} -> ok;
            {'DOWN', M, process, _, Reason} -> error({worker_crash, Reason})
        after 10000 -> error(worker_timeout)
        end
     || {_, M} <- Pids
    ],
    Results.

seed(C) ->
    sql(C, <<
        "TRUNCATE customer_service_session,customer_service_seat,customer_service_event,customer_service_read_cursor,enterprise_message;"
        "INSERT INTO customer_service_seat VALUES(1,1,true,5),(1,2,true,5),(1,3,true,1),(2,4,true,5);"
        "INSERT INTO customer_service_session(id,organization_id,workspace_id,conversation_id,business_identity_id,status,version) VALUES(1,1,10,100,1,'active',1),(2,1,10,200,2,'active',1);"
        "INSERT INTO enterprise_message VALUES(90,1,10,100);"
    >>).

fixture(C) ->
    sql(C, <<
        "CREATE TABLE workspace(id bigint,organization_id bigint); INSERT INTO workspace VALUES(10,1);"
        "CREATE TABLE customer_service_seat(organization_id bigint,business_identity_id bigint PRIMARY KEY,enabled boolean,max_concurrent integer);"
        "CREATE TABLE customer_service_session(id bigint PRIMARY KEY,organization_id bigint,workspace_id bigint,contact_id bigint,conversation_id bigint,"
        "business_identity_id bigint,visit_token_id bigint,status text,rating integer,rating_at timestamptz,queued_at timestamptz,claimed_at timestamptz,closed_at timestamptz,close_reason text,version integer,updated_at timestamptz);"
        "CREATE TABLE customer_service_event(id bigint PRIMARY KEY,organization_id bigint,workspace_id bigint,session_id bigint,business_identity_id bigint,actor_user_id bigint,actor_kind text,action text CHECK(action <> 'reject'),detail jsonb);"
        "CREATE TABLE customer_service_read_cursor(id bigint,organization_id bigint,workspace_id bigint,session_id bigint,business_identity_id bigint,last_read_message_id bigint,created_at timestamptz,updated_at timestamptz,UNIQUE(organization_id,session_id,business_identity_id));"
        "CREATE TABLE enterprise_message(id bigint,organization_id bigint,workspace_id bigint,conversation_id bigint);"
    >>).

value(C, Query) ->
    {ok, _, [{Value}]} = epgsql:squery(C, Query),
    binary_to_integer(Value).

sql(C, Query) ->
    Result = epgsql:squery(C, Query),
    [
        ?assertNotMatch({error, _}, Item)
     || Item <-
            case is_list(Result) of
                true -> Result;
                false -> [Result]
            end
    ],
    ok.
