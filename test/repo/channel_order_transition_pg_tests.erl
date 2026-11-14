-module(channel_order_transition_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-export([run/1]).

%% The controller owns the empty marker DB and supplies every connection field.
%% Invoke run(#{host=>Host,port=>Port,username=>User,password=>Password,database=>Db}).
%% No environment/config fallback; the private connection cannot roll back a caller's work.
-spec run(map()) -> ok.
run(ConnectOpts) when is_map(ConnectOpts) ->
    Required = [database, host, password, port, username],
    ?assertEqual(Required, lists:sort(maps:keys(maps:with(Required, ConnectOpts)))),
    ExpectedDb = unicode:characters_to_binary(maps:get(database, ConnectOpts)),
    ?assertMatch(
        match,
        re:run(ExpectedDb, <<"\\Aimboy_order_transition_[a-z0-9_]+\\z">>, [{capture, none}])
    ),
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, Conn} = epgsql:connect(ConnectOpts),
    try
        run_conn(Conn, ExpectedDb)
    after
        epgsql:close(Conn)
    end.

run_conn(Conn, ExpectedDb) ->
    {ok, [#{<<"db">> := Db, <<"empty">> := true}]} = elib_pg:query(
        Conn,
        <<
            "SELECT current_database() AS db, "
            "to_regclass('public.channel_order') IS NULL AS empty"
        >>,
        []
    ),
    ?assertEqual(ExpectedDb, Db),
    {ok, []} = elib_pg:query(Conn, <<"BEGIN">>, []),
    try
        create_fixture(Conn),
        assert_transitions(Conn)
    after
        {ok, []} = elib_pg:query(Conn, <<"ROLLBACK">>, []),
        ?assertEqual(
            {ok, [#{<<"absent">> => true}]},
            elib_pg:query(
                Conn, <<"SELECT to_regclass('public.channel_order') IS NULL AS absent">>, []
            )
        )
    end.

create_fixture(Conn) ->
    %% Column types: migrations 00000003_channel; status 5: 00000073_channel_order_refunding.
    {ok, []} = elib_pg:query(
        Conn,
        <<
            "CREATE TABLE public.channel_order ("
            "order_no varchar(64) NOT NULL PRIMARY KEY, "
            "status smallint NOT NULL CHECK (status IN (0,1,2,3,4,5)), "
            "refund_reason varchar(500), refund_at timestamptz, updated_at timestamptz)"
        >>,
        []
    ),
    {ok, 1} = elib_pg:execute_count(
        Conn,
        <<
            "INSERT INTO public.channel_order(order_no,status,updated_at) "
            "VALUES($1,1,'2000-01-01 00:00:00+00'::timestamptz)"
        >>,
        [<<"transition-regression">>]
    ),
    ok.

assert_transitions(Conn) ->
    Order = <<"transition-regression">>,
    ?assertEqual(
        {ok, 1}, channel_order_repo:transition_tx(Conn, Order, 1, {5, <<"reserving">>})
    ),
    Reserving = row(Conn, Order),
    ?assertMatch(
        #{
            <<"status">> := 5,
            <<"refund_reason">> := <<"reserving">>,
            <<"refund_at">> := null,
            <<"updated_now">> := true
        },
        Reserving
    ),
    ?assertEqual(
        {ok, 0}, channel_order_repo:transition_tx(Conn, Order, 1, {5, <<"duplicate-reserve">>})
    ),
    ?assertEqual(Reserving, row(Conn, Order)),
    ?assertEqual(
        {ok, 1}, channel_order_repo:transition_tx(Conn, Order, 5, {2, <<"approved">>})
    ),
    Refunded = row(Conn, Order),
    ?assertMatch(
        #{
            <<"status">> := 2,
            <<"refund_reason">> := <<"approved">>,
            <<"updated_now">> := true,
            <<"refunded_now">> := true
        },
        Refunded
    ),
    ?assertNotEqual(null, maps:get(<<"refund_at">>, Refunded)),
    ?assertEqual(
        {ok, 0}, channel_order_repo:transition_tx(Conn, Order, 5, {2, <<"duplicate-refund">>})
    ),
    ?assertEqual(Refunded, row(Conn, Order)),
    ok.

row(Conn, Order) ->
    {ok, [Row]} = elib_pg:query(
        Conn,
        <<
            "SELECT order_no,status,refund_reason,refund_at,updated_at, "
            "updated_at=NOW() AS updated_now, refund_at=NOW() AS refunded_now "
            "FROM public.channel_order WHERE order_no=$1"
        >>,
        [Order]
    ),
    Row.
