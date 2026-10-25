-module(msg_delivery_ack_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-export([run/0]).

%% Explicit fresh database with the complete migration chain; no application VM.
run() ->
    application:load(imboy),
    application:set_env(imboy, sql_driver, pgsql),
    application:set_env(lager, handlers, [{lager_console_backend, [{level, error}]}]),
    {ok, _} = application:ensure_all_started(lager),
    eunit:test(
        {timeout, 900,
            {setup, fun setup/0, fun cleanup/1, fun(S) ->
                [
                    ?_test(sequence(S)),
                    ?_test(two_devices(S)),
                    ?_test(early_unknown(S)),
                    ?_test(scope(S)),
                    ?_test(concurrent(S)),
                    ?_test(worker_retry(S)),
                    ?_test(notice_lifecycle(S)),
                    ?_test(s2c_save_lifecycle(S)),
                    ?_test(s2c_capacity(S))
                ]
            end}},
        [verbose]
    ).

setup() ->
    Opts = #{
        host => os:getenv("ACK_FIX_PG_HOST"),
        port => list_to_integer(os:getenv("ACK_FIX_PG_PORT")),
        username => os:getenv("ACK_FIX_PG_USER"),
        password => os:getenv("ACK_FIX_PG_PASSWORD"),
        database => os:getenv("ACK_FIX_MAINT_DB")
    },
    {ok, Guard} = epgsql:connect(Opts),
    {ok, _, [{<<"7700547928978026537">>}]} = epgsql:squery(
        Guard,
        <<"SELECT system_identifier::text FROM pg_control_system()">>
    ),
    ok = epgsql:close(Guard),
    S = inttest_marker_db:provision(#{
        env_prefix => <<"ACK_FIX">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    try
        application:set_env(pooler, pools, []),
        {ok, _} = application:ensure_all_started(pooler),
        PoolOpts = (maps:get(server, S))#{
            database => binary_to_list(maps:get(db, S)),
            codecs => [{epgsql_codec_rfc3339_bin, []}]
        },
        {ok, _} = pooler:new_pool(#{
            name => pgsql,
            init_count => 8,
            max_count => 8,
            start_mfa => {epgsql, connect, [PoolOpts]}
        }),
        S
    catch
        C:R:St ->
            inttest_marker_db:release(S),
            erlang:raise(C, R, St)
    end.

cleanup(S) ->
    [meck:unload(M) || M <- [msg_c2c_ds, msg_archive_repo], lists:member(M, meck:mocked())],
    pooler:rm_pool(pgsql),
    inttest_marker_db:release(S).

reset() ->
    [
        begin
            {ok, _} = elib_pg:execute(<<"DELETE FROM public.", T/binary>>, [])
        end
     || T <- [
            <<"msg_delivery">>,
            <<"msg_c2c">>,
            <<"msg_s2c">>,
            <<"msg_store_staging">>,
            <<"user_device">>
        ]
    ],
    device(9, <<"a">>).
device(U, D) ->
    {ok, 1} = elib_pg:execute(
        <<"INSERT INTO public.user_device(id,user_id,device_id) VALUES($1,$2,$3)">>,
        [erlang:unique_integer([positive]), U, D]
    ),
    ok.
stage(K, M, U, Processed) ->
    {ok, 1} = elib_pg:execute(
        <<"INSERT INTO public.msg_store_staging ",
            "(id,type,msg_id,payload,from_id,to_id,conv_seq,processed_at) ",
            "VALUES($1,$2,$3,'{}',8,$4,$5,CASE WHEN $6 THEN now() ELSE NULL END)">>,
        [erlang:unique_integer([positive]), K, M, U, erlang:unique_integer([positive]), Processed]
    ),
    ok.
main(K, M, U) ->
    {ok, Rows} = elib_pg:query(
        <<"SELECT conv_seq FROM public.msg_store_staging WHERE type=$1 AND msg_id=$2">>, [K, M]
    ),
    Seq =
        case Rows of
            [#{<<"conv_seq">> := N}] -> N;
            [] -> erlang:unique_integer([positive])
        end,
    Tb = <<"public.msg_", K/binary>>,
    Extra =
        case K of
            <<"c2c">> -> <<",conv_seq">>;
            _ -> <<>>
        end,
    Values =
        case K of
            <<"c2c">> -> <<",$4">>;
            _ -> <<>>
        end,
    Ps =
        [erlang:unique_integer([positive]), M, U] ++
            case K of
                <<"c2c">> -> [Seq];
                _ -> []
            end,
    {ok, _} = elib_pg:execute(
        <<"INSERT INTO ", Tb/binary, "(id,msg_id,from_id,to_id,payload,msg_type", Extra/binary,
            ") VALUES($1,$2,8,$3,'{}','text'", Values/binary, ") ON CONFLICT DO NOTHING">>,
        Ps
    ),
    ok.
ack(K, M, U, D) ->
    {ok, _} = msg_delivery_repo:mark_acked(K, M, U, D),
    ok = msg_delivery_repo:delete_delivered(K, M, U, 30).
counts(K, M, U) ->
    {
        one(<<"SELECT count(*) FROM public.msg_", K/binary, " WHERE msg_id=$1 AND to_id=$2">>, [
            M, U
        ]),
        one(
            <<"SELECT count(*) FROM public.msg_delivery WHERE msg_kind=$1 AND msg_id=$2 AND to_uid=$3">>,
            [K, M, U]
        )
    }.
one(Sql, Ps) ->
    {ok, [Row]} = elib_pg:query(Sql, Ps),
    hd(maps:values(Row)).

sequence(_) ->
    reset(),
    stage(<<"c2c">>, <<"sequential">>, 9, true),
    main(<<"c2c">>, <<"sequential">>, 9),
    ack(<<"c2c">>, <<"sequential">>, 9, <<"a">>),
    ?assertEqual({0, 0}, counts(<<"c2c">>, <<"sequential">>, 9)),
    ack(<<"c2c">>, <<"sequential">>, 9, <<"a">>),
    ?assertEqual({0, 0}, counts(<<"c2c">>, <<"sequential">>, 9)).
two_devices(_) ->
    reset(),
    device(9, <<"b">>),
    stage(<<"c2c">>, <<"two-devices">>, 9, true),
    main(<<"c2c">>, <<"two-devices">>, 9),
    ack(<<"c2c">>, <<"two-devices">>, 9, <<"a">>),
    ack(<<"c2c">>, <<"two-devices">>, 9, <<"a">>),
    ?assertEqual({1, 1}, counts(<<"c2c">>, <<"two-devices">>, 9)),
    ack(<<"c2c">>, <<"two-devices">>, 9, <<"b">>),
    ?assertEqual({0, 0}, counts(<<"c2c">>, <<"two-devices">>, 9)).
early_unknown(_) ->
    reset(),
    stage(<<"c2c">>, <<"early">>, 9, false),
    ack(<<"c2c">>, <<"early">>, 9, <<"a">>),
    ?assertEqual({0, 1}, counts(<<"c2c">>, <<"early">>, 9)),
    main(<<"c2c">>, <<"early">>, 9),
    ack(<<"c2c">>, <<"early">>, 9, <<"a">>),
    ?assertEqual({1, 1}, counts(<<"c2c">>, <<"early">>, 9)),
    ack(<<"c2c">>, <<"unknown">>, 9, <<"a">>),
    ?assertEqual({0, 1}, counts(<<"c2c">>, <<"unknown">>, 9)),
    {ok, 1} = msg_store_repo:mark_processed(<<"c2c">>, <<"early">>),
    ok = msg_delivery_repo:delete_delivered(<<"c2c">>, <<"early">>, 9, 30),
    ?assertEqual({0, 0}, counts(<<"c2c">>, <<"early">>, 9)).
scope(_) ->
    reset(),
    stage(<<"s2c">>, <<"same">>, 9, true),
    stage(<<"c2c">>, <<"same">>, 10, true),
    stage(<<"c2c">>, <<"pending">>, 9, false),
    main(<<"c2c">>, <<"pending">>, 9),
    {ok, 2} = msg_delivery_repo:mark_acked_batch(
        <<"c2c">>, [<<"same">>, <<"pending">>], 9, <<"a">>
    ),
    ok = msg_delivery_repo:delete_delivered_batch(<<"c2c">>, [<<"same">>, <<"pending">>], 9, 30),
    ?assertEqual({0, 1}, counts(<<"c2c">>, <<"same">>, 9)),
    ?assertEqual({1, 1}, counts(<<"c2c">>, <<"pending">>, 9)),
    ?assertEqual({ok, 0}, msg_delivery_repo:mark_acked(<<"s2c">>, <<"same">>, 9, <<"a">>)).
concurrent(S) ->
    reset(),
    stage(<<"c2c">>, <<"lock-barrier">>, 9, true),
    main(<<"c2c">>, <<"lock-barrier">>, 9),
    Conn = maps:get(conn, S),
    {ok, _, _} = epgsql:squery(Conn, <<"BEGIN">>),
    Key = jsone:encode([<<"c2c">>, 9, <<"lock-barrier">>]),
    {ok, _, _} = epgsql:equery(Conn, <<"SELECT pg_advisory_xact_lock(hashtextextended($1,0))">>, [
        Key
    ]),
    P = self(),
    Barrier = make_ref(),
    {Pid, Mon} = spawn_monitor(fun() ->
        ack(<<"c2c">>, <<"lock-barrier">>, 9, <<"a">>),
        P ! {Barrier, done}
    end),
    try
        ok = wait_advisory(100),
        ?assertEqual({1, 0}, counts(<<"c2c">>, <<"lock-barrier">>, 9))
    after
        {ok, _, _} = epgsql:squery(Conn, <<"COMMIT">>)
    end,
    receive
        {Barrier, done} -> ok
    after 3000 -> error(barrier_timeout)
    end,
    receive
        {'DOWN', Mon, process, Pid, normal} -> ok
    after 1000 -> error(barrier_crash)
    end,
    ?assertEqual({0, 0}, counts(<<"c2c">>, <<"lock-barrier">>, 9)),
    reset(),
    stage(<<"c2c">>, <<"parallel">>, 9, true),
    main(<<"c2c">>, <<"parallel">>, 9),
    Parent = self(),
    Ref = make_ref(),
    Pids = [
        spawn_monitor(fun() ->
            receive
                Ref -> ok
            end,
            [ack(<<"c2c">>, <<"parallel">>, 9, <<"a">>) || _ <- lists:seq(1, 10)],
            Parent ! {Ref, done}
        end)
     || _ <- lists:seq(1, 6)
    ],
    [P ! Ref || {P, _} <- Pids],
    [
        receive
            {Ref, done} -> ok
        after 30000 -> error(concurrent_timeout)
        end
     || _ <- Pids
    ],
    [
        receive
            {'DOWN', R, process, P, normal} -> ok
        after 5000 -> error(concurrent_crash)
        end
     || {P, R} <- Pids
    ],
    ?assertEqual({0, 0}, counts(<<"c2c">>, <<"parallel">>, 9)).
worker_retry(_) ->
    reset(),
    stage(<<"c2c">>, <<"worker">>, 9, false),
    ack(<<"c2c">>, <<"worker">>, 9, <<"a">>),
    application:set_env(imboy, msg_archive_enabled, true),
    meck:new(msg_c2c_ds, [no_link]),
    meck:expect(msg_c2c_ds, write_msg, fun(_, M, _, _, U, _, _, _, _) ->
        case get(write_failure) of
            true -> {error, timeout};
            _ -> main(<<"c2c">>, M, U)
        end
    end),
    meck:new(msg_archive_repo, [no_link]),
    meck:expect(msg_archive_repo, archive, fun(_) ->
        case get(archive_failure) of
            true -> {error, timeout};
            _ -> ok
        end
    end),
    Row = #{
        <<"type">> => <<"c2c">>,
        <<"msg_id">> => <<"worker">>,
        <<"from_id">> => 8,
        <<"to_id">> => 9,
        <<"payload">> => <<"{}">>
    },
    try
        put(write_failure, true),
        msg_store_worker:process_row(Row),
        ?assertEqual({0, 1}, counts(<<"c2c">>, <<"worker">>, 9)),
        put(write_failure, false),
        put(archive_failure, true),
        msg_store_worker:process_row(Row),
        ?assertEqual({1, 1}, counts(<<"c2c">>, <<"worker">>, 9)),
        ?assertEqual(
            1,
            one(
                <<"SELECT count(*) FROM msg_store_staging WHERE msg_id='worker' AND processed_at IS NULL">>,
                []
            )
        ),
        put(archive_failure, false),
        rollback_fault(
            Row,
            <<"msg_store_staging">>,
            <<"UPDATE">>,
            <<"WHEN (NEW.processed_at IS NOT NULL)">>
        ),
        rollback_fault(Row, <<"msg_delivery">>, <<"DELETE">>, <<>>),
        msg_store_worker:process_row(Row),
        ?assertEqual({0, 0}, counts(<<"c2c">>, <<"worker">>, 9)),
        ?assertEqual(
            1,
            one(
                <<"SELECT count(*) FROM msg_store_staging WHERE msg_id='worker' AND processed_at IS NOT NULL AND error_msg IS NULL">>,
                []
            )
        )
    after
        erase(write_failure),
        erase(archive_failure),
        meck:unload(msg_c2c_ds),
        meck:unload(msg_archive_repo)
    end.

rollback_fault(Row, Table, Op, Condition) ->
    {ok, _} = elib_pg:execute(
        <<"CREATE OR REPLACE FUNCTION public.ack_test_fail() RETURNS trigger ",
            "LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'ack controlled fault'; END $$">>,
        []
    ),
    {ok, _} = elib_pg:execute(
        <<"CREATE TRIGGER ack_controlled_fault BEFORE ", Op/binary, " ON public.", Table/binary,
            " FOR EACH ROW ", Condition/binary, " EXECUTE FUNCTION public.ack_test_fail()">>,
        []
    ),
    try
        msg_store_worker:process_row(Row),
        ?assertEqual({1, 1}, counts(<<"c2c">>, <<"worker">>, 9)),
        ?assertEqual(
            1,
            one(
                <<"SELECT count(*) FROM msg_store_staging ",
                    "WHERE msg_id='worker' AND processed_at IS NULL">>,
                []
            )
        )
    after
        {ok, _} = elib_pg:execute(
            <<"DROP TRIGGER ack_controlled_fault ON public.", Table/binary>>, []
        )
    end.

wait_advisory(0) ->
    error(advisory_wait_not_observed);
wait_advisory(N) ->
    case
        one(
            <<"SELECT count(*) FROM pg_locks WHERE locktype='advisory' AND NOT granted ",
                "AND database=(SELECT oid FROM pg_database WHERE datname=current_database())">>,
            []
        )
    of
        0 ->
            timer:sleep(20),
            wait_advisory(N - 1);
        _ ->
            ok
    end.

notice_lifecycle(_) ->
    reset(),
    with_notice_mocks(fun() ->
        Payload = #{<<"msg_ids">> => [<<"read-business">>]},
        ack(<<"c2c">>, <<"notice">>, 9, <<"a">>),
        {ok, _} = elib_pg:execute(
            <<"CREATE OR REPLACE FUNCTION public.notice_register_fail() RETURNS trigger ",
                "LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'notice register controlled fault'; END $$">>,
            []
        ),
        {ok, _} = elib_pg:execute(
            <<"CREATE TRIGGER notice_register_fault BEFORE INSERT ON msg_store_staging ",
                "FOR EACH ROW WHEN (NEW.type='c2c_notice') EXECUTE FUNCTION public.notice_register_fail()">>,
            []
        ),
        try
            ?assertMatch(
                {error, _}, msg_operation_ds:register_read_notice(<<"notice">>, 8, 9, Payload)
            ),
            ?assertEqual({0, 1}, counts(<<"c2c">>, <<"notice">>, 9)),
            ?assertEqual(
                0, one(<<"SELECT count(*) FROM msg_store_staging WHERE type='c2c_notice'">>, [])
            )
        after
            {ok, _} = elib_pg:execute(
                <<"DROP TRIGGER notice_register_fault ON msg_store_staging">>, []
            )
        end,
        ok = msg_operation_ds:register_read_notice(<<"notice">>, 8, 9, Payload),
        ?assertMatch(
            {error, _},
            msg_operation_ds:register_read_notice(
                <<"notice">>,
                8,
                9,
                #{<<"msg_ids">> => [<<"different-business">>]}
            )
        ),
        ?assertEqual(
            ok,
            msg_operation_ds:register_read_notice(<<"notice">>, 8, 9, Payload#{<<"read_at">> => 123})
        ),
        ack(<<"c2c">>, <<"notice">>, 9, <<"a">>),
        ack(<<"c2c">>, <<"notice">>, 9, <<"a">>),
        ?assertEqual({0, 0}, counts(<<"c2c">>, <<"notice">>, 9)),
        ?assertEqual(
            1,
            one(
                <<"SELECT count(*) FROM msg_store_staging WHERE type='c2c_notice' ",
                    "AND msg_id='notice' AND action='message_read' AND to_id=9 AND from_id=8 ",
                    "AND processed_at IS NOT NULL AND conv_seq IS NULL">>,
                []
            )
        ),
        ?assertMatch(
            {error, _}, msg_operation_ds:register_read_notice(<<"notice">>, 8, 10, Payload)
        ),
        ack(<<"c2c">>, <<"notice">>, 10, <<"a">>),
        ?assertEqual({0, 1}, counts(<<"c2c">>, <<"notice">>, 10)),
        ack(<<"s2c">>, <<"notice">>, 9, <<"a">>),
        ?assertEqual({0, 1}, counts(<<"s2c">>, <<"notice">>, 9)),
        stage(<<"c2c">>, <<"notice">>, 9, false),
        ack(<<"c2c">>, <<"notice">>, 9, <<"a">>),
        ?assertEqual({0, 1}, counts(<<"c2c">>, <<"notice">>, 9)),
        {ok, 1} = elib_pg:execute(
            <<"UPDATE msg_store_staging SET processed_at=now(),error_msg='terminal failure' ",
                "WHERE type='c2c' AND msg_id='notice'">>,
            []
        ),
        ack(<<"c2c">>, <<"notice">>, 9, <<"a">>),
        ?assertEqual({0, 1}, counts(<<"c2c">>, <<"notice">>, 9)),
        main(<<"c2c">>, <<"notice">>, 9),
        ?assertEqual({1, 1}, counts(<<"c2c">>, <<"notice">>, 9))
    end).
s2c_save_lifecycle(_) ->
    reset(),
    with_notice_mocks(fun() ->
        meck:new(message_ds, [passthrough, no_link]),
        meck:expect(message_ds, send_next, fun(_, _, _, _) -> ok end),
        try
            {ok, _} = elib_pg:execute(
                <<"CREATE OR REPLACE FUNCTION public.ack_notice_fail() RETURNS trigger ",
                    "LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'notification controlled fault'; END $$">>,
                []
            ),
            {ok, _} = elib_pg:execute(
                <<"CREATE TRIGGER ack_notice_fault BEFORE INSERT ON msg_store_staging ",
                    "FOR EACH ROW EXECUTE FUNCTION public.ack_notice_fail()">>,
                []
            ),
            try
                ?assertMatch(
                    {error, _},
                    msg_s2c_ds:send(0, [9], <<"message_read_sync">>, <<>>, null, #{}, save)
                ),
                ?assertEqual(0, meck:num_calls(message_ds, send_next, 4)),
                ?assertEqual(0, one(<<"SELECT count(*) FROM msg_store_staging">>, []))
            after
                {ok, _} = elib_pg:execute(
                    <<"DROP TRIGGER ack_notice_fault ON msg_store_staging">>, []
                )
            end,
            ok = msg_s2c_ds:send(
                0,
                [9],
                <<"message_read_sync">>,
                <<"custom">>,
                #{<<"synthetic_key">> => <<"preserved">>},
                #{<<"msg_ids">> => [<<"business">>]},
                save
            ),
            {ok, [Row]} = elib_pg:query(<<"SELECT * FROM msg_store_staging WHERE type='s2c'">>, []),
            Id = maps:get(<<"msg_id">>, Row),
            application:set_env(imboy, msg_archive_enabled, false),
            msg_store_worker:process_row(Row),
            msg_store_worker:process_row(Row),
            ?assertEqual({1, 0}, counts(<<"s2c">>, Id, 9)),
            ?assertEqual(
                1,
                one(
                    <<"SELECT count(*) FROM msg_s2c m JOIN msg_store_staging s ON s.id=m.id ",
                        "AND s.created_at=m.created_at AND s.server_ts=m.server_ts ",
                        "WHERE m.msg_id=$1 AND m.action=s.action AND m.msg_type=s.msg_type ",
                        "AND m.e2ee IS NOT DISTINCT FROM s.e2ee AND m.payload=s.payload">>,
                    [Id]
                )
            ),
            Bad = Row#{<<"msg_id">> => <<"conflicting-storage-id">>},
            ?assertEqual({error, staging_identity_conflict}, msg_store_worker:do_write(s2c, Bad)),
            ack(<<"s2c">>, Id, 9, <<"a">>),
            ack(<<"s2c">>, Id, 9, <<"a">>),
            ?assertEqual({0, 0}, counts(<<"s2c">>, Id, 9)),
            ?assertEqual(
                1,
                one(
                    <<"SELECT count(*) FROM msg_store_staging ",
                        "WHERE type='s2c' AND processed_at IS NOT NULL AND action='message_read_sync'">>,
                    []
                )
            )
        after
            meck:unload(message_ds)
        end
    end).
with_notice_mocks(F) ->
    meck:new(elib_tsid, [passthrough, no_link]),
    meck:expect(elib_tsid, generate, fun(_) -> erlang:unique_integer([positive]) end),
    try
        F()
    after
        meck:unload(elib_tsid)
    end.

s2c_capacity(_) ->
    reset(),
    with_notice_mocks(fun() ->
        {ok, 5000} = elib_pg:execute(
            <<"INSERT INTO msg_s2c(id,from_id,to_id,msg_id,action,payload) ",
                "SELECT -n,0,9,'cap-'||n,'capacity','{}' FROM generate_series(1,5000) n">>,
            []
        ),
        CreatedAt = elib_dt:now(),
        Row = #{
            <<"id">> => 123456789,
            <<"type">> => <<"s2c">>,
            <<"msg_id">> => <<"capacity-new">>,
            <<"payload">> => <<"{}">>,
            <<"from_id">> => 0,
            <<"to_id">> => 9,
            <<"created_at">> => CreatedAt,
            <<"server_ts">> => CreatedAt,
            <<"action">> => <<"capacity">>
        },
        ?assertEqual(ok, msg_store_worker:do_write(s2c, Row)),
        ok = wait_s2c_count(5000, 100),
        ?assertEqual(0, one(<<"SELECT count(*) FROM msg_s2c WHERE id=-5000">>, [])),
        ?assertEqual(ok, msg_store_worker:do_write(s2c, Row)),
        ?assertEqual(5000, one(<<"SELECT count(*) FROM msg_s2c WHERE to_id=9">>, [])),
        ?assertEqual(1, one(<<"SELECT count(*) FROM msg_s2c WHERE id=-4999">>, []))
    end).
wait_s2c_count(_, 0) ->
    error(capacity_cleanup_timeout);
wait_s2c_count(Expected, N) ->
    case one(<<"SELECT count(*) FROM msg_s2c WHERE to_id=9">>, []) of
        Expected ->
            ok;
        _ ->
            timer:sleep(20),
            wait_s2c_count(Expected, N - 1)
    end.
