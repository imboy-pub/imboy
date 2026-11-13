-module(elib_pg_acquisition_tests).
-include_lib("eunit/include/eunit.hrl").

transaction_acquisition_test_() ->
    [?_test(run_isolated(Mode)) || Mode <- [success, timeout, exception]].

connection_execution_test_() ->
    [
        ?_test(pure_mock_fixture:run(fun() -> check_execution(Mode) end))
     || Mode <- [exception, rollback_failure, rollback_status_mismatch]
    ].

check_execution(Mode) ->
    Conn = setup(),
    try
        ok = meck:expect(pooler, take_member, fun(pgsql) -> Conn end),
        ok = meck:expect(pooler, return_member, fun(pgsql, C, fail) when C =:= Conn -> ok end),
        ok = meck:expect(epgsql, squery, fun(_, <<"ROLLBACK">>) ->
            case Mode of
                rollback_failure -> {error, synthetic_rollback_failure};
                _ -> {ok, [], []}
            end
        end),
        ok = meck:expect(epgsql, get_cmd_status, fun(_) ->
            case Mode of
                rollback_status_mismatch -> {ok, commit};
                _ -> {ok, rollback}
            end
        end),
        Count = atomics:new(1, []),
        Result = elib_pg:with_conn(
            fun(_) ->
                atomics:add(Count, 1, 1),
                case Mode of
                    exception -> error(synthetic_after_write);
                    _ -> throw({abort_tx, synthetic_abort})
                end
            end,
            1
        ),
        Expected =
            case Mode of
                exception -> {error, {db_exception, error, synthetic_after_write}};
                _ -> {error, synthetic_abort}
            end,
        ?assertEqual(Expected, Result),
        ?assertEqual(1, atomics:get(Count, 1)),
        case Mode of
            exception ->
                ?assertEqual(1, meck:num_calls(pooler, return_member, 2));
            _ ->
                ?assertEqual(1, meck:num_calls(pooler, return_member, [pgsql, Conn, fail])),
                ?assertEqual(0, meck:num_calls(pooler, return_member, 2))
        end
    after
        cleanup(Conn)
    end.

%% Pooler mocks are VM-global. The full suite has live application workers;
%% keep all original acquisition assertions in a peer with no application boot.
run_isolated(Mode) ->
    pure_mock_fixture:run(fun() ->
        Conn = setup(),
        try
            check_wait(Conn, Mode)
        after
            cleanup(Conn)
        end
    end).

setup() ->
    Conn = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ok = meck:new(pooler, [no_link, passthrough]),
    ok = meck:expect(pooler, take_member, fun(pgsql) -> error_no_members end),
    ok = meck:expect(pooler, return_member, fun(pgsql, Conn0) when Conn0 =:= Conn -> ok end),
    ok = meck:expect(pooler, return_member, fun(pgsql, Conn0, fail) when Conn0 =:= Conn -> ok end),
    ok = meck:new(config_ds, [no_link, passthrough]),
    ok = meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
    ok = meck:new(epgsql, [no_link, passthrough]),
    ok = meck:expect(epgsql, with_transaction, fun(C, F, _Opts) -> F(C) end),
    ok = meck:expect(epgsql, get_cmd_status, fun(_) -> {ok, commit} end),
    ok = meck:expect(epgsql, squery, fun(_C, <<"ROLLBACK">>) -> {ok, [], []} end),
    Conn.

cleanup(Conn) ->
    [meck:unload(M) || M <- [epgsql, config_ds, pooler]],
    Conn ! stop.

check_wait(Conn, Mode) ->
    ok = meck:expect(pooler, take_member, fun(pgsql, 1000) ->
        case Mode of
            timeout -> error_no_members;
            _ -> Conn
        end
    end),
    Result = elib_pg:with_tx(fun(C) ->
        Conn = C,
        case Mode of
            exception -> error(synthetic_transaction_error);
            _ -> committed
        end
    end),
    Expected =
        case Mode of
            success -> committed;
            timeout -> {error, no_connection};
            exception -> {error, {db_exception, error, synthetic_transaction_error}}
        end,
    Calls =
        case Mode of
            timeout -> 0;
            _ -> 1
        end,
    ?assertEqual(Expected, Result),
    ?assertEqual(1, meck:num_calls(pooler, take_member, [pgsql])),
    ?assertEqual(1, meck:num_calls(pooler, take_member, [pgsql, 1000])),
    ?assertEqual(Calls, meck:num_calls(epgsql, with_transaction, 3)),
    ReturnArity =
        case Mode of
            exception -> 3;
            _ -> 2
        end,
    ?assertEqual(Calls, meck:num_calls(pooler, return_member, ReturnArity)).
