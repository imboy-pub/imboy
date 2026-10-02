-module(elib_pg_acquisition_tests).
-include_lib("eunit/include/eunit.hrl").

transaction_acquisition_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(C) -> ?_test(check_wait(C, success)) end,
        fun(C) -> ?_test(check_wait(C, timeout)) end,
        fun(C) -> ?_test(check_wait(C, exception)) end
    ]}.

setup() ->
    Conn = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ok = meck:new(pooler, [no_link, passthrough]),
    ok = meck:expect(pooler, take_member, fun(pgsql) -> error_no_members end),
    ok = meck:expect(pooler, return_member, fun(pgsql, Conn0) when Conn0 =:= Conn -> ok end),
    ok = meck:new(config_ds, [no_link, passthrough]),
    ok = meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
    ok = meck:new(epgsql, [no_link, passthrough]),
    ok = meck:expect(epgsql, with_transaction, fun(C, F, _Opts) -> F(C) end),
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
    ?assertEqual(Calls, meck:num_calls(pooler, return_member, 2)).
