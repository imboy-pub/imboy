-module(elib_pg_deadlock_retry_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

retry_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(C) -> ?_test(retry_only_confirmed_deadlock(C)) end,
        fun(C) -> ?_test(rollback_failure_discards_connection(C)) end,
        fun(C) -> ?_test(commit_unknown_never_retries(C)) end,
        fun(C) -> ?_test(commit_status_rollback_does_not_run_actions(C)) end,
        fun(C) -> ?_test(other_sql_error_never_retries(C)) end,
        fun(C) -> ?_test(after_commit_failure_never_retries(C)) end,
        fun(C) -> ?_test(nested_transaction_never_takes_second_connection(C)) end,
        fun(C) -> ?_test(normal_transaction_inside_retry_never_runs(C)) end,
        fun(C) -> ?_test(implicit_connection_aborts_without_checkout(C)) end,
        fun(C) -> ?_test(after_commit_can_checkout_after_context_clears(C)) end,
        fun(C) -> ?_test(active_connection_deadlock_aborts_retry(C)) end,
        fun(C) -> ?_test(other_connection_and_legacy_preserve_errors(C)) end,
        fun(C) -> ?_test(begin_failure_discards_and_clears_context(C)) end,
        fun(C) -> ?_test(rollback_status_unknown_discards_and_clears_context(C)) end,
        fun(C) -> ?_test(commit_eof_discards_and_clears_context(C)) end,
        fun(C) -> ?_test(after_commit_can_start_normal_transaction(C)) end,
        fun(C) -> ?_test(retry_is_bounded(C)) end
    ]}.

setup() ->
    Conn = spawn(fun wait/0),
    meck:new([pooler, epgsql], [no_link, passthrough]),
    meck:new(config_ds, [no_link, non_strict]),
    meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
    meck:expect(pooler, take_member, fun(pgsql) -> Conn end),
    meck:expect(pooler, take_member, fun(pgsql, _) -> Conn end),
    meck:expect(pooler, return_member, fun(pgsql, _) -> ok end),
    meck:expect(pooler, return_member, fun(pgsql, _, fail) -> ok end),
    meck:expect(epgsql, squery, fun(_, Sql) ->
        put(retry_test_status, status(Sql)),
        {ok, [], []}
    end),
    meck:expect(epgsql, get_cmd_status, fun(_) -> {ok, get(retry_test_status)} end),
    put(retry_test_attempt, 0),
    put(retry_test_actions, []),
    Conn.

wait() ->
    receive
        stop -> ok
    end.
status(<<"BEGIN">>) -> 'begin';
status(<<"COMMIT">>) -> commit;
status(<<"ROLLBACK">>) -> rollback.

cleanup(Conn) ->
    [meck:unload(M) || M <- [epgsql, config_ds, pooler]],
    Conn ! stop,
    erase(retry_test_status),
    erase(retry_test_attempt),
    erase(retry_test_actions).

deadlock() ->
    #error{
        severity = error,
        code = <<"40P01">>,
        codename = deadlock_detected,
        message = <<"deadlock">>,
        extra = []
    }.

retry_only_confirmed_deadlock(_) ->
    put(retry_test_attempt, 0),
    put(retry_test_actions, []),
    Result = elib_pg:with_deadlock_retry_tx(fun(C) ->
        N = get(retry_test_attempt) + 1,
        put(retry_test_attempt, N),
        elib_pg:after_commit(C, action, fun() ->
            put(retry_test_actions, [N]),
            ok
        end),
        case N of
            1 -> throw({abort_tx, deadlock()});
            2 -> ok
        end
    end),
    ?assertEqual(ok, Result),
    ?assertEqual(2, get(retry_test_attempt)),
    ?assertEqual([2], get(retry_test_actions)).

rollback_failure_discards_connection(Conn) ->
    meck:expect(epgsql, squery, fun
        (_, <<"ROLLBACK">>) ->
            {error, lost};
        (_, Sql) ->
            put(retry_test_status, status(Sql)),
            {ok, [], []}
    end),
    ?assertMatch(
        {error, {rollback_not_confirmed, _, _}},
        elib_pg:with_deadlock_retry_tx(fun(_) ->
            put(retry_test_attempt, 1),
            {error, deadlock()}
        end)
    ),
    ?assertEqual(1, get(retry_test_attempt)),
    ?assertEqual(1, meck:num_calls(pooler, return_member, [pgsql, Conn, fail])).

commit_unknown_never_retries(Conn) ->
    meck:expect(epgsql, squery, fun
        (_, <<"COMMIT">>) ->
            exit(lost);
        (_, Sql) ->
            put(retry_test_status, status(Sql)),
            {ok, [], []}
    end),
    ?assertMatch(
        {error, {commit_not_confirmed, _}},
        elib_pg:with_deadlock_retry_tx(fun(_) ->
            put(retry_test_attempt, 1),
            ok
        end)
    ),
    ?assertEqual(1, get(retry_test_attempt)),
    ?assertEqual(1, meck:num_calls(pooler, return_member, [pgsql, Conn, fail])).

commit_status_rollback_does_not_run_actions(Conn) ->
    put(retry_test_actions, []),
    meck:expect(epgsql, get_cmd_status, fun(_) ->
        case get(retry_test_status) of
            commit -> {ok, rollback};
            S -> {ok, S}
        end
    end),
    ?assertMatch(
        {error, {commit_not_confirmed, _}},
        elib_pg:with_deadlock_retry_tx(fun(C) ->
            elib_pg:after_commit(C, action, fun() ->
                put(retry_test_actions, [bad]),
                ok
            end),
            ok
        end)
    ),
    ?assertEqual([], get(retry_test_actions)),
    ?assertEqual(1, meck:num_calls(pooler, return_member, [pgsql, Conn, fail])).

other_sql_error_never_retries(_) ->
    put(retry_test_attempt, 0),
    Err = #error{
        severity = error,
        code = <<"23505">>,
        codename = unique_violation,
        message = <<"unique">>,
        extra = []
    },
    ?assertEqual(
        {error, Err},
        elib_pg:with_deadlock_retry_tx(fun(_) ->
            put(retry_test_attempt, get(retry_test_attempt) + 1),
            {error, Err}
        end)
    ),
    ?assertEqual(1, get(retry_test_attempt)).

after_commit_failure_never_retries(_) ->
    ?assertMatch(
        {error, {after_commit_failed, _, _}},
        elib_pg:with_deadlock_retry_tx(fun(C) ->
            put(retry_test_attempt, 1),
            elib_pg:after_commit(C, bad, fun() -> error(fail) end),
            ok
        end)
    ),
    ?assertEqual(1, get(retry_test_attempt)).

retry_is_bounded(_) ->
    put(retry_test_attempt, 0),
    ?assertEqual(
        {error, deadlock()},
        elib_pg:with_deadlock_retry_tx(fun(_) ->
            put(retry_test_attempt, get(retry_test_attempt) + 1),
            {error, deadlock()}
        end)
    ),
    ?assertEqual(3, get(retry_test_attempt)).

nested_transaction_never_takes_second_connection(_) ->
    put(elib_pg_commit_actions, {self(), #{}}),
    try
        ?assertEqual(
            {error, nested_transaction_unsupported},
            elib_pg:with_deadlock_retry_tx(fun(_) -> error(must_not_run) end)
        ),
        ?assertEqual(0, meck:num_calls(pooler, take_member, 2))
    after
        erase(elib_pg_commit_actions)
    end.

normal_transaction_inside_retry_never_runs(_) ->
    put(retry_test_attempt, 0),
    put(retry_test_inner_action, false),
    ?assertEqual(
        {error, deadlock()},
        elib_pg:with_deadlock_retry_tx(fun(_) ->
            put(retry_test_attempt, get(retry_test_attempt) + 1),
            case
                elib_pg:with_tx(fun(_) ->
                    put(retry_test_inner_action, true),
                    ok
                end)
            of
                {error, nested_transaction_unsupported} -> ok;
                Other -> error({unexpected_nested_result, Other})
            end,
            {error, deadlock()}
        end)
    ),
    ?assertEqual(3, get(retry_test_attempt)),
    ?assertEqual(false, get(retry_test_inner_action)),
    ?assertEqual(3, meck:num_calls(pooler, take_member, 2)),
    ?assertEqual(0, meck:num_calls(pooler, take_member, 1)),
    ?assertEqual(undefined, get(elib_pg_retry_tx_active)).

implicit_connection_aborts_without_checkout(_) ->
    Calls = [
        fun() -> elib_pg:with_conn(fun(_) -> error(unreachable) end) end,
        fun() -> elib_pg:query(<<"SELECT 1">>, []) end,
        fun() -> elib_pg:execute(<<"DELETE FROM a">>, []) end,
        fun() -> elib_pg:execute_count(<<"DELETE FROM a">>, []) end
    ],
    lists:foreach(
        fun(Call) ->
            Before = meck:num_calls(pooler, take_member, 2),
            ?assertEqual(
                {error, nested_connection_unsupported},
                elib_pg:with_deadlock_retry_tx(fun(_) ->
                    Call(),
                    ok
                end)
            ),
            ?assertEqual(Before + 1, meck:num_calls(pooler, take_member, 2)),
            ?assertEqual(undefined, get(elib_pg_retry_tx_active))
        end,
        Calls
    ).

after_commit_can_checkout_after_context_clears(_) ->
    ?assertEqual(
        ok,
        elib_pg:with_deadlock_retry_tx(fun(C) ->
            elib_pg:after_commit(
                C,
                action,
                fun() -> elib_pg:with_conn(fun(_) -> ok end) end
            ),
            ok
        end)
    ),
    ?assertEqual(1, meck:num_calls(pooler, take_member, 2)),
    ?assertEqual(1, meck:num_calls(pooler, take_member, 1)),
    ?assertEqual(undefined, get(elib_pg_retry_tx_active)).

active_connection_deadlock_aborts_retry(_) ->
    meck:expect(epgsql, equery, fun(_, _, _) -> {error, deadlock()} end),
    meck:expect(epgsql, parse, fun(_, _) -> {ok, statement} end),
    meck:expect(epgsql, execute_batch, fun(_, _) -> [{error, deadlock()}] end),
    lists:foreach(
        fun(Operation) ->
            put(retry_test_attempt, 0),
            ?assertEqual(
                {error, deadlock()},
                elib_pg:with_deadlock_retry_tx(fun(C) ->
                    put(retry_test_attempt, get(retry_test_attempt) + 1),
                    Operation(C),
                    error(must_not_continue)
                end)
            ),
            ?assertEqual(3, get(retry_test_attempt))
        end,
        [
            fun(C) -> elib_pg:query(C, <<"SELECT 1">>, []) end,
            fun(C) -> elib_pg:execute(C, <<"UPDATE x SET y=1">>, []) end
        ]
    ).

other_connection_and_legacy_preserve_errors(Conn) ->
    meck:expect(epgsql, equery, fun(_, _, _) -> {error, deadlock()} end),
    ?assertEqual({error, deadlock()}, elib_pg:query(Conn, <<"SELECT 1">>, [])),
    Other = spawn(fun wait/0),
    try
        ?assertEqual(
            ok,
            elib_pg:with_deadlock_retry_tx(fun(_) ->
                put(retry_other_result, elib_pg:query(Other, <<"SELECT 1">>, [])),
                ok
            end)
        ),
        ?assertEqual({error, deadlock()}, erase(retry_other_result))
    after
        Other ! stop
    end.

begin_failure_discards_and_clears_context(Conn) ->
    meck:expect(epgsql, squery, fun(_, <<"BEGIN">>) -> {error, closed} end),
    ?assertMatch(
        {error, {begin_not_confirmed, _}},
        elib_pg:with_deadlock_retry_tx(fun(_) -> error(must_not_run) end)
    ),
    ?assertEqual(1, meck:num_calls(pooler, return_member, [pgsql, Conn, fail])),
    ?assertEqual(undefined, get(elib_pg_commit_actions)),
    ?assertEqual(undefined, get(elib_pg_retry_tx_active)).

rollback_status_unknown_discards_and_clears_context(Conn) ->
    meck:expect(epgsql, get_cmd_status, fun(_) ->
        case get(retry_test_status) of
            rollback -> {error, closed};
            S -> {ok, S}
        end
    end),
    ?assertMatch(
        {error, {rollback_not_confirmed, _, _}},
        elib_pg:with_deadlock_retry_tx(fun(_) -> {error, deadlock()} end)
    ),
    ?assertEqual(1, meck:num_calls(pooler, take_member, 2)),
    ?assertEqual(1, meck:num_calls(pooler, return_member, [pgsql, Conn, fail])),
    ?assertEqual(undefined, get(elib_pg_commit_actions)),
    ?assertEqual(undefined, get(elib_pg_retry_tx_active)).

commit_eof_discards_and_clears_context(Conn) ->
    put(retry_test_actions, []),
    meck:expect(epgsql, squery, fun
        (_, <<"COMMIT">>) ->
            {error, closed};
        (_, Sql) ->
            put(retry_test_status, status(Sql)),
            {ok, [], []}
    end),
    ?assertMatch(
        {error, {commit_not_confirmed, _}},
        elib_pg:with_deadlock_retry_tx(fun(C) ->
            elib_pg:after_commit(C, action, fun() ->
                put(retry_test_actions, [bad]),
                ok
            end),
            ok
        end)
    ),
    ?assertEqual([], get(retry_test_actions)),
    ?assertEqual(1, meck:num_calls(pooler, take_member, 2)),
    ?assertEqual(1, meck:num_calls(pooler, return_member, [pgsql, Conn, fail])),
    ?assertEqual(undefined, get(elib_pg_commit_actions)),
    ?assertEqual(undefined, get(elib_pg_retry_tx_active)).

after_commit_can_start_normal_transaction(Conn) ->
    meck:expect(epgsql, with_transaction, fun(C, Fun, _) ->
        InnerResult = Fun(C),
        put(retry_test_status, commit),
        InnerResult
    end),
    ?assertEqual(
        ok,
        elib_pg:with_deadlock_retry_tx(fun(C) ->
            elib_pg:after_commit(C, action, fun() ->
                Result = elib_pg:with_tx(fun(C2) ->
                    case C2 =:= Conn of
                        true -> ok;
                        false -> error(wrong_connection)
                    end
                end),
                put(retry_inner_result, Result),
                ok
            end),
            ok
        end)
    ),
    ?assertEqual(ok, erase(retry_inner_result)),
    ?assertEqual(1, meck:num_calls(pooler, take_member, 2)),
    ?assertEqual(1, meck:num_calls(pooler, take_member, 1)),
    ?assertEqual(undefined, get(elib_pg_commit_actions)),
    ?assertEqual(undefined, get(elib_pg_retry_tx_active)).
