-module(elib_pg_acquisition_pg_checks).
-export([run/1]).

%% Run only in a disposable VM/database with no existing pgsql pool.
run(ConnOpts) ->
    {ok, _} = application:ensure_all_started(pooler),
    ok = application:set_env(imboy, sql_driver, pgsql),
    {ok, _} = pooler:new_pool(#{
        name => pgsql,
        init_count => 1,
        max_count => 1,
        queue_max => 10,
        start_mfa => {epgsql, connect, [ConnOpts]}
    }),
    Count = atomics:new(1, []),
    try
        waiting_transaction(Count),
        exhausted_transaction(Count),
        {rollback, synthetic_rollback} = elib_pg:with_tx(fun(_Conn) ->
            atomics:add(Count, 1, 1),
            throw({rollback, synthetic_rollback})
        end),
        2 = atomics:get(Count, 1),
        0 = proplists:get_value(in_use_count, pooler:pool_utilization(pgsql)),
        io:format("PG_ACQUISITION_ROLLBACK_ONCE=PASS~n"),
        ok
    after
        pooler:rm_pool(pgsql)
    end.

waiting_transaction(Count) ->
    Held = pooler:take_member(pgsql, 1000),
    true = is_pid(Held),
    Parent = self(),
    {Pid, Ref} = spawn_monitor(fun() ->
        Result = elib_pg:with_tx(fun(Conn) ->
            atomics:add(Count, 1, 1),
            epgsql:equery(Conn, "SELECT 1", [])
        end),
        Parent ! {transaction_result, self(), Result}
    end),
    try
        wait_for_queue(50),
        0 = atomics:get(Count, 1)
    after
        pooler:return_member(pgsql, Held)
    end,
    receive
        {transaction_result, Pid, {ok, _, [{1}]}} -> ok
    after 3000 -> error(waiting_transaction_failed)
    end,
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok
    after 1000 -> error(waiting_transaction_worker_failed)
    end,
    1 = atomics:get(Count, 1),
    io:format("PG_ACQUISITION_QUEUED_RELEASE_ONCE=PASS~n").

exhausted_transaction(Count) ->
    Held = pooler:take_member(pgsql, 1000),
    true = is_pid(Held),
    Start = erlang:monotonic_time(millisecond),
    try
        {error, no_connection} = elib_pg:with_tx(fun(_Conn) ->
            atomics:add(Count, 1, 1),
            error(transaction_must_not_run)
        end),
        Elapsed = erlang:monotonic_time(millisecond) - Start,
        true = Elapsed >= 900 andalso Elapsed < 3000,
        1 = atomics:get(Count, 1),
        0 = proplists:get_value(queued_count, pooler:pool_utilization(pgsql)),
        io:format("PG_ACQUISITION_TIMEOUT_BODY_ZERO=PASS~n")
    after
        pooler:return_member(pgsql, Held)
    end.

wait_for_queue(0) ->
    error(transaction_not_queued);
wait_for_queue(Attempts) ->
    case proplists:get_value(queued_count, pooler:pool_utilization(pgsql)) of
        1 ->
            ok;
        _ ->
            timer:sleep(10),
            wait_for_queue(Attempts - 1)
    end.
