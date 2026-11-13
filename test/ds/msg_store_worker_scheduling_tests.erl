-module(msg_store_worker_scheduling_tests).
-include_lib("eunit/include/eunit.hrl").

burst_kicks_are_coalesced_test() ->
    pure_mock_fixture:run(fun() ->
        Count = atomics:new(1, []),
        with_worker(
            fun(_, _) ->
                atomics:add(Count, 1, 1),
                {ok, []}
            end,
            fun(Pid) ->
                ok = sys:suspend(Pid),
                [gen_statem:cast(Pid, kick) || _ <- lists:seq(1, 1000)],
                Started = erlang:monotonic_time(microsecond),
                ok = sys:resume(Pid),
                ok = await_idle(Pid, erlang:monotonic_time(millisecond) + 3000),
                Calls = atomics:get(Count, 1),
                io:format(user, "KICK_BURST kicks=1000 claims=~p elapsed_us=~p~n", [
                    Calls, erlang:monotonic_time(microsecond) - Started
                ]),
                ?assertEqual(1, Calls)
            end
        )
    end).

full_batches_yield_to_system_messages_test() ->
    pure_mock_fixture:run(fun() ->
        Parent = self(),
        Busy = atomics:new(1, []),
        atomics:put(Busy, 1, 1),
        Rows = lists:duplicate(100, #{
            <<"type">> => <<"synthetic_unknown">>,
            <<"msg_id">> => <<"synthetic-row">>
        }),
        with_worker(
            fun(_, _) ->
                Parent ! batch_started,
                case atomics:get(Busy, 1) of
                    1 -> {ok, Rows};
                    0 -> {ok, []}
                end
            end,
            fun(Pid) ->
                gen_statem:cast(Pid, kick),
                receive
                    batch_started -> ok
                after 1000 -> error(batch_not_started)
                end,
                Result =
                    try
                        sys:get_state(Pid, 100)
                    catch
                        Class:Reason -> {Class, Reason}
                    end,
                atomics:put(Busy, 1, 0),
                ?assertMatch({draining, _}, Result)
            end
        )
    end).

kick_during_last_batch_is_not_lost_test() ->
    pure_mock_fixture:run(fun() ->
        Parent = self(),
        Calls = atomics:new(1, []),
        with_worker(
            fun(_, _) ->
                case atomics:add_get(Calls, 1, 1) of
                    1 ->
                        Parent ! {claim_started, self()},
                        receive
                            finish_claim -> {ok, []}
                        after 1000 -> error(claim_stuck)
                        end;
                    _ ->
                        {ok, []}
                end
            end,
            fun(Pid) ->
                gen_statem:cast(Pid, kick),
                receive
                    {claim_started, Pid} -> ok
                after 1000 -> error(no_claim)
                end,
                gen_statem:cast(Pid, kick),
                Pid ! finish_claim,
                ok = await_idle(Pid, erlang:monotonic_time(millisecond) + 3000),
                ?assertEqual(2, atomics:get(Calls, 1))
            end
        )
    end).

staging_metrics_test_() ->
    [{atom_to_list(Mode), fun() -> staging_metrics(Mode) end} || Mode <- [success, failure]].

staging_metrics(Mode) ->
    pure_mock_fixture:run(fun() ->
        with_worker(fun(_, _) -> {ok, []} end, fun(Pid) ->
            {ok, MetricPid} = elib_metric:start_link(),
            try
                meck:expect(msg_store_repo, get_staging_stats, fun(_) ->
                    case Mode of
                        success ->
                            {ok, [
                                #{
                                    <<"pending">> => 42,
                                    <<"oldest_pending_seconds">> => 7
                                }
                            ]};
                        failure ->
                            {error, timeout}
                    end
                end),
                gen_statem:cast(Pid, kick),
                ok = await_idle(Pid, erlang:monotonic_time(millisecond) + 3000),
                #{gauges := G, histograms := H} = elib_metric:get_all_metrics(),
                Expected =
                    case Mode of
                        success -> {42, 7};
                        failure -> {-1, -1}
                    end,
                ?assertEqual(Expected, {
                    maps:get(imboy_msg_staging_pending, G),
                    maps:get(imboy_msg_staging_oldest_seconds, G)
                }),
                ?assertEqual(0, maps:get(imboy_msg_store_mailbox, G)),
                ?assertMatch(#{count := 1}, maps:get(imboy_msg_store_batch_seconds, H)),
                ?assertMatch(#{count := 1}, maps:get(imboy_msg_store_claim_seconds, H)),
                gen_statem:cast(Pid, kick),
                ok = await_idle(Pid, erlang:monotonic_time(millisecond) + 3000),
                ?assertEqual(1, meck:num_calls(msg_store_repo, get_staging_stats, 1))
            after
                gen_server:stop(MetricPid)
            end
        end)
    end).

code_change_preserves_existing_timer_test() ->
    Timer = make_ref(),
    {ok, idle, State} = msg_store_worker:code_change(old, idle, {state, Timer}, []),
    ?assertEqual(Timer, element(2, State)),
    ?assert(tuple_size(State) > 2).

with_worker(Claim, Test) ->
    application:set_env(imboy, msg_store_worker_tick_ms, 0),
    meck:new(msg_store_repo, [passthrough, no_link, non_strict]),
    meck:expect(msg_store_repo, claim_pending, Claim),
    meck:expect(msg_store_repo, mark_terminal, fun(_, _, _) -> {ok, 1} end),
    meck:expect(msg_store_repo, get_staging_stats, fun(_) ->
        {ok, [#{<<"pending">> => 0, <<"oldest_pending_seconds">> => 0}]}
    end),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, with_tx, fun(Fun) -> Fun(self()) end),
    meck:expect(elib_pg, query, fun(_, <<"SELECT set_config", _/binary>>, _) -> {ok, []} end),
    meck:new(elib_log, [passthrough, no_link]),
    meck:expect(elib_log, internal_log, fun(_, _, _, _) -> ok end),
    meck:expect(elib_log, internal_log, fun(_, _, _, _, _) -> ok end),
    {ok, Pid} = msg_store_worker:start_link(),
    unlink(Pid),
    try
        Test(Pid)
    after
        Ref = monitor(process, Pid),
        exit(Pid, kill),
        receive
            {'DOWN', Ref, process, Pid, _} -> ok
        after 1000 -> error(worker_not_stopped)
        end,
        meck:unload(elib_log),
        meck:unload(elib_pg),
        meck:unload(msg_store_repo)
    end.

await_idle(Pid, Deadline) ->
    case {sys:get_state(Pid, 1000), process_info(Pid, message_queue_len)} of
        {{idle, _}, {message_queue_len, 0}} ->
            ok;
        _ ->
            case erlang:monotonic_time(millisecond) < Deadline of
                true ->
                    timer:sleep(1),
                    await_idle(Pid, Deadline);
                false ->
                    error(worker_not_idle)
            end
    end.
