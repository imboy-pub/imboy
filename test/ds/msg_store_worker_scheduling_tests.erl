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

group_fanout_keeps_per_device_ack_and_budget_test() ->
    pure_mock_fixture:run(fun() ->
        ok = imboy_syn:init(),
        ok = ack_retry_cache:init_table(),
        ok = application:set_env(imboy, delivery_budget_max_count, 8),
        ok = application:set_env(imboy, delivery_budget_reserve_count, 3),
        Devices = [{Uid, spawn(fun hold_device/0)} || Uid <- [1001, 1002, 1003]],
        [{1001, Fast}, {1002, Slow}, {1003, Acked}] = Devices,
        lists:foreach(
            fun({Uid, Pid}) -> ok = imboy_syn:join(Uid, <<"android">>, Pid, <<"device">>) end,
            Devices
        ),
        try
            Id = <<"group-budget-single-hop">>,
            Frame = jsone:encode(#{
                <<"type">> => <<"C2G">>,
                <<"id">> => Id,
                <<"e2ee">> => #{<<"ciphertext">> => <<"opaque">>}
            }),
            [Slow ! {pending, N} || N <- lists:seq(1, 5)],
            ok = ack_retry_cache:set({ack_received, 1003, <<"device">>, Id}, true, 5000),
            ok = message_ds:send_group_next([1001, 1002, 1003, 1004], Id, Frame, [0]),
            ?assertMatch({messages, [{timeout, _, Frame}]}, process_info(Fast, messages)),
            {messages, SlowMessages} = process_info(Slow, messages),
            [Notice] = [jsone:decode(Msg) || {timeout, _, Msg} <- SlowMessages],
            ?assertMatch(#{<<"action">> := <<"pull_offline_msg">>}, Notice),
            ?assertEqual(null, maps:get(<<"e2ee">>, Notice)),
            ?assertEqual({messages, []}, process_info(Acked, messages)),
            ?assertMatch(
                #{bulk_delivered := 1, bulk_deferred := 1, notice_sent := 1},
                imboy_syn:delivery_budget_counters()
            )
        after
            [Pid ! stop || {_Uid, Pid} <- Devices]
        end
    end).

hold_device() ->
    receive
        stop -> ok
    end.

group_fanout_reroutes_a_lost_pid_to_the_exact_device_once_test() ->
    pure_mock_fixture:run(fun() ->
        ok = imboy_syn:init(),
        ok = ack_retry_cache:init_table(),
        {Stale, Ref} = spawn_monitor(fun() -> ok end),
        receive
            {'DOWN', Ref, process, Stale, normal} -> ok
        after 1000 ->
            error(stale_device_not_stopped)
        end,
        Current = spawn(fun hold_device/0),
        Other = spawn(fun hold_device/0),
        try
            Did = <<"moved-device">>,
            OtherDid = <<"other-device">>,
            Id = <<"group-after-route-move">>,
            Frame = <<"{\"type\":\"C2G\",\"id\":\"group-after-route-move\"}">>,
            ok = imboy_syn:join(1001, <<"android">>, Current, Did),
            ok = imboy_syn:join(1001, <<"android">>, Other, OtherDid),
            ok = message_ds:deliver_group_batch(Id, Frame, [{Stale, 1001, Did, true}]),
            ok = ack_retry_cache:set({ack_received, 1001, Did, Id}, true, 5000),
            ok = message_ds:deliver_group_batch(Id, Frame, [
                {Stale, 1001, Did, true},
                {Stale, 1001, <<"still-offline">>, true}
            ]),
            OverlapId = <<"group-during-route-move">>,
            OverlapFrame = <<"{\"type\":\"C2G\",\"id\":\"group-during-route-move\"}">>,
            ok = message_ds:deliver_group_batch(OverlapId, OverlapFrame, [
                {Stale, 1001, Did, false},
                {Current, 1001, Did, false}
            ]),
            ?assertMatch(
                {messages, [{timeout, _, Frame}, {timeout, _, OverlapFrame}]},
                process_info(Current, messages)
            ),
            ?assertEqual({messages, []}, process_info(Other, messages)),
            ?assertMatch(#{bulk_delivered := 2}, imboy_syn:delivery_budget_counters())
        after
            Current ! stop,
            Other ! stop
        end
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

materialized_c2g_archives_on_retry_before_completing_under_current_lease_test() ->
    pure_mock_fixture:run(fun() ->
        with_worker(fun(_, _) -> {ok, []} end, fun(_) ->
            application:set_env(imboy, msg_archive_enabled, true),
            meck:new(msg_archive_repo, [passthrough, no_link]),
            try
                exercise_materialized_archive_retry()
            after
                meck:unload(msg_archive_repo),
                application:unset_env(imboy, msg_archive_enabled)
            end
        end)
    end).

exercise_materialized_archive_retry() ->
    MsgId = <<"materialized-c2g">>,
    Type = <<"c2g">>,
    Row = #{
        <<"id">> => 123,
        <<"type">> => Type,
        <<"msg_id">> => MsgId,
        <<"c2g_materialized">> => true
    },
    meck:expect(msg_store_repo, renew_lease, fun
        (123, <<"claim-one">>, _) -> {ok, <<"renewed-one">>};
        (123, <<"claim-two">>, _) -> {ok, <<"renewed-two">>};
        (123, <<"stale">>, _) -> {ok, 0}
    end),
    meck:expect(msg_archive_repo, archive, fun
        (#{<<"lease_token">> := <<"renewed-one">>}) -> {error, synthetic_storage_failure};
        (#{<<"lease_token">> := <<"renewed-two">>}) -> ok
    end),
    meck:expect(msg_store_repo, mark_failed, fun(Type0, MsgId0, Reason, _, Token) ->
        ?assertEqual({Type, MsgId, <<"renewed-one">>}, {Type0, MsgId0, Token}),
        ?assertNotEqual(nomatch, binary:match(Reason, <<"synthetic_storage_failure">>)),
        {ok, 1}
    end),
    meck:expect(msg_store_repo, mark_processed, fun(_, Type0, MsgId0, Token) ->
        ?assertEqual({Type, MsgId, <<"renewed-two">>}, {Type0, MsgId0, Token}),
        {ok, 1}
    end),
    _ = msg_store_worker:process_row(Row#{<<"lease_token">> => <<"claim-one">>}),
    ?assertEqual(1, meck:num_calls(msg_store_repo, mark_failed, 5)),
    ?assertEqual(0, meck:num_calls(msg_store_repo, mark_processed, 4)),
    ok = msg_store_worker:process_row(Row#{<<"lease_token">> => <<"stale">>}),
    ?assertEqual(1, meck:num_calls(msg_archive_repo, archive, 1)),
    _ = msg_store_worker:process_row(Row#{<<"lease_token">> => <<"claim-two">>}),
    ?assertEqual(2, meck:num_calls(msg_archive_repo, archive, 1)),
    ?assertEqual(1, meck:num_calls(msg_store_repo, mark_processed, 4)),
    ?assertEqual(1, meck:num_calls(msg_store_repo, mark_failed, 5)).

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
