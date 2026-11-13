-module(group_event_subscription_tests).
-include_lib("eunit/include/eunit.hrl").

subscription_test_() ->
    [
        {timeout, 20, ?_test(pure_mock_fixture:run(Case))}
     || Case <- [
            fun legacy_attach_is_idempotent/0,
            fun manager_restart_restores_subscription/0,
            fun handler_failure_restores_subscription/0,
            fun owner_restart_removes_old_subscription/0,
            fun delayed_manager_start_is_recovered/0,
            fun failed_best_effort_publish_is_counted/0,
            fun probe_suite_does_not_invoke_group_business/0,
            fun blocked_manager_does_not_block_owner_start/0,
            fun owner_shutdown_cleans_up_late_registration/0
        ]
    ].

legacy_attach_is_idempotent() ->
    Bus = start_bus(),
    try
        ?assertNot(group_event_subscription:ready()),
        ok = group_event_handler:attach(),
        ok = group_event_handler:attach(),
        ?assertEqual([group_event_handler], gen_event:which_handlers(Bus))
    after
        stop(Bus)
    end.

manager_restart_restores_subscription() ->
    Bus = start_bus(),
    Owner = start_owner(),
    try
        await_ready(true),
        Ref = monitor(process, Bus),
        exit(Bus, kill),
        receive
            {'DOWN', Ref, process, Bus, killed} -> ok
        after 1000 -> error(manager_not_stopped)
        end,
        await_ready(false),
        NewBus = start_bus(),
        try
            await_ready(true),
            ?assert(is_process_alive(Owner)),
            ?assertEqual([group_event_handler], gen_event:which_handlers(NewBus)),
            [ok = group_event_handler:attach() || _ <- lists:seq(1, 10)],
            ?assertEqual([group_event_handler], gen_event:which_handlers(NewBus))
        after
            stop(NewBus)
        end
    after
        stop(Owner),
        stop(Bus)
    end.

handler_failure_restores_subscription() ->
    Parent = self(),
    meck:new(group_event_handler, [no_link, passthrough]),
    meck:expect(group_event_handler, init, fun(_) ->
        Parent ! initialized,
        {ok, #{}}
    end),
    meck:expect(group_event_handler, handle_event, fun
        (subscription_failure, _) -> error(synthetic_handler_failure);
        (_, State) -> {ok, State}
    end),
    Bus = start_bus(),
    Owner = start_owner(),
    try
        receive
            initialized -> ok
        after 1000 -> error(initial_subscription_missing)
        end,
        ok = gen_event:notify(Bus, subscription_failure),
        receive
            initialized -> ok
        after 2500 -> error(subscription_not_restored)
        end,
        await_ready(true),
        ?assertEqual([group_event_handler], gen_event:which_handlers(Bus))
    after
        stop(Owner),
        stop(Bus),
        meck:unload(group_event_handler)
    end.

owner_restart_removes_old_subscription() ->
    Bus = start_bus(),
    Owner = start_owner(),
    try
        await_ready(true),
        stop(Owner),
        await(fun() -> gen_event:which_handlers(Bus) =:= [] end),
        Next = start_owner(),
        try
            await_ready(true),
            ?assertEqual([group_event_handler], gen_event:which_handlers(Bus))
        after
            stop(Next)
        end
    after
        stop(Owner),
        stop(Bus)
    end.

delayed_manager_start_is_recovered() ->
    Owner = start_owner(),
    try
        ?assertNot(group_event_subscription:ready()),
        ?assertEqual({error, not_ready}, group_event_handler:attach()),
        Bus = start_bus(),
        try
            await_ready(true)
        after
            stop(Bus)
        end
    after
        stop(Owner)
    end.

failed_best_effort_publish_is_counted() ->
    ?assertEqual(undefined, whereis(imboy_domain_event)),
    meck:new(elib_metric, [no_link, passthrough]),
    meck:expect(elib_metric, increment, fun(domain_event_publish_failed) -> ok end),
    try
        ?assertEqual(ok, imboy_domain_event:publish([{best_effort_probe, 10}])),
        ?assertEqual(1, meck:num_calls(elib_metric, increment, [domain_event_publish_failed])),
        meck:expect(elib_metric, increment, fun(_) -> error(synthetic_metric_failure) end),
        ?assertEqual(ok, imboy_domain_event:publish([{best_effort_probe, 11}]))
    after
        meck:unload(elib_metric)
    end.

probe_suite_does_not_invoke_group_business() ->
    meck:new(group_ds, [no_link, passthrough]),
    meck:new(eunit_runner, [no_link, non_strict]),
    meck:expect(group_ds, member_uids, fun(_) -> error(probe_invoked_group_business) end),
    Bus = start_bus(),
    Owner = start_owner(),
    meck:expect(eunit_runner, ensure_named_server, fun(imboy_domain_event) -> {ok, Bus} end),
    meck:expect(eunit_runner, eunit_setup, fun() -> error(unexpected_application_boot) end),
    try
        ?assertNot(lists:keymember(imboy, 1, application:which_applications())),
        await_ready(true),
        Tests = imboy_domain_event_tests:bus_test_(),
        ?assertMatch({foreach, _, _, [_ | _]}, Tests),
        ?assertEqual(ok, eunit:test(Tests, [verbose])),
        ?assertEqual(0, meck:num_calls(group_ds, member_uids, 1)),
        ?assertNot(lists:keymember(imboy, 1, application:which_applications()))
    after
        stop(Owner),
        stop(Bus),
        meck:unload(group_ds),
        meck:unload(eunit_runner)
    end.

blocked_manager_does_not_block_owner_start() ->
    Bus = start_bus(),
    ok = sys:suspend(Bus),
    {ok, Timer} = timer:apply_after(3000, sys, resume, [Bus]),
    Started = erlang:monotonic_time(millisecond),
    try
        Owner = start_owner(),
        try
            ?assert(erlang:monotonic_time(millisecond) - Started < 1000),
            ?assertNot(group_event_subscription:ready()),
            timer:sleep(1200),
            ?assertNot(group_event_subscription:ready()),
            ok = sys:resume(Bus),
            await_ready(true),
            ?assertEqual([group_event_handler], gen_event:which_handlers(Bus))
        after
            stop(Owner)
        end
    after
        timer:cancel(Timer),
        resume(Bus),
        stop(Bus)
    end.

owner_shutdown_cleans_up_late_registration() ->
    Bus = start_bus(),
    ok = sys:suspend(Bus),
    Owner = start_owner(),
    try
        timer:sleep(1200),
        stop(Owner),
        ok = sys:resume(Bus),
        await(fun() -> gen_event:which_handlers(Bus) =:= [] end)
    after
        resume(Bus),
        stop(Owner),
        stop(Bus)
    end.

start_bus() ->
    {ok, Pid} = imboy_domain_event:start_link(),
    unlink(Pid),
    Pid.

start_owner() ->
    {ok, Pid} = group_event_subscription:start_link(),
    unlink(Pid),
    Pid.

await_ready(Expected) -> await(fun() -> group_event_subscription:ready() =:= Expected end).

await(Check) -> await(Check, erlang:monotonic_time(millisecond) + 2500).
await(Check, Deadline) ->
    case Check() of
        true ->
            ok;
        false ->
            case erlang:monotonic_time(millisecond) < Deadline of
                true ->
                    timer:sleep(10),
                    await(Check, Deadline);
                false ->
                    error(subscription_deadline_exceeded)
            end
    end.

stop(Pid) ->
    case is_process_alive(Pid) of
        true ->
            try
                case whereis(imboy_domain_event) =:= Pid of
                    true -> gen_event:stop(Pid, normal, 1000);
                    false -> gen_server:stop(Pid, normal, 1000)
                end
            catch
                exit:_ -> ok
            end;
        false ->
            ok
    end.

resume(Bus) ->
    try
        sys:resume(Bus)
    catch
        exit:_ -> ok
    end.
