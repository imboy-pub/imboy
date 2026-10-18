%% Deterministic callback/timer tests; no database or distribution required.
%% Real sync/node restart and rejoin are also covered by cache_shared_dcheck.
-module(imboy_cache_rejoin_tests).
-include_lib("eunit/include/eunit.hrl").
-include("imboy_cache_sync_state.hrl").

ready_nodeup_advances_only_timer_test() ->
    Old = erlang:start_timer(60000, self(), reconcile_tick),
    S = #state{status = ready, boot_id = <<1:128>>, counter = 17,
        origin_seq = 9, reconcile_timer = Old, requires_authority = false},
    {noreply, Next} = nodeup(S),
    Ref = Next#state.reconcile_timer,
    try
        ?assertNotEqual(Old, Ref),
        ?assertEqual(false, erlang:read_timer(Old)),
        ?assertEqual(S#state{reconcile_timer = Ref}, Next),
        ?assert(erlang:read_timer(Ref) =< 1000),
        ?assertEqual({noreply, Next},
            imboy_cache_sync:handle_info({timeout, Old, reconcile_tick}, Next))
    after
        cancel(Ref), cancel(Old)
    end.

nodeup_storm_cannot_postpone_tick_test() ->
    with_meta(fun() ->
        {noreply, First} = nodeup(#state{status = ready}),
        Ref = First#state.reconcile_timer,
        try
            Final = lists:foldl(fun(_, S) ->
                {noreply, Next} = nodeup(S),
                ?assertEqual(Ref, Next#state.reconcile_timer),
                Next
            end, First, lists:seq(1, 10000)),
            %% No timing-sensitive sleep: await the actual deadline message.
            Tick = receive
                {timeout, Ref, reconcile_tick} = Message -> Message
            after 2000 -> error(nodeup_storm_starved_timer)
            end,
            {noreply, Periodic} = imboy_cache_sync:handle_info(Tick, Final),
            PeriodicRef = Periodic#state.reconcile_timer,
            try
                ?assertNotEqual(Ref, PeriodicRef),
                ?assertEqual(ready, Periodic#state.status),
                ?assert(erlang:read_timer(PeriodicRef) > 1000),
                ?assert(erlang:read_timer(PeriodicRef) =< ?RECONCILE_INTERVAL_MS),
                ?assertEqual({noreply, Periodic},
                    imboy_cache_sync:handle_info(Tick, Periodic))
            after cancel(PeriodicRef) end
        after cancel(Ref) end
    end).

sooner_timer_is_not_postponed_test() ->
    Ref = erlang:start_timer(500, self(), reconcile_tick),
    S = #state{status = ready, reconcile_timer = Ref},
    try
        ?assertEqual({noreply, S}, nodeup(S))
    after cancel(Ref) end.

queued_tick_is_not_replaced_test() ->
    with_meta(fun() ->
        Ref = erlang:start_timer(0, self(), reconcile_tick),
        Tick = receive
            {timeout, Ref, reconcile_tick} = Message -> Message
        after 1000 -> error(timer_not_delivered)
        end,
        S = #state{status = ready, reconcile_timer = Ref},
        ?assertEqual(false, erlang:read_timer(Ref)),
        ?assertEqual({noreply, S}, nodeup(S)),
        {noreply, Next} = imboy_cache_sync:handle_info(Tick, S),
        try
            ?assertEqual(ready, Next#state.status),
            ?assert(erlang:read_timer(Next#state.reconcile_timer) > 1000)
        after cancel(Next#state.reconcile_timer) end
    end).

nonready_nodeup_preserves_state_test() ->
    Ref = erlang:start_timer(60000, self(), reconcile_tick),
    try
        lists:foreach(fun(Status) ->
            S = #state{status = Status, reconcile_timer = Ref,
                recovery_timer = make_ref(), requires_authority = true,
                identity_recovery = true, snapshot_round = #{sentinel => preserved}},
            ?assertEqual({noreply, S}, nodeup(S)),
            ?assertEqual({noreply, S#state{reconcile_timer = undefined}},
                nodeup(S#state{reconcile_timer = undefined}))
        end, [starting, discovering, recovering, degraded]),
        ?assert(is_integer(erlang:read_timer(Ref)))
    after cancel(Ref) end.

timer_owner_crash_does_not_leak_to_replacement_test() ->
    %% Exercise the actual callback in separate timer-owning processes. Real
    %% supervised sync boot/snapshot recovery is a distributed-driver concern.
    Parent = self(),
    {Pid, Monitor} = spawn_monitor(fun() ->
        {noreply, S} = nodeup(#state{status = ready}),
        Parent ! {owned_timer, self(), S#state.reconcile_timer},
        receive stop -> ok end
    end),
    Old = receive {owned_timer, Pid, R} -> R after 1000 -> error(owner_timeout) end,
    exit(Pid, kill),
    receive {'DOWN', Monitor, process, Pid, killed} -> ok after 1000 -> error(crash_timeout) end,
    ?assertEqual(false, erlang:read_timer(Old)),
    Restarting = #state{status = discovering, boot_id = <<2:128>>},
    ?assertEqual({noreply, Restarting}, nodeup(Restarting)),
    ?assertEqual({noreply, Restarting},
        imboy_cache_sync:handle_info({timeout, Old, reconcile_tick}, Restarting)),
    {noreply, Ready} = nodeup(Restarting#state{status = ready}),
    try
        ?assertNotEqual(Old, Ready#state.reconcile_timer),
        ?assertEqual({noreply, Ready},
            imboy_cache_sync:handle_info({timeout, Old, reconcile_tick}, Ready))
    after cancel(Ready#state.reconcile_timer) end.

nodeup(S) -> imboy_cache_sync:handle_info({nodeup, 'peer@synthetic'}, S).
cancel(Ref) ->
    _ = erlang:cancel_timer(Ref),
    receive {timeout, Ref, reconcile_tick} -> ok after 0 -> ok end.

%% Empty real metadata lets the ordinary READY reconciliation run unchanged.
%% No mock of the timer, reconciliation callback or recovery authorization.
with_meta(Fun) ->
    undefined = ets:whereis(imboy_cache_meta),
    Saved = application:get_env(imboy, cluster_nodes),
    application:set_env(imboy, cluster_nodes, []),
    imboy_cache_meta = ets:new(imboy_cache_meta, [named_table, public, set]),
    try Fun()
    after
        ets:delete(imboy_cache_meta),
        case Saved of
            undefined -> application:unset_env(imboy, cluster_nodes);
            {ok, V} -> application:set_env(imboy, cluster_nodes, V)
        end
    end.
