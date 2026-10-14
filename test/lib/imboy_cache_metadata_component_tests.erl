%%% 使用真实 depcache/sync 验证字节预算拒绝及冷恢复清值。
-module(imboy_cache_metadata_component_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("imboy_cache_sync_state.hrl").
recovery_completion_rejects_loader_started_during_recovery_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_budget(#{}, fun() ->
            OldFenced = application:get_env(imboy, cache_fenced_namespaces),
            application:set_env(imboy, cache_fenced_namespaces, [group]),
            Key = {group, recovery_loader},
            try
                {Peer, R} = begin_completion_round(),
                assert_completion_loader(Key, Peer, R)
            after
                restore(cache_fenced_namespaces, OldFenced)
            end
        end)
    end).
expired_floor_pages_converge_in_both_orders_in_real_receiver_test_() ->
    ?TEST_WITH_APP(fun() ->
        lists:foreach(
            fun(Order) ->
                with_budget(#{}, fun() ->
                    assert_expired_floor_page_order(Order)
                end)
            end,
            [floor_first, floor_last, transit_past, transit_skew]
        )
    end).
assert_expired_floor_page_order(Order) ->
    A = 'expired_page_source@synthetic',
    B = 'older_page_source@synthetic',
    {A, R0} = begin_completion_round(A),
    imboy_cache_source_cut_tests:fixture_identity(B, <<7:128>>, imboy_cache_sync_proto:epoch()),
    R = requested_peer(imboy_cache_sync_snapshot:expect_peer(R0, B), B),
    sys:replace_state(imboy_cache_sync, fun(S) -> S#state{snapshot_round = R} end),
    S = sys:get_state(imboy_cache_sync),
    C = S#state.counter,
    Round = maps:get(round, R),
    K = {cache_v2_test_rep, transferred_floor},
    Exp =
        case Order of
            transit_skew -> erlang:system_time(millisecond) + 4000;
            _ -> erlang:system_time(millisecond) - 1
        end,
    Floor =
        case Order of
            transit_past -> {K, newest, Exp, {C + 20, A}};
            transit_skew -> {K, newest, Exp, {C + 20, A}};
            _ -> {expired, K, undefined, Exp, {C + 20, A}}
        end,
    Live = {K, stale, erlang:system_time(millisecond) + 60000, {C + 10, B}},
    Ordered =
        case Order of
            floor_first -> [{A, Floor}, {B, Live}];
            _ -> [{B, Live}, {A, Floor}]
        end,
    [
        begin
            D = imboy_cache_sync_snapshot:page_digest(Round, 1, Entry),
            whereis(imboy_cache_sync) ! {cache_snapshot_page, Peer, Round, 1, Entry, D, false},
            pong = gen_server:call(imboy_cache_sync, ping_ok)
        end
     || {Peer, E} <- Ordered, Entry <- [[{with_origin, {Peer, <<7:128>>, 1}, E}]]
    ],
    ets:insert(imboy_cache_meta, [{{seq, {A, <<7:128>>}}, 1}, {{seq, {B, <<7:128>>}}, 1}]),
    finish_floor_sources([{A, C + 20}, {B, C + 10}], Round),
    pong = gen_server:call(imboy_cache_sync, ping_ok),
    ?assertEqual(ready, imboy_cache_sync:status()),
    ?assertEqual({C + 20, A}, imboy_cache_sync:current_rev(K)),
    ?assertEqual(undefined, depcache:get(K, imboy_cache)),
    ?assertEqual(undefined, imboy_cache:get(K)).
finish_floor_sources(Sources, Round) ->
    [
        begin
            whereis(imboy_cache_sync) !
                {cache_snapshot_cut, Peer, Round, <<7:128>>, imboy_cache_sync_proto:epoch(), 1,
                    imboy_cache_sync_proto:config_hash(), imboy_cache_sync_proto:registry_hash(),
                    {ok, []}, source_fixture_certificate(Peer)},
            whereis(imboy_cache_sync) ! {cache_snapshot_done, Peer, Round, Max, 1}
        end
     || {Peer, Max} <- Sources
    ],
    ok.
expired_remote_set_retains_floor_in_real_receiver_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_budget(#{}, fun() ->
            lists:foreach(fun assert_expired_remote_set/1, [past, skew])
        end)
    end).
assert_expired_remote_set(Kind) ->
    K = {cache_v2_test_rep, {expired_delta, Kind}},
    ok = imboy_cache:put(K, old, 60, replicate),
    C = (sys:get_state(imboy_cache_sync))#state.counter,
    Peer =
        case Kind of
            past -> 'expired_delta@synthetic';
            skew -> 'skew_delta@synthetic'
        end,
    Boot = <<9:128>>,
    ets:insert(imboy_cache_meta, {{peer_incarnation, Peer}, Boot}),
    Exp =
        erlang:system_time(millisecond) +
            case Kind of
                past -> -1;
                skew -> 4000
            end,
    E = (imboy_cache_sync_proto:event(set, K, newest, Exp, {C + 20, Peer}, 1, Boot))#{
        origin_node => Peer
    },
    whereis(imboy_cache_sync) ! {cache_sync, E},
    pong = gen_server:call(imboy_cache_sync, ping_ok),
    ?assertMatch([{_, {{_, Peer}, _, Exp, live, _}}], ets:lookup(imboy_cache_meta, {rev, K})),
    ?assertEqual({C + 20, Peer}, imboy_cache_sync:current_rev(K)),
    ?assertEqual(undefined, depcache:get(K, imboy_cache)),
    ?assertEqual([{{seq, {Peer, Boot}}, 1}], ets:lookup(imboy_cache_meta, {seq, {Peer, Boot}})),
    whereis(imboy_cache_sync) ! {cache_sync, E},
    pong = gen_server:call(imboy_cache_sync, ping_ok),
    ?assertMatch([{_, {{_, Peer}, _, Exp, live, _}}], ets:lookup(imboy_cache_meta, {rev, K})),
    ?assertEqual(ready, imboy_cache_sync:status()),
    Older = E#{
        revision => {C + 10, Peer},
        origin_seq => 2,
        expires_at_ms => erlang:system_time(millisecond) + 60000,
        value => stale
    },
    whereis(imboy_cache_sync) ! {cache_sync, Older},
    pong = gen_server:call(imboy_cache_sync, ping_ok),
    ?assertEqual({C + 20, Peer}, imboy_cache_sync:current_rev(K)),
    ?assertEqual(undefined, depcache:get(K, imboy_cache)).
over_window_remote_set_cannot_mutate_real_cache_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_budget(#{}, fun() ->
            K = {cache_v2_test_rep, over_window_delta},
            ok = imboy_cache:put(K, current, 60, replicate),
            Before = ets:lookup(imboy_cache_meta, {rev, K}),
            Peer = 'expired_delta@synthetic',
            Incarnation = ets:lookup(imboy_cache_meta, {peer_incarnation, Peer}),
            Seq = ets:lookup(imboy_cache_meta, {seq, {Peer, <<9:128>>}}),
            Exp =
                erlang:system_time(millisecond) -
                    imboy_cache_policy:tombstone_ttl_sec() * 1000 - 1,
            C = (sys:get_state(imboy_cache_sync))#state.counter,
            E = (imboy_cache_sync_proto:event(set, K, old, Exp, {C + 20, Peer}, 1, <<9:128>>))#{
                origin_node => Peer
            },
            ?assertEqual({error, expired}, imboy_cache_sync_proto:validate(E)),
            whereis(imboy_cache_sync) ! {cache_sync, E},
            pong = gen_server:call(imboy_cache_sync, ping_ok),
            ?assertEqual(Before, ets:lookup(imboy_cache_meta, {rev, K})),
            ?assertEqual(Incarnation, ets:lookup(imboy_cache_meta, {peer_incarnation, Peer})),
            ?assertEqual(Seq, ets:lookup(imboy_cache_meta, {seq, {Peer, <<9:128>>}})),
            ?assertEqual({ok, current}, depcache:get(K, imboy_cache))
        end)
    end).
expired_live_floor_rejects_old_write_with_real_cache_and_sync_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_budget(#{}, fun() ->
            K = {cache_v2_test_rep, expired_revision_floor},
            ?assertEqual(ok, imboy_cache:put(K, newest, 1, replicate)),
            [{_, {Rev = {Counter, _}, _, Exp, live, _}}] = ets:lookup(imboy_cache_meta, {rev, K}),
            timer:sleep(erlang:max(1, Exp - erlang:system_time(millisecond) + 20)),
            ok = depcache:flush(K, imboy_cache),
            sys:replace_state(imboy_cache_sync, fun(S) ->
                ok = imboy_cache_sync_recover:bounded_sweep(),
                S
            end),
            Peer = 'expired_floor@synthetic',
            Boot = <<6:128>>,
            ets:insert(imboy_cache_meta, {{peer_incarnation, Peer}, Boot}),
            Event = (imboy_cache_sync_proto:event(
                set,
                K,
                stale,
                erlang:system_time(millisecond) + 60000,
                {Counter - 1, Peer},
                1,
                Boot
            ))#{
                origin_node => Peer
            },
            whereis(imboy_cache_sync) ! {cache_sync, Event},
            pong = gen_server:call(imboy_cache_sync, ping_ok),
            ?assertEqual(Rev, imboy_cache_sync:current_rev(K)),
            ?assertEqual(undefined, depcache:get(K, imboy_cache)),
            ?assertEqual(undefined, imboy_cache:get(K)),
            ?assertEqual(ready, imboy_cache_sync:status())
        end)
    end).
begin_completion_round() ->
    begin_completion_round('completion_fence@synthetic').
begin_completion_round(Peer) ->
    imboy_cache_source_cut_tests:fixture_identity(Peer, <<7:128>>, imboy_cache_sync_proto:epoch()),
    Deadline = erlang:system_time(millisecond) + 60000,
    %% recovery_round 冻结真实 census；共享 requested_peer 经产品入口记录
    %% 独立 origin_target（不再由 fixture 手工清空义务或手写目标）。
    R = imboy_cache_source_cut_tests:requested_peer(
        imboy_cache_sync_snapshot:expect_peer(
            imboy_cache_sync_snapshot:recovery_round(0, Deadline), Peer
        ),
        Peer
    ),
    sys:replace_state(imboy_cache_sync, fun(S) ->
        S#state{status = recovering, snapshot_round = R, recover_deadline_ms = Deadline}
    end),
    ets:insert(imboy_cache_meta, {status, recovering}),
    {Peer, R}.
missing_unknown_or_gapped_cut_cannot_complete_real_actor_test_() ->
    ?TEST_WITH_APP(fun() ->
        lists:foreach(
            fun(Kind) ->
                with_budget(#{}, fun() ->
                    {Peer, R} = begin_completion_round(),
                    Round = maps:get(round, R),
                    S = sys:get_state(imboy_cache_sync),
                    Frame =
                        {cache_snapshot_cut, Peer, Round, <<7:128>>, imboy_cache_sync_proto:epoch(),
                            0, imboy_cache_sync_proto:config_hash(),
                            imboy_cache_sync_proto:registry_hash(), {ok, []}},
                    case Kind of
                        missing -> ok;
                        unknown_boot -> whereis(imboy_cache_sync) ! setelement(4, Frame, <<8:128>>);
                        gap -> whereis(imboy_cache_sync) ! setelement(6, Frame, 1)
                    end,
                    whereis(imboy_cache_sync) !
                        {cache_snapshot_done, Peer, Round, S#state.counter, 0},
                    pong = gen_server:call(imboy_cache_sync, ping_ok),
                    %% 隔离正常定时重试，完整状态相等只衡量迟到 done 的副作用。
                    Next = sys:replace_state(imboy_cache_sync, fun(Current) ->
                        Ref = Current#state.recovery_timer,
                        case is_reference(Ref) of
                            true -> erlang:cancel_timer(Ref);
                            false -> ok
                        end,
                        Current#state{recovery_timer = undefined}
                    end),
                    ?assertNotEqual(ready, Next#state.status),
                    ?assertNotEqual(Round, maps:get(round, Next#state.snapshot_round)),
                    whereis(imboy_cache_sync) !
                        {cache_snapshot_done, Peer, Round, S#state.counter, 0},
                    pong = gen_server:call(imboy_cache_sync, ping_ok),
                    ?assertEqual(Next, sys:get_state(imboy_cache_sync)),
                    ?assert(is_process_alive(whereis(imboy_cache_sync)))
                end)
            end,
            [missing, unknown_boot, gap]
        )
    end).
completion_loader(Parent, Key) ->
    put(loader_calls, 0),
    Loader = fun() ->
        N = get(loader_calls),
        put(loader_calls, N + 1),
        case N of
            0 ->
                Parent ! {loader_started, self()},
                receive
                    resume -> stale
                end;
            _ ->
                fresh
        end
    end,
    Parent ! {loader_result, imboy_cache:memo(Loader, Key, 60, [])}.

incarnation_grace_overflow_enters_cold_recovery_test_() ->
    ?TEST_WITH_APP(fun() ->
        lists:foreach(
            fun(Kind) ->
                with_budget(#{grace_max => 1}, fun() -> assert_grace_overflow(Kind) end)
            end,
            [set, receipt, report]
        )
    end).
assert_grace_overflow(Kind) ->
    ets:match_delete(imboy_cache_meta, {{grace_incarnation, '_'}, '_'}),
    Key = {cache_v2_test_rep, grace_capacity},
    ok = imboy_cache:put(Key, old, 60, replicate),
    Peer =
        case Kind of
            set -> 'grace_capacity@synthetic';
            receipt -> 'grace_receipt@synthetic';
            report -> 'grace_report@synthetic'
        end,
    registered = imboy_cache_sync_proto:register_incarnation(Peer, <<1:128>>),
    changed = imboy_cache_sync_proto:register_incarnation(Peer, <<2:128>>),
    S = sys:get_state(imboy_cache_sync),
    Epoch = imboy_cache_sync_proto:epoch(),
    deliver_grace_overflow(Kind, Key, Peer, S, Epoch),
    pong = gen_server:call(imboy_cache_sync, ping_ok),
    Next = sys:get_state(imboy_cache_sync),
    ?assertNotEqual(ready, Next#state.status),
    ?assert(Next#state.requires_authority),
    Peers =
        case Next#state.snapshot_round of
            #{peers := Ps} -> Ps;
            _ -> #{}
        end,
    ?assertNot(lists:any(fun({_N, P}) -> maps:get(authority, P, false) end, maps:to_list(Peers))),
    ?assertEqual(Kind =:= report, imboy_cache_sync_proto:epoch() > Epoch),
    ?assertEqual(
        [{{peer_incarnation, Peer}, <<2:128>>}],
        ets:lookup(imboy_cache_meta, {peer_incarnation, Peer})
    ),
    ?assertEqual(undefined, depcache:get(Key, imboy_cache)),
    ?assertEqual(undefined, imboy_cache:get(Key)).
deliver_grace_overflow(set, Key, Peer, S, _Epoch) ->
    Event = (imboy_cache_sync_proto:event(
        set,
        Key,
        newer,
        erlang:system_time(millisecond) + 60000,
        {S#state.counter + 10, Peer},
        1,
        <<3:128>>
    ))#{
        origin_node => Peer
    },
    whereis(imboy_cache_sync) ! {cache_sync, Event};
deliver_grace_overflow(receipt, Key, Peer, _S, _Epoch) ->
    whereis(imboy_cache_sync) !
        {cache_sync,
            {legacy_invalidation_seq, Peer, <<3:128>>, 1, imboy_cache_sync_proto:config_hash(),
                imboy_cache_sync_proto:registry_hash(), Key}};
deliver_grace_overflow(report, _Key, Peer, S, Epoch) ->
    sys:replace_state(imboy_cache_sync, fun(Current) ->
        R = imboy_cache_sync_snapshot:expect_peer(
            imboy_cache_sync_snapshot:new_round(0, erlang:system_time(millisecond) + 60000), Peer
        ),
        imboy_cache_sync_recover:handle_version_report(
            Peer,
            S#state.counter + 10,
            Epoch,
            imboy_cache_sync_proto:config_hash(),
            imboy_cache_sync_proto:registry_hash(),
            <<3:128>>,
            true,
            self(),
            Current#state{status = recovering, snapshot_round = R}
        )
    end).
third_party_gap_after_cut_cannot_complete_real_actor_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_budget(#{}, fun() ->
            {Peer, R} = begin_completion_round('source_cut_gap@synthetic'),
            Round = maps:get(round, R),
            whereis(imboy_cache_sync) !
                {cache_snapshot_cut, Peer, Round, <<7:128>>, imboy_cache_sync_proto:epoch(), 0,
                    imboy_cache_sync_proto:config_hash(), imboy_cache_sync_proto:registry_hash(),
                    {ok, []}},
            pong = gen_server:call(imboy_cache_sync, ping_ok),
            Before = sys:get_state(imboy_cache_sync),
            ?assert(
                maps:is_key(
                    source_cut,
                    maps:get(Peer, maps:get(peers, Before#state.snapshot_round))
                )
            ),
            Third = 'third_party_gap@synthetic',
            Boot = <<10:128>>,
            ets:insert(imboy_cache_meta, {{peer_incarnation, Third}, Boot}),
            Event = (imboy_cache_sync_proto:event(
                set,
                {cache_v2_test_rep, third_party_gap},
                current,
                erlang:system_time(millisecond) + 60000,
                {Before#state.counter + 1, Third},
                2,
                Boot
            ))#{
                origin_node => Third
            },
            whereis(imboy_cache_sync) ! {cache_sync, Event},
            pong = gen_server:call(imboy_cache_sync, ping_ok),
            Interrupted = sys:get_state(imboy_cache_sync),
            ?assertNotEqual(Round, maps:get(round, Interrupted#state.snapshot_round)),
            whereis(imboy_cache_sync) ! {cache_snapshot_done, Peer, Round, Before#state.counter, 0},
            pong = gen_server:call(imboy_cache_sync, ping_ok),
            Next = sys:get_state(imboy_cache_sync),
            ?assertNotEqual(ready, Next#state.status),
            ?assertEqual(Before#state.recover_deadline_ms, Next#state.recover_deadline_ms),
            ?assertEqual(undefined, imboy_cache:get({cache_v2_test_rep, third_party_gap}))
        end)
    end).

source_restart_after_cut_cannot_complete_real_actor_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_budget(#{}, fun() ->
            {Peer, R} = begin_completion_round('source_restart_cut@synthetic'),
            Round = maps:get(round, R),
            whereis(imboy_cache_sync) !
                {cache_snapshot_cut, Peer, Round, <<7:128>>, imboy_cache_sync_proto:epoch(), 0,
                    imboy_cache_sync_proto:config_hash(), imboy_cache_sync_proto:registry_hash(),
                    {ok, []}},
            pong = gen_server:call(imboy_cache_sync, ping_ok),
            Before = sys:get_state(imboy_cache_sync),
            ?assert(
                maps:is_key(
                    source_cut, maps:get(Peer, maps:get(peers, Before#state.snapshot_round))
                )
            ),
            ets:insert(imboy_cache_meta, {{peer_incarnation, Peer}, <<8:128>>}),
            whereis(imboy_cache_sync) ! {cache_snapshot_done, Peer, Round, Before#state.counter, 0},
            pong = gen_server:call(imboy_cache_sync, ping_ok),
            Next = sys:get_state(imboy_cache_sync),
            ?assertNotEqual(ready, Next#state.status),
            ?assertNotEqual(Round, maps:get(round, Next#state.snapshot_round)),
            ?assertEqual(Before#state.recover_deadline_ms, Next#state.recover_deadline_ms)
        end)
    end).

assert_completion_loader(Key, Peer, R) ->
    Parent = self(),
    {Worker, Monitor} = spawn_monitor(fun() -> completion_loader(Parent, Key) end),
    try
        receive
            {loader_started, Worker} -> ok
        after 1000 -> error(loader_not_started)
        end,
        G = imboy_cache_sync:generation(Key),
        S = sys:get_state(imboy_cache_sync),
        whereis(imboy_cache_sync) !
            {cache_snapshot_cut, Peer, maps:get(round, R), <<7:128>>,
                imboy_cache_sync_proto:epoch(), 0, imboy_cache_sync_proto:config_hash(),
                imboy_cache_sync_proto:registry_hash(), {ok, []},
                {0, crypto:hash(sha256, term_to_binary([]))}},
        whereis(imboy_cache_sync) !
            {cache_snapshot_done, Peer, maps:get(round, R), S#state.counter, 0},
        pong = gen_server:call(imboy_cache_sync, ping_ok),
        ?assertEqual(ready, imboy_cache_sync:status()),
        ?assert(imboy_cache_sync:generation(Key) > G),
        Worker ! resume,
        receive
            {loader_result, Value} -> ?assertEqual(fresh, Value)
        after 1000 -> error(no_result)
        end,
        receive
            {'DOWN', Monitor, process, Worker, normal} -> ok
        after 1000 -> error(worker_alive)
        end,
        ?assertEqual(fresh, imboy_cache:memo(fun() -> unexpected end, Key, 60, []))
    after
        exit(Worker, kill),
        erlang:demonitor(Monitor, [flush])
    end.

journal_capture_read_failure_keeps_public_put_successful_and_actor_alive_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_budget(#{}, fun() ->
            Round = <<8:96>>,
            ets:insert(imboy_cache_meta, {{snapshot_sender, self()}, #{round => Round}}),
            ok = imboy_cache_sync_apply:journal_refresh(),
            %% 仅 facade 读取故障注入；depcache 和 sync actor 均保持真实运行。
            meck:new(imboy_cache, [passthrough, no_link]),
            meck:expect(imboy_cache, get, 1, fun(_) -> erlang:error(capture_failed) end),
            Key = {cache_v2_test_rep, journal_capture_failure},
            try
                ?assertEqual(ok, imboy_cache:put(Key, accepted, 60, replicate)),
                ?assertEqual({ok, accepted}, depcache:get(Key, imboy_cache)),
                ?assert(is_process_alive(whereis(imboy_cache_sync))),
                ?assertEqual(ready, imboy_cache_sync:status()),
                receive
                    {cache_snapshot_abort, _, Round, journal_entry_unavailable} -> ok
                after 1000 -> ?assert(false)
                end
            after
                meck:unload(imboy_cache)
            end
        end)
    end).

local_metadata_overflow_preserves_value_and_clock_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_budget(#{n_max_bytes => 500}, fun() ->
            Key = {cache_v2_test_rep, retained},
            ?assertEqual(ok, imboy_cache:put(Key, <<"live">>, 60, replicate)),
            Before = sys:get_state(imboy_cache_sync),
            HugeKey = {cache_v2_test_rep, binary:copy(<<0>>, 600)},
            ?assertEqual(
                {error, index_full}, imboy_cache:put(HugeKey, <<"rejected">>, 60, replicate)
            ),
            ?assertEqual(Before, sys:get_state(imboy_cache_sync)),
            ?assertEqual({ok, <<"live">>}, depcache:get(Key, imboy_cache)),
            ?assertEqual(undefined, depcache:get(HugeKey, imboy_cache)),
            [{metadata_usage, {1, Bytes, 0, 0}}] = ets:lookup(imboy_cache_meta, metadata_usage),
            ?assert(Bytes =< 500)
        end)
    end).

tombstone_byte_overflow_clears_real_values_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_budget(#{t_max_bytes => 1, recovery_window_sec => 1}, fun() ->
            Key = {cache_v2_test_rep, cold},
            ?assertEqual(ok, imboy_cache:put(Key, <<"live">>, 60, replicate)),
            Epoch = imboy_cache_sync_proto:epoch(),
            ?assertEqual({error, tombstone_budget_exceeded}, imboy_cache:put_delete(Key)),
            ?assert(imboy_cache_sync_proto:epoch() > Epoch),
            ?assertEqual(undefined, depcache:get(Key, imboy_cache)),
            ?assertEqual(
                [{metadata_usage, {0, 0, 0, 0}}], ets:lookup(imboy_cache_meta, metadata_usage)
            ),
            ?assertNotEqual(ready, imboy_cache_sync:status()),
            ?assertMatch(
                {error, {not_ready, _}}, imboy_cache:put(Key, <<"blocked">>, 60, replicate)
            ),
            ?assertEqual(degraded, await_degraded(30))
        end)
    end).

remote_tombstone_overflow_preserves_recovery_state_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_budget(#{t_max_bytes => 1}, fun() ->
            Key = {cache_v2_test_rep, remote_cold},
            ?assertEqual(ok, imboy_cache:put(Key, <<"live">>, 60, replicate)),
            Counter = (sys:get_state(imboy_cache_sync))#state.counter + 1,
            Event = (imboy_cache_sync_proto:event(
                delete,
                Key,
                undefined,
                erlang:system_time(millisecond) + imboy_cache_policy:tombstone_ttl_sec() * 1000,
                {Counter, 'remote_metadata@synthetic'},
                1,
                <<3:128>>
            ))#{
                origin_node => 'remote_metadata@synthetic'
            },
            whereis(imboy_cache_sync) ! {cache_sync, Event},
            pong = gen_server:call(imboy_cache_sync, ping_ok),
            State = sys:get_state(imboy_cache_sync),
            ?assert(State#state.requires_authority),
            ?assertNotEqual(ready, State#state.status),
            ?assert(is_reference(State#state.recovery_timer)),
            ?assertEqual(undefined, depcache:get(Key, imboy_cache))
        end)
    end).

remote_metadata_capacity_rejection_cold_quarantines_old_sources_test_() ->
    ?TEST_WITH_APP(fun() ->
        lists:foreach(
            fun(Operation) ->
                with_budget(#{recovery_window_sec => 1}, fun() ->
                    Key = {cache_v2_test_rep, {remote_capacity, Operation}},
                    ok = imboy_cache:put(Key, original, 60, replicate),
                    {ok, Config} = application:get_env(imboy, cache_replication),
                    application:set_env(imboy, cache_replication, Config#{n_max_bytes => 1}),
                    Counter = (sys:get_state(imboy_cache_sync))#state.counter + 1,
                    TTL =
                        case Operation of
                            set -> 60;
                            expired_set -> -1;
                            delete -> imboy_cache_policy:tombstone_ttl_sec()
                        end,
                    Value =
                        case Operation of
                            set -> newer;
                            expired_set -> newer;
                            delete -> undefined
                        end,
                    Peer = 'remote_capacity@synthetic',
                    registered = imboy_cache_sync_proto:register_incarnation(Peer, <<5:128>>),
                    RejectedEpoch = imboy_cache_sync_proto:epoch() + 1,
                    Event = (imboy_cache_sync_proto:event(
                        case Operation of
                            expired_set -> set;
                            _ -> Operation
                        end,
                        Key,
                        Value,
                        erlang:system_time(millisecond) + TTL * 1000,
                        {Counter, Peer},
                        1,
                        <<5:128>>
                    ))#{
                        origin_node => Peer, epoch => RejectedEpoch
                    },
                    whereis(imboy_cache_sync) ! {cache_sync, Event},
                    pong = gen_server:call(imboy_cache_sync, ping_ok),
                    S = sys:get_state(imboy_cache_sync),
                    ?assertNotEqual(ready, S#state.status),
                    ?assert(S#state.requires_authority),
                    ?assert(imboy_cache_sync_proto:epoch() > RejectedEpoch),
                    ?assertEqual([], ets:lookup(imboy_cache_meta, {rev, Key})),
                    ?assertEqual(undefined, depcache:get(Key, imboy_cache)),
                    ?assertEqual(undefined, imboy_cache:get(Key)),
                    assert_old_epoch_source_is_excluded(RejectedEpoch, Counter),
                    ?assertEqual(degraded, await_degraded(30))
                end)
            end,
            [set, delete, expired_set]
        )
    end).

assert_old_epoch_source_is_excluded(Epoch, Counter) ->
    Peer = node(),
    S = sys:replace_state(imboy_cache_sync, fun(Current) ->
        case Current#state.recovery_timer of
            Ref when is_reference(Ref) -> erlang:cancel_timer(Ref);
            _ -> ok
        end,
        R = imboy_cache_sync_snapshot:expect_peer(
            imboy_cache_sync_snapshot:new_round(0, Current#state.recover_deadline_ms), Peer
        ),
        Current#state{status = recovering, recovery_timer = undefined, snapshot_round = R}
    end),
    Round = maps:get(round, S#state.snapshot_round),
    whereis(imboy_cache_sync) !
        {cache_version_report, Peer, Counter - 1, Epoch, imboy_cache_sync_proto:config_hash(),
            imboy_cache_sync_proto:registry_hash(), <<9:128>>, true, whereis(imboy_cache_sync)},
    pong = gen_server:call(imboy_cache_sync, ping_ok),
    Classified = sys:get_state(imboy_cache_sync),
    P = maps:get(Peer, maps:get(peers, Classified#state.snapshot_round)),
    ?assert(maps:get(reported, P)),
    ?assertNot(maps:get(authority, P)),
    whereis(imboy_cache_sync) ! {cache_snapshot_done, Peer, Round, Counter - 1, 0},
    pong = gen_server:call(imboy_cache_sync, ping_ok),
    ?assertNotEqual(ready, imboy_cache_sync:status()),
    ?assertEqual(
        S#state.recover_deadline_ms, (sys:get_state(imboy_cache_sync))#state.recover_deadline_ms
    ).

lost_tail_watermark_hides_replicas_and_does_not_extend_deadline_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_budget(#{recovery_window_sec => 1}, fun() ->
            K = {cache_v2_test_rep, missing_tail},
            ?assertEqual(ok, imboy_cache:put(K, old, 60, replicate)),
            Rev = imboy_cache_sync:current_rev(K),
            Epoch = imboy_cache_sync_proto:epoch(),
            Peer = 'lost_tail_peer@synthetic',
            Boot = <<4:128>>,
            ets:insert(imboy_cache_meta, [
                {{peer_incarnation, Peer}, Boot}, {{seq, {Peer, Boot}}, 1}
            ]),
            Frame =
                {cache_invalidation_watermark, Peer, Boot, 2, imboy_cache_sync_proto:config_hash(),
                    imboy_cache_sync_proto:registry_hash()},
            whereis(imboy_cache_sync) ! Frame,
            pong = gen_server:call(imboy_cache_sync, ping_ok),
            S1 = sys:get_state(imboy_cache_sync),
            ?assert(S1#state.requires_authority),
            ?assertNotEqual(ready, S1#state.status),
            ?assertEqual(undefined, imboy_cache:get(K)),
            ?assertEqual(undefined, depcache:get(K, imboy_cache)),
            ?assertEqual(Rev, imboy_cache_sync:current_rev(K)),
            whereis(imboy_cache_sync) ! Frame,
            pong = gen_server:call(imboy_cache_sync, ping_ok),
            S2 = sys:get_state(imboy_cache_sync),
            ?assertEqual(S1#state.recover_deadline_ms, S2#state.recover_deadline_ms),
            ?assertEqual(Epoch, imboy_cache_sync_proto:epoch()),
            ?assertEqual(degraded, await_degraded(30))
        end)
    end).

rejected_later_event_still_preserves_gap_recovery_state_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_budget(#{}, fun() ->
            K = {cache_v2_test_rep, stale_after_gap},
            ?assertEqual(ok, imboy_cache:put(K, old, 60, replicate)),
            Rev = imboy_cache_sync:current_rev(K),
            Peer = 'event_gap_peer@synthetic',
            Boot = <<5:128>>,
            ets:insert(imboy_cache_meta, [
                {{peer_incarnation, Peer}, Boot}, {{seq, {Peer, Boot}}, 1}
            ]),
            Event = (imboy_cache_sync_proto:event(
                set,
                K,
                stale,
                erlang:system_time(millisecond) + 60000,
                {0, Peer},
                3,
                Boot
            ))#{
                origin_node => Peer
            },
            whereis(imboy_cache_sync) ! {cache_sync, Event},
            pong = gen_server:call(imboy_cache_sync, ping_ok),
            State = sys:get_state(imboy_cache_sync),
            ?assert(State#state.requires_authority),
            ?assertNotEqual(ready, State#state.status),
            ?assertEqual(Rev, imboy_cache_sync:current_rev(K)),
            ?assertEqual(undefined, depcache:get(K, imboy_cache)),
            ?assert(is_reference(State#state.recovery_timer))
        end)
    end).

receipt_alone_executes_invalidation_before_acknowledging_sequence_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_budget(#{}, fun() ->
            K = {cache_v2_test_rep, missed_legacy_flush},
            ?assertEqual(ok, imboy_cache:put(K, old, 60, replicate)),
            Peer = 'receipt_only_peer@synthetic',
            Boot = <<6:128>>,
            ets:insert(imboy_cache_meta, [
                {{peer_incarnation, Peer}, Boot}, {{seq, {Peer, Boot}}, 1}
            ]),
            Frame =
                {legacy_invalidation_seq, Peer, Boot, 2, imboy_cache_sync_proto:config_hash(),
                    imboy_cache_sync_proto:registry_hash(), K},
            %% 不发送旧 flush，只收到连续收据，必须清除真实值且保持 READY。
            whereis(imboy_cache_sync) ! {cache_sync, Frame},
            pong = gen_server:call(imboy_cache_sync, ping_ok),
            ?assertEqual(undefined, depcache:get(K, imboy_cache)),
            ?assertEqual(undefined, imboy_cache:get(K)),
            ?assertEqual(ready, imboy_cache_sync:status()),
            [{_, 2}] = ets:lookup(imboy_cache_meta, {seq, {Peer, Boot}}),
            Watermark = erlang:delete_element(
                7, setelement(1, Frame, cache_invalidation_watermark)
            ),
            whereis(imboy_cache_sync) ! Watermark,
            pong = gen_server:call(imboy_cache_sync, ping_ok),
            ?assertEqual(ready, imboy_cache_sync:status()),
            ?assertEqual(undefined, imboy_cache:get(K))
        end)
    end).

accepted_snapshot_page_advances_clock_even_if_later_page_aborts_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_budget(#{}, fun() ->
            Peer = 'snapshot_clock_peer@synthetic',
            Initial = sys:get_state(imboy_cache_sync),
            Observed = Initial#state.counter + 10,
            Deadline = erlang:system_time(millisecond) + 60000,
            R0 = imboy_cache_sync_snapshot:new_round(0, Deadline),
            R1 = requested_peer(
                imboy_cache_sync_snapshot:expect_peer(R0, Peer), Peer
            ),
            Round = maps:get(round, R1),
            sys:replace_state(imboy_cache_sync, fun(S) ->
                S#state{
                    status = recovering,
                    snapshot_round = R1,
                    recover_deadline_ms = Deadline
                }
            end),
            ets:insert(imboy_cache_meta, {status, recovering}),
            Entry = {{cache_v2_test_rep, clock_page}, value, Deadline, {Observed, Peer}},
            send_clock_page(Peer, Round, 1, Entry),
            ?assertEqual(Observed, (sys:get_state(imboy_cache_sync))#state.counter),
            ?assertEqual([{sync_counter, Observed}], ets:lookup(imboy_cache_meta, sync_counter)),
            Bad = {{unknown_clock_ns, key}, value, Deadline, {Observed + 1000, Peer}},
            send_clock_page(Peer, Round, 2, Bad),
            After = sys:get_state(imboy_cache_sync),
            ?assertEqual(Observed, After#state.counter),
            ?assertNotEqual(Round, maps:get(round, After#state.snapshot_round)),
            ?assertEqual([{sync_counter, Observed}], ets:lookup(imboy_cache_meta, sync_counter))
        end)
    end).

send_clock_page(Peer, Round, Number, Entry) ->
    Digest = imboy_cache_sync_snapshot:page_digest(Round, Number, [Entry]),
    whereis(imboy_cache_sync) ! {cache_snapshot_page, Peer, Round, Number, [Entry], Digest, true},
    pong = gen_server:call(imboy_cache_sync, ping_ok).

%% 下层合成来源 fixture；来源独立枚举的证明由 sender/census 测试负责。
source_fixture_certificate(Peer) ->
    State = sys:get_state(imboy_cache_sync),
    P = maps:get(Peer, maps:get(peers, State#state.snapshot_round)),
    {ok, Rows} = imboy_cache_policy:final_snapshot_manifest(P),
    imboy_cache_sync_coverage:manifest_certificate(Rows).

wait_sync_ready(0) ->
    timeout;
wait_sync_ready(WaitMs) ->
    case imboy_cache_sync:status() of
        ready ->
            ok;
        _ ->
            receive
            after 100 -> ok
            end,
            wait_sync_ready(WaitMs - 100)
    end.

with_budget(Limits, Fun) ->
    OldCfg = application:get_env(imboy, cache_replication),
    OldRegistry = application:get_env(imboy, cache_registry_overrides),
    OriginalState = sys:get_state(imboy_cache_sync),
    try
        %% 启动后 sync 需经历有界发现重试（3×500ms）才单节点转 READY；
        %% 测试体先等待就绪，避免对 discovering 瞬态的时序竞态。
        wait_sync_ready(5000),
        ?assertEqual(ready, imboy_cache_sync:status()),
        ok = imboy_cache_sync_apply:controlled_cold_recovery(),
        application:set_env(imboy, cache_replication, Limits#{whitelist => [cache_v2_test_rep]}),
        application:set_env(imboy, cache_registry_overrides, #{
            cache_v2_test_rep => {replicate, false}
        }),
        Fun()
    after
        ok = imboy_cache_sync_apply:controlled_cold_recovery(),
        CurrentState = sys:get_state(imboy_cache_sync),
        Ref = CurrentState#state.recovery_timer,
        case is_reference(Ref) of
            true -> erlang:cancel_timer(Ref);
            false -> ok
        end,
        %% 合成回合注入会与产品恢复自动机（member_loss/冷恢复/回合超时）
        %% 交互，仅回放快照状态会留下不一致定时器与恢复循环；结束后重启
        %% sync 进程取干净初态，等待其单节点就绪后再交还下一用例。
        OldPid = whereis(imboy_cache_sync),
        case is_pid(OldPid) of
            true ->
                exit(OldPid, kill),
                wait_sync_restart(OldPid, 100);
            false ->
                ok
        end,
        wait_sync_ready(5000),
        restore(cache_replication, OldCfg),
        restore(cache_registry_overrides, OldRegistry)
    end.
wait_sync_restart(_OldPid, 0) ->
    ok;
wait_sync_restart(OldPid, N) ->
    case whereis(imboy_cache_sync) of
        P when is_pid(P), P =/= OldPid ->
            ok;
        _ ->
            timer:sleep(50),
            wait_sync_restart(OldPid, N - 1)
    end.

restore(Key, undefined) -> application:unset_env(imboy, Key);
restore(Key, {ok, Value}) -> application:set_env(imboy, Key, Value).

await_degraded(0) ->
    imboy_cache_sync:status();
await_degraded(N) ->
    case imboy_cache_sync:status() of
        degraded ->
            degraded;
        _ ->
            timer:sleep(100),
            await_degraded(N - 1)
    end.

requested_peer(Round, Peer) -> imboy_cache_source_cut_tests:requested_peer(Round, Peer).
