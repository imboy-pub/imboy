-module(imboy_cache_capability_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("imboy_cache_sync_state.hrl").

rejection_metrics_are_bounded_and_legacy_frames_are_counted_test_() ->
    ?TEST_WITH_APP(fun() ->
        #{counters := Before} = elib_metric:get_all_metrics(),
        Pid = whereis(imboy_cache_sync),
        Pid ! {cache_sync, #{protocol => 999, key => <<"private-key">>}},
        Pid ! {cache_sync, {flush, {metric_test, absent}}},
        ?assertEqual(pong, gen_server:call(Pid, ping_ok)),
        #{counters := After} = elib_metric:get_all_metrics(),
        lists:foreach(
            fun(Name) ->
                ?assertEqual(maps:get(Name, Before, 0) + 1, maps:get(Name, After, 0))
            end,
            [
                {cache_sync_reject_reason_total, #{reason => invalid_protocol}},
                cache_sync_legacy_receive_total
            ]
        ),
        ok = imboy_cache_sync_proto:record_reject({untrusted, <<"private-key">>}),
        ok = imboy_cache_sync_proto:record_recovery_abort(round_timeout),
        ok = imboy_cache_sync_proto:record_recovery_abort({untrusted, <<"private-key">>}),
        #{counters := Bounded} = elib_metric:get_all_metrics(),
        ?assertEqual(
            maps:get({cache_sync_reject_reason_total, #{reason => other}}, After, 0) + 1,
            maps:get({cache_sync_reject_reason_total, #{reason => other}}, Bounded)
        ),
        ?assertEqual(
            maps:get({cache_sync_snapshot_abort_reason_total, #{reason => round_timeout}}, After, 0) +
                1,
            maps:get({cache_sync_snapshot_abort_reason_total, #{reason => round_timeout}}, Bounded)
        ),
        ?assertEqual(nomatch, binary:match(term_to_binary(Bounded), <<"private-key">>))
    end).

expired_recovery_deadline_emits_real_timeout_signal_test_() ->
    ?TEST_WITH_APP(fun() ->
        Before = elib_metric:get_all_metrics(),
        Past = erlang:system_time(millisecond) - 1,
        State = #state{
            status = recovering,
            snapshot_round = #{deadline_ms => Past, peers => #{}},
            recover_deadline_ms = Past
        },
        {noreply, Degraded} = imboy_cache_sync_recover:handle_timeout(State),
        ?assertEqual(degraded, Degraded#state.status),
        #{counters := Counters} = elib_metric:get_all_metrics(),
        #{counters := Previous} = Before,
        ?assertEqual(
            maps:get(
                {cache_sync_recovery_timeout_total, #{reason => deadline_timeout}}, Previous, 0
            ) + 1,
            maps:get({cache_sync_recovery_timeout_total, #{reason => deadline_timeout}}, Counters)
        )
    end).

unnegotiated_member_blocks_managed_writes_without_mutation_test_() ->
    ?TEST_WITH_APP(fun capability_boundary/0).

sensitive_hot_cache_cannot_bypass_missing_members_test_() ->
    ?TEST_WITH_APP(fun sensitive_boundary/0).

census_budget_failure_cannot_kill_sync_or_claim_empty_test_() ->
    ?TEST_WITH_APP(fun() ->
        Old = application:get_env(imboy, cache_replication),
        Key = {census_budget_test, retained},
        Pid = whereis(imboy_cache_sync),
        try
            Exp = erlang:system_time(millisecond) + 60000,
            ok = imboy_cache_sync_apply:store_revision(Key, {1, node()}, Exp, deleted),
            application:set_env(imboy, cache_replication, #{b_max_bytes => 1}),
            Round = <<71:96>>,
            Pid ! {cache_version_request, self(), node(), Round},
            receive
                {cache_version_report, _, _, _, _, _, _, _, _, _, Round, Census} ->
                    ?assertEqual({error, source_census_byte_budget}, Census)
            after 1000 -> error(missing_fail_closed_census)
            end,
            ?assertEqual(Pid, whereis(imboy_cache_sync)),
            ?assert(erlang:is_process_alive(Pid)),
            ?assertEqual(pong, gen_server:call(Pid, ping_ok))
        after
            restore(cache_replication, Old),
            case erlang:is_process_alive(Pid) of
                true -> imboy_cache_sync_apply:delete_metadata({rev, Key});
                false -> ok
            end
        end
    end).

malformed_membership_observer_cannot_crash_actor_test_() ->
    ?TEST_WITH_APP(fun() ->
        Old = application:get_env(imboy, cluster_nodes),
        Pid = whereis(imboy_cache_sync),
        try
            lists:foreach(
                fun(Config) ->
                    application:set_env(imboy, cluster_nodes, Config),
                    Parent = self(),
                    sys:replace_state(Pid, fun(State) ->
                        Parent !
                            {observer_result, imboy_cache_sync_coverage:observe_node_down(node()),
                                imboy_cache_sync_coverage:observe_member_loss([])},
                        State
                    end),
                    receive
                        {observer_result, A, B} ->
                            ?assertEqual(invalid, A),
                            ?assertEqual(invalid, B)
                    after 1000 -> error(observer_result_missing)
                    end,
                    ?assertEqual(pong, gen_server:call(Pid, ping_ok))
                end,
                [bad_config, [node() | bad_tail]]
            )
        after
            restore(cluster_nodes, Old)
        end
    end).

malformed_node_down_rejects_without_killing_actor_test_() ->
    ?TEST_WITH_APP(fun() ->
        Old = application:get_env(imboy, cluster_nodes),
        Pid = whereis(imboy_cache_sync),
        State = sys:get_state(Pid),
        Saved = ets:tab2list(imboy_cache_meta),
        try
            application:set_env(imboy, cluster_nodes, [node() | invalid_tail]),
            Pid ! {nodedown, 'malformed_observer@synthetic'},
            ?assertEqual(pong, gen_server:call(Pid, ping_ok)),
            ?assertEqual(Pid, whereis(imboy_cache_sync)),
            ?assertNotEqual(ready, imboy_cache_sync:status())
        after
            restore(cluster_nodes, Old),
            sys:replace_state(Pid, fun(Current) ->
                case Current#state.recovery_timer of
                    R when is_reference(R) -> erlang:cancel_timer(R);
                    _ -> ok
                end,
                ets:delete_all_objects(imboy_cache_meta),
                ets:insert(imboy_cache_meta, Saved),
                State
            end)
        end
    end).

observed_partial_loss_preserves_first_window_test_() ->
    ?TEST_WITH_APP(fun() ->
        Old = application:get_env(imboy, cluster_nodes),
        try
            application:set_env(imboy, cluster_nodes, [node(), 'partial_lost@synthetic']),
            Parent = self(),
            sys:replace_state(imboy_cache_sync, fun(State) ->
                Since = erlang:monotonic_time(millisecond) - 1,
                ets:insert(imboy_cache_meta, {cache_peer_loss, {Since, undefined}}),
                continue = imboy_cache_sync_coverage:observe_member_loss([]),
                [{_, {Since, _}}] = ets:lookup(imboy_cache_meta, cache_peer_loss),
                Past = Since - imboy_cache_policy:grace_window_ms(),
                ets:insert(imboy_cache_meta, {cache_peer_loss, {Past, undefined}}),
                Parent ! {partial_expiry, imboy_cache_sync_coverage:observe_member_loss([])},
                State
            end),
            receive
                {partial_expiry, Result} -> ?assertEqual(cold, Result)
            after 1000 -> error(partial_result_missing)
            end
        after
            imboy_cache_sync_apply:delete_metadata(cache_peer_loss),
            restore(cluster_nodes, Old)
        end
    end).

observed_island_window_expiry_requires_cold_epoch_test_() ->
    ?TEST_WITH_APP(fun() ->
        OldSync = application:get_env(imboy, dsync_enabled),
        OldNodes = application:get_env(imboy, cluster_nodes),
        State = sys:get_state(imboy_cache_sync),
        Saved = ets:tab2list(imboy_cache_meta),
        Epoch = imboy_cache_sync_proto:epoch(),
        try
            application:set_env(imboy, dsync_enabled, true),
            application:set_env(imboy, cluster_nodes, [node(), 'island_other@synthetic']),
            %% 仅阈值单元 fixture；不替代 C9m 的真实跨 W 时钟证据。
            Since = erlang:monotonic_time(millisecond) - imboy_cache_policy:grace_window_ms() - 1,
            sys:replace_state(imboy_cache_sync, fun(S) ->
                ets:insert(imboy_cache_meta, {cache_peer_loss, {Since, Since}}),
                imboy_cache_sync_recover:periodic_reconcile(S)
            end),
            ?assert(imboy_cache_sync_proto:epoch() > Epoch),
            ?assertNotEqual(ready, imboy_cache_sync:status())
        after
            sys:replace_state(imboy_cache_sync, fun(Current) ->
                case Current#state.recovery_timer of
                    R when is_reference(R) -> erlang:cancel_timer(R);
                    _ -> ok
                end,
                ets:delete_all_objects(imboy_cache_meta),
                ets:insert(imboy_cache_meta, Saved),
                State
            end),
            restore(dsync_enabled, OldSync),
            restore(cluster_nodes, OldNodes)
        end
    end).

known_peer_loss_cannot_reuse_sensitive_hot_value_test_() ->
    ?TEST_WITH_APP(fun() ->
        OldSync = application:get_env(imboy, dsync_enabled),
        OldNodes = application:get_env(imboy, cluster_nodes),
        Key = {auth_session_epoch, known_peer_loss},
        Peer = 'known_lost@synthetic',
        try
            application:set_env(imboy, dsync_enabled, true),
            application:set_env(imboy, cluster_nodes, []),
            ?assertEqual(old, imboy_cache:memo(fun() -> old end, Key, 120)),
            sys:replace_state(imboy_cache_sync, fun(S) ->
                registered = imboy_cache_sync_proto:register_incarnation(Peer, <<91:128>>),
                S
            end),
            ?assertEqual(fresh, imboy_cache:memo(fun() -> fresh end, Key, 120))
        after
            imboy_cache_sync_apply:delete_metadata({peer_incarnation, Peer}),
            depcache:flush(imboy_cache),
            restore(dsync_enabled, OldSync),
            restore(cluster_nodes, OldNodes)
        end
    end).

equal_revision_conflicting_content_cannot_be_an_applied_effect_test_() ->
    ?TEST_WITH_APP(fun() ->
        Key = {actual_effect_test, equal},
        {_, Boot} = imboy_cache_sync:node_boot(),
        Exp = erlang:system_time(millisecond) + 60000,
        Rev = {1, node()},
        Origin = {node(), Boot, 1},
        Event = imboy_cache_sync_proto:event(set, Key, old, Exp, Rev, 1, Boot),
        try
            ok = depcache:set(Key, old, 60, [], imboy_cache),
            ok = imboy_cache_sync_apply:store_revision(
                Key,
                Rev,
                Exp,
                live,
                imboy_cache_sync_proto:epoch(),
                Origin
            ),
            ?assert(imboy_cache_sync_coverage:idempotent_event(Event)),
            ?assertNot(imboy_cache_sync_coverage:idempotent_event(Event#{value := forged})),
            ?assertNot(imboy_cache_sync_coverage:idempotent_event(Event#{operation := delete})),
            ?assertEqual(
                snapshot_content_conflict,
                imboy_cache_sync_apply:snapshot_budget_error(
                    [{with_origin, Origin, {Key, forged, Exp, Rev}}], imboy_cache_sync_proto:epoch()
                )
            ),
            ?assertEqual({ok, old}, gen_server:call(imboy_cache, {get, Key}))
        after
            depcache:flush(Key, imboy_cache),
            imboy_cache_sync_apply:delete_metadata({rev, Key})
        end
    end).

unsafe_authority_request_cannot_join_pre_revocation_loader_test_() ->
    ?TEST_WITH_APP(fun() ->
        OldSync = application:get_env(imboy, dsync_enabled),
        OldNodes = application:get_env(imboy, cluster_nodes),
        Key = {auth_session_epoch, unsafe_waiter},
        Parent = self(),
        Count = counters:new(1, []),
        Loader = fun() ->
            counters:add(Count, 1, 1),
            case counters:get(Count, 1) of
                1 ->
                    Parent ! {authority_blocked, self()},
                    receive
                        resume -> old
                    end;
                _ ->
                    fresh
            end
        end,
        application:set_env(imboy, dsync_enabled, true),
        application:set_env(imboy, cluster_nodes, [node(), 'missing_expected@synthetic']),
        Producer = spawn(fun() -> Parent ! {authority_first, imboy_cache:memo(Loader, Key, 120)} end),
        try
            receive
                {authority_blocked, Producer} -> ok
            after 1000 -> error(no_authority_loader)
            end,
            Waiter = spawn(fun() ->
                Parent ! {authority_after_revoke, imboy_cache:memo(Loader, Key, 120)}
            end),
            try
                receive
                    {authority_after_revoke, Result} -> ?assertEqual(fresh, Result)
                after 1000 -> error(authority_joined_old_query)
                end
            after
                exit(Waiter, kill)
            end
        after
            Producer ! resume,
            receive
                {authority_first, _} -> ok
            after 1000 -> exit(Producer, kill)
            end,
            restore(dsync_enabled, OldSync),
            restore(cluster_nodes, OldNodes)
        end
    end).

sensitive_boundary() ->
    OldNodes = application:get_env(imboy, cluster_nodes),
    OldSync = application:get_env(imboy, dsync_enabled),
    OldFences = application:get_env(imboy, cache_fenced_namespaces),
    Key = {auth_session_epoch, synthetic_health_uid},
    try
        ready = wait_ready(100),
        application:set_env(imboy, dsync_enabled, false),
        application:set_env(imboy, cache_fenced_namespaces, []),
        ok = imboy_cache:set(Key, 0, 120),
        application:set_env(imboy, dsync_enabled, true),
        application:set_env(imboy, cluster_nodes, [node(), 'missing_expected@synthetic']),
        ?assertEqual(undefined, imboy_cache:get(Key)),
        ?assertEqual(1, imboy_cache:memo(fun() -> 1 end, Key, 120)),
        ?assertEqual(2, imboy_cache:memo(fun() -> 2 end, Key, 120)),
        application:set_env(imboy, cache_fenced_namespaces, [auth_session_epoch]),
        ?assertEqual(3, imboy_cache:memo(fun() -> 3 end, Key, 120)),
        ?assertEqual(4, imboy_cache:memo(fun() -> 4 end, Key, 120)),
        application:set_env(imboy, dsync_enabled, false),
        ?assertEqual(undefined, imboy_cache:get(Key))
    after
        depcache:flush(Key, imboy_cache),
        restore(cluster_nodes, OldNodes),
        restore(dsync_enabled, OldSync),
        restore(cache_fenced_namespaces, OldFences)
    end.

physical_dependency_flush_reaches_memo_and_set_test_() ->
    ?TEST_WITH_APP(fun() ->
        Old = application:get_env(imboy, cache_fenced_namespaces),
        K1 = {dependency_test, memo},
        K2 = {dependency_test, set},
        Tag = {plain_tag, dependency},
        try
            application:set_env(imboy, cache_fenced_namespaces, [dependency_test]),
            ?assertEqual(old, imboy_cache:memo(fun() -> old end, K1, 120, [Tag])),
            ok = imboy_cache:set(K2, old, 120, [Tag]),
            ?assertEqual({ok, old}, imboy_cache:get(K2)),
            ok = imboy_cache:flush(Tag),
            ?assertEqual(undefined, imboy_cache:get(K2)),
            ?assertEqual(new, imboy_cache:memo(fun() -> new end, K1, 120, [Tag]))
        after
            depcache:flush(imboy_cache),
            restore(cache_fenced_namespaces, Old)
        end
    end).

new_dependency_waiter_rejects_old_loader_production_proof_test_() ->
    ?TEST_WITH_APP(fun dependency_waiter_boundary/0).

dependency_waiter_boundary() ->
    Old = application:get_env(imboy, cache_fenced_namespaces),
    Key = {dependency_test, raced},
    Tag = {plain_tag, raced},
    Parent = self(),
    Count = counters:new(1, []),
    Loader = fun() ->
        counters:add(Count, 1, 1),
        case counters:get(Count, 1) of
            1 ->
                Parent ! {blocked_loader, self()},
                receive
                    resume -> old
                end;
            _ ->
                fresh
        end
    end,
    try
        application:set_env(imboy, cache_fenced_namespaces, [dependency_test]),
        Producer = spawn(fun() -> Parent ! {produced, imboy_cache:memo(Loader, Key, 120, [Tag])} end),
        receive
            {blocked_loader, Producer} -> ok
        after 1000 -> error(no_loader)
        end,
        {node_boot, boot, Boot} = {node_boot, boot, imboy_cache_sync:node_boot()},
        Physical = {cache_v2, Key, {Boot, imboy_cache_sync:generation(Key)}},
        _ = imboy_cache_sync:next_dep_generation(Tag),
        Waiter = spawn(fun() ->
            Parent ! {waited, imboy_cache:memo(fun() -> fresh end, Key, 120, [Tag])}
        end),
        wait_dependency_waiter(Physical, 100),
        Producer ! resume,
        receive
            {produced, Result1} -> ?assertEqual(fresh, Result1)
        after 2000 -> error(producer_stuck)
        end,
        receive
            {waited, Result2} -> ?assertEqual(fresh, Result2)
        after 2000 ->
            exit(Waiter, kill),
            error(waiter_stuck)
        end,
        ?assertEqual({ok, fresh}, imboy_cache:get(Key))
    after
        depcache:flush(imboy_cache),
        restore(cache_fenced_namespaces, Old)
    end.

wait_dependency_waiter(_, 0) ->
    error(waiter_did_not_join);
wait_dependency_waiter(Key, Left) ->
    Pending = element(5, sys:get_state(imboy_cache)),
    case maps:get(Key, Pending, undefined) of
        {_, [_ | _], _} ->
            ok;
        _ ->
            timer:sleep(10),
            wait_dependency_waiter(Key, Left - 1)
    end.

fenced_getter_preserves_all_native_subkey_shapes_test_() ->
    ?TEST_WITH_APP(fun() ->
        Old = application:get_env(imboy, cache_fenced_namespaces),
        Key = {dependency_test, shapes},
        try
            application:set_env(imboy, cache_fenced_namespaces, [dependency_test]),
            lists:foreach(
                fun({Body, Sub, Expected}) ->
                    ok = imboy_cache:set(Key, Body, 120),
                    ?assertEqual({ok, Body}, imboy_cache:get(Key)),
                    ?assertEqual({ok, Expected}, imboy_cache:get(Key, Sub)),
                    ?assertEqual({ok, Expected}, imboy_cache:get_subkey(Key, Sub))
                end,
                [
                    {#{a => 1}, a, 1},
                    {[{a, 2}], a, 2},
                    {[one, two], 2, two},
                    {{one, two}, 2, two},
                    {array:from_list([one, two]), 1, two},
                    {{rsc_list, [#{a => 3}]}, a, 3},
                    {#{}, absent, undefined},
                    {{cache_fenced_value, user_payload, payload}, 1, cache_fenced_value}
                ]
            )
        after
            depcache:flush(imboy_cache),
            restore(cache_fenced_namespaces, Old)
        end
    end).

capability_boundary() ->
    OldCfg = application:get_env(imboy, cache_replication),
    OldRegistry = application:get_env(imboy, cache_registry_overrides),
    OldNodes = application:get_env(imboy, cluster_nodes),
    Peer = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    Key = {capability_test, key},
    try
        application:set_env(imboy, cache_replication, #{whitelist => [capability_test]}),
        application:set_env(imboy, cache_registry_overrides, #{
            capability_test => {replicate, false}
        }),
        ready = wait_ready(100),
        ok = imboy_cache:put(Key, original, 120, replicate),
        Before = sys:get_state(imboy_cache_sync),
        Rows = lists:sort(ets:tab2list(imboy_cache_meta)),
        application:set_env(imboy, cluster_nodes, [node(), 'missing_expected@synthetic']),
        ?assertEqual(
            {error, incompatible_peers}, imboy_cache:put(Key, isolated, 120, replicate)
        ),
        ?assertEqual({error, incompatible_peers}, imboy_cache:put_delete(Key)),
        ?assertEqual(Before, sys:get_state(imboy_cache_sync)),
        ?assertEqual(Rows, lists:sort(ets:tab2list(imboy_cache_meta))),
        application:set_env(imboy, cluster_nodes, malformed),
        ?assertEqual({error, incompatible_peers}, imboy_cache:put_delete(Key)),
        application:set_env(imboy, cluster_nodes, ["invalid_node"]),
        ?assertEqual({error, incompatible_peers}, imboy_cache:put_delete(Key)),
        application:set_env(imboy, cluster_nodes, [node() | malformed_tail]),
        ?assertEqual({error, incompatible_peers}, imboy_cache:put_delete(Key)),
        application:set_env(imboy, cluster_nodes, lists:duplicate(66, node())),
        ?assertEqual({error, incompatible_peers}, imboy_cache:put_delete(Key)),
        application:set_env(imboy, cluster_nodes, [node()]),
        ?assert(imboy_cache_sync:replication_capable()),
        application:set_env(imboy, cluster_nodes, []),
        ok = syn:join(imboy_cache, dsync_handler, Peer, #{}),
        ?assertEqual(
            {error, incompatible_peers}, imboy_cache:put(Key, replacement, 120, replicate)
        ),
        ?assertEqual({error, incompatible_peers}, imboy_cache:put_delete(Key)),
        ?assertEqual(Before, sys:get_state(imboy_cache_sync)),
        ?assertEqual(Rows, lists:sort(ets:tab2list(imboy_cache_meta))),
        ok = syn:join(imboy_cache, dsync_handler, Peer, malformed_metadata),
        ?assertEqual({error, incompatible_peers}, imboy_cache:put_delete(Key)),
        ?assertEqual(Before, sys:get_state(imboy_cache_sync)),
        ?assertEqual(Rows, lists:sort(ets:tab2list(imboy_cache_meta))),
        ?assertEqual({ok, original}, imboy_cache:get(Key)),
        ?assertEqual(
            {error, incompatible_peers},
            imboy_cache_sync:broadcast(#{protocol => 2, operation => set})
        ),
        ok = imboy_cache:invalidate(Key),
        ?assertEqual(undefined, imboy_cache:get(Key)),
        ok = syn:leave(imboy_cache, dsync_handler, Peer),
        ok = imboy_cache:put(Key, restored, 120, replicate),
        ?assertEqual({ok, restored}, imboy_cache:get(Key))
    after
        syn:leave(imboy_cache, dsync_handler, Peer),
        Peer ! stop,
        restore(cache_replication, OldCfg),
        restore(cache_registry_overrides, OldRegistry),
        restore(cluster_nodes, OldNodes)
    end.

wait_ready(0) ->
    error(cache_not_ready);
wait_ready(N) ->
    case imboy_cache_sync:status() of
        ready ->
            ready;
        _ ->
            timer:sleep(25),
            wait_ready(N - 1)
    end.

restore(Key, undefined) -> application:unset_env(imboy, Key);
restore(Key, {ok, Value}) -> application:set_env(imboy, Key, Value).
