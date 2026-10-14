%%% 失效序列回归：无需应用/PG，真实同步回调与接收校验，通信由测试捕获。
-module(imboy_cache_invalidation_tests).
-include_lib("eunit/include/eunit.hrl").
-include("imboy_cache_sync_state.hrl").

invalidation_aborts_prepared_snapshot_sessions_test() ->
    lists:foreach(
        fun(Message) ->
            with_sync_fixture(fun() ->
                Round = <<7:96>>,
                ets:insert(?META_TABLE, {{snapshot_sender, self()}, #{round => Round}}),
                State = #state{boot_id = <<1:128>>, status = ready},
                meck:expect(depcache, flush, 1, fun(_) -> ok end),
                meck:expect(syn, publish, 3, fun(_, _, _) -> {ok, 1} end),
                invalidate_sender(Message, State),
                receive
                    {cache_snapshot_abort, _, Round, snapshot_invalidated} -> ok
                after 0 -> error(missing_invalidation_abort)
                end,
                ?assertEqual([], ets:lookup(?META_TABLE, {snapshot_sender, self()})),
                ok = imboy_cache_sync_snapshot:continue_snapshot(self(), Round, 2),
                receive
                    {cache_snapshot_done, _, Round, _, _} -> error(stale_done)
                after 0 -> ok
                end
            end)
        end,
        [{flush, {cache_invalidation_test, key}}, flush, local, v2, receipt]
    ).

invalidate_sender(local, State) ->
    {reply, ok, _} = imboy_cache_sync:handle_call(
        {invalidate, {cache_invalidation_test, key}}, {self(), make_ref()}, State
    ),
    ok;
invalidate_sender(Kind, State) ->
    Peer = 'invalidate_sender@synthetic',
    Boot = <<2:128>>,
    ets:insert(?META_TABLE, {{peer_incarnation, Peer}, Boot}),
    Key = {cache_invalidation_test, key},
    Message =
        case Kind of
            v2 ->
                (imboy_cache_sync_proto:event(
                    invalidate,
                    Key,
                    undefined,
                    0,
                    {1, Peer},
                    1,
                    Boot
                ))#{
                    origin_node => Peer
                };
            receipt ->
                {legacy_invalidation_seq, Peer, Boot, 1, imboy_cache_sync_proto:config_hash(),
                    imboy_cache_sync_proto:registry_hash(), Key};
            _ ->
                Kind
        end,
    {noreply, _} = imboy_cache_sync:handle_info({cache_sync, Message}, State),
    ok.

invalidation_uses_shared_origin_sequence_test() ->
    with_sync_fixture(fun() ->
        Controller = self(),
        meck:expect(syn, publish, 3, fun(_, _, {cache_sync, Event}) ->
            Controller ! {published, Event},
            {ok, 1}
        end),
        Boot = <<0:128>>,
        S0 = #state{boot_id = Boot, counter = 7, origin_seq = 7, status = ready},
        {reply, ok, S1} = imboy_cache_sync:handle_call(
            {invalidate, {cache_invalidation_test, first}}, {self(), make_ref()}, S0
        ),
        {reply, ok, S2} = imboy_cache_sync:handle_call(
            {invalidate, {cache_invalidation_test, second}}, {self(), make_ref()}, S1
        ),
        ?assertEqual(9, S2#state.origin_seq),
        ?assertEqual([8, 9], [maps:get(origin_seq, receive_event()) || _ <- [1, 2]]),
        %% v2 信封已覆盖新节点；legacy 兼容帧按成员能力单独路由。
        ?assertEqual(2, meck:num_calls(syn, publish, 3))
    end).

legacy_flush_targets_only_compatible_peers_test() ->
    with_sync_fixture(fun() ->
        Parent = self(),
        LegacyPeer = spawn(fun() -> invalidation_test_peer(Parent) end),
        V2Peer = spawn(fun() -> invalidation_test_peer(Parent) end),
        meck:expect(syn, members, 2, fun(_, _) ->
            [
                {self(), #{cache_protocol => 2}},
                {LegacyPeer, #{}},
                {V2Peer, #{cache_protocol => 2}}
            ]
        end),
        Key = {cache_invalidation_test, peer_routing},
        try
            Clock0 = fence_clock(),
            S0 = #state{boot_id = <<0:128>>, status = ready},
            {reply, ok, S1} = imboy_cache_sync:handle_call(
                {invalidate, Key}, {self(), make_ref()}, S0
            ),
            Clock1 = fence_clock(),
            ?assertEqual(Clock0 + 2, Clock1),
            ?assertEqual({cache_sync, {flush, Key}}, receive_invalidation_peer(LegacyPeer)),
            ?assertEqual(none, receive_invalidation_peer(V2Peer)),
            application:set_env(imboy, cache_sync_protocol, legacy),
            {reply, ok, _S2} = imboy_cache_sync:handle_call(
                {invalidate, Key}, {self(), make_ref()}, S1
            ),
            ?assertEqual({cache_sync, {flush, Key}}, receive_invalidation_peer(LegacyPeer)),
            ?assertEqual({cache_sync, {flush, Key}}, receive_invalidation_peer(V2Peer))
        after
            LegacyPeer ! stop,
            V2Peer ! stop
        end
    end).

invalidation_test_peer(Parent) ->
    receive
        stop ->
            ok;
        Message ->
            Parent ! {invalidation_peer_message, self(), Message},
            invalidation_test_peer(Parent)
    end.

receive_invalidation_peer(Pid) ->
    receive
        {invalidation_peer_message, Pid, Message} -> Message
    after 1000 ->
        none
    end.

fence_clock() ->
    case ets:lookup(?META_TABLE, fence_clock) of
        [{fence_clock, Clock}] -> Clock;
        [] -> 0
    end.

missing_invalidation_advances_fence_test() ->
    with_sync_fixture(fun() ->
        Origin = 'invalidation_peer@synthetic',
        Boot = <<1:128>>,
        ets:insert(?META_TABLE, {{peer_incarnation, Origin}, Boot}),
        Key = {cache_invalidation_test, receiving},
        S0 = #state{boot_id = <<0:128>>, status = ready},
        Event0 = imboy_cache_sync_proto:event(invalidate, Key, undefined, 0, {1, Origin}, 1, Boot),
        Event = Event0#{origin_node => Origin},
        {ok, S1} = imboy_cache_sync_apply:apply_v2(Event, S0),
        ?assertEqual(2, imboy_cache_sync:generation(Key)),
        %% 中间 seq=2 丢失，seq=3 必须同时补偿缺口和执行本次失效。
        {ok, _} = imboy_cache_sync_apply:apply_v2(Event#{origin_seq => 3}, S1),
        ?assertEqual(5, imboy_cache_sync:generation(Key)),
        ?assertEqual([{{seq, {Origin, Boot}}, 3}], ets:lookup(?META_TABLE, {seq, {Origin, Boot}}))
    end).

gap_invalidates_other_keys_and_inflight_generation_test() ->
    with_sync_fixture(fun() ->
        {ok, Cache} = depcache:start_link(imboy_cache, #{}),
        try
            A = {cache_invalidation_test, cached_a},
            B = {cache_invalidation_test, cached_b},
            Pending = {cache_invalidation_test, loading},
            Loader = fun() -> old end,
            ?assertEqual(old, imboy_cache:memo(Loader, A, 60, [])),
            ?assertEqual(old, imboy_cache:memo(Loader, B, 60, [])),
            G0 = imboy_cache_sync:generation(Pending),
            Origin = {'multi_key_peer@synthetic', <<2:128>>},
            ok = imboy_cache_sync_apply:detect_gap(Origin, 1, A),
            %% 后续消息的 Key 不属于 fenced 域，仍不能遗漏先前丢失的失效。
            gap = imboy_cache_sync_apply:detect_gap(Origin, 3, {unmanaged, later}),
            ?assert(imboy_cache_sync:generation(Pending) > G0),
            ?assertEqual(new, imboy_cache:memo(fun() -> new end, A, 60, [])),
            ?assertEqual(new, imboy_cache:memo(fun() -> new end, B, 60, []))
        after
            gen_server:stop(Cache)
        end
    end).

restarted_origin_does_not_inherit_old_sequence_test() ->
    with_sync_fixture(fun() ->
        Node = 'restarted_peer@synthetic',
        OldBoot = <<3:128>>,
        NewBoot = <<4:128>>,
        ets:insert(?META_TABLE, [
            {{peer_incarnation, Node}, OldBoot}, {{seq, {Node, OldBoot}}, 100}
        ]),
        changed = imboy_cache_sync_proto:register_incarnation(Node, NewBoot),
        ?assertEqual([], ets:lookup(?META_TABLE, {seq, {Node, OldBoot}})),
        Key = {cache_invalidation_test, restarted},
        E0 = imboy_cache_sync_proto:event(invalidate, Key, undefined, 0, {1, Node}, 1, NewBoot),
        Event = E0#{origin_node => Node},
        S0 = #state{boot_id = <<0:128>>, status = ready},
        {ok, S1} = imboy_cache_sync_apply:apply_v2(Event, S0),
        {ok, S2} = imboy_cache_sync_apply:apply_v2(
            Event#{origin_seq => 100, origin_boot_id => OldBoot}, S1
        ),
        ?assertEqual([], ets:lookup(?META_TABLE, {seq, {Node, OldBoot}})),
        G1 = imboy_cache_sync:generation(Key),
        {ok, _} = imboy_cache_sync_apply:apply_v2(Event#{origin_seq => 3}, S2),
        ?assertEqual(G1 + 3, imboy_cache_sync:generation(Key))
    end).

sync_restart_isolates_cached_and_inflight_values_test() ->
    with_sync_fixture(fun() ->
        {ok, Cache} = depcache:start_link(imboy_cache, #{}),
        Controller = self(),
        Count = counters:new(1, []),
        Key = {cache_invalidation_test, restart_loader},
        Cached = {cache_invalidation_test, restart_cached},
        ?assertEqual(old, imboy_cache:memo(fun() -> old end, Cached, 60)),
        Loader = fun() ->
            counters:add(Count, 1, 1),
            case counters:get(Count, 1) of
                1 ->
                    Controller ! {started, self()},
                    receive
                        release -> old
                    end;
                _ ->
                    new
            end
        end,
        Worker = spawn(fun() -> Controller ! {result, imboy_cache:memo(Loader, Key)} end),
        try
            receive
                {started, Worker} -> ok
            after 1000 -> error(loader_not_started)
            end,
            %% 重建同步 owner 的元数据；depcache 和旧 loader 均持续存活。
            ets:delete(?META_TABLE),
            ets:new(?META_TABLE, [named_table, public, set]),
            ets:insert(?META_TABLE, {node_boot, {node(), <<9:128>>}}),
            ?assertEqual(new, imboy_cache:memo(fun() -> new end, Cached, 60)),
            Worker ! release,
            receive
                {result, Result} -> ?assertEqual(new, Result)
            after 1000 -> error(loader_did_not_finish)
            end
        after
            exit(Worker, kill),
            gen_server:stop(Cache)
        end
    end).

receive_event() ->
    receive
        {published, #{protocol := 2} = Event} -> Event
    after 1000 -> error(missing_invalidation_event)
    end.

last_lost_event_is_compensated_by_periodic_watermark_test() ->
    with_sync_fixture(fun() ->
        Origin = 'tail_peer@synthetic',
        Boot = <<5:128>>,
        ets:insert(?META_TABLE, [
            {{peer_incarnation, Origin}, Boot}, {{seq, {Origin, Boot}}, 1}
        ]),
        Key = {cache_invalidation_test, lost_tail},
        G0 = imboy_cache_sync:generation(Key),
        State = #state{boot_id = <<0:128>>, status = ready},
        Frame =
            {cache_invalidation_watermark, Origin, Boot, 2, imboy_cache_sync_proto:config_hash(),
                imboy_cache_sync_proto:registry_hash()},
        {noreply, S1} = imboy_cache_sync:handle_info(Frame, State),
        ?assertEqual(discovering, S1#state.status),
        ?assert(S1#state.requires_authority),
        ?assertEqual(G0 + 1, imboy_cache_sync:generation(Key)),
        {noreply, S1} = imboy_cache_sync:handle_info(Frame, S1),
        ?assertEqual(G0 + 1, imboy_cache_sync:generation(Key)),
        %% 旧化身报告不能推进当前水位，也不能清理新代缓存。
        {noreply, S1} = imboy_cache_sync:handle_info(setelement(3, Frame, <<6:128>>), S1),
        erlang:cancel_timer(S1#state.recovery_timer),
        ?assertEqual(G0 + 1, imboy_cache_sync:generation(Key))
    end).

version_reply_includes_origin_sequence_and_checkpoint_test() ->
    with_sync_fixture(fun() ->
        State = #state{boot_id = <<7:128>>, counter = 99, origin_seq = 3, status = ready},
        {noreply, State} = imboy_cache_sync:handle_info(
            {cache_version_request, self(), node()}, State
        ),
        receive
            {cache_version_report, _, 99, _, _, _, <<7:128>>, true, _, 3} -> ok
        after 1000 -> error(missing_origin_sequence_report)
        end,
        receive
            {cache_invalidation_watermark, _, <<7:128>>, 3, _, _} -> ok
        after 1000 -> error(missing_sequence_checkpoint)
        end
    end).

legacy_invalidation_also_advances_checkpoint_test() ->
    with_sync_fixture(fun() ->
        application:set_env(imboy, cache_sync_protocol, legacy),
        Controller = self(),
        LegacyPeer = spawn(fun() -> invalidation_test_peer(Controller) end),
        meck:expect(syn, members, 2, fun(_, _) -> [{self(), #{}}, {LegacyPeer, #{}}] end),
        Key = {cache_invalidation_test, legacy},
        try
            meck:expect(syn, publish, 3, fun(_, _, {cache_sync, Message}) ->
                Controller ! {legacy_message, Message},
                {ok, 1}
            end),
            State = #state{boot_id = <<8:128>>, status = ready},
            {reply, ok, Next} = imboy_cache_sync:handle_call(
                {invalidate, Key}, {self(), make_ref()}, State
            ),
            ?assertEqual(1, Next#state.origin_seq),
            receive
                {legacy_message,
                    {legacy_invalidation_seq, Source, <<8:128>>, 1, Hash, Registry, Key}} ->
                    ?assertEqual(node(), Source),
                    ?assertEqual(imboy_cache_sync_proto:config_hash(), Hash),
                    ?assertEqual(imboy_cache_sync_proto:registry_hash(), Registry)
            after 1000 -> error(missing_legacy_sequence_receipt)
            end,
            ?assertEqual({cache_sync, {flush, Key}}, receive_invalidation_peer(LegacyPeer)),
            ?assertEqual(1, meck:num_calls(syn, publish, 3))
        after
            LegacyPeer ! stop
        end
    end).

with_sync_fixture(Test) ->
    pure_mock_fixture:run(fun() -> with_sync_fixture_local(Test) end).

with_sync_fixture_local(Test) ->
    Names = [
        cache_sync_protocol,
        cache_fenced_namespaces,
        dsync_enabled,
        cluster_nodes,
        cache_replication,
        cache_registry_overrides
    ],
    Previous = [{N, application:get_env(imboy, N)} || N <- Names],
    ets:new(?META_TABLE, [named_table, public, set]),
    ets:insert(?META_TABLE, [{node_boot, {node(), <<1:128>>}}, {sync_origin_seq, 0}]),
    try
        application:set_env(imboy, cache_sync_protocol, v2),
        application:set_env(imboy, cache_fenced_namespaces, [cache_invalidation_test]),
        application:set_env(imboy, dsync_enabled, false),
        meck:new(depcache, [unstick, passthrough, no_link]),
        meck:new(syn, [unstick, passthrough, no_link]),
        meck:expect(syn, members, 2, fun(imboy_cache, dsync_handler) ->
            [{self(), #{cache_protocol => 2}}]
        end),
        meck:expect(depcache, flush, 2, fun(_, _) -> ok end),
        Test()
    after
        imboy_cache_source_cut_tests:drain(),
        lists:foreach(
            fun(Module) ->
                try
                    meck:unload(Module)
                catch
                    error:{not_mocked, _} -> ok
                end
            end,
            [syn, depcache]
        ),
        ets:delete(?META_TABLE),
        lists:foreach(
            fun
                ({N, undefined}) -> application:unset_env(imboy, N);
                ({N, {ok, Value}}) -> application:set_env(imboy, N, Value)
            end,
            Previous
        )
    end.

malformed_replication_envelopes_are_rejected_without_application_test() ->
    with_sync_fixture(fun() ->
        application:set_env(imboy, cache_registry_overrides, #{protocol_test => {replicate, false}}),
        application:set_env(imboy, cache_replication, #{whitelist => [protocol_test]}),
        Key = {protocol_test, synthetic},
        Event = imboy_cache_sync_proto:event(
            set,
            Key,
            value,
            erlang:system_time(millisecond) + 60000,
            {1, node()},
            1,
            <<0:128>>
        ),
        ?assertEqual(ok, imboy_cache_sync_proto:validate(Event)),
        ?assertEqual(ok, imboy_cache_sync_proto:validate(Event#{value => [synthetic | tail]})),
        ?assertEqual(
            ok,
            imboy_cache_sync_proto:validate(Event#{
                expires_at_ms => erlang:system_time(millisecond) - 1000
            })
        ),
        ?assertEqual(
            ok,
            imboy_cache_sync_proto:validate(Event#{
                expires_at_ms => erlang:system_time(millisecond) + 1000
            })
        ),
        Bad = [
            {Event#{expires_at_ms => erlang:system_time(millisecond) + 660000}, ttl_too_large},
            {
                Event#{
                    expires_at_ms => erlang:system_time(millisecond) -
                        imboy_cache_policy:tombstone_ttl_sec() * 1000 - 1
                },
                expired
            },
            {
                Event#{
                    operation => delete, expires_at_ms => erlang:system_time(millisecond) + 1000
                },
                expired
            },
            {maps:remove(value, Event), invalid_value},
            {Event#{value => self()}, value_type_forbidden},
            {Event#{value => #{nested => make_ref()}}, value_type_forbidden},
            {Event#{value => [synthetic | self()]}, value_type_forbidden},
            {Event#{value => [synthetic | make_ref()]}, value_type_forbidden},
            {Event#{key => {protocol_test, [synthetic | self()]}}, key_type_forbidden},
            {Event#{revision => {<<"counter">>, node()}}, invalid_revision},
            {Event#{revision => {-1, node()}}, invalid_revision},
            {Event#{origin_seq => -1}, invalid_origin_seq},
            {maps:remove(origin_node, Event), invalid_origin_node},
            {Event#{origin_boot_id => <<0>>}, invalid_boot_id},
            {Event#{namespace => other_namespace}, namespace_mismatch},
            {maps:remove(expires_at_ms, Event), invalid_expiry},
            {Event#{key => {protocol_test, self()}}, key_type_forbidden},
            {Event#{key => {protocol_test, binary:copy(<<0>>, 33000)}}, key_too_large}
        ],
        State = #state{boot_id = <<1:128>>, status = ready},
        lists:foreach(
            fun({Envelope, Reason}) ->
                ?assertEqual({error, Reason}, imboy_cache_sync_proto:validate(Envelope)),
                ?assertEqual({reject, Reason}, imboy_cache_sync_apply:apply_v2(Envelope, State))
            end,
            Bad
        ),
        ?assertEqual(0, meck:num_calls(depcache, flush, 2)),
        ?assertEqual([], ets:match_object(?META_TABLE, {{peer_incarnation, '_'}, '_'}))
    end).

generation_capacity_rollover_never_reuses_old_values_test() ->
    with_sync_fixture(fun() ->
        application:set_env(imboy, cache_replication, #{n_max => 2, gen_max_bytes => 128}),
        {ok, Cache} = depcache:start_link(imboy_cache, #{}),
        try
            Key = {cache_invalidation_test, capacity_cached},
            ?assertEqual(old, imboy_cache:memo(fun() -> old end, Key, 60)),
            lists:foreach(
                fun(N) ->
                    Before = imboy_cache_sync:generation({cache_invalidation_test, N}),
                    After = imboy_cache_sync:bump_generation({cache_invalidation_test, N}),
                    ?assert(After > Before),
                    Rows = ets:match_object(?META_TABLE, {{gen, '_'}, '_'}),
                    ?assert(length(Rows) =< 2),
                    ?assert(lists:sum([erlang:external_size(R) || R <- Rows]) =< 128)
                end,
                lists:seq(1, 50)
            ),
            ?assertEqual(new, imboy_cache:memo(fun() -> new end, Key, 60))
        after
            gen_server:stop(Cache)
        end
    end).

oversized_generation_key_uses_constant_space_fallback_test() ->
    with_sync_fixture(fun() ->
        application:set_env(imboy, cache_replication, #{gen_max_bytes => 64}),
        Key = {cache_invalidation_test, binary:copy(<<0>>, 1024)},
        G = imboy_cache_sync:bump_generation(Key),
        ?assert(G > 0),
        ?assertEqual(G, imboy_cache_sync:generation(Key)),
        ?assertEqual([], ets:match_object(?META_TABLE, {{gen, '_'}, '_'}))
    end).

snapshot_pages_reject_before_any_entry_is_applied_test() ->
    with_sync_fixture(fun() ->
        meck:expect(depcache, set, 5, fun(_, _, _, _, _) -> ok end),
        Valid = snapshot_entry(first),
        Cases = [
            {[Valid, malformed], #{}, invalid_snapshot_entry},
            {
                [Valid, {{unknown_snapshot, key}, value, element(3, Valid), {1, node()}}],
                #{},
                invalid_snapshot_entry
            },
            {[Valid, snapshot_entry(second)], #{p_max => 1}, page_overflow},
            {[Valid], #{p_max_bytes => 1}, page_bytes_overflow},
            {[Valid], #{b_max_bytes => 1}, buffer_bytes_overflow},
            {[Valid], #{n_max => 1}, index_full}
        ],
        %% 已有一个索引：新快照不得绕过 Nmax。
        ets:insert(?META_TABLE, [{rindex_count, 1}, {{rindex, {snapshot_test, existing}}, 0}]),
        lists:foreach(
            fun({Entries, Limits, Reason}) ->
                snapshot_policy(Limits),
                Peer = 'snapshot_peer@synthetic',
                State = snapshot_round(Peer),
                Round = maps:get(round, State),
                Digest = imboy_cache_sync_snapshot:page_digest(Round, 1, Entries),
                ?assertEqual(
                    {abort, Reason, State},
                    imboy_cache_sync_snapshot:handle_page(
                        Peer, Round, 1, Entries, Digest, false, State
                    )
                )
            end,
            Cases
        ),
        ?assertEqual(0, meck:num_calls(depcache, set, 5))
    end).

snapshot_buffer_budget_is_released_between_applied_pages_test() ->
    with_sync_fixture(fun() ->
        snapshot_policy(#{b_max => 1}),
        meck:expect(depcache, set, 5, fun(_, _, _, _, _) -> ok end),
        A = 'snapshot_a@synthetic',
        B = 'snapshot_b@synthetic',
        S0 = snapshot_authority(snapshot_round(A), B),
        R = maps:get(round, S0),
        E1 = [snapshot_entry(first)],
        {ok, S1} = imboy_cache_sync_snapshot:handle_page(
            A,
            R,
            1,
            E1,
            imboy_cache_sync_snapshot:page_digest(R, 1, E1),
            false,
            S0
        ),
        E2 = [snapshot_entry(second)],
        {ok, S2} = imboy_cache_sync_snapshot:handle_page(
            B, R, 1, E2, imboy_cache_sync_snapshot:page_digest(R, 1, E2), false, S1
        ),
        ?assertEqual(2, imboy_cache_sync_snapshot:applied_count(S2)),
        ?assertEqual(2, meck:num_calls(depcache, set, 5))
    end).

snapshot_shape_deadline_and_watermark_are_checked_test() ->
    with_sync_fixture(fun() ->
        snapshot_policy(#{}),
        Peer = 'snapshot_peer@synthetic',
        S0 = snapshot_round(Peer),
        R = maps:get(round, S0),
        E = [snapshot_entry(first)],
        ?assertEqual(
            {abort, invalid_snapshot_page, S0},
            imboy_cache_sync_snapshot:handle_page(
                Peer, R, 1, [synthetic | improper], 0, false, S0
            )
        ),
        Expired = S0#{deadline_ms := erlang:system_time(millisecond) - 1},
        ?assertEqual(
            {abort, deadline_expired, Expired},
            imboy_cache_sync_snapshot:handle_page(
                Peer, R, 1, E, imboy_cache_sync_snapshot:page_digest(R, 1, E), false, Expired
            )
        ),
        ?assertEqual(
            {abort, invalid_watermark, S0},
            imboy_cache_sync_snapshot:handle_done(
                Peer, R, <<"bad">>, 0, S0
            )
        )
    end).

oversized_snapshot_entry_aborts_sender_without_sending_a_page_test() ->
    with_sync_fixture(fun() ->
        snapshot_policy(#{p_max_bytes => 64}),
        {Key, _Value, ExpiresAt, Rev} = snapshot_entry(large),
        ets:insert(?META_TABLE, [{{rindex, Key}, 0}, {{rev, Key}, {Rev, 1, ExpiresAt, live, 0}}]),
        meck:expect(depcache, get, 2, fun(_, _) -> {ok, binary:copy(<<0>>, 256)} end),
        Round = crypto:strong_rand_bytes(12),
        ?assertEqual(ok, imboy_cache_sync_snapshot:send_snapshot(self(), Round)),
        receive
            {cache_snapshot_abort, _, Round, oversized_entry} -> ok
        after 1000 -> error(missing_snapshot_abort)
        end,
        receive
            {cache_snapshot_page, _, _, _, _, _, _} -> error(oversized_page_sent)
        after 0 -> ok
        end
    end).

filtered_snapshot_tail_without_certificate_aborts_round_test() ->
    with_sync_fixture(fun() ->
        snapshot_policy(#{p_max => 2}),
        meck:expect(depcache, set, 5, fun(_, _, _, _, _) -> ok end),
        Entries = [snapshot_entry(N) || N <- [one, two, three]],
        Ceiling = lists:max([C || {_, _, _, {C, _}} <- Entries]),
        ets:insert(?META_TABLE, {sync_counter, Ceiling}),
        lists:foreach(
            fun({K, _, Exp, Rev}) ->
                ets:insert(?META_TABLE, [{{rindex, K}, 0}, {{rev, K}, {Rev, 1, Exp, live, 0}}])
            end,
            Entries
        ),
        Keys = [K || [K] <- ets:match(?META_TABLE, {{rindex, '$1'}, '_'})],
        Tail = lists:last(Keys),
        Controller = self(),
        meck:expect(depcache, get, 2, fun(K, _) ->
            case K =:= Tail of
                true -> {ok, Controller};
                false -> {ok, <<"value">>}
            end
        end),
        Round = crypto:strong_rand_bytes(12),
        ok = imboy_cache_sync_snapshot:send_snapshot(self(), Round),
        receive
            {cache_snapshot_page, _, Round, 1, Page, Digest, HasMore} ->
                ?assertEqual(2, length(Page)),
                ?assertEqual(false, HasMore),
                R0 = (snapshot_round(node()))#{round := Round, retained_obligations := []},
                ?assertMatch(
                    {abort, snapshot_content_conflict, _},
                    imboy_cache_sync_snapshot:handle_page(
                        node(), Round, 1, Page, Digest, HasMore, R0
                    )
                ),
                ?assertNot(imboy_cache_sync_snapshot:round_complete(R0)),
                receive
                    {cache_snapshot_done, _, Round, _, _} -> ok
                after 1000 -> error(missing_snapshot_done)
                end
        after 1000 -> error(missing_snapshot_page)
        end
    end).

snapshot_done_requires_page_total_and_authority_test() ->
    with_sync_fixture(fun() ->
        snapshot_policy(#{}),
        meck:expect(depcache, set, 5, fun(_, _, _, _, _) -> ok end),
        Peer = 'done_peer@synthetic',
        S0 = snapshot_round(Peer),
        R = maps:get(round, S0),
        E = [snapshot_entry(done)],
        {ok, S1} = imboy_cache_sync_snapshot:handle_page(
            Peer, R, 1, E, imboy_cache_sync_snapshot:page_digest(R, 1, E), false, S0
        ),
        lists:foreach(
            fun(Total) ->
                ?assertEqual(
                    {abort, incomplete_snapshot, S1},
                    imboy_cache_sync_snapshot:handle_done(Peer, R, 8, Total, S1)
                )
            end,
            [0, 2]
        ),
        ?assertEqual(
            {abort, invalid_page_total, S1},
            imboy_cache_sync_snapshot:handle_done(Peer, R, 8, bad, S1)
        ),
        {ok, S2} = imboy_cache_source_cut_tests:done(Peer, R, 8, 1, S1),
        ?assert(imboy_cache_sync_snapshot:round_complete(S2)),
        ?assertEqual({ok, S2}, imboy_cache_sync_snapshot:handle_done(Peer, R, 8, 1, S2)),
        ?assertEqual(
            {abort, conflicting_done, S2},
            imboy_cache_sync_snapshot:handle_done(Peer, R, 9, 1, S2)
        ),
        Pending = imboy_cache_sync_snapshot:expect_peer(
            imboy_cache_sync_snapshot:new_round(0, erlang:system_time(millisecond) + 60000), Peer
        ),
        ?assertEqual(
            {abort, invalid_authority, Pending},
            imboy_cache_sync_snapshot:handle_done(Peer, maps:get(round, Pending), 0, 0, Pending)
        )
    end).

recovering_peer_is_not_a_snapshot_authority_test() ->
    with_sync_fixture(fun() ->
        Peer = 'recovering_peer@synthetic',
        RoundSt = imboy_cache_sync_snapshot:expect_peer(
            imboy_cache_sync_snapshot:new_round(0, erlang:system_time(millisecond) + 60000), Peer
        ),
        S0 = #state{status = recovering, snapshot_round = RoundSt},
        S1 = imboy_cache_sync_recover:handle_version_report(
            Peer,
            2,
            0,
            imboy_cache_sync_proto:config_hash(),
            imboy_cache_sync_proto:registry_hash(),
            <<9:128>>,
            false,
            self(),
            S0
        ),
        ?assertNot(imboy_cache_sync_snapshot:round_complete(S1#state.snapshot_round)),
        ?assertEqual(
            false,
            maps:get(
                authority,
                maps:get(Peer, maps:get(peers, S1#state.snapshot_round))
            )
        ),
        ?assertEqual(recovering, S1#state.status),
        receive
            {cache_snapshot_request, _, _} -> error(requested_unready_peer)
        after 0 -> ok
        end,
        {noreply, S1} = imboy_cache_sync:handle_info(
            {cache_snapshot_request, self(), maps:get(round, RoundSt)}, S1
        ),
        receive
            {cache_snapshot_abort, _, _, source_not_ready} -> ok
        after 1000 -> error(missing_source_abort)
        end
    end).

multi_peer_round_waits_for_every_discovered_peer_test() ->
    with_sync_fixture(fun() ->
        A = 'authority_a@synthetic',
        B = 'authority_b@synthetic',
        R0 = imboy_cache_sync_snapshot:expect_peer(snapshot_round(A), B),
        R = maps:get(round, R0),
        {ok, R1} = imboy_cache_source_cut_tests:done(A, R, 4, 0, R0),
        ?assertNot(imboy_cache_sync_snapshot:round_complete(R1)),
        S0 = #state{status = recovering, snapshot_round = R1},
        ?assertEqual(S0, imboy_cache_sync_recover:maybe_complete_round(S0))
    end).

simultaneously_recovering_peers_use_ready_authority_test() ->
    with_sync_fixture(fun() ->
        A = 'ready_authority@synthetic',
        B = 'recovering_b@synthetic',
        C = 'recovering_c@synthetic',
        R0 = imboy_cache_sync_snapshot:expect_peer(
            imboy_cache_sync_snapshot:expect_peer(snapshot_round(A), B), C
        ),
        R = maps:get(round, R0),
        {ok, R1} = imboy_cache_source_cut_tests:done(A, R, 2, 0, R0),
        Empty = {0, crypto:hash(sha256, term_to_binary([]))},
        Report = fun(Node, Boot, State) ->
            imboy_cache_sync_recover:handle_version_report(
                Node,
                0,
                0,
                imboy_cache_sync_proto:config_hash(),
                imboy_cache_sync_proto:registry_hash(),
                Boot,
                false,
                self(),
                {Node, Boot, 0, 0, R, Empty, self()},
                State
            )
        end,
        S0 = #state{status = recovering, snapshot_round = R1},
        S1 = Report(B, <<10:128>>, S0),
        ?assertEqual(recovering, S1#state.status),
        S2 = Report(C, <<11:128>>, S1),
        ?assertEqual(ready, S2#state.status),
        EmptyRound = imboy_cache_sync_snapshot:new_round(
            0, erlang:system_time(millisecond) + 60000
        ),
        ExcludedRound = lists:foldl(
            fun(P, RoundState) ->
                imboy_cache_sync_snapshot:exclude_peer(
                    imboy_cache_sync_snapshot:expect_peer(RoundState, P), P
                )
            end,
            EmptyRound,
            [B, C]
        ),
        ?assertNot(imboy_cache_sync_snapshot:round_complete(ExcludedRound))
    end).

authority_change_aborts_partial_snapshot_and_preserves_versions_test() ->
    lists:foreach(
        fun(Change) ->
            with_sync_fixture(fun() ->
                {ok, Cache} = depcache:start_link(imboy_cache, #{}),
                try
                    snapshot_policy(#{}),
                    meck:expect(depcache, set, 5, fun(_, _, _, _, _) -> ok end),
                    A = 'complete_authority@synthetic',
                    B = 'partial_authority@synthetic',
                    ets:insert(?META_TABLE, {{peer_incarnation, B}, <<11:128>>}),
                    R0 = snapshot_authority(snapshot_round(A), B),
                    R = maps:get(round, R0),
                    {ok, R1} = imboy_cache_source_cut_tests:done(A, R, 1, 0, R0),
                    Entry = {Key, _, _, Rev} = snapshot_entry(partial),
                    {ok, R2} = imboy_cache_sync_snapshot:handle_page(
                        B,
                        R,
                        1,
                        [Entry],
                        imboy_cache_sync_snapshot:page_digest(R, 1, [Entry]),
                        true,
                        R1
                    ),
                    {Ready, Boot} =
                        case Change of
                            not_ready ->
                                {false, <<11:128>>};
                            new_boot ->
                                {true, <<12:128>>};
                            new_boot_already_registered ->
                                ets:insert(?META_TABLE, {{peer_incarnation, B}, <<12:128>>}),
                                {true, <<12:128>>}
                        end,
                    S0 = #state{
                        status = recovering,
                        snapshot_round = R2,
                        recover_deadline_ms = erlang:system_time(millisecond) + 60000
                    },
                    S1 = imboy_cache_sync_recover:handle_version_report(
                        B,
                        1,
                        0,
                        imboy_cache_sync_proto:config_hash(),
                        imboy_cache_sync_proto:registry_hash(),
                        Boot,
                        Ready,
                        self(),
                        S0
                    ),
                    ?assertEqual(recovering, S1#state.status),
                    ?assertNotEqual(R, maps:get(round, S1#state.snapshot_round)),
                    ?assertEqual(Rev, imboy_cache_sync:current_rev(Key)),
                    ?assert(meck:called(depcache, flush, [Key, imboy_cache])),
                    %% 值已清理，下一轮同版本快照仍可回填；不删除版本/墓碑来假恢复。
                    meck:expect(depcache, get, 2, fun(_, _) -> undefined end),
                    R3 = snapshot_round(A),
                    RNew = maps:get(round, R3),
                    {ok, _} = imboy_cache_sync_snapshot:handle_page(
                        A,
                        RNew,
                        1,
                        [Entry],
                        imboy_cache_sync_snapshot:page_digest(RNew, 1, [Entry]),
                        false,
                        R3
                    ),
                    ?assertEqual(2, meck:num_calls(depcache, set, 5))
                after
                    gen_server:stop(Cache)
                end
            end)
        end,
        [not_ready, new_boot, new_boot_already_registered]
    ).

recovery_abort_rearms_timer_and_deadline_enters_degraded_test() ->
    with_sync_fixture(fun() ->
        S0 = #state{
            status = recovering,
            snapshot_round = snapshot_round('timer_peer@synthetic'),
            recover_deadline_ms = erlang:system_time(millisecond) + 60000
        },
        S1 = imboy_cache_sync_recover:abort_round(test_abort, S0),
        ?assert(is_reference(S1#state.recovery_timer)),
        ?assertEqual(
            {noreply, S1},
            imboy_cache_sync:handle_info(
                {timeout, make_ref(), cache_recovery}, S1
            )
        ),
        Past = erlang:system_time(millisecond) - 1,
        R1 = (S1#state.snapshot_round)#{deadline_ms := Past},
        S2 = S1#state{recover_deadline_ms = Past, snapshot_round = R1},
        {noreply, S3} = imboy_cache_sync_recover:handle_timeout(S2),
        ?assertEqual(degraded, S3#state.status),
        ?assertEqual(undefined, S3#state.recovery_timer),
        ?assertEqual(false, erlang:read_timer(S1#state.recovery_timer))
    end).

partial_snapshot_timeout_cannot_expose_value_after_empty_recovery_test() ->
    with_sync_fixture(fun() ->
        snapshot_policy(#{}),
        Values = ets:new(snapshot_values, [public, set]),
        try
            mock_snapshot_values(Values),
            Peer = 'timeout_source@synthetic',
            R0 = snapshot_round(Peer),
            R = maps:get(round, R0),
            Entry = {Key, _, _, _} = snapshot_entry(incomplete),
            {ok, R1} = imboy_cache_sync_snapshot:handle_page(
                Peer,
                R,
                1,
                [Entry],
                imboy_cache_sync_snapshot:page_digest(R, 1, [Entry]),
                true,
                R0
            ),
            S0 = #state{
                status = recovering,
                snapshot_round = R1,
                recover_deadline_ms = erlang:system_time(millisecond) + 60000
            },
            {noreply, S1} = imboy_cache_sync_recover:handle_timeout(S0),
            ?assertEqual(1, maps:get(rounds_used, S1#state.snapshot_round)),
            ?assertEqual([], ets:lookup(Values, Key)),
            R2 = snapshot_authority(S1#state.snapshot_round, Peer),
            {ok, R3} = imboy_cache_source_cut_tests:done(Peer, maps:get(round, R2), 1, 0, R2),
            S2 = imboy_cache_sync_recover:maybe_complete_round(S1#state{snapshot_round = R3}),
            ?assertEqual(ready, S2#state.status),
            ?assertEqual(undefined, imboy_cache:get(Key))
        after
            ets:delete(Values)
        end
    end).

mock_snapshot_values(Values) ->
    meck:expect(depcache, set, 5, fun(K, V, _, _, _) ->
        ets:insert(Values, {K, V}),
        ok
    end),
    meck:expect(depcache, get, 2, fun(K, _) ->
        case ets:lookup(Values, K) of
            [{_, V}] -> {ok, V};
            [] -> undefined
        end
    end),
    meck:expect(depcache, flush, 2, fun(K, _) ->
        ets:delete(Values, K),
        ok
    end).

isolated_configured_cluster_never_becomes_single_node_ready_test() ->
    with_sync_fixture(fun() ->
        application:set_env(imboy, cluster_nodes, ['another_node@synthetic']),
        meck:expect(syn, members, 2, fun(_, _) -> [] end),
        S0 = #state{
            status = discovering,
            retries = 3,
            recover_deadline_ms = erlang:system_time(millisecond) + 60000
        },
        {noreply, S1} = imboy_cache_sync_recover:discover(S0),
        ?assertEqual(recovering, S1#state.status),
        ?assertNot(imboy_cache_sync_snapshot:round_complete(S1#state.snapshot_round))
    end).

snapshot_entry(Name) ->
    {{snapshot_test, Name}, <<"value">>, erlang:system_time(millisecond) + 60000, {1, node()}}.

snapshot_policy(Limits) ->
    application:set_env(imboy, cache_registry_overrides, #{snapshot_test => {replicate, false}}),
    application:set_env(imboy, cache_replication, Limits#{whitelist => [snapshot_test]}).

snapshot_round(Peer) ->
    snapshot_authority(
        imboy_cache_sync_snapshot:new_round(
            0, erlang:system_time(millisecond) + 60000
        ),
        Peer
    ).
snapshot_authority(State, Peer) ->
    requested_peer(
        imboy_cache_sync_snapshot:expect_peer(State, Peer), Peer
    ).

requested_peer(Round, Peer) ->
    Authorized = imboy_cache_sync_snapshot:authorize_peer(Round, Peer),
    imboy_cache_sync_snapshot:mark_requested(Authorized, Peer).
