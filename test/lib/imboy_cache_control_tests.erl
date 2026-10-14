%%% 控制帧结构验证：直接同步回调，无应用/数据库/缓存进程。
-module(imboy_cache_control_tests).
-include_lib("eunit/include/eunit.hrl").
-include("imboy_cache_sync_state.hrl").

malformed_version_reports_leave_state_and_metadata_unchanged_test() ->
    Report = {cache_version_report, node(), 1, 0, <<0:128>>, <<0:128>>, <<1:128>>, true, self()},
    ?assert(imboy_cache_sync_proto:valid_version_report(Report)),
    Bad = [
        setelement(Pos, Report, Value)
     || {Pos, Value} <- [
            {2, 42},
            {2, 'different_source@synthetic'},
            {3, <<"counter">>},
            {3, -1},
            {4, non_integer},
            {4, -1},
            {5, <<0>>},
            {6, <<0:256>>},
            {7, <<0>>},
            {8, ready},
            {9, not_a_pid}
        ]
    ],
    ets:new(?META_TABLE, [named_table, public, set]),
    try
        ets:insert(?META_TABLE, {sentinel, preserved}),
        State = #state{boot_id = <<2:128>>, status = recovering},
        lists:foreach(
            fun(Frame) ->
                ?assertNot(imboy_cache_sync_proto:valid_version_report(Frame)),
                ?assertEqual({noreply, State}, imboy_cache_sync:handle_info(Frame, State)),
                ?assertEqual([{sentinel, preserved}], ets:tab2list(?META_TABLE))
            end,
            Bad
        ),
        %% 结构合法但不在当前发现回合的应答也不得改版本/化身。
        ?assertEqual({noreply, State}, imboy_cache_sync:handle_info(Report, State)),
        ?assertEqual([{sentinel, preserved}], ets:tab2list(?META_TABLE))
    after
        ets:delete(?META_TABLE)
    end.

invalid_envelopes_do_not_allocate_or_mutate_test() ->
    ets:new(?META_TABLE, [named_table, public, set]),
    try
        ets:insert(?META_TABLE, [{sentinel, preserved}, {status, ready}]),
        State = #state{boot_id = <<2:128>>, status = ready},
        Event = imboy_cache_sync_proto:event(
            invalidate,
            {test_ns, key},
            undefined,
            0,
            {1, node()},
            1,
            <<2:128>>
        ),
        ?assertEqual(ok, imboy_cache_sync_proto:validate(Event)),
        Huge = 1 bsl (8 * (2 * imboy_cache_policy:max_value_bytes() + 4096)),
        Bad = [
            {Event#{protocol => 3}, invalid_protocol},
            {Event#{extension => <<0:800000>>}, unknown_protocol_field},
            {Event#{revision => {Huge, node()}}, event_too_large}
        ],
        lists:foreach(
            fun({E, Reason}) ->
                ?assertEqual({error, Reason}, imboy_cache_sync_proto:validate(E)),
                ?assertEqual({reject, Reason}, imboy_cache_sync_apply:apply_v2(E, State))
            end,
            Bad
        ),
        %% 直接调用同步入口也不能绕过 Key 授权或 TTL 类型检查。
        ?assertMatch(
            {reply, {error, _}, State},
            imboy_cache_sync:handle_call(
                {publish_set, {fcm, secret}, value, 30},
                {self(), ref},
                State
            )
        ),
        ?assertMatch(
            {reply, {error, _}, State},
            imboy_cache_sync:handle_call(
                {publish_delete, {fcm, secret}},
                {self(), ref},
                State
            )
        ),
        ?assertEqual(
            lists:sort([{sentinel, preserved}, {status, ready}]),
            lists:sort(ets:tab2list(?META_TABLE))
        )
    after
        ets:delete(?META_TABLE)
    end.

near_limit_envelope_and_oversize_sender_test() ->
    OldRep = application:get_env(imboy, cache_replication),
    OldRegistry = application:get_env(imboy, cache_registry_overrides),
    application:set_env(imboy, cache_replication, #{whitelist => [cache_v2_test_rep]}),
    application:set_env(
        imboy,
        cache_registry_overrides,
        #{cache_v2_test_rep => {replicate, false}}
    ),
    ets:new(?META_TABLE, [named_table, public, set]),
    try
        ets:insert(?META_TABLE, [{status, ready}, {sentinel, preserved}]),
        Bytes = imboy_cache_policy:max_value_bytes() - 128,
        Payload = binary:copy(<<0>>, Bytes),
        Key = {cache_v2_test_rep, Payload},
        Event = imboy_cache_sync_proto:event(
            set,
            Key,
            Payload,
            erlang:system_time(millisecond) + 60000,
            {1, node()},
            1,
            <<2:128>>
        ),
        ?assertEqual(ok, imboy_cache_sync_proto:validate(Event)),
        Huge = 1 bsl (8 * (2 * imboy_cache_policy:max_value_bytes() + 4096)),
        State = #state{boot_id = <<2:128>>, status = ready, counter = Huge},
        lists:foreach(
            fun(Request) ->
                ?assertEqual(
                    {reply, {error, event_too_large}, State},
                    imboy_cache_sync:handle_call(Request, {self(), ref}, State)
                )
            end,
            [{publish_set, Key, Payload, 60}, {publish_delete, Key}]
        ),
        ?assertEqual(
            lists:sort([{status, ready}, {sentinel, preserved}]),
            lists:sort(ets:tab2list(?META_TABLE))
        )
    after
        ets:delete(?META_TABLE),
        restore_env(cache_replication, OldRep),
        restore_env(cache_registry_overrides, OldRegistry)
    end.

restore_env(Key, undefined) -> application:unset_env(imboy, Key);
restore_env(Key, {ok, Value}) -> application:set_env(imboy, Key, Value).

metadata_cannot_borrow_payload_budget_test() ->
    with_control_metadata(fun() ->
        Huge = 1 bsl (8 * imboy_cache_policy:protocol_meta_max_bytes()),
        Event = imboy_cache_sync_proto:event(
            invalidate, {test_ns, key}, undefined, 0, {1, node()}, 1, <<2:128>>
        ),
        Before = ets:tab2list(?META_TABLE),
        State = #state{boot_id = <<2:128>>, status = ready},
        lists:foreach(
            fun(E) ->
                ?assertEqual({error, metadata_too_large}, imboy_cache_sync_proto:validate(E)),
                ?assertEqual(
                    {reject, metadata_too_large}, imboy_cache_sync_apply:apply_v2(E, State)
                )
            end,
            [Event#{revision => {Huge, node()}}, Event#{epoch => Huge}, Event#{origin_seq => Huge}]
        ),
        ?assertEqual(Before, ets:tab2list(?META_TABLE))
    end).

oversized_report_and_watermark_leave_clock_unchanged_test() ->
    with_control_metadata(fun() ->
        Huge = 1 bsl (8 * imboy_cache_policy:protocol_meta_max_bytes()),
        Hash = imboy_cache_sync_proto:config_hash(),
        Registry = imboy_cache_sync_proto:registry_hash(),
        Report = {cache_version_report, node(), 1, 0, Hash, Registry, <<3:128>>, true, self()},
        Round = imboy_cache_sync_snapshot:expect_peer(
            imboy_cache_sync_snapshot:new_round(0, erlang:system_time(millisecond) + 60000), node()
        ),
        State = #state{boot_id = <<2:128>>, status = recovering, snapshot_round = Round},
        ets:insert(?META_TABLE, [
            {{peer_incarnation, node()}, <<3:128>>}, {{seq, {node(), <<3:128>>}}, 1}
        ]),
        Before = lists:sort(ets:tab2list(?META_TABLE)),
        Watermark = {cache_invalidation_watermark, node(), <<3:128>>, Huge, Hash, Registry},
        ?assertNot(imboy_cache_sync_proto:valid_watermark(Watermark)),
        lists:foreach(
            fun(Frame) ->
                ?assertEqual({noreply, State}, imboy_cache_sync:handle_info(Frame, State)),
                ?assertEqual(Before, lists:sort(ets:tab2list(?META_TABLE)))
            end,
            [setelement(3, Report, Huge), setelement(4, Report, Huge), Watermark]
        )
    end).

malformed_snapshot_requests_do_not_serve_data_test() ->
    with_control_metadata(fun() ->
        State = #state{boot_id = <<2:128>>, status = ready},
        Before = ets:tab2list(?META_TABLE),
        lists:foreach(
            fun(Frame) ->
                ?assertEqual({noreply, State}, imboy_cache_sync:handle_info(Frame, State))
            end,
            [
                {cache_snapshot_request, not_pid, <<0:96>>},
                {cache_snapshot_request, self(), wrong_round},
                {cache_snapshot_request, self(), <<0:128>>}
            ]
        ),
        ?assertEqual(Before, ets:tab2list(?META_TABLE)),
        receive
            {cache_snapshot_abort, _, _, _} -> error(unexpected_response)
        after 0 -> ok
        end
    end).

snapshot_metadata_overflow_aborts_before_application_test() ->
    with_control_metadata(fun() ->
        Previous = application:get_env(imboy, cache_replication),
        OldRegistry = application:get_env(imboy, cache_registry_overrides),
        application:set_env(imboy, cache_replication, #{whitelist => [cache_v2_test_rep]}),
        application:set_env(imboy, cache_registry_overrides, #{
            cache_v2_test_rep => {replicate, false}
        }),
        try
            Huge = 1 bsl (8 * imboy_cache_policy:protocol_meta_max_bytes()),
            Peer = 'metadata_peer@synthetic',
            R0 = imboy_cache_sync_snapshot:new_round(0, erlang:system_time(millisecond) + 60000),
            R1 = requested_peer(
                imboy_cache_sync_snapshot:expect_peer(R0, Peer), Peer
            ),
            Round = maps:get(round, R1),
            Exp = erlang:system_time(millisecond) + 60000,
            Entry = {{cache_v2_test_rep, key}, value, Exp, {1, Peer}},
            Digest = imboy_cache_sync_snapshot:page_digest(Round, 1, [Entry]),
            ?assertEqual(
                {abort, metadata_too_large, R1},
                imboy_cache_sync_snapshot:handle_page(Peer, Round, Huge, [Entry], Digest, false, R1)
            ),
            ?assertEqual(
                {abort, metadata_too_large, R1},
                imboy_cache_sync_snapshot:handle_done(Peer, Round, Huge, 0, R1)
            ),
            BadEntry = setelement(4, Entry, {Huge, Peer}),
            BadDigest = imboy_cache_sync_snapshot:page_digest(Round, 1, [BadEntry]),
            ?assertEqual(
                {abort, invalid_snapshot_entry, R1},
                imboy_cache_sync_snapshot:handle_page(
                    Peer, Round, 1, [BadEntry], BadDigest, false, R1
                )
            ),
            ?assertEqual([], ets:match(?META_TABLE, {{rev, '_'}, '_'}))
        after
            restore_env(cache_replication, Previous),
            restore_env(cache_registry_overrides, OldRegistry)
        end
    end).

incompatible_reports_do_not_change_peer_identity_or_clock_test() ->
    with_control_metadata(fun() ->
        Peer = 'incompatible_report@synthetic',
        Boot = <<7:128>>,
        ets:insert(?META_TABLE, {{peer_incarnation, Peer}, Boot}),
        Before = lists:sort(ets:tab2list(?META_TABLE)),
        Hash = imboy_cache_sync_proto:config_hash(),
        Registry = imboy_cache_sync_proto:registry_hash(),
        Bad = [{<<0:128>>, Registry, 0}, {Hash, <<0:128>>, 0}, {Hash, Registry, 1}],
        R0 = imboy_cache_sync_snapshot:new_round(0, erlang:system_time(millisecond) + 60000),
        Round = imboy_cache_sync_snapshot:expect_peer(R0, Peer),
        States = [
            #state{boot_id = <<2:128>>, counter = 3, status = ready},
            #state{
                boot_id = <<2:128>>,
                counter = 3,
                status = recovering,
                requires_authority = true,
                snapshot_round = Round
            }
        ],
        lists:foreach(
            fun(State) ->
                lists:foreach(
                    fun(BadReport) ->
                        assert_incompatible_report(Peer, State, BadReport, Before)
                    end,
                    Bad
                )
            end,
            States
        )
    end).

assert_incompatible_report(Peer, State, {Config, Reg, Epoch}, Before) ->
    S1 = imboy_cache_sync_recover:handle_version_report(
        Peer, 999, Epoch, Config, Reg, <<8:128>>, true, self(), State
    ),
    ?assertEqual(3, S1#state.counter),
    ?assertEqual(Before, lists:sort(ets:tab2list(?META_TABLE))),
    case S1#state.status of
        ready ->
            ?assertEqual(State, S1);
        recovering ->
            PeerState = maps:get(Peer, maps:get(peers, S1#state.snapshot_round)),
            ?assertNot(maps:get(authority, PeerState)),
            ?assertNot(imboy_cache_sync_snapshot:round_complete(S1#state.snapshot_round))
    end.

authorized_source_hash_change_still_aborts_round_test() ->
    with_control_metadata(fun() ->
        Peer = 'changed_hash_authority@synthetic',
        ets:insert(?META_TABLE, {{peer_incarnation, Peer}, <<8:128>>}),
        R0 = imboy_cache_sync_snapshot:new_round(0, erlang:system_time(millisecond) + 60000),
        Round = requested_peer(
            imboy_cache_sync_snapshot:expect_peer(R0, Peer), Peer
        ),
        State = #state{
            status = recovering,
            counter = 3,
            snapshot_round = Round,
            recover_deadline_ms = erlang:system_time(millisecond) + 60000
        },
        Before = lists:sort(ets:tab2list(?META_TABLE)),
        S1 = imboy_cache_sync_recover:handle_version_report(
            Peer,
            999,
            0,
            <<0:128>>,
            imboy_cache_sync_proto:registry_hash(),
            <<8:128>>,
            true,
            self(),
            State
        ),
        try
            ?assertEqual(recovering, S1#state.status),
            ?assertNotEqual(maps:get(round, Round), maps:get(round, S1#state.snapshot_round)),
            ?assertEqual(1, maps:get(rounds_used, S1#state.snapshot_round)),
            ?assertEqual(3, S1#state.counter),
            ?assertEqual(Before, lists:sort(ets:tab2list(?META_TABLE)))
        after
            erlang:cancel_timer(S1#state.recovery_timer)
        end
    end).

legacy_receipts_keep_contiguous_healthy_watermarks_ready_test() ->
    with_legacy_control_metadata(fun() ->
        State = #state{boot_id = <<2:128>>, status = ready},
        lists:foreach(
            fun(Peer) ->
                registered = imboy_cache_sync_proto:register_incarnation(Peer, <<3:128>>),
                Frame =
                    {legacy_invalidation_seq, Peer, <<3:128>>, 1,
                        imboy_cache_sync_proto:config_hash(),
                        imboy_cache_sync_proto:registry_hash(), {legacy_test, key}},
                ?assertEqual(State, imboy_cache_sync_apply:apply_legacy_sequence(Frame, State)),
                ?assertEqual(
                    State,
                    imboy_cache_sync_apply:apply_legacy_sequence(setelement(4, Frame, 2), State)
                ),
                Watermark = erlang:delete_element(
                    7,
                    setelement(1, setelement(4, Frame, 2), cache_invalidation_watermark)
                ),
                ?assertEqual({noreply, State}, imboy_cache_sync:handle_info(Watermark, State))
            end,
            ['healthy_a@synthetic', 'healthy_b@synthetic']
        ),
        ?assertEqual([{status, ready}], ets:lookup(?META_TABLE, status))
    end).

legacy_grace_receipt_does_not_replace_current_identity_test() ->
    with_legacy_control_metadata(fun() ->
        Peer = 'legacy_grace_peer@synthetic',
        Current = <<3:128>>,
        Old = <<4:128>>,
        ets:insert(?META_TABLE, [
            {{peer_incarnation, Peer}, Current},
            {{seq, {Peer, Current}}, 2},
            {{grace_incarnation, {Peer, Old}}, erlang:system_time(millisecond)}
        ]),
        Before = lists:sort(ets:tab2list(?META_TABLE)),
        State = #state{boot_id = <<2:128>>, status = ready},
        Frame =
            {legacy_invalidation_seq, Peer, Old, 99, imboy_cache_sync_proto:config_hash(),
                imboy_cache_sync_proto:registry_hash(), {legacy_test, key}},
        ?assertEqual(State, imboy_cache_sync_apply:apply_legacy_sequence(Frame, State)),
        ?assertEqual(Before, lists:sort(ets:tab2list(?META_TABLE)))
    end).

rejected_event_cannot_replace_incarnation_or_erase_sequence_test() ->
    with_control_metadata(fun() ->
        Peer = 'rejected_identity@synthetic',
        Boot = <<7:128>>,
        ets:insert(?META_TABLE, [
            {{peer_incarnation, Peer}, Boot},
            {{seq, {Peer, Boot}}, 10},
            {sync_epoch, 2}
        ]),
        State = #state{boot_id = <<2:128>>, status = ready},
        Base = (imboy_cache_sync_proto:event(
            set,
            {test_ns, key},
            value,
            erlang:system_time(millisecond) + 60000,
            {1, Peer},
            1,
            <<8:128>>
        ))#{
            origin_node => Peer
        },
        %% 直接检查已通过结构验证后的双端一致性层。
        Cases = [
            {Base#{epoch => 1}, stale_epoch},
            {Base#{config_hash => <<0:128>>}, hash_mismatch},
            {Base#{epoch => 1, operation => invalidate, config_hash => <<0:128>>}, hash_mismatch}
        ],
        lists:foreach(
            fun({Event, Reason}) ->
                ?assertEqual(
                    {error, Reason},
                    imboy_cache_sync_proto:check_consistency(Event, State#state.boot_id)
                ),
                ?assertEqual(
                    [{{peer_incarnation, Peer}, Boot}],
                    ets:lookup(?META_TABLE, {peer_incarnation, Peer})
                ),
                ?assertEqual(
                    [{{seq, {Peer, Boot}}, 10}], ets:lookup(?META_TABLE, {seq, {Peer, Boot}})
                ),
                ?assertEqual([], ets:match_object(?META_TABLE, {{grace_incarnation, '_'}, '_'}))
            end,
            Cases
        )
    end).

stale_epoch_invalidation_with_bad_hash_is_rejected_by_sync_test() ->
    with_control_metadata(fun() ->
        Peer = 'rejected_invalidation@synthetic',
        Boot = <<7:128>>,
        ets:insert(?META_TABLE, [
            {{peer_incarnation, Peer}, Boot},
            {{seq, {Peer, Boot}}, 10},
            {sync_epoch, 2},
            {sync_counter, 17}
        ]),
        State = #state{boot_id = <<2:128>>, status = ready, counter = 17},
        Event = (imboy_cache_sync_proto:event(
            invalidate,
            {test_ns, key},
            undefined,
            0,
            {1, Peer},
            1,
            <<8:128>>
        ))#{
            origin_node => Peer,
            epoch => 1,
            config_hash => <<0:128>>
        },
        ?assertEqual(ok, imboy_cache_sync_proto:validate(Event)),
        ?assertEqual({noreply, State}, imboy_cache_sync:handle_info({cache_sync, Event}, State)),
        ?assertEqual(
            [{{peer_incarnation, Peer}, Boot}], ets:lookup(?META_TABLE, {peer_incarnation, Peer})
        ),
        ?assertEqual([{{seq, {Peer, Boot}}, 10}], ets:lookup(?META_TABLE, {seq, {Peer, Boot}})),
        ?assertEqual([{sync_counter, 17}], ets:lookup(?META_TABLE, sync_counter))
    end).

new_event_incarnation_cancels_old_snapshot_authority_test() ->
    restarted_authority_case(v2).

new_legacy_incarnation_cancels_old_snapshot_authority_test() ->
    restarted_authority_case(legacy).

restarted_authority_case(Protocol) ->
    with_legacy_control_metadata(fun() ->
        Peer = 'restarted_source@synthetic',
        OldBoot = <<7:128>>,
        Deadline = erlang:system_time(millisecond) + 60000,
        ets:insert(?META_TABLE, {{peer_incarnation, Peer}, OldBoot}),
        R0 = imboy_cache_sync_snapshot:new_round(0, Deadline),
        R1 = requested_peer(
            imboy_cache_sync_snapshot:expect_peer(R0, Peer), Peer
        ),
        State = #state{
            boot_id = <<2:128>>,
            status = recovering,
            recover_deadline_ms = Deadline,
            snapshot_round = R1
        },
        Event = (imboy_cache_sync_proto:event(
            invalidate,
            {test_ns, key},
            undefined,
            0,
            {1, Peer},
            3,
            <<8:128>>
        ))#{
            origin_node => Peer
        },
        Next =
            case Protocol of
                v2 ->
                    {reject, unknown_incarnation, Applied} = imboy_cache_sync_apply:apply_v2(
                        Event, State
                    ),
                    Applied;
                legacy ->
                    imboy_cache_sync_apply:apply_legacy_sequence(
                        {legacy_invalidation_seq, Peer, <<8:128>>, 3,
                            imboy_cache_sync_proto:config_hash(),
                            imboy_cache_sync_proto:registry_hash(), {test_ns, key}},
                        State
                    )
            end,
        try
            ?assertNotEqual(maps:get(round, R1), maps:get(round, Next#state.snapshot_round)),
            ?assertEqual(Deadline, Next#state.recover_deadline_ms),
            ?assertEqual(recovering, Next#state.status),
            ?assertEqual(1, maps:get(rounds_used, Next#state.snapshot_round)),
            ?assertEqual(
                {noreply, Next},
                imboy_cache_sync:handle_info(
                    {cache_snapshot_done, Peer, maps:get(round, R1), 1, 0}, Next
                )
            )
        after
            case Next#state.recovery_timer of
                undefined ->
                    ok;
                Timer ->
                    erlang:cancel_timer(Timer),
                    receive
                        {timeout, Timer, cache_recovery} -> ok
                    after 0 -> ok
                    end
            end
        end
    end).

unchanged_authority_and_other_peer_do_not_cancel_round_test() ->
    with_control_metadata(fun() ->
        Peer = 'stable_source@synthetic',
        Boot = <<7:128>>,
        ets:insert(?META_TABLE, {{peer_incarnation, Peer}, Boot}),
        R0 = imboy_cache_sync_snapshot:new_round(0, erlang:system_time(millisecond) + 60000),
        R1 = requested_peer(
            imboy_cache_sync_snapshot:expect_peer(R0, Peer), Peer
        ),
        State = #state{status = recovering, snapshot_round = R1},
        ?assertEqual(State, imboy_cache_sync_recover:check_authority_incarnation(Peer, State)),
        ?assertEqual(
            State,
            imboy_cache_sync_recover:check_authority_incarnation(
                'other_peer@synthetic', State
            )
        )
    end).

unrequested_authority_cannot_send_page_or_empty_done_test() ->
    with_control_metadata(fun() ->
        Peer = 'waiting_source@synthetic',
        R0 = imboy_cache_sync_snapshot:new_round(0, erlang:system_time(millisecond) + 60000),
        R1 = imboy_cache_sync_snapshot:authorize_peer(
            imboy_cache_sync_snapshot:expect_peer(R0, Peer), Peer
        ),
        Round = maps:get(round, R1),
        ?assertEqual(
            {ok, R1},
            imboy_cache_sync_snapshot:handle_page(
                Peer, Round, 1, [invalid], 0, false, R1
            )
        ),
        ?assertEqual({ok, R1}, imboy_cache_sync_snapshot:handle_done(Peer, Round, 0, 0, R1)),
        ?assertNot(imboy_cache_sync_snapshot:round_complete(R1))
    end).

delete_event_preserves_absolute_expiry_and_rejects_invalid_expiry_test() ->
    with_legacy_control_metadata(fun() ->
        Peer = 'delete_expiry@synthetic',
        Boot = <<9:128>>,
        Key = {test_ns, delete_expiry},
        Expiry = erlang:system_time(millisecond) + 120000,
        ets:insert(?META_TABLE, {{peer_incarnation, Peer}, Boot}),
        %% 独立授权测试 namespace，按原设置恢复。
        OldConfig = application:get_env(imboy, cache_replication),
        OldRegistry = application:get_env(imboy, cache_registry_overrides),
        application:set_env(imboy, cache_replication, #{whitelist => [test_ns]}),
        application:set_env(imboy, cache_registry_overrides, #{test_ns => {replicate, false}}),
        try
            E = (imboy_cache_sync_proto:event(
                delete,
                Key,
                undefined,
                Expiry,
                {1, Peer},
                1,
                Boot
            ))#{
                origin_node => Peer
            },
            State = #state{boot_id = <<2:128>>, status = ready},
            ?assertEqual(ok, imboy_cache_sync_proto:validate(E)),
            ?assertEqual(
                ok,
                imboy_cache_sync_proto:validate_publish(
                    delete, Key, undefined, 0, {1, node()}, 1, Boot
                )
            ),
            {ok, S1} = imboy_cache_sync_apply:apply_v2(E, State),
            ?assertMatch(
                [{_, {{1, Peer}, {Boot, 1}, Expiry, deleted, _}}],
                ets:lookup(?META_TABLE, {rev, Key})
            ),
            %% 冻结合同：相同版本内容核对、同版本不续期。同 revision 携带不同
            %% 绝对期限属于内容冲突，拒绝且首次绝对期限不被覆盖。
            ?assertMatch(
                {reject, revision_content_conflict, _},
                imboy_cache_sync_apply:apply_v2(E#{expires_at_ms := Expiry + 1000}, S1)
            ),
            ?assertMatch(
                [{_, {{1, Peer}, {Boot, 1}, Expiry, deleted, _}}],
                ets:lookup(?META_TABLE, {rev, Key})
            ),
            %% 字节一致的重放保持幂等。
            ?assertEqual(
                {ok, S1},
                imboy_cache_sync_apply:apply_v2(E, S1)
            ),
            [
                ?assertEqual(
                    {error, invalid_expiry},
                    imboy_cache_sync_proto:validate(
                        E#{expires_at_ms := Invalid}
                    )
                )
             || Invalid <- [0, -1, undefined]
            ],
            ?assertEqual(
                {error, ttl_too_large},
                imboy_cache_sync_proto:validate(E#{
                    expires_at_ms := erlang:system_time(millisecond) +
                        (imboy_cache_policy:tombstone_ttl_sec() + 10) * 1000
                })
            )
        after
            restore_delete_test_env(cache_replication, OldConfig),
            restore_delete_test_env(cache_registry_overrides, OldRegistry)
        end
    end).

restore_delete_test_env(Key, undefined) -> application:unset_env(imboy, Key);
restore_delete_test_env(Key, {ok, Value}) -> application:set_env(imboy, Key, Value).

snapshot_tail_sequence_gap_prevents_old_done_from_closing_test() ->
    with_legacy_control_metadata(fun() ->
        Peer = 'tail_source@synthetic',
        Boot = <<9:128>>,
        Deadline = erlang:system_time(millisecond) + 60000,
        ets:insert(?META_TABLE, [{{peer_incarnation, Peer}, Boot}, {{seq, {Peer, Boot}}, 10}]),
        R0 = imboy_cache_sync_snapshot:new_round(0, Deadline),
        R1 = requested_peer(imboy_cache_sync_snapshot:expect_peer(R0, Peer), Peer),
        State = #state{status = recovering, snapshot_round = R1, recover_deadline_ms = Deadline},
        Tail =
            {cache_invalidation_watermark, Peer, Boot, 11, imboy_cache_policy:config_hash(),
                imboy_cache_sync_proto:registry_hash()},
        {noreply, Next} = imboy_cache_sync:handle_info(Tail, State),
        try
            ?assertNotEqual(maps:get(round, R1), maps:get(round, Next#state.snapshot_round)),
            ?assertEqual(Deadline, Next#state.recover_deadline_ms),
            ?assertEqual(recovering, Next#state.status),
            ?assertEqual(
                {noreply, Next},
                imboy_cache_sync:handle_info(
                    {cache_snapshot_done, Peer, maps:get(round, R1), 20, 0}, Next
                )
            ),
            ?assertEqual([{{seq, {Peer, Boot}}, 11}], ets:lookup(?META_TABLE, {seq, {Peer, Boot}}))
        after
            Timer = Next#state.recovery_timer,
            erlang:cancel_timer(Timer),
            receive
                {timeout, Timer, cache_recovery} -> ok
            after 0 -> ok
            end
        end
    end).

completed_authority_epoch_change_aborts_round_test() ->
    with_legacy_control_metadata(fun() ->
        A = 'completed_epoch_source@synthetic',
        B = 'waiting_epoch_source@synthetic',
        Boot = <<8:128>>,
        Deadline = erlang:system_time(millisecond) + 60000,
        ets:insert(?META_TABLE, [{{peer_incarnation, A}, Boot}, {{peer_epoch, A}, 0}]),
        R0 = imboy_cache_sync_snapshot:new_round(0, Deadline),
        R1 = requested_peer(imboy_cache_sync_snapshot:expect_peer(R0, A), A),
        R2 = imboy_cache_sync_snapshot:expect_peer(R1, B),
        {ok, R3} = imboy_cache_source_cut_tests:done(A, maps:get(round, R2), 0, 0, R2),
        State = #state{status = recovering, snapshot_round = R3, recover_deadline_ms = Deadline},
        Next = imboy_cache_sync_recover:handle_version_report(
            A,
            1,
            1,
            imboy_cache_sync_proto:config_hash(),
            imboy_cache_sync_proto:registry_hash(),
            Boot,
            true,
            self(),
            State
        ),
        try
            ?assertNotEqual(maps:get(round, R3), maps:get(round, Next#state.snapshot_round)),
            ?assertEqual(recovering, Next#state.status),
            ?assertEqual(Deadline, Next#state.recover_deadline_ms)
        after
            case Next#state.recovery_timer of
                undefined ->
                    ok;
                Timer ->
                    erlang:cancel_timer(Timer),
                    receive
                        {timeout, Timer, cache_recovery} -> ok
                    after 0 -> ok
                    end
            end
        end
    end).

new_epoch_event_cancels_selected_authority_before_report_test() ->
    with_legacy_control_metadata(fun() ->
        Peer = 'event_epoch_source@synthetic',
        Boot = <<8:128>>,
        Deadline = erlang:system_time(millisecond) + 60000,
        ets:insert(?META_TABLE, [{{peer_incarnation, Peer}, Boot}, {{peer_epoch, Peer}, 0}]),
        R0 = imboy_cache_sync_snapshot:new_round(0, Deadline),
        R1 = requested_peer(imboy_cache_sync_snapshot:expect_peer(R0, Peer), Peer),
        State = #state{
            boot_id = <<2:128>>,
            status = recovering,
            snapshot_round = R1,
            recover_deadline_ms = Deadline
        },
        Event = (imboy_cache_sync_proto:event(
            invalidate,
            {test_ns, event_epoch},
            undefined,
            0,
            {1, Peer},
            1,
            Boot
        ))#{
            origin_node => Peer, epoch => 1
        },
        ?assertEqual(ok, imboy_cache_sync_proto:validate(Event)),
        {ok, Next} = imboy_cache_sync_apply:apply_v2(Event, State),
        try
            ?assertNotEqual(maps:get(round, R1), maps:get(round, Next#state.snapshot_round)),
            ?assertEqual(1, maps:get(rounds_used, Next#state.snapshot_round)),
            ?assertEqual(Deadline, Next#state.recover_deadline_ms)
        after
            case Next#state.recovery_timer of
                undefined ->
                    ok;
                Timer ->
                    erlang:cancel_timer(Timer),
                    receive
                        {timeout, Timer, cache_recovery} -> ok
                    after 0 -> ok
                    end
            end
        end
    end).

with_legacy_control_metadata(Fun) ->
    meck:new(depcache, [passthrough, no_link]),
    meck:expect(depcache, flush, 2, fun(_, _) -> ok end),
    try
        with_control_metadata(Fun)
    after
        meck:unload(depcache)
    end.

with_control_metadata(Fun) ->
    %% 真实来源身份：source_identity_current 要求来源 pid 在 syn 成员视图内。
    {ok, _} = application:ensure_all_started(syn),
    ok = syn:add_node_to_scopes([imboy_cache]),
    ok = syn:join(imboy_cache, dsync_handler, self(), #{}),
    ets:new(?META_TABLE, [named_table, public, set]),
    ets:insert(?META_TABLE, [{status, ready}, {sync_epoch, 0}, {sentinel, preserved}]),
    try
        Fun()
    after
        _ = syn:leave(imboy_cache, dsync_handler, self()),
        ets:delete(?META_TABLE)
    end.

requested_peer(Round, Peer) ->
    Authorized = imboy_cache_sync_snapshot:authorize_peer(Round, Peer),
    imboy_cache_sync_snapshot:mark_requested(Authorized, Peer).
