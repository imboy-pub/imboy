%%% 元数据字节预算与计数转换；无应用/数据库进程。
-module(imboy_cache_metadata_tests).
-include_lib("eunit/include/eunit.hrl").
-include("imboy_cache_sync_state.hrl").

snapshot_origin_is_bounded_and_bound_to_revision_node_test() ->
    with_metadata(fun() ->
        Peer = 'origin_validator@synthetic',
        Key = {origin_validator, key},
        application:set_env(
            imboy,
            cache_registry_overrides,
            #{origin_validator => {replicate, false}}
        ),
        set_budget(#{whitelist => [origin_validator]}),
        Entry = {deleted, Key, undefined, erlang:system_time(millisecond) + 60000, {1000, Peer}},
        Origin = {Peer, <<1:128>>, 7},
        ?assert(imboy_cache_sync_proto:valid_snapshot_origin(Origin, Entry)),
        [
            ?assertNot(imboy_cache_sync_proto:valid_snapshot_origin(Bad, Entry))
         || Bad <- [
                unknown,
                {node(), <<1:128>>, 7},
                {Peer, <<1>>, 7},
                {Peer, <<1:128>>, 0},
                {Peer, <<1:128>>, -1},
                {Peer, <<1:128>>, 1 bsl 40000}
            ]
        ],
        ?assertNot(imboy_cache_sync_proto:valid_snapshot_origin(Origin, malformed)),
        ?assertEqual([], ets:match_object(?META_TABLE, {{peer_incarnation, '_'}, '_'}))
    end).

invalid_journal_origin_aborts_capture_without_undoing_written_revision_test() ->
    with_metadata(fun() ->
        Peer = 'invalid_journal_origin@synthetic',
        Round = <<5:96>>,
        Exp = erlang:system_time(millisecond) + 60000,
        application:set_env(
            imboy,
            cache_registry_overrides,
            #{journal_origin_test => {replicate, false}}
        ),
        set_budget(#{whitelist => [journal_origin_test]}),
        ets:insert(?META_TABLE, {{snapshot_sender, self()}, #{round => Round}}),
        ok = imboy_cache_sync_apply:journal_refresh(),
        ok = imboy_cache_sync_apply:store_revision(
            {journal_origin_test, invalid}, {1, Peer}, Exp, deleted, 0, {node(), <<1:128>>, 1}
        ),
        receive
            {cache_snapshot_abort, _, Round, journal_entry_unavailable} -> ok
        after 0 -> ?assert(false)
        end,
        ?assertEqual({ok, []}, imboy_cache_sync_apply:journal_after(0)),
        ?assertMatch(
            [{_, {{1, Peer}, 1, Exp, deleted, 0}}],
            ets:lookup(?META_TABLE, {rev, {journal_origin_test, invalid}})
        )
    end).

journal_retains_explicit_origin_without_inventing_legacy_provenance_test() ->
    with_metadata(fun() ->
        Peer = 'journal_origin@synthetic',
        Origin = {Peer, <<4:128>>, 7},
        Exp = erlang:system_time(millisecond) + 60000,
        application:set_env(
            imboy,
            cache_registry_overrides,
            #{journal_origin_test => {replicate, false}}
        ),
        set_budget(#{whitelist => [journal_origin_test]}),
        ets:insert(?META_TABLE, {{snapshot_sender, self()}, #{round => <<4:96>>}}),
        ok = imboy_cache_sync_apply:journal_refresh(),
        ok = imboy_cache_sync_apply:store_revision(
            {journal_origin_test, origin}, {100, Peer}, Exp, deleted, 0, Origin
        ),
        ok = imboy_cache_sync_apply:store_revision(
            {journal_origin_test, legacy}, {101, Peer}, Exp, deleted, 0
        ),
        {ok, [
            {_, 0, {deleted, {journal_origin_test, origin}, undefined, Exp, {100, Peer}}, Origin},
            {_, 0, {deleted, {journal_origin_test, legacy}, undefined, Exp, {101, Peer}}, unknown}
        ]} =
            imboy_cache_sync_apply:journal_after(0),
        Rows = ets:match_object(?META_TABLE, {{journal, '_'}, '_'}),
        [{journal_window, {0, 2, 2, Bytes}}] = ets:lookup(?META_TABLE, journal_window),
        ?assertEqual(lists:sum([erlang:external_size(R) || R <- Rows]), Bytes),
        {Replay, 2} = imboy_cache_sync_apply:journal_keys_after(0),
        ?assertEqual(2, length(Replay))
    end).

coverage_projection_preserves_unclaimed_sender_payloads_test() ->
    with_metadata(fun() ->
        Other = spawn(fun() ->
            receive
                stop -> ok
            end
        end),
        Legacy = spawn(fun() ->
            receive
                stop -> ok
            end
        end),
        try
            Payload = lists:duplicate(10000, payload),
            ErrorRow = {{snapshot_sender, Other}, #{
                round => <<2:96>>, application_coverage => {error, missing}, prepared => Payload
            }},
            LegacyRow = {{snapshot_sender, Legacy}, #{round => <<3:96>>, prepared => Payload}},
            ets:insert(?META_TABLE, [
                ErrorRow,
                LegacyRow,
                {{snapshot_sender, self()}, #{
                    round => <<1:96>>,
                    application_coverage => {ok, []},
                    prepared => Payload
                }}
            ]),
            ok = imboy_cache_sync_apply:abort_application_coverage(projection_test),
            receive
                {cache_snapshot_abort, _, <<1:96>>, projection_test} -> ok
            after 0 -> ?assert(false)
            end,
            ?assertEqual([], ets:lookup(?META_TABLE, {snapshot_sender, self()})),
            ?assertEqual([ErrorRow], ets:lookup(?META_TABLE, {snapshot_sender, Other})),
            ?assertEqual([LegacyRow], ets:lookup(?META_TABLE, {snapshot_sender, Legacy})),
            ?assertEqual({message_queue_len, 0}, process_info(Other, message_queue_len)),
            ?assertEqual({message_queue_len, 0}, process_info(Legacy, message_queue_len)),
            ?assertEqual([{journal_active, true}], ets:lookup(?META_TABLE, journal_active))
        after
            Other ! stop,
            Legacy ! stop
        end
    end).

application_coverage_lifecycle_retracts_captured_cursor_test() ->
    lists:foreach(
        fun(Kind) ->
            with_metadata(fun() ->
                Peer = 'coverage_lifecycle@synthetic',
                Boot = <<4:128>>,
                Round = <<2:96>>,
                registered = imboy_cache_sync_proto:register_incarnation(Peer, Boot),
                ok = imboy_cache_sync_proto:note_applied_sequence(Peer, Boot, 1),
                Candidate = imboy_cache_sync_proto:application_coverage_candidate(),
                ets:insert(
                    ?META_TABLE,
                    {{snapshot_sender, self()}, #{round => Round, application_coverage => Candidate}}
                ),
                Reason =
                    case Kind of
                        reset ->
                            ok = imboy_cache_sync_proto:reset_sequence_coverage(),
                            coverage_reset;
                        epoch ->
                            ok = imboy_cache_sync_proto:note_applied_event(
                                #{
                                    origin_node => Peer,
                                    origin_boot_id => Boot,
                                    origin_seq => 2,
                                    epoch => 1
                                },
                                {ok, ignored}
                            ),
                            coverage_epoch_changed;
                        new_identity ->
                            registered = imboy_cache_sync_proto:register_incarnation(
                                'coverage_new@synthetic', <<5:128>>
                            ),
                            coverage_identity_changed
                    end,
                receive
                    {cache_snapshot_abort, _, Round, Reason} -> ok
                after 0 -> ?assert(false)
                end,
                ?assertEqual([], ets:lookup(?META_TABLE, {snapshot_sender, self()}))
            end)
        end,
        [reset, epoch, new_identity]
    ).

snapshot_cursor_captures_immutable_application_evidence_test() ->
    with_metadata(fun() ->
        Peer = 'cursor_coverage@synthetic',
        Boot = <<7:128>>,
        Round = <<1:96>>,
        Exp = erlang:system_time(millisecond) + 60000,
        application:set_env(
            imboy,
            cache_registry_overrides,
            #{coverage_cursor_test => {replicate, false}}
        ),
        set_budget(#{whitelist => [coverage_cursor_test], p_max => 1}),
        registered = imboy_cache_sync_proto:register_incarnation(Peer, Boot),
        ok = imboy_cache_sync_proto:note_applied_sequence(Peer, Boot, 1),
        ets:insert(?META_TABLE, [
            {node_boot, {node(), <<8:128>>}},
            {{rev, {coverage_cursor_test, 1}}, {{1, Peer}, 1, Exp, deleted, 0}},
            {{rev, {coverage_cursor_test, 2}}, {{2, Peer}, 2, Exp, deleted, 0}}
        ]),
        ok = imboy_cache_sync_snapshot:send_snapshot(self(), Round),
        receive
            {cache_snapshot_page, _, Round, 1, _, _, true} -> ok
        after 0 -> ?assert(false)
        end,
        [{_, Cursor}] = ets:lookup(?META_TABLE, {snapshot_sender, self()}),
        ?assertEqual(Round, maps:get(round, Cursor)),
        ?assertEqual({node(), <<8:128>>}, maps:get(boot, Cursor)),
        ?assertEqual({ok, [{Peer, Boot, 0, 1}]}, maps:get(application_coverage, Cursor)),
        ok = imboy_cache_sync_proto:note_applied_sequence(Peer, Boot, 2),
        ?assertEqual(
            {ok, [{Peer, Boot, 0, 2}]},
            imboy_cache_sync_proto:application_coverage_candidate()
        ),
        [{_, Unchanged}] = ets:lookup(?META_TABLE, {snapshot_sender, self()}),
        ?assertEqual(Cursor, Unchanged),
        changed = imboy_cache_sync_proto:register_incarnation(Peer, <<9:128>>),
        receive
            {cache_snapshot_abort, _, Round, coverage_identity_changed} -> ok
        after 0 -> ?assert(false)
        end,
        ?assertEqual([], ets:lookup(?META_TABLE, {snapshot_sender, self()}))
    end).

application_coverage_candidate_requires_every_current_identity_test() ->
    with_metadata(fun() ->
        Peer = 'candidate_peer@synthetic',
        Boot = <<4:128>>,
        ?assertEqual({ok, []}, imboy_cache_sync_proto:application_coverage_candidate()),
        %% 表手术直接登记化身（绕过真实登记函数的零下限初始化）构造缺项。
        true = ets:insert(?META_TABLE, {{peer_incarnation, Peer}, Boot}),
        ?assertEqual(
            {error, incomplete_application_coverage},
            imboy_cache_sync_proto:application_coverage_candidate()
        ),
        ets:insert(?META_TABLE, {{covered_seq, {Peer, Boot}}, {0, 0}}),
        ?assertEqual(
            {ok, [{Peer, Boot, 0, 0}]},
            imboy_cache_sync_proto:application_coverage_candidate()
        ),
        ok = imboy_cache_sync_proto:note_applied_sequence(Peer, Boot, 1),
        ?assertEqual(
            {ok, [{Peer, Boot, 0, 1}]},
            imboy_cache_sync_proto:application_coverage_candidate()
        ),
        ets:insert(?META_TABLE, {{covered_seq, {Peer, Boot}}, {1, 1}}),
        ?assertEqual(
            {error, incomplete_application_coverage},
            imboy_cache_sync_proto:application_coverage_candidate()
        ),
        ets:insert(?META_TABLE, {{covered_seq, {Peer, Boot}}, 99}),
        ?assertEqual(
            {error, incomplete_application_coverage},
            imboy_cache_sync_proto:application_coverage_candidate()
        ),
        ets:insert(?META_TABLE, {{covered_seq, {Peer, Boot}}, {0, 1}}),
        %% 真实登记函数维护不变量：换 Boot 删除旧下限并重建当前代零前缀行。
        changed = imboy_cache_sync_proto:register_incarnation(Peer, <<5:128>>),
        ?assertEqual(
            {ok, [{Peer, <<5:128>>, 0, 0}]},
            imboy_cache_sync_proto:application_coverage_candidate()
        ),
        %% 表手术改登记化身但不建行：换 Boot 缺项仍整份拒绝。
        true = ets:insert(?META_TABLE, {{peer_incarnation, Peer}, <<6:128>>}),
        ?assertEqual(
            {error, incomplete_application_coverage},
            imboy_cache_sync_proto:application_coverage_candidate()
        )
    end).

application_coverage_candidate_rejects_count_and_byte_overflow_test() ->
    with_metadata(fun() ->
        Boot = <<6:128>>,
        Peers = [
            list_to_atom(lists:duplicate(240, $a) ++ integer_to_list(N))
         || N <- lists:seq(1, 20)
        ],
        ets:insert(
            ?META_TABLE,
            lists:append([
                [{{peer_incarnation, P}, Boot}, {{covered_seq, {P, Boot}}, {0, 1}}]
             || P <- Peers
            ])
        ),
        Before = ets:info(?META_TABLE, size),
        ?assertEqual(
            {error, coverage_byte_budget},
            imboy_cache_sync_proto:application_coverage_candidate()
        ),
        set_budget(#{peer_max => 1}),
        ?assertEqual(
            {error, coverage_peer_budget},
            imboy_cache_sync_proto:application_coverage_candidate()
        ),
        ?assertEqual(Before, ets:info(?META_TABLE, size))
    end).

coverage_prefix_cannot_stitch_different_epochs_test() ->
    with_metadata(fun() ->
        Peer = 'epoch_coverage@synthetic',
        Boot = <<3:128>>,
        registered = imboy_cache_sync_proto:register_incarnation(Peer, Boot),
        E = #{origin_node => Peer, origin_boot_id => Boot, origin_seq => 1, epoch => 0},
        K = {covered_seq, {Peer, Boot}},
        ok = imboy_cache_sync_proto:note_applied_event(E, {ok, ignored}),
        ?assertEqual([{K, {0, 1}}], ets:lookup(?META_TABLE, K)),
        ok = imboy_cache_sync_proto:note_applied_event(
            E#{epoch => 1, origin_seq => 2}, {ok, ignored}
        ),
        ?assertEqual([{K, {1, 0}}], ets:lookup(?META_TABLE, K)),
        ok = imboy_cache_sync_proto:note_applied_event(E, {ok, ignored}),
        ?assertEqual([{K, {1, 0}}], ets:lookup(?META_TABLE, K)),
        ok = imboy_cache_sync_apply:adopt_epoch(1),
        %% 采纳新代重建登记来源的零前缀行（不继承旧代计数）。
        ?assertEqual([{K, {1, 0}}], ets:lookup(?META_TABLE, K)),
        ok = imboy_cache_sync_proto:note_applied_event(E, {ok, ignored}),
        ?assertEqual([{K, {1, 0}}], ets:lookup(?META_TABLE, K)),
        ok = imboy_cache_sync_proto:note_applied_event(E#{epoch => 1}, {ok, ignored}),
        ?assertEqual([{K, {1, 1}}], ets:lookup(?META_TABLE, K))
    end).

diagnostic_snapshot_filters_payload_rows_inside_ets_test() ->
    with_metadata(fun() ->
        Kept = [
            {status, ready},
            {journal, counter},
            {{journal}, one_element_key},
            {{journal, one, two}, three_element_key},
            {{rev, key}, version},
            {<<"key">>, binary_key}
        ],
        Hidden = [
            {{journal, 1}, lists:duplicate(10000, payload)},
            {{snapshot_sender, peer}, #{buffer => lists:duplicate(10000, payload)}}
        ],
        ets:delete_all_objects(?META_TABLE),
        ets:insert(?META_TABLE, Kept ++ Hidden),
        ?assertEqual(lists:sort(Kept), lists:sort(imboy_cache_sync_apply:metadata_snapshot())),
        ?assertEqual(lists:sort(Kept ++ Hidden), lists:sort(ets:tab2list(?META_TABLE)))
    end).

observed_sequence_is_not_contiguous_application_coverage_test() ->
    with_metadata(fun() ->
        Peer = 'coverage_peer@synthetic',
        Boot = <<1:128>>,
        registered = imboy_cache_sync_proto:register_incarnation(Peer, Boot),
        E = #{origin_node => Peer, origin_boot_id => Boot, origin_seq => 1, epoch => 0},
        ok = imboy_cache_sync_apply:detect_gap({Peer, Boot}, 1, ignored),
        ok = imboy_cache_sync_proto:note_applied_event(E, {reject, index_full, ignored}),
        %% index_full 拒绝不推进前缀；登记初始化的零下限行保持为零。
        ?assertEqual(
            [{{covered_seq, {Peer, Boot}}, {0, 0}}],
            ets:lookup(?META_TABLE, {covered_seq, {Peer, Boot}})
        ),
        ok = imboy_cache_sync_proto:note_applied_event(E, {ok, ignored}),
        gap = imboy_cache_sync_apply:detect_gap({Peer, Boot}, 3, ignored),
        ok = imboy_cache_sync_proto:note_applied_event(E#{origin_seq => 3}, {ok, ignored}),
        ?assertEqual([{{seq, {Peer, Boot}}, 3}], ets:lookup(?META_TABLE, {seq, {Peer, Boot}})),
        ?assertEqual(
            [{{covered_seq, {Peer, Boot}}, {0, 1}}],
            ets:lookup(?META_TABLE, {covered_seq, {Peer, Boot}})
        ),
        ok = imboy_cache_sync_proto:note_applied_event(
            E#{origin_seq => 2}, {reject, stale_revision, ignored}
        ),
        ?assertEqual(
            [{{covered_seq, {Peer, Boot}}, {0, 2}}],
            ets:lookup(?META_TABLE, {covered_seq, {Peer, Boot}})
        ),
        changed = imboy_cache_sync_proto:register_incarnation(Peer, <<2:128>>),
        ?assertEqual([], ets:lookup(?META_TABLE, {covered_seq, {Peer, Boot}})),
        ok = imboy_cache_sync_proto:note_applied_sequence(Peer, Boot, 1),
        ?assertEqual([], ets:lookup(?META_TABLE, {covered_seq, {Peer, Boot}})),
        ok = imboy_cache_sync_proto:note_applied_sequence(Peer, <<2:128>>, 1),
        ok = imboy_cache_sync_apply:flush_replicas(),
        %% reset 清的是覆盖证据不是登记：当前登记化身保留零下限行（冻结合同
        %% 不变量），缺项态只允许表手术构造。
        ?assertEqual(
            [{{covered_seq, {Peer, <<2:128>>}}, {0, 0}}],
            ets:match_object(?META_TABLE, {{covered_seq, '_'}, '_'})
        )
    end).

discovery_and_reconciliation_reject_over_capacity_scope_test() ->
    lists:foreach(
        fun(Kind) ->
            with_metadata(fun() ->
                set_budget(#{peer_max => 1}),
                Pid = fun(N) ->
                    B = atom_to_binary(N, utf8),
                    binary_to_term(<<131, 103, 100, (byte_size(B)):16, B/binary, 1:32, 0:32, 0:8>>)
                end,
                Members = [
                    {Pid('capacity_a@synthetic'), undefined},
                    {Pid('capacity_b@synthetic'), undefined}
                ],
                meck:new(syn, [unstick, passthrough, no_link]),
                meck:expect(syn, members, 2, fun(_, _) -> Members end),
                Deadline = erlang:system_time(millisecond) + 60000,
                State = #state{boot_id = <<1:128>>, status = ready, recover_deadline_ms = Deadline},
                try
                    Next =
                        case Kind of
                            discovery ->
                                {noreply, N} = imboy_cache_sync_recover:discover(
                                    State#state{status = discovering}
                                ),
                                N;
                            reconcile ->
                                imboy_cache_sync_recover:periodic_reconcile(State)
                        end,
                    ?assertEqual(degraded, Next#state.status),
                    ?assert(Next#state.requires_authority),
                    ?assertEqual(undefined, Next#state.snapshot_round),
                    ?assertEqual(undefined, Next#state.recovery_timer),
                    ?assertEqual([], ets:match_object(?META_TABLE, {{peer_incarnation, '_'}, '_'})),
                    ?assertEqual([], ets:match_object(?META_TABLE, {{peer_counter, '_'}, '_'}))
                after
                    meck:unload(syn)
                end
            end)
        end,
        [discovery, reconcile]
    ).

peer_budget_rejects_new_origin_without_allocating_metadata_test() ->
    with_metadata(fun() ->
        set_budget(#{peer_max => 1}),
        Peer = 'retained_peer@synthetic',
        Other = 'rejected_peer@synthetic',
        registered = imboy_cache_sync_proto:register_incarnation(Peer, <<1:128>>),
        Before = ets:tab2list(?META_TABLE),
        ?assertEqual(
            {error, incarnation_budget_exceeded},
            imboy_cache_sync_proto:register_incarnation(Other, <<2:128>>)
        ),
        ?assertEqual(lists:sort(Before), lists:sort(ets:tab2list(?META_TABLE))),
        ?assertEqual(ok, imboy_cache_sync_proto:check_incarnation(Peer, <<1:128>>)),
        ?assertEqual(changed, imboy_cache_sync_proto:register_incarnation(Peer, <<2:128>>)),
        Event = #{
            config_hash => <<0:128>>,
            registry_hash => <<0:128>>,
            origin_node => Other,
            origin_boot_id => <<2:128>>,
            epoch => imboy_cache_sync_proto:epoch()
        },
        BeforeHash = ets:tab2list(?META_TABLE),
        ?assertEqual(
            {error, hash_mismatch}, imboy_cache_sync_proto:check_consistency(Event, <<3:128>>)
        ),
        ?assertEqual(lists:sort(BeforeHash), lists:sort(ets:tab2list(?META_TABLE)))
    end).

peer_report_overflow_does_not_allocate_epoch_or_counter_test() ->
    with_metadata(fun() ->
        set_budget(#{peer_max => 1}),
        ?assertEqual(1, imboy_cache_policy:peer_max()),
        registered = imboy_cache_sync_proto:register_incarnation('occupied_peer@synthetic', <<
            3:128
        >>),
        Peer = 'over_capacity_report@synthetic',
        Deadline = erlang:system_time(millisecond) + 60000,
        R = imboy_cache_sync_snapshot:expect_peer(
            imboy_cache_sync_snapshot:new_round(0, Deadline), Peer
        ),
        State = #state{
            boot_id = <<1:128>>,
            status = recovering,
            snapshot_round = R,
            recover_deadline_ms = Deadline
        },
        Next = imboy_cache_sync_recover:handle_version_report(
            Peer,
            20,
            imboy_cache_sync_proto:epoch(),
            imboy_cache_sync_proto:config_hash(),
            imboy_cache_sync_proto:registry_hash(),
            <<2:128>>,
            true,
            self(),
            State
        ),
        ?assertNotEqual(ready, Next#state.status),
        ?assert(Next#state.requires_authority),
        ?assertEqual(undefined, Next#state.snapshot_round),
        [
            ?assertEqual([], ets:lookup(?META_TABLE, {Kind, Peer}))
         || Kind <- [peer_incarnation, peer_epoch, peer_counter, hash_divergent]
        ],
        Ref = Next#state.recovery_timer,
        erlang:cancel_timer(Ref),
        receive
            {timeout, Ref, cache_recovery} -> ok
        after 0 -> ok
        end
    end).

incarnation_grace_budget_rejects_without_replacing_current_boot_test() ->
    with_metadata(fun() ->
        set_budget(#{grace_max => 2}),
        Peer = 'bounded_grace@synthetic',
        registered = imboy_cache_sync_proto:register_incarnation(Peer, <<1:128>>),
        changed = imboy_cache_sync_proto:register_incarnation(Peer, <<2:128>>),
        changed = imboy_cache_sync_proto:register_incarnation(Peer, <<3:128>>),
        ets:insert(?META_TABLE, {{seq, {Peer, <<3:128>>}}, 7}),
        Before = ets:tab2list(?META_TABLE),
        ?assertEqual(
            {error, incarnation_budget_exceeded},
            imboy_cache_sync_proto:register_incarnation(Peer, <<4:128>>)
        ),
        ?assertEqual(lists:sort(Before), lists:sort(ets:tab2list(?META_TABLE))),
        ?assertEqual(ok, imboy_cache_sync_proto:check_incarnation(Peer, <<3:128>>)),
        ?assertEqual(ok, imboy_cache_sync_proto:check_incarnation(Peer, <<1:128>>)),
        ets:insert(?META_TABLE, {
            {grace_incarnation, {Peer, <<1:128>>}},
            erlang:system_time(millisecond) - imboy_cache_policy:grace_window_ms()
        }),
        ?assertEqual(changed, imboy_cache_sync_proto:register_incarnation(Peer, <<4:128>>)),
        ?assertEqual([], ets:lookup(?META_TABLE, {seq, {Peer, <<3:128>>}})),
        ?assertEqual(
            2,
            ets:select_count(?META_TABLE, [
                {{{grace_incarnation, '_'}, '_'}, [], [true]}
            ])
        )
    end).

grace_pruning_preserves_other_metadata_and_window_boundary_test() ->
    with_metadata(fun() ->
        Now = erlang:system_time(millisecond),
        W = imboy_cache_policy:grace_window_ms(),
        Expired = {grace_incarnation, {'grace_peer@synthetic', <<1:128>>}},
        Boundary = {grace_incarnation, {'grace_peer@synthetic', <<2:128>>}},
        Retained = {grace_incarnation, {'grace_peer@synthetic', <<3:128>>}},
        Journal = {{journal, 1}, {Now, 0, binary:copy(<<1>>, 400000)}},
        Current = {{peer_incarnation, 'grace_peer@synthetic'}, <<4:128>>},
        ets:insert(?META_TABLE, [
            {Expired, Now - W - 1},
            {Boundary, Now - W},
            {Retained, Now - W + 1},
            Journal,
            Current
        ]),
        Before = ets:tab2list(?META_TABLE),
        ok = imboy_cache_sync_proto:prune_grace(Now),
        Expected = [Row || Row = {K, _} <- Before, K =/= Expired, K =/= Boundary],
        ?assertEqual(lists:sort(Expected), lists:sort(ets:tab2list(?META_TABLE)))
    end).

metadata_accounting_tracks_overwrite_and_sweep_test() ->
    with_metadata(fun() ->
        Key = {cache_v2_test_rep, binary:copy(<<0>>, 200)},
        Rev = {1, node()},
        Exp = erlang:system_time(millisecond) + 60000,
        ok = imboy_cache_sync_apply:store_revision(Key, Rev, Exp, live),
        ok = imboy_cache_sync_apply:index_replicate(Key),
        assert_accounting(),
        ok = imboy_cache_sync_apply:store_revision(Key, {1 bsl 256, node()}, Exp, live),
        assert_accounting(),
        ok = imboy_cache_sync_apply:store_revision(Key, {2, node()}, 0, deleted),
        assert_accounting(),
        ok = imboy_cache_sync_recover:bounded_sweep(),
        ?assertEqual([], ets:lookup(?META_TABLE, {rev, Key})),
        ?assertEqual([], ets:lookup(?META_TABLE, {rindex, Key})),
        ?assertEqual([{metadata_usage, {0, 0, 0, 0}}], ets:lookup(?META_TABLE, metadata_usage)),
        assert_accounting()
    end).

new_and_existing_keys_obey_byte_budget_test() ->
    with_metadata(fun() ->
        Key = {cache_v2_test_rep, bytes},
        Exp = erlang:system_time(millisecond) + 60000,
        Rows = imboy_cache_sync_apply:prospective_rows(set, Key, {1, node()}, Exp),
        Bytes = lists:sum([erlang:external_size(R) || R <- Rows]),
        set_budget(#{n_max => 100, n_max_bytes => Bytes}),
        ?assert(imboy_cache_sync_apply:metadata_fits(Rows)),
        lists:foreach(fun imboy_cache_sync_apply:store_metadata/1, Rows),
        ?assert(imboy_cache_sync_apply:metadata_fits(Rows)),
        Larger = imboy_cache_sync_apply:prospective_rows(set, Key, {1 bsl 1024, node()}, Exp),
        ?assertNot(imboy_cache_sync_apply:metadata_fits(Larger)),
        Other = imboy_cache_sync_apply:prospective_rows(
            set, {cache_v2_test_rep, other}, {2, node()}, Exp
        ),
        ?assertNot(imboy_cache_sync_apply:metadata_fits(Other)),
        ?assertNot(imboy_cache_sync_apply:metadata_fits(Rows ++ Larger)),
        assert_accounting()
    end).

tombstone_bytes_and_repeated_delete_are_bounded_test() ->
    with_metadata(fun() ->
        Key = {cache_v2_test_rep, tombstone},
        Exp = erlang:system_time(millisecond) + 720000,
        [Row] = imboy_cache_sync_apply:prospective_rows(delete, Key, {1, node()}, Exp),
        Bytes = erlang:external_size(Row),
        set_budget(#{t_max => 1, t_max_bytes => Bytes}),
        ?assertNot(imboy_cache_sync_apply:tombstone_budget_exceeded(Key, {1, node()}, Exp)),
        ok = imboy_cache_sync_apply:store_metadata(Row),
        ?assertNot(imboy_cache_sync_apply:tombstone_budget_exceeded(Key, {2, node()}, Exp)),
        ?assert(imboy_cache_sync_apply:tombstone_budget_exceeded(Key, {1 bsl 1024, node()}, Exp)),
        ?assert(
            imboy_cache_sync_apply:tombstone_budget_exceeded(
                {cache_v2_test_rep, second}, {2, node()}, Exp
            )
        ),
        assert_accounting()
    end).

sender_budget_rejection_preserves_clock_and_metadata_test() ->
    with_metadata(fun() ->
        set_budget(#{whitelist => [cache_v2_test_rep], n_max_bytes => 1}),
        application:set_env(imboy, cache_registry_overrides, #{
            cache_v2_test_rep => {replicate, false}
        }),
        true = register(imboy_cache_sync, self()),
        State = #state{status = ready, boot_id = <<2:128>>},
        Before = ets:tab2list(?META_TABLE),
        %% 本项隔离测试容量门；先提供已声明 V2 的自身成员视图。
        meck:new(syn, [no_link, passthrough]),
        meck:expect(syn, members, fun(imboy_cache, dsync_handler) ->
            [{self(), #{cache_protocol => 2}}]
        end),
        try
            ?assertEqual(
                {reply, {error, index_full}, State},
                imboy_cache_sync:handle_call(
                    {publish_set, {cache_v2_test_rep, key}, value, 60}, {self(), ref}, State
                )
            )
        after
            meck:unload(syn)
        end,
        ?assertEqual(Before, ets:tab2list(?META_TABLE)),
        true = unregister(imboy_cache_sync)
    end).

stale_snapshot_cannot_borrow_metadata_space_test() ->
    with_metadata(fun() ->
        application:set_env(imboy, cache_registry_overrides, #{
            cache_v2_test_rep => {replicate, false}
        }),
        Key = {cache_v2_test_rep, retained},
        NewKey = {cache_v2_test_rep, added},
        Exp = erlang:system_time(millisecond) + 60000,
        ok = imboy_cache_sync_apply:store_revision(Key, {1 bsl 4096, node()}, Exp, live),
        ok = imboy_cache_sync_apply:index_replicate(Key),
        [{_, {_, Bytes, _, _}}] = ets:lookup(?META_TABLE, metadata_usage),
        set_budget(#{whitelist => [cache_v2_test_rep], n_max_bytes => Bytes}),
        Peer = 'snapshot_peer@synthetic',
        R0 = imboy_cache_sync_snapshot:new_round(0, Exp),
        RoundState = requested_peer(
            imboy_cache_sync_snapshot:expect_peer(R0, Peer), Peer
        ),
        Round = maps:get(round, RoundState),
        Entries = [{Key, value, Exp, {1, node()}}, {NewKey, value, Exp, {2, node()}}],
        Digest = imboy_cache_sync_snapshot:page_digest(Round, 1, Entries),
        Before = ets:tab2list(?META_TABLE),
        ?assertEqual(
            {abort, index_full, RoundState},
            imboy_cache_sync_snapshot:handle_page(
                Peer, Round, 1, Entries, Digest, false, RoundState
            )
        ),
        ?assertEqual(Before, ets:tab2list(?META_TABLE)),
        assert_accounting()
    end).

cold_recovery_rejects_older_epoch_authority_test() ->
    with_metadata(fun() ->
        Peer = 'cold_authority@synthetic',
        Epoch = 2,
        ets:insert(?META_TABLE, {sync_epoch, Epoch}),
        R0 = imboy_cache_sync_snapshot:recovery_round(0, erlang:system_time(millisecond) + 60000),
        Excluded = 'excluded_epoch@synthetic',
        R1 = imboy_cache_sync_snapshot:expect_peer(
            imboy_cache_sync_snapshot:expect_peer(R0, Peer), Excluded
        ),
        State = #state{
            status = recovering, boot_id = <<2:128>>, requires_authority = true, snapshot_round = R1
        },
        S1 = imboy_cache_sync_recover:handle_version_report(
            Peer,
            1,
            Epoch - 1,
            imboy_cache_sync_proto:config_hash(),
            imboy_cache_sync_proto:registry_hash(),
            <<3:128>>,
            true,
            self(),
            State
        ),
        ?assertNotEqual(ready, S1#state.status),
        PeerState = maps:get(Peer, maps:get(peers, S1#state.snapshot_round)),
        ?assertNot(maps:get(authority, PeerState)),
        Entries = [
            {{cache_v2_test_rep, old}, value, erlang:system_time(millisecond) + 60000, {1, Peer}}
        ],
        Round = maps:get(round, S1#state.snapshot_round),
        Digest = imboy_cache_sync_snapshot:page_digest(Round, 1, Entries),
        ?assertMatch(
            {abort, _, _},
            imboy_cache_sync_snapshot:handle_page(
                Peer,
                Round,
                1,
                Entries,
                Digest,
                false,
                S1#state.snapshot_round
            )
        ),
        ?assertEqual([], ets:lookup(?META_TABLE, {rev, {cache_v2_test_rep, old}})),
        %% 合格新 epoch 来源可成为权威；但回合内已登记的 hash 不一致来源
        %% 不可证明，按冻结合同 fail-closed：完整回合也不得产出 READY。
        Classified = imboy_cache_sync_recover:handle_version_report(
            Excluded,
            1,
            99,
            <<4:128>>,
            imboy_cache_sync_proto:registry_hash(),
            <<5:128>>,
            true,
            self(),
            S1
        ),
        S2 = imboy_cache_sync_recover:handle_version_report(
            Peer,
            1,
            Epoch,
            imboy_cache_sync_proto:config_hash(),
            imboy_cache_sync_proto:registry_hash(),
            <<3:128>>,
            true,
            self(),
            Classified
        ),
        {ok, DoneRound} = imboy_cache_source_cut_tests:done(
            Peer, Round, 1, 0, S2#state.snapshot_round
        ),
        ?assertEqual(
            {error, peer_content_unproven}, imboy_cache_policy:verify_snapshot_coverage(DoneRound)
        ),
        S3 = imboy_cache_sync_recover:maybe_complete_round(S2#state{snapshot_round = DoneRound}),
        try
            ?assertNotEqual(ready, S3#state.status),
            ?assertEqual(recovering, S3#state.status),
            ?assertEqual(1, maps:get(rounds_used, S3#state.snapshot_round))
        after
            cancel_recovery_timer(S3#state.recovery_timer)
        end,
        %% 新回合独立证明旧代来源；不改写前一回合冻结的 epoch99 义务。
        Fresh = imboy_cache_sync_snapshot:recovery_round(
            0, erlang:system_time(millisecond) + 60000
        ),
        Expected = imboy_cache_sync_snapshot:expect_peer(
            imboy_cache_sync_snapshot:expect_peer(Fresh, Peer), Excluded
        ),
        Token = maps:get(round, Expected),
        {[], OldCut, _, OldCensus} = ready_source_fixture:snapshot(
            Excluded, <<5:128>>, Epoch - 1, 0, [], Token
        ),
        {cache_snapshot_cut, Excluded, Token, OldBoot, OldEpoch, OldSeq, Hash, Registry, _, _} =
            OldCut,
        OldReport = imboy_cache_sync_recover:handle_version_report(
            Excluded,
            1000,
            OldEpoch,
            Hash,
            Registry,
            OldBoot,
            true,
            self(),
            {Excluded, OldBoot, OldEpoch, OldSeq, Token},
            State#state{snapshot_round = Expected}
        ),
        imboy_cache_source_cut_tests:fixture_identity(Excluded, OldBoot, OldEpoch),
        {ok, RecordedOld} = imboy_cache_sync_proto:record_origin_target(
            {Excluded, OldBoot, OldEpoch, OldSeq, Token, OldCensus, self()},
            OldReport#state.snapshot_round
        ),
        Positive = imboy_cache_sync_recover:handle_version_report(
            Peer, 1000, Epoch, Hash, Registry, <<3:128>>, true, self(), OldReport#state{
                snapshot_round = RecordedOld
            }
        ),
        receive
            {cache_snapshot_request, Pid, Token} when Pid =:= self() -> ok
        after 0 -> ?assert(false)
        end,
        {[], Cut, Done, _} = ready_source_fixture:snapshot(Peer, <<3:128>>, Epoch, 0, [], Token),
        {ok, Complete} = ready_source_fixture:finish(
            Peer, {Cut, Done}, Positive#state.snapshot_round
        ),
        S5 = imboy_cache_sync_recover:maybe_complete_round(Positive#state{snapshot_round = Complete}),
        try
            ?assertEqual(ready, S5#state.status),
            ?assertNot(S5#state.requires_authority),
            ?assertEqual(Epoch, imboy_cache_sync_proto:epoch())
        after
            cancel_recovery_timer(S5#state.recovery_timer)
        end
    end).

cancel_recovery_timer(undefined) ->
    ok;
cancel_recovery_timer(Timer) ->
    erlang:cancel_timer(Timer),
    receive
        {timeout, Timer, cache_recovery} -> ok
    after 0 -> ok
    end.

assert_accounting() ->
    Rows = [
        R
     || {{Kind, _}, _} = R <- ets:tab2list(?META_TABLE), Kind =:= rev orelse Kind =:= rindex
    ],
    Index = [R || {{rindex, _}, _} = R <- Rows],
    Tombs = [R || {{rev, _}, {_, _, _, deleted, _}} = R <- Rows],
    Expected = {
        length(Index),
        lists:sum([erlang:external_size(R) || R <- Rows]),
        length(Tombs),
        lists:sum([erlang:external_size(R) || R <- Tombs])
    },
    ?assertEqual([{metadata_usage, Expected}], ets:lookup(?META_TABLE, metadata_usage)).

set_budget(Config) -> application:set_env(imboy, cache_replication, Config).

with_metadata(Fun) ->
    pure_mock_fixture:run(fun() -> metadata_fixture_checks(Fun) end).

metadata_fixture_checks(Fun) ->
    OldCfg = application:get_env(imboy, cache_replication),
    OldRegistry = application:get_env(imboy, cache_registry_overrides),
    %% 真实来源身份：source_identity_current 要求来源 pid 在 syn 成员视图内。
    {ok, _} = application:ensure_all_started(syn),
    ok = syn:add_node_to_scopes([imboy_cache]),
    ok = syn:join(imboy_cache, dsync_handler, self(), #{}),
    ets:new(?META_TABLE, [named_table, public, set]),
    ets:insert(?META_TABLE, {status, ready}),
    try
        Fun()
    after
        clear_fixture_snapshot_requests(),
        _ = syn:leave(imboy_cache, dsync_handler, self()),
        ets:delete(?META_TABLE),
        case whereis(imboy_cache_sync) of
            P when P =:= self() -> unregister(imboy_cache_sync);
            _ -> ok
        end,
        restore(cache_replication, OldCfg),
        restore(cache_registry_overrides, OldRegistry)
    end.
clear_fixture_snapshot_requests() ->
    receive
        {cache_snapshot_request, Pid, _} when Pid =:= self() -> clear_fixture_snapshot_requests()
    after 0 -> ok
    end.
restore(Key, undefined) -> application:unset_env(imboy, Key);
restore(Key, {ok, Value}) -> application:set_env(imboy, Key, Value).

requested_peer(Round, Peer) ->
    Authorized = imboy_cache_sync_snapshot:authorize_peer(Round, Peer),
    imboy_cache_sync_snapshot:mark_requested(Authorized, Peer).
