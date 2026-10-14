-module(imboy_cache_entry_epoch_tests).
-include_lib("eunit/include/eunit.hrl").
-include("imboy_cache_sync_state.hrl").
higher_epoch_page_can_replace_lower_epoch_tombstone_test() ->
    with_epoch(fun(Cache) ->
        K = {entry_epoch_test, replacement},
        Exp = expiry(),
        imboy_cache_sync_apply:store_revision(K, {1000, node()}, Exp, deleted, 0),
        Peer = 'higher_epoch@synthetic',
        R0 = requested_round(Peer, 1),
        Entry = {K, fresh, Exp, {10, Peer}},
        {ok, R1} = page(Peer, Entry, R0),
        ?assertMatch([{_, {{10, Peer}, 10, Exp, live, 1}}], ets:lookup(?META_TABLE, {rev, K})),
        ?assertEqual([{K, fresh}], ets:lookup(Cache, K)),
        {ok, R2} = imboy_cache_source_cut_tests:done(Peer, maps:get(round, R1), 1000, 1, R1),
        S = imboy_cache_sync_recover:maybe_complete_round(#state{
            status = recovering,
            counter = 1000,
            snapshot_round = R2
        }),
        ?assertEqual(ready, S#state.status),
        ?assertEqual(1000, S#state.counter),
        ?assertEqual({ok, fresh}, imboy_cache:get(K))
    end).

expired_newer_live_revision_survives_sweep_and_rejects_old_long_ttl_event_test() ->
    with_epoch(fun(_) ->
        K = {entry_epoch_test, expired_newer},
        Peer = 'expiry_replay@synthetic',
        Boot = <<2:128>>,
        ets:insert(?META_TABLE, {{peer_incarnation, Peer}, Boot}),
        ok = imboy_cache_sync_apply:store_revision(
            K,
            {200, Peer},
            erlang:system_time(millisecond) - 1,
            live,
            0
        ),
        ok = imboy_cache_sync_apply:index_replicate(K),
        ok = imboy_cache_sync_recover:bounded_sweep(),
        Event = (imboy_cache_sync_proto:event(set, K, stale, expiry(), {100, Peer}, 1, Boot))#{
            origin_node => Peer
        },
        ?assertMatch(
            {reject, stale_revision, #state{status = ready, counter = 200}},
            imboy_cache_sync_apply:apply_v2(
                Event,
                #state{boot_id = <<1:128>>, status = ready, counter = 200}
            )
        ),
        ?assertEqual(0, meck:num_calls(depcache, set, 5)),
        ?assertEqual({200, Peer}, imboy_cache_sync:current_rev(K)),
        %% 保留仍有界：越过窗口后版本与索引一起回收，账目归零。
        Past = erlang:system_time(millisecond) - imboy_cache_policy:tombstone_ttl_sec() * 1000 - 1,
        ok = imboy_cache_sync_apply:store_revision(K, {200, Peer}, Past, live, 0),
        ok = imboy_cache_sync_recover:bounded_sweep(),
        ?assertEqual([], ets:lookup(?META_TABLE, {rev, K})),
        ?assertEqual([], ets:lookup(?META_TABLE, {rindex, K})),
        ?assertEqual([{metadata_usage, {0, 0, 0, 0}}], ets:lookup(?META_TABLE, metadata_usage))
    end).

expired_source_revision_floor_blocks_other_source_old_live_page_test() ->
    with_epoch(fun(Cache) ->
        K = {entry_epoch_test, cold_floor},
        Exp = erlang:system_time(millisecond) - 1,
        Rev = {200, node()},
        ets:insert(?META_TABLE, [
            {sync_origin_seq, 0},
            {sync_counter, 200},
            {node_boot, {node(), <<1:128>>}}
        ]),
        ok = imboy_cache_sync_apply:store_revision(K, Rev, Exp, live, 0),
        ok = imboy_cache_sync_apply:index_replicate(K),
        Round = crypto:strong_rand_bytes(12),
        ok = imboy_cache_sync_snapshot:send_snapshot(self(), Round),
        Floor =
            receive
                {cache_snapshot_page, _, Round, 1, [E], _, false} -> E
            after 1000 -> error(missing_expired_floor)
            end,
        ?assertEqual({expired, K, undefined, Exp, Rev}, Floor),
        receive
            {cache_invalidation_watermark, _, _, _, _, _} -> ok
        after 1000 -> error(no_watermark)
        end,
        receive
            {cache_snapshot_cut, _, Round, _, _, _, _, _, _, _} -> ok
        after 1000 -> error(no_cut)
        end,
        receive
            {cache_snapshot_done, _, Round, 200, 1} -> ok
        after 1000 -> error(no_done)
        end,
        %% 冷接收端没有该 Key 的历史，不能靠发送端本地版本碰巧拒绝旧页。
        ok = imboy_cache_sync_apply:delete_metadata({rev, K}),
        ok = imboy_cache_sync_apply:delete_metadata({rindex, K}),
        Source = 'floor_source@synthetic',
        {ok, _} = page(Source, Floor, requested_round(Source, 0)),
        Old = 'older_floor_source@synthetic',
        {ok, _} = page(Old, {K, stale, expiry(), {100, Old}}, requested_round(Old, 0)),
        ?assertEqual([], ets:lookup(Cache, K)),
        ?assertEqual(Rev, imboy_cache_sync:current_rev(K)),
        ?assertMatch([{_, {Rev, _, Exp, live, 0}}], ets:lookup(?META_TABLE, {rev, K})),
        {ok, _} = page(Old, {K, fresh, expiry(), {201, Old}}, requested_round(Old, 0)),
        ?assertEqual([{K, fresh}], ets:lookup(Cache, K))
    end).

lower_epoch_page_cannot_resurrect_higher_epoch_tombstone_test() ->
    with_epoch(fun(_) ->
        K = {entry_epoch_test, deleted},
        Exp = expiry(),
        imboy_cache_sync_apply:store_revision(K, {10, node()}, Exp, deleted, 2),
        Peer = 'lower_epoch@synthetic',
        R = requested_round(Peer, 1),
        {ok, _} = page(Peer, {K, old, Exp, {1000, Peer}}, R),
        ?assertMatch([{_, {{10, _}, 10, Exp, deleted, 2}}], ets:lookup(?META_TABLE, {rev, K})),
        ?assertEqual(0, meck:num_calls(depcache, set, 5))
    end).

older_epoch_missing_key_is_rejected_test() ->
    with_epoch(fun(_) ->
        ets:insert(?META_TABLE, {data_epoch_ceiling, 2}),
        K = {entry_epoch_test, absent},
        ?assertEqual(older, imboy_cache_sync_apply:compare_entry(K, {1000, node()}, 1)),
        Peer = 'lower_epoch@synthetic',
        {ok, _} = page(Peer, {K, old, expiry(), {1000, Peer}}, requested_round(Peer, 1)),
        ?assertEqual([], ets:lookup(?META_TABLE, {rev, K})),
        ?assertEqual(0, meck:num_calls(depcache, set, 5))
    end).

local_low_epoch_writer_is_rejected_before_clock_allocation_test() ->
    with_epoch(fun(_) ->
        K = {entry_epoch_test, writer},
        imboy_cache_sync_apply:store_revision(K, {1, node()}, expiry(), live, 2),
        State = #state{boot_id = <<1:128>>, status = ready, counter = 100},
        ?assertEqual(
            {error, stale_epoch}, imboy_cache_sync_apply:pre_publish(set, K, fresh, 60, State)
        ),
        ?assertEqual([{sync_counter, 100}], ets:lookup(?META_TABLE, sync_counter)),
        ?assertEqual(0, meck:num_calls(depcache, set, 5))
    end).

future_epoch_event_closes_ready_before_applying_data_test() ->
    with_epoch(fun(Cache) ->
        K = {entry_epoch_test, event},
        Exp = expiry(),
        Peer = 'future_data@synthetic',
        Boot = <<2:128>>,
        ets:insert(?META_TABLE, {{peer_incarnation, Peer}, Boot}),
        imboy_cache_sync_apply:store_revision(K, {1000, node()}, Exp, deleted, 0),
        E = (imboy_cache_sync_proto:event(set, K, fresh, Exp, {10, Peer}, 1, Boot))#{
            origin_node => Peer, epoch => 2
        },
        {ok, S} = imboy_cache_sync_apply:apply_v2(E, #state{
            boot_id = <<1:128>>, status = ready, counter = 1000
        }),
        try
            ?assertEqual(discovering, S#state.status),
            ?assertEqual(undefined, imboy_cache:get(K)),
            ?assertEqual([{K, fresh}], ets:lookup(Cache, K)),
            ?assertMatch(
                [{_, {{10, Peer}, {Boot, 1}, Exp, live, 2}}], ets:lookup(?META_TABLE, {rev, K})
            ),
            ?assertEqual(2, imboy_cache_sync_apply:required_epoch())
        after
            cancel(S#state.recovery_timer)
        end
    end).

cold_cleanup_preserves_confirmed_future_epoch_data_test() ->
    with_epoch(fun(Cache) ->
        Old = {entry_epoch_test, old},
        Future = {entry_epoch_test, future},
        Exp = expiry(),
        imboy_cache_sync_apply:store_revision(Old, {1, node()}, Exp, live, 0),
        imboy_cache_sync_apply:index_replicate(Old),
        ets:insert(Cache, {Old, old}),
        imboy_cache_sync_apply:store_revision(Future, {2, node()}, Exp, live, 2),
        imboy_cache_sync_apply:index_replicate(Future),
        ets:insert(Cache, {Future, fresh}),
        ets:insert(?META_TABLE, {{seq, {'future@synthetic', <<2:128>>}}, 2}),
        ok = imboy_cache_sync_apply:controlled_cold_recovery(),
        ?assertEqual(2, imboy_cache_sync_proto:epoch()),
        ?assertEqual([], ets:lookup(?META_TABLE, {rev, Old})),
        ?assertEqual([], ets:lookup(Cache, Old)),
        ?assertMatch([{_, {_, _, _, live, 2}}], ets:lookup(?META_TABLE, {rev, Future})),
        ?assertEqual([{Future, fresh}], ets:lookup(Cache, Future)),
        ?assertEqual([{sync_counter, 100}], ets:lookup(?META_TABLE, sync_counter)),
        ?assertEqual(
            [{{seq, {'future@synthetic', <<2:128>>}}, 2}],
            ets:lookup(?META_TABLE, {seq, {'future@synthetic', <<2:128>>}})
        )
    end).

recovery_cannot_finish_below_accepted_data_epoch_test() ->
    with_epoch(fun(_) ->
        K = {entry_epoch_test, ceiling},
        imboy_cache_sync_apply:store_revision(K, {1, node()}, expiry(), deleted, 2),
        Peer = 'insufficient_epoch@synthetic',
        R0 = requested_round(Peer, 1),
        {ok, R1} = imboy_cache_source_cut_tests:done(Peer, maps:get(round, R0), 0, 0, R0),
        S0 = #state{status = recovering, snapshot_round = R1},
        ?assertEqual(S0, imboy_cache_sync_recover:maybe_complete_round(S0))
    end).

metadata_projection_accounts_for_entry_epoch_bytes_test() ->
    with_epoch(fun(_) ->
        K = {entry_epoch_test, bytes},
        Rev = {1, node()},
        Small = imboy_cache_sync_apply:prospective_rows(delete, K, Rev, expiry(), 0),
        Large = imboy_cache_sync_apply:prospective_rows(delete, K, Rev, expiry(), 1 bsl 512),
        Limit = erlang:external_size(hd(Small)),
        application:set_env(imboy, cache_replication, #{
            whitelist => [entry_epoch_test], n_max_bytes => Limit
        }),
        ?assert(imboy_cache_sync_apply:metadata_fits(Small)),
        ?assertNot(imboy_cache_sync_apply:metadata_fits(Large))
    end).

journal_item_limit_discards_coverage_without_undoing_writes_test() ->
    with_epoch(fun(Cache) ->
        application:set_env(imboy, cache_replication, #{whitelist => [entry_epoch_test], j_max => 2}),
        Round = crypto:strong_rand_bytes(12),
        ets:insert(?META_TABLE, {{snapshot_sender, self()}, #{round => Round}}),
        ok = imboy_cache_sync_apply:journal_refresh(),
        [
            begin
                K = {entry_epoch_test, {journal, N}},
                ets:insert(Cache, {K, N}),
                ok = imboy_cache_sync_apply:store_revision(K, {N, node()}, expiry(), live, 0)
            end
         || N <- [1, 2, 3]
        ],
        ?assertEqual({error, journal_window_exceeded}, imboy_cache_sync_apply:journal_after(0)),
        {ok, Entries} = imboy_cache_sync_apply:journal_after(1),
        ?assertEqual([2, 3], [V || {_, 0, {_, V, _, _}, unknown} <- Entries]),
        ?assertEqual([], [Row || Row = {{journal, _}, _} <- imboy_cache_sync:meta_snapshot()]),
        ?assertMatch([{journal_window, {1, 3, 2, _}}], ets:lookup(?META_TABLE, journal_window)),
        ?assertEqual(
            [{{entry_epoch_test, {journal, 3}}, 3}],
            ets:lookup(Cache, {entry_epoch_test, {journal, 3}})
        ),
        ets:delete(?META_TABLE, {snapshot_sender, self()}),
        ok = imboy_cache_sync_apply:journal_refresh(),
        ?assertEqual(3, imboy_cache_sync_apply:journal_position()),
        ?assertEqual({ok, []}, imboy_cache_sync_apply:journal_after(3)),
        ?assertEqual([], ets:match_object(?META_TABLE, {{journal, '_'}, '_'}))
    end).

journal_byte_limit_does_not_report_successful_write_as_failed_test() ->
    with_epoch(fun(Cache) ->
        application:set_env(imboy, cache_replication, #{
            whitelist => [entry_epoch_test], j_max_bytes => 1
        }),
        ets:insert(?META_TABLE, {{snapshot_sender, self()}, #{round => <<1:96>>}}),
        ok = imboy_cache_sync_apply:journal_refresh(),
        K = {entry_epoch_test, journal_bytes},
        ets:insert(Cache, {K, accepted}),
        ?assertEqual(ok, imboy_cache_sync_apply:store_revision(K, {1, node()}, expiry(), live, 0)),
        ?assertEqual({ok, accepted}, imboy_cache:get(K)),
        ?assertEqual([{journal_window, {1, 1, 0, 0}}], ets:lookup(?META_TABLE, journal_window)),
        ?assertNot(imboy_cache_sync_apply:journal_covers(0))
    end).

missing_live_journal_value_aborts_session_without_inventing_delete_test() ->
    with_epoch(fun(_) ->
        Round = <<2:96>>,
        ets:insert(?META_TABLE, {{snapshot_sender, self()}, #{round => Round}}),
        ok = imboy_cache_sync_apply:journal_refresh(),
        K = {entry_epoch_test, journal_evicted},
        ?assertEqual(ok, imboy_cache_sync_apply:store_revision(K, {1, node()}, expiry(), live, 0)),
        receive
            {cache_snapshot_abort, _, Round, journal_entry_unavailable} -> ok
        after 0 -> ?assert(false)
        end,
        ?assertMatch([{_, {_, _, _, live, 0}}], ets:lookup(?META_TABLE, {rev, K})),
        ?assertEqual([], ets:match_object(?META_TABLE, {{snapshot_sender, '_'}, '_'}))
    end).

snapshot_replays_update_delete_and_new_key_before_done_test() ->
    with_epoch(fun(Cache) ->
        application:set_env(imboy, cache_replication, #{whitelist => [entry_epoch_test], p_max => 1}),
        ets:insert(?META_TABLE, {sync_origin_seq, 0}),
        Exp = expiry(),
        [
            begin
                K = {entry_epoch_test, N},
                ets:insert(Cache, {K, old}),
                imboy_cache_sync_apply:store_revision(K, {N, node()}, Exp, live, 0)
            end
         || N <- [1, 2, 3]
        ],
        Round = crypto:strong_rand_bytes(12),
        ok = imboy_cache_sync_snapshot:send_snapshot(self(), Round),
        First =
            receive
                {cache_snapshot_page, _, Round, 1, [E], _, true} -> E
            after 1000 -> error(no_page)
            end,
        Updated = element(1, First),
        Deleted = hd([{entry_epoch_test, N} || N <- [1, 2, 3], {entry_epoch_test, N} =/= Updated]),
        New = {entry_epoch_test, new},
        ets:insert(Cache, [{Updated, fresh}, {New, added}]),
        ok = imboy_cache_sync_apply:store_revision(Updated, {101, node()}, Exp, live, 0),
        ets:delete(Cache, Deleted),
        ok = imboy_cache_sync_apply:store_revision(Deleted, {102, node()}, Exp, deleted, 0),
        ok = imboy_cache_sync_apply:store_revision(New, {103, node()}, Exp, live, 0),
        Pages = collect_snapshot_tail(Round, 2, []),
        ?assert(lists:member({Updated, fresh, Exp, {101, node()}}, Pages)),
        ?assert(lists:member({deleted, Deleted, undefined, Exp, {102, node()}}, Pages)),
        ?assert(lists:member({New, added, Exp, {103, node()}}, Pages)),
        ?assertEqual([], ets:lookup(?META_TABLE, {snapshot_sender, self()})),
        ?assertEqual([], ets:match_object(?META_TABLE, {{journal, '_'}, '_'}))
    end).

collect_snapshot_tail(Round, N, Acc) ->
    ok = imboy_cache_sync_snapshot:continue_snapshot(self(), Round, N),
    receive
        {cache_snapshot_page, _, Round, N, Entries, _, true} ->
            collect_snapshot_tail(Round, N + 1, Acc ++ Entries);
        {cache_snapshot_page, _, Round, N, Entries, _, false} ->
            receive
                {cache_invalidation_watermark, _, _, _, _, _} -> ok
            after 1000 -> error(no_watermark)
            end,
            receive
                {cache_snapshot_cut, _, Round, _, _, _, _, _, _, _} -> ok
            after 1000 -> error(no_source_cut)
            end,
            receive
                {cache_snapshot_done, _, Round, _, N} -> Acc ++ Entries
            after 1000 -> error(no_done)
            end;
        {cache_snapshot_abort, _, Round, Reason} ->
            error({unexpected_abort, Reason})
    after 1000 -> error(no_tail)
    end.

prepared_journal_expiring_before_pull_cannot_send_done_test_() ->
    {timeout, 10, fun() ->
        with_epoch(fun(Cache) ->
            Cfg = #{whitelist => [entry_epoch_test], p_max => 1},
            application:set_env(imboy, cache_replication, Cfg),
            [
                begin
                    K = {entry_epoch_test, {expiry_baseline, N}},
                    ets:insert(Cache, {K, baseline}),
                    imboy_cache_sync_apply:store_revision(K, {N, node()}, expiry(), live, 0)
                end
             || N <- [1, 2]
            ],
            Round = crypto:strong_rand_bytes(12),
            ok = imboy_cache_sync_snapshot:send_snapshot(self(), Round),
            receive
                {cache_snapshot_page, _, Round, 1, _, _, true} -> ok
            after 1000 -> error(no_page)
            end,
            K = {entry_epoch_test, expiring_journal},
            ets:insert(Cache, {K, temporary}),
            Exp = erlang:system_time(millisecond) + 6500,
            ?assert(imboy_cache_sync:remaining_ttl_sec(Exp) > 0),
            ok = imboy_cache_sync_apply:store_revision(K, {101, node()}, Exp, live, 0),
            ok = imboy_cache_sync_snapshot:continue_snapshot(self(), Round, 2),
            receive
                {cache_snapshot_page, _, Round, 2, _, _, true} -> ok
            after 1000 -> error(no_second_page)
            end,
            timer:sleep(erlang:max(1, Exp - erlang:system_time(millisecond) + 20)),
            ok = imboy_cache_sync_snapshot:continue_snapshot(self(), Round, 3),
            receive
                {cache_snapshot_abort, _, Round, prepared_page_expired} -> ok
            after 1000 -> error(no_expiry_abort)
            end,
            receive
                {cache_snapshot_done, _, Round, _, _} -> error(unexpected_done)
            after 0 -> ok
            end,
            receive
                {cache_snapshot_cut, _, Round, _, _, _, _, _, _, _} -> error(unexpected_cut)
            after 0 -> ok
            end,
            ?assertEqual([], ets:lookup(?META_TABLE, {snapshot_sender, self()}))
        end)
    end}.

next_page_construction_cannot_send_expired_prepared_page_test_() ->
    {timeout, 10, fun() ->
        with_epoch(fun(Cache) ->
            Cfg = #{whitelist => [entry_epoch_test], p_max => 1},
            application:set_env(imboy, cache_replication, Cfg),
            Exp = erlang:system_time(millisecond) + 6500,
            [
                begin
                    K = {entry_epoch_test, {construction_expiry, N}},
                    ets:insert(Cache, {K, baseline}),
                    imboy_cache_sync_apply:store_revision(K, {N, node()}, Exp, live, 0)
                end
             || N <- [1, 2, 3]
            ],
            Round = crypto:strong_rand_bytes(12),
            ok = imboy_cache_sync_snapshot:send_snapshot(self(), Round),
            receive
                {cache_snapshot_page, _, Round, 1, _, _, true} -> ok
            after 1000 -> error(no_page)
            end,
            %% 当前页尚有效，下一页读取受控延迟至它过期；不修改发送游标或 TTL。
            meck:expect(depcache, get, 2, fun(K, _) ->
                timer:sleep(erlang:max(1, Exp - erlang:system_time(millisecond) + 20)),
                [{K, V}] = ets:lookup(Cache, K),
                {ok, V}
            end),
            ?assert(imboy_cache_sync:remaining_ttl_sec(Exp) > 0),
            ok = imboy_cache_sync_snapshot:continue_snapshot(self(), Round, 2),
            receive
                {cache_snapshot_abort, _, Round, prepared_page_expired} -> ok
            after 1000 -> error(no_expiry_abort)
            end,
            receive
                {cache_snapshot_page, _, Round, 2, _, _, _} -> error(expired_page_sent)
            after 0 -> ok
            end,
            receive
                {cache_snapshot_done, _, Round, _, _} -> error(unexpected_done)
            after 0 -> ok
            end,
            receive
                {cache_snapshot_cut, _, Round, _, _, _, _, _, _, _} -> error(unexpected_cut)
            after 0 -> ok
            end
        end)
    end}.

snapshot_cursor_aborts_when_journal_loses_its_start_test() ->
    with_epoch(fun(Cache) ->
        application:set_env(imboy, cache_replication, #{
            whitelist => [entry_epoch_test], j_max => 1, p_max => 1
        }),
        [
            begin
                K = {entry_epoch_test, {baseline, N}},
                ets:insert(Cache, {K, N}),
                imboy_cache_sync_apply:store_revision(K, {N, node()}, expiry(), live, 0)
            end
         || N <- [1, 2, 3]
        ],
        Round = crypto:strong_rand_bytes(12),
        ok = imboy_cache_sync_snapshot:send_snapshot(self(), Round),
        receive
            {cache_snapshot_page, _, Round, 1, _, _, true} -> ok
        after 0 -> ?assert(false)
        end,
        [
            begin
                K = {entry_epoch_test, {delta, N}},
                ets:insert(Cache, {K, N}),
                imboy_cache_sync_apply:store_revision(K, {N + 3, node()}, expiry(), live, 0)
            end
         || N <- [1, 2]
        ],
        ok = imboy_cache_sync_snapshot:continue_snapshot(self(), Round, 2),
        receive
            {cache_snapshot_abort, _, Round, journal_window_exceeded} -> ok
        after 0 -> ?assert(false)
        end,
        receive
            {cache_snapshot_done, _, Round, _, _} -> ?assert(false)
        after 0 -> ok
        end,
        ?assertEqual([], ets:lookup(?META_TABLE, {snapshot_sender, self()})),
        ?assertEqual([], ets:match_object(?META_TABLE, {{journal, '_'}, '_'}))
    end).

journal_capture_exception_does_not_undo_successful_metadata_write_test() ->
    with_epoch(fun(Cache) ->
        Round = <<3:96>>,
        ets:insert(?META_TABLE, {{snapshot_sender, self()}, #{round => Round}}),
        ok = imboy_cache_sync_apply:journal_refresh(),
        K = {entry_epoch_test, journal_read_failure},
        ets:insert(Cache, {K, accepted}),
        meck:expect(depcache, get, 2, fun(_, _) -> erlang:error(capture_read_failed) end),
        ?assertEqual(ok, imboy_cache_sync_apply:store_revision(K, {1, node()}, expiry(), live, 0)),
        ?assertEqual([{K, accepted}], ets:lookup(Cache, K)),
        ?assertMatch([{_, {_, _, _, live, 0}}], ets:lookup(?META_TABLE, {rev, K})),
        receive
            {cache_snapshot_abort, _, Round, journal_entry_unavailable} -> ok
        after 0 -> ?assert(false)
        end
    end).

repeated_cold_quarantine_does_not_extend_recovery_deadline_test() ->
    with_epoch(fun(_) ->
        Deadline = erlang:system_time(millisecond) + 10000,
        S1 = imboy_cache_sync_recover:start_cold_recovery(
            #state{status = recovering, recover_deadline_ms = Deadline}, 2
        ),
        S2 = imboy_cache_sync_recover:start_cold_recovery(S1, 3),
        try
            ?assertEqual(Deadline, S1#state.recover_deadline_ms),
            ?assertEqual(Deadline, S2#state.recover_deadline_ms),
            ?assert(S2#state.requires_authority),
            ?assertEqual(3, imboy_cache_sync_proto:epoch())
        after
            cancel(S1#state.recovery_timer),
            cancel(S2#state.recovery_timer)
        end
    end).

replica_read_discards_value_if_version_boot_or_readiness_changes_test() ->
    lists:foreach(
        fun(Change) ->
            with_epoch(fun(Cache) ->
                K = {entry_epoch_test, read_race},
                Exp = expiry(),
                imboy_cache_sync_apply:store_revision(K, {1, node()}, Exp, live, 0),
                ets:insert(Cache, {K, old}),
                ets:insert(?META_TABLE, {node_boot, {node(), <<1:128>>}}),
                meck:expect(depcache, get, 2, fun(_, _) ->
                    case Change of
                        version ->
                            imboy_cache_sync_apply:store_revision(K, {2, node()}, Exp, live, 0);
                        boot ->
                            ets:insert(?META_TABLE, {node_boot, {node(), <<2:128>>}});
                        readiness ->
                            ets:insert(?META_TABLE, {status, recovering})
                    end,
                    {ok, old}
                end),
                ?assertEqual(undefined, imboy_cache:get(K))
            end)
        end,
        [version, boot, readiness]
    ).

implicit_memo_keys_cannot_bypass_versioned_writer_boundary_test() ->
    with_epoch(fun(_) ->
        K = {entry_epoch_test, implicit},
        erlang:put(implicit_key_calls, 0),
        F = fun() ->
            erlang:put(implicit_key_calls, erlang:get(implicit_key_calls) + 1),
            K
        end,
        try
            ?assertError({unsupported_replicate_api, memo}, imboy_cache:memo(F)),
            ?assertEqual(1, erlang:get(implicit_key_calls)),
            ?assertError(
                {unsupported_replicate_api, memo},
                imboy_cache:memo({entry_epoch_test, missing, []}, 60)
            ),
            ?assertEqual(0, meck:num_calls(depcache, set, 5))
        after
            erlang:erase(implicit_key_calls)
        end
    end).

all_reports_select_highest_cohort_and_preserve_confirmed_data_test() ->
    with_epoch(fun(Cache) ->
        Low = 'cohort_low@synthetic',
        High = 'cohort_high@synthetic',
        OldKey = {entry_epoch_test, old_cohort},
        NewKey = {entry_epoch_test, confirmed},
        ets:insert(Cache, [{OldKey, old}, {NewKey, fresh}]),
        imboy_cache_sync_apply:store_revision(OldKey, {1000, node()}, expiry(), live, 0),
        imboy_cache_sync_apply:store_revision(NewKey, {10, High}, expiry(), live, 2),
        ets:insert(?META_TABLE, {{seq, {High, <<2:128>>}}, 7}),
        S0 = discovery_state([Low, High]),
        R = maps:get(round, S0#state.snapshot_round),
        S1 = report(Low, 0, true, S0),
        assert_no_request(R),
        ?assertEqual(0, imboy_cache_sync_proto:epoch()),
        S2 = report(High, 2, true, S1),
        Peers = maps:get(peers, S2#state.snapshot_round),
        ?assertNot(maps:get(authority, maps:get(Low, Peers))),
        ?assert(maps:get(requested, maps:get(High, Peers))),
        ?assertEqual(2, imboy_cache_sync_proto:epoch()),
        ?assertEqual([], ets:lookup(?META_TABLE, {rev, OldKey})),
        ?assertEqual([{NewKey, fresh}], ets:lookup(Cache, NewKey)),
        ?assertMatch([{_, {_, _, _, live, 2}}], ets:lookup(?META_TABLE, {rev, NewKey})),
        ?assertEqual(
            [{{seq, {High, <<2:128>>}}, 7}], ets:lookup(?META_TABLE, {seq, {High, <<2:128>>}})
        ),
        receive
            {cache_snapshot_request, _, R} -> ok
        after 0 -> ?assert(false)
        end,
        S3 = report(Low, 0, true, S2),
        ?assertNot(maps:get(authority, maps:get(Low, maps:get(peers, S3#state.snapshot_round)))),
        assert_no_request(R)
    end).

cohort_below_confirmed_floor_cannot_start_or_complete_test() ->
    with_epoch(fun(_) ->
        Peer = 'cohort_below_floor@synthetic',
        ets:insert(?META_TABLE, {data_epoch_ceiling, 2}),
        S = report(Peer, 1, true, discovery_state([Peer])),
        R = S#state.snapshot_round,
        ?assertNot(maps:is_key(cohort_epoch, R)),
        ?assertEqual(recovering, S#state.status),
        ?assertEqual(0, imboy_cache_sync_proto:epoch()),
        assert_no_request(maps:get(round, R))
    end).

higher_epoch_report_after_selection_aborts_frozen_cohort_test() ->
    with_epoch(fun(_) ->
        Low = 'cohort_late_low@synthetic',
        High = 'cohort_late_high@synthetic',
        S1 = report(Low, 0, true, discovery_state([Low, High])),
        S2 = report(High, 1, true, S1),
        Round = maps:get(round, S2#state.snapshot_round),
        receive
            {cache_snapshot_request, _, Round} -> ok
        after 0 -> ?assert(false)
        end,
        S3 = report(Low, 2, true, S2),
        try
            ?assertNotEqual(Round, maps:get(round, S3#state.snapshot_round)),
            ?assertEqual(recovering, S3#state.status),
            ?assertEqual(S2#state.recover_deadline_ms, S3#state.recover_deadline_ms)
        after
            cancel(S3#state.recovery_timer)
        end
    end).

higher_epoch_data_from_excluded_source_aborts_cohort_without_losing_data_test() ->
    with_epoch(fun(Cache) ->
        Low = 'cohort_event_low@synthetic',
        High = 'cohort_event_high@synthetic',
        S1 = report(Low, 0, true, discovery_state([Low, High])),
        S2 = report(High, 1, true, S1),
        Round = maps:get(round, S2#state.snapshot_round),
        receive
            {cache_snapshot_request, _, Round} -> ok
        after 0 -> ?assert(false)
        end,
        K = {entry_epoch_test, future_delta},
        E = (imboy_cache_sync_proto:event(set, K, future, expiry(), {200, Low}, 1, <<2:128>>))#{
            origin_node => Low, epoch => 2
        },
        {ok, S3} = imboy_cache_sync_apply:apply_v2(E, S2),
        try
            ?assertNotEqual(Round, maps:get(round, S3#state.snapshot_round)),
            ?assertEqual(S2#state.recover_deadline_ms, S3#state.recover_deadline_ms),
            ?assertEqual(2, imboy_cache_sync_apply:required_epoch()),
            ?assertEqual([{K, future}], ets:lookup(Cache, K)),
            ?assertMatch([{_, {{200, Low}, _, _, live, 2}}], ets:lookup(?META_TABLE, {rev, K})),
            ?assertEqual(undefined, imboy_cache:get(K))
        after
            cancel(S3#state.recovery_timer)
        end
    end).

same_epoch_confirmed_delta_survives_older_snapshot_and_becomes_readable_test() ->
    with_epoch(fun(Cache) ->
        Peer = 'cohort_delta@synthetic',
        K = {entry_epoch_test, delta},
        Exp = expiry(),
        imboy_cache_sync_apply:store_revision(K, {1000, Peer}, Exp, live, 1),
        ets:insert(Cache, {K, confirmed}),
        S1 = report(Peer, 1, true, discovery_state([Peer])),
        R = S1#state.snapshot_round,
        Round = maps:get(round, R),
        receive
            {cache_snapshot_request, _, Round} -> ok
        after 0 -> ?assert(false)
        end,
        {ok, R1} = page(Peer, {K, stale, Exp, {10, Peer}}, R),
        {ok, R2} = imboy_cache_source_cut_tests:done(Peer, Round, 1000, 1, R1),
        S2 = imboy_cache_sync_recover:maybe_complete_round(S1#state{snapshot_round = R2}),
        ?assertEqual(ready, S2#state.status),
        ?assertEqual({ok, confirmed}, imboy_cache:get(K)),
        ?assertMatch([{_, {{1000, Peer}, _, _, live, 1}}], ets:lookup(?META_TABLE, {rev, K}))
    end).

unreported_frozen_peer_prevents_request_and_expires_degraded_test() ->
    with_epoch(fun(_) ->
        A = 'cohort_responded@synthetic',
        B = 'cohort_silent@synthetic',
        S1 = report(A, 1, true, discovery_state([A, B])),
        R = S1#state.snapshot_round,
        assert_no_request(maps:get(round, R)),
        {noreply, S2} = imboy_cache_sync_recover:handle_timeout(S1#state{
            snapshot_round = R#{deadline_ms := 0}, recover_deadline_ms = 0
        }),
        ?assertEqual(degraded, S2#state.status),
        ?assertEqual(0, imboy_cache_sync_proto:epoch())
    end).

cold_recovery_cannot_adopt_different_epoch_source_test() ->
    with_epoch(fun(_) ->
        Peer = 'cohort_cold@synthetic',
        ets:insert(?META_TABLE, {sync_epoch, 1}),
        S0 = (discovery_state([Peer]))#state{requires_authority = true},
        S1 = report(Peer, 2, true, S0),
        R = S1#state.snapshot_round,
        ?assertEqual(recovering, S1#state.status),
        ?assertEqual(1, imboy_cache_sync_proto:epoch()),
        ?assertNot(maps:get(authority, maps:get(Peer, maps:get(peers, R)))),
        assert_no_request(maps:get(round, R))
    end).

discovery_state(Peers) ->
    ets:insert(?META_TABLE, {status, recovering}),
    Deadline = erlang:system_time(millisecond) + 60000,
    R = imboy_cache_sync_snapshot:new_round(0, Deadline),
    Round = lists:foldl(fun(P, Acc) -> imboy_cache_sync_snapshot:expect_peer(Acc, P) end, R, Peers),
    #state{
        status = recovering, counter = 100, snapshot_round = Round, recover_deadline_ms = Deadline
    }.

report(Peer, Epoch, Ready, State) ->
    imboy_cache_sync_recover:handle_version_report(
        Peer,
        100,
        Epoch,
        imboy_cache_sync_proto:config_hash(),
        imboy_cache_sync_proto:registry_hash(),
        <<2:128>>,
        Ready,
        self(),
        State
    ).

assert_no_request(Round) ->
    receive
        {cache_snapshot_request, _, Round} -> ?assert(false)
    after 0 -> ok
    end.

requested_round(Peer, Epoch) ->
    ets:insert(?META_TABLE, [{{peer_epoch, Peer}, Epoch}, {{peer_incarnation, Peer}, <<2:128>>}]),
    R = imboy_cache_sync_snapshot:new_round(0, erlang:system_time(millisecond) + 60000),
    imboy_cache_sync_snapshot:mark_requested(
        imboy_cache_sync_snapshot:authorize_peer(
            imboy_cache_sync_snapshot:expect_peer(R, Peer), Peer
        ),
        Peer
    ).
page(Peer, Entry, R) ->
    Round = maps:get(round, R),
    imboy_cache_sync_snapshot:handle_page(
        Peer,
        Round,
        1,
        [Entry],
        imboy_cache_sync_snapshot:page_digest(Round, 1, [Entry]),
        false,
        R
    ).
expiry() -> erlang:system_time(millisecond) + 60000.
cancel(undefined) ->
    ok;
cancel(Timer) ->
    erlang:cancel_timer(Timer),
    receive
        {timeout, Timer, cache_recovery} -> ok
    after 0 -> ok
    end.

with_epoch(Fun) ->
    application:load(imboy),
    application:ensure_all_started(crypto),
    Keys = [cache_registry_overrides, cache_replication, cache_fenced_namespaces],
    Original = [{K, application:get_env(imboy, K)} || K <- Keys],
    application:set_env(imboy, cache_registry_overrides, #{entry_epoch_test => {replicate, false}}),
    application:set_env(imboy, cache_replication, #{whitelist => [entry_epoch_test]}),
    application:set_env(imboy, cache_fenced_namespaces, []),
    register(imboy_cache_sync, self()),
    ets:new(?META_TABLE, [named_table, public, set]),
    ets:insert(?META_TABLE, [
        {status, ready},
        {sync_epoch, 0},
        {sync_counter, 100},
        {node_boot, {node(), <<1:128>>}}
    ]),
    Cache = ets:new(epoch_cache, [public, set]),
    meck:new(depcache, [passthrough, no_link]),
    meck:expect(depcache, get, 2, fun(K, _) ->
        case ets:lookup(Cache, K) of
            [{_, V}] -> {ok, V};
            [] -> undefined
        end
    end),
    meck:expect(depcache, set, 5, fun(K, V, _, _, _) ->
        ets:insert(Cache, {K, V}),
        ok
    end),
    meck:expect(depcache, flush, 2, fun(K, _) ->
        ets:delete(Cache, K),
        ok
    end),
    try
        Fun(Cache)
    after
        imboy_cache_source_cut_tests:drain(),
        meck:unload(depcache),
        unregister(imboy_cache_sync),
        ets:delete(Cache),
        ets:delete(?META_TABLE),
        imboy_cache_source_cut_tests:restore_envs(Original)
    end.
