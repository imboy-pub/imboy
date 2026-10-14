%%% 墓碑快照：真实元数据记账；独立 fixture 的 depcache 调用观察。
-module(imboy_cache_snapshot_tombstone_tests).
-include_lib("eunit/include/eunit.hrl").
-define(META_TABLE, imboy_cache_meta).

evicted_live_snapshot_transfers_version_floor_without_value_test() ->
    with_snapshot(fun() ->
        K = {snapshot_delete_test, evicted_live},
        Exp = erlang:system_time(millisecond) + 60000,
        Rev = {20, node()},
        meck:expect(depcache, get, 2, fun(_, _) -> undefined end),
        ok = imboy_cache_sync_apply:store_revision(K, Rev, Exp, live),
        Round = crypto:strong_rand_bytes(12),
        ok = imboy_cache_sync_snapshot:send_snapshot(self(), Round),
        Entry = {unavailable, K, undefined, Exp, Rev},
        receive
            {cache_snapshot_page, _, Round, 1, [Entry], _, false} -> ok
        after 1000 -> error(missing_evicted_floor)
        end,
        receive
            {cache_snapshot_done, _, Round, _, 1} -> ok
        after 1000 -> error(missing_done)
        end,
        ok = imboy_cache_sync_apply:delete_metadata({rev, K}),
        ok = imboy_cache_sync_apply:delete_metadata({rindex, K}),
        R0 = authorized_round(),
        {ok, R1} = apply_page([Entry], 1, R0),
        ?assertEqual(Rev, imboy_cache_sync:current_rev(K)),
        {ok, _} = apply_page([{K, stale, Exp, {10, node()}}], 2, R1),
        ?assertEqual(0, meck:num_calls(depcache, set, 5)),
        ?assertEqual(1, meck:num_calls(depcache, flush, 2))
    end).

unavailable_floor_preserves_same_version_value_in_both_orders_test() ->
    lists:foreach(
        fun(Order) ->
            with_snapshot(fun() ->
                Cache = ets:new(unavailable_value, [set]),
                try
                    meck:expect(depcache, set, 5, fun(K, V, _, _, _) ->
                        ets:insert(Cache, {K, V}),
                        ok
                    end),
                    meck:expect(depcache, flush, 2, fun(K, _) ->
                        ets:delete(Cache, K),
                        ok
                    end),
                    meck:expect(depcache, get, 2, fun(K, _) ->
                        case ets:lookup(Cache, K) of
                            [{_, V}] -> {ok, V};
                            [] -> undefined
                        end
                    end),
                    K = {snapshot_delete_test, unavailable_order},
                    Exp = erlang:system_time(millisecond) + 60000,
                    Rev = {20, node()},
                    Live = {K, current, Exp, Rev},
                    Floor = {unavailable, K, undefined, Exp, Rev},
                    [First, Second] =
                        case Order of
                            live_first -> [Live, Floor];
                            floor_first -> [Floor, Live]
                        end,
                    {ok, R1} = apply_page([First], 1, authorized_round()),
                    {ok, _} = apply_page([Second], 2, R1),
                    ?assertEqual([{K, current}], ets:lookup(Cache, K)),
                    ?assertEqual(Rev, imboy_cache_sync:current_rev(K))
                after
                    ets:delete(Cache)
                end
            end)
        end,
        [live_first, floor_first]
    ).

unavailable_floor_negative_boundaries_are_atomic_test() ->
    with_snapshot(fun() ->
        K = {snapshot_delete_test, unavailable_negative},
        Now = erlang:system_time(millisecond),
        Rev = {20, node()},
        Exp = Now + 60000,
        Floor = {unavailable, K, undefined, Exp, Rev},
        ?assertNot(
            imboy_cache_sync_proto:valid_snapshot_entry(
                setelement(
                    4,
                    Floor,
                    Now + (imboy_cache_policy:max_ttl_sec() + 6) * 1000
                )
            )
        ),
        ?assertNot(
            imboy_cache_sync_proto:valid_snapshot_entry(
                setelement(
                    4,
                    Floor,
                    Now - imboy_cache_policy:tombstone_ttl_sec() * 1000 - 1
                )
            )
        ),
        R = authorized_round(),
        ?assertMatch(
            {abort, expired_floor_conflict, R},
            apply_page([Floor, {K, bad, Exp + 1, Rev}], 1, R)
        ),
        set_limits(#{n_max_bytes => 1}),
        ?assertMatch({abort, index_full, R}, apply_page([Floor], 1, R)),
        ?assertEqual(0, meck:num_calls(depcache, set, 5)),
        ?assertEqual(0, meck:num_calls(depcache, flush, 2)),
        set_limits(#{}),
        ok = imboy_cache_sync_apply:store_revision(K, Rev, Exp, deleted),
        {ok, _} = apply_page([Floor], 1, R),
        ?assertMatch([{_, {Rev, _, Exp, deleted, _}}], ets:lookup(?META_TABLE, {rev, K}))
    end).

live_page_expiring_in_transit_retains_newer_version_floor_test() ->
    lists:foreach(
        fun(Kind) ->
            with_snapshot(fun() ->
                K = {snapshot_delete_test, transit_expiry},
                R0 = authorized_round(),
                Future = erlang:system_time(millisecond) + 60000,
                {ok, R1} = apply_page([{K, old, Future, {10, node()}}], 1, R0),
                Exp =
                    case Kind of
                        past -> erlang:system_time(millisecond) - 1;
                        skew -> erlang:system_time(millisecond) + 4000
                    end,
                {ok, R2} = apply_page([{K, newest, Exp, {20, node()}}], 2, R1),
                ?assertEqual({20, node()}, imboy_cache_sync:current_rev(K)),
                ?assertMatch([{_, {{20, _}, _, Exp, live, _}}], ets:lookup(?META_TABLE, {rev, K})),
                ?assertEqual(1, meck:num_calls(depcache, set, 5)),
                ?assertEqual(1, meck:num_calls(depcache, flush, 2)),
                {ok, _} = apply_page([{K, stale, Future, {10, node()}}], 3, R2),
                ?assertEqual(1, meck:num_calls(depcache, set, 5))
            end)
        end,
        [past, skew]
    ).

unreplicable_live_outside_retention_window_rejects_whole_page_test() ->
    with_snapshot(fun() ->
        R = authorized_round(),
        Exp = erlang:system_time(millisecond) - imboy_cache_policy:tombstone_ttl_sec() * 1000 - 1,
        Bad = {{snapshot_delete_test, too_old_live}, value, Exp, {20, node()}},
        Good = {
            {snapshot_delete_test, good_live},
            value,
            erlang:system_time(millisecond) + 60000,
            {21, node()}
        },
        ?assertEqual({abort, expired_floor_window_exceeded, R}, apply_page([Good, Bad], 1, R)),
        ?assertEqual(0, meck:num_calls(depcache, set, 5)),
        ?assertEqual(0, meck:num_calls(depcache, flush, 2)),
        ?assertEqual([], ets:match_object(?META_TABLE, {{rev, '_'}, '_'}))
    end).

expired_floor_clock_allowance_has_a_bounded_future_limit_test() ->
    with_snapshot(fun() ->
        K = {snapshot_delete_test, clock_allowance},
        Now = erlang:system_time(millisecond),
        %% 来源在 +5s 偏差下剩余不足一整秒；接收方保留原期限而非改写时钟。
        Allowed = {expired, K, undefined, Now + 10500, {20, node()}},
        ?assert(imboy_cache_sync:remaining_ttl_sec(Now + 10500) > 0),
        ?assert(imboy_cache_sync_proto:valid_snapshot_entry(Allowed)),
        ?assertNot(imboy_cache_sync_proto:valid_snapshot_entry(setelement(4, Allowed, Now + 12000)))
    end).

expired_floor_blocks_same_revision_ttl_extension_atomically_test() ->
    with_snapshot(fun() ->
        K = {snapshot_delete_test, expiry_floor},
        Rev = {10, node()},
        Exp = erlang:system_time(millisecond) - 1,
        R0 = authorized_round(),
        {ok, R1} = apply_page([{expired, K, undefined, Exp, Rev}], 1, R0),
        Rows = ets:tab2list(?META_TABLE),
        Other = {
            {snapshot_delete_test, other},
            valid,
            erlang:system_time(millisecond) + 60000,
            {11, node()}
        },
        Renewed = {K, stale, erlang:system_time(millisecond) + 60000, Rev},
        ?assertEqual({abort, expired_floor_conflict, R1}, apply_page([Other, Renewed], 2, R1)),
        ?assertEqual(lists:sort(Rows), lists:sort(ets:tab2list(?META_TABLE))),
        ?assertEqual(0, meck:num_calls(depcache, set, 5))
    end).

expired_floor_preserves_equal_delete_and_newer_live_test() ->
    with_snapshot(fun() ->
        Deleted = {snapshot_delete_test, deleted_floor},
        Live = {snapshot_delete_test, newer_live},
        Exp = erlang:system_time(millisecond) + 60000,
        Past = erlang:system_time(millisecond) - 1,
        ok = imboy_cache_sync_apply:store_revision(Deleted, {10, node()}, Exp, deleted),
        ok = imboy_cache_sync_apply:store_revision(Live, {20, node()}, Exp, live),
        {ok, _} = apply_page(
            [
                {expired, Deleted, undefined, Past, {10, node()}},
                {expired, Live, undefined, Past, {10, node()}}
            ],
            1,
            authorized_round()
        ),
        ?assertMatch([{_, {_, _, Exp, deleted, _}}], ets:lookup(?META_TABLE, {rev, Deleted})),
        ?assertMatch([{_, {{20, _}, _, Exp, live, _}}], ets:lookup(?META_TABLE, {rev, Live})),
        ?assertEqual(0, meck:num_calls(depcache, flush, 2))
    end).

expired_floor_validation_budget_and_journal_capture_test() ->
    with_snapshot(fun() ->
        K = {snapshot_delete_test, floor_validation},
        Exp = erlang:system_time(millisecond) - 1,
        Floor = {expired, K, undefined, Exp, {10, node()}},
        [
            ?assertNot(imboy_cache_sync_proto:valid_snapshot_entry(setelement(P, Floor, V)))
         || {P, V} <- [
                {2, {unknown_floor, key}},
                {3, value},
                {4, Exp + 15000},
                {4, Exp - imboy_cache_policy:tombstone_ttl_sec() * 1000},
                {5, {-1, node()}}
            ]
        ],
        set_limits(#{n_max_bytes => 1}),
        R = authorized_round(),
        ?assertEqual({abort, index_full, R}, apply_page([Floor], 1, R)),
        ?assertEqual([], ets:lookup(?META_TABLE, {rev, K})),
        ?assertEqual(0, meck:num_calls(depcache, flush, 2)),
        set_limits(#{}),
        Round = <<4:96>>,
        ets:insert(?META_TABLE, {{snapshot_sender, self()}, #{round => Round}}),
        ok = imboy_cache_sync_apply:journal_refresh(),
        {ok, _} = apply_page([Floor], 1, R),
        ?assertMatch({ok, [{_, 0, Floor, unknown}]}, imboy_cache_sync_apply:journal_after(0)),
        ?assert(ets:member(?META_TABLE, {snapshot_sender, self()})),
        receive
            {cache_snapshot_abort, _, Round, _} -> error(floor_capture_aborted)
        after 0 -> ok
        end
    end).

prepared_expired_floor_crossing_retention_window_aborts_test() ->
    with_snapshot(fun() ->
        %% 使用真实 W，把 fixture 放在窗口最后一秒，不修改生产窗口。
        set_limits(#{p_max => 1}),
        W = imboy_cache_policy:tombstone_ttl_sec() * 1000,
        Exp = erlang:system_time(millisecond) - W + 1000,
        [
            imboy_cache_sync_apply:store_revision(
                {snapshot_delete_test, {floor_window, N}},
                {N, node()},
                Exp,
                live
            )
         || N <- [1, 2]
        ],
        Round = crypto:strong_rand_bytes(12),
        ok = imboy_cache_sync_snapshot:send_snapshot(self(), Round),
        receive
            {cache_snapshot_page, _, Round, 1, _, _, true} -> ok
        after 1000 -> error(no_first_floor)
        end,
        timer:sleep(erlang:max(1, Exp + W - erlang:system_time(millisecond) + 20)),
        ok = imboy_cache_sync_snapshot:continue_snapshot(self(), Round, 2),
        receive
            {cache_snapshot_abort, _, Round, prepared_page_expired} -> ok
        after 1000 -> error(no_window_abort)
        end,
        receive
            {cache_snapshot_page, _, Round, 2, _, _, _} -> error(expired_floor_sent)
        after 0 -> ok
        end,
        receive
            {cache_snapshot_cut, _, Round, _, _, _, _, _, _, _} -> error(unexpected_cut)
        after 0 -> ok
        end,
        receive
            {cache_snapshot_cut, _, Round, _, _, _, _, _, _} -> error(unexpected_cut)
        after 0 -> ok
        end,
        receive
            {cache_snapshot_done, _, Round, _, _} -> error(unexpected_done)
        after 0 -> ok
        end
    end).

sender_preserves_delete_and_receiver_rejects_old_live_test() ->
    with_snapshot(fun() ->
        K = {snapshot_delete_test, retained},
        Exp = erlang:system_time(millisecond) + 60000,
        Rev = {2, node()},
        ok = imboy_cache_sync_apply:store_revision(K, Rev, Exp, deleted),
        ok = imboy_cache_sync_apply:store_revision(
            {snapshot_delete_test, expired}, {3, node()}, 1, deleted
        ),
        Round = crypto:strong_rand_bytes(12),
        ok = imboy_cache_sync_snapshot:send_snapshot(self(), Round),
        Entries =
            receive
                {cache_snapshot_page, _, Round, 1, Page, _, false} -> Page
            after 1000 -> error(missing_tombstone_page)
            end,
        ?assertEqual([{deleted, K, undefined, Exp, Rev}], Entries),
        receive
            {cache_snapshot_done, _, Round, _, 1} -> ok
        after 1000 -> error(missing_done)
        end,
        ?assertNot(meck:called(depcache, get, [K, imboy_cache])),
        ok = imboy_cache_sync_apply:store_revision(K, {1, node()}, Exp, live),
        R0 = authorized_round(),
        {ok, R1} = apply_page(Entries, 1, R0),
        ?assert(meck:called(depcache, flush, [K, imboy_cache])),
        ?assertEqual(deleted, imboy_cache_sync:replicate_state(K)),
        ?assertEqual({2, node()}, imboy_cache_sync:current_rev(K)),
        {ok, R2} = apply_page([{K, old, Exp, {1, node()}}], 2, R1),
        {ok, _} = apply_page([{deleted, K, undefined, Exp + 1000, Rev}], 3, R2),
        ?assertNot(meck:called(depcache, set, [K, old, '_', [], imboy_cache])),
        ?assertMatch([{_, {Rev, _, Exp, deleted, _}}], ets:lookup(?META_TABLE, {rev, K}))
    end).

tombstone_budget_rejects_entire_mixed_page_test() ->
    with_snapshot(fun() ->
        set_limits(#{t_max_bytes => 1}),
        Exp = erlang:system_time(millisecond) + 60000,
        Entries = [
            {{snapshot_delete_test, live}, value, Exp, {1, node()}},
            {deleted, {snapshot_delete_test, removed}, undefined, Exp, {2, node()}}
        ],
        R = authorized_round(),
        ?assertEqual({abort, tombstone_budget_exceeded, R}, apply_page(Entries, 1, R)),
        ?assertEqual([], ets:match_object(?META_TABLE, {{rev, '_'}, '_'})),
        ?assertEqual(0, meck:num_calls(depcache, set, 5)),
        ?assertEqual(0, meck:num_calls(depcache, flush, 2))
    end).

stale_live_cannot_borrow_tombstone_capacity_test() ->
    with_snapshot(fun() ->
        set_limits(#{t_max => 1}),
        K = {snapshot_delete_test, retained},
        Exp = erlang:system_time(millisecond) + 60000,
        ok = imboy_cache_sync_apply:store_revision(K, {5, node()}, Exp, deleted),
        Entries = [
            {K, stale, Exp, {1, node()}},
            {deleted, {snapshot_delete_test, added}, undefined, Exp, {6, node()}}
        ],
        R = authorized_round(),
        ?assertEqual({abort, tombstone_budget_exceeded, R}, apply_page(Entries, 1, R)),
        ?assertEqual({5, node()}, imboy_cache_sync:current_rev(K)),
        ?assertEqual(0, meck:num_calls(depcache, set, 5))
    end).

duplicate_kind_cannot_hide_tombstone_peak_test() ->
    with_snapshot(fun() ->
        set_limits(#{t_max_bytes => 1}),
        K = {snapshot_delete_test, duplicate},
        Exp = erlang:system_time(millisecond) + 60000,
        Entries = [
            {deleted, K, undefined, Exp, {1, node()}},
            {K, large_revision, Exp, {1 bsl 2048, node()}}
        ],
        R = authorized_round(),
        ?assertEqual({abort, tombstone_budget_exceeded, R}, apply_page(Entries, 1, R)),
        ?assertEqual([], ets:lookup(?META_TABLE, {rev, K}))
    end).

invalid_tombstones_do_not_apply_test() ->
    with_snapshot(fun() ->
        K = {snapshot_delete_test, invalid},
        Exp = erlang:system_time(millisecond) + 60000,
        Rev = {2, node()},
        Valid = {deleted, K, undefined, Exp, Rev},
        Bad = [
            setelement(3, Valid, value),
            setelement(2, Valid, {unknown, key}),
            setelement(
                4,
                Valid,
                erlang:system_time(millisecond) +
                    (imboy_cache_policy:tombstone_ttl_sec() + 10) * 1000
            )
        ],
        R = authorized_round(),
        lists:foreach(
            fun(E) ->
                ?assertEqual({abort, invalid_snapshot_entry, R}, apply_page([E], 1, R))
            end,
            Bad
        ),
        Round = maps:get(round, R),
        ?assertNotEqual(
            imboy_cache_sync_snapshot:page_digest(Round, 1, [Valid]),
            imboy_cache_sync_snapshot:page_digest(Round, 1, [{K, value, Exp, Rev}])
        ),
        ?assertEqual([], ets:lookup(?META_TABLE, {rev, K}))
    end).

done_watermark_must_cover_live_and_deleted_revisions_test() ->
    with_snapshot(fun() ->
        Exp = erlang:system_time(millisecond) + 60000,
        R0 = authorized_round(),
        {ok, R1} = apply_page([{{snapshot_delete_test, live}, value, Exp, {9, node()}}], 1, R0),
        {ok, R2} = apply_page(
            [{deleted, {snapshot_delete_test, tomb}, undefined, Exp, {10, node()}}], 2, R1
        ),
        Round = maps:get(round, R2),
        ?assertEqual(
            {abort, invalid_version_watermark, R2},
            imboy_cache_sync_snapshot:handle_done(node(), Round, 9, 2, R2)
        ),
        ?assertNot(imboy_cache_sync_snapshot:round_complete(R2)),
        {ok, R3} = imboy_cache_source_cut_tests:done(node(), Round, 10, 2, R2),
        ?assert(imboy_cache_sync_snapshot:round_complete(R3)),
        ?assertEqual({ok, R3}, imboy_cache_sync_snapshot:handle_done(node(), Round, 10, 2, R3))
    end).

sender_page_byte_boundaries_match_actual_external_encoding_test() ->
    with_snapshot(fun() ->
        Exp = erlang:system_time(millisecond) + 60000,
        Values = [
            <<"雪"/utf8>>,
            #{tuple => {1 bsl 200, -7}},
            lists:seq(256, 280),
            {nested, [#{binary => <<0:512>>}]},
            <<0:2048>>
        ],
        Live = [
            {{snapshot_delete_test, I}, V, Exp, {I, node()}}
         || {I, V} <- lists:zip(lists:seq(1, 5), Values)
        ],
        Tomb = {deleted, {snapshot_delete_test, 6}, undefined, Exp, {6, node()}},
        Rows = maps:from_list([{element(2, entry_key(E)), E} || E <- Live ++ [Tomb]]),
        [store_source_entry(E) || E <- Live ++ [Tomb]],
        ets:insert(?META_TABLE, {sync_counter, 6}),
        meck:expect(depcache, get, 2, fun({_, I}, _) ->
            {_, V, _, _} = maps:get(I, Rows),
            {ok, V}
        end),
        Ordered = [
            maps:get(element(2, K), Rows)
         || {{rev, K}, _} <- ets:match_object(?META_TABLE, {{rev, '_'}, '_'})
        ],
        MaxSingle = lists:max([erlang:external_size([E]) || E <- Ordered]),
        MaxPair = lists:max([erlang:external_size([A, B]) || A <- Ordered, B <- Ordered]),
        [assert_sender_pages(Ordered, Limit) || Limit <- [MaxSingle, MaxSingle + 1, MaxPair]]
    end).

entry_key({K, _, _, _}) -> K;
entry_key({deleted, K, _, _, _}) -> K.

store_source_entry({K, _, Exp, Rev}) ->
    imboy_cache_sync_apply:store_revision(K, Rev, Exp, live);
store_source_entry({deleted, K, _, Exp, Rev}) ->
    imboy_cache_sync_apply:store_revision(K, Rev, Exp, deleted).

assert_sender_pages(Ordered, Limit) ->
    set_limits(#{p_max => 3, p_max_bytes => Limit}),
    Round = crypto:strong_rand_bytes(12),
    ok = imboy_cache_sync_snapshot:send_snapshot(self(), Round),
    Pages = collect_source_pages(Round, []),
    ?assertEqual(Ordered, lists:append(Pages)),
    lists:foreach(
        fun(Page) ->
            ?assert(length(Page) =< 3),
            ?assert(erlang:external_size(Page) =< Limit)
        end,
        Pages
    ),
    assert_maximal_pages(Pages, Limit).

collect_source_pages(Round, Acc) ->
    receive
        {cache_snapshot_page, _, Round, N, Entries, _, More} ->
            ?assertEqual(length(Acc) + 1, N),
            case More of
                true -> imboy_cache_sync_snapshot:continue_snapshot(self(), Round, N + 1);
                false -> ok
            end,
            collect_source_pages(Round, [{Entries, More} | Acc]);
        {cache_snapshot_done, _, Round, 6, Total} ->
            ?assertEqual(length(Acc), Total),
            Ordered = lists:reverse(Acc),
            [
                ?assertEqual(I < Total, More)
             || {I, {_, More}} <- lists:zip(lists:seq(1, Total), Ordered)
            ],
            [E || {E, _} <- Ordered]
    after 1000 -> error(missing_sender_pages)
    end.

assert_maximal_pages([Page, [Next | _] = Rest | Tail], Limit) ->
    ?assert(length(Page) =:= 3 orelse erlang:external_size(Page ++ [Next]) > Limit),
    assert_maximal_pages([Rest | Tail], Limit);
assert_maximal_pages([_], _) ->
    ok.

sender_waits_for_exact_pull_and_expires_cursor_test() ->
    with_snapshot(fun() ->
        set_limits(#{p_max => 1}),
        Exp = erlang:system_time(millisecond) + 60000,
        [
            store_source_entry({deleted, {snapshot_delete_test, I}, undefined, Exp, {I, node()}})
         || I <- lists:seq(1, 3)
        ],
        ets:insert(?META_TABLE, {sync_counter, 3}),
        Round = crypto:strong_rand_bytes(12),
        ok = imboy_cache_sync_snapshot:send_snapshot(self(), Round),
        receive
            {cache_snapshot_page, _, Round, 1, [_], _, true} -> ok
        after 1000 -> error(no_first_page)
        end,
        ok = imboy_cache_sync_snapshot:continue_snapshot(self(), Round, 3),
        receive
            {cache_snapshot_page, _, Round, _, _, _, _} -> error(unrequested_page)
        after 0 -> ok
        end,
        ok = imboy_cache_sync_snapshot:continue_snapshot(self(), Round, 2),
        receive
            {cache_snapshot_page, _, Round, 2, [_], _, true} -> ok
        after 1000 -> error(no_second_page)
        end,
        [{K, Cursor}] = ets:lookup(?META_TABLE, {snapshot_sender, self()}),
        ets:insert(?META_TABLE, {K, Cursor#{deadline := 0}}),
        ok = imboy_cache_sync_snapshot:continue_snapshot(self(), Round, 3),
        ?assertEqual([], ets:lookup(?META_TABLE, K)),
        receive
            {cache_snapshot_page, _, Round, _, _, _, _} -> error(expired_cursor_sent)
        after 0 -> ok
        end
    end).

source_identity_change_cancels_prepared_page_test() ->
    lists:foreach(fun source_identity_change/1, [epoch, config, registry]).

source_identity_change(Kind) ->
    with_snapshot(fun() ->
        set_limits(#{p_max => 1}),
        Exp = erlang:system_time(millisecond) + 60000,
        [
            store_source_entry({deleted, {snapshot_delete_test, I}, undefined, Exp, {I, node()}})
         || I <- lists:seq(1, 3)
        ],
        Round = crypto:strong_rand_bytes(12),
        ok = imboy_cache_sync_snapshot:send_snapshot(self(), Round),
        receive
            {cache_snapshot_page, _, Round, 1, [_], _, true} -> ok
        after 1000 -> error(no_first_page)
        end,
        case Kind of
            epoch ->
                imboy_cache_sync_proto:bump_epoch();
            config ->
                set_limits(#{p_max => 2});
            registry ->
                application:set_env(
                    imboy,
                    cache_registry_overrides,
                    #{snapshot_delete_test => {local, false}}
                )
        end,
        ok = imboy_cache_sync_snapshot:continue_snapshot(self(), Round, 2),
        receive
            {cache_snapshot_abort, _, Round, source_changed} -> ok
        after 1000 -> error(no_source_change_abort)
        end,
        ?assertEqual([], ets:lookup(?META_TABLE, {snapshot_sender, self()})),
        receive
            {cache_snapshot_page, _, Round, _, _, _, _} -> error(old_epoch_page_sent)
        after 0 -> ok
        end
    end).

sender_cursor_bytes_are_bounded_before_first_page_test() ->
    with_snapshot(fun() ->
        set_limits(#{p_max => 1, b_max_bytes => 128}),
        Exp = erlang:system_time(millisecond) + 60000,
        [
            store_source_entry({deleted, {snapshot_delete_test, I}, undefined, Exp, {I, node()}})
         || I <- lists:seq(1, 3)
        ],
        Round = crypto:strong_rand_bytes(12),
        ok = imboy_cache_sync_snapshot:send_snapshot(self(), Round),
        receive
            {cache_snapshot_abort, _, Round, sender_budget_exceeded} -> ok
        after 1000 -> error(no_sender_budget_abort)
        end,
        ?assertEqual([], ets:lookup(?META_TABLE, {snapshot_sender, self()})),
        receive
            {cache_snapshot_page, _, Round, _, _, _, _} -> error(over_budget_page_sent)
        after 0 -> ok
        end
    end).

sender_done_is_preceded_by_current_origin_sequence_test() ->
    with_snapshot(fun() ->
        ets:insert(?META_TABLE, {sync_origin_seq, 17}),
        Round = crypto:strong_rand_bytes(12),
        ok = imboy_cache_sync_snapshot:send_snapshot(self(), Round),
        receive
            {cache_invalidation_watermark, Origin, <<1:128>>, 17, Hash, Registry} ->
                ?assertEqual(node(), Origin),
                ?assertEqual(imboy_cache_policy:config_hash(), Hash),
                ?assertEqual(imboy_cache_sync_proto:registry_hash(), Registry);
            Other ->
                error({watermark_not_first, Other})
        after 1000 -> error(no_tail_watermark)
        end,
        receive
            {cache_snapshot_done, _, Round, _, 0} -> ok
        after 1000 -> error(no_empty_done)
        end
    end).

apply_page(Entries, Number, State) ->
    Round = maps:get(round, State),
    Digest = imboy_cache_sync_snapshot:page_digest(Round, Number, Entries),
    imboy_cache_sync_snapshot:handle_page(node(), Round, Number, Entries, Digest, false, State).

authorized_round() ->
    R = imboy_cache_sync_snapshot:new_round(0, erlang:system_time(millisecond) + 60000),
    requested_peer(
        imboy_cache_sync_snapshot:expect_peer(R, node()), node()
    ).

set_limits(Limits) ->
    application:set_env(imboy, cache_replication, Limits#{whitelist => [snapshot_delete_test]}).

with_snapshot(Fun) ->
    OldCfg = application:get_env(imboy, cache_replication),
    OldReg = application:get_env(imboy, cache_registry_overrides),
    ets:new(?META_TABLE, [named_table, public, set]),
    ets:insert(?META_TABLE, [
        {status, ready},
        {node_boot, {node(), <<1:128>>}},
        {sync_origin_seq, 0}
    ]),
    set_limits(#{}),
    application:set_env(imboy, cache_registry_overrides, #{
        snapshot_delete_test => {replicate, false}
    }),
    meck:new(depcache, [passthrough, no_link]),
    meck:expect(depcache, set, 5, fun(_, _, _, _, _) -> ok end),
    meck:expect(depcache, flush, 2, fun(_, _) -> ok end),
    try
        Fun()
    after
        drain_fixture_watermarks(),
        meck:unload(depcache),
        ets:delete(?META_TABLE),
        restore(cache_replication, OldCfg),
        restore(cache_registry_overrides, OldReg)
    end.
restore(K, undefined) -> application:unset_env(imboy, K);
restore(K, {ok, V}) -> application:set_env(imboy, K, V).

requested_peer(Round, Peer) ->
    Authorized = imboy_cache_sync_snapshot:authorize_peer(Round, Peer),
    imboy_cache_sync_snapshot:mark_requested(Authorized, Peer).

drain_fixture_watermarks() ->
    receive
        {cache_invalidation_watermark, Origin, <<1:128>>, _, _, _} when Origin =:= node() ->
            drain_fixture_watermarks();
        {cache_snapshot_cut, Origin, _, <<1:128>>, _, _, _, _, _, _} when Origin =:= node() ->
            drain_fixture_watermarks();
        {cache_snapshot_cut, Origin, _, <<1:128>>, _, _, _, _, _} when Origin =:= node() ->
            drain_fixture_watermarks();
        {cache_snapshot_done, Origin, _, _, _} when Origin =:= node() ->
            drain_fixture_watermarks()
    after 0 -> ok
    end.
