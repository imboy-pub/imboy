-module(imboy_cache_snapshot_origin_tests).
-include_lib("eunit/include/eunit.hrl").
-include("imboy_cache_sync_state.hrl").

revision_origin_layout_and_budget_rows_test() ->
    Key = {origin_wire_test, layout},
    Rev = {1000, node()},
    Boot = <<17:128>>,
    Origin = {node(), Boot, 7},
    Row = {{rev, Key}, {Rev, {Boot, 7}, 9000, live, 3}},
    ?assertEqual(
        Row,
        imboy_cache_policy:revision_metadata_row(
            Key, Rev, 9000, unavailable, 3, Origin
        )
    ),
    ?assertEqual(
        {{rev, Key}, {Rev, 1000, 9000, live, 3}},
        imboy_cache_policy:revision_metadata_row(Key, Rev, 9000, live, 3)
    ),
    ?assertEqual(
        {{rev, Key}, {Rev, 1000, 9000, live, 3}},
        imboy_cache_policy:revision_metadata_row(
            Key, Rev, 9000, live, 3, {foreign_origin, Boot, 7}
        )
    ),
    [SetRow, _] = imboy_cache_policy:prospective_revision_rows(
        set, Key, Rev, 9000, 3, Origin
    ),
    ?assertEqual(Row, SetRow),
    [SnapshotRow, _] = imboy_cache_policy:snapshot_metadata_rows(
        {with_origin, Origin, {unavailable, Key, undefined, 9000, Rev}}, 3
    ),
    ?assertEqual(Row, SnapshotRow),
    DeleteRow = imboy_cache_policy:revision_metadata_row(
        Key, Rev, 9000, deleted, 3, Origin
    ),
    ?assertEqual(
        [DeleteRow],
        imboy_cache_policy:prospective_revision_rows(
            delete, Key, Rev, 9000, 3, Origin
        )
    ),
    ?assertEqual(
        [DeleteRow],
        imboy_cache_policy:snapshot_metadata_rows(
            {with_origin, Origin, {deleted, Key, undefined, 9000, Rev}}, 3
        )
    ).

origin_envelope_binds_digest_identity_and_full_page_budget_test() ->
    with_metadata(fun(Peer, Boot, Entry) ->
        Origin = {Peer, Boot, 7},
        Wire = imboy_cache_policy:snapshot_entry_with_origin(Entry, Origin),
        ?assert(imboy_cache_sync_proto:valid_received_snapshot_entry(Wire)),
        ?assertEqual(
            imboy_cache_sync_proto:snapshot_entry_deadline(Entry),
            imboy_cache_sync_proto:snapshot_entry_deadline(Wire)
        ),
        ?assertEqual(1000, imboy_cache_sync_proto:snapshot_revision_counter(Wire)),
        D = imboy_cache_sync_snapshot:page_digest(<<1:96>>, 1, [Wire]),
        ?assertNotEqual(
            D,
            imboy_cache_sync_snapshot:page_digest(
                <<1:96>>,
                1,
                [{with_origin, {Peer, Boot, 8}, Entry}]
            )
        ),
        ets:delete(imboy_cache_meta, {peer_incarnation, Peer}),
        ?assertNot(imboy_cache_sync_proto:valid_received_snapshot_entry(Wire)),
        ?assertEqual([], ets:lookup(imboy_cache_meta, {peer_incarnation, Peer})),
        ?assertNot(
            imboy_cache_sync_proto:valid_snapshot_entry(
                {with_origin, Origin, Wire}
            )
        ),
        application:set_env(
            imboy,
            cache_replication,
            #{whitelist => [origin_wire_test], p_max_bytes => erlang:external_size(Entry)}
        ),
        ets:insert(imboy_cache_meta, {{peer_incarnation, Peer}, Boot}),
        R0 = imboy_cache_sync_snapshot:new_round(0, erlang:system_time(millisecond) + 60000),
        R = imboy_cache_sync_snapshot:mark_requested(
            imboy_cache_sync_snapshot:authorize_peer(
                imboy_cache_sync_snapshot:expect_peer(R0, Peer), Peer
            ),
            Peer
        ),
        Round = maps:get(round, R),
        ?assertMatch(
            {abort, page_bytes_overflow, _},
            imboy_cache_sync_snapshot:handle_page(
                Peer,
                Round,
                1,
                [Wire],
                imboy_cache_sync_snapshot:page_digest(Round, 1, [Wire]),
                false,
                R
            )
        )
    end).

origin_envelope_reaches_receiver_without_claiming_foreign_coverage_test() ->
    with_metadata(fun(Peer, Boot, Entry) ->
        Exp = element(4, Entry),
        Wire = {with_origin, {Peer, Boot, 7}, Entry},
        R0 = imboy_cache_sync_snapshot:new_round(0, erlang:system_time(millisecond) + 60000),
        R = imboy_cache_sync_snapshot:mark_requested(
            imboy_cache_sync_snapshot:authorize_peer(
                imboy_cache_sync_snapshot:expect_peer(R0, Peer), Peer
            ),
            Peer
        ),
        Round = maps:get(round, R),
        Digest = imboy_cache_sync_snapshot:page_digest(Round, 1, [Wire]),
        meck:new(depcache, [passthrough, no_link]),
        meck:expect(depcache, flush, 2, fun(_, _) -> ok end),
        try
            ets:insert(imboy_cache_meta, {{snapshot_sender, self()}, #{round => <<2:96>>}}),
            ok = imboy_cache_sync_apply:journal_refresh(),
            {ok, Next} = imboy_cache_sync_snapshot:handle_page(
                Peer, Round, 1, [Wire], Digest, true, R
            ),
            ?assertEqual(1, maps:get(applied_items, Next)),
            ?assertMatch(
                [{_, {{1000, Peer}, {Boot, 7}, Exp, deleted, 0}}],
                ets:lookup(imboy_cache_meta, {rev, {origin_wire_test, key}})
            ),
            ?assertEqual([], ets:match_object(imboy_cache_meta, {{covered_seq, '_'}, '_'})),
            ?assertEqual([], ets:match_object(imboy_cache_meta, {{snapshot_seq_floor, '_'}, '_'})),
            {ok, [{_, 0, Entry, {Peer, Boot, 7}}]} = imboy_cache_sync_apply:journal_after(0),
            {[{journal_entry, _, Wire}], 1} = imboy_cache_sync_apply:journal_keys_after(0),
            Older = {with_origin, {Peer, Boot, 8}, setelement(5, Entry, {900, Peer})},
            {ok, _} = imboy_cache_sync_snapshot:handle_page(
                Peer,
                Round,
                2,
                [Older],
                imboy_cache_sync_snapshot:page_digest(Round, 2, [Older]),
                false,
                Next
            ),
            ?assertEqual(1, imboy_cache_sync_apply:journal_position()),
            ?assertEqual(1, meck:num_calls(depcache, flush, ['_', '_']))
        after
            meck:unload(depcache)
        end
    end).

expired_origin_envelope_preserves_floor_and_replay_origin_test() ->
    with_metadata(fun(Peer, Boot, Base) ->
        Entry = setelement(4, setelement(1, Base, expired), erlang:system_time(millisecond) - 1),
        Wire = {with_origin, {Peer, Boot, 7}, Entry},
        R0 = imboy_cache_sync_snapshot:new_round(0, erlang:system_time(millisecond) + 60000),
        R = imboy_cache_sync_snapshot:mark_requested(
            imboy_cache_sync_snapshot:authorize_peer(
                imboy_cache_sync_snapshot:expect_peer(R0, Peer), Peer
            ),
            Peer
        ),
        Round = maps:get(round, R),
        meck:new(depcache, [passthrough, no_link]),
        meck:expect(depcache, flush, 2, fun(_, _) -> ok end),
        try
            ets:insert(imboy_cache_meta, {{snapshot_sender, self()}, #{round => <<3:96>>}}),
            ok = imboy_cache_sync_apply:journal_refresh(),
            {ok, _} = imboy_cache_sync_snapshot:handle_page(
                Peer,
                Round,
                1,
                [Wire],
                imboy_cache_sync_snapshot:page_digest(Round, 1, [Wire]),
                false,
                R
            ),
            {ok, [{_, 0, Entry, {Peer, Boot, 7}}]} = imboy_cache_sync_apply:journal_after(0),
            {[{journal_entry, _, Wire}], 1} = imboy_cache_sync_apply:journal_keys_after(0)
        after
            meck:unload(depcache)
        end
    end).

unavailable_origin_envelope_replays_floor_without_inventing_value_test() ->
    with_metadata(fun(Peer, Boot, Base) ->
        Entry = setelement(1, Base, unavailable),
        Wire = {with_origin, {Peer, Boot, 7}, Entry},
        R0 = imboy_cache_sync_snapshot:new_round(0, erlang:system_time(millisecond) + 60000),
        R = imboy_cache_sync_snapshot:mark_requested(
            imboy_cache_sync_snapshot:authorize_peer(
                imboy_cache_sync_snapshot:expect_peer(R0, Peer), Peer
            ),
            Peer
        ),
        Round = maps:get(round, R),
        meck:new(depcache, [passthrough, no_link]),
        meck:expect(depcache, flush, 2, fun(_, _) -> ok end),
        meck:expect(depcache, get, 2, fun(_, _) -> undefined end),
        try
            ets:insert(imboy_cache_meta, {{snapshot_sender, self()}, #{round => <<4:96>>}}),
            ok = imboy_cache_sync_apply:journal_refresh(),
            {ok, _} = imboy_cache_sync_snapshot:handle_page(
                Peer,
                Round,
                1,
                [Wire],
                imboy_cache_sync_snapshot:page_digest(Round, 1, [Wire]),
                false,
                R
            ),
            {ok, [{_, 0, Entry, {Peer, Boot, 7}}]} = imboy_cache_sync_apply:journal_after(0),
            {[{journal_entry, _, Wire}], 1} = imboy_cache_sync_apply:journal_keys_after(0),
            ?assertEqual(undefined, depcache:get({origin_wire_test, key}, imboy_cache)),
            ?assertMatch(
                [{_, {{1000, Peer}, _, _, live, 0}}],
                ets:lookup(imboy_cache_meta, {rev, {origin_wire_test, key}})
            ),
            ?assertEqual([], ets:match_object(imboy_cache_meta, {{covered_seq, '_'}, '_'}))
        after
            meck:unload(depcache)
        end
    end).

historical_annotation_is_data_not_active_identity_test() ->
    with_metadata(fun(Peer, CurrentBoot, Entry) ->
        OldBoot = <<99:128>>,
        Wire = {with_origin, {Peer, OldBoot, 7}, Entry},
        ?assertNot(imboy_cache_sync_proto:valid_received_snapshot_entry(Wire)),
        R = requested_round(Peer),
        Round = maps:get(round, R),
        meck:new(depcache, [passthrough, no_link]),
        meck:expect(depcache, flush, 2, fun(_, _) -> ok end),
        try
            {ok, _} = imboy_cache_sync_snapshot:handle_page(
                Peer,
                Round,
                1,
                [Wire],
                imboy_cache_sync_snapshot:page_digest(Round, 1, [Wire]),
                false,
                R
            ),
            ?assertMatch(
                [{_, {{1000, Peer}, {OldBoot, 7}, _, deleted, 0}}],
                ets:lookup(imboy_cache_meta, {rev, {origin_wire_test, key}})
            ),
            ?assertEqual(
                [{{peer_incarnation, Peer}, CurrentBoot}],
                ets:lookup(imboy_cache_meta, {peer_incarnation, Peer})
            ),
            ?assertEqual(
                {error, unknown_incarnation},
                imboy_cache_sync_proto:check_incarnation(Peer, OldBoot)
            ),
            [
                ?assertEqual([], ets:match_object(imboy_cache_meta, {{Tag, '_'}, '_'}))
             || Tag <- [peer_grace, peer_seq, covered_seq, snapshot_seq_floor]
            ],
            ?assertNot(imboy_cache_sync_proto:valid_received_snapshot_entry(Wire))
        after
            meck:unload(depcache)
        end
    end).

historical_annotation_requires_current_snapshot_authority_test() ->
    with_metadata(fun(Peer, Boot, Entry) ->
        Wire = {with_origin, {Peer, <<99:128>>, 7}, Entry},
        R = requested_round(Peer),
        Round = maps:get(round, R),
        Digest = imboy_cache_sync_snapshot:page_digest(Round, 1, [Wire]),
        ets:insert(imboy_cache_meta, {{peer_incarnation, Peer}, <<100:128>>}),
        ?assertMatch(
            {abort, invalid_snapshot_entry, _},
            imboy_cache_sync_snapshot:handle_page(Peer, Round, 1, [Wire], Digest, false, R)
        ),
        ets:insert(imboy_cache_meta, {{peer_incarnation, Peer}, Boot}),
        ets:insert(imboy_cache_meta, {{peer_epoch, Peer}, 1}),
        ?assertMatch(
            {abort, invalid_snapshot_entry, _},
            imboy_cache_sync_snapshot:handle_page(Peer, Round, 1, [Wire], Digest, false, R)
        ),
        ets:delete(imboy_cache_meta, {peer_epoch, Peer}),
        ?assertMatch(
            {abort, page_digest_mismatch, _},
            imboy_cache_sync_snapshot:handle_page(
                Peer, Round, 1, [Wire], corrupt_digest(Digest), false, R
            )
        ),
        ?assertEqual([], ets:lookup(imboy_cache_meta, {rev, {origin_wire_test, key}}))
    end).

base_snapshot_emits_original_boot_and_sequence_for_all_kinds_test() ->
    with_metadata(fun(Peer, _, Base) ->
        Boot = <<99:128>>,
        Exp = element(4, Base),
        Rev = element(5, Base),
        ets:insert(imboy_cache_meta, [
            {node_boot, {node(), <<1:128>>}}, {status, ready}, {sync_origin_seq, 0}
        ]),
        meck:new(depcache, [passthrough, no_link]),
        meck:expect(depcache, get, 2, fun
            ({origin_wire_test, live}, _) -> {ok, value};
            (_, _) -> undefined
        end),
        try
            lists:foreach(
                fun(Kind) ->
                    Key = {origin_wire_test, Kind},
                    KindExp =
                        case Kind of
                            expired -> erlang:system_time(millisecond) - 1000;
                            _ -> Exp
                        end,
                    MetaKind =
                        case Kind of
                            deleted -> deleted;
                            _ -> live
                        end,
                    imboy_cache_sync_apply:store_revision(
                        Key,
                        Rev,
                        KindExp,
                        MetaKind,
                        0,
                        {Peer, Boot, 7}
                    )
                end,
                [live, deleted, expired, unavailable]
            ),
            Round = <<89:96>>,
            ok = send_fixture_snapshot(Round),
            receive
                {cache_snapshot_page, _, Round, 1, Page, _, false} ->
                    ?assertEqual(4, length(Page)),
                    ?assert(
                        lists:all(
                            fun
                                ({with_origin, {P, B, 7}, _}) ->
                                    P =:= Peer andalso B =:= Boot;
                                (_) ->
                                    false
                            end,
                            Page
                        )
                    ),
                    Bodies = [imboy_cache_policy:snapshot_entry_body(E) || E <- Page],
                    ?assert(lists:member({{origin_wire_test, live}, value, Exp, Rev}, Bodies)),
                    ?assert(
                        lists:member(
                            {deleted, {origin_wire_test, deleted}, undefined, Exp, Rev}, Bodies
                        )
                    ),
                    ?assert(
                        lists:member(
                            {unavailable, {origin_wire_test, unavailable}, undefined, Exp, Rev},
                            Bodies
                        )
                    ),
                    ?assertEqual(1, length([E || E = {expired, _, _, _, _} <- Bodies]))
            after 1000 -> error(missing_origin_base_page)
            end,
            receive
                {cache_invalidation_watermark, _, <<1:128>>, 0, _, _} -> ok
            after 1000 -> error(missing_origin_base_watermark)
            end,
            receive
                {cache_snapshot_cut, _Source, Round, <<1:128>>, 0, 0, _Hash, _Registry, _Candidate,
                    _Certificate} = Cut ->
                    assert_frozen_cut(Cut)
            after 1000 -> error(missing_origin_base_cut)
            end,
            receive
                {cache_snapshot_done, _, Round, _, 1} -> ok
            after 1000 -> error(missing_origin_base_done)
            end
        after
            meck:unload(depcache)
        end
    end).

retained_expired_tombstone_is_sent_with_original_deadline_test() ->
    with_metadata(fun(Peer, Boot, Base) ->
        Exp = erlang:system_time(millisecond) - 1000,
        Key = element(2, Base),
        Rev = element(5, Base),
        Entry = {deleted, Key, undefined, Exp, Rev},
        Wire = {with_origin, {Peer, Boot, 7}, Entry},
        ets:insert(imboy_cache_meta, [
            {node_boot, {node(), <<1:128>>}},
            {status, ready},
            {sync_origin_seq, 0}
        ]),
        imboy_cache_sync_apply:store_revision(Key, Rev, Exp, deleted, 0, {Peer, Boot, 7}),
        Round = <<90:96>>,
        ok = send_fixture_snapshot(Round),
        try
            receive
                {cache_snapshot_page, _, Round, 1, [Wire], _, false} -> ok
            after 1000 -> error(retained_tombstone_omitted)
            end,
            ?assertEqual(
                Exp + imboy_cache_policy:tombstone_ttl_sec() * 1000,
                imboy_cache_policy:snapshot_entry_deadline(Wire)
            ),
            sender_end(Round, 1)
        after
            drain_sender(Round)
        end
    end).

expired_tombstone_suppresses_later_low_revision_in_both_source_orders_test() ->
    with_metadata(fun(Peer, Boot, Base) ->
        Key = element(2, Base),
        Exp = erlang:system_time(millisecond) - 1000,
        Rev = element(5, Base),
        Delete = {with_origin, {Peer, Boot, 7}, {deleted, Key, undefined, Exp, Rev}},
        Other = 'later_low_revision@synthetic',
        ets:insert(imboy_cache_meta, {{peer_incarnation, Other}, <<8:128>>}),
        Live = {Key, stale, erlang:system_time(millisecond) + 60000, {900, Other}},
        Cache = ets:new(origin_merge_values, [public, set]),
        meck:new(depcache, [passthrough, no_link]),
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
        try
            lists:foreach(
                fun(Order) ->
                    ets:delete(imboy_cache_meta, {rev, Key}),
                    ets:delete(Cache, Key),
                    R = imboy_cache_sync_snapshot:mark_requested(
                        imboy_cache_sync_snapshot:authorize_peer(
                            imboy_cache_sync_snapshot:expect_peer(requested_round(Peer), Other),
                            Other
                        ),
                        Other
                    ),
                    lists:foldl(
                        fun({Source, Entry}, State) ->
                            Round = maps:get(round, State),
                            {ok, Next} = imboy_cache_sync_snapshot:handle_page(
                                Source,
                                Round,
                                1,
                                [Entry],
                                imboy_cache_sync_snapshot:page_digest(Round, 1, [Entry]),
                                false,
                                State
                            ),
                            Next
                        end,
                        R,
                        Order
                    ),
                    ?assertEqual(undefined, depcache:get(Key, imboy_cache)),
                    ?assertEqual(
                        [{{rev, Key}, {Rev, {Boot, 7}, Exp, deleted, 0}}],
                        ets:lookup(imboy_cache_meta, {rev, Key})
                    )
                end,
                [[{Other, Live}, {Peer, Delete}], [{Peer, Delete}, {Other, Live}]]
            )
        after
            meck:unload(depcache),
            ets:delete(Cache)
        end
    end).

expired_tombstone_has_bounded_window_and_tombstone_budget_test() ->
    with_metadata(fun(Peer, Boot, Base) ->
        Exp = erlang:system_time(millisecond) - 1000,
        Entry = setelement(4, Base, Exp),
        Wire = {with_origin, {Peer, Boot, 7}, Entry},
        ?assert(imboy_cache_sync_proto:valid_snapshot_entry(Wire)),
        TooOld = setelement(4, Entry, Exp - imboy_cache_policy:tombstone_ttl_sec() * 1000),
        ?assertNot(imboy_cache_sync_proto:valid_snapshot_entry(TooOld)),
        application:set_env(imboy, cache_replication, #{
            whitelist => [origin_wire_test], t_max_bytes => 1
        }),
        R = requested_round(Peer),
        Round = maps:get(round, R),
        ?assertMatch(
            {abort, tombstone_budget_exceeded, _},
            imboy_cache_sync_snapshot:handle_page(
                Peer,
                Round,
                1,
                [Wire],
                imboy_cache_sync_snapshot:page_digest(Round, 1, [Wire]),
                false,
                R
            )
        ),
        ?assertEqual([], ets:lookup(imboy_cache_meta, {rev, element(2, Base)}))
    end).

retained_expired_tombstone_replays_without_renewing_deadline_test() ->
    with_metadata(fun(Peer, Boot, Base) ->
        Exp = erlang:system_time(millisecond) - 1000,
        Entry = setelement(4, Base, Exp),
        Wire = {with_origin, {Peer, Boot, 7}, Entry},
        R = requested_round(Peer),
        Round = maps:get(round, R),
        ets:insert(imboy_cache_meta, {{snapshot_sender, self()}, #{round => <<91:96>>}}),
        imboy_cache_sync_apply:journal_refresh(),
        Start = imboy_cache_sync_apply:journal_position(),
        meck:new(depcache, [passthrough, no_link]),
        meck:expect(depcache, flush, 2, fun(_, _) -> ok end),
        try
            {ok, _} = imboy_cache_sync_snapshot:handle_page(
                Peer,
                Round,
                1,
                [Wire],
                imboy_cache_sync_snapshot:page_digest(Round, 1, [Wire]),
                false,
                R
            ),
            ?assertMatch(
                {ok, [{_, 0, Entry, {Peer, Boot, 7}}]},
                imboy_cache_sync_apply:journal_after(Start)
            ),
            {[{journal_entry, _, Wire}], _} = imboy_cache_sync_apply:journal_keys_after(Start),
            ?assertEqual(Exp, element(4, imboy_cache_policy:snapshot_entry_body(Wire)))
        after
            meck:unload(depcache)
        end
    end).

receiver_crossing_tombstone_window_does_not_ack_page_test() ->
    with_metadata(fun(Peer, Boot, Base) ->
        Live = {
            {origin_wire_test, slow}, value, erlang:system_time(millisecond) + 60000, {1, Peer}
        },
        R = requested_round(Peer),
        Round = maps:get(round, R),
        meck:new(depcache, [passthrough, no_link]),
        meck:expect(depcache, set, 5, fun(_, _, _, _, _) ->
            timer:sleep(500),
            ok
        end),
        meck:expect(depcache, flush, 2, fun(_, _) -> ok end),
        try
            Exp =
                erlang:system_time(millisecond) + 300 -
                    imboy_cache_policy:tombstone_ttl_sec() * 1000,
            Tomb = {with_origin, {Peer, Boot, 7}, setelement(4, Base, Exp)},
            Page = [Live, Tomb],
            {abort, snapshot_entry_expired, Failed} = imboy_cache_sync_snapshot:handle_page(
                Peer,
                Round,
                1,
                Page,
                imboy_cache_sync_snapshot:page_digest(Round, 1, Page),
                false,
                R
            ),
            ?assertEqual(0, maps:get(manifest_items, Failed, 0)),
            ?assertEqual([], maps:get(manifest, maps:get(Peer, maps:get(peers, Failed)), [])),
            ?assertEqual(0, imboy_cache_sync_snapshot:applied_count(R)),
            ?assertNot(imboy_cache_sync_snapshot:round_complete(R))
        after
            meck:unload(depcache)
        end
    end).

partial_page_abort_retains_accepted_clock_and_hides_values_test() ->
    with_metadata(fun(Peer, Boot, Base) ->
        application:set_env(imboy, cache_replication, #{
            whitelist => [origin_wire_test], r_max_rounds => 1
        }),
        LiveKey = {origin_wire_test, partial_high},
        Live = {LiveKey, value, erlang:system_time(millisecond) + 60000, {5000, Peer}},
        Cache = ets:new(origin_partial_values, [public, set]),
        R = requested_round(Peer),
        Round = maps:get(round, R),
        State = #state{boot_id = <<1:128>>, counter = 0, status = recovering, snapshot_round = R},
        meck:new(depcache, [passthrough, no_link]),
        meck:expect(depcache, set, 5, fun(K, V, _, _, _) ->
            ets:insert(Cache, {K, V}),
            timer:sleep(500),
            ok
        end),
        meck:expect(depcache, flush, 2, fun(K, _) ->
            ets:delete(Cache, K),
            ok
        end),
        try
            Exp =
                erlang:system_time(millisecond) + 300 -
                    imboy_cache_policy:tombstone_ttl_sec() * 1000,
            Tomb = {with_origin, {Peer, Boot, 7}, setelement(4, Base, Exp)},
            Page = [Live, Tomb],
            {noreply, Next} = imboy_cache_sync:handle_info(
                {cache_snapshot_page, Peer, Round, 1, Page,
                    imboy_cache_sync_snapshot:page_digest(Round, 1, Page), false},
                State
            ),
            ?assertEqual(degraded, Next#state.status),
            ?assertEqual([], ets:lookup(Cache, LiveKey)),
            ?assertMatch(
                [{_, {{5000, Peer}, _, _, live, 0}}], ets:lookup(imboy_cache_meta, {rev, LiveKey})
            ),
            ?assertEqual(5000, Next#state.counter),
            ?assertEqual([{sync_counter, 5000}], ets:lookup(imboy_cache_meta, sync_counter)),
            BadKey = {origin_wire_test, invalid_high},
            BadPage = [{BadKey, value, erlang:system_time(millisecond) + 60000, {9999, Peer}}],
            R2 = requested_round(Peer),
            Round2 = maps:get(round, R2),
            {noreply, Rejected} = imboy_cache_sync:handle_info(
                {cache_snapshot_page, Peer, Round2, 1, BadPage,
                    corrupt_digest(imboy_cache_sync_snapshot:page_digest(Round2, 1, BadPage)),
                    false},
                Next#state{status = recovering, snapshot_round = R2}
            ),
            ?assertEqual(5000, Rejected#state.counter),
            ?assertEqual([{sync_counter, 5000}], ets:lookup(imboy_cache_meta, sync_counter)),
            ?assertEqual([], ets:lookup(imboy_cache_meta, {rev, BadKey}))
        after
            meck:unload(depcache),
            ets:delete(Cache)
        end
    end).

sender_end(Round, Total) ->
    receive
        {cache_invalidation_watermark, _, <<1:128>>, 0, _, _} -> ok
    after 1000 -> error(missing_origin_base_watermark)
    end,
    receive
        {cache_snapshot_cut, _Source, Round, <<1:128>>, 0, 0, _Hash, _Registry, _Candidate,
            _Certificate} =
                Cut ->
            assert_frozen_cut(Cut)
    after 1000 -> error(missing_origin_base_cut)
    end,
    receive
        {cache_snapshot_done, _, Round, _, Total} -> ok
    after 1000 -> error(missing_origin_base_done)
    end.
drain_sender(Round) ->
    receive
        {cache_snapshot_page, _, Round, _, _, _, _} ->
            drain_sender(Round);
        {cache_snapshot_cut, Source, Round, <<1:128>>, 0, 0, _Hash, _Registry, _Candidate,
            _Certificate} =
                Cut when
            Source =:= node()
        ->
            assert_frozen_cut(Cut),
            drain_sender(Round);
        {cache_snapshot_done, _, Round, _, _} ->
            drain_sender(Round);
        {cache_snapshot_abort, _, Round, _} ->
            drain_sender(Round);
        {cache_invalidation_watermark, _, <<1:128>>, 0, _, _} ->
            drain_sender(Round)
    after 0 -> ok
    end.

requested_round(Peer) ->
    imboy_cache_sync_snapshot:mark_requested(
        imboy_cache_sync_snapshot:authorize_peer(
            imboy_cache_sync_snapshot:expect_peer(
                imboy_cache_sync_snapshot:new_round(
                    0,
                    erlang:system_time(millisecond) + 60000
                ),
                Peer
            ),
            Peer
        ),
        Peer
    ).
with_metadata(Fun) ->
    pure_mock_fixture:run(fun() -> with_metadata_checks(Fun) end).

with_metadata_checks(Fun) ->
    OldCfg = application:get_env(imboy, cache_replication),
    OldRegistry = application:get_env(imboy, cache_registry_overrides),
    Peer = 'origin_wire@synthetic',
    Boot = <<7:128>>,
    ets:new(imboy_cache_meta, [named_table, public, set]),
    application:set_env(imboy, cache_registry_overrides, #{origin_wire_test => {replicate, false}}),
    application:set_env(imboy, cache_replication, #{whitelist => [origin_wire_test]}),
    ets:insert(imboy_cache_meta, [{sync_epoch, 0}, {{peer_incarnation, Peer}, Boot}]),
    Entry =
        {deleted, {origin_wire_test, key}, undefined, erlang:system_time(millisecond) + 60000,
            {1000, Peer}},
    try
        Fun(Peer, Boot, Entry)
    after
        ets:delete(imboy_cache_meta),
        restore(cache_replication, OldCfg),
        restore(cache_registry_overrides, OldRegistry)
    end.

restore(Key, undefined) -> application:unset_env(imboy, Key);
restore(Key, {ok, Value}) -> application:set_env(imboy, Key, Value).

corrupt_digest(<<First, Rest/binary>>) -> <<(First bxor 1), Rest/binary>>.

send_fixture_snapshot(Round) ->
    {Source, Boot} = imboy_cache_sync:node_boot(),
    [{sync_origin_seq, Seq}] = ets:lookup(imboy_cache_meta, sync_origin_seq),
    Epoch = imboy_cache_sync_proto:epoch(),
    Cut =
        {cache_snapshot_cut, Source, Round, Boot, Epoch, Seq, imboy_cache_policy:config_hash(),
            imboy_cache_sync_proto:registry_hash(),
            imboy_cache_sync_proto:application_coverage_candidate(),
            imboy_cache_sync_coverage:source_certificate(Epoch, erlang:system_time(millisecond))},
    ?assert(imboy_cache_sync_proto:valid_snapshot_cut(Cut)),
    put({fixture_cut, Round}, Cut),
    imboy_cache_sync_snapshot:send_snapshot(self(), Round).

assert_frozen_cut({cache_snapshot_cut, _, Round, _, _, _, _, _, _, _} = Cut) ->
    ?assertEqual(get({fixture_cut, Round}), Cut).
