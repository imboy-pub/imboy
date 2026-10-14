-module(imboy_cache_snapshot_coverage_tests).
-include_lib("eunit/include/eunit.hrl").
-include("imboy_cache_sync_state.hrl").

fresh_peer_prefix_zero_is_not_an_empty_content_certificate_test() ->
    fixture(fun(Source, _, _) ->
        %% 冻结合同：候选要求每个登记来源有显式 covered_seq 行（缺项整份拒绝）；
        %% 新化身合法零前缀以显式零行表达，不以缺项造零。
        ets:insert(imboy_cache_meta, {{covered_seq, {Source, <<7:128>>}}, {0, 0}}),
        ?assertEqual(
            {ok, [{Source, <<7:128>>, 0, 0}]},
            imboy_cache_sync_proto:application_coverage_candidate()
        ),
        Empty = {0, crypto:hash(sha256, term_to_binary([]))},
        ?assertEqual(Empty, imboy_cache_sync_coverage:content_census()),
        Exp = erlang:system_time(millisecond) + 60000,
        ets:insert(
            imboy_cache_meta,
            imboy_cache_policy:revision_metadata_row(
                {coverage_wire_test, future}, {7, Source}, Exp, live, 1
            )
        ),
        ?assertMatch({1, _}, imboy_cache_sync_coverage:content_census()),
        ?assertEqual(
            Empty, imboy_cache_sync_coverage:source_certificate(0, erlang:system_time(millisecond))
        )
    end).

round_census_is_bound_to_exact_pid_boot_epoch_and_first_report_test() ->
    fixture(fun(_, R, _) ->
        Empty = {0, crypto:hash(sha256, term_to_binary([]))},
        Token = maps:get(round, R),
        Report =
            {cache_version_report, node(), 0, 0, imboy_cache_sync_proto:config_hash(),
                imboy_cache_sync_proto:registry_hash(), <<1:128>>, false, self(), 0, Token, Empty},
        ?assert(imboy_cache_sync_proto:valid_version_report(Report)),
        Target = imboy_cache_policy:version_report_target(Report),
        R0 = imboy_cache_sync_snapshot:expect_peer(R, node()),
        {ok, R1} = imboy_cache_sync_proto:record_origin_target(Target, R0),
        P = maps:get(node(), maps:get(peers, R1)),
        ?assert(maps:get(reported_empty, P)),
        ?assertEqual(self(), maps:get(reported_pid, P)),
        ?assertMatch(
            {abort, peer_census_changed, _},
            imboy_cache_sync_proto:record_origin_target(setelement(6, Target, {1, <<0:256>>}), R1)
        ),
        ?assertMatch(
            {abort, origin_target_round_mismatch, _},
            imboy_cache_sync_proto:record_origin_target(setelement(5, Target, <<2:96>>), R1)
        ),
        ?assertNot(imboy_cache_sync_proto:valid_version_report(setelement(12, Report, {0, <<0>>})))
    end).

coverage_cut_is_canonical_bounded_and_epoch_bound_test() ->
    fixture(fun(_, _, Base) ->
        A = {a_coverage@synthetic, <<8:128>>, 0, 7},
        B = {b_coverage@synthetic, <<9:128>>, 0, 8},
        Good = erlang:append_element(Base, {ok, [A, B]}),
        ?assert(imboy_cache_sync_proto:valid_snapshot_cut(Good)),
        ?assertEqual(Base, imboy_cache_policy:snapshot_cut_base(Good)),
        ?assertEqual({ok, [A, B]}, imboy_cache_policy:snapshot_cut_coverage(Good)),
        ?assertEqual(
            {error, missing_application_coverage},
            imboy_cache_policy:snapshot_cut_coverage(Base)
        ),
        Bad = [
            [B, A],
            [A, A],
            [A, setelement(2, A, <<9:128>>)],
            [setelement(3, A, 1)],
            [setelement(4, A, -1)],
            [setelement(2, A, <<1>>)],
            [{undefined, <<8:128>>, 0, 1}, {undefined, <<8:128>>, 0, 2}]
        ],
        [
            ?assertNot(
                imboy_cache_sync_proto:valid_snapshot_cut(
                    erlang:append_element(Base, {ok, V})
                )
            )
         || V <- Bad
        ],
        ?assertNot(
            imboy_cache_sync_proto:valid_snapshot_cut(
                erlang:append_element(Base, {error, invented_reason})
            )
        ),
        application:set_env(imboy, cache_replication, #{peer_max => 1}),
        ?assertNot(imboy_cache_sync_proto:valid_snapshot_cut(Good))
    end).

coverage_error_is_preserved_without_fabricating_empty_proof_test() ->
    fixture(fun(_, R, Base) ->
        lists:foreach(
            fun(Reason) ->
                F = erlang:append_element(Base, {error, Reason}),
                ?assert(imboy_cache_sync_proto:valid_snapshot_cut(F)),
                {ok, Next} = imboy_cache_sync_recover:accept_source_cut(F, R),
                ?assertEqual(
                    {error, Reason},
                    imboy_cache_policy:snapshot_cut_coverage(
                        stored_cut(Next, element(2, Base))
                    )
                )
            end,
            [coverage_peer_budget, coverage_byte_budget, incomplete_application_coverage]
        ),
        ?assert(imboy_cache_sync_proto:valid_snapshot_cut(erlang:append_element(Base, {ok, []}))),
        ?assertEqual([], ets:match_object(imboy_cache_meta, {{covered_seq, '_'}, '_'})),
        ?assertEqual([], ets:match_object(imboy_cache_meta, {{snapshot_seq_floor, '_'}, '_'}))
    end).

coverage_cut_is_immutable_and_cannot_register_foreign_identity_test() ->
    fixture(fun(Source, R, Base) ->
        Foreign = coverage_other@synthetic,
        Candidate = {ok, [{Foreign, <<9:128>>, 0, 7}]},
        F = erlang:append_element(
            erlang:append_element(Base, Candidate),
            {0, crypto:hash(sha256, term_to_binary([]))}
        ),
        {ok, Next} = imboy_cache_sync_recover:accept_source_cut(F, R),
        ?assertEqual(F, stored_cut(Next, Source)),
        ?assertEqual({ok, Next}, imboy_cache_sync_recover:accept_source_cut(F, Next)),
        Changed = setelement(9, F, {ok, [{Foreign, <<9:128>>, 0, 8}]}),
        ?assertMatch(
            {abort, conflicting_source_cut, _},
            imboy_cache_sync_recover:accept_source_cut(Changed, Next)
        ),
        {ok, Complete} = imboy_cache_sync_snapshot:handle_done(
            Source, maps:get(round, R), 0, 0, Next
        ),
        ?assert(imboy_cache_sync_snapshot:round_complete(Complete)),
        ok = imboy_cache_sync_proto:commit_snapshot_floors(Complete),
        ?assertEqual([], ets:lookup(imboy_cache_meta, {peer_incarnation, Foreign})),
        ?assertEqual([], ets:lookup(imboy_cache_meta, {covered_seq, {Foreign, <<9:128>>}})),
        ?assertEqual([], ets:lookup(imboy_cache_meta, {snapshot_seq_floor, {Foreign, <<9:128>>}})),
        ?assertEqual(
            [{{snapshot_seq_floor, {Source, <<7:128>>}}, 0}],
            ets:lookup(imboy_cache_meta, {snapshot_seq_floor, {Source, <<7:128>>}})
        )
    end).

source_cut_cannot_survive_sender_leaving_current_membership_test() ->
    fixture(fun(_, R, Base) ->
        F = erlang:append_element(Base, {ok, []}),
        meck:expect(imboy_cache_sync, syn_scope_members, 0, fun() -> [] end),
        ?assertMatch(
            {abort, source_cut_identity_changed, _},
            imboy_cache_sync_recover:accept_source_cut(F, R)
        )
    end).

coverage_cut_rechecks_source_identity_epoch_and_round_test() ->
    fixture(fun(Source, R, Base) ->
        F = erlang:append_element(Base, {ok, []}),
        ?assertEqual(
            {ok, R},
            imboy_cache_sync_recover:accept_source_cut(
                setelement(3, F, <<99:96>>), R
            )
        ),
        Peers = maps:get(peers, R),
        P = maps:get(Source, Peers),
        Waiting = R#{peers := Peers#{Source := P#{requested := false}}},
        ?assertEqual({ok, Waiting}, imboy_cache_sync_recover:accept_source_cut(F, Waiting)),
        {ok, Accepted} = imboy_cache_sync_recover:accept_source_cut(F, R),
        ets:insert(imboy_cache_meta, {{peer_epoch, Source}, 1}),
        ?assertMatch(
            {abort, source_cut_identity_changed, _},
            imboy_cache_sync_recover:accept_source_cut(F, Accepted)
        ),
        ets:delete(imboy_cache_meta, {peer_epoch, Source}),
        ets:insert(imboy_cache_meta, {{peer_incarnation, Source}, <<8:128>>}),
        ?assertMatch(
            {abort, source_cut_identity_changed, _},
            imboy_cache_sync_recover:accept_source_cut(F, Accepted)
        ),
        ?assertEqual([], ets:match_object(imboy_cache_meta, {{snapshot_seq_floor, '_'}, '_'}))
    end).

older_nonempty_peer_does_not_invalidate_selected_higher_epoch_cohort_test() ->
    fixture(fun(Source, R, Base) ->
        Cohort = 2,
        SourceBoot = <<7:128>>,
        Stale = 'coverage_stale_epoch@synthetic',
        StaleBoot = <<8:128>>,
        Cut = setelement(5, Base, Cohort),
        ets:insert(imboy_cache_meta, [
            {sync_epoch, Cohort},
            {{peer_epoch, Source}, Cohort},
            {{peer_incarnation, Stale}, StaleBoot},
            {{peer_epoch, Stale}, Cohort - 1}
        ]),
        Peers0 = maps:get(peers, R),
        SourcePeer = maps:get(Source, Peers0),
        R1 = R#{
            cohort_epoch => Cohort,
            peers := Peers0#{
                Source := SourcePeer#{
                    boot => SourceBoot,
                    source_epoch => Cohort,
                    source_pid => self(),
                    source_cut => Cut
                },
                Stale => #{
                    authority => false,
                    reported => true,
                    reported_empty => false,
                    reported_pid => self(),
                    reported_boot => StaleBoot,
                    reported_epoch => Cohort - 1
                }
            }
        },
        ?assert(imboy_cache_sync_recover:sources_current(R1)),
        ets:insert(imboy_cache_meta, {{peer_epoch, Stale}, Cohort}),
        SameEpoch = R1#{
            peers := (maps:get(peers, R1))#{
                Stale := (maps:get(Stale, maps:get(peers, R1)))#{reported_epoch => Cohort}
            }
        },
        ?assertNot(imboy_cache_sync_recover:sources_current(SameEpoch))
    end).

empty_peer_census_covers_its_reported_origin_target_test() ->
    fixture(fun(Source, R, Base) ->
        Empty = {0, crypto:hash(sha256, term_to_binary([]))},
        Peer = 'coverage_empty_peer@synthetic',
        PeerBoot = <<8:128>>,
        SourceBoot = <<7:128>>,
        Exp = erlang:system_time(millisecond) + 60000,
        Row = imboy_cache_policy:snapshot_manifest_entry(
            {with_origin, {Source, SourceBoot, 1}, {coverage_wire_test, present, Exp, {1, Source}}}
        ),
        SourceCut = erlang:append_element(
            erlang:append_element(
                setelement(6, setelement(4, Base, SourceBoot), 1),
                {ok, [{Source, SourceBoot, 0, 1}]}
            ),
            imboy_cache_sync_coverage:manifest_certificate(
                #{element(2, Row) => Row}
            )
        ),
        SourcePeer = #{
            authority => true,
            pages => 1,
            manifest => [{1, [Row]}],
            source_cut => SourceCut,
            origin_target => {Source, SourceBoot, 0, 1}
        },
        EmptyPeer = #{
            authority => false,
            origin_target => {Peer, PeerBoot, 0, 0},
            reported_empty => true,
            reported_pid => self(),
            reported_boot => PeerBoot,
            reported_epoch => 0,
            content_census => {Empty, self()}
        },
        Round = R#{
            cohort_epoch => 0,
            peers := #{Source => SourcePeer, Peer => EmptyPeer},
            retained_obligations := []
        },
        ?assertMatch(
            {ok, _},
            imboy_cache_sync_coverage:verify(
                Round, {1000000, 100}, erlang:system_time(millisecond)
            )
        ),
        InvalidEmptyPeers = [
            maps:remove(reported_empty, EmptyPeer),
            EmptyPeer#{origin_target := {Source, PeerBoot, 0, 0}},
            EmptyPeer#{origin_target := {Peer, <<9:128>>, 0, 0}},
            EmptyPeer#{origin_target := {Peer, PeerBoot, 0, 1}},
            EmptyPeer#{content_census := {{0, <<0:256>>}, self()}},
            EmptyPeer#{reported_pid := undefined}
        ],
        [
            ?assertEqual(
                {error, peer_content_unproven},
                imboy_cache_sync_coverage:verify(
                    Round#{peers := (maps:get(peers, Round))#{Peer := InvalidPeer}},
                    {1000000, 100},
                    erlang:system_time(millisecond)
                )
            )
         || InvalidPeer <- InvalidEmptyPeers
        ],
        ?assertEqual(Empty, element(1, maps:get(content_census, EmptyPeer)))
    end).

reported_watermark_gap_does_not_abort_covered_recovery_test() ->
    fixture(fun(Source, R, _) ->
        Boot = <<7:128>>,
        Peers0 = maps:get(peers, R),
        Peer0 = maps:get(Source, Peers0),
        Peer = Peer0#{
            origin_target => {Source, Boot, 0, 1},
            reported_boot => Boot,
            reported_epoch => 0
        },
        Round = R#{peers := Peers0#{Source := Peer}},
        State = #state{boot_id = <<1:128>>, status = recovering, snapshot_round = Round},
        meck:new(imboy_cache_sync_apply, [passthrough, no_link]),
        meck:expect(imboy_cache_sync_apply, reconcile_sequence, 3, fun(_, _, _) -> gap end),
        Frame = {
            cache_invalidation_watermark,
            Source,
            Boot,
            1,
            imboy_cache_sync_proto:config_hash(),
            imboy_cache_sync_proto:registry_hash()
        },
        try
            ?assertEqual(State, imboy_cache_sync_recover:handle_watermark(Frame, State)),
            ?assert(meck:validate(imboy_cache_sync_apply))
        after
            meck:unload(imboy_cache_sync_apply)
        end
    end).

coverage_cut_reaches_sync_callback_without_changing_ready_test() ->
    fixture(fun(Source, R, Base) ->
        F = erlang:append_element(Base, {ok, [{coverage_other@synthetic, <<9:128>>, 0, 7}]}),
        S = #state{boot_id = <<1:128>>, status = recovering, snapshot_round = R},
        {noreply, Next} = imboy_cache_sync:handle_info(F, S),
        ?assertEqual(recovering, Next#state.status),
        ?assertEqual(F, stored_cut(Next#state.snapshot_round, Source)),
        ?assertEqual([], ets:match_object(imboy_cache_meta, {{covered_seq, '_'}, '_'}))
    end).

sender_uses_captured_coverage_after_prefix_advances_test() ->
    fixture(fun(Source, _, _) ->
        application:set_env(imboy, cache_replication, #{
            whitelist => [coverage_wire_test], p_max => 1
        }),
        ets:insert(imboy_cache_meta, [
            {{covered_seq, {Source, <<7:128>>}}, {0, 7}},
            {status, ready},
            {sync_counter, 10}
        ]),
        Exp = erlang:system_time(millisecond) + 60000,
        [
            imboy_cache_sync_apply:store_revision(
                {coverage_wire_test, I}, {10, Source}, Exp, deleted
            )
         || I <- [1, 2]
        ],
        Round = <<95:96>>,
        ok = imboy_cache_sync_snapshot:send_snapshot(self(), Round),
        receive
            {cache_snapshot_page, _, Round, 1, [_], _, true} -> ok
        after 1000 -> error(missing_first_coverage_page)
        end,
        ok = imboy_cache_sync_proto:note_applied_sequence(Source, <<7:128>>, 8),
        ok = imboy_cache_sync_snapshot:continue_snapshot(self(), Round, 2),
        receive
            {cache_snapshot_page, _, Round, 2, [_], _, false} -> ok
        after 1000 -> error(missing_second_coverage_page)
        end,
        receive
            {cache_invalidation_watermark, _, <<1:128>>, 0, _, _} -> ok
        after 1000 -> error(missing_coverage_watermark)
        end,
        receive
            {cache_snapshot_cut, _, Round, <<1:128>>, 0, 0, _, _, Candidate, Certificate} ->
                ?assertEqual({ok, [{Source, <<7:128>>, 0, 7}]}, Candidate),
                ?assertEqual(
                    imboy_cache_sync_coverage:source_certificate(
                        0, erlang:system_time(millisecond)
                    ),
                    Certificate
                )
        after 1000 -> error(missing_captured_coverage_cut)
        end,
        receive
            {cache_snapshot_done, _, Round, 10, 2} -> ok
        after 1000 -> error(missing_coverage_done)
        end
    end).

full_cut_budget_includes_header_and_sender_fails_explicitly_test() ->
    fixture(fun(_, _, _) ->
        Round = <<96:96>>,
        Base =
            {cache_snapshot_cut, node(), Round, <<1:128>>, 0, 0,
                imboy_cache_sync_proto:config_hash(), imboy_cache_sync_proto:registry_hash()},
        {Vector, F} = over_budget_cut(Base),
        ?assert(imboy_cache_policy:within_meta_budget(Vector)),
        ?assertNot(imboy_cache_policy:within_meta_budget(F)),
        ?assertNot(imboy_cache_sync_proto:valid_snapshot_cut(F)),
        Cursor = #{application_coverage => {ok, Vector}, watermark => 0},
        ?assertError(
            snapshot_cut_invalid_or_over_budget,
            imboy_cache_sync_proto:send_snapshot_completion(self(), Round, 1, [], Cursor)
        ),
        receive
            {cache_snapshot_done, _, Round, _, _} -> error(over_budget_cut_completed)
        after 0 -> ok
        end
    end).

over_budget_cut(Base) ->
    Origins = lists:sort([
        list_to_atom(
            "coverage_budget_" ++ integer_to_list(I) ++
                "_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        )
     || I <- lists:seq(1, 32)
    ]),
    Candidates = [
        {[{N, <<9:128>>, 0, 1 bsl Bits} || N <- Origins], Bits}
     || Bits <- lists:seq(0, 600, 8)
    ],
    [{Vector, _} | _] = [
        {V, Bits}
     || {V, Bits} <- Candidates,
        imboy_cache_policy:within_meta_budget(V),
        not imboy_cache_policy:within_meta_budget(erlang:append_element(Base, {ok, V}))
    ],
    {Vector, erlang:append_element(Base, {ok, Vector})}.

event_revision_cannot_claim_another_origin_test() ->
    fixture(fun(Source, _, _) ->
        Key = {coverage_wire_test, mismatched_origin},
        E = (imboy_cache_sync_proto:event(
            set,
            Key,
            value,
            erlang:system_time(millisecond) + 60000,
            {1, coverage_other@synthetic},
            1,
            <<7:128>>
        ))#{
            origin_node => Source
        },
        ?assertEqual({error, revision_origin_mismatch}, imboy_cache_sync_proto:validate(E)),
        Before = lists:sort(ets:tab2list(imboy_cache_meta)),
        ?assertEqual(
            {reject, revision_origin_mismatch},
            imboy_cache_sync_apply:apply_v2(
                E,
                #state{boot_id = <<1:128>>, status = ready}
            )
        ),
        ?assertEqual(Before, lists:sort(ets:tab2list(imboy_cache_meta)))
    end).

manifest_summary_binds_payload_origin_kind_and_deadline_test() ->
    Exp = erlang:system_time(millisecond) + 60000,
    Entry = {{coverage_wire_test, manifest}, value, Exp, {7, node()}},
    Origin = {node(), <<7:128>>, 3},
    Wire = {with_origin, Origin, Entry},
    {live, K, Rev, Exp, Origin, Hash, BodyHash} = imboy_cache_policy:snapshot_manifest_entry(Wire),
    ?assertEqual(crypto:hash(sha256, term_to_binary(value)), BodyHash),
    ?assertEqual(element(1, Entry), K),
    ?assertEqual(element(4, Entry), Rev),
    ?assertEqual(crypto:hash(sha256, term_to_binary(Wire)), Hash),
    ?assertNotEqual(
        Hash,
        element(
            6,
            imboy_cache_policy:snapshot_manifest_entry(
                {with_origin, Origin, setelement(2, Entry, changed)}
            )
        )
    ),
    ?assertEqual(unknown, element(5, imboy_cache_policy:snapshot_manifest_entry(Entry))),
    [
        ?assertEqual(
            Kind,
            element(
                1,
                imboy_cache_policy:snapshot_manifest_entry(
                    {Kind, K, undefined, Exp, Rev}
                )
            )
        )
     || Kind <- [deleted, expired, unavailable]
    ].

proof_preparation_crossing_deadline_cannot_publish_floor_or_ready_test() ->
    fixture(fun(Source, R, Base) ->
        Cut = erlang:append_element(
            erlang:append_element(Base, {ok, []}),
            {0, crypto:hash(sha256, term_to_binary([]))}
        ),
        {ok, R0} = imboy_cache_sync_recover:accept_source_cut(Cut, R),
        {ok, Complete} = imboy_cache_sync_snapshot:handle_done(
            Source, maps:get(round, R), 0, 0, R0
        ),
        meck:new(imboy_cache_sync_proto, [passthrough, no_link]),
        meck:expect(imboy_cache_sync_proto, peer_epoch, 1, fun(_) ->
            timer:sleep(50),
            0
        end),
        try
            Tight = Complete#{deadline_ms := erlang:system_time(millisecond) + 30},
            ?assertMatch({error, _}, imboy_cache_sync_proto:commit_snapshot_floors(Tight)),
            ?assertEqual([], ets:match_object(imboy_cache_meta, {{snapshot_seq_floor, '_'}, '_'})),
            ?assertNotEqual([{status, ready}], ets:lookup(imboy_cache_meta, status))
        after
            meck:unload(imboy_cache_sync_proto)
        end
    end).

certified_cut_only_stitches_contiguous_successful_delta_receipts_test() ->
    fixture(fun(Source, R, Base) ->
        Boot = <<7:128>>,
        Cut = erlang:append_element(
            erlang:append_element(setelement(6, Base, 7), {ok, []}),
            {0, crypto:hash(sha256, term_to_binary([]))}
        ),
        P = maps:get(Source, maps:get(peers, R)),
        R0 = R#{
            peers := #{Source => P#{source_cut => Cut}},
            delta_receipts => #{{Source, Boot, 0, 9} => true}
        },
        ets:insert(imboy_cache_meta, {{seq, {Source, Boot}}, 9}),
        ?assertError(
            incremental_content_gap, imboy_cache_sync_coverage:verify_current_prefixes(R0)
        ),
        R1 = R0#{delta_receipts := #{{Source, Boot, 0, 8} => true, {Source, Boot, 0, 9} => true}},
        ?assertEqual(ok, imboy_cache_sync_coverage:verify_current_prefixes(R1)),
        ?assertEqual(#{{Source, Boot} => 9}, imboy_cache_sync_coverage:certified_prefixes(R1))
    end).

manifest_success_and_rejected_pages_do_not_confuse_receipts_test() ->
    fixture(fun(Source, R, _) ->
        {ok, Cache} = depcache:start_link(imboy_cache, #{memory_max => 64}),
        unlink(Cache),
        meck:new(depcache, [passthrough, no_link]),
        meck:expect(depcache, set, 5, fun(_, _, _, _, _) -> ok end),
        meck:expect(depcache, get, 2, fun(_, _) -> {ok, value} end),
        try
            E = {
                {coverage_wire_test, manifest_receipt},
                value,
                erlang:system_time(millisecond) + 60000,
                {7, Source}
            },
            Ref = maps:get(round, R),
            Page = [E],
            D = imboy_cache_sync_snapshot:page_digest(Ref, 1, Page),
            {ok, Next} = imboy_cache_sync_snapshot:handle_page(Source, Ref, 1, Page, D, true, R),
            P = maps:get(Source, maps:get(peers, Next)),
            ?assertEqual(
                [{snapshot_page_peak, {length(Page), erlang:external_size(Page)}}],
                ets:lookup(imboy_cache_meta, snapshot_page_peak)
            ),
            ?assertEqual(
                [{1, [imboy_cache_policy:snapshot_manifest_entry(E)]}], maps:get(manifest, P)
            ),
            ?assertEqual(1, maps:get(manifest_items, Next)),
            ?assertMatch(
                {abort, page_gap, Next},
                imboy_cache_sync_snapshot:handle_page(Source, Ref, 1, Page, D, true, Next)
            ),
            Before = lists:sort(ets:tab2list(imboy_cache_meta)),
            LimitItems = imboy_cache_policy:n_max() + imboy_cache_policy:t_max(),
            LimitBytes = imboy_cache_policy:n_max_bytes() + imboy_cache_policy:t_max_bytes(),
            [
                begin
                    Limited = R#{Field => Limit},
                    ?assertEqual(
                        {abort, Reason, Limited},
                        imboy_cache_sync_snapshot:handle_page(
                            Source, Ref, 1, Page, D, true, Limited
                        )
                    )
                end
             || {Field, Limit, Reason} <- [
                    {manifest_items, LimitItems, snapshot_manifest_items_overflow},
                    {manifest_bytes, LimitBytes, snapshot_manifest_bytes_overflow}
                ]
            ],
            ?assertEqual(Before, lists:sort(ets:tab2list(imboy_cache_meta))),
            ?assertEqual(1, meck:num_calls(depcache, set, 5)),
            Charge = 7 + erlang:external_size({1, [imboy_cache_policy:snapshot_manifest_entry(E)]}),
            Exact = R#{manifest_items => LimitItems - 1, manifest_bytes => LimitBytes - Charge},
            {ok, ExactNext} = imboy_cache_sync_snapshot:handle_page(
                Source, Ref, 1, Page, D, true, Exact
            ),
            ?assertEqual(LimitItems, maps:get(manifest_items, ExactNext)),
            ?assertEqual(LimitBytes, maps:get(manifest_bytes, ExactNext)),
            E2 = setelement(1, E, {coverage_wire_test, manifest_receipt_2}),
            LargerPage = [E, E2],
            LargerDigest = imboy_cache_sync_snapshot:page_digest(Ref, 1, LargerPage),
            {ok, _} = imboy_cache_sync_snapshot:handle_page(
                Source, Ref, 1, LargerPage, LargerDigest, true, R
            ),
            ?assertEqual(
                [{snapshot_page_peak, {length(LargerPage), erlang:external_size(LargerPage)}}],
                ets:lookup(imboy_cache_meta, snapshot_page_peak)
            )
        after
            gen_server:stop(Cache),
            meck:unload(depcache)
        end
    end).

source_cut_cannot_precede_received_own_origin_annotation_test() ->
    fixture(fun(Source, R, Cut) ->
        meck:new(depcache, [passthrough, no_link]),
        meck:expect(depcache, set, 5, fun(_, _, _, _, _) -> ok end),
        try
            Entry =
                {with_origin, {Source, <<7:128>>, 7}, {
                    {coverage_wire_test, own_cut},
                    value,
                    erlang:system_time(millisecond) + 60000,
                    {10, Source}
                }},
            Ref = maps:get(round, R),
            Page = [Entry],
            {ok, Next} = imboy_cache_sync_snapshot:handle_page(
                Source,
                Ref,
                1,
                Page,
                imboy_cache_sync_snapshot:page_digest(Ref, 1, Page),
                false,
                R
            ),
            Before = lists:sort(ets:tab2list(imboy_cache_meta)),
            ?assertEqual(
                {abort, regressed_source_cut, Next},
                imboy_cache_sync_recover:accept_source_cut(Cut, Next)
            ),
            ?assertEqual(Before, lists:sort(ets:tab2list(imboy_cache_meta))),
            ets:insert(imboy_cache_meta, {{seq, {Source, <<7:128>>}}, 7}),
            ?assertMatch(
                {ok, _},
                imboy_cache_sync_recover:accept_source_cut(
                    setelement(6, Cut, 7), Next
                )
            )
        after
            meck:unload(depcache)
        end
    end).

own_manifest_sequence_ignores_foreign_old_boot_and_unknown_test() ->
    Source = own_manifest@synthetic,
    Boot = <<7:128>>,
    Row = {live, key, {9999, Source}, 0, {Source, Boot, 7}, <<0:256>>},
    Other = [
        setelement(5, Row, O)
     || O <- [
            {foreign_manifest@synthetic, Boot, 100}, {Source, <<8:128>>, 100}, unknown
        ]
    ],
    ?assertEqual(0, imboy_cache_policy:snapshot_manifest_own_seq(Other, {Source, Boot}, 0)),
    ?assertEqual(7, imboy_cache_policy:snapshot_manifest_own_seq([Row | Other], {Source, Boot}, 0)),
    ?assertEqual(9, imboy_cache_policy:snapshot_manifest_own_seq([Row], {Source, Boot}, 9)).

stored_cut(R, Source) -> maps:get(source_cut, maps:get(Source, maps:get(peers, R))).
fixture(Fun) ->
    OldCfg = application:get_env(imboy, cache_replication),
    OldRegistry = application:get_env(imboy, cache_registry_overrides),
    ets:new(imboy_cache_meta, [named_table, public, set]),
    Source = coverage_source@synthetic,
    ets:insert(imboy_cache_meta, [
        {sync_epoch, 0},
        {node_boot, {node(), <<1:128>>}},
        {sync_origin_seq, 0},
        {{peer_incarnation, Source}, <<7:128>>}
    ]),
    application:set_env(imboy, cache_replication, #{whitelist => [coverage_wire_test]}),
    application:set_env(imboy, cache_registry_overrides, #{coverage_wire_test => {replicate, false}}),
    R0 = imboy_cache_sync_snapshot:new_round(0, erlang:system_time(millisecond) + 60000),
    R1 = imboy_cache_sync_snapshot:mark_requested(
        imboy_cache_sync_snapshot:authorize_peer(
            imboy_cache_sync_snapshot:expect_peer(R0, Source), Source
        ),
        Source
    ),
    Peers = maps:get(peers, R1),
    Peer = maps:get(Source, Peers),
    R = R1#{
        cohort_epoch => 0,
        retained_obligations := [],
        peers := Peers#{
            Source := Peer#{source_pid => self(), origin_target => {Source, <<7:128>>, 0, 0}}
        }
    },
    meck:new(imboy_cache_sync, [passthrough, no_link]),
    meck:expect(imboy_cache_sync, syn_scope_members, 0, fun() -> [self()] end),
    Base =
        {cache_snapshot_cut, Source, maps:get(round, R), <<7:128>>, 0, 0,
            imboy_cache_sync_proto:config_hash(), imboy_cache_sync_proto:registry_hash()},
    try
        Fun(Source, R, Base)
    after
        drain(),
        meck:unload(imboy_cache_sync),
        ets:delete(imboy_cache_meta),
        restore(cache_replication, OldCfg),
        restore(cache_registry_overrides, OldRegistry)
    end.
drain() ->
    receive
        {cache_snapshot_cut, _, _, _, _, _, _, _, _, _} -> drain();
        {cache_snapshot_page, _, _, _, _, _, _} -> drain();
        {cache_snapshot_cut, _, _, _, _, _, _, _, _} -> drain();
        {cache_invalidation_watermark, _, _, _, _, _} -> drain();
        {cache_snapshot_done, _, _, _, _} -> drain();
        {cache_snapshot_abort, _, _, _} -> drain()
    after 0 -> ok
    end.
restore(K, undefined) -> application:unset_env(imboy, K);
restore(K, {ok, V}) -> application:set_env(imboy, K, V).
