%%% 来源自身序列确认；共享 done fixture 显式走真实 cut 校验，不绕过完成门禁。
-module(imboy_cache_source_cut_tests).
-include_lib("eunit/include/eunit.hrl").
-include("imboy_cache_sync_state.hrl").
-export([done/5, drain/0, fixture_identity/3, restore_envs/1, requested_peer/2]).
-define(META, imboy_cache_meta).

nonready_report_does_not_replace_accepted_data_floor_test() ->
    lists:foreach(
        fun(Floor) ->
            fixture(fun(_, _, _) ->
                High = 'nonready_high@synthetic',
                Low = 'ready_low@synthetic',
                D = erlang:system_time(millisecond) + 60000,
                R0 = imboy_cache_sync_snapshot:new_round(0, D),
                R = imboy_cache_sync_snapshot:expect_peer(
                    imboy_cache_sync_snapshot:expect_peer(R0, High), Low
                ),
                ets:insert(?META, {data_epoch_ceiling, Floor}),
                S = #state{
                    boot_id = <<8:128>>,
                    status = recovering,
                    recover_deadline_ms = D,
                    snapshot_round = R
                },
                H = imboy_cache_sync_proto:config_hash(),
                G = imboy_cache_sync_proto:registry_hash(),
                S1 = imboy_cache_sync_recover:handle_version_report(
                    High, 0, 2, H, G, <<2:128>>, false, self(), S
                ),
                S2 = imboy_cache_sync_recover:handle_version_report(
                    Low, 0, 0, H, G, <<1:128>>, true, self(), S1
                ),
                Final = S2#state.snapshot_round,
                RoundId = maps:get(round, R),
                receive
                    {cache_snapshot_request, Self, RoundId} when Self =:= self() ->
                        ?assertEqual(0, Floor)
                after 0 ->
                    ?assertEqual(2, Floor)
                end,
                ?assertEqual(Floor, imboy_cache_sync_apply:required_epoch()),
                ?assertEqual(Floor =:= 0, maps:is_key(cohort_epoch, Final)),
                ?assertEqual(
                    Floor =:= 0,
                    maps:get(requested, maps:get(Low, maps:get(peers, Final)))
                ),
                ?assertEqual(recovering, S2#state.status)
            end)
        end,
        [0, 2]
    ).

ready_transition_replaces_existing_reconcile_timer_test() ->
    fixture(fun(_, _, _) ->
        meck:new(syn, [non_strict, no_link]),
        meck:expect(syn, members, fun(_, _) -> [] end),
        Old = erlang:start_timer(60000, self(), reconcile_tick),
        try
            {noreply, Ready} = imboy_cache_sync_recover:become_ready(
                #state{boot_id = <<8:128>>, status = recovering, reconcile_timer = Old}
            ),
            New = Ready#state.reconcile_timer,
            try
                ?assertEqual(ready, Ready#state.status),
                ?assertEqual(false, erlang:read_timer(Old)),
                ?assert(is_integer(erlang:read_timer(New))),
                ?assertEqual(
                    {noreply, Ready},
                    imboy_cache_sync:handle_info({timeout, Old, reconcile_tick}, Ready)
                )
            after
                erlang:cancel_timer(New)
            end
        after
            erlang:cancel_timer(Old),
            meck:unload(syn)
        end
    end).

reconcile_timer_does_not_multiply_test() ->
    S = #state{boot_id = <<8:128>>, status = recovering},
    {noreply, First} = imboy_cache_sync:handle_info(reconcile_tick, S),
    R1 = First#state.reconcile_timer,
    {noreply, Second} = imboy_cache_sync:handle_info(reconcile_tick, First),
    R2 = Second#state.reconcile_timer,
    try
        ?assertEqual(false, erlang:read_timer(R1)),
        ?assert(is_integer(erlang:read_timer(R2))),
        ?assertEqual(
            {noreply, Second}, imboy_cache_sync:handle_info({timeout, R1, reconcile_tick}, Second)
        ),
        %% 模拟已到期入邮箱：当前引用对应的运行时 timer 已不存在。
        erlang:cancel_timer(R2),
        {noreply, Third} = imboy_cache_sync:handle_info({timeout, R2, reconcile_tick}, Second),
        R3 = Third#state.reconcile_timer,
        try
            ?assertEqual(false, erlang:read_timer(R2)),
            ?assert(is_integer(erlang:read_timer(R3))),
            ?assertNotEqual(R2, R3)
        after
            erlang:cancel_timer(R3)
        end
    after
        erlang:cancel_timer(R2)
    end.

report_from_unregistered_process_cannot_replace_incarnation_test() ->
    fixture(fun(_Peer, _R, _F) ->
        Member = spawn(fun() ->
            receive
                stop -> ok
            end
        end),
        meck:new(syn, [non_strict, no_link]),
        try
            meck:expect(syn, members, fun(imboy_cache, dsync_handler) -> [{Member, #{}}] end),
            ets:insert(?META, {{peer_incarnation, node()}, <<7:128>>}),
            R = imboy_cache_sync_snapshot:expect_peer(
                imboy_cache_sync_snapshot:new_round(0, erlang:system_time(millisecond) + 60000),
                node()
            ),
            S = #state{boot_id = <<8:128>>, status = recovering, snapshot_round = R},
            Report =
                {cache_version_report, node(), 0, 0, imboy_cache_sync_proto:config_hash(),
                    imboy_cache_sync_proto:registry_hash(), <<9:128>>, false, self()},
            Before = lists:sort(ets:tab2list(?META)),
            ?assertEqual({noreply, S}, imboy_cache_sync:handle_info(Report, S)),
            ?assertEqual(Before, lists:sort(ets:tab2list(?META))),
            {noreply, Accepted} = imboy_cache_sync:handle_info(setelement(9, Report, Member), S),
            ?assert(
                maps:get(reported, maps:get(node(), maps:get(peers, Accepted#state.snapshot_round)))
            ),
            ?assertEqual(
                [{{peer_incarnation, node()}, <<9:128>>}],
                ets:lookup(?META, {peer_incarnation, node()})
            )
        after
            meck:unload(syn),
            Member ! stop
        end
    end).

drain() ->
    receive
        {cache_snapshot_cut, _, _, _, _, _, _, _, _, _} -> drain();
        {cache_snapshot_cut, _, _, _, _, _, _, _, _} -> drain();
        {cache_snapshot_cut, _, _, _, _, _, _, _} -> drain();
        {cache_snapshot_abort, _, _, _} -> drain()
    after 0 -> ok
    end.

retired_event_cannot_register_old_boot_again_test() ->
    fixture(fun(Peer, _R, _F) ->
        Old = <<7:128>>,
        New = <<9:128>>,
        changed = imboy_cache_sync_proto:register_incarnation(Peer, New),
        ets:insert(?META, {
            {grace_incarnation, {Peer, Old}},
            erlang:system_time(millisecond) - imboy_cache_policy:grace_window_ms()
        }),
        ok = imboy_cache_sync_proto:prune_grace(erlang:system_time(millisecond)),
        E = (imboy_cache_sync_proto:event(
            set,
            {replay_test, removed},
            old,
            erlang:system_time(millisecond) + 60000,
            {1, Peer},
            1,
            Old
        ))#{
            origin_node => Peer
        },
        Before = lists:sort(ets:tab2list(?META)),
        ?assertEqual(
            {error, unknown_incarnation},
            imboy_cache_sync_proto:check_consistency(E, <<8:128>>)
        ),
        ?assertEqual(Before, lists:sort(ets:tab2list(?META))),
        ?assertEqual(ok, imboy_cache_sync_proto:check_incarnation(Peer, New)),
        ?assertEqual(
            {error, unknown_incarnation},
            imboy_cache_sync_proto:check_incarnation('unknown_peer@synthetic', Old)
        )
    end).

identity_report_can_advance_epoch_but_cold_target_stays_strict_test() ->
    fixture(fun(Peer, R, _F) ->
        S = #state{
            boot_id = <<8:128>>,
            status = recovering,
            requires_authority = true,
            snapshot_round = R
        },
        Report = fun(State) ->
            imboy_cache_sync_recover:handle_version_report(
                Peer,
                0,
                1,
                imboy_cache_sync_proto:config_hash(),
                imboy_cache_sync_proto:registry_hash(),
                <<9:128>>,
                false,
                self(),
                State
            )
        end,
        Cold = Report(S),
        cancel_timer(Cold),
        ?assertEqual(
            [{{peer_incarnation, Peer}, <<7:128>>}],
            ets:lookup(?META, {peer_incarnation, Peer})
        ),
        ?assertEqual([], ets:lookup(?META, {peer_epoch, Peer})),
        Identity = Report(S#state{identity_recovery = true}),
        cancel_timer(Identity),
        ?assertEqual(
            [{{peer_incarnation, Peer}, <<9:128>>}],
            ets:lookup(?META, {peer_incarnation, Peer})
        ),
        ?assertEqual([{{peer_epoch, Peer}, 1}], ets:lookup(?META, {peer_epoch, Peer})),
        ?assertEqual(0, imboy_cache_sync_proto:epoch())
    end).

cancel_timer(#state{recovery_timer = Ref}) when is_reference(Ref) -> erlang:cancel_timer(Ref);
cancel_timer(_) -> ok.

done(Peer, Round, Watermark, Total, R) ->
    Peers = maps:get(peers, R),
    P = maps:get(Peer, Peers),
    Boot =
        case maps:get(boot, P, undefined) of
            undefined -> <<7:128>>;
            B -> B
        end,
    Epoch = maps:get(source_epoch, P, imboy_cache_sync_proto:epoch()),
    %% 合法合成来源 fixture，不能用于未知/换 Boot/缺口负例。
    fixture_identity(Peer, Boot, Epoch),
    {ok, Manifest} = imboy_cache_policy:final_snapshot_manifest(P),
    %% CutSeq 取来源自身声明完整点：自身序列计数器（由真实 apply_v2/
    %% note_applied_sequence 推进）与刚发送页集所证明前缀的较大者——真实发送端
    %% 的计数器必然覆盖其发出的页前缀。origin_target 经产品入口以同一声明值独立
    %% 冻结（绑定回合与身份），不从接收侧状态反推，也不清空保留义务。
    Seq0 =
        case ets:lookup(?META, {seq, {Peer, Boot}}) of
            [{_, N}] -> N;
            [] -> 0
        end,
    Seq = erlang:max(
        Seq0, imboy_cache_policy:snapshot_manifest_own_seq(maps:values(Manifest), {Peer, Boot}, 0)
    ),
    {ok, RT} = imboy_cache_sync_proto:record_origin_target(
        {Peer, Boot, Epoch, Seq, maps:get(round, R)}, R
    ),
    PeersT = maps:get(peers, RT),
    PT = maps:get(Peer, PeersT),
    %% 不清空 retained_obligations（由 recovery_round 的真实 census 冻结）；
    %% cohort_epoch 为 fixture 声明的同代分类（单代来源，与 select_cohort 一致）。
    R0 = RT#{
        cohort_epoch => Epoch,
        peers := PeersT#{
            Peer := PT#{
                boot => Boot,
                source_epoch => Epoch,
                source_pid => self()
            }
        }
    },
    %% 证书证明来源声明已发送的内容，接收端按页集核验。
    Certificate = imboy_cache_sync_coverage:manifest_certificate(Manifest),
    F =
        {cache_snapshot_cut, Peer, maps:get(round, R0), Boot, Epoch, Seq,
            imboy_cache_sync_proto:config_hash(), imboy_cache_sync_proto:registry_hash(), {ok, []},
            Certificate},
    {ok, R1} = imboy_cache_sync_recover:accept_source_cut(F, R0),
    imboy_cache_sync_snapshot:handle_done(Peer, Round, Watermark, Total, R1).

incomplete_round_cannot_fall_back_to_single_node_ready_test() ->
    fixture(fun(_Peer, R, _F) ->
        S = #state{
            boot_id = <<8:128>>,
            status = discovering,
            snapshot_round = R,
            recover_deadline_ms = maps:get(deadline_ms, R)
        },
        {noreply, Next} = imboy_cache_sync_recover:become_ready(S),
        ?assertEqual(recovering, Next#state.status),
        ?assert(Next#state.requires_authority),
        ?assertEqual(S#state.recover_deadline_ms, Next#state.recover_deadline_ms),
        ?assertEqual([], ets:match_object(?META, {{snapshot_seq_floor, '_'}, '_'})),
        erlang:cancel_timer(Next#state.recovery_timer),
        {noreply, Required} = imboy_cache_sync_recover:become_ready(
            S#state{snapshot_round = undefined, requires_authority = true}
        ),
        ?assertEqual(recovering, Required#state.status),
        ?assert(Required#state.requires_authority),
        erlang:cancel_timer(Required#state.recovery_timer)
    end).

completed_round_below_data_epoch_cannot_become_ready_test() ->
    fixture(fun(Peer, R, _F) ->
        {ok, Complete} = done(Peer, maps:get(round, R), 0, 0, R),
        ?assert(imboy_cache_sync_snapshot:round_complete(Complete)),
        ets:insert(?META, {data_epoch_ceiling, 1}),
        S = #state{
            boot_id = <<8:128>>,
            status = recovering,
            snapshot_round = Complete,
            recover_deadline_ms = maps:get(deadline_ms, R)
        },
        {noreply, Next} = imboy_cache_sync_recover:become_ready(S),
        try
            ?assertEqual(recovering, Next#state.status),
            ?assert(Next#state.requires_authority),
            ?assertEqual(S#state.recover_deadline_ms, Next#state.recover_deadline_ms),
            ?assertEqual([{status, recovering}], ets:lookup(?META, status)),
            ?assertEqual([], ets:match_object(?META, {{snapshot_seq_floor, '_'}, '_'}))
        after
            erlang:cancel_timer(Next#state.recovery_timer)
        end
    end).

completed_snapshot_rejects_old_events_without_per_key_metadata_test() ->
    fixture(fun(Peer, R, _F) ->
        Boot = <<7:128>>,
        ets:insert(?META, {{seq, {Peer, Boot}}, 2}),
        ?assertEqual({error, {badmatch, false}}, imboy_cache_sync_proto:commit_snapshot_floors(R)),
        ?assertEqual([], ets:match_object(?META, {{snapshot_seq_floor, '_'}, '_'})),
        {ok, Complete} = done(Peer, maps:get(round, R), 2, 0, R),
        ok = imboy_cache_sync_proto:commit_snapshot_floors(Complete),
        Event = (imboy_cache_sync_proto:event(
            set,
            {replay_test, removed},
            old,
            erlang:system_time(millisecond) + 60000,
            {1, Peer},
            1,
            Boot
        ))#{
            origin_node => Peer
        },
        ?assertEqual(
            {error, stale_sequence}, imboy_cache_sync_proto:check_consistency(Event, <<8:128>>)
        ),
        ?assertEqual(
            {error, stale_sequence},
            imboy_cache_sync_proto:check_consistency(Event#{operation => delete}, <<8:128>>)
        ),
        ?assertEqual(
            ok, imboy_cache_sync_proto:check_consistency(Event#{origin_seq => 3}, <<8:128>>)
        ),
        ?assertEqual(
            ok, imboy_cache_sync_proto:check_consistency(Event#{operation => invalidate}, <<8:128>>)
        ),
        ?assertEqual([], ets:lookup(?META, {rev, {replay_test, removed}})),
        changed = imboy_cache_sync_proto:register_incarnation(Peer, <<9:128>>),
        ?assertEqual(
            {error, stale_sequence}, imboy_cache_sync_proto:check_consistency(Event, <<8:128>>)
        ),
        ets:insert(?META, {
            {grace_incarnation, {Peer, Boot}},
            erlang:system_time(millisecond) - imboy_cache_policy:grace_window_ms()
        }),
        ok = imboy_cache_sync_proto:prune_grace(erlang:system_time(millisecond)),
        ?assertEqual([], ets:lookup(?META, {snapshot_seq_floor, {Peer, Boot}}))
    end).

done_requires_current_source_cut_test() ->
    fixture(fun(Peer, R, F) ->
        fixture_identity(Peer, <<7:128>>, 0),
        Round = maps:get(round, R),
        ?assertEqual(
            {abort, missing_source_cut, R},
            imboy_cache_sync_snapshot:handle_done(Peer, Round, 0, 0, R)
        ),
        {ok, R1} = imboy_cache_sync_recover:accept_source_cut(F, R),
        ?assertEqual({ok, R1}, imboy_cache_sync_recover:accept_source_cut(F, R1)),
        {ok, R2} = imboy_cache_sync_snapshot:handle_done(Peer, Round, 0, 0, R1),
        ?assert(imboy_cache_sync_snapshot:round_complete(R2)),
        ?assertMatch(
            {abort, conflicting_source_cut, R1},
            imboy_cache_sync_recover:accept_source_cut(setelement(6, F, 1), R1)
        )
    end).

current_cut_rejects_identity_changes_and_expiry_test() ->
    fixture(fun(Peer, R, F) ->
        fixture_identity(Peer, <<7:128>>, 0),
        [
            ?assertMatch(
                {abort, source_cut_identity_changed, R},
                imboy_cache_sync_recover:accept_source_cut(setelement(Pos, F, V), R)
            )
         || {Pos, V} <- [{4, <<8:128>>}, {5, 1}, {7, <<8:128>>}, {8, <<8:128>>}]
        ],
        ets:delete(?META, {peer_incarnation, Peer}),
        ?assertMatch(
            {abort, source_cut_identity_changed, R},
            imboy_cache_sync_recover:accept_source_cut(F, R)
        ),
        ets:insert(?META, {{peer_incarnation, Peer}, <<7:128>>}),
        Past = R#{deadline_ms := erlang:system_time(millisecond) - 1},
        ?assertMatch(
            {abort, source_cut_identity_changed, Past},
            imboy_cache_sync_recover:accept_source_cut(F, Past)
        )
    end).

cut_cannot_regress_below_frozen_origin_target_test() ->
    fixture(fun(Peer, R, F) ->
        fixture_identity(Peer, <<7:128>>, 0),
        Ps = maps:get(peers, R),
        Targeted = R#{
            peers := Ps#{
                Peer := (maps:get(Peer, Ps))#{
                    origin_target => {Peer, <<7:128>>, 0, 7}
                }
            }
        },
        Before = lists:sort(ets:tab2list(?META)),
        ?assertEqual(
            {abort, regressed_source_cut, Targeted},
            imboy_cache_sync_recover:accept_source_cut(F, Targeted)
        ),
        ?assertEqual(Before, lists:sort(ets:tab2list(?META))),
        ets:insert(?META, {{seq, {Peer, <<7:128>>}}, 7}),
        ?assertMatch(
            {ok, _},
            imboy_cache_sync_recover:accept_source_cut(setelement(6, F, 7), Targeted)
        )
    end).

duplicate_cut_cannot_bypass_new_sequence_floor_test() ->
    fixture(fun(Peer, R, F) ->
        fixture_identity(Peer, <<7:128>>, 0),
        {ok, Confirmed} = imboy_cache_sync_recover:accept_source_cut(F, R),
        ets:insert(?META, {{seq, {Peer, <<7:128>>}}, 1}),
        ?assertEqual(
            {abort, regressed_source_cut, Confirmed},
            imboy_cache_sync_recover:accept_source_cut(F, Confirmed)
        )
    end).

cut_sequence_gap_and_regression_cannot_confirm_test() ->
    fixture(fun(Peer, R, F) ->
        fixture_identity(Peer, <<7:128>>, 0),
        ets:insert(?META, {{seq, {Peer, <<7:128>>}}, 2}),
        ?assertMatch(
            {abort, regressed_source_cut, R}, imboy_cache_sync_recover:accept_source_cut(F, R)
        ),
        ?assertMatch(
            {abort, source_cut_sequence_gap, R},
            imboy_cache_sync_recover:accept_source_cut(setelement(6, F, 3), R)
        ),
        ?assertNot(maps:is_key(source_cut, maps:get(Peer, maps:get(peers, R))))
    end).

old_unrequested_or_unrelated_cut_does_not_confirm_test() ->
    fixture(fun(Peer, R, F) ->
        ?assertEqual(
            {ok, R}, imboy_cache_sync_recover:accept_source_cut(setelement(3, F, <<9:96>>), R)
        ),
        ?assertEqual(
            {ok, R},
            imboy_cache_sync_recover:accept_source_cut(setelement(2, F, 'unrelated@synthetic'), R)
        ),
        Ps = maps:get(peers, R),
        Waiting = R#{peers := Ps#{Peer := (maps:get(Peer, Ps))#{requested := false}}},
        ?assertEqual({ok, Waiting}, imboy_cache_sync_recover:accept_source_cut(F, Waiting)),
        [
            ?assertNot(imboy_cache_sync_proto:valid_snapshot_cut(setelement(Pos, F, V)))
         || {Pos, V} <- [{3, <<0>>}, {4, <<0>>}, {5, -1}, {6, -1}, {6, 1 bsl 40000}]
        ]
    end).

fixture(Fun) ->
    pure_mock_fixture:run(fun() -> fixture_checks(Fun) end).

fixture_checks(Fun) ->
    {ok, _} = application:ensure_all_started(syn),
    ok = syn:add_node_to_scopes([imboy_cache]),
    ok = syn:join(imboy_cache, dsync_handler, self(), #{}),
    ets:new(?META, [named_table, public, set]),
    Peer = 'source_cut@synthetic',
    Boot = <<7:128>>,
    ets:insert(?META, [{sync_epoch, 0}, {{peer_incarnation, Peer}, Boot}]),
    %% recovery_round 以真实 census 冻结保留义务（本 fixture 无 rev 行 → 空表），
    %% 不能由 fixture 清空义务制造可闭合回合。
    R0 = imboy_cache_sync_snapshot:recovery_round(0, erlang:system_time(millisecond) + 60000),
    Requested = imboy_cache_sync_snapshot:mark_requested(
        imboy_cache_sync_snapshot:authorize_peer(
            imboy_cache_sync_snapshot:expect_peer(R0, Peer), Peer
        ),
        Peer
    ),
    Peers = maps:get(peers, Requested),
    P = maps:get(Peer, Peers),
    R = Requested#{peers := Peers#{Peer := P#{source_pid => self()}}},
    F =
        {cache_snapshot_cut, Peer, maps:get(round, R), Boot, 0, 0,
            imboy_cache_sync_proto:config_hash(), imboy_cache_sync_proto:registry_hash()},
    try
        Fun(Peer, R, F)
    after
        _ = syn:leave(imboy_cache, dsync_handler, self()),
        ets:delete(?META)
    end.

%% 仅用于专属测试 VM 的合法握手 fixture，不适用于未知身份负例。
fixture_identity(Peer, Boot, Epoch) ->
    ets:insert(?META, [{{peer_incarnation, Peer}, Boot}, {{peer_epoch, Peer}, Epoch}]).

restore_envs(Original) ->
    lists:foreach(
        fun
            ({K, undefined}) -> application:unset_env(imboy, K);
            ({K, {ok, V}}) -> application:set_env(imboy, K, V)
        end,
        Original
    ).

requested_peer(Round, Peer) ->
    Authorized = imboy_cache_sync_snapshot:authorize_peer(Round, Peer),
    Requested = imboy_cache_sync_snapshot:mark_requested(Authorized, Peer),
    Peers = maps:get(peers, Requested),
    P = maps:get(Peer, Peers),
    Boot = maps:get(boot, P, <<7:128>>),
    Epoch = maps:get(source_epoch, P, imboy_cache_sync_proto:epoch()),
    %% 空来源的零目标同样经产品入口冻结；不动 retained_obligations/cohort_epoch。
    {ok, Targeted} = imboy_cache_sync_proto:record_origin_target(
        {Peer, Boot, Epoch, 0, maps:get(round, Requested)}, Requested
    ),
    PeersT = maps:get(peers, Targeted),
    PT = maps:get(Peer, PeersT),
    Targeted#{
        peers := PeersT#{
            Peer := PT#{
                source_pid => whereis(imboy_cache_sync),
                boot => Boot,
                source_epoch => Epoch
            }
        }
    }.
