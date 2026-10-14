-module(imboy_cache_origin_target_tests).
-include_lib("eunit/include/eunit.hrl").
-include("imboy_cache_sync_state.hrl").

report_target_uses_origin_sequence_and_exact_round_test() ->
    fixture(fun(State, Round) ->
        F = report(Round, <<7:128>>, 0, 5000, 7),
        ?assert(imboy_cache_sync_proto:valid_version_report(F)),
        {noreply, Next} = imboy_cache_sync:handle_info(F, State),
        ?assertEqual({node(), <<7:128>>, 0, 7}, target(Next)),
        ?assertEqual(5000, Next#state.counter),
        ?assertEqual(recovering, Next#state.status),
        ?assertNot(maps:get(authority, peer(Next))),
        ?assertEqual([], ets:lookup(imboy_cache_meta, data_epoch_ceiling)),
        Before = lists:sort(ets:tab2list(imboy_cache_meta)),
        ?assertEqual(
            {noreply, Next}, imboy_cache_sync:handle_info(setelement(11, F, <<99:96>>), Next)
        ),
        ?assertEqual(Before, lists:sort(ets:tab2list(imboy_cache_meta)))
    end).

first_origin_target_does_not_move_when_reports_advance_or_regress_test() ->
    fixture(fun(State, Round) ->
        {noreply, First} = imboy_cache_sync:handle_info(
            report(Round, <<7:128>>, 0, 5000, 7), State
        ),
        {noreply, Advanced} = imboy_cache_sync:handle_info(
            report(Round, <<7:128>>, 0, 6000, 9), First
        ),
        {noreply, Regressed} = imboy_cache_sync:handle_info(
            report(Round, <<7:128>>, 0, 4000, 3), Advanced
        ),
        ?assertEqual({node(), <<7:128>>, 0, 7}, target(Advanced)),
        ?assertEqual(target(Advanced), target(Regressed)),
        ?assertEqual(6000, Regressed#state.counter)
    end).

legacy_and_unbound_reports_do_not_fabricate_zero_target_test() ->
    fixture(fun(State, Round) ->
        Full = report(Round, <<7:128>>, 0, 5000, 0),
        Legacy = imboy_cache_policy:version_report_base(Full),
        Unbound = erlang:delete_element(11, Full),
        ?assertEqual(unknown, imboy_cache_policy:version_report_target(Legacy)),
        ?assertEqual(unknown, imboy_cache_policy:version_report_target(Unbound)),
        {noreply, S1} = imboy_cache_sync:handle_info(Legacy, State),
        ?assertEqual(unknown, target(S1)),
        {noreply, S2} = imboy_cache_sync:handle_info(Unbound, S1),
        ?assertEqual(unknown, target(S2)),
        {noreply, S3} = imboy_cache_sync:handle_info(Full, S2),
        ?assertEqual({node(), <<7:128>>, 0, 0}, target(S3))
    end).

nonready_source_boot_or_epoch_change_aborts_old_target_test() ->
    lists:foreach(
        fun({Boot, Epoch}) ->
            fixture(fun(State, Round) ->
                {noreply, First} = imboy_cache_sync:handle_info(
                    report(Round, <<7:128>>, 0, 5000, 7), State
                ),
                {noreply, Changed} = imboy_cache_sync:handle_info(
                    report(Round, Boot, Epoch, 6000, 8), First
                ),
                try
                    ?assertNotEqual(Round, maps:get(round, Changed#state.snapshot_round)),
                    ?assertEqual(#{}, maps:get(peers, Changed#state.snapshot_round)),
                    ?assertNotEqual(ready, Changed#state.status)
                after
                    cancel(Changed#state.recovery_timer)
                end
            end)
        end,
        [{<<8:128>>, 0}, {<<7:128>>, 1}]
    ).

unbound_report_can_revoke_but_cannot_replace_frozen_target_test() ->
    fixture(fun(State, Round) ->
        {noreply, First} = imboy_cache_sync:handle_info(
            report(Round, <<7:128>>, 0, 5000, 7), State
        ),
        Unbound = erlang:delete_element(11, report(Round, <<8:128>>, 0, 6000, 8)),
        {noreply, Changed} = imboy_cache_sync:handle_info(Unbound, First),
        try
            ?assertNotEqual(Round, maps:get(round, Changed#state.snapshot_round)),
            ?assertEqual(#{}, maps:get(peers, Changed#state.snapshot_round))
        after
            cancel(Changed#state.recovery_timer)
        end
    end).

invalid_sequence_old_pid_and_budget_rejection_leave_no_target_test() ->
    fixture(fun(State, Round) ->
        F = report(Round, <<7:128>>, 0, 5000, 7),
        [
            ?assertNot(imboy_cache_sync_proto:valid_version_report(setelement(10, F, V)))
         || V <- [-1, bad_seq, 1 bsl 50000]
        ],
        Before = lists:sort(ets:tab2list(imboy_cache_meta)),
        {noreply, State} = imboy_cache_sync:handle_info(setelement(10, F, -1), State),
        meck:expect(syn, members, fun(_, _) -> [] end),
        {noreply, State} = imboy_cache_sync:handle_info(F, State),
        ?assertEqual(Before, lists:sort(ets:tab2list(imboy_cache_meta))),
        meck:expect(syn, members, fun(_, _) -> [{self(), #{}}] end),
        application:set_env(imboy, cache_replication, #{peer_max => 1}),
        ets:insert(imboy_cache_meta, {{peer_incarnation, other_budget@synthetic}, <<9:128>>}),
        {noreply, Rejected} = imboy_cache_sync:handle_info(
            report(Round, <<7:128>>, 0, 5000, 7), State
        ),
        try
            ?assertEqual([], ets:lookup(imboy_cache_meta, {peer_incarnation, node()})),
            ?assertEqual(undefined, Rejected#state.snapshot_round)
        after
            cancel(Rejected#state.recovery_timer)
        end
    end).

recovery_request_echoes_round_and_true_sequence_test() ->
    fixture(fun(State, Round) ->
        Request = imboy_cache_sync_proto:version_request_frame(self(), Round),
        Sender = State#state{counter = 5000, origin_seq = 7, boot_id = <<7:128>>, status = ready},
        Census = imboy_cache_sync_coverage:content_census(),
        {noreply, Sender} = imboy_cache_sync:handle_info(Request, Sender),
        receive
            {cache_version_report, N, 5000, 0, _, _, <<7:128>>, true, Pid, 7, Round, Census} = F ->
                ?assertEqual(node(), N),
                ?assertEqual(self(), Pid),
                ?assert(imboy_cache_sync_proto:valid_version_report(F))
        after 1000 -> error(missing_round_bound_report)
        end,
        receive
            {cache_invalidation_watermark, _, <<7:128>>, 7, _, _} -> ok
        after 1000 -> error(missing_report_watermark)
        end
    end).

report(Round, Boot, Epoch, Counter, Seq) ->
    {cache_version_report, node(), Counter, Epoch, imboy_cache_sync_proto:config_hash(),
        imboy_cache_sync_proto:registry_hash(), Boot, false, self(), Seq, Round}.
peer(State) -> maps:get(node(), maps:get(peers, State#state.snapshot_round)).
target(State) -> maps:get(origin_target, peer(State), unknown).
cancel(Ref) when is_reference(Ref) ->
    erlang:cancel_timer(Ref),
    receive
        {timeout, Ref, cache_recovery} -> ok
    after 0 -> ok
    end;
cancel(_) ->
    ok.

fixture(Fun) ->
    pure_mock_fixture:run(fun() -> fixture_checks(Fun) end).

fixture_checks(Fun) ->
    OldCfg = application:get_env(imboy, cache_replication),
    OldFenced = application:get_env(imboy, cache_fenced_namespaces),
    application:set_env(imboy, cache_replication, #{}),
    application:set_env(imboy, cache_fenced_namespaces, []),
    ets:new(imboy_cache_meta, [named_table, public, set]),
    ets:insert(imboy_cache_meta, [{sync_epoch, 0}, {sync_counter, 0}]),
    meck:new(syn, [non_strict, no_link]),
    meck:expect(syn, members, fun(_, _) -> [{self(), #{}}] end),
    R = imboy_cache_sync_snapshot:expect_peer(
        imboy_cache_sync_snapshot:new_round(
            0,
            erlang:system_time(millisecond) + 60000
        ),
        node()
    ),
    State = #state{
        status = recovering,
        boot_id = <<1:128>>,
        snapshot_round = R,
        recover_deadline_ms = maps:get(deadline_ms, R)
    },
    try
        Fun(State, maps:get(round, R))
    after
        meck:unload(syn),
        ets:delete(imboy_cache_meta),
        imboy_cache_source_cut_tests:restore_envs([
            {cache_replication, OldCfg},
            {cache_fenced_namespaces, OldFenced}
        ])
    end.
