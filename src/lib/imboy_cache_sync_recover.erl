%% @doc 同步actor串行调用的发现、恢复屏障、冷恢复及有界清扫引擎。
-module(imboy_cache_sync_recover).
-include("log.hrl").
-include("imboy_cache_sync_state.hrl").
-export([
    discover/1,
    reject_membership/1,
    handle_timeout/1,
    schedule_recovery/2,
    start_cold_recovery/1,
    start_cold_recovery/2,
    start_gap_recovery/1,
    handle_version_report/9,
    handle_version_report/10,
    current_or_new_round/1,
    maybe_complete_round/1,
    sources_current/1,
    abort_round/2,
    check_authority_incarnation/2,
    check_authority_event/2,
    handle_watermark/2,
    handle_snapshot_cut/2,
    accept_source_cut/2,
    guard_data_epoch/2,
    pull_next_page/3,
    round_expired/1,
    become_ready/1,
    merge_version_ceiling/2,
    periodic_reconcile/1,
    reconcile_tick/2,
    bounded_sweep/0
]).
-spec discover(#state{}) -> {noreply, #state{}}.
discover(#state{retries = R, recover_deadline_ms = Deadline} = State) when
    R > ?RECOVER_MAX_RETRY
->
    case erlang:system_time(millisecond) >= Deadline of
        true ->
            _ = imboy_cache_sync_proto:record_recovery_timeout(deadline_timeout),
            enter_degraded(State);
        false ->
            %% Dmax 未到：继续有界重试（重置轮计数，保持 5s 节奏）
            discover(State#state{retries = 0})
    end;
discover(#state{recover_deadline_ms = Deadline} = State) ->
    case
        is_integer(Deadline) andalso
            erlang:system_time(millisecond) >= Deadline
    of
        true ->
            _ = imboy_cache_sync_proto:record_recovery_timeout(deadline_timeout),
            enter_degraded(State);
        false ->
            discover_peers(State)
    end;
discover(State) ->
    discover_peers(State).
-spec discover_peers(#state{}) -> {noreply, #state{}}.
discover_peers(State) ->
    Members = [P || P <- imboy_cache_sync:syn_scope_members(), node(P) =/= node()],
    case length(Members) > imboy_cache_policy:peer_max() of
        true -> reject_membership(State);
        false -> discover_members(Members, State)
    end.
reject_membership(State) ->
    imboy_cache_sync:incr(reject_total),
    enter_degraded(start_cold_recovery(State)).
discover_members(Members, State) ->
    case Members of
        [] when State#state.retries < 3 ->
            %% syn 成员视图最终一致：刚 join/重启后可能瞬时为空，
            %% 有界重试后再判定（防止误判单节点而跳过版本上界恢复）
            {noreply, schedule_recovery(State#state{retries = State#state.retries + 1}, 500)};
        [] ->
            case application:get_env(imboy, cluster_nodes, []) of
                [] when State#state.requires_authority =:= false ->
                    case
                        imboy_cache_sync_apply:required_epoch() =< imboy_cache_sync_proto:epoch()
                    of
                        true -> become_ready(State);
                        false -> wait_for_authority([], State)
                    end;
                _ ->
                    wait_for_authority([], State)
            end;
        _ ->
            wait_for_authority(Members, State)
    end.
wait_for_authority(Members, State) ->
    %% 先冻结全部已发现对端；第一个应答不能跳过其余未应答对端。
    RoundsUsed =
        case State#state.snapshot_round of
            #{rounds_used := N} -> N;
            _ -> 0
        end,
    R0 = imboy_cache_sync_snapshot:recovery_round(RoundsUsed, State#state.recover_deadline_ms),
    RoundSt = lists:foldl(
        fun(P, R) ->
            imboy_cache_sync_snapshot:expect_peer(R, node(P))
        end,
        R0,
        Members
    ),
    _ = [
        P ! imboy_cache_sync_proto:version_request_frame(self(), maps:get(round, RoundSt))
     || P <- Members
    ],
    true = ets:insert(?META_TABLE, {status, recovering}),
    {noreply,
        schedule_recovery(
            State#state{status = recovering, snapshot_round = RoundSt},
            ?RECOVER_ROUND_MS
        )}.
%% 冷清理后的可用性屏障与状态必须由同一写入者一起切换。
start_cold_recovery(State) ->
    start_cold_recovery(State, 0).
start_cold_recovery(State, MinimumEpoch) when is_integer(MinimumEpoch), MinimumEpoch >= 0 ->
    true = ets:insert(?META_TABLE, {status, discovering}),
    ok = imboy_cache_sync_apply:controlled_cold_recovery(MinimumEpoch),
    Deadline =
        case State#state.status of
            ready -> erlang:system_time(millisecond) + imboy_cache_policy:d_max_ms();
            _ -> recover_deadline_of(State)
        end,
    schedule_recovery(
        State#state{
            status = discovering,
            requires_authority = true,
            identity_recovery = false,
            retries = 0,
            snapshot_round = undefined,
            recover_deadline_ms = Deadline
        },
        0
    ).
%% 缺口内容未知：隐藏并清理 live 值，保留版本/墓碑，在同 epoch 请求完整快照。
%% 恢复中的新缺口中止本轮，但不续总期限；重复水位不会再次调用本入口。
start_gap_recovery(#state{status = ready} = State) ->
    true = ets:insert(?META_TABLE, {status, discovering}),
    ok = imboy_cache_sync_apply:flush_replicas(),
    schedule_recovery(
        State#state{
            status = discovering,
            requires_authority = true,
            retries = 0,
            snapshot_round = undefined,
            recover_deadline_ms = erlang:system_time(millisecond) + imboy_cache_policy:d_max_ms()
        },
        0
    );
start_gap_recovery(#state{status = recovering} = State) ->
    abort_round(sequence_gap, State);
start_gap_recovery(State) ->
    State.
schedule_recovery(State, Delay) ->
    State1 = cancel_recovery_timer(State),
    Remaining = erlang:max(0, recover_deadline_of(State1) - erlang:system_time(millisecond)),
    Ref = erlang:start_timer(erlang:min(Delay, Remaining), self(), cache_recovery),
    State1#state{recovery_timer = Ref}.
cancel_recovery_timer(#state{recovery_timer = Ref} = State) ->
    _ =
        case is_reference(Ref) of
            true -> _ = erlang:cancel_timer(Ref);
            false -> ok
        end,
    State#state{recovery_timer = undefined}.
handle_timeout(#state{status = discovering} = State) ->
    discover(State);
handle_timeout(#state{status = recovering} = State) ->
    case round_expired(State) of
        true ->
            ok = imboy_cache_sync_apply:flush_replicas(),
            _ = imboy_cache_sync_proto:record_recovery_timeout(deadline_timeout),
            enter_degraded(State);
        false ->
            case has_snapshot_authority(State#state.snapshot_round) of
                true -> {noreply, abort_round(round_timeout, State)};
                false -> discover(State#state{retries = State#state.retries + 1})
            end
    end;
handle_timeout(State) ->
    {noreply, State}.
has_snapshot_authority(#{peers := Peers}) ->
    lists:any(fun(P) -> maps:get(authority, P) end, maps:values(Peers));
has_snapshot_authority(_) ->
    false.
-spec enter_degraded(#state{}) -> {noreply, #state{}}.
enter_degraded(State) ->
    ?WARN_LOG(["cache sync recovery retries exhausted, entering DEGRADED"]),
    imboy_cache_sync:incr(degraded_total),
    true = ets:insert(?META_TABLE, {status, degraded}),
    State1 = cancel_recovery_timer(State),
    {noreply, State1#state{status = degraded, snapshot_round = undefined}}.
%% 成员报告登记身份、并入版本上限；本轮合法目标冻结在来源分类之前。
%% 旧诊断入口没有来源目标，不能从 counter 推导目标序列。
handle_version_report(Node, Counter, Epoch, Hash, Registry, Boot, Ready, Pid, State) ->
    handle_version_report(Node, Counter, Epoch, Hash, Registry, Boot, Ready, Pid, unknown, State).
handle_version_report(
    FromNode,
    MaxCounter,
    Epoch,
    ConfigHash,
    RegistryHash,
    BootId,
    ReadyFlag,
    FromPid,
    Target,
    #state{status = recovering} = State
) ->
    HashOk =
        ConfigHash =:= imboy_cache_sync_proto:config_hash() andalso
            RegistryHash =:= imboy_cache_sync_proto:registry_hash() andalso
            (State#state.identity_recovery orelse not State#state.requires_authority orelse
                Epoch =:= imboy_cache_sync_proto:epoch()),
    case HashOk of
        true ->
            case accept_peer_report(FromNode, BootId, MaxCounter, State) of
                {rejected, State1} ->
                    State1;
                {Changed, State1} ->
                    true = ets:insert(?META_TABLE, {{peer_epoch, FromNode}, Epoch}),
                    TargetInput =
                        case Target of
                            unknown -> {FromNode, BootId, Epoch, unknown};
                            _ -> Target
                        end,
                    case
                        imboy_cache_sync_proto:record_origin_target(
                            TargetInput, State1#state.snapshot_round
                        )
                    of
                        {ok, R} ->
                            handle_recovery_source(
                                FromNode,
                                BootId,
                                Epoch,
                                ReadyFlag,
                                FromPid,
                                Changed,
                                true,
                                State1#state{snapshot_round = R}
                            );
                        {abort, Reason, _} ->
                            abort_round(Reason, State1)
                    end
            end;
        false ->
            imboy_cache_sync:incr(reject_total),
            handle_recovery_source(
                FromNode, BootId, Epoch, ReadyFlag, FromPid, registered, false, State
            )
    end;
handle_version_report(
    FromNode,
    MaxCounter,
    Epoch,
    ConfigHash,
    RegistryHash,
    BootId,
    _ReadyFlag,
    _FromPid,
    _Target,
    State
) ->
    case
        ConfigHash =:= imboy_cache_sync_proto:config_hash() andalso
            RegistryHash =:= imboy_cache_sync_proto:registry_hash() andalso
            Epoch =:= imboy_cache_sync_proto:epoch()
    of
        true ->
            {_, State1} = accept_peer_report(FromNode, BootId, MaxCounter, State),
            State1;
        false ->
            imboy_cache_sync:incr(reject_total),
            State
    end.
accept_peer_report(FromNode, BootId, MaxCounter, State) ->
    case imboy_cache_sync_proto:register_incarnation(FromNode, BootId) of
        {error, incarnation_budget_exceeded} ->
            {rejected, start_cold_recovery(State)};
        Changed ->
            changed_compensation(Changed, {peer_epoch, FromNode}),
            true = ets:insert(?META_TABLE, {{peer_counter, FromNode}, MaxCounter}),
            {Changed, merge_version_ceiling(MaxCounter, State)}
    end.
handle_recovery_source(FromNode, BootId, Epoch, ReadyFlag, FromPid, Changed, HashOk, State1) ->
    RoundSt0 = current_or_new_round(State1),
    Cohort = maps:get(cohort_epoch, RoundSt0, Epoch),
    AuthorityOk =
        HashOk andalso ReadyFlag =:= true andalso
            maps:is_key(FromNode, maps:get(peers, RoundSt0)) andalso Epoch =:= Cohort,
    HigherCohort =
        HashOk andalso ReadyFlag =:= true andalso Epoch > Cohort andalso
            maps:is_key(FromNode, maps:get(peers, RoundSt0)),
    {WasAuthority, SameBoot, SameEpoch} =
        case maps:find(FromNode, maps:get(peers, RoundSt0)) of
            {ok, PeerSt} ->
                {
                    maps:get(authority, PeerSt),
                    maps:get(boot, PeerSt, undefined) =:= BootId,
                    maps:get(source_epoch, PeerSt, undefined) =:= Epoch
                };
            error ->
                {false, false, false}
        end,
    case
        {
            HigherCohort orelse
                (WasAuthority andalso
                    (not AuthorityOk orelse not SameBoot orelse not SameEpoch orelse
                        Changed =:= changed)),
            AuthorityOk
        }
    of
        {true, _} ->
            abort_round(authority_changed, State1);
        {false, true} ->
            %% 只有当前发现回合内、hash 一致且已 READY 的对端可提供快照。
            %% 快照请求直达 report 携带的 pid（r2）：本端 syn 视图滞后时
            %% 反查会命中陈旧死 pid，与 version_request 应答同源竞态
            RoundSt1 = imboy_cache_sync_snapshot:authorize_peer(RoundSt0, FromNode),
            Peers = maps:get(peers, RoundSt1),
            SourcePeer = maps:get(FromNode, Peers),
            RoundSt2 = RoundSt1#{peers := Peers#{FromNode := SourcePeer#{source_pid => FromPid}}},
            maybe_complete_round(State1#state{snapshot_round = RoundSt2});
        {false, false} ->
            %% 非权威应答完成发现分类，但不提供快照。全体均非权威仍不可闭合。
            RoundSt1 = imboy_cache_sync_snapshot:exclude_peer(RoundSt0, FromNode),
            maybe_complete_round(State1#state{snapshot_round = RoundSt1})
    end.
-spec current_or_new_round(#state{}) -> map().
current_or_new_round(#state{snapshot_round = undefined, recover_deadline_ms = Dl}) ->
    imboy_cache_sync_snapshot:recovery_round(0, Dl);
current_or_new_round(#state{snapshot_round = RoundSt}) when is_map(RoundSt) ->
    RoundSt;
current_or_new_round(#state{recover_deadline_ms = Dl}) ->
    imboy_cache_sync_snapshot:recovery_round(0, Dl).
-spec changed_compensation(term(), term()) -> ok.
changed_compensation(changed, Key) ->
    ok = imboy_cache_sync_apply:gap_compensation_key(Key);
changed_compensation(registered, _Key) ->
    ok.
%% @doc 回合闭合检查（C04 三条件）：闭合 → 并入全部水位 → READY。
%% 页序悬挂但 done 的 peer 视为失败：有界重试新回合。
-spec maybe_complete_round(#state{}) -> #state{}.
maybe_complete_round(#state{snapshot_round = RoundSt0} = State0) when
    is_map(RoundSt0)
->
    State = select_cohort(State0),
    RoundSt = State#state.snapshot_round,
    case round_ready(RoundSt) of
        true ->
            case sources_current(RoundSt) of
                true ->
                    case imboy_cache_policy:verify_snapshot_coverage(RoundSt) of
                        {ok, _} ->
                            case complete_sources_current(RoundSt) of
                                ok -> become_ready_state(merge_all_watermarks(RoundSt, State));
                                {error, Reason0} -> abort_round(Reason0, State)
                            end;
                        {error, Reason} ->
                            abort_round(Reason, State)
                    end;
                false ->
                    abort_round(source_cut_identity_changed, State)
            end;
        false ->
            request_next_source(State)
    end;
maybe_complete_round(State) ->
    State.
%% 同一接收节点仅拉取一个来源的一页；done 后再启动下一来源。
request_next_source(#state{snapshot_round = #{peers := Peers, cohort_epoch := Target} = R} = State) ->
    Pending = [{N, P} || {N, P = #{authority := true, done := false}} <- maps:to_list(Peers)],
    Active = lists:any(fun({_, P}) -> maps:get(requested, P, false) end, Pending),
    Waiting = [{N, P} || {N, P = #{source_pid := Pid}} <- Pending, is_pid(Pid)],
    case {Target >= imboy_cache_sync_apply:required_epoch(), Active, Waiting} of
        {true, false, [{Node, Peer} | _]} ->
            maps:get(source_pid, Peer) ! {cache_snapshot_request, self(), maps:get(round, R)},
            State#state{snapshot_round = imboy_cache_sync_snapshot:mark_requested(R, Node)};
        _ ->
            State
    end;
request_next_source(State) ->
    State.
%% 报告分类全部完成后冻结最高合法同代来源，首份报告不能启动传输。
select_cohort(#state{snapshot_round = #{cohort_epoch := _}} = State) ->
    State;
select_cohort(#state{snapshot_round = #{peers := Peers} = R} = State) ->
    Epochs = [maps:get(source_epoch, P) || P = #{authority := true} <- maps:values(Peers)],
    Classified = lists:all(fun(P) -> maps:get(reported, P) end, maps:values(Peers)),
    case Classified andalso Epochs =/= [] of
        true -> select_cohort(lists:max(Epochs), R, State);
        false -> State
    end.
select_cohort(Target, R, State) ->
    case Target >= imboy_cache_sync_apply:required_epoch() of
        true ->
            Selected = maps:map(
                fun(_, P) ->
                    P#{
                        authority := maps:get(authority, P) andalso
                            maps:get(source_epoch, P, undefined) =:= Target
                    }
                end,
                maps:get(peers, R)
            ),
            case Target > imboy_cache_sync_proto:epoch() of
                true -> ok = imboy_cache_sync_apply:adopt_epoch(Target);
                false -> ok
            end,
            State#state{snapshot_round = R#{peers := Selected, cohort_epoch => Target}};
        false ->
            State
    end.
pull_next_page(Node, PageNo, #{peers := Peers, round := Round}) ->
    case maps:find(Node, Peers) of
        {ok, #{expected_more := true, requested := true, source_pid := Pid}} ->
            Pid ! {cache_snapshot_pull, self(), Round, PageNo + 1},
            ok;
        _ ->
            ok
    end.
handle_watermark({cache_invalidation_watermark, Origin, Boot, Seq, Hash, Registry} = F, State) ->
    case
        imboy_cache_sync_proto:valid_watermark(F) andalso
            Hash =:= imboy_cache_sync_proto:config_hash() andalso
            Registry =:= imboy_cache_sync_proto:registry_hash()
    of
        true ->
            case imboy_cache_sync_apply:reconcile_sequence(Origin, Boot, Seq) of
                gap ->
                    case covered_recovery_watermark(Origin, Boot, Seq, State) of
                        true -> State;
                        false -> start_gap_recovery(State)
                    end;
                ok ->
                    State;
                {error, _} ->
                    imboy_cache_sync:incr(reject_total),
                    State
            end;
        false ->
            imboy_cache_sync:incr(reject_total),
            State
    end.
covered_recovery_watermark(
    Origin,
    Boot,
    Seq,
    #state{status = recovering, snapshot_round = #{peers := Peers}}
) ->
    case maps:find(Origin, Peers) of
        {ok, #{
            origin_target := {Origin, Boot, Epoch, Target},
            reported_boot := Boot,
            reported_epoch := Epoch
        }} ->
            Seq =< Target;
        _ ->
            false
    end;
covered_recovery_watermark(_, _, _, _) ->
    false.
handle_snapshot_cut(Frame, #state{status = recovering, snapshot_round = R} = State) when
    is_map(R)
->
    case accept_source_cut(Frame, R) of
        {ok, Next} -> State#state{snapshot_round = Next};
        {abort, Reason, _} -> abort_round(Reason, State)
    end;
handle_snapshot_cut(_Frame, State) ->
    State.
accept_source_cut(F, R) ->
    case imboy_cache_sync_proto:valid_snapshot_cut(F) of
        false ->
            {abort, invalid_source_cut, R};
        true ->
            {cache_snapshot_cut, Origin, Round, Boot, _, Seq, _, _} = imboy_cache_policy:snapshot_cut_base(
                F
            ),
            Peers = maps:get(peers, R),
            case {Round =:= maps:get(round, R), maps:find(Origin, Peers)} of
                {true, {ok, #{authority := true, requested := true} = P}} ->
                    validate_source_cut(F, Boot, Seq, Origin, P, R);
                _ ->
                    {ok, R}
            end
    end.
validate_source_cut(F, Boot, Seq, Origin, P, R) ->
    Valid =
        erlang:system_time(millisecond) < maps:get(deadline_ms, R) andalso
            source_identity_current(Origin, F, P, R),
    case {Valid, maps:find(source_cut, P)} of
        {false, _} ->
            {abort, source_cut_identity_changed, R};
        {true, {ok, F}} ->
            finish_source_cut(F, Origin, Boot, Seq, P, R);
        {true, {ok, _}} ->
            {abort, conflicting_source_cut, R};
        {true, error} ->
            finish_source_cut(F, Origin, Boot, Seq, P, R)
    end.
%% 最后一个来源完成时，重新核对所有已确认来源；前一来源的确认可能已经失效。
sources_current(#{peers := Peers} = R) ->
    CohortEpoch = maps:get(cohort_epoch, R, undefined),
    Members = imboy_cache_sync:syn_scope_members(),
    erlang:system_time(millisecond) < maps:get(deadline_ms, R) andalso
        lists:all(
            fun(Pid) -> node(Pid) =:= node() orelse maps:is_key(node(Pid), Peers) end, Members
        ) andalso
        lists:all(
            fun({Origin, P}) ->
                case maps:get(authority, P) of
                    false ->
                        StaleEpoch =
                            is_integer(CohortEpoch) andalso
                                is_integer(maps:get(reported_epoch, P, unknown)) andalso
                                maps:get(reported_epoch, P) < CohortEpoch,
                        (StaleEpoch orelse maps:get(reported_empty, P, false)) andalso
                            lists:member(maps:get(reported_pid, P, undefined), Members) andalso
                            maps:get(reported_epoch, P, unknown) =:=
                                imboy_cache_sync_proto:peer_epoch(Origin) andalso
                            ets:lookup(?META_TABLE, {peer_incarnation, Origin}) =:=
                                [{{peer_incarnation, Origin}, maps:get(reported_boot, P, unknown)}];
                    true ->
                        source_identity_current(Origin, maps:get(source_cut, P, undefined), P, R)
                end
            end,
            maps:to_list(Peers)
        ).
complete_sources_current(Round) ->
    try
        ok = imboy_cache_sync_coverage:verify_current_prefixes(Round),
        case sources_current(Round) of
            true -> ok;
            false -> {error, source_cut_identity_changed}
        end
    catch
        error:Reason -> {error, Reason}
    end.
source_identity_current(Origin, Frame, P, R) ->
    case imboy_cache_policy:snapshot_cut_base(Frame) of
        {cache_snapshot_cut, Origin, Round, Boot, Epoch, _, Hash, Registry} ->
            lists:member(maps:get(source_pid, P, undefined), imboy_cache_sync:syn_scope_members()) andalso
                Round =:= maps:get(round, R) andalso Boot =:= maps:get(boot, P, undefined) andalso
                Epoch =:= maps:get(source_epoch, P, undefined) andalso
                Epoch =:= imboy_cache_sync_proto:peer_epoch(Origin) andalso
                Hash =:= imboy_cache_sync_proto:config_hash() andalso
                Registry =:= imboy_cache_sync_proto:registry_hash() andalso
                ets:lookup(?META_TABLE, {peer_incarnation, Origin}) =:=
                    [{{peer_incarnation, Origin}, Boot}];
        _ ->
            false
    end.
finish_source_cut(F, Origin, _Boot, Seq, P, R) when tuple_size(F) =:= 10 ->
    Target =
        case maps:get(origin_target, P, unknown) of
            {Origin, _, _, N} -> N;
            _ -> 0
        end,
    case Seq >= erlang:max(Target, maps:get(manifest_own_seq, P, 0)) of
        true -> {ok, R#{peers := (maps:get(peers, R))#{Origin := P#{source_cut => F}}}};
        false -> {abort, regressed_source_cut, R}
    end;
finish_source_cut(F, Origin, Boot, Seq, P, R) ->
    Last =
        case ets:lookup(?META_TABLE, {seq, {Origin, Boot}}) of
            [{_, N}] -> N;
            [] -> 0
        end,
    Epoch = maps:get(source_epoch, P),
    Target =
        case maps:get(origin_target, P, unknown) of
            {Origin, Boot, Epoch, N0} -> N0;
            _ -> 0
        end,
    case Seq < erlang:max(erlang:max(Last, Target), maps:get(manifest_own_seq, P, 0)) of
        true ->
            {abort, regressed_source_cut, R};
        false ->
            case imboy_cache_sync_apply:reconcile_sequence(Origin, Boot, Seq) of
                ok -> {ok, R#{peers := (maps:get(peers, R))#{Origin := P#{source_cut => F}}}};
                gap -> {abort, source_cut_sequence_gap, R};
                {error, _} -> {abort, source_cut_unknown_incarnation, R}
            end
    end.
-spec merge_all_watermarks(map(), #state{}) -> #state{}.
merge_all_watermarks(#{peers := Peers}, #state{counter = C} = State) ->
    Watermarks = [maps:get(watermark, St, 0) || St <- maps:values(Peers)],
    MaxWm = lists:max([C | Watermarks]),
    true = ets:insert(?META_TABLE, {sync_counter, MaxWm}),
    State#state{counter = MaxWm}.
%% 已验证事件登记新化身后，旧来源快照不得再完成；宽限重放不会改登记化身。
check_authority_incarnation(Origin, #state{snapshot_round = #{peers := Peers}} = State) ->
    case maps:find(Origin, Peers) of
        {ok, #{authority := true, boot := Boot}} ->
            case ets:lookup(?META_TABLE, {peer_incarnation, Origin}) of
                [{_, Boot}] -> State;
                _ -> abort_round(authority_changed, State)
            end;
        _ ->
            State
    end;
check_authority_incarnation(_Origin, State) ->
    State.
%% 高代复制数据先关闭 READY 读写屏障；随后数据写入保留其原 epoch。
guard_data_epoch(Event, State) ->
    Epoch = maps:get(epoch, Event),
    case maps:get(operation, Event) =/= invalidate andalso Epoch > imboy_cache_sync_proto:epoch() of
        true ->
            true = ets:insert(
                ?META_TABLE,
                {data_epoch_ceiling, erlang:max(Epoch, imboy_cache_sync_apply:required_epoch())}
            ),
            case State#state.status of
                ready ->
                    ok = imboy_cache_sync:bump_all_fenced(),
                    (start_gap_recovery(State))#state{requires_authority = false};
                recovering ->
                    case State#state.snapshot_round of
                        #{cohort_epoch := Bound} when Epoch > Bound ->
                            abort_round(data_epoch_changed, State);
                        _ ->
                            State
                    end;
                _ ->
                    State
            end;
        false ->
            State
    end.
%% 仅当前授权 Boot 的新 epoch 是来源变化；宽限旧化身不更换当前来源。
check_authority_event(Event, State) ->
    Origin = maps:get(origin_node, Event),
    State1 = check_authority_incarnation(Origin, State),
    case State1 of
        #state{snapshot_round = #{peers := Peers}} when State1 =:= State ->
            Boot = maps:get(origin_boot_id, Event),
            Epoch = maps:get(epoch, Event),
            case maps:find(Origin, Peers) of
                {ok, #{authority := true, boot := Boot, source_epoch := Bound}} when
                    Epoch > Bound
                ->
                    abort_round(authority_epoch_changed, State1);
                _ ->
                    State1
            end;
        _ ->
            State1
    end.
%% @doc 回合失败：Rmax_rounds 内重试新回合，超限 DEGRADED。
-spec abort_round(term(), #state{}) -> #state{}.
abort_round(Reason, #state{snapshot_round = RoundSt} = State) when is_map(RoundSt) ->
    %% 部分恢复页不得在后续回合 READY 时暴露；保留版本与墓碑拒绝旧重放。
    ok = imboy_cache_sync_apply:flush_replicas(),
    RoundsUsed = maps:get(rounds_used, RoundSt, 0) + 1,
    ?WARN_LOG(["cache sync snapshot round aborted", Reason, RoundsUsed]),
    _ = imboy_cache_sync_proto:record_recovery_abort(Reason),
    case RoundsUsed >= imboy_cache_policy:r_max_rounds() of
        true ->
            {noreply, S1} = enter_degraded(State),
            S1;
        false ->
            Deadline = recover_deadline_of(State),
            schedule_recovery(
                State#state{
                    snapshot_round =
                        imboy_cache_sync_snapshot:recovery_round(RoundsUsed, Deadline)
                },
                0
            )
    end;
abort_round(_Reason, State) ->
    State.
-spec recover_deadline_of(#state{}) -> non_neg_integer().
recover_deadline_of(#state{recover_deadline_ms = Dl}) when is_integer(Dl) ->
    Dl;
recover_deadline_of(_) ->
    erlang:system_time(millisecond) + imboy_cache_policy:d_max_ms().
%% 回合是否过期（无回合 = 未过期，走常规恢复轮）。
-spec round_expired(#state{}) -> boolean().
round_expired(#state{snapshot_round = RoundSt}) when is_map(RoundSt) ->
    Deadline = maps:get(deadline_ms, RoundSt, 0),
    erlang:system_time(millisecond) >= Deadline;
round_expired(_) ->
    false.
-spec become_ready(#state{}) -> {noreply, #state{}}.
become_ready(#state{snapshot_round = undefined, requires_authority = true} = State) ->
    wait_for_authority([], State);
become_ready(#state{snapshot_round = undefined} = State) ->
    {noreply, become_ready_state(State)};
become_ready(#state{snapshot_round = Round} = State) ->
    case round_ready(Round) andalso sources_current(Round) of
        true -> {noreply, maybe_complete_round(State)};
        false -> wait_for_authority([], State#state{requires_authority = true})
    end.
round_ready(Round) ->
    imboy_cache_sync_snapshot:round_complete(Round) andalso
        lists:max([imboy_cache_sync_proto:epoch() | authority_epochs(Round)]) >=
            imboy_cache_sync_apply:required_epoch().
-spec become_ready_state(#state{}) -> #state{}.
become_ready_state(State) ->
    %% 冷恢复完成：epoch 取 max(本 epoch, 本轮已授权来源 epoch)（C01 第 3 节）
    PeerEpochs = authority_epochs(State#state.snapshot_round),
    MaxEpoch = lists:max([imboy_cache_sync_proto:epoch() | PeerEpochs]),
    case MaxEpoch > imboy_cache_sync_proto:epoch() of
        true -> true = ets:insert(?META_TABLE, {sync_epoch, MaxEpoch});
        false -> ok
    end,
    %% 恢复期间新启动的查询也必须跨完成栅栏；不能只依赖恢复开始时的失效。
    ok = imboy_cache_sync:bump_all_fenced(),
    case imboy_cache_sync_proto:commit_snapshot_floors(State#state.snapshot_round) of
        ok ->
            %% floor 只覆盖本轮来源；已登记但无 floor 的来源在此补零下限行，
            %% 否则 ready 后 send_snapshot 的 census 缺项整份拒绝，恢复方无法闭合。
            ok = imboy_cache_sync_proto:rebase_sequence_coverage(),
            publish_ready_state(State);
        {error, Reason} ->
            abort_round(Reason, State)
    end.
publish_ready_state(State) ->
    State1 = arm_reconcile(cancel_recovery_timer(State)),
    true = ets:insert(?META_TABLE, {status, ready}),
    Ready = State1#state{
        status = ready,
        requires_authority = false,
        identity_recovery = false,
        retries = 0,
        snapshot_round = undefined
    },
    announce_ready(Ready),
    Ready.
%% 使用既有报告/水位，且只在完成恢复后通知；不把部分回合广播成 authority。
announce_ready(State) ->
    Peers = [P || P <- imboy_cache_sync:syn_scope_members(), node(P) =/= node()],
    _ =
        case length(Peers) =< imboy_cache_policy:peer_max() of
            true -> [ok = imboy_cache_sync:send_version_report(P, State) || P <- Peers];
            false -> self() ! reconcile_tick
        end,
    ok.
%% 被排除的 hash/epoch 不一致报告不能影响 READY 的 epoch。
authority_epochs(#{peers := Peers}) ->
    [
        E
     || #{authority := true, source_epoch := E} <- maps:values(Peers),
        is_integer(E)
    ];
authority_epochs(_) ->
    [].
-spec merge_version_ceiling(non_neg_integer(), #state{}) -> #state{}.
merge_version_ceiling(MaxCounter, #state{counter = C} = State) ->
    State#state{counter = erlang:max(C, MaxCounter)}.
%% @doc 定期轻量校对（9.4）：交换版本摘要上界 + 有界清理（W4-06/W4-07）
%% + 宽限化身淘汰（C02）+ rindex 计数校准（C03）。
-spec periodic_reconcile(#state{}) -> #state{}.
periodic_reconcile(#state{status = ready} = State) ->
    imboy_cache_sync:incr(reconcile_total),
    Members = [P || P <- imboy_cache_sync:syn_scope_members(), node(P) =/= node()],
    case length(Members) > imboy_cache_policy:peer_max() of
        true ->
            {noreply, Next} = reject_membership(State),
            Next;
        false ->
            case imboy_cache_sync_coverage:observe_member_loss(Members) of
                cold ->
                    start_cold_recovery(State);
                continue ->
                    reconcile_members(Members, State);
                invalid ->
                    {noreply, Next} = reject_membership(State),
                    Next
            end
    end;
periodic_reconcile(State) ->
    State.
%% 取消不能撤回已入邮箱的 timeout，故必须核对当前引用。
reconcile_tick(Ref, #state{reconcile_timer = Ref} = State) when is_reference(Ref) ->
    arm_reconcile(periodic_reconcile(State#state{reconcile_timer = undefined}));
reconcile_tick(reconcile_tick, State) ->
    arm_reconcile(periodic_reconcile(State));
reconcile_tick(_Stale, State) ->
    State.
arm_reconcile(#state{reconcile_timer = Ref} = State) ->
    _ =
        case is_reference(Ref) of
            true -> _ = erlang:cancel_timer(Ref);
            false -> ok
        end,
    State#state{
        reconcile_timer = erlang:start_timer(?RECONCILE_INTERVAL_MS, self(), reconcile_tick)
    }.
reconcile_members(Members, State) ->
    _ = [P ! {cache_version_request, self(), node()} || P <- Members],
    ok = bounded_sweep(),
    ok = imboy_cache_sync_proto:prune_grace(erlang:system_time(millisecond)),
    ok = imboy_cache_sync_apply:recalibrate_rindex_count(),
    State.
%% @doc 有界清理（W4-06/W4-07）：过期墓碑/过期 live 淘汰 + rindex 连带。
-spec bounded_sweep() -> ok.
bounded_sweep() ->
    ok = imboy_cache_sync_snapshot:sweep_senders(),
    NowMs = erlang:system_time(millisecond),
    TTLms = imboy_cache_policy:tombstone_ttl_sec() * 1000,
    %% tab2list 直取整行：ets:match('$1') 绑定的是整行元组，按 [K, V] 列表
    %% 解构永不匹配、清理空转（F4 场景实证的潜伏缺陷）
    _ = [
        begin
            ok = sweep_entry(K),
            ok = imboy_cache_sync_apply:delete_metadata(K)
        end
     || {K, V} <- ets:tab2list(?META_TABLE),
        is_sweepable(K, V, NowMs, TTLms)
    ],
    ok.
%% 淘汰 rev 时连带清理 rindex，避免无界增长（W4-06/07）
sweep_entry({rev, K}) ->
    ok = imboy_cache_sync_apply:delete_metadata({rindex, K}),
    ok;
sweep_entry(_) ->
    ok.
-spec is_sweepable(term(), term(), non_neg_integer(), non_neg_integer()) -> boolean().
%% 过期墓碑：再保留一个 Tmax 写窗（陈旧事件防复活），之后连同 rindex 淘汰
is_sweepable(K, {_Rev, _C, ExpiresAt, deleted, _Epoch}, NowMs, TTLms) ->
    is_tuple(K) andalso tuple_size(K) =:= 2 andalso element(1, K) =:= rev andalso
        ExpiresAt + TTLms < NowMs;
%% 较旧 SET 可能有更长 TTL；总 counter 不能替代逐键版本下限。
%% 值过期后保留有界版本窗口，仍计入 N/bytes，超过窗口才连 rindex 回收。
is_sweepable(K, {_Rev, _C, ExpiresAt, live, _Epoch}, NowMs, TTLms) ->
    is_tuple(K) andalso tuple_size(K) =:= 2 andalso element(1, K) =:= rev andalso
        ExpiresAt + TTLms < NowMs;
is_sweepable(_, _, _, _) ->
    false.
