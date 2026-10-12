%% @doc 缓存同步恢复引擎：发现/降级/版本上界握手应答处理（r1/r2）、C04 快照
%% 回合闭合、9.4 定期校对与 W4-06/07 有界清扫。由 imboy_cache_sync 串行调用，
%% 函数体自 imboy_cache_sync.erl 拆分迁移（closeout 例外 4），零行为变更。
-module(imboy_cache_sync_recover).

-include("log.hrl").
-include("imboy_cache_sync_state.hrl").

-export([
    discover/1,
    handle_version_report/9,
    current_or_new_round/1,
    maybe_complete_round/1,
    abort_round/2,
    round_expired/1,
    become_ready/1,
    merge_version_ceiling/2,
    periodic_reconcile/1,
    bounded_sweep/0
]).

-spec discover(#state{}) -> {noreply, #state{}}.
discover(#state{retries = R, recover_deadline_ms = Deadline} = State) when
    R > ?RECOVER_MAX_RETRY
->
    case erlang:system_time(millisecond) >= Deadline of
        true ->
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
            enter_degraded(State);
        false ->
            discover_peers(State)
    end;
discover(State) ->
    discover_peers(State).

-spec discover_peers(#state{}) -> {noreply, #state{}}.
discover_peers(State) ->
    Members = [P || P <- imboy_cache_sync:syn_scope_members(), node(P) =/= node()],
    case Members of
        [] when State#state.retries < 3 ->
            %% syn 成员视图最终一致：刚 join/重启后可能瞬时为空，
            %% 有界重试后再判定（防止误判单节点而跳过版本上界恢复）
            _ = erlang:send_after(500, self(), timeout),
            {noreply, State#state{retries = State#state.retries + 1}};
        [] ->
            %% 稳定后仍空：真单节点，允许从空缓存启动（5.3）
            become_ready(State);
        _ ->
            %% 多节点：向全部 peer 请求版本上界（5.3 RECOVER_VERSION）
            _ = [P ! {cache_version_request, self(), node()} || P <- Members],
            _ = erlang:send_after(?RECOVER_ROUND_MS, self(), timeout),
            true = ets:insert(?META_TABLE, {status, recovering}),
            {noreply, State#state{status = recovering}}
    end.

-spec enter_degraded(#state{}) -> {noreply, #state{}}.
enter_degraded(State) ->
    ?WARN_LOG(["cache sync recovery retries exhausted, entering DEGRADED"]),
    imboy_cache_sync:incr(degraded_total),
    true = ets:insert(?META_TABLE, {status, degraded}),
    {noreply, State#state{status = degraded, snapshot_round = undefined}}.

%% @doc 处理版本上界应答（新协议帧）：
%% 1. 登记 peer 化身（变化 → C05 全量 fenced 补偿）；
%% 2. hash 一致才作为快照源（divergent 排除 authority）；
%% 3. 版本上界并入（既有 5.3）；
%% 4. recovering 时向 ready peer 请求快照（round 回合，C04）。
-spec handle_version_report(
    node(),
    non_neg_integer(),
    non_neg_integer(),
    binary(),
    binary(),
    binary(),
    boolean(),
    pid(),
    #state{}
) ->
    #state{}.
handle_version_report(
    FromNode,
    MaxCounter,
    Epoch,
    ConfigHash,
    RegistryHash,
    BootId,
    _ReadyFlag,
    FromPid,
    #state{status = recovering} = State
) ->
    Changed = imboy_cache_sync_proto:register_incarnation(FromNode, BootId),
    changed_compensation(Changed, {peer_epoch, FromNode}),
    true = ets:insert(?META_TABLE, {{peer_epoch, FromNode}, Epoch}),
    %% peer 版本水位留痕（19.8 peer-lag 观测数据源；标签按节点集合有界）
    true = ets:insert(?META_TABLE, {{peer_counter, FromNode}, MaxCounter}),
    State1 = merge_version_ceiling(MaxCounter, State),
    HashOk =
        ConfigHash =:= imboy_cache_sync_proto:config_hash() andalso
            RegistryHash =:= imboy_cache_sync_proto:registry_hash(),
    case HashOk of
        true ->
            %% hash 一致的 peer（ready 发数据快照；同在恢复的 peer 回空
            %% done——三节点同时冷启动时全部空快照闭合是合法路径：
            %% "全部 peer done + 水位合并"三条件仍需满足，C01 第 4 节）。
            %% 快照请求直达 report 携带的 pid（r2）：本端 syn 视图滞后时
            %% 反查会命中陈旧死 pid，与 version_request 应答同源竞态
            RoundSt0 = current_or_new_round(State1),
            RoundSt1 = imboy_cache_sync_snapshot:expect_peer(RoundSt0, FromNode),
            FromPid ! {cache_snapshot_request, self(), maps:get(round, RoundSt1)},
            State1#state{snapshot_round = RoundSt1};
        false ->
            %% divergent peer：不请求数据、不闭合回合（G05：不假装同步正常；
            %% 全部 peer divergent → 回合空 → 轮超时重试 → Dmax 后 DEGRADED）
            State1
    end;
handle_version_report(
    FromNode,
    MaxCounter,
    _Epoch,
    _ConfigHash,
    _RegistryHash,
    BootId,
    _ReadyFlag,
    _FromPid,
    State
) ->
    %% 非 recovering（ready/degraded）：仅并入版本上界与登记化身
    Changed = imboy_cache_sync_proto:register_incarnation(FromNode, BootId),
    changed_compensation(Changed, {peer_epoch, FromNode}),
    true = ets:insert(?META_TABLE, {{peer_counter, FromNode}, MaxCounter}),
    merge_version_ceiling(MaxCounter, State).

-spec current_or_new_round(#state{}) -> map().
current_or_new_round(#state{snapshot_round = undefined, recover_deadline_ms = Dl}) ->
    imboy_cache_sync_snapshot:new_round(0, Dl);
current_or_new_round(#state{snapshot_round = RoundSt}) when is_map(RoundSt) ->
    RoundSt;
current_or_new_round(#state{recover_deadline_ms = Dl}) ->
    imboy_cache_sync_snapshot:new_round(0, Dl).

-spec changed_compensation(term(), term()) -> ok.
changed_compensation(changed, Key) ->
    ok = imboy_cache_sync_apply:gap_compensation_key(Key);
changed_compensation(registered, _Key) ->
    ok.

%% @doc 回合闭合检查（C04 三条件）：闭合 → 并入全部水位 → READY。
%% 页序悬挂但 done 的 peer 视为失败：有界重试新回合。
-spec maybe_complete_round(#state{}) -> #state{}.
maybe_complete_round(#state{snapshot_round = RoundSt} = State) when
    is_map(RoundSt)
->
    case imboy_cache_sync_snapshot:round_complete(RoundSt) of
        true ->
            State1 = merge_all_watermarks(RoundSt, State),
            become_ready_state(State1#state{snapshot_round = undefined});
        false ->
            State
    end;
maybe_complete_round(State) ->
    State.

-spec merge_all_watermarks(map(), #state{}) -> #state{}.
merge_all_watermarks(#{peers := Peers}, #state{counter = C} = State) ->
    Watermarks = [maps:get(watermark, St, 0) || St <- maps:values(Peers)],
    MaxWm = lists:max([C | Watermarks]),
    true = ets:insert(?META_TABLE, {sync_counter, MaxWm}),
    State#state{counter = MaxWm}.

%% @doc 回合失败：Rmax_rounds 内重试新回合，超限 DEGRADED。
-spec abort_round(term(), #state{}) -> #state{}.
abort_round(Reason, #state{snapshot_round = RoundSt} = State) when is_map(RoundSt) ->
    RoundsUsed = maps:get(rounds_used, RoundSt, 0) + 1,
    ?WARN_LOG(["cache sync snapshot round aborted", Reason, RoundsUsed]),
    imboy_cache_sync:incr(snapshot_abort_total),
    case RoundsUsed >= imboy_cache_policy:r_max_rounds() of
        true ->
            {noreply, S1} = enter_degraded(State),
            S1;
        false ->
            Deadline = recover_deadline_of(State),
            State#state{
                snapshot_round =
                    imboy_cache_sync_snapshot:new_round(RoundsUsed, Deadline)
            }
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
become_ready(State) ->
    {noreply, become_ready_state(State)}.

-spec become_ready_state(#state{}) -> #state{}.
become_ready_state(State) ->
    _ = erlang:send_after(?RECONCILE_INTERVAL_MS, self(), reconcile_tick),
    true = ets:insert(?META_TABLE, {status, ready}),
    %% 冷恢复完成：epoch 取 max(本 epoch, 全部已知 peer epoch)（C01 第 3 节）
    PeerEpochs = [E || {{peer_epoch, _N}, E} <- ets:tab2list(?META_TABLE), is_integer(E)],
    MaxEpoch = lists:max([imboy_cache_sync_proto:epoch() | PeerEpochs]),
    case MaxEpoch > imboy_cache_sync_proto:epoch() of
        true -> _ = imboy_cache_sync_proto:bump_epoch();
        false -> ok
    end,
    State#state{status = ready, retries = 0, snapshot_round = undefined}.

-spec merge_version_ceiling(non_neg_integer(), #state{}) -> #state{}.
merge_version_ceiling(MaxCounter, #state{counter = C} = State) ->
    State#state{counter = erlang:max(C, MaxCounter)}.

%% @doc 定期轻量校对（9.4）：交换版本摘要上界 + 有界清理（W4-06/W4-07）
%% + 宽限化身淘汰（C02）+ rindex 计数校准（C03）。
-spec periodic_reconcile(#state{}) -> #state{}.
periodic_reconcile(#state{status = ready} = State) ->
    imboy_cache_sync:incr(reconcile_total),
    Members = [P || P <- imboy_cache_sync:syn_scope_members(), node(P) =/= node()],
    _ = [P ! {cache_version_request, self(), node()} || P <- Members],
    ok = bounded_sweep(),
    ok = imboy_cache_sync_proto:prune_grace(erlang:system_time(millisecond)),
    ok = imboy_cache_sync_apply:recalibrate_rindex_count(),
    State;
periodic_reconcile(State) ->
    State.

%% @doc 有界清理（W4-06/W4-07）：过期墓碑/过期 live 淘汰 + rindex 连带。
-spec bounded_sweep() -> ok.
bounded_sweep() ->
    NowMs = erlang:system_time(millisecond),
    TTLms = imboy_cache_policy:tombstone_ttl_sec() * 1000,
    %% tab2list 直取整行：ets:match('$1') 绑定的是整行元组，按 [K, V] 列表
    %% 解构永不匹配、清理空转（F4 场景实证的潜伏缺陷）
    _ = [
        begin
            ok = sweep_entry(K),
            true = ets:delete(?META_TABLE, K)
        end
     || {K, V} <- ets:tab2list(?META_TABLE),
        is_sweepable(K, V, NowMs, TTLms)
    ],
    ok.

%% 淘汰 rev 行时连带清理同名 rindex 行——rindex 无独立过期路径，
%% 不连带清理则随复制写入无界增长（W4-06/07）
sweep_entry({rev, K}) ->
    _ = ets:delete(?META_TABLE, {rindex, K}),
    ok;
sweep_entry(_) ->
    ok.

-spec is_sweepable(term(), term(), non_neg_integer(), non_neg_integer()) -> boolean().
%% 过期墓碑：再保留一个 Tmax 写窗（陈旧事件防复活），之后连同 rindex 淘汰
is_sweepable(K, {_Rev, _C, ExpiresAt, deleted}, NowMs, TTLms) ->
    is_tuple(K) andalso tuple_size(K) =:= 2 andalso element(1, K) =:= rev andalso
        ExpiresAt + TTLms < NowMs;
%% 过期 live：值已随 depcache TTL 消失，延迟重放由事件自带 expires_at 拒绝
%% （expired），版本上界由 gen_server counter 承载——元数据到期即淘汰
is_sweepable(K, {_Rev, _C, ExpiresAt, live}, NowMs, _TTLms) ->
    is_tuple(K) andalso tuple_size(K) =:= 2 andalso element(1, K) =:= rev andalso
        ExpiresAt < NowMs;
is_sweepable(_, _, _, _) ->
    false.
