%% @doc 三真实节点分布式缓存验证脚本（W3 Gate + W4 恢复/过载）
%%
%% 独立 BEAM 驱动（W0 框架同款模式）：不启动 IMBoy app / PG / Garage，
%% 三个真实 peer 节点（loopback + 随机 cookie），验证 V2 协议收敛、
%% 墓碑、幂等、TTL、重启版本上界恢复、invalidate 栅栏传播与过载保护。
%% 本地三节点（同机 loopback）证明协议路径，不代表三机部署网络/容量
%% 资格（19.9）。
%%
%% 用法（在 imboy 仓库根目录）：
%%   erl -noshell -pa ebin -pa deps/depcache/ebin -pa deps/syn/ebin \
%%       -eval 'cache_v21_dcheck:main(["out.json"]), halt(0).'
%% 退出码：0 全部通过；2 存在失败场景；1 参数错误。
-module(cache_v21_dcheck).
-include("imboy_cache_sync_state.hrl").
-export([main/1]).
-export([
    init_on_peer/0,
    init_on_peer_small/0,
    read_rev/1,
    read_expires/1,
    read_rev_full/1,
    inject_v2_set/4,
    inject_v2_set/5,
    inject_v2_set/6,
    restart_sync/0,
    restart_sync_only/0,
    peer_sync_down/0,
    sync_alive/0,
    sync_view/0,
    restore_nmax_then_put/1,
    meta_inject/1,
    meta_probe/1,
    overload_probe/0,
    drain_sync_mailbox/0,
    dump_debug/0,
    advance_epoch_for_dcheck/0,
    bulk_snapshot/1,
    assert_unknown_event_in_actor/3,
    install_phase_barrier/2,
    restart_with_phase_barrier/2,
    invalid_done_probe/0,
    tombstone_usage/0,
    stored_set_frame/2,
    deliver_frame/1,
    resource_sample/0,
    install_completion_observer/1
]).

-define(RPC_MS, 30000).
-define(CONVERGE_MS, 15000).
-define(POLL_MS, 50).

%% ===================================================================
%% 入口
%% ===================================================================

main([OutPath | _]) ->
    T0 = erlang:monotonic_time(millisecond),
    {ok, PeerPids, {A, B, C}} = start_cluster(),
    PrimaryResults =
        try
            Scenarios0 = scenario_list(A, B, C),
            %% 诊断模式：DCHECK_ONLY=<子串> 只跑命中场景；证据轮必须运行完整矩阵。
            Scenarios =
                case os:getenv("DCHECK_ONLY") of
                    false ->
                        Scenarios0;
                    Pat ->
                        [S || {Name, _} = S <- Scenarios0, string:find(Name, Pat) =/= nomatch]
                end,
            [run_one(Name, Fun) || {Name, Fun} <- Scenarios]
        after
            stop_cluster(PeerPids)
        end,
    Extra = [
        {"C9n delete storm exceeds configured Tmax without resurrection", fun delete_storm/3},
        {"C9o slow source crosses Dmax and drains stopped-input mailbox",
            fun slow_source_resource/3}
    ],
    SelectedExtra = select_scenarios(Extra),
    LongResults = shared_long_partition_results(),
    Results =
        PrimaryResults ++ LongResults ++
            [run_isolated_case(Name, Fun) || {Name, Fun} <- SelectedExtra],
    FinalResults =
        case Results of
            [] -> [{"DCHECK_ONLY selected no scenarios", fail, "no scenario name matched"}];
            _ -> Results
        end,
    T1 = erlang:monotonic_time(millisecond),
    Json = results_json(FinalResults, T1 - T0),
    ok = file:write_file(OutPath, Json),
    Failed = [Name || {Name, fail, _} <- FinalResults],
    io:format(
        "== cache_v21_dcheck: ~p passed, ~p failed, ~p ms ==~n",
        [length([ok || {_, pass, _} <- FinalResults]), length(Failed), T1 - T0]
    ),
    case Failed of
        [] -> halt(0);
        _ -> halt(2)
    end;
main(_) ->
    io:format("usage: cache_v21_dcheck:main([\"<output.json>\"]).~n"),
    halt(1).

%% 超窗/风暴会改变 epoch 或耗尽权威，使用独立真实集群隔离前置。
%% 仍是 required：正式无 DCHECK_ONLY 入口会全部执行，失败计入同一退出码。
select_scenarios(Scenarios) ->
    case os:getenv("DCHECK_ONLY") of
        false -> Scenarios;
        Pat -> [S || {Name, _} = S <- Scenarios, string:find(Name, Pat) =/= nomatch]
    end.

run_isolated_case(Name, Fun) ->
    try
        {ok, Pids, {A, B, C}} = start_cluster(),
        try
            run_one(Name, fun() -> Fun(A, B, C) end)
        after
            stop_cluster(Pids)
        end
    catch
        Class:Reason:Stack ->
            {Name, fail,
                io_lib:format("isolated_boot ~p:~p at ~p", [Class, Reason, lists:sublist(Stack, 3)])}
    end.

scenario_list(A, B, C) ->
    [
        {"healthy legacy receipts and concurrent reconciliation retain READY", fun() ->
            healthy_legacy_reconciliation(A, B, C)
        end},
        {"ready restart announces incarnation without a data write", fun() ->
            ready_restart_announces_identity(A, B, C)
        end},
        {"SET propagates to all nodes", fun() -> s_set(A, B, C) end},
        {"update overwrites everywhere", fun() -> s_update(A, B, C) end},
        {"DELETE tombstone removes everywhere", fun() -> s_delete(A, B, C) end},
        {"delayed stale SET cannot resurrect", fun() -> s_zombie(A, B, C) end},
        {"short-lived SET retains cross-node floor against delayed old SET", fun() ->
            s_expired_delta_floor(A, B, C)
        end},
        {"duplicate events are idempotent", fun() -> s_dup(A, B) end},
        {"concurrent SET converges to winner", fun() -> s_race(A, B, C) end},
        {"invalidate propagates fenced generation", fun() -> s_invalidate(A, B) end},
        {"TTL remaining not reset", fun() -> s_ttl(A, B) end},
        {"restart recovers version ceiling", fun() -> s_restart(A, C) end},
        {"overload protection rejects new replicate", fun() -> s_overload(A) end},
        {"snapshot backfills lost replicas after restart", fun() -> s_snapshot(A, B, C) end},
        {"evicted live version floor survives cold snapshot and rejects delayed old SET", fun() ->
            s_evicted_floor_snapshot(A, B, C)
        end},
        {"2500-entry pull snapshot exceeds buffer window and fully recovers", fun() ->
            s_large_snapshot(A, B, C)
        end},
        {"F1 peer dies during recovery round; survivor closes the round", fun() ->
            f1_peer_dies_during_recovery(A, B, C)
        end},
        {"C9l old-round and unknown-source done cannot advance blocked snapshot", fun() ->
            stale_done_during_snapshot(A, B, C)
        end},
        {"F2 delta writes during recovery merge with snapshot", fun() ->
            f2_delta_during_recovery(A, B, C)
        end},
        {"F3 hard crash (halt) + same-name incarnation recovers", fun() ->
            f3_hard_crash_incarnation(A, B, C)
        end},
        {"F4 bounded sweep removes expired meta incl. rindex", fun() ->
            f4_bounded_sweep(A, B, C)
        end},
        {"F5 early eviction: survivors-only snapshot, no resurrection", fun() ->
            f5_early_evict_survivors_only(A, B, C)
        end},
        %% --- C09 收口新增（C02/C03/C05 协议、预算与补偿）---
        {"C9a stale epoch event rejected cross-node", fun() ->
            c9a_stale_epoch(A, B)
        end},
        {"C9b hash mismatch event rejected cross-node", fun() ->
            c9b_hash_mismatch(A, B)
        end},
        {"C9c seq gap compensates fenced generation", fun() ->
            c9c_gap_compensation(A, B)
        end},
        {"C9d incarnation change compensates fenced key", fun() ->
            c9d_incarnation_compensation(A, B)
        end},
        {"C9e partition reconnect returns authority value", fun() ->
            c9e_partition_reconnect(A, B, C)
        end},
        {"C9f nmax ceiling rejects new replicate", fun() ->
            c9f_nmax(A)
        end},
        {"C9g lost tail invalidation reconciles without a new write", fun() ->
            c9g_tail_checkpoint(A, B, C)
        end},
        {"lost tail invalidation recovers through the natural periodic timer", fun() ->
            tail_checkpoint(A, B, C, periodic)
        end},
        {"C9h two recovering nodes share a ready authority", fun() ->
            c9h_parallel_recovery(A, B, C)
        end},
        {"C9j retained replica is hidden during sync-only restart", fun() ->
            c9j_retained_replica(A, B, C)
        end},
        {"C9k mixed epochs adopt the ready cohort and hide old-era values", fun() ->
            c9k_mixed_epoch_cohort(A, B, C)
        end},
        {"unconfirmed higher-epoch incarnation recovers through member snapshot", fun() ->
            unconfirmed_higher_epoch(A, B, C)
        end},
        {"C9i all replicas lost without authority stays degraded", fun() ->
            c9i_no_authority(A, B, C)
        end}
    ].

ready_restart_announces_identity(A, B, C) ->
    [ready = wait_status(N, 300) || N <- [A, B, C]],
    {A, OldBoot} = rpc:call(A, imboy_cache_sync, node_boot, [], ?RPC_MS),
    ok = rpc:call(A, ?MODULE, restart_sync, [], ?RPC_MS),
    ready = wait_status(A, 300),
    {A, NewBoot} = rpc:call(A, imboy_cache_sync, node_boot, [], ?RPC_MS),
    true = OldBoot =/= NewBoot,
    %% 不发送数据，也不请求 B/C reconcile；只观察 A 的就绪通知。
    true = converge_pred(fun() ->
        lists:all(
            fun(N) ->
                rpc:call(N, ets, lookup, [imboy_cache_meta, {peer_incarnation, A}], ?RPC_MS) =:=
                    [{{peer_incarnation, A}, NewBoot}]
            end,
            [B, C]
        )
    end),
    [ready = wait_status(N, 300) || N <- [B, C]],
    ok.

healthy_legacy_reconciliation(A, B, C) ->
    Nodes = [A, B, C],
    Old = [
        {N, rpc:call(N, application, get_env, [imboy, cache_sync_protocol], ?RPC_MS)}
     || N <- Nodes
    ],
    try
        [ready = wait_status(N, 300) || N <- Nodes],
        [
            ok = rpc:call(N, application, set_env, [imboy, cache_sync_protocol, legacy], ?RPC_MS)
         || N <- Nodes
        ],
        lists:foreach(
            fun(Source) ->
                ok = rpc:call(
                    Source, imboy_cache, invalidate, [{cache_v2_test, healthy_legacy}], ?RPC_MS
                ),
                Boot = element(2, rpc:call(Source, sys, get_state, [imboy_cache_sync], ?RPC_MS)),
                [ok = await_legacy_receipt(N, Source, Boot, 100) || N <- Nodes, N =/= Source]
            end,
            Nodes
        ),
        lists:foreach(
            fun(N) ->
                P = rpc:call(N, erlang, whereis, [imboy_cache_sync], ?RPC_MS),
                reconcile_tick = rpc:call(N, erlang, send, [P, reconcile_tick], ?RPC_MS)
            end,
            Nodes
        ),
        timer:sleep(500),
        [ready = rpc:call(N, imboy_cache_sync, status, [], ?RPC_MS) || N <- Nodes],
        ok
    after
        [
            case Cfg of
                undefined ->
                    rpc:call(N, application, unset_env, [imboy, cache_sync_protocol], ?RPC_MS);
                {ok, V} ->
                    rpc:call(N, application, set_env, [imboy, cache_sync_protocol, V], ?RPC_MS)
            end
         || {N, Cfg} <- Old
        ]
    end.

await_legacy_receipt(_Node, _Source, _Boot, 0) ->
    error(legacy_receipt_missing);
await_legacy_receipt(Node, Source, Boot, Left) ->
    case rpc:call(Node, ets, lookup, [imboy_cache_meta, {seq, {Source, Boot}}], ?RPC_MS) of
        [{_, 1}] ->
            ok;
        _ ->
            timer:sleep(20),
            await_legacy_receipt(Node, Source, Boot, Left - 1)
    end.

c9j_retained_replica(A, B, C) ->
    Key = {cache_v2_test_rep, retained_restart},
    Old = [{field, old}],
    New = [{field, new}],
    ok = rpc:call(B, imboy_cache, put, [Key, Old, 120, replicate], ?RPC_MS),
    {ok, Old} = converge_get(A, Key, {ok, Old}),
    Cookie = rpc:call(A, erlang, get_cookie, [], ?RPC_MS),
    Cfg = rpc:call(A, application, get_env, [imboy, cluster_nodes], ?RPC_MS),
    try
        ok = rpc:call(A, application, set_env, [imboy, cluster_nodes, [A, B, C]], ?RPC_MS),
        try
            [
                true = rpc:call(A, erlang, set_cookie, [N, retained_isolation], ?RPC_MS)
             || N <- [B, C]
            ],
            [true = rpc:call(A, erlang, disconnect_node, [N], ?RPC_MS) || N <- [B, C]],
            ok = rpc:call(B, imboy_cache, put, [Key, New, 120, replicate], ?RPC_MS),
            assert_retained_replica_hidden(A, Key, Old)
        after
            [true = rpc:call(A, erlang, set_cookie, [N, Cookie], ?RPC_MS) || N <- [B, C]],
            [true = rpc:call(A, net_kernel, connect_node, [N], ?RPC_MS) || N <- [B, C]]
        end,
        ready = wait_status(A, 400),
        {ok, New} = converge_get(A, Key, {ok, New})
    after
        restore_cluster_config(A, Cfg)
    end,
    ok.

assert_retained_replica_hidden(Node, Key, Value) ->
    {Node, OldBoot} = rpc:call(Node, imboy_cache_sync, node_boot, [], ?RPC_MS),
    ok = rpc:call(Node, ?MODULE, restart_sync_only, [], ?RPC_MS),
    {Node, NewBoot} = rpc:call(Node, imboy_cache_sync, node_boot, [], ?RPC_MS),
    true = OldBoot =/= NewBoot,
    {ok, Value} = rpc:call(Node, depcache, get, [Key, imboy_cache], ?RPC_MS),
    undefined = rpc:call(Node, imboy_cache, get, [Key], ?RPC_MS),
    undefined = rpc:call(Node, imboy_cache, get, [Key, field], ?RPC_MS),
    undefined = rpc:call(Node, imboy_cache, get_subkey, [Key, field], ?RPC_MS).

restore_cluster_config(Node, undefined) ->
    rpc:call(Node, application, unset_env, [imboy, cluster_nodes], ?RPC_MS);
restore_cluster_config(Node, {ok, Cfg}) ->
    rpc:call(Node, application, set_env, [imboy, cluster_nodes, Cfg], ?RPC_MS).

restart_sync_only() ->
    ok = gen_server:stop(imboy_cache_sync, normal, 10000),
    {ok, Pid} = imboy_cache_sync:start_link(),
    unlink(Pid),
    ok.

c9h_parallel_recovery(A, B, C) ->
    ready = wait_status(A, 400),
    Key = {cache_v2_test_rep, parallel_recovery},
    ok = rpc:call(A, imboy_cache, put, [Key, authority_value, 120, replicate], ?RPC_MS),
    {[ok, ok], []} = rpc:multicall([B, C], ?MODULE, restart_sync, [], ?RPC_MS),
    ready = wait_status(B, 400),
    ready = wait_status(C, 400),
    {ok, authority_value} = converge_get(B, Key, {ok, authority_value}),
    {ok, authority_value} = converge_get(C, Key, {ok, authority_value}),
    ok.

c9k_mixed_epoch_cohort(A, B, C) ->
    Nodes = [A, B, C],
    [ok = ensure_c9_ready(N) || N <- Nodes],
    OldKey = {cache_v2_test_rep, cohort_old},
    NewKey = {cache_v2_test_rep, cohort_new},
    ok = rpc:call(A, imboy_cache, put, [OldKey, old_era, 120, replicate], ?RPC_MS),
    [converge_get(N, OldKey, {ok, old_era}) || N <- Nodes],
    Epoch = rpc:call(A, ?MODULE, advance_epoch_for_dcheck, [], ?RPC_MS),
    ok = rpc:call(A, imboy_cache, put, [NewKey, fresh_era, 120, replicate], ?RPC_MS),
    lists:foreach(
        fun(N) ->
            {ok, fresh_era} = converge_get(N, NewKey, {ok, fresh_era}),
            ready = wait_status(N, 300),
            Epoch = rpc:call(N, imboy_cache_sync_proto, epoch, [], ?RPC_MS),
            undefined = rpc:call(N, imboy_cache, get, [OldKey], ?RPC_MS),
            {ok, {_, _, _, live, Epoch}} = rpc:call(N, ?MODULE, read_rev_full, [NewKey], ?RPC_MS)
        end,
        Nodes
    ),
    [{error, not_found} = rpc:call(N, ?MODULE, read_rev_full, [OldKey], ?RPC_MS) || N <- [B, C]],
    ok.

unconfirmed_higher_epoch(A, B, C) ->
    Nodes = [A, B, C],
    [ready = wait_status(N, 300) || N <- Nodes],
    {A, OldBoot} = rpc:call(A, imboy_cache_sync, node_boot, [], ?RPC_MS),
    ok = rpc:call(A, ?MODULE, restart_sync, [], ?RPC_MS),
    ready = wait_status(A, 300),
    {A, NewBoot} = rpc:call(A, imboy_cache_sync, node_boot, [], ?RPC_MS),
    true = OldBoot =/= NewBoot,
    Key = {cache_v2_test_rep, unknown_high_epoch},
    ok = rpc:call(B, sys, suspend, [imboy_cache_sync], ?RPC_MS),
    {Epoch, Rev} =
        try
            E = rpc:call(A, ?MODULE, advance_epoch_for_dcheck, [], ?RPC_MS),
            ok = rpc:call(A, imboy_cache, put, [Key, recovered, 120, replicate], ?RPC_MS),
            {ok, V} = rpc:call(A, ?MODULE, read_rev, [Key], ?RPC_MS),
            {ok, Exp} = rpc:call(A, ?MODULE, read_expires, [Key], ?RPC_MS),
            [{sync_origin_seq, Seq}] = rpc:call(
                A,
                ets,
                lookup,
                [imboy_cache_meta, sync_origin_seq],
                ?RPC_MS
            ),
            Event = rpc:call(
                A,
                imboy_cache_sync_proto,
                event,
                [set, Key, recovered, Exp, V, Seq, NewBoot],
                ?RPC_MS
            ),
            %% 自证回调断言失败会传播，不能被 sys 的错误返回吞掉。
            case
                rpc:call(
                    B,
                    ?MODULE,
                    assert_unknown_event_in_actor,
                    [#{origin_boot_id => NewBoot, key => Key}, A, OldBoot],
                    ?RPC_MS
                )
            of
                {badrpc,
                    {'EXIT', {
                        {callback_failed, {gen_server, system_replace_state},
                            {error, {badmatch, {reject, invalid_protocol}}}},
                        _
                    }}} ->
                    ok;
                BadResult ->
                    erlang:error({callback_failure_not_propagated, BadResult})
            end,
            ok = rpc:call(
                B,
                ?MODULE,
                assert_unknown_event_in_actor,
                [Event, A, OldBoot],
                ?RPC_MS
            ),
            {E, V}
        after
            ok = rpc:call(B, sys, resume, [imboy_cache_sync], ?RPC_MS)
        end,
    %% 此处不重启、不 heal；只能由真实成员报告、分页、cut/done 完成恢复。
    ready = wait_status(B, 300),
    #state{status = ready, identity_recovery = false, requires_authority = false} =
        rpc:call(B, sys, get_state, [imboy_cache_sync], ?RPC_MS),
    {ok, recovered} = converge_get(B, Key, {ok, recovered}),
    Epoch = rpc:call(B, imboy_cache_sync_proto, epoch, [], ?RPC_MS),
    {ok, {Rev, _, _, live, Epoch}} = rpc:call(B, ?MODULE, read_rev_full, [Key], ?RPC_MS),
    [{{peer_incarnation, A}, NewBoot}] = rpc:call(
        B,
        ets,
        lookup,
        [imboy_cache_meta, {peer_incarnation, A}],
        ?RPC_MS
    ),
    [{{snapshot_seq_floor, {A, NewBoot}}, Floor}] = rpc:call(
        B,
        ets,
        lookup,
        [imboy_cache_meta, {snapshot_seq_floor, {A, NewBoot}}],
        ?RPC_MS
    ),
    true = Floor > 0,
    ready = wait_status(C, 300),
    ok.

assert_unknown_event_in_actor(Event, Origin, OldBoot) ->
    #state{identity_recovery = true, status = discovering} = sys:replace_state(
        imboy_cache_sync, fun(S) ->
            %% 同一 actor 回调内安排尚未确认状态，排除正常报告抢先到达。
            true = ets:insert(imboy_cache_meta, {{peer_incarnation, Origin}, OldBoot}),
            true = ets:delete(
                imboy_cache_meta,
                {grace_incarnation, {Origin, maps:get(origin_boot_id, Event)}}
            ),
            {reject, unknown_incarnation, Next} = imboy_cache_sync_apply:apply_v2(Event, S),
            true = Next#state.identity_recovery,
            true = Next#state.requires_authority,
            discovering = Next#state.status,
            [{{peer_incarnation, Origin}, OldBoot}] = ets:lookup(
                imboy_cache_meta, {peer_incarnation, Origin}
            ),
            [] = ets:lookup(imboy_cache_meta, {rev, maps:get(key, Event)}),
            Next
        end
    ),
    ok.

%% 构造已 READY 的高代 authority 前置状态，并在同一 actor 内重置代次覆盖证据。
advance_epoch_for_dcheck() ->
    #state{status = ready} = sys:replace_state(imboy_cache_sync, fun(State) ->
        _ = imboy_cache_sync_proto:bump_epoch(),
        ok = imboy_cache_sync_proto:reset_sequence_coverage(),
        State
    end),
    imboy_cache_sync_proto:epoch().

c9i_no_authority(A, B, C) ->
    Nodes = [A, B, C],
    lists:foreach(
        fun(N) ->
            ok = rpc:call(N, application, set_env, [imboy, cluster_nodes, Nodes], ?RPC_MS),
            {ok, Cfg} = rpc:call(N, application, get_env, [imboy, cache_replication], ?RPC_MS),
            ok = rpc:call(
                N,
                application,
                set_env,
                [imboy, cache_replication, Cfg#{recovery_window_sec => 3}],
                ?RPC_MS
            )
        end,
        Nodes
    ),
    {[ok, ok, ok], []} = rpc:multicall(Nodes, ?MODULE, peer_sync_down, [], ?RPC_MS),
    {[ok, ok, ok], []} = rpc:multicall(Nodes, ?MODULE, restart_sync, [], ?RPC_MS),
    lists:foreach(
        fun(N) ->
            case wait_status(N, degraded, 400) of
                degraded ->
                    ok;
                Other ->
                    erlang:error(
                        {no_authority_status, Other, rpc:call(N, ?MODULE, dump_debug, [], ?RPC_MS)}
                    )
            end,
            {error, {not_ready, degraded}} = rpc:call(
                N,
                imboy_cache,
                put,
                [{cache_v2_test_rep, no_authority}, unavailable, 60, replicate],
                ?RPC_MS
            )
        end,
        Nodes
    ),
    ok.

run_one(Name, Fun) ->
    try
        _ = Fun(),
        {Name, pass, ""}
    catch
        C:R:ST ->
            {Name, fail, io_lib:format("~p:~p at ~p", [C, R, lists:sublist(ST, 3)])}
    end.

%% ===================================================================
%% 场景（W3 测试场景表 + W4）
%% ===================================================================

s_set(A, B, C) ->
    Key = {cache_v2_test_rep, d_set1},
    ok = rpc:call(A, imboy_cache, put, [Key, 1, 120, replicate], ?RPC_MS),
    {ok, 1} = converge_get(B, Key, {ok, 1}),
    {ok, 1} = converge_get(C, Key, {ok, 1}),
    ok.

s_update(A, B, C) ->
    Key = {cache_v2_test_rep, d_set2},
    ok = rpc:call(A, imboy_cache, put, [Key, 1, 120, replicate], ?RPC_MS),
    {ok, 1} = converge_get(B, Key, {ok, 1}),
    ok = rpc:call(A, imboy_cache, put, [Key, 2, 120, replicate], ?RPC_MS),
    {ok, 2} = converge_get(B, Key, {ok, 2}),
    {ok, 2} = converge_get(C, Key, {ok, 2}),
    ok.

s_delete(A, B, C) ->
    Key = {cache_v2_test_rep, d_del1},
    ok = rpc:call(A, imboy_cache, put, [Key, v1, 120, replicate], ?RPC_MS),
    {ok, v1} = converge_get(B, Key, {ok, v1}),
    ok = rpc:call(A, imboy_cache, put_delete, [Key], ?RPC_MS),
    undefined = converge_get(B, Key, undefined),
    undefined = converge_get(C, Key, undefined),
    ok.

s_zombie(A, B, C) ->
    Key = {cache_v2_test_rep, d_zombie1},
    ok = rpc:call(A, imboy_cache, put, [Key, v1, 120, replicate], ?RPC_MS),
    {ok, v1} = converge_get(B, Key, {ok, v1}),
    {ok, Rev1} = rpc:call(B, ?MODULE, read_rev, [Key], 5000),
    ok = rpc:call(A, imboy_cache, put_delete, [Key], ?RPC_MS),
    undefined = converge_get(C, Key, undefined),
    %% 延迟旧 SET（revision 低于墓碑）不得复活（AC-15）
    ok = rpc:call(B, ?MODULE, inject_v2_set, [Key, <<"zombie">>, Rev1, A], 5000),
    timer:sleep(300),
    undefined = rpc:call(C, imboy_cache, get, [Key], 5000),
    undefined = rpc:call(B, imboy_cache, get, [Key], 5000),
    ok.

s_expired_delta_floor(A, B, C) ->
    Key = {cache_v2_test_rep, d_expired_delta_floor},
    [ok = ensure_c9_ready(N) || N <- [A, B, C]],
    ok = rpc:call(A, imboy_cache, put, [Key, older, 120, replicate], ?RPC_MS),
    [{ok, older} = converge_get(N, Key, {ok, older}) || N <- [B, C]],
    {ok, OldRev} = rpc:call(A, ?MODULE, read_rev, [Key], ?RPC_MS),
    ok = rpc:call(A, imboy_cache, put, [Key, newest, 1, replicate], ?RPC_MS),
    {ok, NewRev} = rpc:call(A, ?MODULE, read_rev, [Key], ?RPC_MS),
    {ok, Exp} = rpc:call(A, ?MODULE, read_expires, [Key], ?RPC_MS),
    true = NewRev > OldRev,
    [undefined = converge_get(N, Key, undefined) || N <- [B, C]],
    timer:sleep(erlang:max(1, Exp - erlang:system_time(millisecond) + 20)),
    Before = [{N, conflict_count(N)} || N <- [B, C]],
    ok = rpc:call(B, ?MODULE, inject_v2_set, [Key, stale, OldRev, A], ?RPC_MS),
    [ok = await_conflict(N, Count, 100) || {N, Count} <- Before],
    [
        begin
            ok = rpc:call(N, ?MODULE, drain_sync_mailbox, [], ?RPC_MS),
            {ok, NewRev} = rpc:call(N, ?MODULE, read_rev, [Key], ?RPC_MS),
            {ok, Exp} = rpc:call(N, ?MODULE, read_expires, [Key], ?RPC_MS),
            undefined = rpc:call(N, depcache, get, [Key, imboy_cache], ?RPC_MS),
            undefined = rpc:call(N, imboy_cache, get, [Key], ?RPC_MS),
            ready = rpc:call(N, imboy_cache_sync, status, [], ?RPC_MS)
        end
     || N <- [B, C]
    ],
    ok.

conflict_count(Node) ->
    Metrics = rpc:call(Node, elib_metric, get_all_metrics, [], ?RPC_MS),
    maps:get(conflict_total, maps:get(counters, Metrics), 0).

await_conflict(Node, _Before, 0) ->
    error({stale_frame_not_processed, Node});
await_conflict(Node, Before, Left) ->
    case conflict_count(Node) > Before of
        true ->
            ok;
        false ->
            timer:sleep(?POLL_MS),
            await_conflict(Node, Before, Left - 1)
    end.

s_dup(A, B) ->
    Key = {cache_v2_test_rep, d_dup1},
    ok = rpc:call(A, imboy_cache, put, [Key, v1, 120, replicate], ?RPC_MS),
    {ok, v1} = converge_get(B, Key, {ok, v1}),
    {ok, Rev1} = rpc:call(B, ?MODULE, read_rev, [Key], 5000),
    ok = rpc:call(B, ?MODULE, inject_v2_set, [Key, v1, Rev1, A], 5000),
    ok = rpc:call(B, ?MODULE, inject_v2_set, [Key, v1, Rev1, A], 5000),
    timer:sleep(300),
    {ok, v1} = rpc:call(B, imboy_cache, get, [Key], 5000),
    ok = rpc:call(A, imboy_cache, put_delete, [Key], ?RPC_MS),
    ok = rpc:call(A, imboy_cache, put_delete, [Key], ?RPC_MS),
    undefined = converge_get(B, Key, undefined),
    ok.

s_race(A, B, C) ->
    %% 前置：三节点 ready（前序故障场景的恢复长尾期 get 对 replicate
    %% key 返回 miss，b1 轮实证 10s 窗口不够——竞态 oracle 的前提是
    %% 集群正常服务，不是恢复中的收敛）
    ok = ensure_c9_ready(A),
    ok = ensure_c9_ready(B),
    ok = ensure_c9_ready(C),
    Key = {cache_v2_test_rep, d_race1},
    Self = self(),
    spawn(fun() ->
        ok = rpc:call(A, imboy_cache, put, [Key, from_a, 120, replicate], ?RPC_MS),
        Self ! done_a
    end),
    spawn(fun() ->
        ok = rpc:call(B, imboy_cache, put, [Key, from_b, 120, replicate], ?RPC_MS),
        Self ! done_b
    end),
    receive
        done_a -> ok
    after 35000 -> error(a_timeout)
    end,
    receive
        done_b -> ok
    after 35000 -> error(b_timeout)
    end,
    %% 两个本地写均已提交；取写入节点版本的最大值，而非 C 首次收到的中间值。
    {ok, RevA} = rpc:call(A, ?MODULE, read_rev, [Key], ?RPC_MS),
    {ok, RevB} = rpc:call(B, ?MODULE, read_rev, [Key], ?RPC_MS),
    Winner = {_Counter, Origin} = erlang:max(RevA, RevB),
    Value =
        case Origin of
            A -> from_a;
            B -> from_b
        end,
    [
        begin
            ok = converge_to(N, Key, {ok, Value}, 300),
            {ok, Winner} = rpc:call(N, ?MODULE, read_rev, [Key], ?RPC_MS)
        end
     || N <- [A, B, C]
    ],
    ok.

s_invalidate(A, B) ->
    %% W5-03 场景级验证：invalidate 的跨节点值扇出（业务迁移依赖的管道）。
    %% A/B 各自本地写入（非 replicate 键不广播），A 发起失效后两端本地值
    %% 都必须被清除，且 B 的 fenced generation 推进。
    Key = {cache_v2_test, d_inv1},
    ok = rpc:call(A, imboy_cache, put, [Key, inv_a, 300, local], ?RPC_MS),
    ok = rpc:call(B, imboy_cache, put, [Key, inv_b, 300, local], ?RPC_MS),
    {ok, inv_a} = rpc:call(A, imboy_cache, get, [Key], 5000),
    {ok, inv_b} = rpc:call(B, imboy_cache, get, [Key], 5000),
    G0 = rpc:call(B, imboy_cache_sync, generation, [Key], 5000),
    ok = rpc:call(A, imboy_cache, invalidate, [Key], ?RPC_MS),
    undefined = converge_get(A, Key, undefined),
    undefined = converge_get(B, Key, undefined),
    true = converge_pred(fun() ->
        rpc:call(B, imboy_cache_sync, generation, [Key], 5000) > G0
    end),
    ok.

s_ttl(A, B) ->
    %% TTL=8s > 5s 时钟偏差预算：接收端按剩余缓存（约 2-3s，保守扣减），
    %% 不得重置为完整 8s；原始截止时间过后必须过期（AC-16）
    Key = {cache_v2_test_rep, d_ttl1},
    ok = rpc:call(A, imboy_cache, put, [Key, shortlived, 8, replicate], ?RPC_MS),
    {ok, shortlived} = converge_get(B, Key, {ok, shortlived}),
    {ok, ExpA} = rpc:call(A, ?MODULE, read_expires, [Key], 5000),
    {ok, ExpB} = rpc:call(B, ?MODULE, read_expires, [Key], 5000),
    true = abs(ExpA - ExpB) =< 1500,
    %% 原始 8s 截止（含 margin）后 B 已过期——若被重置会更晚消失
    undefined = converge_get(B, Key, undefined, 9500),
    ok.

s_restart(A, C) ->
    Key = {cache_v2_test_rep, d_restart1},
    ok = rpc:call(A, imboy_cache, put, [Key, v1, 300, replicate], ?RPC_MS),
    {ok, {{C1, _}, _, _, _, _}} = rpc:call(A, ?MODULE, read_rev_full, [Key], 5000),
    OldBoot = rpc:call(C, imboy_cache_sync, node_boot, [], 5000),
    ok = rpc:call(C, ?MODULE, restart_sync, [], ?RPC_MS),
    NewBoot = rpc:call(C, imboy_cache_sync, node_boot, [], 5000),
    %% BootId 换新（5.3 incarnation）
    case element(2, OldBoot) =:= element(2, NewBoot) of
        true -> erlang:error({boot_id_not_rotated, OldBoot, NewBoot});
        false -> ok
    end,
    St = wait_status(C, 300),
    ready = St,
    %% 重启后首次写入 revision 不回退（版本上界恢复，AC-17）
    Key2 = {cache_v2_test_rep, d_restart2},
    PutR = rpc:call(C, imboy_cache, put, [Key2, from_c, 120, replicate], ?RPC_MS),
    ok = PutR,
    {ok, {{C2, _}, _, _, _, _}} = rpc:call(C, ?MODULE, read_rev_full, [Key2], 5000),
    case C2 >= C1 of
        true ->
            ok;
        false ->
            erlang:error(
                {version_ceiling_regressed, C1, C2, rpc:call(C, ?MODULE, dump_debug, [], 5000)}
            )
    end.

%% W4-03/04：C 重启丢失 replicate 数据后，经快照回填恢复；恢复期间
%% peer 的新写入（delta）也不丢失。
s_evicted_floor_snapshot(A, B, C) ->
    Key = {cache_v2_test_rep, d_evicted_floor},
    [ok = ensure_c9_ready(N) || N <- [A, B, C]],
    ok = rpc:call(A, imboy_cache, put, [Key, older, 120, replicate], ?RPC_MS),
    [{ok, older} = converge_get(N, Key, {ok, older}) || N <- [B, C]],
    {ok, OldRev} = rpc:call(A, ?MODULE, read_rev, [Key], ?RPC_MS),
    ok = rpc:call(A, imboy_cache, put, [Key, current, 120, replicate], ?RPC_MS),
    [{ok, current} = converge_get(N, Key, {ok, current}) || N <- [B, C]],
    {ok, NewRev} = rpc:call(A, ?MODULE, read_rev, [Key], ?RPC_MS),
    {ok, Exp} = rpc:call(A, ?MODULE, read_expires, [Key], ?RPC_MS),
    try
        [ok = rpc:call(N, depcache, flush, [Key, imboy_cache], ?RPC_MS) || N <- [A, C]],
        ok = rpc:call(B, ?MODULE, restart_sync, [], ?RPC_MS),
        ready = recover_ready(B, heal_restart(B), 2),
        {ok, NewRev} = rpc:call(B, ?MODULE, read_rev, [Key], ?RPC_MS),
        {ok, Exp} = rpc:call(B, ?MODULE, read_expires, [Key], ?RPC_MS),
        undefined = rpc:call(B, depcache, get, [Key, imboy_cache], ?RPC_MS),
        Before = conflict_count(B),
        ok = rpc:call(C, ?MODULE, inject_v2_set, [Key, stale, OldRev, A], ?RPC_MS),
        ok = await_conflict(B, Before, 100),
        {ok, NewRev} = rpc:call(B, ?MODULE, read_rev, [Key], ?RPC_MS),
        {ok, Exp} = rpc:call(B, ?MODULE, read_expires, [Key], ?RPC_MS),
        undefined = rpc:call(B, depcache, get, [Key, imboy_cache], ?RPC_MS),
        undefined = rpc:call(B, imboy_cache, get, [Key], ?RPC_MS),
        ready = rpc:call(B, imboy_cache_sync, status, [], ?RPC_MS)
    after
        ok = ensure_c9_ready(A),
        ok = rpc:call(A, imboy_cache, put, [Key, restored, 120, replicate], ?RPC_MS)
    end.

s_large_snapshot(A, B, C) ->
    ok = rpc:call(A, ?MODULE, bulk_snapshot, [write], ?RPC_MS),
    true = converge_pred(fun() -> rpc:call(B, ?MODULE, bulk_snapshot, [verify], ?RPC_MS) =:= ok end),
    ok = rpc:call(C, ?MODULE, restart_sync, [], ?RPC_MS),
    ready = wait_status(C, 400),
    ok = rpc:call(C, ?MODULE, bulk_snapshot, [verify], ?RPC_MS),
    ok.

bulk_snapshot(write) ->
    [
        ok = imboy_cache:put({cache_v2_test_rep, {large_snapshot, I}}, I, 300, replicate)
     || I <- lists:seq(1, 2500)
    ],
    ok;
bulk_snapshot(verify) ->
    case
        lists:all(
            fun(I) -> imboy_cache:get({cache_v2_test_rep, {large_snapshot, I}}) =:= {ok, I} end,
            lists:seq(1, 2500)
        )
    of
        true -> ok;
        false -> incomplete
    end.

s_snapshot(A, B, C) ->
    [ready = wait_status(N, 300) || N <- [A, B, C]],
    K1 = {cache_v2_test_rep, d_snap1},
    K2 = {cache_v2_test_rep, d_snap2},
    ok = rpc:call(A, imboy_cache, put, [K1, snap_v1, 300, replicate], ?RPC_MS),
    ok = rpc:call(B, imboy_cache, put, [K2, snap_v2, 300, replicate], ?RPC_MS),
    DeletedKey = {cache_v2_test_rep, d_snap_deleted},
    ok = rpc:call(A, imboy_cache, put, [DeletedKey, removed, 300, replicate], ?RPC_MS),
    {ok, removed} = converge_get(C, DeletedKey, {ok, removed}),
    ok = rpc:call(A, imboy_cache, put_delete, [DeletedKey], ?RPC_MS),
    {ok, {
        DeletedRev = {_, _},
        {DeletedBoot, DeletedSeq},
        DeletedExp,
        deleted,
        DeletedEpoch
    }} = rpc:call(
        A, ?MODULE, read_rev_full, [DeletedKey], ?RPC_MS
    ),
    true = is_binary(DeletedBoot) andalso byte_size(DeletedBoot) =:= 16,
    true = is_integer(DeletedSeq) andalso DeletedSeq > 0,
    undefined = converge_get(C, DeletedKey, undefined),
    %% 两键在 C 上先收敛可见
    {ok, snap_v1} = converge_get(C, K1, {ok, snap_v1}),
    {ok, snap_v2} = converge_get(C, K2, {ok, snap_v2}),
    %% C 重启：内存副本全丢
    ok = rpc:call(C, ?MODULE, restart_sync, [], ?RPC_MS),
    undefined = rpc:call(C, imboy_cache, get, [K1], 5000),
    %% C 恢复完成（discover→version→snapshot→ready）
    ready = wait_status(C, 300),
    %% 快照回填：K1/K2 在 C 上重新可见（原版本，TTL 未重置）
    case converge_get_quiet(C, K1, {ok, snap_v1}, 8000) of
        {ok, snap_v1} ->
            {ok, snap_v2} = converge_get(C, K2, {ok, snap_v2}),
            ok;
        _Other ->
            %% 诊断：C 与 A 的元数据状态
            erlang:error(
                {snapshot_not_backfilled, {c_get, _Other},
                    {c_debug, rpc:call(C, ?MODULE, dump_debug, [], 5000)},
                    {a_rindex, rpc:call(A, ?MODULE, rindex_dump, [], 5000)}}
            )
    end,
    %% 完整基础快照必须保留原来源 Boot/seq，不能降格为 unknown。
    {ok, {DeletedRev, {DeletedBoot, DeletedSeq}, DeletedExp, deleted, DeletedEpoch}} =
        rpc:call(C, ?MODULE, read_rev_full, [DeletedKey], ?RPC_MS),
    undefined = rpc:call(C, imboy_cache, get, [DeletedKey], ?RPC_MS),
    ok.

s_overload(A) ->
    ready = wait_status(A, 300),
    %% 受控注入：同一存活进程内挂起→压队列→put→恢复（suspend 所有权约束）
    {error, overloaded} = rpc:call(A, ?MODULE, overload_probe, [], ?RPC_MS),
    ok = rpc:call(A, ?MODULE, drain_sync_mailbox, [], ?RPC_MS),
    Key = {cache_v2_test_rep, d_overload1},
    ok = rpc:call(A, imboy_cache, put, [Key, v, 120, replicate], ?RPC_MS),
    ok.

%% ===================================================================
%% 故障注入（W4-11：功能性故障矩阵；压测项 W4-10 另需安静窗口）
%% ===================================================================

%% F1：恢复轮中某 peer（A）sync 下线——C 的恢复回合由存活 reporter（B）
%% 闭合，READY 后回填 B 侧数据；A 回归后集群完好。
f1_peer_dies_during_recovery(A, B, C) ->
    KeyB = {cache_v2_test_rep, d_f1b},
    ok = rpc:call(B, imboy_cache, put, [KeyB, from_b, 300, replicate], ?RPC_MS),
    {ok, from_b} = converge_get(A, KeyB, {ok, from_b}),
    %% 前置断言：A 的 sync 必须存活（若被先前场景打死，显式暴露而非 noproc 含混）
    true =
        rpc:call(A, ?MODULE, sync_alive, [], 5000) orelse
            erlang:error({a_sync_dead, rpc:call(A, ?MODULE, dump_debug, [], 5000)}),
    ok = rpc:call(C, ?MODULE, restart_sync, [], ?RPC_MS),
    %% C 已进入恢复窗口，立即下线 A 的 sync（其 report/done 是否已发生均收敛）
    ok = rpc:call(A, ?MODULE, peer_sync_down, [], ?RPC_MS),
    ready = recover_ready(C, heal_restart(C), 2),
    {ok, from_b} = converge_get(C, KeyB, {ok, from_b}),
    %% sync 不可用时 put 按 overloaded fail-closed 拒绝（不崩业务调用方）
    case rpc:call(A, imboy_cache, put, [{cache_v2_test_rep, d_f1dead}, x, 60, replicate], 5000) of
        {error, overloaded} ->
            ok;
        {error, sync_unavailable} = Ok2 ->
            %% 语义上同为 fail-closed 拒绝，但出现路径存疑（sync 应已死）——转储取证
            erlang:error({f1_unexpected_reject, Ok2, rpc:call(A, ?MODULE, dump_debug, [], 5000)});
        Other ->
            erlang:error({f1_put_not_rejected, Other, rpc:call(A, ?MODULE, dump_debug, [], 5000)})
    end,
    %% A 回归（sync+depcache 全量重启），集群互通恢复。
    %% after 兜底：场景中途失败也必恢复 A，不把后续场景拖进 fail-closed。
    try
        ready = recover_ready(A, heal_restart(A), 4),
        KeyA2 = {cache_v2_test_rep, d_f1a2},
        ok = rpc:call(A, imboy_cache, put, [KeyA2, back, 120, replicate], ?RPC_MS),
        {ok, back} = converge_get(C, KeyA2, {ok, back})
    after
        catch rpc:call(A, ?MODULE, restart_sync, [], ?RPC_MS)
    end,
    ok.

%% F2：恢复窗口内的并发 delta 写——快照旧值不得覆盖 delta 新值（版本比较），
%% delta 新键不丢失（9.3 融合语义）。
f2_delta_during_recovery(A, B, C) ->
    ready = recover_ready(A, heal_restart(A), 2),
    ready = recover_ready(B, heal_restart(B), 4),
    K1 = {cache_v2_test_rep, d_f2a},
    K2 = {cache_v2_test_rep, d_f2b},
    KD = {cache_v2_test_rep, d_f2delete},
    KF = {cache_v2_test_rep, d_f2flush},
    [ok = rpc:call(A, imboy_cache, put, [K, v1, 300, replicate], ?RPC_MS) || K <- [K1, KD, KF]],
    [{ok, v1} = converge_get(C, K, {ok, v1}) || K <- [K1, KD, KF]],
    ok = rpc:call(C, ?MODULE, restart_with_phase_barrier, [page, self()], ?RPC_MS),
    {C, Actor, Ref, _Round} = wait_phase_barrier(C),
    try
        %% 真正的第一页已到 C actor、尚未应用；旧快照捕获已发生。
        ok = rpc:call(A, imboy_cache, put, [K1, v2, 300, replicate], ?RPC_MS),
        ok = rpc:call(B, imboy_cache, put, [K2, fresh, 300, replicate], ?RPC_MS),
        ok = rpc:call(A, imboy_cache, put_delete, [KD], ?RPC_MS),
        ok = rpc:call(B, imboy_cache, invalidate, [KF], ?RPC_MS)
    after
        ok = release_phase_barrier(Actor, Ref)
    end,
    ready = wait_status(C, 300),
    {ok, v2} = converge_get(C, K1, {ok, v2}),
    {ok, fresh} = converge_get(C, K2, {ok, fresh}),
    undefined = rpc:call(C, imboy_cache, get, [KD], ?RPC_MS),
    deleted = rpc:call(C, imboy_cache_sync, replicate_state, [KD], ?RPC_MS),
    undefined = rpc:call(C, imboy_cache, get, [KF], ?RPC_MS),
    [{ok, v2} = converge_get(N, K1, {ok, v2}) || N <- [A, B]],
    [undefined = rpc:call(N, imboy_cache, get, [KD], ?RPC_MS) || N <- [A, B]],
    ok.

%% A/B 在收到真实 snapshot_request、执行捕获前停住，C 能处理负向 done，
%% 且没有合法页能竞争推进 READY。仅检查真实当前回合，不制造恢复状态。
stale_done_during_snapshot(A, B, C) ->
    ready = wait_status(A, 300),
    ready = wait_status(B, 300),
    [ok = rpc:call(N, ?MODULE, install_phase_barrier, [request, self()], ?RPC_MS) || N <- [A, B]],
    ok = rpc:call(C, ?MODULE, restart_sync, [], ?RPC_MS),
    {Source, Actor, Ref, _Round} = wait_phase_barrier(any),
    try
        true = lists:member(Source, [A, B]),
        ok = rpc:call(C, ?MODULE, invalid_done_probe, [], ?RPC_MS)
    after
        %% 先移除另一个来源的未触发屏障，避免释放首源后下一请求抢先阻塞。
        Other =
            case Source of
                A -> B;
                B -> A
            end,
        try
            ok = rpc:call(Other, sys, remove, [imboy_cache_sync, cache_phase], ?RPC_MS)
        after
            ok = release_phase_barrier(Actor, Ref)
        end
    end,
    ready = wait_status(C, 300),
    ok.

install_phase_barrier(Phase, Controller) ->
    Fun = fun(State, Event, _Extra) -> phase_event(State, Event) end,
    sys:install(imboy_cache_sync, {cache_phase, Fun, {Phase, Controller}}).

restart_with_phase_barrier(Phase, Controller) ->
    ok = restart_sync(),
    install_phase_barrier(Phase, Controller).

phase_event({Phase, Controller} = State, {in, Message}) ->
    Round =
        case {Phase, Message} of
            {request, {cache_snapshot_request, _, R}} -> R;
            {page, {cache_snapshot_page, _, R, _, _, _, _}} -> R;
            _ -> undefined
        end,
    case Round of
        undefined ->
            State;
        _ ->
            Ref = make_ref(),
            Controller ! {cache_phase, node(), self(), Ref, Round},
            receive
                {release_cache_phase, Ref} ->
                    Controller ! {cache_phase_released, Ref},
                    done
            after 10000 ->
                Controller ! {cache_phase_timeout, Ref},
                erlang:error(cache_phase_release_timeout)
            end
    end;
phase_event(State, _) ->
    State.

release_phase_barrier(Actor, Ref) ->
    Actor ! {release_cache_phase, Ref},
    receive
        {cache_phase_released, Ref} -> ok;
        {cache_phase_timeout, Ref} -> erlang:error(cache_phase_release_timeout)
    after 2000 -> erlang:error(cache_phase_release_not_acknowledged)
    end.

wait_phase_barrier(Expected) ->
    receive
        {cache_phase, Node, Actor, Ref, Round} when Expected =:= any; Node =:= Expected ->
            {Node, Actor, Ref, Round}
    after 5000 -> erlang:error({cache_phase_not_observed, Expected})
    end.

invalid_done_probe() ->
    #state{status = recovering, snapshot_round = Before} = sys:get_state(imboy_cache_sync),
    Round = maps:get(round, Before),
    [Known | _] = maps:keys(maps:get(peers, Before)),
    WrongRound = crypto:strong_rand_bytes(12),
    true = WrongRound =/= Round,
    false = maps:is_key(node(), maps:get(peers, Before)),
    Pid = whereis(imboy_cache_sync),
    Pid ! {cache_snapshot_done, Known, WrongRound, 0, 0},
    pong = gen_server:call(Pid, ping_ok, 5000),
    #state{status = recovering, snapshot_round = AfterOld} = sys:get_state(Pid),
    Round = maps:get(round, AfterOld),
    false = lists:any(fun(P) -> maps:get(done, P) end, maps:values(maps:get(peers, AfterOld))),
    Pid ! {cache_snapshot_done, node(), Round, 0, 0},
    pong = gen_server:call(Pid, ping_ok, 5000),
    #state{status = recovering, snapshot_round = AfterUnknown} = sys:get_state(Pid),
    Round = maps:get(round, AfterUnknown),
    false = lists:any(fun(P) -> maps:get(done, P) end, maps:values(maps:get(peers, AfterUnknown))),
    ok.

%% 两个独立结果共用一次真实 >W 等待；任何一个 FAIL 仍决定最终非零退出。
shared_long_partition_results() ->
    Cases = select_scenarios([
        {"C9m partition exceeds W; recover from same-era authority or stay degraded",
            fun assert_long_safety/2}
    ]),
    case Cases of
        [] ->
            [];
        _ ->
            try
                {ok, Pids, {A, B, C}} = start_cluster(),
                try
                    Context = natural_long_partition(A, B, C),
                    [run_one(Name, fun() -> Oracle(A, Context) end) || {Name, Oracle} <- Cases]
                after
                    stop_cluster(Pids)
                end
            catch
                Class:Reason:Stack ->
                    [
                        {Name, fail,
                            io_lib:format("shared_long_fixture ~p:~p at ~p", [
                                Class, Reason, lists:sublist(Stack, 3)
                            ])}
                     || {Name, _} <- Cases
                    ]
            end
    end.

%% 真实时钟跨 W。当前策略硬下限 W=720s，正式矩阵不可缩短/跳过。
natural_long_partition(A, B, C) ->
    [ready = wait_status(N, 300) || N <- [A, B, C]],
    %% 只缩短合法恢复 Dmax，W 仍由策略硬下限720秒决定，三节点配置同摘要。
    [
        begin
            Cfg = rpc:call(N, application, get_env, [imboy, cache_replication, #{}], ?RPC_MS),
            ok = rpc:call(
                N,
                application,
                set_env,
                [imboy, cache_replication, Cfg#{recovery_window_sec => 3}],
                ?RPC_MS
            )
        end
     || N <- [A, B, C]
    ],
    [Hash, Hash, Hash] = [
        rpc:call(N, imboy_cache_sync_proto, config_hash, [], ?RPC_MS)
     || N <- [A, B, C]
    ],
    Key = {cache_v2_test_rep, long_partition_live},
    Deleted = {cache_v2_test_rep, long_partition_deleted},
    [ok = rpc:call(B, imboy_cache, put, [K, old, 600, replicate], ?RPC_MS) || K <- [Key, Deleted]],
    [{ok, old} = converge_get(A, K, {ok, old}) || K <- [Key, Deleted]],
    Frame = rpc:call(B, ?MODULE, stored_set_frame, [Deleted, old], ?RPC_MS),
    Epoch0 = rpc:call(A, imboy_cache_sync_proto, epoch, [], ?RPC_MS),
    W = rpc:call(A, imboy_cache_policy, tombstone_ttl_sec, [], ?RPC_MS),
    true = is_integer(W) andalso W > 0,
    %% B/C 保留初始显式空 expected 集合，不覆盖门禁配置；实际成员仍需全部 V2。
    [] = rpc:call(B, application, get_env, [imboy, cluster_nodes, []], ?RPC_MS),
    [] = rpc:call(C, application, get_env, [imboy, cluster_nodes, []], ?RPC_MS),
    Config = rpc:call(A, application, get_env, [imboy, cluster_nodes], ?RPC_MS),
    Cookie = erlang:get_cookie(),
    ok = rpc:call(A, application, set_env, [imboy, cluster_nodes, [A, B, C]], ?RPC_MS),
    [true = rpc:call(A, erlang, set_cookie, [N, cache_long_partition], ?RPC_MS) || N <- [B, C]],
    [true = rpc:call(A, erlang, disconnect_node, [N], ?RPC_MS) || N <- [B, C]],
    %% 只读取同步actor原生观察槽，绝不手写时间戳；等待从实际失联观察起算。
    true = converge_pred(fun() ->
        case rpc:call(A, ets, lookup, [imboy_cache_meta, cache_peer_loss], ?RPC_MS) of
            [{cache_peer_loss, {AnySince, IslandSince}}] ->
                is_integer(AnySince) andalso is_integer(IslandSince);
            _ ->
                false
        end
    end),
    [{cache_peer_loss, {LossSince, IslandSince}}] = rpc:call(
        A, ets, lookup, [imboy_cache_meta, cache_peer_loss], ?RPC_MS
    ),
    Start = erlang:monotonic_time(millisecond),
    WallStart = erlang:system_time(millisecond),
    FreshWrite =
        try
            true = converge_pred(fun() ->
                Members = rpc:call(B, imboy_cache_sync, syn_scope_members, [], ?RPC_MS),
                is_list(Members) andalso not lists:any(fun(P) -> node(P) =:= A end, Members)
            end),
            true = rpc:call(B, imboy_cache_sync, replication_capable, [], ?RPC_MS),
            ok = rpc:call(B, imboy_cache, put_delete, [Deleted], ?RPC_MS),
            wait_natural_window(Start + W * 1000 + 1000),
            true = erlang:system_time(millisecond) - WallStart > W * 1000,
            %% 如真实 B 已冷恢复，保留公开写失败供 availability oracle，不能跳过 safety 取证。
            rpc:call(B, imboy_cache, put, [Key, new_authority, 300, replicate], ?RPC_MS)
        after
            [true = rpc:call(A, erlang, set_cookie, [N, Cookie], ?RPC_MS) || N <- [B, C]],
            [true = rpc:call(A, net_kernel, connect_node, [N], ?RPC_MS) || N <- [B, C]]
        end,
    #{
        key => Key,
        deleted => Deleted,
        frame => Frame,
        epoch => Epoch0,
        window => W,
        peers => [B, C],
        config => Config,
        loss_since => LossSince,
        island_since => IslandSince,
        fresh_write => FreshWrite
    }.

%% 跨 W 后只从真实同代 READY 权威恢复；没有可信来源时按 C04 合同保持 DEGRADED。
assert_long_safety(A, #{
    key := Key, deleted := Deleted, frame := Frame, epoch := Epoch0, window := W, peers := Peers
}) ->
    Epoch1 = rpc:call(A, imboy_cache_sync_proto, epoch, [], ?RPC_MS),
    true = Epoch1 > Epoch0,
    Cohort = [
        {N, rpc:call(N, imboy_cache_sync_proto, epoch, [], ?RPC_MS),
            rpc:call(N, imboy_cache_sync, status, [], ?RPC_MS)}
     || N <- Peers
    ],
    MatchingReady = [N || {N, E, ready} <- Cohort, E =:= Epoch1],
    io:format("C9m actual cohort: ~p~n", [#{receiver_epoch => Epoch1, peers => Cohort}]),
    case MatchingReady of
        [] ->
            degraded = wait_status(A, degraded, 400),
            [undefined = rpc:call(A, imboy_cache, get, [K], ?RPC_MS) || K <- [Key, Deleted]],
            [
                undefined = rpc:call(A, depcache, get, [K, imboy_cache], ?RPC_MS)
             || K <- [Key, Deleted]
            ],
            {error, {not_ready, degraded}} = rpc:call(
                A, imboy_cache, put, [Key, unsafe_old_era, 60, replicate], ?RPC_MS
            );
        [Source | _] ->
            %% 有真实同代 READY 权威则验证内容恢复，不套用无源 DEGRADED 判据。
            ready = wait_status(A, 400),
            [
                begin
                    Value = rpc:call(Source, imboy_cache, get, [K], ?RPC_MS),
                    Value = converge_get(A, K, Value)
                end
             || K <- [Key, Deleted]
            ]
    end,
    assert_protected_replay_window(Frame, W),
    ok = rpc:call(A, ?MODULE, deliver_frame, [Frame], ?RPC_MS),
    undefined = rpc:call(A, depcache, get, [Deleted, imboy_cache], ?RPC_MS),
    Expected =
        case MatchingReady of
            [] -> degraded;
            _ -> ready
        end,
    Expected = rpc:call(A, imboy_cache_sync, status, [], ?RPC_MS),
    ok.

assert_protected_replay_window({cache_sync, Replay}, W) ->
    Now = erlang:system_time(millisecond),
    Exp = maps:get(expires_at_ms, Replay),
    true = Exp < Now andalso Now < Exp + W * 1000,
    ok.

wait_natural_window(Deadline) ->
    case erlang:monotonic_time(millisecond) >= Deadline of
        true ->
            ok;
        false ->
            timer:sleep(200),
            wait_natural_window(Deadline)
    end.

%% 仅缩小已有 t_max 预算，真实 public DELETE 造墓碑，绝不手工写过期元数据。
delete_storm(A, B, C) ->
    Nodes = [A, B, C],
    [ready = wait_status(N, 300) || N <- Nodes],
    [{0, _} = rpc:call(N, ?MODULE, tombstone_usage, [], ?RPC_MS) || N <- Nodes],
    Old = [
        {N, rpc:call(N, application, get_env, [imboy, cache_replication, #{}], ?RPC_MS)}
     || N <- Nodes
    ],
    Limit = 4,
    [
        ok = rpc:call(
            N, application, set_env, [imboy, cache_replication, Cfg#{t_max => Limit}], ?RPC_MS
        )
     || {N, Cfg} <- Old
    ],
    try
        Keys = [{cache_v2_test_rep, {delete_storm, I}} || I <- lists:seq(1, Limit + 1)],
        [
            ok = rpc:call(B, imboy_cache, put, [K, before_delete, 300, replicate], ?RPC_MS)
         || K <- Keys
        ],
        [{ok, before_delete} = converge_get(A, K, {ok, before_delete}) || K <- Keys],
        [First | _] = Keys,
        Frame = rpc:call(B, ?MODULE, stored_set_frame, [First, before_delete], ?RPC_MS),
        Epoch0 = rpc:call(A, imboy_cache_sync_proto, epoch, [], ?RPC_MS),
        [
            begin
                ok = rpc:call(A, imboy_cache, put_delete, [K], ?RPC_MS),
                true = converge_pred(fun() ->
                    lists:all(
                        fun(N) ->
                            rpc:call(N, imboy_cache_sync, replicate_state, [K], ?RPC_MS) =:= deleted
                        end,
                        Nodes
                    )
                end)
            end
         || K <- lists:sublist(Keys, Limit)
        ],
        {error, tombstone_budget_exceeded} = rpc:call(
            A, imboy_cache, put_delete, [lists:last(Keys)], ?RPC_MS
        ),
        [
            begin
                {Count, Bytes} = rpc:call(N, ?MODULE, tombstone_usage, [], ?RPC_MS),
                true = Count =< Limit,
                true = Bytes =< rpc:call(N, imboy_cache_policy, t_max_bytes, [], ?RPC_MS)
            end
         || N <- Nodes
        ],
        Epoch1 = rpc:call(A, imboy_cache_sync_proto, epoch, [], ?RPC_MS),
        true = Epoch1 > Epoch0,
        ok = rpc:call(A, ?MODULE, deliver_frame, [Frame], ?RPC_MS),
        undefined = rpc:call(A, imboy_cache, get, [First], ?RPC_MS),
        undefined = rpc:call(A, depcache, get, [First, imboy_cache], ?RPC_MS),
        true = rpc:call(A, ?MODULE, sync_alive, [], ?RPC_MS)
    after
        [
            ok = rpc:call(N, application, set_env, [imboy, cache_replication, Cfg], ?RPC_MS)
         || {N, Cfg} <- Old
        ]
    end,
    ok.

tombstone_usage() ->
    case ets:lookup(imboy_cache_meta, metadata_usage) of
        [{metadata_usage, {_Live, _Bytes, Count, TombBytes}}] -> {Count, TombBytes};
        [] -> {0, 0}
    end.

stored_set_frame(Key, Value) ->
    [{{rev, Key}, {Rev, {Boot, Seq}, Exp, live, _Epoch}}] = ets:lookup(
        imboy_cache_meta, {rev, Key}
    ),
    {cache_sync, imboy_cache_sync_proto:event(set, Key, Value, Exp, Rev, Seq, Boot)}.

deliver_frame(Frame) ->
    Pid = whereis(imboy_cache_sync),
    Pid ! Frame,
    pong = gen_server:call(Pid, ping_ok, 5000),
    ok.

%% 真实请求阶段慢源超过总 Dmax；停止输入后观察队列 drain。
%% memory/reductions 是实测趋势，绝不把采样峰值冒称 heap/mailbox 硬上限。
slow_source_resource(A, B, C) ->
    Nodes = [A, B, C],
    [ready = wait_status(N, 300) || N <- Nodes],
    Old = [
        {N, rpc:call(N, application, get_env, [imboy, cache_replication, #{}], ?RPC_MS)}
     || N <- Nodes
    ],
    [
        ok = rpc:call(
            N,
            application,
            set_env,
            [imboy, cache_replication, Cfg#{recovery_window_sec => 3}],
            ?RPC_MS
        )
     || {N, Cfg} <- Old
    ],
    [Hash, Hash, Hash] = [
        rpc:call(N, imboy_cache_sync_proto, config_hash, [], ?RPC_MS)
     || N <- Nodes
    ],
    try
        [
            ok = rpc:call(N, ?MODULE, install_phase_barrier, [request, self()], ?RPC_MS)
         || N <- [A, B]
        ],
        ok = rpc:call(C, ?MODULE, restart_sync, [], ?RPC_MS),
        {Source, Actor, Ref, _Round} = wait_phase_barrier(any),
        Other =
            case Source of
                A -> B;
                B -> A
            end,
        Before = rpc:call(Source, ?MODULE, resource_sample, [], ?RPC_MS),
        try
            [
                ok = rpc:call(
                    Other,
                    imboy_cache,
                    put,
                    [{cache_v2_test_rep, {slow_source_input, I}}, <<0:2048>>, 120, replicate],
                    ?RPC_MS
                )
             || I <- lists:seq(1, 128)
            ],
            %% 从此不再输入业务事件，只采样。真实来源 actor 仍停在request屏障。
            Peak = rpc:call(Source, ?MODULE, resource_sample, [], ?RPC_MS),
            true = maps:get(message_queue_len, Peak) > maps:get(message_queue_len, Before),
            degraded = wait_status(C, degraded, 100),
            #state{status = degraded, snapshot_round = undefined} =
                rpc:call(C, sys, get_state, [imboy_cache_sync], ?RPC_MS),
            ok = rpc:call(Other, sys, remove, [imboy_cache_sync, cache_phase], ?RPC_MS),
            ok = rpc:call(C, ?MODULE, install_completion_observer, [self()], ?RPC_MS),
            ok = release_phase_barrier(Actor, Ref),
            receive
                {cache_late_done, C, _FinishedRound} -> ok
            after 5000 -> erlang:error(slow_source_no_real_late_done)
            end,
            true = converge_pred(fun() ->
                Sample = rpc:call(Source, ?MODULE, resource_sample, [], ?RPC_MS),
                maps:get(message_queue_len, Sample) =:= 0
            end),
            After = rpc:call(Source, ?MODULE, resource_sample, [], ?RPC_MS),
            true = maps:get(message_queue_len, After) < maps:get(message_queue_len, Peak),
            true = maps:get(reductions, After) > maps:get(reductions, Before),
            %% 同发送者屏障后检查真实迟到页/done没有解除降级。
            true = converge_pred(fun() ->
                #state{snapshot_round = R} = rpc:call(
                    C, sys, get_state, [imboy_cache_sync], ?RPC_MS
                ),
                R =:= undefined
            end),
            degraded = rpc:call(C, imboy_cache_sync, status, [], ?RPC_MS),
            io:format("C9o sampled resource trend (no hard heap claim): ~p~n", [
                #{before => Before, peak => Peak, stopped => After}
            ])
        after
            Actor ! {release_cache_phase, Ref},
            _ = rpc:call(Other, sys, remove, [imboy_cache_sync, cache_phase], ?RPC_MS)
        end
    after
        [
            ok = rpc:call(N, application, set_env, [imboy, cache_replication, Cfg], ?RPC_MS)
         || {N, Cfg} <- Old
        ]
    end,
    ok.

resource_sample() ->
    Info = erlang:process_info(whereis(imboy_cache_sync), [message_queue_len, memory, reductions]),
    Usage =
        case ets:lookup(imboy_cache_meta, metadata_usage) of
            [{metadata_usage, Stored}] -> Stored;
            [] -> {0, 0, 0, 0}
        end,
    maps:from_list([{metadata_usage, Usage} | Info]).

install_completion_observer(Controller) ->
    Fun = fun
        (State, {in, {cache_snapshot_done, _, Round, _, _}}, _Extra) ->
            State ! {cache_late_done, node(), Round},
            done;
        (State, _, _) ->
            State
    end,
    sys:install(imboy_cache_sync, {cache_late_done, Fun, Controller}).

%% F3：节点硬崩（erlang:halt，无 terminate/syn:leave）+ 同名新化身：
%% 存活节点不受影响；新化身经快照回填恢复，版本上界不回退（AC-17 硬崩路径）。
f3_hard_crash_incarnation(A, B, C) ->
    Key = {cache_v2_test_rep, d_f3},
    ok = rpc:call(A, imboy_cache, put, [Key, f3_v, 300, replicate], ?RPC_MS),
    {ok, f3_v} = converge_get(B, Key, {ok, f3_v}),
    {ok, {{C1, _}, _, _, _, _}} = rpc:call(A, ?MODULE, read_rev_full, [Key], 5000),
    %% 硬崩
    catch rpc:call(C, erlang, halt, [0], 3000),
    timer:sleep(300),
    C2 = restart_c_hard(A, B),
    ready = recover_ready(C2, heal_restart(C2), 2),
    {ok, f3_v} = converge_get(C2, Key, {ok, f3_v}),
    Key2 = {cache_v2_test_rep, d_f3b},
    ok = rpc:call(C2, imboy_cache, put, [Key2, f3_new, 120, replicate], ?RPC_MS),
    {ok, {{C2c, _}, _, _, _, _}} = rpc:call(C2, ?MODULE, read_rev_full, [Key2], 5000),
    true = C2c >= C1 orelse erlang:error({ceiling_regressed, C1, C2c}),
    ok.

%% F4：有界清理（W4-06/07）受控注入——过期墓碑、过期 live 元数据连同
%% rindex 行一并淘汰；未过期条目不受影响。
f4_bounded_sweep(A, B, C) ->
    NowMs = erlang:system_time(millisecond),
    Rev = {1, node()},
    %% 直接构造三类元数据行（公共 ETS，受控注入）：
    %% ktomb = 过期墓碑且已跨完整写窗（ExpiresAt+Tmax < now）→ 连 rindex 淘汰；
    %% kexp  = 过期 live 已跨版本保护窗 → 淘汰；
    %% kguard = 最近过期 live → 保留版本，不能接受更旧的长 TTL 写入；
    %% klive = 未过期 live → 保留。
    Rows = [
        {{rev, ktomb}, {Rev, 1, NowMs - 721000, deleted, 0}},
        {{rindex, ktomb}, NowMs - 721000},
        {{rev, kexp}, {Rev, 1, NowMs - 721000, live, 0}},
        {{rindex, kexp}, NowMs - 721000},
        {{rev, kguard}, {Rev, 1, NowMs - 1000, live, 0}},
        {{rindex, kguard}, NowMs - 1000},
        {{rev, klive}, {Rev, 1, NowMs + 60000, live, 0}},
        {{rindex, klive}, NowMs}
    ],
    ok = rpc:call(C, ?MODULE, meta_inject, [Rows], 5000),
    %% 直发 reconcile_tick 触发 bounded_sweep（不必等 60s 周期）
    rpc:call(C, erlang, send, [imboy_cache_sync, reconcile_tick], 5000),
    ok = rpc:call(C, ?MODULE, drain_sync_mailbox, [], ?RPC_MS),
    gone = rpc:call(C, ?MODULE, meta_probe, [ktomb], 5000),
    gone = rpc:call(C, ?MODULE, meta_probe, [kexp], 5000),
    kept = rpc:call(C, ?MODULE, meta_probe, [kguard], 5000),
    kept = rpc:call(C, ?MODULE, meta_probe, [klive], 5000),
    ok.

%% F5：提前逐出（W4-08 功能性回归）。C 以小容量 depcache（1MB）经快照恢复
%% 后被 depcache 随机逐出裁剪为存活子集；随后 B 仅从 C 恢复——
%% send_snapshot 的 valid_live_entry 必须跳过已被逐出（depcache 无值）的
%% 条目：B 的内容与 C 的存活子集完全一致，被逐出的键不得复活。
f5_early_evict_survivors_only(A, B, C) ->
    [ready = wait_status(N, 300) || N <- [A, B, C]],
    %% 1) A 灌入 400 行 × 2000 元素列表（ets:info(memory) 按 cons cell 计 words，
    %% 每行约 4000 words ≈ 32KB；合计约 12MB ≫ C 的小容量 1MB——refc binary
    %% 不计入 ets 内存，此前 4KB binary 版本永不触发逐出）。值经 V2 大小校验
    %% （external ≈ 6KB < 32KB 上限）。
    Keys = [{cache_v2_test_rep, {f5, I}} || I <- lists:seq(1, 400)],
    Val = lists:seq(1, 2000),
    lists:foreach(
        fun(K) ->
            ok = rpc:call(A, imboy_cache, put, [K, Val, 300, replicate], ?RPC_MS)
        end,
        Keys
    ),
    {ok, Val} = converge_get(B, lists:last(Keys), {ok, Val}),
    %% 2) C 换小容量 depcache 重启 → 快照回填后被随机逐出裁剪
    ok = rpc:call(C, ?MODULE, peer_sync_down, [], ?RPC_MS),
    ok = rpc:call(C, ?MODULE, init_on_peer_small, [], ?RPC_MS),
    HealCSmall =
        fun() ->
            ok = rpc:call(C, ?MODULE, peer_sync_down, [], ?RPC_MS),
            ok = rpc:call(C, ?MODULE, init_on_peer_small, [], ?RPC_MS)
        end,
    ready = recover_ready(C, HealCSmall, 2),
    Retained = count_retained(C, Keys, Val, 200),
    true = Retained < length(Keys) orelse erlang:error({no_eviction, Retained}),
    %% 3) A 下线，B 仅从 C（存活子集持有者）恢复
    ok = rpc:call(A, ?MODULE, peer_sync_down, [], ?RPC_MS),
    ok = rpc:call(B, ?MODULE, restart_sync, [], ?RPC_MS),
    %% 诊断开关 DCHECK_TRACE=1：trace 两端 sync 进程收发（stdout）
    TraceOn =
        case os:getenv("DCHECK_TRACE") of
            "1" ->
                %% collector 各节点本地起（tracer 必须本地）；其 group_leader
                %% 经 rpc 链回到 driver，io:format 输出进 driver 日志
                LocalCollector = fun(Node) ->
                    rpc:call(Node, erlang, spawn, [fun collect_trace_loop/0], ?RPC_MS)
                end,
                {Bc, Cc} = {LocalCollector(B), LocalCollector(C)},
                {Bp, Cp} = {
                    rpc:call(B, erlang, whereis, [imboy_cache_sync], ?RPC_MS),
                    rpc:call(C, erlang, whereis, [imboy_cache_sync], ?RPC_MS)
                },
                1 = rpc:call(
                    B, erlang, trace, [Bp, true, ['receive', send, {tracer, Bc}]], ?RPC_MS
                ),
                1 = rpc:call(
                    C, erlang, trace, [Cp, true, ['receive', send, {tracer, Cc}]], ?RPC_MS
                ),
                true;
            _ ->
                false
        end,
    case catch recover_ready(B, heal_restart(B), 4) of
        ready ->
            ok;
        Other ->
            %% 诊断（C09 根因定位）：B 视图里的 C pid vs C 实际 sync pid
            Diag = #{
                b_view => rpc:call(B, ?MODULE, sync_view, [], ?RPC_MS),
                c_actual_sync => rpc:call(C, erlang, whereis, [imboy_cache_sync], ?RPC_MS),
                c_status => rpc:call(C, imboy_cache_sync, status, [], ?RPC_MS),
                c_view => rpc:call(C, ?MODULE, sync_view, [], ?RPC_MS)
            },
            erlang:error({f5_b_recover_failed, Other, Diag})
    end,
    case TraceOn of
        true ->
            {_, _} = rpc:multicall([B, C], erlang, trace, [all, false, ['receive', send]], 5000),
            ok;
        false ->
            ok
    end,
    %% 逐键比对：B 的存在性 == C 的存在性（被逐出键不复活、存活键不丢失）
    lists:foreach(
        fun(K) ->
            VC = rpc:call(C, imboy_cache, get, [K], 5000),
            VB = rpc:call(B, imboy_cache, get, [K], 5000),
            case {VC =/= undefined, VB =/= undefined} of
                {Same, Same} when Same =:= true orelse Same =:= false -> ok;
                _ -> erlang:error({survivor_mismatch, K, VC, VB})
            end
        end,
        Keys
    ),
    %% 4) A 回归，集群互通恢复（after 兜底同 F1）
    try
        ready = recover_ready(A, heal_restart(A), 4),
        KeyNew = {cache_v2_test_rep, {f5, new}},
        ok = rpc:call(A, imboy_cache, put, [KeyNew, fresh, 120, replicate], ?RPC_MS),
        {ok, fresh} = converge_get(C, KeyNew, {ok, fresh})
    after
        catch rpc:call(A, ?MODULE, restart_sync, [], ?RPC_MS)
    end,
    ok.

%% 恢复收敛（含自愈）：syn 视图传播滞后可饿死整个版本应答轮（peer 视图中
%% 尚无恢复节点的新 pid → 不应答 → 有界重试耗尽 → DEGRADED，fail-closed）。
%% 产品重试预算按计划冻结不改；测试与运维路径对齐——超时未 ready 则节点级
%% 重启 sync（视图此时已热）再收敛，仍失败则带状态转储报错。
recover_ready(Node, _Heal, 0) ->
    erlang:error({recovery_timeout, Node, rpc:call(Node, ?MODULE, dump_debug, [], 5000)});
recover_ready(Node, Heal, N) ->
    case wait_status(Node, 400) of
        ready ->
            ready;
        _ ->
            ok = Heal(),
            recover_ready(Node, Heal, N - 1)
    end.

heal_restart(Node) ->
    fun() -> ok = rpc:call(Node, ?MODULE, restart_sync, [], ?RPC_MS) end.

count_retained(_C, _Keys, _Val, 0) ->
    erlang:error(eviction_settle_timeout);
count_retained(C, Keys, Val, Tries) ->
    N = length([ok || K <- Keys, rpc:call(C, imboy_cache, get, [K], 5000) =:= {ok, Val}]),
    case N < length(Keys) of
        true ->
            N;
        false ->
            timer:sleep(250),
            count_retained(C, Keys, Val, Tries - 1)
    end.

%% ===================================================================
%% Cluster 管理（结构经独立实验验证）
%% ===================================================================

start_cluster() ->
    ok = new_driver_context(),
    try
        boot_cluster()
    catch
        Class:Reason:Stack ->
            stop_cluster([]),
            erlang:raise(Class, Reason, Stack)
    end.

boot_cluster() ->
    %% 节点名只用安全字符；随机 cookie；loopback 分布接口
    RandBin = binary:encode_hex(crypto:strong_rand_bytes(6), lowercase),
    Rand = binary_to_atom(iolist_to_binary("c21" ++ binary_to_list(RandBin)), utf8),
    Main = binary_to_atom(iolist_to_binary("c21m" ++ binary_to_list(RandBin)), utf8),
    {ok, _} = net_kernel:start([Main, longnames]),
    true = ets:insert(c21dctx, {owned_net_kernel, true}),
    CookieAtom = binary_to_atom(
        iolist_to_binary(
            "ck" ++ binary_to_list(integer_to_binary(erlang:unique_integer([positive]), 36))
        ),
        utf8
    ),
    true = erlang:set_cookie(node(), CookieAtom),
    Ebin = filename:absname("ebin"),
    %% driver 实际加载 dcheck 的目录（独立构建产物，peer 继承同一路径，
    %% 避免主 ebin 被 eunit test-build 清理导致 peer undef）
    DrvDir =
        case code:which(cache_v21_dcheck) of
            Path when is_list(Path) -> filename:dirname(Path);
            _ -> Ebin
        end,
    Deps = [DrvDir | [filename:absname("deps/" ++ D ++ "/ebin") || D <- ["depcache", "syn"]]],
    Args = [
        "-setcookie",
        atom_to_list(CookieAtom),
        "-kernel",
        "inet_dist_use_interface",
        "{127,0,0,1}",
        %% C09 实证：driver 也身处 global 网络，集群拓扑变化（重启/恢复）会
        %% 触发 global"防重叠分区"预防性断连（driver↔peer 间歇 nodedown，
        %% C9e/C9f 实证）。回环全连通的测试集群禁用该防护是标准做法。
        "prevent_overlapping_partitions",
        "false",
        "-pa",
        Ebin,
        "-pa"
        | Deps
    ],
    NameA = binary_to_atom(iolist_to_binary(io_lib:format("~p_a", [Rand])), utf8),
    NameB = binary_to_atom(iolist_to_binary(io_lib:format("~p_b", [Rand])), utf8),
    NameC = binary_to_atom(iolist_to_binary(io_lib:format("~p_c", [Rand])), utf8),
    %% wait_boot => false：异步启动，由主驱动 ping 轮询确认可达
    {ok, PA, A} = peer:start(#{
        name => NameA,
        host => "127.0.0.1",
        wait_boot => false,
        args => Args
    }),
    ok = register_owned_peer(PA, A),
    pong = wait_peer(A, 200),
    {ok, PB, B} = peer:start(#{
        name => NameB,
        host => "127.0.0.1",
        wait_boot => false,
        args => Args
    }),
    ok = register_owned_peer(PB, B),
    pong = wait_peer(B, 200),
    {ok, PC, C} = peer:start(#{
        name => NameC,
        host => "127.0.0.1",
        wait_boot => false,
        args => Args
    }),
    ok = register_owned_peer(PC, C),
    pong = wait_peer(C, 200),
    %% F3 同名硬重启所需身份（驱动节点侧暂存）
    true = ets:insert(c21dctx, {c, {NameC, Args}}),
    true = rpc:call(A, net_kernel, connect_node, [B], ?RPC_MS),
    true = rpc:call(A, net_kernel, connect_node, [C], ?RPC_MS),
    true = rpc:call(B, net_kernel, connect_node, [C], ?RPC_MS),
    ok = rpc:call(A, ?MODULE, init_on_peer, [], ?RPC_MS),
    %% 首节点以默认空 cluster_nodes 的单节点配置建立权威，再加入其他节点。
    %% 全节点同时 recovering 不再允许空 done 相互放行为 READY。
    ready = wait_status(A, 400),
    ok = rpc:call(B, ?MODULE, init_on_peer, [], ?RPC_MS),
    ok = rpc:call(C, ?MODULE, init_on_peer, [], ?RPC_MS),
    ready = wait_status(A, 400),
    ready = wait_status(B, 400),
    ready = wait_status(C, 400),
    try
        case confirm_initial_identities([A, B, C]) of
            true -> {ok, [PA, PB, PC], {A, B, C}};
            false -> erlang:error(initial_identity_handshake_timeout)
        end
    catch
        Class:Reason:Stack ->
            stop_cluster([PA, PB, PC]),
            erlang:raise(Class, Reason, Stack)
    end.

%% READY 是本节点恢复状态；健康流前置还须验证互相确认当前 Boot。
%% 使用真实 reconcile/report，不注入身份、不重启、不清副本。
confirm_initial_identities(Nodes) ->
    ViewReady = converge_pred(fun() ->
        lists:all(
            fun(N) ->
                lists:sort([Peer || {Peer, _, _} <- rpc:call(N, ?MODULE, sync_view, [], ?RPC_MS)]) =:=
                    lists:sort(Nodes)
            end,
            Nodes
        )
    end),
    case ViewReady of
        false ->
            false;
        true ->
            Boots = [
                {N, element(2, rpc:call(N, imboy_cache_sync, node_boot, [], ?RPC_MS))}
             || N <- Nodes
            ],
            [
                reconcile_tick = rpc:call(
                    N,
                    erlang,
                    send,
                    [imboy_cache_sync, reconcile_tick],
                    ?RPC_MS
                )
             || N <- Nodes
            ],
            converge_pred(fun() ->
                lists:all(
                    fun({N, Peer, Boot}) ->
                        rpc:call(
                            N,
                            ets,
                            lookup,
                            [imboy_cache_meta, {peer_incarnation, Peer}],
                            ?RPC_MS
                        ) =:= [{{peer_incarnation, Peer}, Boot}]
                    end,
                    [{N, Peer, Boot} || N <- Nodes, {Peer, Boot} <- Boots, N =/= Peer]
                )
            end)
    end.

%% F3：C 硬崩后的同名新化身（同一 args/代码树，新 BootId）
restart_c_hard(A, B) ->
    [{c, {NameC, Args}}] = ets:lookup(c21dctx, c),
    {ok, Replacement, C2} = peer:start(#{
        name => NameC,
        host => "127.0.0.1",
        wait_boot => false,
        args => Args
    }),
    ok = register_owned_peer(Replacement, C2),
    pong = wait_peer(C2, 200),
    true = connect_retry(A, C2),
    true = connect_retry(B, C2),
    true = rpc:call(C2, net_kernel, connect_node, [A], ?RPC_MS),
    true = rpc:call(C2, net_kernel, connect_node, [B], ?RPC_MS),
    ok = rpc:call(C2, ?MODULE, init_on_peer, [], ?RPC_MS),
    C2.

connect_retry(From, To) ->
    connect_retry(From, To, 15).

connect_retry(_From, _To, 0) ->
    erlang:error({connect_timeout, _From, _To});
connect_retry(From, To, N) ->
    case rpc:call(From, net_kernel, connect_node, [To], ?RPC_MS) of
        true ->
            true;
        _ ->
            timer:sleep(200),
            connect_retry(From, To, N - 1)
    end.

new_driver_context() ->
    case ets:whereis(c21dctx) of
        undefined ->
            c21dctx = ets:new(c21dctx, [set, named_table, public]),
            ok;
        _ ->
            erlang:error(driver_context_not_cleaned)
    end.

register_owned_peer(Pid, Node) ->
    true = ets:info(c21dctx, owner) =:= self(),
    true = ets:insert(c21dctx, {{owned_peer, Pid}, Node}),
    ok.

stop_cluster(PeerPids) ->
    case ets:info(c21dctx, owner) of
        undefined when PeerPids =:= [] -> ok;
        Owner when Owner =:= self() ->
            Owned = [P || {{owned_peer, P}, _Node} <- ets:tab2list(c21dctx)],
            true = lists:all(fun(P) -> lists:member(P, Owned) end, PeerPids),
            [ok = stop_owned_peer(P) || P <- Owned],
            case ets:lookup(c21dctx, owned_net_kernel) of
                [{owned_net_kernel, true}] -> ok = net_kernel:stop();
                [] -> ok
            end,
            true = ets:delete(c21dctx),
            ok;
        _ ->
            erlang:error(foreign_driver_context)
    end.

stop_owned_peer(Pid) ->
    Ref = erlang:monitor(process, Pid),
    case is_process_alive(Pid) of
        true ->
            try
                peer:stop(Pid)
            catch
                Class:Reason:Stack ->
                    case is_process_alive(Pid) of
                        false -> ok;
                        true -> erlang:raise(Class, Reason, Stack)
                    end
            end;
        false ->
            ok
    end,
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 5000 -> erlang:error({owned_peer_not_stopped, Pid})
    end.

wait_peer(Full, 0) ->
    erlang:error({peer_ping_timeout, Full});
wait_peer(Full, N) ->
    case net_adm:ping(Full) of
        pong ->
            pong;
        pang ->
            timer:sleep(100),
            wait_peer(Full, N - 1)
    end.

%% ===================================================================
%% Peer-side helpers（rpc 到 peer 执行）
%% ===================================================================

init_on_peer() ->
    ok = application:load(imboy),
    %% 隔离测试 namespace（不进生产默认配置）
    application:set_env(
        imboy,
        cache_registry_overrides,
        #{cache_v2_test_rep => {replicate, false}}
    ),
    application:set_env(imboy, cache_replication, #{whitelist => [cache_v2_test_rep]}),
    application:set_env(imboy, cache_fenced_namespaces, [cache_v2_test, cache_v2_test_rep]),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(syn),
    ok = syn:add_node_to_scopes([imboy_cache]),
    %% 独立无监督进程（测试 peer 专用；unlink 使 rpc 临时进程退出不连坐）
    {ok, Dp} = depcache:start_link(imboy_cache, #{memory_max => 64}),
    true = unlink(Dp),
    {ok, M} = elib_metric:start_link(),
    true = unlink(M),
    {ok, S} = imboy_cache_sync:start_link(),
    true = unlink(S),
    ok = wait_local_sync(100).

%% F5：小容量 depcache 变体（1MB，触发 depcache 随机逐出）。供节点已运行
%% 过一轮 init_on_peer 后复用（crypto/syn/elib_metric 仍在，需幂等）。
init_on_peer_small() ->
    %% C 已加载过 imboy（首次 init_on_peer）：already_loaded 幂等放行
    ok =
        case application:load(imboy) of
            ok -> ok;
            {error, {already_loaded, imboy}} -> ok
        end,
    application:set_env(
        imboy,
        cache_registry_overrides,
        #{cache_v2_test_rep => {replicate, false}}
    ),
    application:set_env(imboy, cache_replication, #{whitelist => [cache_v2_test_rep]}),
    application:set_env(imboy, cache_fenced_namespaces, [cache_v2_test, cache_v2_test_rep]),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(syn),
    ok = syn:add_node_to_scopes([imboy_cache]),
    {ok, Dp} = depcache:start_link(imboy_cache, #{memory_max => 1}),
    true = unlink(Dp),
    case whereis(elib_metric) of
        undefined ->
            {ok, M} = elib_metric:start_link(),
            true = unlink(M);
        _ ->
            already_running
    end,
    {ok, S} = imboy_cache_sync:start_link(),
    true = unlink(S),
    ok = wait_local_sync(100).

read_rev(Key) ->
    case ets:lookup(imboy_cache_meta, {rev, Key}) of
        [{{rev, Key}, {Rev, _C, _Exp, _St, _Epoch}}] -> {ok, Rev};
        [] -> {error, not_found}
    end.

read_expires(Key) ->
    case ets:lookup(imboy_cache_meta, {rev, Key}) of
        [{{rev, Key}, {_Rev, _C, Exp, _St, _Epoch}}] -> {ok, Exp};
        [] -> {error, not_found}
    end.

read_rev_full(Key) ->
    case ets:lookup(imboy_cache_meta, {rev, Key}) of
        [{{rev, Key}, {Rev, C, Exp, St, Epoch}}] -> {ok, {Rev, C, Exp, St, Epoch}};
        [] -> {error, not_found}
    end.

%% 定向注入 V2 事件（模拟延迟/重复消息；OriginNode 避开接收端自环过滤）
inject_v2_set(Key, Value, Rev, OriginNode) ->
    inject_v2_set(Key, Value, Rev, OriginNode, 0).

%% C01 冻结协议字段版本（收口 C09）：epoch/hash 现场计算（同配置 peer 匹配）
inject_v2_set(Key, Value, Rev, OriginNode, Seq) ->
    %% 延迟/重复事件沿用真实来源化身，不伪造新 Boot 或虚构高序列。
    {OriginNode, Boot} = rpc:call(OriginNode, imboy_cache_sync, node_boot, [], ?RPC_MS),
    inject_v2_set(Key, Value, Rev, OriginNode, Seq, Boot).

%% 显式化身版本（收口 C09）：C9d 用真实 peer 化身驱动 changed 补偿
inject_v2_set(Key, Value, Rev, OriginNode, Seq, BootId) ->
    Event = #{
        protocol => 2,
        operation => set,
        namespace => imboy_cache_policy:namespace_of(Key),
        key => Key,
        value => Value,
        expires_at_ms => erlang:system_time(millisecond) + 120000,
        revision => Rev,
        origin_seq => Seq,
        origin_node => OriginNode,
        origin_boot_id => BootId,
        %% C01 冻结新增字段（收口 C09 实证：缺任一即 missing_protocol_field
        %% 被拒，C9a~d 的 oracle 全部失真——必须现场计算补齐）
        epoch => imboy_cache_sync_proto:epoch(),
        config_hash => imboy_cache_sync_proto:config_hash(),
        registry_hash => imboy_cache_sync_proto:registry_hash()
    },
    _ = imboy_cache_sync:broadcast(Event),
    ok.

%% 模拟真实节点重启：sync 与 depcache 实例一并重启（内存副本全丢，
%% 含版本元数据），新 BootId；随后恢复流程 discover→version→snapshot。
%% V2 能力下线：sync+depcache 停止且不重启（F1 恢复轮故障注入）
peer_sync_down() ->
    case whereis(imboy_cache_sync) of
        undefined -> ok;
        _ -> ok = gen_server:stop(imboy_cache_sync, normal, 10000)
    end,
    case whereis(imboy_cache) of
        undefined ->
            ok;
        DepP ->
            Ref = monitor(process, DepP),
            exit(DepP, kill),
            receive
                {'DOWN', Ref, process, DepP, _} -> ok
            after 5000 -> erlang:error(depcache_kill_timeout)
            end
    end,
    ok.

%% F4 受控注入：直接写入公共元数据表
meta_inject(Rows) ->
    true = ets:insert(imboy_cache_meta, Rows),
    ok.

sync_alive() ->
    case whereis(imboy_cache_sync) of
        P when is_pid(P) -> is_process_alive(P);
        undefined -> false
    end.

%% 诊断（C09）：本节点视角的产品组视图（谁在组里、pid 是什么）
sync_view() ->
    [
        {node(P), P, whereis(imboy_cache_sync) =:= P}
     || {P, _} <- syn:members(imboy_cache, dsync_handler)
    ].

%% trace 收集器：把 B/C 两端 sync 进程的收发事件打到 stdout
%% 诊断辅助：hash binary 前 8 字节转 tuple（~p 对 integer list 会因
%% latin1 可打印性字符串化污染 JSON，tuple 永不字符串化）
hash_head(<<A:8, B:8, C:8, D:8, E:8, F:8, G:8, H:8, _/binary>>) ->
    {A, B, C, D, E, F, G, H};
hash_head(Other) ->
    Other.

collect_trace_loop() ->
    receive
        Ev when element(1, Ev) =:= trace ->
            io:format("TRACE ~p~n", [Ev]),
            collect_trace_loop();
        _Other ->
            collect_trace_loop()
    after 120000 -> ok
    end.

%% F4 探针：该键的 rev+rindex 行是否存在（gone/kept）
meta_probe(K) ->
    HasRev = ets:member(imboy_cache_meta, {rev, K}),
    HasIdx = ets:member(imboy_cache_meta, {rindex, K}),
    case {HasRev, HasIdx} of
        {false, false} -> gone;
        {true, true} -> kept;
        {_, _} -> partial
    end.

restart_sync() ->
    %% F1 场景中 sync 可能已被 peer_sync_down 停掉，stop 需守卫
    case whereis(imboy_cache_sync) of
        undefined -> ok;
        _ -> ok = gen_server:stop(imboy_cache_sync, normal, 10000)
    end,
    case whereis(imboy_cache) of
        undefined ->
            ok;
        DepP ->
            Ref = monitor(process, DepP),
            exit(DepP, kill),
            receive
                {'DOWN', Ref, process, DepP, _} -> ok
            after 5000 -> erlang:error(depcache_kill_timeout)
            end
    end,
    ok = retry_start(30).

retry_start(0) ->
    {error, cannot_restart};
retry_start(N) ->
    DepOk =
        case whereis(imboy_cache) of
            undefined ->
                case depcache:start_link(imboy_cache, #{memory_max => 64}) of
                    {ok, D} ->
                        true = unlink(D),
                        true;
                    _ ->
                        false
                end;
            _ ->
                true
        end,
    SyncOk =
        case whereis(imboy_cache_sync) of
            undefined ->
                case imboy_cache_sync:start_link() of
                    {ok, P} ->
                        true = unlink(P),
                        true;
                    {error, {already_started, _}} ->
                        true;
                    _ ->
                        false
                end;
            _ ->
                true
        end,
    case DepOk andalso SyncOk of
        true ->
            ok;
        false ->
            timer:sleep(100),
            retry_start(N - 1)
    end.

%% 过载探针：suspend 的所有权绑定调用进程（调用者退出自动 resume），
%% 因此挂起→压队列→put→恢复必须在同一存活进程内完成。
overload_probe() ->
    Sync = whereis(imboy_cache_sync),
    erlang:suspend_process(Sync),
    try
        [Sync ! {cache_sync, flood_marker_X} || _ <- lists:seq(1, 12000)],
        imboy_cache:put({cache_v2_test_rep, d_overload1}, v, 120, replicate)
    after
        erlang:resume_process(Sync)
    end.

drain_sync_mailbox() ->
    pong = gen_server:call(imboy_cache_sync, ping_ok, 60000),
    ok.

rindex_dump() ->
    [K || [K] <- ets:match(imboy_cache_meta, {{rindex, '$1'}, '_'})].

converge_get_quiet(Node, Key, Expected, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    converge_quiet_loop(Node, Key, Expected, Deadline).

converge_quiet_loop(Node, Key, Expected, Deadline) ->
    V = rpc:call(Node, imboy_cache, get, [Key], 5000),
    case V of
        Expected ->
            Expected;
        _ ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true ->
                    V;
                false ->
                    timer:sleep(?POLL_MS),
                    converge_quiet_loop(Node, Key, Expected, Deadline)
            end
    end.

dump_debug() ->
    %% 诊断用产品同款 scope/group（chat.hrl: CACHE_SCOPE=imboy_cache）
    SyncMembers = [{node(P), P} || {P, _Meta} <- syn:members(imboy_cache, dsync_handler)],
    #{
        status => imboy_cache_sync:status(),
        sync_members => SyncMembers,
        meta => imboy_cache_sync:meta_snapshot()
    }.

wait_local_sync(0) ->
    {error, sync_not_started};
wait_local_sync(N) ->
    case whereis(imboy_cache_sync) of
        undefined ->
            timer:sleep(100),
            wait_local_sync(N - 1);
        _ ->
            ok
    end.

%% ===================================================================
%% 收敛等待（等待协议收敛条件，非固定 sleep 断言）
%% ===================================================================

converge_get(Node, Key, Expected) ->
    converge_get(Node, Key, Expected, ?CONVERGE_MS).

converge_get(Node, Key, Expected, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    converge_get_loop(Node, Key, Expected, Deadline).

converge_get_loop(Node, Key, Expected, Deadline) ->
    case rpc:call(Node, imboy_cache, get, [Key], 5000) of
        Expected ->
            Expected;
        Other ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true ->
                    erlang:error({converge_timeout, Node, Key, Expected, Other});
                false ->
                    timer:sleep(?POLL_MS),
                    converge_get_loop(Node, Key, Expected, Deadline)
            end
    end.

converge_pred(Pred) ->
    Deadline = erlang:monotonic_time(millisecond) + ?CONVERGE_MS,
    converge_pred_loop(Pred, Deadline).

converge_pred_loop(Pred, Deadline) ->
    case Pred() of
        true ->
            true;
        false ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true ->
                    false;
                false ->
                    timer:sleep(?POLL_MS),
                    converge_pred_loop(Pred, Deadline)
            end
    end.

wait_status(Node, N) ->
    wait_status(Node, ready, N).

wait_status(_Node, _Expected, 0) ->
    timeout;
wait_status(Node, Expected, N) ->
    case rpc:call(Node, imboy_cache_sync, status, [], 5000) of
        Expected ->
            Expected;
        _ ->
            timer:sleep(100),
            wait_status(Node, Expected, N - 1)
    end.

%% ===================================================================
%% JSON 输出（无外部依赖的最小实现）
%% ===================================================================

results_json(Results, ElapsedMs) ->
    Items = [
        io_lib:format(
            "{\"name\": \"~s\", \"status\": \"~s\", \"detail\": \"~s\"}",
            [Name, Status, escape(Detail)]
        )
     || {Name, Status, Detail} <- Results
    ],
    %% C14 证据 schema：candidate 绑定由环境变量注入（C12 冻结时计算并
    %% 导出），缺失记 null（校验器对收口系列缺绑定判 FAIL，逐轮可追溯）
    Candidate =
        case os:getenv("CANDIDATE_SHA256") of
            false ->
                "null";
            Sha when length(Sha) =:= 64 ->
                io_lib:format("\"~s\"", [Sha]);
            Bad ->
                io:format("CANDIDATE_SHA256 malformed (~p), expect 64-hex~n", [Bad]),
                halt(3)
        end,
    io_lib:format(
        "{\"task_id\": \"CACHE_V221_BACKEND_CLOSEOUT_V1\", "
        "\"candidate_sha256\": ~s, \"driver_node\": \"~s\", \"elapsed_ms\": ~p, "
        "\"utc\": \"~s\", \"scenarios\": [~s]}",
        [
            Candidate,
            escape(atom_to_list(node())),
            ElapsedMs,
            calendar:system_time_to_rfc3339(
                erlang:system_time(millisecond),
                [{unit, millisecond}, {offset, "Z"}]
            ),
            string:join(Items, ",")
        ]
    ).

escape(Bin) when is_binary(Bin) -> escape(binary_to_list(Bin));
escape(L) when is_list(L) ->
    %% 先拍平：~p 产出的是 io_lib 深列表，子列表会被下面的推导当作单个
    %% "字符"整段跳过转义（真实换行/引号直达 JSON——f1 轮实证）
    Flat = lists:flatten(L),
    lists:flatten([
        case Ch of
            $" -> "\\\"";
            $\\ -> "\\\\";
            $\n -> "\\n";
            $\t -> "\\t";
            C when C < 16#20 -> io_lib:format("\\u~4.16.0b", [C]);
            %% C09 实证：~p 会把"全部 latin1 可打印"的随机 binary/list
            %% 打成原文字符串（boot/hash 偶发命中），>=128 字节直接污染
            %% JSON（UTF-8 解码失败）。字节级消毒：高位一律 \uXXXX
            C when C >= 16#80 -> io_lib:format("\\u~4.16.0b", [C]);
            C -> [C]
        end
     || Ch <- Flat
    ]).

%% ===================================================================
%% C09 收口场景（C02 双端一致性 / C03 预算 / C05 补偿）
%% ===================================================================

%% C02：B 推进 epoch 后，A 的旧 epoch 复制事件被 B 拒绝（值不被修改）。

%% C9 前置自愈：F5 等前序场景可能留下非 ready 的节点状态；
%% C9 合同验证须独立于前序残留（ensure ready，否则 restart_sync 后等待）。
ensure_c9_ready(Node) ->
    case rpc:call(Node, imboy_cache_sync, status, [], ?RPC_MS) of
        ready ->
            ok;
        _ ->
            ok = rpc:call(Node, ?MODULE, restart_sync, [], ?RPC_MS),
            ready = recover_ready(Node, heal_restart(Node), 4),
            ok
    end.

c9a_stale_epoch(A, B) ->
    ok = ensure_c9_ready(A),
    ok = ensure_c9_ready(B),
    Key = {cache_v2_test_rep, c9a_epoch},
    ok = rpc:call(A, imboy_cache, put, [Key, v1, 120, replicate], ?RPC_MS),
    {ok, v1} = converge_get(B, Key, {ok, v1}),
    Before = rpc:call(B, ?MODULE, read_rev_full, [Key], 5000),
    %% B 推进 epoch 至 1：A（epoch=0）的后续事件对 B 是 stale_epoch
    1 = rpc:call(B, imboy_cache_sync_proto, bump_epoch, [], ?RPC_MS),
    try
        ok = rpc:call(A, imboy_cache, put, [Key, v2, 120, replicate], ?RPC_MS),
        timer:sleep(1000),
        %% 旧代值不可公开读取；版本和底层值均未被旧事件修改。
        undefined = rpc:call(B, imboy_cache, get, [Key], ?RPC_MS),
        Before = rpc:call(B, ?MODULE, read_rev_full, [Key], 5000),
        {ok, v1} = rpc:call(B, depcache, get, [Key, imboy_cache], ?RPC_MS)
    after
        %% 本场景失败仍恢复 epoch，避免污染后续场景。
        true = rpc:call(B, ets, insert, [imboy_cache_meta, {sync_epoch, 0}], ?RPC_MS)
    end,
    ok.

%% C02：B 侧 registry 配置漂移（hash 不一致）后，A 的事件被 B 拒绝。
c9b_hash_mismatch(A, B) ->
    ok = ensure_c9_ready(A),
    ok = ensure_c9_ready(B),
    Key = {cache_v2_test_rep, c9b_hash},
    ok = rpc:call(A, imboy_cache, put, [Key, v1, 120, replicate], ?RPC_MS),
    {ok, v1} = converge_get(B, Key, {ok, v1}),
    %% B 改 registry override：hash 漂移
    rpc:call(
        B,
        application,
        set_env,
        [
            imboy,
            cache_registry_overrides,
            #{cache_v2_test_rep => {replicate, false}, drift_ns => {local, false}}
        ],
        ?RPC_MS
    ),
    ok = rpc:call(A, imboy_cache, put, [Key, v2, 120, replicate], ?RPC_MS),
    timer:sleep(1000),
    {ok, v1} = rpc:call(B, imboy_cache, get, [Key], ?RPC_MS),
    %% 恢复配置
    rpc:call(
        B,
        application,
        set_env,
        [
            imboy,
            cache_registry_overrides,
            #{cache_v2_test_rep => {replicate, false}}
        ],
        ?RPC_MS
    ),
    true = rpc:call(B, ets, delete, [imboy_cache_meta, {hash_divergent, A}], ?RPC_MS),
    ok.

%% C05：origin_seq 跳号触发 fenced key 的 generation 补偿（不依赖 TTL）。
c9c_gap_compensation(A, B) ->
    ok = ensure_c9_ready(A),
    ok = ensure_c9_ready(B),
    Key = {cache_v2_test_rep, c9c_gap},
    ok = rpc:call(A, imboy_cache, put, [Key, <<"initial">>, 120, replicate], ?RPC_MS),
    {ok, <<"initial">>} = converge_get(B, Key, {ok, <<"initial">>}),
    G0 = rpc:call(B, imboy_cache_sync, generation, [Key], ?RPC_MS),
    Members = rpc:call(B, imboy_cache_sync, syn_scope_members, [], ?RPC_MS),
    Others = lists:usort([node(P) || P <- Members, node(P) =/= B]),
    Cookie = erlang:get_cookie(),
    [true = rpc:call(B, erlang, set_cookie, [N, gap_partition], ?RPC_MS) || N <- Others],
    try
        [rpc:call(B, erlang, disconnect_node, [N], ?RPC_MS) || N <- Others],
        ok = rpc:call(A, imboy_cache, put, [Key, <<"missed">>, 120, replicate], ?RPC_MS),
        {ok, <<"initial">>} = rpc:call(B, imboy_cache, get, [Key], ?RPC_MS)
    after
        [true = rpc:call(B, erlang, set_cookie, [N, Cookie], ?RPC_MS) || N <- Others],
        [true = rpc:call(B, net_kernel, connect_node, [N], ?RPC_MS) || N <- Others]
    end,
    %% 新事件暴露中间缺口；A 保留权威新值，不向所有节点注入虚构来源。
    ok = rpc:call(A, imboy_cache, put, [Key, <<"latest">>, 120, replicate], ?RPC_MS),
    G1 = converge_generation(B, Key, G0),
    true = G1 > G0,
    {ok, <<"latest">>} = converge_get(B, Key, {ok, <<"latest">>}),
    ready = rpc:call(A, imboy_cache_sync, status, [], ?RPC_MS),
    ok.

%% C05：peer 化身变化（A 的 sync 重启）触发 B 的 key 级 fenced 补偿。
c9d_incarnation_compensation(A, B) ->
    ok = ensure_c9_ready(A),
    ok = ensure_c9_ready(B),
    %% FKey 必须用授权测试域 cache_v2_test_rep：cache_v2_test 不在
    %% registry（validate 的 not_authorized 拒——19.5 fail-closed 正确防御，
    %% r30 三节点实证 reject_total=1 即此拒因）
    FKey = {cache_v2_test_rep, c9d_inc},
    G0 = rpc:call(B, imboy_cache_sync, generation, [FKey], ?RPC_MS),
    %% A 重启 sync（新化身；restart_sync 在 peer 本地原子完成
    %% stop+start+unlink，rpc 分离的 unlink 有 normal 退出传播竞态）
    ok = rpc:call(A, ?MODULE, restart_sync, [], ?RPC_MS),
    ready = wait_status(A, 400),
    %% A 新化身事件到达 B → changed → key 级补偿
    %% （事件必须携带 A 的真实新化身：默认 <<0:128>> 是快照回放伪装特例，
    %%  化身层放行不补偿，C9d 的 oracle 就永不成立）
    {_, NewBoot} = rpc:call(A, imboy_cache_sync, node_boot, [], ?RPC_MS),
    true = is_binary(NewBoot) andalso byte_size(NewBoot) > 0,
    ok = rpc:call(
        A, ?MODULE, inject_v2_set, [FKey, <<"newinc">>, {9101, A}, A, 1, NewBoot], ?RPC_MS
    ),
    G1 = converge_generation(B, FKey, G0),
    case G1 > G0 of
        true ->
            ok;
        false ->
            %% 诊断（C09）：B 侧化身表与 FKey 栅栏状态
            RejectCnt =
                case rpc:call(B, elib_metric, get_all_metrics, [], ?RPC_MS) of
                    AM when is_map(AM) ->
                        maps:get(reject_total, maps:get(counters, AM, #{}), -1);
                    _ ->
                        rpc_failed
                end,
            ReceiveCnt =
                case rpc:call(B, elib_metric, get_all_metrics, [], ?RPC_MS) of
                    AM2 when is_map(AM2) ->
                        maps:get(receive_total, maps:get(counters, AM2, #{}), -1);
                    _ ->
                        rpc_failed
                end,
            Diag = #{
                b_meta => rpc:call(B, imboy_cache_sync, meta_snapshot, [], ?RPC_MS),
                fkey_gen => rpc:call(B, imboy_cache_sync, generation, [FKey], ?RPC_MS),
                a_boot => rpc:call(A, imboy_cache_sync, node_boot, [], ?RPC_MS),
                b_reject_total => RejectCnt,
                b_receive_total => ReceiveCnt,
                a_cfg_hash => hash_head(
                    rpc:call(A, imboy_cache_sync_proto, config_hash, [], ?RPC_MS)
                ),
                b_cfg_hash => hash_head(
                    rpc:call(B, imboy_cache_sync_proto, config_hash, [], ?RPC_MS)
                ),
                a_reg_hash => hash_head(
                    rpc:call(A, imboy_cache_sync_proto, registry_hash, [], ?RPC_MS)
                ),
                b_reg_hash => hash_head(
                    rpc:call(B, imboy_cache_sync_proto, registry_hash, [], ?RPC_MS)
                ),
                b_epoch => rpc:call(B, imboy_cache_sync_proto, epoch, [], ?RPC_MS)
            },
            erlang:error({c9d_no_compensation, G0, G1, Diag})
    end,
    ok.

%% C05 oracle（核心）：分区期间权威侧更新、失效丢失；重连后受影响 Key
%% 在 deadline 内返回权威值，不依赖旧 TTL 自然到期。
c9e_partition_reconnect(A, B, C) ->
    ok = ensure_c9_ready(A),
    ok = ensure_c9_ready(B),
    ok = ensure_c9_ready(C),
    FKey = {cache_v2_test_rep, c9e_part},
    %% A 预写旧值（fenced namespace 的 invalidate 类语义：值+栅栏）
    ok = rpc:call(A, imboy_cache_sync, invalidate, [FKey], ?RPC_MS),
    timer:sleep(300),
    %% 分区：A 与 B/C 断开
    true = rpc:call(A, erlang, disconnect_node, [B], ?RPC_MS),
    true = rpc:call(A, erlang, disconnect_node, [C], ?RPC_MS),
    timer:sleep(500),
    %% 权威侧（B）在分区期间推进：模拟 PG 权威变更后的缓存失效（B 侧
    %% invalidate 广播在分区内丢失，A 不感知）
    ok = rpc:call(B, imboy_cache_sync, invalidate, [FKey], ?RPC_MS),
    timer:sleep(500),
    %% 重连
    true = rpc:call(A, net_kernel, connect_node, [B], ?RPC_MS),
    true = rpc:call(A, net_kernel, connect_node, [C], ?RPC_MS),
    timer:sleep(1500),
    %% 重连后 B 发新事件（<<0:128>> 伪装化身 ≠ A 已知的 B 化身 → changed）：
    %% A 检测并补偿 fenced key。G0 必须在事件前读——inject 的 rpc 返回时
    %% A 可能已完成应用与补偿（r35 实证 GA==G0 假阴性）
    G0 = rpc:call(A, imboy_cache_sync, generation, [FKey], ?RPC_MS),
    ok = rpc:call(B, imboy_cache, put, [FKey, <<"authoritative">>, 120, replicate], ?RPC_MS),
    GA = converge_generation(A, FKey, G0),
    true = GA > G0,
    %% A 在 deadline 内返回权威值：补偿 flush 旧值 + 事件 set 新值
    %% （depcache 直读——get 拦截层对未 verified key miss 是另一层语义）
    <<"authoritative">> = converge_value(A, FKey),
    ok.

%% 同化身、无后续事件：真实分区丢失最后一次失效，水位校对使多 Key 回源。
c9g_tail_checkpoint(A, B, C) ->
    tail_checkpoint(A, B, C, manual).

tail_checkpoint(A, B, C, Mode) ->
    ok = ensure_c9_ready(A),
    ok = ensure_c9_ready(B),
    Keys = [{cache_v2_test, {tail_a, Mode}}, {cache_v2_test, {tail_b, Mode}}],
    [old = rpc:call(A, imboy_cache, memo, [fun() -> old end, K, 120], ?RPC_MS) || K <- Keys],
    GBeforePartition = rpc:call(A, imboy_cache_sync, generation, [hd(Keys)], ?RPC_MS),
    Cookie = erlang:get_cookie(),
    Blocked = 'synthetic_tail_partition',
    [true = rpc:call(A, erlang, set_cookie, [N, Blocked], ?RPC_MS) || N <- [B, C]],
    G0 =
        try
            [rpc:call(A, erlang, disconnect_node, [N], ?RPC_MS) || N <- [B, C]],
            true = converge_pred(fun() ->
                Connected = rpc:call(A, erlang, nodes, [], ?RPC_MS),
                not lists:member(B, Connected) andalso not lists:member(C, Connected)
            end),
            %% 成员失联的即时 fenced 栅栏是独立行为；以它完成后的代次作为
            %% 基线，避免把分区保护误判为丢失失效尾部的恢复结果。
            GBase = converge_generation(A, hd(Keys), GBeforePartition),
            true = GBase > GBeforePartition,
            ok = rpc:call(B, imboy_cache, invalidate, [hd(Keys)], ?RPC_MS),
            %% RPC 屏障确认发送完成；分区中接收方没有推进代次。
            pong = rpc:call(B, gen_server, call, [imboy_cache_sync, ping_ok], ?RPC_MS),
            GBase = rpc:call(A, imboy_cache_sync, generation, [hd(Keys)], ?RPC_MS),
            GBase
        after
            [true = rpc:call(A, erlang, set_cookie, [N, Cookie], ?RPC_MS) || N <- [B, C]],
            [true = rpc:call(A, net_kernel, connect_node, [N], ?RPC_MS) || N <- [B, C]]
        end,
    G1 = tail_checkpoint_wait(A, hd(Keys), G0, Mode),
    case G1 > G0 of
        true -> ok;
        false -> error({tail_checkpoint_not_advanced, G0, G1, tail_checkpoint_state([A, B, C])})
    end,
    [new = rpc:call(A, imboy_cache, memo, [fun() -> new end, K, 120], ?RPC_MS) || K <- Keys],
    ok.

tail_checkpoint_wait(A, Key, G0, manual) ->
    APid = rpc:call(A, erlang, whereis, [imboy_cache_sync], ?RPC_MS),
    reconcile_tick = rpc:call(A, erlang, send, [APid, reconcile_tick], ?RPC_MS),
    converge_generation(A, Key, G0);
tail_checkpoint_wait(A, Key, G0, periodic) ->
    %% 不发校对消息、不新增业务写入；同时要求实际校对计数增长。
    Before = reconcile_count(A),
    await_periodic_checkpoint(A, Key, G0, Before, 750).

reconcile_count(Node) ->
    Metrics = rpc:call(Node, elib_metric, get_all_metrics, [], ?RPC_MS),
    maps:get(reconcile_total, maps:get(counters, Metrics), 0).

await_periodic_checkpoint(_Node, _Key, G0, _Before, 0) ->
    G0;
await_periodic_checkpoint(Node, Key, G0, Before, Left) ->
    G = rpc:call(Node, imboy_cache_sync, generation, [Key], ?RPC_MS),
    case G > G0 andalso reconcile_count(Node) > Before of
        true ->
            G;
        false ->
            timer:sleep(100),
            await_periodic_checkpoint(Node, Key, G0, Before, Left - 1)
    end.

%% 失败现场只取控制状态，不复制缓存值或整张元表。
tail_checkpoint_state(Nodes) ->
    [
        {N, rpc:call(N, imboy_cache_sync, status, [], ?RPC_MS),
            rpc:call(N, ?MODULE, sync_view, [], ?RPC_MS), [
                {K, rpc:call(N, ets, lookup, [imboy_cache_meta, K], ?RPC_MS)}
             || K <- [sync_origin_seq, sync_epoch] ++ [{peer_incarnation, P} || P <- Nodes]
            ]}
     || N <- Nodes
    ].

%% C03：Nmax 上限满时新 replicate 写被拒（fail-closed）。
c9f_nmax(A) ->
    ok = ensure_c9_ready(A),
    ok = rpc:call(
        A,
        imboy_cache,
        put,
        [{cache_v2_test_rep, capacity_precondition}, v, 120, replicate],
        ?RPC_MS
    ),
    OldCfg = rpc:call(A, application, get_env, [imboy, cache_replication, #{}], ?RPC_MS),
    rpc:call(
        A,
        application,
        set_env,
        [
            imboy,
            cache_replication,
            maps:merge(OldCfg, #{n_max => 1})
        ],
        ?RPC_MS
    ),
    try
        %% 前序场景已使 rindex_count > 0（n_max=1 即"已满"）；fail-closed
        %% 语义：已满状态下任何新 replicate 写一律 {error, index_full}
        K1 = {cache_v2_test_rep, c9f_n1},
        K2 = {cache_v2_test_rep, c9f_n2},
        {error, index_full} = rpc:call(A, imboy_cache, put, [K1, v, 120, replicate], ?RPC_MS),
        {error, index_full} = rpc:call(A, imboy_cache, put, [K2, v, 120, replicate], ?RPC_MS),
        %% 预算恢复后写路径立即回归（fail-closed 不残留）
        ok = rpc:call(A, ?MODULE, restore_nmax_then_put, [K1], ?RPC_MS)
    after
        rpc:call(A, application, set_env, [imboy, cache_replication, OldCfg], ?RPC_MS)
    end,
    ok.

%% 预算恢复 + 写入验证（C9f after 兜底前的正向回归断言）
restore_nmax_then_put(K1) ->
    %% get_env/3 返回裸值（非 {ok,V}——/2 才是 {ok,V} 语义）
    Cfg = application:get_env(imboy, cache_replication, #{}),
    application:set_env(imboy, cache_replication, maps:merge(Cfg, #{n_max => 50000})),
    try
        ok = imboy_cache:put(K1, v, 120, replicate)
    after
        application:set_env(imboy, cache_replication, Cfg)
    end.

%% 等待节点侧 fenced generation 超过基线（有界轮询）。
converge_generation(Node, Key, Baseline) ->
    converge_generation(Node, Key, Baseline, 100).

%% 等待节点读到期望终值（并发双写的确定性收敛；有界轮询）。
converge_to(_Node, _Key, _Expect, 0) ->
    error(converge_timeout);
converge_to(Node, Key, Expect, N) ->
    case rpc:call(Node, imboy_cache, get, [Key], ?RPC_MS) of
        Expect ->
            ok;
        _ ->
            timer:sleep(100),
            converge_to(Node, Key, Expect, N - 1)
    end.

%% 等待 A 侧收敛到权威值（事件应用 + 补偿 flush 后的最终状态）。
converge_value(Node, Key) ->
    converge_value(Node, Key, 50).
converge_value(Node, _Key, 0) ->
    timeout_value;
converge_value(Node, Key, N) ->
    case depcache_direct_get(Node, Key) of
        <<"authoritative">> ->
            <<"authoritative">>;
        _ ->
            timer:sleep(100),
            converge_value(Node, Key, N - 1)
    end.

%% 直读 depcache（绕过 imboy_cache:get 的 replicate 拦截层：
%% 未 verified 的 replicate key 对上层 miss 回权威是正确语义，不能作断言）
depcache_direct_get(Node, Key) ->
    case rpc:call(Node, depcache, get, [Key, imboy_cache], ?RPC_MS) of
        {ok, V} -> V;
        Other -> Other
    end.

converge_generation(_Node, _Key, Baseline, 0) ->
    Baseline;
converge_generation(Node, Key, Baseline, N) ->
    G = rpc:call(Node, imboy_cache_sync, generation, [Key], ?RPC_MS),
    case G > Baseline of
        true ->
            G;
        false ->
            timer:sleep(100),
            converge_generation(Node, Key, Baseline, N - 1)
    end.
