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
-export([main/1]).
-export([
    init_on_peer/0,
    init_on_peer_small/0,
    read_rev/1,
    read_expires/1,
    read_rev_full/1,
    inject_v2_set/4,
    restart_sync/0,
    peer_sync_down/0,
    sync_alive/0,
    meta_inject/1,
    meta_probe/1,
    overload_probe/0,
    drain_sync_mailbox/0,
    dump_debug/0
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
    Results =
        try
            Scenarios = scenario_list(A, B, C),
            [run_one(Name, Fun) || {Name, Fun} <- Scenarios]
        after
            stop_cluster(PeerPids)
        end,
    T1 = erlang:monotonic_time(millisecond),
    Json = results_json(Results, T1 - T0),
    ok = file:write_file(OutPath, Json),
    Failed = [Name || {Name, fail, _} <- Results],
    io:format(
        "== cache_v21_dcheck: ~p passed, ~p failed, ~p ms ==~n",
        [length([ok || {_, pass, _} <- Results]), length(Failed), T1 - T0]
    ),
    case Failed of
        [] -> halt(0);
        _ -> halt(2)
    end;
main(_) ->
    io:format("usage: cache_v21_dcheck:main([\"<output.json>\"]).~n"),
    halt(1).

scenario_list(A, B, C) ->
    [
        {"SET propagates to all nodes", fun() -> s_set(A, B, C) end},
        {"update overwrites everywhere", fun() -> s_update(A, B, C) end},
        {"DELETE tombstone removes everywhere", fun() -> s_delete(A, B, C) end},
        {"delayed stale SET cannot resurrect", fun() -> s_zombie(A, B, C) end},
        {"duplicate events are idempotent", fun() -> s_dup(A, B) end},
        {"concurrent SET converges to winner", fun() -> s_race(A, B, C) end},
        {"invalidate propagates fenced generation", fun() -> s_invalidate(A, B) end},
        {"TTL remaining not reset", fun() -> s_ttl(A, B) end},
        {"restart recovers version ceiling", fun() -> s_restart(A, C) end},
        {"overload protection rejects new replicate", fun() -> s_overload(A) end},
        {"snapshot backfills lost replicas after restart", fun() -> s_snapshot(A, B, C) end},
        {"F1 peer dies during recovery round; survivor closes the round", fun() ->
            f1_peer_dies_during_recovery(A, B, C)
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
        end}
    ].

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
    VA = rpc:call(A, imboy_cache, get, [Key], 5000),
    VB = rpc:call(B, imboy_cache, get, [Key], 5000),
    VC = converge_any(C, Key),
    true = (VA =:= VB) andalso (VB =:= VC),
    true = lists:member(VA, [{ok, from_a}, {ok, from_b}]),
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
    {ok, {{C1, _}, _, _, _}} = rpc:call(A, ?MODULE, read_rev_full, [Key], 5000),
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
    {ok, {{C2, _}, _, _, _}} = rpc:call(C, ?MODULE, read_rev_full, [Key2], 5000),
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
s_snapshot(A, B, C) ->
    K1 = {cache_v2_test_rep, d_snap1},
    K2 = {cache_v2_test_rep, d_snap2},
    ok = rpc:call(A, imboy_cache, put, [K1, snap_v1, 300, replicate], ?RPC_MS),
    ok = rpc:call(B, imboy_cache, put, [K2, snap_v2, 300, replicate], ?RPC_MS),
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
    end.

s_overload(A) ->
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
        ready = recover_ready(A, heal_restart(A), 2),
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
    %% F1 的 after 兜底重启可能让 A 仍在收敛：写方必须自身 ready 才能承载
    %% “恢复窗口内的并发 delta”语义
    ready = recover_ready(A, heal_restart(A), 2),
    ready = recover_ready(B, heal_restart(B), 2),
    K1 = {cache_v2_test_rep, d_f2a},
    ok = rpc:call(A, imboy_cache, put, [K1, v1, 300, replicate], ?RPC_MS),
    {ok, v1} = converge_get(C, K1, {ok, v1}),
    ok = rpc:call(C, ?MODULE, restart_sync, [], ?RPC_MS),
    %% C 恢复窗口内的并发写
    ok = rpc:call(A, imboy_cache, put, [K1, v2, 300, replicate], ?RPC_MS),
    K2 = {cache_v2_test_rep, d_f2b},
    ok = rpc:call(B, imboy_cache, put, [K2, fresh, 300, replicate], ?RPC_MS),
    ready = recover_ready(C, heal_restart(C), 2),
    {ok, v2} = converge_get(C, K1, {ok, v2}),
    {ok, fresh} = converge_get(C, K2, {ok, fresh}),
    ok.

%% F3：节点硬崩（erlang:halt，无 terminate/syn:leave）+ 同名新化身：
%% 存活节点不受影响；新化身经快照回填恢复，版本上界不回退（AC-17 硬崩路径）。
f3_hard_crash_incarnation(A, B, C) ->
    Key = {cache_v2_test_rep, d_f3},
    ok = rpc:call(A, imboy_cache, put, [Key, f3_v, 300, replicate], ?RPC_MS),
    {ok, f3_v} = converge_get(B, Key, {ok, f3_v}),
    {ok, {{C1, _}, _, _, _}} = rpc:call(A, ?MODULE, read_rev_full, [Key], 5000),
    %% 硬崩
    catch rpc:call(C, erlang, halt, [0], 3000),
    timer:sleep(300),
    C2 = restart_c_hard(A, B),
    ready = recover_ready(C2, heal_restart(C2), 2),
    {ok, f3_v} = converge_get(C2, Key, {ok, f3_v}),
    Key2 = {cache_v2_test_rep, d_f3b},
    ok = rpc:call(C2, imboy_cache, put, [Key2, f3_new, 120, replicate], ?RPC_MS),
    {ok, {{C2c, _}, _, _, _}} = rpc:call(C2, ?MODULE, read_rev_full, [Key2], 5000),
    true = C2c >= C1 orelse erlang:error({ceiling_regressed, C1, C2c}),
    ok.

%% F4：有界清理（W4-06/07）受控注入——过期墓碑、过期 live 元数据连同
%% rindex 行一并淘汰；未过期条目不受影响。
f4_bounded_sweep(A, B, C) ->
    NowMs = erlang:system_time(millisecond),
    Rev = {1, node()},
    %% 直接构造三类元数据行（公共 ETS，受控注入）：
    %% ktomb = 过期墓碑且已跨完整写窗（ExpiresAt+Tmax < now）→ 连 rindex 淘汰；
    %% kexp  = 过期 live（值已随 depcache TTL 消失）→ 淘汰；
    %% klive = 未过期 live → 保留。
    Rows = [
        {{rev, ktomb}, {Rev, 1, NowMs - 721000, deleted}},
        {{rindex, ktomb}, NowMs - 721000},
        {{rev, kexp}, {Rev, 1, NowMs - 1000, live}},
        {{rindex, kexp}, NowMs - 1000},
        {{rev, klive}, {Rev, 1, NowMs + 60000, live}},
        {{rindex, klive}, NowMs}
    ],
    ok = rpc:call(C, ?MODULE, meta_inject, [Rows], 5000),
    %% 直发 reconcile_tick 触发 bounded_sweep（不必等 60s 周期）
    rpc:call(C, erlang, send, [imboy_cache_sync, reconcile_tick], 5000),
    ok = rpc:call(C, ?MODULE, drain_sync_mailbox, [], ?RPC_MS),
    gone = rpc:call(C, ?MODULE, meta_probe, [ktomb], 5000),
    gone = rpc:call(C, ?MODULE, meta_probe, [kexp], 5000),
    kept = rpc:call(C, ?MODULE, meta_probe, [klive], 5000),
    ok.

%% F5：提前逐出（W4-08 功能性回归）。C 以小容量 depcache（1MB）经快照恢复
%% 后被 depcache 随机逐出裁剪为存活子集；随后 B 仅从 C 恢复——
%% send_snapshot 的 valid_live_entry 必须跳过已被逐出（depcache 无值）的
%% 条目：B 的内容与 C 的存活子集完全一致，被逐出的键不得复活。
f5_early_evict_survivors_only(A, B, C) ->
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
    ready = recover_ready(B, heal_restart(B), 2),
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
        ready = recover_ready(A, heal_restart(A), 2),
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
    %% 节点名只用安全字符；随机 cookie；loopback 分布接口
    RandBin = integer_to_binary(erlang:unique_integer([positive]), 36),
    Rand = binary_to_atom(iolist_to_binary("c21" ++ binary_to_list(RandBin)), utf8),
    Main = binary_to_atom(iolist_to_binary("c21m" ++ binary_to_list(RandBin)), utf8),
    {ok, _} = net_kernel:start([Main, longnames]),
    CookieAtom = binary_to_atom(
        iolist_to_binary(
            "ck" ++ binary_to_list(integer_to_binary(erlang:unique_integer([positive]), 36))
        ),
        utf8
    ),
    true = erlang:set_cookie(node(), CookieAtom),
    Ebin = filename:absname("ebin"),
    Deps = [filename:absname("deps/" ++ D ++ "/ebin") || D <- ["depcache", "syn"]],
    Args = [
        "-setcookie",
        atom_to_list(CookieAtom),
        "-kernel",
        "inet_dist_use_interface",
        "{127,0,0,1}",
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
    pong = wait_peer(A, 200),
    {ok, PB, B} = peer:start(#{
        name => NameB,
        host => "127.0.0.1",
        wait_boot => false,
        args => Args
    }),
    pong = wait_peer(B, 200),
    {ok, PC, C} = peer:start(#{
        name => NameC,
        host => "127.0.0.1",
        wait_boot => false,
        args => Args
    }),
    pong = wait_peer(C, 200),
    %% F3 同名硬重启所需身份（驱动节点侧暂存）
    _ = ets:new(c21dctx, [set, named_table, public]),
    true = ets:insert(c21dctx, {c, {NameC, Args}}),
    true = rpc:call(A, net_kernel, connect_node, [B], ?RPC_MS),
    true = rpc:call(A, net_kernel, connect_node, [C], ?RPC_MS),
    true = rpc:call(B, net_kernel, connect_node, [C], ?RPC_MS),
    ok = rpc:call(A, ?MODULE, init_on_peer, [], ?RPC_MS),
    ok = rpc:call(B, ?MODULE, init_on_peer, [], ?RPC_MS),
    ok = rpc:call(C, ?MODULE, init_on_peer, [], ?RPC_MS),
    ready = wait_status(A, 400),
    ready = wait_status(B, 400),
    ready = wait_status(C, 400),
    {ok, [PA, PB, PC], {A, B, C}}.

%% F3：C 硬崩后的同名新化身（同一 args/代码树，新 BootId）
restart_c_hard(A, B) ->
    [{c, {NameC, Args}}] = ets:lookup(c21dctx, c),
    {ok, _P, C2} = peer:start(#{
        name => NameC,
        host => "127.0.0.1",
        wait_boot => false,
        args => Args
    }),
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

stop_cluster(PeerPids) ->
    _ = [
        try
            peer:stop(P)
        catch
            _:_ -> ok
        end
     || P <- PeerPids
    ],
    catch net_kernel:stop(),
    ok.

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
    application:set_env(imboy, cache_fenced_namespaces, [cache_v2_test]),
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
    application:set_env(imboy, cache_fenced_namespaces, [cache_v2_test]),
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
        [{{rev, Key}, {Rev, _C, _Exp, _St}}] -> {ok, Rev};
        [] -> {error, not_found}
    end.

read_expires(Key) ->
    case ets:lookup(imboy_cache_meta, {rev, Key}) of
        [{{rev, Key}, {_Rev, _C, Exp, _St}}] -> {ok, Exp};
        [] -> {error, not_found}
    end.

read_rev_full(Key) ->
    case ets:lookup(imboy_cache_meta, {rev, Key}) of
        [{{rev, Key}, {Rev, C, Exp, St}}] -> {ok, {Rev, C, Exp, St}};
        [] -> {error, not_found}
    end.

%% 定向注入 V2 事件（模拟延迟/重复消息；OriginNode 避开接收端自环过滤）
inject_v2_set(Key, Value, Rev, OriginNode) ->
    Event = #{
        protocol => 2,
        operation => set,
        key => Key,
        value => Value,
        expires_at_ms => erlang:system_time(millisecond) + 120000,
        revision => Rev,
        origin_seq => 99999,
        origin_node => OriginNode,
        origin_boot_id => <<0:128>>
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
    #{
        status => imboy_cache_sync:status(),
        members => length(syn:members(imboy_cache, dsync_handler)),
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

converge_any(Node, Key) ->
    Deadline = erlang:monotonic_time(millisecond) + ?CONVERGE_MS,
    converge_any_loop(Node, Key, Deadline).

converge_any_loop(Node, Key, Deadline) ->
    V = rpc:call(Node, imboy_cache, get, [Key], 5000),
    case V of
        {ok, _} ->
            V;
        _ ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true ->
                    undefined;
                false ->
                    timer:sleep(?POLL_MS),
                    converge_any_loop(Node, Key, Deadline)
            end
    end.

wait_status(_Node, 0) ->
    timeout;
wait_status(Node, N) ->
    case rpc:call(Node, imboy_cache_sync, status, [], 5000) of
        ready ->
            ready;
        _ ->
            timer:sleep(100),
            wait_status(Node, N - 1)
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
    io_lib:format(
        "{\"driver_node\": \"~s\", \"elapsed_ms\": ~p, \"utc\": \"~s\", \"scenarios\": [~s]}",
        [
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
            C -> [C]
        end
     || Ch <- Flat
    ]).
