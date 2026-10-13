%% @doc 混合版本真实运行演练（W5-05）：旧/新版本 imboy_cache_sync 真实 BEAM
%% 节点混合组网验证（19.8 能力交集语义）。
%%
%% 旧节点：INTEGRATION_BASE（V2 引入前的 main，如 3a01829d）独立构建树；
%% 新节点：当前 main 构建树。两树共用同一 syn scope（imboy_cache）与
%% group（dsync_handler）、同一 depcache server 名（imboy_cache）。
%%
%% 场景：
%%   M1 未分类 legacy SET 可应用；受管键拒绝无版本覆盖，值与版本保持。
%%   M2 混合能力交集拒绝公开 V2 写入/删除/广播；legacy 功能完好
%%   M3 原混合重启 READY oracle，实际 DEGRADED 必须 FAIL；U1 单列升级恢复
%%   M4 新节点仅存 legacy peer：有界重试后 DEGRADED（fail-closed：读 miss、写拒绝）
%%   M5 原丢失全部新版权威后 READY oracle；L1/L2 保留权威的证据独立登记
%%   M6 新节点 invalidate 双通道发布：legacy peer 经 {flush, Key} 失效本地值
%%      （19.8 能力交集；v2 信封被旧节点 catch-all 忽略，缺兼容发布则旧节点滞留陈旧值）
%%
%% C10 收口新增（C01 冻结协议 frozen 文档：
%% docs/architecture/cache-v2.2.1/closeout-v1/protocol-budget-frozen.md）：
%%   M7 hash-mismatch：漂移节点（C 改 registry overrides）的 V2 事件被 B 整体
%%      拒绝——reject_total 递增、值不变、peer 标记 hash_divergent（第 1 节）；
%%      恢复配置后通路复原（一致事件自动清除 divergent 标记）
%%   M8 epoch-shift：B 推进 epoch 后，C 的旧 epoch 复制事件被拒（stale_epoch，
%%      第 1/3 节；invalidate 不参与 epoch 比较的豁免由 src 既有测试覆盖）
%%   M9 old-incarnation-replay：宽限窗（W=720s）内旧化身重放按修订 r1 接受
%%      ——化身层只检测/注册/补偿不拒绝；旧值不复活由 Lamport revision 承担
%%      （第 2 节 r1）。同一宽限化身携带更高 revision 的新值必须可应用
%%      （接受性证明：拒绝旧化身会使重启 peer 全部事件被永久拒绝）
%%   M10 r2 握手帧：version_request 为 {cache_version_request, FromPid, FromNode}
%%      3 元组；旧 2 元组请求仍被应答（走 syn 视图反查，兼容混合矩阵旧节点，
%%      第 4 节 r2）；version_report 9 元组尾增发送者 pid（report 到达 C 由
%%      peer_incarnation 注册重建显式断言）
%%
%% 运行：erlc 编译本文件后，以
%%   erl -noshell -pa <driver_dir> -eval 'cache_v21_mixed_dcheck:main(["out.json"]).'
%% 驱动；旧/新两棵构建树的 ebin 路径经环境变量传入（见 main/1 与 start_mixed/0）。
-module(cache_v21_mixed_dcheck).

-export([main/1]).

%% peer 端辅助函数（driver beam 须同时在新旧节点的 code path 上）
-export([
    init_on_peer/0,
    restart_sync/0,
    peer_sync_down/0,
    change_membership/1,
    sync_alive/0,
    status_get/0,
    rev_present/1,
    read_rev_value/1,
    m1_broadcast_set/2,
    m1_broadcast_set/3,
    inject_v2_set/4,
    inject_v2_set_frozen/5,
    reject_reset/0,
    reject_read/0,
    dump_debug/0
]).

-define(RPC_MS, 30000).

%% ===================================================================
%% 入口
%% ===================================================================

main([OutPath | _]) ->
    T0 = erlang:monotonic_time(millisecond),
    LegacyEbin = os:getenv("LEGACY_EBIN"),
    NewEbin = os:getenv("NEW_EBIN"),
    case {LegacyEbin, NewEbin} of
        {false, _} ->
            io:format("usage: LEGACY_EBIN=<old ebin> NEW_EBIN=<new ebin> erl ...~n"),
            halt(1);
        {_, false} ->
            io:format("usage: LEGACY_EBIN=<old ebin> NEW_EBIN=<new ebin> erl ...~n"),
            halt(1);
        {LE, NE} ->
            Results =
                try
                    {ok, Nodes} = start_mixed(LE, NE),
                    Scenarios = scenario_list(Nodes),
                    Primary = [run_one(Name, Fun) || {Name, Fun} <- Scenarios],
                    stop_all(),
                    Primary ++ destructive_legacy_tail(LE, NE)
                catch
                    Class:Reason:Stack ->
                        [
                            {"BOOT mixed cluster", fail,
                                io_lib:format("~p:~p at ~p", [
                                    Class, Reason, lists:sublist(Stack, 3)
                                ])}
                        ]
                after
                    stop_all()
                end,
            T1 = erlang:monotonic_time(millisecond),
            ok = file:write_file(OutPath, results_json(Results, T1 - T0)),
            Failed = [Name || {Name, fail, _} <- Results],
            io:format(
                "== cache_v21_mixed_dcheck: ~p passed, ~p failed, ~p ms ==~n",
                [length([ok || {_, pass, _} <- Results]), length(Failed), T1 - T0]
            ),
            case Failed of
                [] -> halt(0);
                _ -> halt(2)
            end
    end;
main(_) ->
    io:format("usage: cache_v21_mixed_dcheck:main([\"<output.json>\"]).~n"),
    halt(1).

scenario_list(Nodes) ->
    A = maps:get(a, Nodes),
    B = maps:get(b, Nodes),
    C = maps:get(c, Nodes),
    [
        {"M1 legacy SET accepted only for unmanaged keys", fun() ->
            m1_legacy_set(A, B)
        end},
        {"M2 mixed capability intersection rejects V2 publish", fun() ->
            m2_v2_no_crash_legacy(A, B)
        end},
        {"M3 new-node restart recovers from mixed peers (V2 backfill only)", fun() ->
            m3_mixed_restart_backfill(A, B, C)
        end},
        {"U1 full V2 upgrade restores value and deletion coverage", fun() ->
            upgrade_recovery(A, B, C)
        end},
        {"L1 authority-preserving membership leave degrades recovering node", fun() ->
            leave_authority_degrades(A, B, C)
        end},
        {"L2 preserved authority rejoin plus full upgrade restores READY", fun() ->
            rejoin_authority_recovers(A, B, C)
        end},
        {"M6 V2-node invalidate flushes legacy peer via legacy channel", fun() ->
            m6_v2_invalidate_reaches_legacy(A, B)
        end},
        %% --- C10 收口新增（C01 冻结协议第 1/2/3/4 节，见头注释）---
        {"M7 hash mismatch: drifted-node events rejected (reject_total++, value frozen)", fun() ->
            m7_hash_mismatch(A, B, C)
        end},
        {"M8 epoch shift: stale-epoch events rejected after peer bump", fun() ->
            m8_epoch_shift(A, B, C)
        end},
        {"M9 old incarnation replay in grace: accepted-but-no-resurrect (r1)", fun() ->
            m9_old_incarnation_replay(A, B, C)
        end},
        {"M10 r2 handshake: 3-tuple frame, legacy 2-tuple still answered", fun() ->
            m10_r2_handshake(A, B, C)
        end},
        {"R1 rolling rollback node C: new -> legacy in place", fun() ->
            r1_rollback_node_c(Nodes)
        end},
        {"R2 rolling rollback node B: data-safety oracle, then all-legacy", fun() ->
            r2_rollback_node_b(Nodes)
        end},
        {"R3 full-cluster rollback restart, legacy steady state", fun() ->
            r3_full_cluster_rollback_restart(Nodes)
        end}
    ].

run_one(Name, Fun) ->
    try
        _ = Fun(),
        {Name, pass, ""}
    catch
        Cl:R:ST ->
            {Name, fail, io_lib:format("~p:~p at ~p", [Cl, R, lists:sublist(ST, 3)])}
    end.

%% ===================================================================
%% 场景
%% ===================================================================

%% M1：旧节点 legacy set（dsync_enabled）→ 全组 {set,K,V,TTL,[]} →
%% 未分类键保持兼容；replicate 键必须拒绝且不改变现有值或版本。
m1_legacy_set(A, B) ->
    Key = {mixed_legacy, m1_k1},
    ok = rpc:call(A, ?MODULE, m1_broadcast_set, [Key, legacy_v1], ?RPC_MS),
    {ok, legacy_v1} = converge_get(B, Key, {ok, legacy_v1}),
    ok = read_rev_expect_absent(B, Key),
    Managed = {mixed_test, m1_managed},
    {ok, managed_v1} = rpc:call(B, imboy_cache, get, [Managed], ?RPC_MS),
    {ok, Revision} = rpc:call(B, ?MODULE, read_rev_value, [Managed], 5000),
    ok = rpc:call(B, ?MODULE, reject_reset, [], 5000),
    ok = rpc:call(A, ?MODULE, m1_broadcast_set, [Managed, unsafe_legacy, B], ?RPC_MS),
    Rejected = rpc:call(B, ?MODULE, reject_read, [], 5000),
    true = Rejected > 0 orelse erlang:error({legacy_reject_not_counted, Rejected}),
    {ok, managed_v1} = rpc:call(B, imboy_cache, get, [Managed], 5000),
    {ok, Revision} = rpc:call(B, ?MODULE, read_rev_value, [Managed], 5000),
    ok.

%% M2：原计划 §19.8 / C02 能力交集：混合期公开 V2 路径必须拒绝，
%% 不改变已缓存基线及其版本；未分类 legacy SET 通路无损。
m2_v2_no_crash_legacy(A, B) ->
    Key = {mixed_test, m2_k2},
    {ok, v2_val} = converge_get(B, Key, {ok, v2_val}),
    {ok, Rev} = rpc:call(B, ?MODULE, read_rev_value, [Key], 5000),
    {error, incompatible_peers} =
        rpc:call(B, imboy_cache, put, [Key, unsafe_mixed, 120, replicate], ?RPC_MS),
    {error, incompatible_peers} = rpc:call(B, imboy_cache, put_delete, [Key], ?RPC_MS),
    [
        {error, incompatible_peers} = rpc:call(
            B,
            imboy_cache_sync,
            broadcast,
            [#{protocol => 2, operation => Op}],
            ?RPC_MS
        )
     || Op <- [set, delete]
    ],
    {ok, v2_val} = rpc:call(B, imboy_cache, get, [Key], 5000),
    {ok, Rev} = rpc:call(B, ?MODULE, read_rev_value, [Key], 5000),
    true = rpc:call(A, ?MODULE, sync_alive, [], 5000),
    undefined = rpc:call(A, imboy_cache, get, [Key], 5000),
    %% 旧节点 legacy 功能在收到 V2 map 后仍然完好
    KeyL = {mixed_legacy, m2_kl},
    ok = rpc:call(A, ?MODULE, m1_broadcast_set, [KeyL, legacy_after_v2], ?RPC_MS),
    {ok, legacy_after_v2} = converge_get(B, KeyL, {ok, legacy_after_v2}),
    ok.

%% 原 M3 READY oracle 原样保留。legacy 静默造成 DEGRADED 时真实 FAIL，
%% 不能由随后 U1 全 V2 恢复的 PASS 顶替。
m3_mixed_restart_backfill(A, B, C) ->
    ready = wait_status(B, 60),
    ok = rpc:call(C, ?MODULE, restart_sync, [], ?RPC_MS),
    ready = wait_status(C, 120),
    {ok, v2_val} = converge_get(C, {mixed_test, m2_k2}, {ok, v2_val}),
    undefined = rpc:call(C, imboy_cache, get, [{mixed_legacy, m1_k1}], 5000),
    true = rpc:call(A, ?MODULE, sync_alive, [], 5000),
    ok.

%% 新 U1：独立的全 V2 升级恢复证据，不重写原 M3 的结果。
upgrade_recovery(A, B, C) ->
    ready = wait_status(B, 60),
    A = restart_peer_as(new, a),
    ok = rpc:call(C, ?MODULE, restart_sync, [], ?RPC_MS),
    ready = wait_status(C, 120),
    {ok, v2_val} = converge_get(C, {mixed_test, m2_k2}, {ok, v2_val}),
    undefined = rpc:call(C, imboy_cache, get, [{mixed_test, m3_deleted}], 5000),
    deleted = rpc:call(C, imboy_cache_sync, replicate_state, [{mixed_test, m3_deleted}], 5000),
    undefined = rpc:call(C, imboy_cache, get, [{mixed_legacy, m1_k1}], 5000),
    ok.

%% 新 L1：将 A 回退为 legacy，暂时撤出 B 同步成员（保留其权威），重启 C：
%% 集群仅剩 legacy A 可见 → 版本上界无人应答 → 有界重试（3×5s）后
%% DEGRADED：replicate 读 undefined（soft-state miss）、写被拒（fail-closed）。
leave_authority_degrades(A, B, C) ->
    ready = wait_status(B, 60),
    A = restart_peer_as(legacy, a),
    ok = rpc:call(B, ?MODULE, change_membership, [leave], ?RPC_MS),
    true = converge_true(
        fun() ->
            Members = rpc:call(C, imboy_cache_sync, syn_scope_members, [], 5000),
            is_list(Members) andalso not lists:any(fun(P) -> node(P) =:= B end, Members)
        end,
        100
    ),
    ok = rpc:call(C, ?MODULE, restart_sync, [], ?RPC_MS),
    degraded = wait_status(C, 400),
    Key4 = {mixed_test, m4_k4},
    undefined = rpc:call(C, imboy_cache, get, [Key4], 5000),
    {error, {not_ready, degraded}} =
        rpc:call(C, imboy_cache, put, [Key4, nope, 60, replicate], ?RPC_MS),
    true = rpc:call(A, ?MODULE, sync_alive, [], 5000),
    ok.

%% 新 L2：B 携带原有权威回归，真实升级 A 后重启 C。
%% 全员具备 V2 能力后恢复 READY，B 新写 → C 可见。
rejoin_authority_recovers(A, B, C) ->
    ok = rpc:call(B, ?MODULE, change_membership, [join], ?RPC_MS),
    ready = wait_status(B, 120),
    A = restart_peer_as(new, a),
    ok = rpc:call(C, ?MODULE, restart_sync, [], ?RPC_MS),
    ready = wait_status(C, 120),
    Key5 = {mixed_test, m5_k5},
    ok = rpc:call(B, imboy_cache, put, [Key5, revived, 120, replicate], ?RPC_MS),
    {ok, revived} = converge_get(C, Key5, {ok, revived}),
    true = rpc:call(A, ?MODULE, sync_alive, [], 5000),
    ok.

%% M6：19.8 能力交集——旧节点静默丢弃 v2 信封（catch-all），混合期新节点
%% invalidate 必须同时经 legacy {flush, Key} 通道发布，否则旧节点滞留
%% 陈旧值至 TTL。旧节点本地写（不广播），新节点发起失效后旧节点值必须被清除。
m6_v2_invalidate_reaches_legacy(A, B) ->
    A = restart_peer_as(legacy, a),
    Key = {mixed_test, m6_k},
    ok = rpc:call(A, imboy_cache, set, [Key, legacy_stale, 120], ?RPC_MS),
    {ok, legacy_stale} = converge_get(A, Key, {ok, legacy_stale}),
    ok = rpc:call(B, imboy_cache, invalidate, [Key], ?RPC_MS),
    undefined = converge_get(A, Key, undefined),
    A = restart_peer_as(new, a),
    ok.

%% ===================================================================
%% C10 收口场景（C01 冻结协议：hash / epoch / incarnation / r2 握手）
%% ===================================================================

%% M7 hash-mismatch（C01 第 1 节）：发送端 C 的 registry 配置漂移
%% （overrides 增加未同步条目 → registry_hash 变化）后，C 的 V2 事件对 B
%% 整体拒绝：值不变、reject_total 递增、peer 标记 hash_divergent（供 19.8
%% 可观测与 C10 断言）；恢复配置后通路复原、divergent 标记被一致事件清除。
%% 旧节点 A 同场静默忽略（catch-all，不崩溃）——混合组网下的完整 oracle。
m7_hash_mismatch(A, B, C) ->
    ok = ensure_new_ready(B),
    ok = ensure_new_ready(C),
    Key = {mixed_test, m7_hash},
    ok = rpc:call(C, imboy_cache, put, [Key, v1, 300, replicate], ?RPC_MS),
    {ok, v1} = converge_get(B, Key, {ok, v1}),
    %% 基线：B 侧拒绝计数清零（计数器为节点级，场景前重置保证断言精度）
    ok = rpc:call(B, ?MODULE, reject_reset, [], 5000),
    %% C 漂移：registry overrides 增加未同步条目 → C 的 registry_hash 变
    rpc:call(
        C,
        application,
        set_env,
        [
            imboy,
            cache_registry_overrides,
            #{mixed_test => {replicate, false}, drift_ns => {local, false}}
        ],
        ?RPC_MS
    ),
    ok = rpc:call(C, imboy_cache, put, [Key, v2_drift, 300, replicate], ?RPC_MS),
    timer:sleep(1000),
    %% oracle：事件整体拒绝（值不变）+ reject_total 递增 + hash_divergent 标记
    {ok, v1} = rpc:call(B, imboy_cache, get, [Key], ?RPC_MS),
    Rejected = rpc:call(B, ?MODULE, reject_read, [], 5000),
    true = Rejected >= 1 orelse erlang:error({hash_reject_not_counted, Rejected}),
    true = rpc:call(B, imboy_cache_sync_proto, hash_divergent, [C], 5000),
    %% 混合组网：旧节点 A 对漂移 V2 事件零副作用（catch-all，不崩溃）
    true = rpc:call(A, ?MODULE, sync_alive, [], 5000),
    %% 清理：恢复 C 配置；B 侧 divergent 标记由后续一致事件自动清除，
    %% 显式删除兜底（与 C9b 同款），随后验证通路复原
    rpc:call(
        C,
        application,
        set_env,
        [
            imboy, cache_registry_overrides, #{mixed_test => {replicate, false}}
        ],
        ?RPC_MS
    ),
    true = rpc:call(B, ets, delete, [imboy_cache_meta, {hash_divergent, C}], 5000),
    ok = rpc:call(C, imboy_cache, put, [Key, v3_heal, 300, replicate], ?RPC_MS),
    {ok, v3_heal} = converge_get(B, Key, {ok, v3_heal}),
    ok.

%% M8 epoch-shift（C01 第 1/3 节）：B 推进 sync epoch（受控清理途径语义）
%% 后，C 携带旧 epoch 的复制事件对 B 是 stale_epoch → 拒绝、值不变、
%% reject_total 递增。epoch 回落后通路复原（不污染 R1-R3 回滚场景）。
%% 注：invalidate 不参与 epoch 比较（第 1 节豁免）由 src 既有测试覆盖。
m8_epoch_shift(A, B, C) ->
    ok = ensure_new_ready(B),
    ok = ensure_new_ready(C),
    Key = {mixed_test, m8_epoch},
    ok = rpc:call(C, imboy_cache, put, [Key, v1, 300, replicate], ?RPC_MS),
    {ok, v1} = converge_get(B, Key, {ok, v1}),
    ok = rpc:call(B, ?MODULE, reject_reset, [], 5000),
    %% B 推进 epoch 至 1：C（epoch=0）的后续复制事件对 B 是 stale_epoch
    1 = rpc:call(B, imboy_cache_sync_proto, bump_epoch, [], ?RPC_MS),
    1 = rpc:call(B, imboy_cache_sync_proto, epoch, [], 5000),
    ok = rpc:call(C, imboy_cache, put, [Key, v2_stale, 300, replicate], ?RPC_MS),
    timer:sleep(1000),
    %% oracle：旧 epoch 事件被拒（值不变）+ reject_total 递增
    {ok, v1} = rpc:call(B, imboy_cache, get, [Key], ?RPC_MS),
    Rejected = rpc:call(B, ?MODULE, reject_read, [], 5000),
    true = Rejected >= 1 orelse erlang:error({stale_epoch_not_counted, Rejected}),
    true = rpc:call(A, ?MODULE, sync_alive, [], 5000),
    %% 清理：epoch 回 0 → C→B 通路复原
    true = rpc:call(B, ets, insert, [imboy_cache_meta, {sync_epoch, 0}], 5000),
    ok = rpc:call(C, imboy_cache, put, [Key, v3_heal, 300, replicate], ?RPC_MS),
    {ok, v3_heal} = converge_get(B, Key, {ok, v3_heal}),
    ok.

%% M9 old-incarnation-replay（C01 第 2 节修订 r1）：C 重启换化身后，
%% 旧化身进入 B 的宽限集合（W=720s，时间戳登记）。宽限窗内：
%%   - 旧化身 + 旧 revision 重放 → 接受（化身层放行）但值不复活
%%     （Lamport revision older → stale_revision，第 2 节"防复活由
%%     revision/epoch 承担，不依赖化身"）；
%%   - 同一宽限化身 + 更高 revision 新值 → 必须可应用（接受性证明：
%%     拒绝旧化身将使重启 peer 的事件被永久拒绝——r1 修订动机）。
m9_old_incarnation_replay(A, B, C) ->
    ok = ensure_new_ready(B),
    ok = ensure_new_ready(C),
    Key = {mixed_test, m9_inc},
    ok = rpc:call(C, imboy_cache, put, [Key, v1, 300, replicate], ?RPC_MS),
    {ok, v1} = converge_get(B, Key, {ok, v1}),
    {ok, Rev1} = rpc:call(B, ?MODULE, read_rev_value, [Key], 5000),
    {_, OldBoot} = rpc:call(C, imboy_cache_sync, node_boot, [], 5000),
    %% C 换化身（sync+depcache 重启）：B 观察新化身 → 旧化身转入宽限集合
    ok = rpc:call(C, ?MODULE, restart_sync, [], ?RPC_MS),
    ready = wait_status(C, 120),
    ok = rpc:call(C, imboy_cache, put, [Key, v2_newinc, 300, replicate], ?RPC_MS),
    {ok, v2_newinc} = converge_get(B, Key, {ok, v2_newinc}),
    %% 前提 oracle：旧化身确实在 B 的宽限集合内（r1 语义成立的前提）
    true = rpc:call(
        B, ets, member, [imboy_cache_meta, {grace_incarnation, {C, OldBoot}}], 5000
    ),
    %% 重放：旧化身 + 旧 revision → 不复活（值保持 v2_newinc）
    ok = rpc:call(
        B,
        ?MODULE,
        inject_v2_set_frozen,
        [Key, <<"replay_stale">>, Rev1, C, OldBoot],
        5000
    ),
    timer:sleep(500),
    {ok, v2_newinc} = rpc:call(B, imboy_cache, get, [Key], ?RPC_MS),
    %% 接受性证明：同一宽限化身携带更高 revision → 应用成功（化身层不拒绝）
    {RevC, _} = Rev1,
    HiRev = {RevC + 100000, C},
    ok = rpc:call(
        B,
        ?MODULE,
        inject_v2_set_frozen,
        [Key, <<"grace_accepted">>, HiRev, C, OldBoot],
        5000
    ),
    {ok, <<"grace_accepted">>} = converge_get(B, Key, {ok, <<"grace_accepted">>}),
    true = rpc:call(A, ?MODULE, sync_alive, [], 5000),
    ok.

%% M10 r2 握手帧（C01 第 4 节 r2）：version_request 为 3 元组
%% {cache_version_request, FromPid, FromNode}（pid 直达，规避 syn 视图
%% 传播滞后饿死恢复轮）；旧 2 元组请求仍被应答（走视图反查，兼容
%% 混合矩阵旧节点）。应答为 9 元组 report（尾增发送者 sync pid）——
%% report 到达 C 的可观察副作用：{peer_incarnation, B} 注册重建。
m10_r2_handshake(A, B, C) ->
    ok = ensure_new_ready(B),
    ok = ensure_new_ready(C),
    SelfPid = self(),
    %% r2 新帧形状：3 元组（FromPid/FromNode 由发送端现场填充）
    {cache_version_request, SelfPid, B} =
        rpc:call(B, imboy_cache_sync_proto, version_request_frame, [SelfPid], 5000),
    %% r2 兼容：删掉 C 侧 B 的化身注册后，发送旧 2 元组请求（模拟旧版
    %% V2 节点）→ B 经视图反查应答 9 元组 report → C 重新注册 B 化身
    true = rpc:call(C, ets, delete, [imboy_cache_meta, {peer_incarnation, B}], 5000),
    _ = rpc:call(B, erlang, send, [imboy_cache_sync, {cache_version_request, C}], 5000),
    Registered = fun() ->
        rpc:call(C, ets, member, [imboy_cache_meta, {peer_incarnation, B}], 5000)
    end,
    true = converge_true(Registered, 100),
    %% 双向存活：兼容路径无崩溃（B 应答方、C report 处理方）
    true = rpc:call(B, ?MODULE, sync_alive, [], 5000),
    true = rpc:call(C, ?MODULE, sync_alive, [], 5000),
    true = rpc:call(A, ?MODULE, sync_alive, [], 5000),
    ok.

%% ===================================================================
%% 回滚演练（W5-06）：真实滚动回滚 = 同名 peer 进程以旧代码树重启
%% ===================================================================

%% 回滚演练（W5-06）：真实滚动回滚 = 同名 peer 进程以旧代码树重启。
%% 初始角色：A/B/C 均已真实升级为 V2。回滚顺序 C → A → B。

%% R1：滚动回滚 C（new → old）。回滚前 B 的 V2 数据就位并已收敛到 C；
%% 回滚后混合能力门禁拒绝 B 的新受管写入；C（旧）不保留 V2 数据，
%% 未分类 legacy 广播 A → C 可用；全程无崩溃。
r1_rollback_node_c(Nodes) ->
    #{a := A, b := B, c := C} = Nodes,
    KeyPre = {mixed_test, r_k1},
    ok = rpc:call(B, imboy_cache, put, [KeyPre, pre_rb, 300, replicate], ?RPC_MS),
    {ok, pre_rb} = converge_get(C, KeyPre, {ok, pre_rb}),
    C2 = restart_peer_as(legacy, c),
    %% 首个 legacy 节点加入后，受管写入必须拒绝。
    KeyMid = {mixed_test, r_k2},
    {error, incompatible_peers} =
        rpc:call(B, imboy_cache, put, [KeyMid, mid_rb, 300, replicate], ?RPC_MS),
    undefined = rpc:call(B, imboy_cache, get, [KeyMid], 5000),
    undefined = rpc:call(C2, imboy_cache, get, [KeyPre], 5000),
    %% C（旧）存活；legacy 广播通路 A → C 可用
    true = rpc:call(C2, ?MODULE, sync_alive, [], 5000),
    KeyL = {mixed_legacy, r_kl1},
    ok = rpc:call(A, ?MODULE, m1_broadcast_set, [KeyL, rb_legacy], ?RPC_MS),
    {ok, rb_legacy} = converge_get(C2, KeyL, {ok, rb_legacy}),
    ok.

%% R2：滚动回滚 A、B → 全旧集群。数据安全 oracle：回滚前
%% B 写入的 V2 键在旧节点上不复活（soft-state miss 回权威，无脏值）；
%% legacy 广播 B → A/C 全组可用、无崩溃。
r2_rollback_node_b(Nodes) ->
    #{a := A, c := C2} = Nodes,
    A = restart_peer_as(legacy, a),
    B2 = restart_peer_as(legacy, b),
    undefined = rpc:call(B2, imboy_cache, get, [{mixed_test, r_k1}], 5000),
    undefined = rpc:call(C2, imboy_cache, get, [{mixed_test, r_k1}], 5000),
    true = rpc:call(B2, ?MODULE, sync_alive, [], 5000),
    KeyL2 = {mixed_test, r_kl2},
    ok = rpc:call(B2, ?MODULE, m1_broadcast_set, [KeyL2, rb2_legacy], ?RPC_MS),
    {ok, rb2_legacy} = converge_get(A, KeyL2, {ok, rb2_legacy}),
    {ok, rb2_legacy} = converge_get(C2, KeyL2, {ok, rb2_legacy}),
    ok.

%% R3：全机回滚重启稳态：三节点全部以旧代码树重启一遍，legacy set 广播
%% 全组收敛，原系统语义完整（数据安全 oracle：旧 V2 键仍不复活）。
r3_full_cluster_rollback_restart(Nodes) ->
    #{a := A, b := B, c := C} = Nodes,
    restart_peer_as(legacy, a),
    restart_peer_as(legacy, b),
    restart_peer_as(legacy, c),
    undefined = rpc:call(A, imboy_cache, get, [{mixed_test, r_k1}], 5000),
    KeyF = {mixed_test, r_kfinal},
    ok = rpc:call(A, ?MODULE, m1_broadcast_set, [KeyF, all_legacy], ?RPC_MS),
    {ok, all_legacy} = converge_get(B, KeyF, {ok, all_legacy}),
    {ok, all_legacy} = converge_get(C, KeyF, {ok, all_legacy}),
    true = rpc:call(A, ?MODULE, sync_alive, [], 5000),
    true = rpc:call(B, ?MODULE, sync_alive, [], 5000),
    true = rpc:call(C, ?MODULE, sync_alive, [], 5000),
    ok.

%% 原 M4/M5 破坏所有新版内存副本；使用独立尾部集群，避免污染 U1/L1/L2
%% 及后续回滚前置。失败仍计入同一矩阵并决定进程非零退出。
destructive_legacy_tail(LegacyRoot, NewRoot) ->
    try
        {ok, #{a := A, b := B, c := C}} = start_mixed(LegacyRoot, NewRoot),
        [
            run_one("M4 legacy-only peers degrade new node (fail-closed)", fun() ->
                m4_legacy_only_degrades(A, B, C)
            end),
            run_one("M5 V2 peer return + node restart restores READY", fun() ->
                m5_recovery_after_v2_peer_returns(A, B, C)
            end)
        ]
    catch
        Class:Reason:Stack ->
            [
                {"BOOT original M4/M5 isolated tail", fail,
                    io_lib:format("~p:~p at ~p", [Class, Reason, lists:sublist(Stack, 3)])}
            ]
    after
        stop_all()
    end.

%% 原 M4 真路径：停止 B 的 sync/depcache，再重启 C，保留严格降级 oracle。
m4_legacy_only_degrades(A, B, C) ->
    ready = wait_status(B, 60),
    ok = rpc:call(B, ?MODULE, peer_sync_down, [], ?RPC_MS),
    ok = rpc:call(C, ?MODULE, restart_sync, [], ?RPC_MS),
    degraded = wait_status(C, 400),
    Key = {mixed_test, m4_k4},
    undefined = rpc:call(C, imboy_cache, get, [Key], 5000),
    {error, {not_ready, degraded}} = rpc:call(
        C, imboy_cache, put, [Key, nope, 60, replicate], ?RPC_MS
    ),
    true = rpc:call(A, ?MODULE, sync_alive, [], 5000),
    ok.

%% 原 M5 真路径与 READY oracle：无剩余新版权威时不得用 L2 的 PASS 顶替。
m5_recovery_after_v2_peer_returns(A, B, C) ->
    ok = rpc:call(B, ?MODULE, restart_sync, [], ?RPC_MS),
    ready = wait_status(B, 120),
    ok = rpc:call(C, ?MODULE, restart_sync, [], ?RPC_MS),
    ready = wait_status(C, 120),
    Key = {mixed_test, m5_k5},
    ok = rpc:call(B, imboy_cache, put, [Key, revived, 120, replicate], ?RPC_MS),
    {ok, revived} = converge_get(C, Key, {ok, revived}),
    true = rpc:call(A, ?MODULE, sync_alive, [], 5000),
    ok.

%% ===================================================================
%% 集群管理
%% ===================================================================

start_mixed(LegacyRoot, NewRoot) ->
    RandBin = integer_to_binary(erlang:unique_integer([positive]), 36),
    Main = binary_to_atom(iolist_to_binary("c21m" ++ binary_to_list(RandBin)), utf8),
    {ok, _} = net_kernel:start([Main, longnames]),
    CookieAtom = binary_to_atom(
        iolist_to_binary(
            "ck" ++ binary_to_list(integer_to_binary(erlang:unique_integer([positive]), 36))
        ),
        utf8
    ),
    true = erlang:set_cookie(node(), CookieAtom),
    DriverDir = filename:absname(filename:dirname(code:which(?MODULE))),
    %% 驱动节点侧可变状态；前一矩阵已完成 stop_all 清理。
    Ctx = new_context(),
    true = ets:insert(Ctx, [
        {legacy_root, LegacyRoot},
        {new_root, NewRoot},
        {cookie, CookieAtom},
        {driver_dir, DriverDir}
    ]),
    Rand = binary_to_list(RandBin),
    NameA = binary_to_atom(iolist_to_binary("c21" ++ Rand ++ "_la"), utf8),
    NameB = binary_to_atom(iolist_to_binary("c21" ++ Rand ++ "_nb"), utf8),
    NameC = binary_to_atom(iolist_to_binary("c21" ++ Rand ++ "_nc"), utf8),
    {ok, _PA, A} = peer_boot(a, NameA, legacy, Ctx),
    {ok, _PB, B} = peer_boot(b, NameB, new, Ctx),
    {ok, _PC, C} = peer_boot(c, NameC, new, Ctx),
    %% 先由显式单节点自然建立权威；同时启动的空集群必须 fail-closed，
    %% 不能在演练中强设 READY 或把无人应答当作已恢复。
    ok = rpc:call(B, ?MODULE, init_on_peer, [], ?RPC_MS),
    ready = wait_status(B, 10),
    true = rpc:call(B, net_kernel, connect_node, [C], ?RPC_MS),
    ok = rpc:call(C, ?MODULE, init_on_peer, [], ?RPC_MS),
    ready = wait_status(C, 30),
    %% 完整 V2 能力阶段写入基线；混合阶段绝不绕过发布门禁。
    ok = rpc:call(
        B, imboy_cache, put, [{mixed_test, m1_managed}, managed_v1, 600, replicate], ?RPC_MS
    ),
    ok = rpc:call(B, imboy_cache, put, [{mixed_test, m2_k2}, v2_val, 600, replicate], ?RPC_MS),
    {ok, v2_val} = converge_get(C, {mixed_test, m2_k2}, {ok, v2_val}),
    ok = rpc:call(
        B, imboy_cache, put, [{mixed_test, m3_deleted}, deleted_old, 600, replicate], ?RPC_MS
    ),
    {ok, deleted_old} = converge_get(C, {mixed_test, m3_deleted}, {ok, deleted_old}),
    ok = rpc:call(B, imboy_cache, put_delete, [{mixed_test, m3_deleted}], ?RPC_MS),
    true = converge_true(
        fun() ->
            rpc:call(C, imboy_cache_sync, replicate_state, [{mixed_test, m3_deleted}], 5000) =:=
                deleted
        end,
        100
    ),
    %% 旧节点没有版本报告能力；先建立 V2 权威组再接入旧节点。
    %% M3 仍单独验证带 legacy peer 的新版重启恢复，不由启动顺序代替。
    true = rpc:call(A, net_kernel, connect_node, [B], ?RPC_MS),
    true = rpc:call(A, net_kernel, connect_node, [C], ?RPC_MS),
    ok = rpc:call(A, ?MODULE, init_on_peer, [], ?RPC_MS),
    %% 旧节点无 imboy_cache_meta 表：就绪判定用 sync 进程存活
    true = rpc:call(A, ?MODULE, sync_alive, [], 5000),
    ready = wait_status(B, 150),
    ready = wait_status(C, 150),
    {ok, #{a => A, b => B, c => C}}.

peer_boot(Which, Name, Role, Ctx) ->
    {ok, Pid, Node} = peer:start(#{
        name => Name, host => "127.0.0.1", wait_boot => false, args => peer_args(Role, Ctx)
    }),
    true = ets:insert(Ctx, {Which, {Pid, Name, Node}}),
    pong = wait_peer(Node, 200),
    {ok, Pid, Node}.

peer_args(Role, Ctx) ->
    [{legacy_root, LR}, {new_root, NR}, {cookie, Cookie}, {driver_dir, DD}] =
        ets:lookup(Ctx, legacy_root) ++
            ets:lookup(Ctx, new_root) ++
            ets:lookup(Ctx, cookie) ++
            ets:lookup(Ctx, driver_dir),
    Root =
        case Role of
            legacy -> LR;
            new -> NR
        end,
    Deps = [
        filename:join(Root, "deps/" ++ D ++ "/ebin")
     || D <- ["depcache", "syn"]
    ],
    [
        "-setcookie",
        atom_to_list(Cookie),
        "-kernel",
        "inet_dist_use_interface",
        "{127,0,0,1}",
        "-pa",
        DD,
        "-pa",
        filename:join(Root, "ebin")
        | Deps
    ].

%% 同名重启 peer 为指定角色（真实滚动升级/回滚 = 进程换部署）：
%% 停旧进程 → 以新代码树同名拉起 → 重建全网状连接 → init → 就绪判定。
restart_peer_as(Role, Which) ->
    [{Which, {OldPid, ShortName, _FullName}}] = ets:lookup(c21ctx, Which),
    ok = peer:stop(OldPid),
    {ok, _NewPid, Node} = peer:start(#{
        name => ShortName,
        host => "127.0.0.1",
        wait_boot => false,
        args => peer_args(Role, c21ctx)
    }),
    true = ets:insert(c21ctx, {Which, {_NewPid, ShortName, Node}}),
    pong = wait_peer(Node, 200),
    Others = [N || {K, {_, _, N}} <- ets:tab2list(c21ctx), K =/= Which, is_atom(N)],
    _ = [ok = connect_retry(Node, N) || N <- Others],
    _ = [ok = connect_retry(N, Node) || N <- Others],
    ok = rpc:call(Node, ?MODULE, init_on_peer, [], ?RPC_MS),
    case Role of
        legacy -> true = rpc:call(Node, ?MODULE, sync_alive, [], 5000);
        new -> ready = wait_status(Node, 150)
    end,
    true = ets:insert(c21ctx, {Which, {_NewPid, ShortName, Node}}),
    Node.

%% 同名重启后旧连接拆除期间 connect_node 会瞬时 false，有界重试
connect_retry(_From, _To, 0) ->
    erlang:error({connect_timeout, _From, _To});
connect_retry(From, To, N) ->
    case rpc:call(From, net_kernel, connect_node, [To], ?RPC_MS) of
        true ->
            ok;
        _ ->
            timer:sleep(200),
            connect_retry(From, To, N - 1)
    end.

connect_retry(From, To) ->
    connect_retry(From, To, 15).

%% 上下文只属于本 driver 进程；不得删除或接管其他进程的同名表。
new_context() ->
    case ets:whereis(c21ctx) of
        undefined -> ets:new(c21ctx, [set, named_table, public]);
        _ -> erlang:error(driver_context_not_cleaned)
    end.

drop_context() ->
    case ets:info(c21ctx, owner) of
        undefined ->
            ok;
        Owner when Owner =:= self() ->
            true = ets:delete(c21ctx),
            ok;
        _ ->
            erlang:error(foreign_driver_context)
    end.

%% 从 Ctx 读取当前 peer pid 集中清理（回滚场景 pid 已轮换）
stop_all() ->
    Entries =
        case ets:info(c21ctx, owner) of
            undefined -> [];
            Owner when Owner =:= self() -> ets:tab2list(c21ctx);
            _ -> erlang:error(foreign_driver_context)
        end,
    Pids = [P || {K, {P, _S, _F}} <- Entries, is_atom(K)],
    _ = [
        try
            peer:stop(P)
        catch
            _:_ -> ok
        end
     || P <- Pids
    ],
    ok = drop_context(),
    try
        net_kernel:stop()
    catch
        _:_ -> ok
    end,
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

wait_status(Node, Seconds) ->
    wait_status_loop(Node, Seconds * 10, undefined).

wait_status_loop(Node, 0, Last) ->
    erlang:error({status_timeout, Last, rpc:call(Node, ?MODULE, dump_debug, [], 5000)});
wait_status_loop(Node, N, _Last) ->
    case rpc:call(Node, ?MODULE, status_get, [], 5000) of
        {ok, S} when S =:= ready orelse S =:= degraded ->
            S;
        _Other ->
            timer:sleep(100),
            wait_status_loop(Node, N - 1, _Other)
    end.

%% C10 场景前置：新节点确保 ready（前序场景可能留下中间态）。
%% 仅对新节点调用（旧节点无 imboy_cache_meta 表，走 sync_alive 判定）。
ensure_new_ready(Node) ->
    case rpc:call(Node, ?MODULE, status_get, [], 5000) of
        {ok, ready} ->
            ok;
        _ ->
            ok = rpc:call(Node, ?MODULE, restart_sync, [], ?RPC_MS),
            ready = wait_status(Node, 150)
    end.

%% 有界轮询至谓词为真（C10：等待协议副作用落地，非固定 sleep 断言）。
converge_true(_Pred, 0) ->
    false;
converge_true(Pred, Tries) ->
    case Pred() of
        true ->
            true;
        false ->
            timer:sleep(100),
            converge_true(Pred, Tries - 1)
    end.

converge_get(Node, Key, Expected) ->
    converge_get(Node, Key, Expected, 15000).

converge_get(Node, Key, Expected, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    case converge_loop(Node, Key, Expected, Deadline) of
        Expected -> Expected;
        Other -> erlang:error({converge_timeout, Node, Key, Expected, Other})
    end.

converge_loop(Node, Key, Expected, Deadline) ->
    V = rpc:call(Node, imboy_cache, get, [Key], 5000),
    TimedOut = erlang:monotonic_time(millisecond) >= Deadline,
    case {V, TimedOut} of
        {Expected, _} ->
            V;
        {_, true} ->
            V;
        _ ->
            timer:sleep(50),
            converge_loop(Node, Key, Expected, Deadline)
    end.

%% ===================================================================
%% peer 端辅助（rpc 执行；须在新旧两棵树上都可用）
%% ===================================================================

%% 暂时从同步成员视图撤出，保留真实 V2 权威，禁止伪造空集群 READY。
change_membership(leave) ->
    syn:leave(imboy_cache, dsync_handler, whereis(imboy_cache_sync));
change_membership(join) ->
    syn:join(imboy_cache, dsync_handler, whereis(imboy_cache_sync), #{cache_protocol => 2}).

init_on_peer() ->
    ok = application:load(imboy),
    %% 隔离测试 namespace（新节点读；旧节点无对应模块，无害）
    application:set_env(
        imboy,
        cache_registry_overrides,
        #{mixed_test => {replicate, false}}
    ),
    application:set_env(imboy, cache_replication, #{whitelist => [mixed_test]}),
    application:set_env(imboy, cache_fenced_namespaces, [mixed_fenced]),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(syn),
    ok = syn:add_node_to_scopes([imboy_cache]),
    {ok, Dp} = depcache:start_link(imboy_cache, #{memory_max => 64}),
    true = unlink(Dp),
    {ok, M} = elib_metric:start_link(),
    true = unlink(M),
    {ok, S} = imboy_cache_sync:start_link(),
    true = unlink(S),
    ok = wait_local_sync(100).

wait_local_sync(0) ->
    erlang:error(local_sync_timeout);
wait_local_sync(N) ->
    case whereis(imboy_cache_sync) of
        P when is_pid(P) -> ok;
        undefined ->
            timer:sleep(100),
            wait_local_sync(N - 1)
    end.

%% sync 进程可能已不存在（peer_sync_down 之后），stop 需守卫
stop_sync_guarded() ->
    case whereis(imboy_cache_sync) of
        undefined -> ok;
        P when is_pid(P) -> ok = gen_server:stop(imboy_cache_sync, normal, 10000)
    end.

%% 模拟新节点重启：sync+depcache 全量重启（内存副本全丢，新 BootId）
restart_sync() ->
    %% M5 场景里 B 的 sync 可能已被 peer_sync_down 停掉，stop 需守卫
    stop_sync_guarded(),
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

%% V2 能力离场：sync+depcache 停止且不重启（节点进程仍在）
peer_sync_down() ->
    stop_sync_guarded(),
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

sync_alive() ->
    case whereis(imboy_cache_sync) of
        P when is_pid(P) -> is_process_alive(P);
        undefined -> false
    end.

%% 新节点的恢复状态（旧节点无 imboy_cache_meta 表，勿对旧节点调用）
status_get() ->
    case ets:lookup(imboy_cache_meta, status) of
        [{_, S}] -> {ok, S};
        [] -> {error, not_found}
    end.

%% M1 旧节点侧：开启 dsync 后走旧版 set 广播路径（legacy {set,...} 全组）
m1_broadcast_set(Key, Value) ->
    application:set_env(imboy, dsync_enabled, true),
    ok = imboy_cache:set(Key, Value, 120),
    application:unset_env(imboy, dsync_enabled),
    ok.

%% syn:publish 在调用进程直接发送消息；同发送者随后 call 同一接收 PID，
%% pong 证明前一个目标 SET 已被同步 actor 处理，避免只依赖总拒绝数。
m1_broadcast_set(Key, Value, Receiver) ->
    [Pid] = [P || {P, _} <- syn:members(imboy_cache, dsync_handler), node(P) =:= Receiver],
    ok = m1_broadcast_set(Key, Value),
    pong = gen_server:call(Pid, ping_ok, 5000),
    ok.

%% 定向注入 V2 SET（诊断用；OriginNode 避开自环过滤）。
%% C01 冻结后缺 epoch/config_hash/registry_hash 的事件会被结构性拒绝
%% （missing_protocol_field，第 1 节），统一走 frozen 构造；
%% <<0:128>> 为快照回放合法特例 boot_id（第 2 节）。
inject_v2_set(Key, Value, Rev, OriginNode) ->
    inject_v2_set_frozen(Key, Value, Rev, OriginNode, <<0:128>>).

%% C01 冻结字段版注入（C10 收口）：epoch/hash 由执行节点现场计算
%% （同配置 peer 匹配；漂移注入由场景改配置实现，不在此伪造 hash）；
%% OriginBootId 可指定（M9 宽限化身重放，r1 语义）。仅 rpc 到新节点执行
%% （旧树无 imboy_cache_sync_proto，模块加载在 peer 上按各自 code path 解析）。
inject_v2_set_frozen(Key, Value, Rev, OriginNode, OriginBootId) ->
    Event = #{
        protocol => 2,
        operation => set,
        key => Key,
        value => Value,
        expires_at_ms => erlang:system_time(millisecond) + 120000,
        revision => Rev,
        origin_seq => 99999,
        origin_node => OriginNode,
        origin_boot_id => OriginBootId,
        epoch => imboy_cache_sync_proto:epoch(),
        config_hash => imboy_cache_sync_proto:config_hash(),
        registry_hash => imboy_cache_sync_proto:registry_hash()
    },
    _ = imboy_cache_sync:broadcast(Event),
    ok.

%% C01 第 1 节拒绝计数探针（elib_metric 节点本地计数器）：
%% 场景前重置、断言时读取，保证 reject_total 递增断言的精度。
reject_reset() ->
    ok = elib_metric:reset_counter(reject_total).

reject_read() ->
    #{counters := Counters} = elib_metric:get_all_metrics(),
    maps:get(reject_total, Counters, 0).

dump_debug() ->
    Status =
        try
            status_get()
        catch
            _:_ -> no_meta_table
        end,
    Members =
        try
            [node(P) || {P, _} <- syn:members(imboy_cache, dsync_handler)]
        catch
            _:_ -> []
        end,
    #{
        node => node(),
        status => Status,
        sync_alive => sync_alive(),
        syn_members => Members
    }.

%% 新节点上 legacy set 不应产生 V2 rev 元数据（19.8：legacy 保持原语义）
read_rev_expect_absent(B, Key) ->
    absent = rpc:call(B, ?MODULE, rev_present, [Key], 5000),
    ok.

rev_present(Key) ->
    case ets:lookup(imboy_cache_meta, {rev, Key}) of
        [] -> absent;
        [_] -> present
    end.

%% C10/M1/M9：读取当前包含 epoch 的版本行中的 Lamport revision。
%% 仅对新节点调用。
read_rev_value(Key) ->
    case ets:lookup(imboy_cache_meta, {rev, Key}) of
        [{{rev, Key}, {Rev, _C, _Exp, _St, _Epoch}}] -> {ok, Rev};
        [] -> {error, not_found}
    end.

results_json(Results, ElapsedMs) ->
    Scenarios = [
        io_lib:format(
            "{\"name\": ~s, \"status\": ~s, \"detail\": ~s}",
            [
                json_str(Name),
                json_str(atom_to_list(Status)),
                json_str(lists:flatten(Detail))
            ]
        )
     || {Name, Status, Detail} <- Results
    ],
    %% C14 证据 schema：candidate 绑定由环境变量注入（与 cache_v21_dcheck
    %% 同款；缺失记 null，校验器对收口系列缺绑定判 FAIL）
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
    iolist_to_binary([
        "{\"task_id\": \"CACHE_V221_BACKEND_CLOSEOUT_V1\", \"candidate_sha256\": ",
        Candidate,
        ", \"driver_node\": \"",
        atom_to_list(node()),
        "\", \"elapsed_ms\": ",
        integer_to_list(ElapsedMs),
        ", \"utc\": \"",
        iso_utc(),
        "\", \"scenarios\": [",
        lists:join(", ", Scenarios),
        "]}"
    ]).

%% 真实 UTC 时间戳（RFC 3339，毫秒取自系统时钟，无伪造占位）
iso_utc() ->
    calendar:system_time_to_rfc3339(
        erlang:system_time(millisecond), [{unit, millisecond}, {offset, "Z"}]
    ).

json_str(S) ->
    %% 尾部追加式：保证转义对（反斜杠+引号）顺序不变。头插+reverse 会把
    %% 转义对翻转为 引号+反斜杠（mixed_r1 实证），头插不 reverse 则整串倒序。
    Escape =
        fun
            (Ch, Acc) when Ch =:= $" orelse Ch =:= $\\ -> Acc ++ [$\\, Ch];
            (Ch, Acc) when Ch < 32 -> Acc;
            (Ch, Acc) -> Acc ++ [Ch]
        end,
    "\"" ++ lists:foldl(Escape, [], unicode:characters_to_list(S)) ++ "\"".
