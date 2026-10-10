%% @doc 混合版本真实运行演练（W5-05）：旧/新版本 imboy_cache_sync 真实 BEAM
%% 节点混合组网验证（19.8 能力交集语义）。
%%
%% 旧节点：INTEGRATION_BASE（V2 引入前的 main，如 3a01829d）独立构建树；
%% 新节点：当前 main 构建树。两树共用同一 syn scope（imboy_cache）与
%% group（dsync_handler）、同一 depcache server 名（imboy_cache）。
%%
%% 场景：
%%   M1 旧节点 legacy set 广播 → 新节点按原语义应用（不产生 V2 元数据）
%%   M2 新节点 V2 事件 → 旧节点 catch-all 忽略：不崩溃、不应用、legacy 功能完好
%%   M3 新节点重启，peer 新旧混合：仅 V2 应答者参与恢复，快照回填 V2 键
%%   M4 新节点仅存 legacy peer：有界重试后 DEGRADED（fail-closed：读 miss、写拒绝）
%%   M5 V2 peer 回归后重启该节点恢复 READY（DEGRADED 无自动退出路径，运维要点）
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
    sync_alive/0,
    status_get/0,
    rev_present/1,
    m1_broadcast_set/2,
    inject_v2_set/4,
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
            {ok, PeerPids, {A, B, C}} = start_mixed(LE, NE),
            Results =
                try
                    Scenarios = scenario_list(A, B, C),
                    [run_one(Name, Fun) || {Name, Fun} <- Scenarios]
                after
                    stop_cluster(PeerPids)
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

scenario_list(A, B, C) ->
    [
        {"M1 legacy set propagates to new node (original semantics)", fun() ->
            m1_legacy_set(A, B)
        end},
        {"M2 V2 event ignored by legacy peer without crash", fun() ->
            m2_v2_no_crash_legacy(A, B)
        end},
        {"M3 new-node restart recovers from mixed peers (V2 backfill only)", fun() ->
            m3_mixed_restart_backfill(A, B, C)
        end},
        {"M4 legacy-only peers degrade new node (fail-closed)", fun() ->
            m4_legacy_only_degrades(A, B, C)
        end},
        {"M5 V2 peer return + node restart restores READY", fun() ->
            m5_recovery_after_v2_peer_returns(A, B, C)
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
%% 新节点 B 按原语义 depcache:set；B 无 V2 rev 元数据（legacy 不进 V2 状态）。
m1_legacy_set(A, B) ->
    Key = {mixed_test, m1_k1},
    ok = rpc:call(A, ?MODULE, m1_broadcast_set, [Key, legacy_v1], ?RPC_MS),
    {ok, legacy_v1} = converge_get(B, Key, {ok, legacy_v1}),
    ok = read_rev_expect_absent(B, Key),
    ok.

%% M2：新节点 V2 发布 → 旧节点收到 {cache_sync, #{protocol:=2,...}}：
%% catch-all 忽略——不崩溃、不应用（soft-state miss 回权威）、legacy 通路无损。
m2_v2_no_crash_legacy(A, B) ->
    Key = {mixed_test, m2_k2},
    ok = rpc:call(B, imboy_cache, put, [Key, v2_val, 120, replicate], ?RPC_MS),
    {ok, v2_val} = converge_get(B, Key, {ok, v2_val}),
    timer:sleep(300),
    true = rpc:call(A, ?MODULE, sync_alive, [], 5000),
    undefined = rpc:call(A, imboy_cache, get, [Key], 5000),
    %% 旧节点 legacy 功能在收到 V2 map 后仍然完好
    KeyL = {mixed_test, m2_kl},
    ok = rpc:call(A, ?MODULE, m1_broadcast_set, [KeyL, legacy_after_v2], ?RPC_MS),
    {ok, legacy_after_v2} = converge_get(B, KeyL, {ok, legacy_after_v2}),
    ok.

%% M3：新节点 C 重启（depcache+sync 全换新 BootId），peer = 旧 A + 新 B：
%% 仅 B 应答版本上界+快照；A 静默不阻塞回合。C READY 后 V2 键回填可见，
%% legacy 键不回填（无 V2 元数据，soft-state miss）。
m3_mixed_restart_backfill(A, B, C) ->
    ready = wait_status(B, 60),
    Key2 = {mixed_test, m2_k2},
    Key1 = {mixed_test, m1_k1},
    ok = rpc:call(C, ?MODULE, restart_sync, [], ?RPC_MS),
    ready = wait_status(C, 120),
    {ok, v2_val} = converge_get(C, Key2, {ok, v2_val}),
    undefined = rpc:call(C, imboy_cache, get, [Key1], 5000),
    true = rpc:call(A, ?MODULE, sync_alive, [], 5000),
    ok.

%% M4：停新 B 的 sync/depcache（模拟 V2 能力暂时离场），重启 C：
%% 集群仅剩 legacy A 可见 → 版本上界无人应答 → 有界重试（3×5s）后
%% DEGRADED：replicate 读 undefined（soft-state miss）、写被拒（fail-closed）。
m4_legacy_only_degrades(A, B, C) ->
    ready = wait_status(B, 60),
    ok = rpc:call(B, ?MODULE, peer_sync_down, [], ?RPC_MS),
    ok = rpc:call(C, ?MODULE, restart_sync, [], ?RPC_MS),
    degraded = wait_status(C, 400),
    Key4 = {mixed_test, m4_k4},
    undefined = rpc:call(C, imboy_cache, get, [Key4], 5000),
    {error, {not_ready, degraded}} =
        rpc:call(C, imboy_cache, put, [Key4, nope, 60, replicate], ?RPC_MS),
    true = rpc:call(A, ?MODULE, sync_alive, [], 5000),
    ok.

%% M5：B 的 sync/depcache 重启回归（V2 能力恢复）；DEGRADED 无自动退出路径，
%% 重启 C 后经 B 的版本上界应答回到 READY（运维恢复路径演练）。注意：M4 已把
%% B、C 双端内存副本清空，replica 数据不复活（soft-state miss 语义正确），
%% 故本场景验证的是恢复后的端到端活性——B 新写 → C 可见。
m5_recovery_after_v2_peer_returns(A, B, C) ->
    ok = rpc:call(B, ?MODULE, restart_sync, [], ?RPC_MS),
    ready = wait_status(B, 120),
    ok = rpc:call(C, ?MODULE, restart_sync, [], ?RPC_MS),
    ready = wait_status(C, 120),
    Key5 = {mixed_test, m5_k5},
    ok = rpc:call(B, imboy_cache, put, [Key5, revived, 120, replicate], ?RPC_MS),
    {ok, revived} = converge_get(C, Key5, {ok, revived}),
    true = rpc:call(A, ?MODULE, sync_alive, [], 5000),
    ok.

%% ===================================================================
%% 集群管理
%% ===================================================================

start_mixed(LegacyEbin, NewEbin) ->
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
    DepsOf =
        fun(Root) ->
            [
                filename:join(Root, "deps/" ++ D ++ "/ebin")
             || D <- ["depcache", "syn"]
            ]
        end,
    ArgsOf =
        fun(Name) ->
            Root =
                case Name of
                    legacy -> LegacyEbin;
                    _ -> NewEbin
                end,
            [
                "-setcookie",
                atom_to_list(CookieAtom),
                "-kernel",
                "inet_dist_use_interface",
                "{127,0,0,1}",
                "-pa",
                DriverDir,
                "-pa",
                filename:join(Root, "ebin")
                | DepsOf(Root)
            ]
        end,
    Rand = binary_to_list(RandBin),
    NameA = binary_to_atom(iolist_to_binary("c21" ++ Rand ++ "_la"), utf8),
    NameB = binary_to_atom(iolist_to_binary("c21" ++ Rand ++ "_nb"), utf8),
    NameC = binary_to_atom(iolist_to_binary("c21" ++ Rand ++ "_nc"), utf8),
    {ok, PA, A} = peer:start(#{
        name => NameA, host => "127.0.0.1", wait_boot => false, args => ArgsOf(legacy)
    }),
    pong = wait_peer(A, 200),
    {ok, PB, B} = peer:start(#{
        name => NameB, host => "127.0.0.1", wait_boot => false, args => ArgsOf(new)
    }),
    pong = wait_peer(B, 200),
    {ok, PC, C} = peer:start(#{
        name => NameC, host => "127.0.0.1", wait_boot => false, args => ArgsOf(new)
    }),
    pong = wait_peer(C, 200),
    true = rpc:call(A, net_kernel, connect_node, [B], ?RPC_MS),
    true = rpc:call(A, net_kernel, connect_node, [C], ?RPC_MS),
    true = rpc:call(B, net_kernel, connect_node, [C], ?RPC_MS),
    ok = rpc:call(A, ?MODULE, init_on_peer, [], ?RPC_MS),
    ok = rpc:call(B, ?MODULE, init_on_peer, [], ?RPC_MS),
    ok = rpc:call(C, ?MODULE, init_on_peer, [], ?RPC_MS),
    %% 旧节点无 imboy_cache_meta 表：就绪判定用 sync 进程存活
    true = rpc:call(A, ?MODULE, sync_alive, [], 5000),
    ready = wait_status(B, 150),
    ready = wait_status(C, 150),
    {ok, [PA, PB, PC], {A, B, C}}.

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

%% 定向注入 V2 SET（诊断用；OriginNode 避开自环过滤）
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
    iolist_to_binary([
        "{\"driver_node\": \"",
        atom_to_list(node()),
        "\", \"elapsed_ms\": ",
        integer_to_list(ElapsedMs),
        ", \"utc\": \"",
        iso_utc(),
        "\", \"scenarios\": [",
        lists:join(", ", Scenarios),
        "]}"
    ]).

iso_utc() ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:system_time_to_universal_time(
        erlang:system_time(second), nanosecond
    ),
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.000Z", [Y, Mo, D, H, Mi, S]).

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
