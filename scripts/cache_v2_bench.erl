%% cache_v2_bench — cache-v2.2.1 候选性能驱动（C12/A7）
%%
%% 与 W0 profile（scripts/cache_w0_bench.erl）同口径的环境锚与输出约定，
%% 但路径覆盖 V2 新链路：replicate put / invalidate（fenced generation）/
%% replicate key 读取（rev+rindex 交互）/ memo 回填，以及 S/M/L Value
%% 分布（计划 C12：覆盖新 API/协议 + S/M/L Key/Value + 混合负载）。
%%
%% 单节点 loopback 口径（本地相对阈值，非生产 SLO——AC-26/27 外部判定）。
%%
%% 用法（由 c12-window-plan.md 的窗口命令调用）：
%%   CACHE_V2_OUTPUT=<json> CACHE_V2_OPERATIONS=10000 \
%%   CANDIDATE_SHA256=<64hex|unset> \
%%   erl -noshell -pa ebin -pa <本脚本编译目录> -eval 'cache_v2_bench:run()'
%% （jsone/depcache/syn 依赖路径由 app_up/0 依据 imboy ebin 位置自动补齐）
-module(cache_v2_bench).
-export([run/0]).

-define(REP_NS, cache_v2_bench_rep).
-define(LOC_NS, cache_v2_bench_loc).

run() ->
    Output = os:getenv("CACHE_V2_OUTPUT"),
    false = (Output =:= false),
    Operations = list_to_integer(os:getenv("CACHE_V2_OPERATIONS", "10000")),
    true = Operations > 0 andalso Operations =< 100000,
    ok = app_up(),
    %% 预热：replicate 首写走 register/快照外的全链（journal/rev 行建立），
    %% 不预热会把首条脏成本计入均摊，轮间不可比
    warmup(256),
    Rows = [
        bench(rep_put_s, 1, Operations, 64, 1024),
        bench(rep_put_m, 1, Operations, 1024, 1024),
        bench(rep_put_l, 1, Operations, 8192, 1024),
        bench(rep_put_m_w8, 8, Operations, 1024, 1024),
        bench(loc_put_m, 1, Operations, 1024, 1024),
        bench(rep_inval, 1, Operations, 64, 1024),
        bench(rep_get, 1, Operations, 64, 1024),
        bench(memo_refill, 1, Operations, 1024, 256),
        bench(mixed, 4, Operations, 1024, 1024)
    ],
    Json = result_json(Rows, Operations),
    ok = file:write_file(Output, Json),
    io:format("== cache_v2_bench: ~p rows, ~p ops ==~n", [length(Rows), Operations]),
    halt(0).

%% --- 基元：各负载模式的单次操作 ---

op(rep_put_s, K, V, _) ->
    ok = imboy_cache:put({?REP_NS, {s, K}}, V, 600, replicate);
op(rep_put_m, K, V, _) ->
    ok = imboy_cache:put({?REP_NS, {m, K}}, V, 600, replicate);
op(rep_put_l, K, V, _) ->
    ok = imboy_cache:put({?REP_NS, {l, K}}, V, 600, replicate);
op(rep_put_m_w8, K, V, _) ->
    ok = imboy_cache:put({?REP_NS, {w, K}}, V, 600, replicate);
op(loc_put_m, K, V, _) ->
    ok = imboy_cache:put({?LOC_NS, {m, K}}, V, 600, local);
op(rep_inval, K, _, _) ->
    ok = imboy_cache_sync:invalidate({?REP_NS, {s, K}});
op(rep_get, K, _, _) ->
    _ = imboy_cache:get({?REP_NS, {s, K}}),
    ok;
op(memo_refill, K, V, _) ->
    %% memo 回填兼容（7.x）：flush 后经 get_wait/loader 重建
    %% （miss 首调用者成为 writer，本进程 set 回填，走真实 miss-rebuild 路径）
    Key = {?LOC_NS, {memo, K}},
    ok = imboy_cache:flush(Key),
    _ = imboy_cache:get_wait(Key),
    ok = imboy_cache:set(Key, V, 600, []),
    ok;
op(mixed, K, V, I) ->
    case I rem 4 of
        0 ->
            ok = imboy_cache:put({?REP_NS, {x, K}}, V, 600, replicate);
        1 ->
            _ = imboy_cache:get({?REP_NS, {x, K rem 128}}),
            ok;
        2 ->
            ok = imboy_cache_sync:invalidate({?REP_NS, {x, K}});
        _ ->
            ok = imboy_cache:put({?LOC_NS, {x, K}}, V, 600, local)
    end.

%% --- 计时骨架 ---
%% 微秒计时（本地 ETS 读类负载单次 <1us，毫秒精度下 duration_ms=0 会让
%% ops_per_sec 退化为 fallback，轮间不可比）；duration_ms 四舍五入保留

bench(Kind, Workers, Operations, ValueBytes, Keyspace) when Workers =:= 1 ->
    V = value_bytes0(ValueBytes),
    T0 = erlang:monotonic_time(microsecond),
    loop(Kind, 0, Operations, V, Keyspace),
    T1 = erlang:monotonic_time(microsecond),
    row(Kind, Workers, Operations, ValueBytes, Keyspace, T1 - T0);
bench(Kind, Workers, Operations, ValueBytes, Keyspace) ->
    V = value_bytes0(ValueBytes),
    Per = Operations div Workers,
    Parent = self(),
    T0 = erlang:monotonic_time(microsecond),
    %% 固定分片区间（W 序号）：unique_integer 起点会跳号，导致实际执行
    %% 数与 row 声称的 Per*Workers 不符
    Pids = [
        begin
            Base = (W - 1) * Per,
            spawn(fun() ->
                loop(Kind, Base, Base + Per, V, Keyspace),
                Parent ! done
            end)
        end
     || W <- lists:seq(1, Workers)
    ],
    [
        receive
            done -> ok
        end
     || _ <- Pids
    ],
    T1 = erlang:monotonic_time(microsecond),
    row(Kind, Workers, Per * Workers, ValueBytes, Keyspace, T1 - T0).

loop(_Kind, I, I, _V, _Keyspace) ->
    ok;
loop(Kind, I, N, V, Keyspace) ->
    ok = op(Kind, I rem Keyspace, V, I),
    loop(Kind, I + 1, N, V, Keyspace).

row(Kind, Workers, Ops, ValueBytes, Keyspace, ElapsedUs) ->
    #{
        kind => Kind,
        workers => Workers,
        operations => Ops,
        value_bytes => ValueBytes,
        keyspace => Keyspace,
        duration_ms => (ElapsedUs + 500) div 1000,
        ops_per_sec =>
            case ElapsedUs of
                0 -> Ops;
                _ -> (Ops * 1000000) div ElapsedUs
            end
    }.

value_bytes0(N) when N =< 64 -> <<0:(64 * 8)>>;
value_bytes0(N) when N =< 1024 -> <<0:(1024 * 8)>>;
value_bytes0(_) -> <<0:(8192 * 8)>>.

warmup(N) ->
    V = <<0:1024>>,
    [
        begin
            ok = imboy_cache:put({?REP_NS, {warm, I}}, V, 600, replicate),
            _ = imboy_cache:get({?REP_NS, {warm, I}}),
            ok = imboy_cache_sync:invalidate({?REP_NS, {warm, I}})
        end
     || I <- lists:seq(1, N)
    ],
    ok.

app_up() ->
    ok = deps_on_path(),
    ok = application:load(imboy),
    %% 隔离测试 namespace（照抄 cache_v21_dcheck:init_on_peer/0 模式，
    %% 不进生产默认配置）：REP_NS 注册 replicate、LOC_NS 注册 local，
    %% whitelist 含两者；fenced 列表对齐 dcheck 口径（invalidate 走
    %% generation 栅栏路径）
    application:set_env(
        imboy,
        cache_registry_overrides,
        #{?REP_NS => {replicate, false}, ?LOC_NS => {local, false}}
    ),
    application:set_env(imboy, cache_replication, #{whitelist => [?REP_NS, ?LOC_NS]}),
    application:set_env(imboy, cache_fenced_namespaces, [?REP_NS, ?LOC_NS]),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(syn),
    ok = syn:add_node_to_scopes([imboy_cache]),
    %% 最小组件（不启动 imboy app：不连 PG、不绑 HTTP/WS 端口）；
    %% 独立无监督进程（测试专用；unlink 使 eval 进程退出不连坐）
    {ok, Dp} = depcache:start_link(imboy_cache, #{memory_max => 64}),
    true = unlink(Dp),
    case whereis(elib_metric) of
        undefined ->
            {ok, M} = elib_metric:start_link(),
            true = unlink(M);
        _ ->
            ok
    end,
    {ok, S} = imboy_cache_sync:start_link(),
    true = unlink(S),
    %% 单节点（无 peer）discover 有界重试（3×500ms）后才 become_ready；
    %% publish_set fail-closed 要求 ready，不等待则 warmup 首写即
    %% {error, {not_ready, discovering}}
    wait_ready(100).

wait_ready(0) ->
    erlang:error({sync_not_ready, imboy_cache_sync:status()});
wait_ready(N) ->
    case imboy_cache_sync:status() of
        ready ->
            ok;
        _ ->
            timer:sleep(100),
            wait_ready(N - 1)
    end.

%% jsone/depcache/syn 的编译产物在 deps/<dep>/ebin（不在主 ebin），
%% 依据 imboy_cache beam 位置推导仓库根自动补路径
deps_on_path() ->
    case code:which(imboy_cache) of
        Path when is_list(Path) ->
            Root = filename:dirname(filename:dirname(filename:absname(Path))),
            [
                %% add_patha 对已在路径中的目录返回 true（首次 ok），都算成功
                _ = code:add_patha(filename:join([Root, deps, D, ebin]))
             || D <- [jsone, depcache, syn]
            ],
            ok;
        _ ->
            erlang:error(imboy_ebin_not_on_path)
    end.

%% --- 输出（C14 证据 schema 对齐 dcheck：task_id + candidate 绑定 + UTC）---

result_json(Rows, Operations) ->
    Candidate =
        case os:getenv("CANDIDATE_SHA256") of
            false -> <<"null">>;
            Sha when length(Sha) =:= 64 -> unicode:characters_to_binary(["\"", Sha, "\""]);
            Bad -> erlang:error({malformed_candidate_sha256, Bad})
        end,
    Env = #{
        %% otp_release 是 char list，jsone 会把 int list 编成 JSON 数组——转 binary
        otp => unicode:characters_to_binary(erlang:system_info(otp_release)),
        schedulers => erlang:system_info(schedulers_online)
    },
    Payload = #{
        task_id => <<"CACHE_V221_BACKEND_CLOSEOUT_V1">>,
        candidate_sha256 => Candidate,
        profile => <<"cache-v2-single-node-loopback">>,
        operations_per_kind => Operations,
        utc => unicode:characters_to_binary(
            calendar:system_time_to_rfc3339(
                erlang:system_time(millisecond),
                [{unit, millisecond}, {offset, "Z"}]
            )
        ),
        env => Env,
        rows => Rows
    },
    unicode:characters_to_binary(jsone:encode(Payload)).
