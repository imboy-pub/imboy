%%% @doc TSID V10 W2 时间算法专项测试：双候选正确性 / 异常行为 / 漂移 / 性能。
%%%
%%% 运行：IMBOYENV=local make eunit t=tsid_w2_proto
%%% 计划 §W2 十场景全覆盖（场景 10 重启语义以契约测试形式验证）。
%%% 性能数据（吞吐/p 分位）输出到 stdout，供选型报告引用。
-module(tsid_w2_proto_tests).
-include_lib("eunit/include/eunit.hrl").

%% 2025-10-09 附近，安全在纪元后
-define(T0, 1760000000000).
-define(EPOCH_MS, 1735689600000).

both_candidates_test_() ->
    [
        {"candA fixed_offset: " ++ N, fun() -> F(fixed_offset) end}
     || {N, F} <- scenario_funs()
    ] ++
        [
            {"candB system: " ++ N, fun() -> F(system) end}
         || {N, F} <- scenario_funs()
        ].

scenario_funs() ->
    [
        {"S1 正常时间递增时间戳正确", fun s1_ts_follows_clock/1},
        {"S2 同毫秒持续发号序列唯一", fun s2_same_ms_seq_unique/1},
        {"S3 2048 序列用尽后等待下一毫秒", fun s3_exhaustion_advance/1},
        {"S3b 冻结时钟下序列用尽有界报错", fun s3b_exhaustion_frozen_clock/1},
        {"S4 回拨100ms不重复", fun s4_rollback_100ms/1},
        {"S5 回拨1s不重复或拒绝", fun s5_rollback_1s/1},
        {"S6 回拨10s耗尽后有界失败不无限等待", fun s6_rollback_10s/1},
        {"S7 前跳10s再回拨运行期不重复", fun s7_jump_then_rollback/1},
        {"S9 同BEAM高并发CAS唯一", fun s9_concurrent/1}
    ].

%% ------------------------------------------------------------------
%% 场景实现（每个场景开头清 seam，保证顺序无关）
%% ------------------------------------------------------------------

clear_seams() ->
    erase({tsid_w2_proto, mono}),
    erase({tsid_w2_proto, sys}),
    ok.

s1_ts_follows_clock(Cand) ->
    clear_seams(),
    Cfg = tsid_w2_proto:new_clock(Cand),
    Cursor = atomics:new(1, [{signed, true}]),
    set_virtual_clock(Cfg, ?T0),
    {ok, Id1} = tsid_w2_proto:reserve(Cursor, Cfg),
    set_virtual_clock(Cfg, ?T0 + 5),
    {ok, Id2} = tsid_w2_proto:reserve(Cursor, Cfg),
    set_virtual_clock(Cfg, ?T0 + 10),
    {ok, Id3} = tsid_w2_proto:reserve(Cursor, Cfg),
    ?assertEqual(?T0, ts_of(Id1)),
    ?assertEqual(?T0 + 5, ts_of(Id2)),
    ?assertEqual(?T0 + 10, ts_of(Id3)).

s2_same_ms_seq_unique(Cand) ->
    clear_seams(),
    Cfg = tsid_w2_proto:new_clock(Cand),
    Cursor = atomics:new(1, [{signed, true}]),
    set_virtual_clock(Cfg, ?T0),
    Ids = [I || {ok, I} <- gen(Cursor, Cfg, 100)],
    ?assertEqual(100, length(Ids)),
    ?assertEqual(100, length(lists:usort(Ids))),
    %% 同毫秒内序列 0..99
    ?assertEqual(lists:seq(0, 99), lists:sort([I band 2047 || I <- Ids])).

s3_exhaustion_advance(Cand) ->
    clear_seams(),
    %% 冻结时钟发满 2048；时钟推进 1ms 后第 2049 号落在下一毫秒（等待/推进语义）
    Cfg = tsid_w2_proto:new_clock(Cand),
    Cursor = atomics:new(1, [{signed, true}]),
    set_virtual_clock(Cfg, ?T0),
    Ids = [I || {ok, I} <- gen(Cursor, Cfg, 2048)],
    ?assertEqual(2048, length(lists:usort(Ids))),
    ?assertEqual([?T0], lists:usort([ts_of(I) || I <- Ids])),
    set_virtual_clock(Cfg, ?T0 + 1),
    {ok, Id2049} = tsid_w2_proto:reserve(Cursor, Cfg),
    ?assertEqual(?T0 + 1, ts_of(Id2049)).

s3b_exhaustion_frozen_clock(Cand) ->
    clear_seams(),
    %% 时钟完全冻结：2048 用尽后必须有界报错（默认 100ms 超时），绝不借未来毫秒
    Cfg = tsid_w2_proto:new_clock(Cand),
    Cursor = atomics:new(1, [{signed, true}]),
    set_virtual_clock(Cfg, ?T0),
    _ = gen(Cursor, Cfg, 2048),
    Started = erlang:monotonic_time(millisecond),
    ?assertMatch(
        {error, {capacity_exhausted, #{lead_ms := 1}}},
        tsid_w2_proto:reserve(Cursor, Cfg)
    ),
    Elapsed = erlang:monotonic_time(millisecond) - Started,
    ?assert(Elapsed >= 90 andalso Elapsed =< 500, {bounded_wait_ms, Elapsed}).

s4_rollback_100ms(Cand) ->
    clear_seams(),
    Cfg = tsid_w2_proto:new_clock(Cand),
    Cursor = atomics:new(1, [{signed, true}]),
    set_virtual_clock(Cfg, ?T0),
    Ids1 = [I || {ok, I} <- gen(Cursor, Cfg, 50)],
    rollback_system(Cfg, 100),
    Ids2 = [I || {ok, I} <- gen(Cursor, Cfg, 50)],
    ?assertEqual(100, length(lists:usort(Ids1 ++ Ids2))),
    ?assertEqual(?T0, ts_of(hd(Ids1))),
    ?assert(ts_of(last(Ids2)) >= ?T0 - 1).

s5_rollback_1s(Cand) ->
    clear_seams(),
    Cfg = tsid_w2_proto:new_clock(Cand),
    Cursor = atomics:new(1, [{signed, true}]),
    set_virtual_clock(Cfg, ?T0),
    Ids1 = [I || {ok, I} <- gen(Cursor, Cfg, 10)],
    rollback_system(Cfg, 1000),
    Ids2 = [I || {ok, I} <- gen(Cursor, Cfg, 10)],
    ?assertEqual(20, length(lists:usort(Ids1 ++ Ids2))).

s6_rollback_10s(Cand) ->
    clear_seams(),
    %% 回拨 10s 并耗尽游标当前毫秒：
    %%  - candB：Now 直读系统钟被拉低 10s，耗尽后有界 typed error（lead=10001）；
    %%  - candA：真实 mono 时钟驱动（不冻结），系统回拨不可见——发号继续成功。
    Cfg = tsid_w2_proto:new_clock(Cand),
    Cursor = atomics:new(1, [{signed, true}]),
    case Cand of
        system ->
            set_virtual_clock(Cfg, ?T0),
            _ = gen(Cursor, Cfg, 2048),
            rollback_system(Cfg, 10000),
            Started = erlang:monotonic_time(millisecond),
            Result = tsid_w2_proto:reserve(Cursor, Cfg),
            Elapsed = erlang:monotonic_time(millisecond) - Started,
            ?assertMatch({error, {capacity_exhausted, #{lead_ms := 10001}}}, Result),
            ?assert(Elapsed =< 1000, {not_bounded, Elapsed});
        fixed_offset ->
            %% 不冻结 mono（真实推进）；仅回拨系统钟 seam（candA 不读它）
            rollback_system(Cfg, 10000),
            _ = gen(Cursor, Cfg, 2048),
            Started = erlang:monotonic_time(millisecond),
            Result = tsid_w2_proto:reserve(Cursor, Cfg),
            Elapsed = erlang:monotonic_time(millisecond) - Started,
            ?assertMatch({ok, _}, Result),
            ?assert(Elapsed =< 1000, {too_slow, Elapsed})
    end.

s7_jump_then_rollback(Cand) ->
    clear_seams(),
    Cfg = tsid_w2_proto:new_clock(Cand),
    Cursor = atomics:new(1, [{signed, true}]),
    set_virtual_clock(Cfg, ?T0),
    Ids1 = [I || {ok, I} <- gen(Cursor, Cfg, 20)],
    set_virtual_clock(Cfg, ?T0 + 10000),
    Ids2 = [I || {ok, I} <- gen(Cursor, Cfg, 20)],
    rollback_system(Cfg, 10000),
    Ids3 = [I || {ok, I} <- gen(Cursor, Cfg, 20)],
    ?assertEqual(60, length(lists:usort(Ids1 ++ Ids2 ++ Ids3))).

s9_concurrent(Cand) ->
    clear_seams(),
    Cfg = tsid_w2_proto:new_clock(Cand),
    Cursor = atomics:new(1, [{signed, true}]),
    Procs = 8,
    Per = 5000,
    Parent = self(),
    Pids = [
        spawn_link(fun() -> Parent ! {self(), [I || {ok, I} <- gen(Cursor, Cfg, Per)]} end)
     || _ <- lists:seq(1, Procs)
    ],
    All = lists:append([
        receive
            {P, L} -> L
        after 60000 ->
            exit(concurrent_test_timeout)
        end
     || P <- Pids
    ]),
    ?assertEqual(Procs * Per, length(All)),
    ?assertEqual(Procs * Per, length(lists:usort(All))).

%% ------------------------------------------------------------------
%% S8 漂移测量（3s 真实运行；输出数据，阈值宽断言）
%% ------------------------------------------------------------------

drift_measurement_test() ->
    clear_seams(),
    A = tsid_w2_proto:new_clock(fixed_offset),
    B = tsid_w2_proto:new_clock(system),
    D0a = tsid_w2_proto:clock_drift_ms(A),
    D0b = tsid_w2_proto:clock_drift_ms(B),
    timer:sleep(3000),
    D1a = tsid_w2_proto:clock_drift_ms(A),
    D1b = tsid_w2_proto:clock_drift_ms(B),
    DeltaA = D1a - D0a,
    DeltaB = D1b - D0b,
    io:format(
        "~n[S8 drift 3s] candA delta=~pms candB delta=~pms (start a=~p b=~p end a=~p b=~p)~n",
        [DeltaA, DeltaB, D0a, D0b, D1a, D1b]
    ),
    ?assert(abs(DeltaB) =< 5),
    ?assert(abs(DeltaA) =< 100).

%% ------------------------------------------------------------------
%% S10 重启语义（契约级）：新 cursor 从当前时钟起跳
%% ------------------------------------------------------------------

restart_semantics_test() ->
    clear_seams(),
    %% 重启 = 新 cursor + 重新采集时钟。时钟前行时新 ID 高于旧水位（不重复）；
    %% 时钟回拨跨越重启则数值重叠——无持久化的固有边界（risks.md R1）。
    Cfg = tsid_w2_proto:new_clock(system),
    C1 = atomics:new(1, [{signed, true}]),
    set_virtual_clock(Cfg, ?T0),
    {ok, IdA} = tsid_w2_proto:reserve(C1, Cfg),
    %% 前行重启
    C2 = atomics:new(1, [{signed, true}]),
    set_virtual_clock(Cfg, ?T0 + 50),
    {ok, IdB} = tsid_w2_proto:reserve(C2, Cfg),
    ?assert(IdB > IdA),
    %% 回拨重启：数值重叠——如实断言该边界（允许重复是文档化行为）
    C3 = atomics:new(1, [{signed, true}]),
    set_virtual_clock(Cfg, ?T0 - 1000),
    {ok, IdC} = tsid_w2_proto:reserve(C3, Cfg),
    ?assert(IdC < IdA).

%% ------------------------------------------------------------------
%% 性能对比：时钟读成本 + 全路径吞吐/分位（串行 + 并发）
%% ------------------------------------------------------------------

perf_test() ->
    clear_seams(),
    N = 200000,
    {MonoUs, _} = timer:tc(fun() -> loop_clock(fun erlang:monotonic_time/1, N) end),
    {SysUs, _} = timer:tc(fun() -> loop_clock(fun erlang:system_time/1, N) end),
    io:format(
        "~n[perf clock read x~p] monotonic=~p us (~.1f ns/op) system=~p us (~.1f ns/op)~n",
        [N, MonoUs, MonoUs * 1000 / N, SysUs, SysUs * 1000 / N]
    ),
    lists:foreach(
        fun(Cand) ->
            Cfg = tsid_w2_proto:new_clock(Cand),
            Cursor = atomics:new(1, [{signed, true}]),
            {SerUs, LatUs} = timer:tc(fun() -> serial(Cursor, Cfg, N) end),
            p_report(Cand, serial, N, SerUs, LatUs),
            %% 并发 8 路（总量 20 万）
            Per = 25000,
            Procs = 8,
            Parent = self(),
            {ConcUs, _} =
                timer:tc(fun() ->
                    Pids = [
                        spawn_link(fun() ->
                            _ = serial(Cursor, Cfg, Per),
                            Parent ! {self(), done}
                        end)
                     || _ <- lists:seq(1, Procs)
                    ],
                    [
                        receive
                            {P, done} -> ok
                        after 60000 -> timeout
                        end
                     || P <- Pids
                    ]
                end),
            Total = Procs * Per,
            io:format(
                "[perf ~p concurrent ~p procs] total=~p ids in ~p us -> ~p ops/s~n",
                [Cand, Procs, Total, ConcUs, round(Total / (ConcUs / 1000000))]
            )
        end,
        [fixed_offset, system]
    ).

p_report(Cand, Mode, N, Us, LatUs) ->
    Sorted = lists:sort(LatUs),
    P = fun(Q) -> lists:nth(max(1, trunc(Q * N)), Sorted) end,
    io:format(
        "[perf ~p ~p] n=~p total=~p us -> ~p ops/s | p50=~p us p95=~p us p99=~p us max=~p us~n",
        [Cand, Mode, N, Us, round(N / (Us / 1000000)), P(0.5), P(0.95), P(0.99), lists:last(Sorted)]
    ).

loop_clock(_F, 0) ->
    ok;
loop_clock(F, N) ->
    _ = F(millisecond),
    loop_clock(F, N - 1).

serial(_Cursor, _Cfg, 0) ->
    [];
serial(Cursor, Cfg, N) ->
    T0 = erlang:monotonic_time(microsecond),
    {ok, _} = tsid_w2_proto:reserve(Cursor, Cfg),
    T1 = erlang:monotonic_time(microsecond),
    [T1 - T0 | serial(Cursor, Cfg, N - 1)].

%% ------------------------------------------------------------------
%% 时钟注入辅助（candidate 感知）
%% ------------------------------------------------------------------

%% 把候选时钟的“虚拟墙钟”设为绝对毫秒
set_virtual_clock(#{type := fixed_offset, offset_ms := O}, Ms) ->
    put({tsid_w2_proto, mono}, Ms - O);
set_virtual_clock(#{type := system}, Ms) ->
    put({tsid_w2_proto, sys}, Ms).

%% 模拟系统时钟回拨：candB 生效（sys 直读）；candA 仅动 sys seam
%% （mono 不动——系统回拨对 candA 不可见，这正是候选 A 的语义）
rollback_system(_Cfg, Ms) ->
    case get({tsid_w2_proto, sys}) of
        undefined ->
            put(
                {tsid_w2_proto, sys},
                erlang:system_time(millisecond) - Ms
            );
        V ->
            put({tsid_w2_proto, sys}, V - Ms)
    end,
    %% candA 场景：同时把 mono seam 从“未设”切到冻结虚拟值会导致语义漂移；
    %% 测试里 candA 的 set_virtual_clock 已设 mono seam（相对偏移稳定），
    %% 回拨不应改动 mono——保持不动。
    ok.

gen(_Cursor, _Cfg, 0) ->
    [];
gen(Cursor, Cfg, N) ->
    case tsid_w2_proto:reserve(Cursor, Cfg) of
        {ok, I} -> [{ok, I} | gen(Cursor, Cfg, N - 1)];
        E = {error, _} -> [E]
    end.

ts_of(Id) ->
    (Id bsr 21) + ?EPOCH_MS.

last([X]) ->
    X;
last([_ | R]) ->
    last(R).
