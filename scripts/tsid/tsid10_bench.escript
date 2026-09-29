#!/usr/bin/env escript
%% TSID-10 candidate 基准：与 TSID-00 基线同机器/同 OTP/同口径对比。
%% 额外采集：CAS reservation 计数（T-308 CAS 次数=chunk 数）、fence renew
%% 计数（热路径无磁盘 I/O 的旁证：renew 只发生在续租而非每次 generate）、
%% lead 峰值采样（AC-10D §5.6 不变量）。
-module(tsid10_bench).
-export([main/1]).

%% 用法: tsid10_bench.escript <out-raw-path>
%% 口径与 TSID-00 基线一致；段前 reset 隔离 lead 预算（见 parameter-decision.md）

-define(EPOCH_MS, 1735689600000).

main([OutPath]) ->
    init_fresh(),
    {ok, Dev} = file:open(OutPath, [write]),
    %% 预热
    _ = elib_tsid:generate_n(user, 10000),

    %% 单 ID 顺序：5 轮，每轮 200_000（与基线同口径；5 轮连续累计——
    %% 段内不 reset，测的是 1M-id 连续突发的真实表现）
    SeqRounds = [bench_seq(200000) || _ <- lists:seq(1, 5)],
    [io:format(Dev, "SEQ_ROUND ~p~n", [R]) || R <- SeqRounds],

    %% 并发：32 workers × 32_000（共 ~1M），3 轮（段前 reset：独立 lead 预算）
    init_fresh(),
    _ = elib_tsid:generate_n(user, 10000),
    ConcRounds = [bench_conc(32, 32000) || _ <- lists:seq(1, 3)],
    [io:format(Dev, "CONC_ROUND ~p~n", [R]) || R <- ConcRounds],

    %% batch：N=1000 与 N=10000，各 5 轮（T-308：CAS 计数；段前 reset）
    init_fresh(),
    _ = elib_tsid:generate_n(user, 10000),
    B1 = [bench_batch(1000) || _ <- lists:seq(1, 5)],
    [io:format(Dev, "BATCH_1K_ROUND ~p~n", [R]) || R <- B1],
    B2 = [bench_batch(10000) || _ <- lists:seq(1, 5)],
    [io:format(Dev, "BATCH_10K_ROUND ~p~n", [R]) || R <- B2],

    %% lead 峰值采样（AC-10D：§5.6 lead <= max_logical_lead_ms；段前 reset）
    init_fresh(),
    _ = elib_tsid:generate_n(user, 10000),
    LeadPeak = lead_peak_sample(200000),
    io:format(Dev, "LEAD_PEAK ~p~n", [LeadPeak]),

    ok = file:close(Dev),
    halt(0).

bench_seq(N) ->
    R0 = reservations(),
    T0 = erlang:monotonic_time(microsecond),
    loop_gen(N),
    T1 = erlang:monotonic_time(microsecond),
    R1 = reservations(),
    #{
        n => N,
        micros => T1 - T0,
        ids_per_sec => round(N * 1000000 / (T1 - T0)),
        reservations => R1 - R0
    }.

loop_gen(0) ->
    ok;
loop_gen(N) ->
    _ = elib_tsid:generate(user),
    loop_gen(N - 1).

bench_conc(Workers, PerWorker) ->
    Self = self(),
    R0 = reservations(),
    T0 = erlang:monotonic_time(microsecond),
    [
        spawn(fun() ->
            loop_gen(PerWorker),
            Self ! done
        end)
     || _ <- lists:seq(1, Workers)
    ],
    [
        receive
            done -> ok
        end
     || _ <- lists:seq(1, Workers)
    ],
    T1 = erlang:monotonic_time(microsecond),
    R1 = reservations(),
    Total = Workers * PerWorker,
    #{
        workers => Workers,
        per_worker => PerWorker,
        total => Total,
        micros => T1 - T0,
        ids_per_sec => round(Total * 1000000 / (T1 - T0)),
        reservations => R1 - R0
    }.

bench_batch(N) ->
    R0 = reservations(),
    T0 = erlang:monotonic_time(microsecond),
    Ids = elib_tsid:generate_n(user, N),
    T1 = erlang:monotonic_time(microsecond),
    R1 = reservations(),
    #{
        n => N,
        micros => T1 - T0,
        sorted_unique => (lists:usort(Ids) =:= Ids),
        reservations => R1 - R0,
        ids_per_sec => round(N * 1000000 / (T1 - T0))
    }.

reservations() ->
    elib_tsid:reservation_count().

%% lead 峰值：快速抽 generate 的 ts 与墙钟差的最大值
lead_peak_sample(N) ->
    Peak = loop_lead(N, 0),
    %% init_fresh/0 未传 max_logical_lead_ms → 用候选缺省（TSID-10 定标 512）
    #{n => N, peak_lead_ms => Peak, max_allowed => 512}.

loop_lead(0, Peak) ->
    Peak;
loop_lead(N, Peak) ->
    Id = elib_tsid:generate(user),
    Ts = (Id bsr 21) + ?EPOCH_MS,
    Lead = Ts - erlang:system_time(millisecond),
    P =
        case Lead > Peak of
            true -> Lead;
            false -> Peak
        end,
    loop_lead(N - 1, P).

%% 段间隔离：全新 init（各自独立 lead 预算，与基线无 lead 概念同起跑线）
init_fresh() ->
    ok = elib_tsid:reset_for_test(),
    ok = elib_tsid:init(#{
        dc_id => 1,
        node_id => 1,
        dc_bits => 3,
        names => [user],
        capacity_wait_timeout_ms => 5000
    }).
