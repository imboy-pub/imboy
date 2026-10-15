#!/usr/bin/env escript
%%! -noshell -noinput +A 8 -pa /Users/leeyi/project/imboy.pub/.worktrees/limiter-v1-a4/ebin
%%%
%%% Limiter V1 AC-16 基线 B：旧代码（0d28ad4f）同有效记账工作量微基准。
%%% 真实旧 login_attempt_ds（gen_server+ETS+imboy_cache/depcache）+ 真实
%%% throttle（passport_per_ip scope）。JSON 输出 stdout。
%%%
%%% 冻结输入（计划 §7.6 / C.9，详见输出 env 段）：
%%%   - 事件规模 100k/场景；warmup 10k；每场景 3 次独立重复
%%%   - 账户键 10000 个（16B "acct_bench_NNNNNN" 均匀轮转）
%%%   - IP 键 1000 个（"10.0.XX.YYY" 9-10B 均匀轮转；check_ip 每 IP ≤100
%%%     次 = 默认 login_max_ip_attempts_per_hour 100 内 → 全部事件有效记账）
%%%   - throttle scope passport_per_ip {10, per_minute}（生产默认）
%%%   - 并发：1 worker 与 8 worker（8×12.5k，spawn_monitor+ref 收齐，不 sleep）
%%%   - erlang:monotonic_time(nanosecond) 每事件计时；调用者进程 reductions
%%%     差值（含每事件计时开销，各场景一致口径）；ets:all() 总 memory 差值
%%%
%%% 混合负载：80% check_ip_rate_limit + 15% record_failure + 5% reset
%%% （reset 仅作用于本轮 record 过的键 → 全部有效删除）。

-define(N_EVENTS, 100000).
-define(N_ACCOUNT_KEYS, 10000).
-define(N_IP_KEYS, 1000).
-define(N_WARMUP, 10000).
-define(N_REPEATS, 3).
-define(N_WORKERS, 8).

main(_) ->
    OldRoot = "/Users/leeyi/project/imboy.pub/.worktrees/limiter-v1-a4",
    [code:add_patha(P) || P <- filelib:wildcard(OldRoot ++ "/deps/*/ebin")],
    {ok, _} = imboy_cache:start_link([]),
    {ok, _} = login_attempt_ds:start_link(),
    {ok, _} = application:ensure_all_started(throttle),
    ok = throttle:setup(passport_per_ip, 10, per_minute),

    Env = #{
        otp => otp_release_s(),
        erts => erlang:system_info(version),
        schedulers_online => erlang:system_info(schedulers_online),
        os => "macos",
        os_version => os_version(),
        old_head => old_head(OldRoot),
        account_key_bytes => 16,
        ip_key_bytes => "9-10",
        n_events => ?N_EVENTS,
        n_account_keys => ?N_ACCOUNT_KEYS,
        n_ip_keys => ?N_IP_KEYS,
        warmup => ?N_WARMUP,
        repeats => ?N_REPEATS,
        throttle_scope => "passport_per_ip {10, per_minute}",
        mixed_ratio => "80 check_ip / 15 record_failure / 5 reset"
    },

    warmup(),

    R1 = [scenario(op_record_failure) || _ <- lists:seq(1, ?N_REPEATS)],
    R2 = [scenario(op_check_ip) || _ <- lists:seq(1, ?N_REPEATS)],
    R3 = [scenario(op_throttle) || _ <- lists:seq(1, ?N_REPEATS)],
    R4 = [scenario(op_reset) || _ <- lists:seq(1, ?N_REPEATS)],
    R5 = [scenario({op_mixed, 1}) || _ <- lists:seq(1, ?N_REPEATS)],
    R6 = [scenario({op_mixed, ?N_WORKERS}) || _ <- lists:seq(1, ?N_REPEATS)],

    Out = #{
        env => Env,
        record_failure => summarize_runs(R1),
        check_ip_rate_limit => summarize_runs(R2),
        throttle_check => summarize_runs(R3),
        reset => summarize_runs(R4),
        mixed_1w => summarize_runs(R5),
        mixed_8w => summarize_runs(R6)
    },
    io:format("~s~n", [json_encode(Out)]),
    halt(0).

%% -------------------------------------------------------------------------
%% 场景驱动
%% -------------------------------------------------------------------------

scenario(Op) ->
    reset_state(),
    Owner = self(),
    {Pid, Mref} = spawn_monitor(fun() -> Owner ! {self(), run_scenario(Op)} end),
    Res =
        receive
            {Pid, R} -> erlang:demonitor(Mref, [flush]), R;
            {'DOWN', Mref, process, _, Reason} -> erlang:error({worker_down, Op, Reason})
        after 480000 -> erlang:error({worker_timeout, Op})
        end,
    Res#{
        op => op_name(Op),
        workers => op_workers_of(Op),
        ds_mailbox_after =>
            case whereis(login_attempt_ds) of
                undefined -> undefined;
                DPid ->
                    {message_queue_len, L} = process_info(DPid, message_queue_len),
                    L
            end
    }.

op_name(op_record_failure) -> record_failure;
op_name(op_check_ip) -> check_ip_rate_limit;
op_name(op_throttle) -> throttle_check;
op_name(op_reset) -> reset;
op_name({op_mixed, _}) -> mixed.

op_workers_of({op_mixed, W}) -> W;
op_workers_of(_) -> 1.

run_scenario(op_record_failure) ->
    AcctTuple = list_to_tuple(account_keys()),
    N = ?N_ACCOUNT_KEYS,
    bench_events(
        lists:seq(1, 10 * N),
        fun(I) ->
            login_attempt_ds:record_failure(element(((I - 1) rem N) + 1, AcctTuple), hot_ip())
        end,
        fun
            ({ok, _}) -> ok;
            (_) -> denied
        end
    );
run_scenario(op_check_ip) ->
    %% 全键均匀轮转（100k/1000 = 每 IP 恰 100 次，默认 100/h 限内 → 全有效）
    IpTuple = list_to_tuple(ip_keys_ordered()),
    NIp = tuple_size(IpTuple),
    bench_events(
        lists:seq(1, ?N_EVENTS),
        fun(I) ->
            login_attempt_ds:check_ip_rate_limit(element(((I - 1) rem NIp) + 1, IpTuple))
        end,
        fun
            ({ok, _}) -> ok;
            (_) -> denied
        end
    );
run_scenario(op_throttle) ->
    %% 全键均匀轮转（1000 IP × 100 次；阈值 10/min → 每 IP 前 10 次允许）
    IpTuple = list_to_tuple(ip_keys_ordered()),
    NIp = tuple_size(IpTuple),
    bench_events(
        lists:seq(1, ?N_EVENTS),
        fun(I) ->
            throttle:check(passport_per_ip, element(((I - 1) rem NIp) + 1, IpTuple))
        end,
        fun
            ({ok, _, _}) -> ok;
            (_) -> denied
        end
    );
run_scenario(op_reset) ->
    %% 专用骨架：每轮填充 10k 键（不计时），逐键计时 reset → 100k 全部
    %% 有效删除。三轮结构：fill+reset × 10。
    AcctTuple = list_to_tuple(account_keys()),
    N = ?N_ACCOUNT_KEYS,
    {_, Red0} = worker_reductions(),
    MemBefore = total_ets_memory(),
    {T, OkC, BadC} =
        lists:foldl(
            fun(_Round, {Acc, O, B}) ->
                fill_tuple_round(AcctTuple, N, 1),
                reset_tuple_timed(AcctTuple, N, 1, Acc, O, B)
            end,
            {[], 0, 0},
            lists:seq(1, 10)
        ),
    {_, Red1} = worker_reductions(),
    MemAfter = total_ets_memory(),
    #{
        times_ns => lists:reverse(T),
        wall_ns => lists:sum(T),
        reductions_delta => Red1 - Red0,
        allowed => OkC,
        denied => BadC,
        mem_before => MemBefore,
        mem_after => MemAfter
    };
run_scenario({op_mixed, Workers}) ->
    PerW = ?N_EVENTS div Workers,
    Owner = self(),
    Refs =
        [
            begin
                WorkerId = W,
                {Pid, Mref} =
                    spawn_monitor(fun() -> Owner ! {self(), run_mixed_worker(WorkerId, PerW, Workers)} end),
                {Pid, Mref}
            end
         || W <- lists:seq(1, Workers)
        ],
    T0 = erlang:monotonic_time(nanosecond),
    All =
        lists:map(
            fun({Pid, Mref}) ->
                receive
                    {Pid, R} -> erlang:demonitor(Mref, [flush]), R;
                    {'DOWN', Mref, process, _, Reason} -> erlang:error({mixed_worker_down, Reason})
                after 480000 -> erlang:error(mixed_worker_timeout)
                end
            end,
            Refs
        ),
    T1 = erlang:monotonic_time(nanosecond),
    Times = lists:append([maps:get(times_ns, R) || R <- All]),
    Red = lists:sum([maps:get(reductions_delta, R) || R <- All]),
    Allowed = lists:sum([maps:get(allowed, R) || R <- All]),
    Denied = lists:sum([maps:get(denied, R) || R <- All]),
    MemBefore = lists:min([maps:get(mem_before, R) || R <- All]),
    MemAfter = lists:max([maps:get(mem_after, R) || R <- All]),
    #{
        times_ns => Times,
        wall_ns => T1 - T0,
        reductions_delta => Red,
        allowed => Allowed,
        denied => Denied,
        mem_before => MemBefore,
        mem_after => MemAfter
    }.

%% 通用逐事件计时骨架：返回场景结果 map
bench_events(Idxs, Op, Verdict) ->
    {_, Red0} = worker_reductions(),
    MemBefore = total_ets_memory(),
    {T, OkC, BadC} =
        lists:foldl(
            fun(_I, {Acc, O, B}) ->
                T0 = erlang:monotonic_time(nanosecond),
                R = Op(_I),
                T1 = erlang:monotonic_time(nanosecond),
                case Verdict(R) of
                    ok -> {[T1 - T0 | Acc], O + 1, B};
                    _ -> {[T1 - T0 | Acc], O, B + 1}
                end
            end,
            {[], 0, 0},
            Idxs
        ),
    {_, Red1} = worker_reductions(),
    MemAfter = total_ets_memory(),
    #{
        times_ns => lists:reverse(T),
        wall_ns => lists:sum(T),
        reductions_delta => Red1 - Red0,
        allowed => OkC,
        denied => BadC,
        mem_before => MemBefore,
        mem_after => MemAfter
    }.

%% 单个混合 worker：80% check_ip / 15% record / 5% reset，逐事件计时
run_mixed_worker(WorkerId, Total, Workers) ->
    NCheck = Total * 80 div 100,
    NRecord = Total * 15 div 100,
    NReset = Total * 5 div 100,
    IpTuple = list_to_tuple(slice_for(ip_keys_ordered(), WorkerId, Workers)),
    AcctTuple = list_to_tuple(slice_for(account_keys(), WorkerId, Workers)),
    NIp = tuple_size(IpTuple),
    NAcct = tuple_size(AcctTuple),
    {_, Red0} = worker_reductions(),
    MemBefore = total_ets_memory(),
    T0 = erlang:monotonic_time(nanosecond),
    {Times1, OkC, BadC, Recorded} = phase_check(NCheck, IpTuple, NIp),
    {Times2, OkR, BadR, Recorded2} = phase_record(NRecord, AcctTuple, NAcct, Recorded),
    RecTuple = list_to_tuple(lists:reverse(Recorded2)),
    NRec = tuple_size(RecTuple),
    {Times3, OkX, BadX} = phase_reset(NReset, RecTuple, NRec),
    T1 = erlang:monotonic_time(nanosecond),
    {_, Red1} = worker_reductions(),
    MemAfter = total_ets_memory(),
    #{
        times_ns => lists:reverse(Times3 ++ Times2 ++ Times1),
        wall_ns => T1 - T0,
        reductions_delta => Red1 - Red0,
        allowed => OkC + OkR + OkX,
        denied => BadC + BadR + BadX,
        mem_before => MemBefore,
        mem_after => MemAfter
    }.

phase_check(0, _Tuple, _N) -> {[], 0, 0, []};
phase_check(N, Tuple, NAcc) -> phase_check(N, Tuple, NAcc, [], 0, 0).
phase_check(0, _Tuple, _N, Acc, O, B) -> {Acc, O, B, []};
phase_check(I, Tuple, N, Acc, O, B) ->
    T0 = erlang:monotonic_time(nanosecond),
    R = login_attempt_ds:check_ip_rate_limit(element(((I - 1) rem N) + 1, Tuple)),
    T1 = erlang:monotonic_time(nanosecond),
    case R of
        {ok, _} -> phase_check(I - 1, Tuple, N, [T1 - T0 | Acc], O + 1, B);
        _ -> phase_check(I - 1, Tuple, N, [T1 - T0 | Acc], O, B + 1)
    end.

phase_record(0, _Tuple, _N, _Recorded0) -> {[], 0, 0, []};
phase_record(N, Tuple, NAcc, Recorded0) -> phase_record(N, Tuple, NAcc, Recorded0, [], 0, 0).
phase_record(0, _Tuple, _N, Recorded0, Acc, O, B) -> {Acc, O, B, Recorded0};
phase_record(I, Tuple, N, Recorded0, Acc, O, B) ->
    K = element(((I - 1) rem N) + 1, Tuple),
    T0 = erlang:monotonic_time(nanosecond),
    R = login_attempt_ds:record_failure(K, hot_ip()),
    T1 = erlang:monotonic_time(nanosecond),
    case R of
        {ok, _} -> phase_record(I - 1, Tuple, N, [K | Recorded0], [T1 - T0 | Acc], O + 1, B);
        _ -> phase_record(I - 1, Tuple, N, Recorded0, [T1 - T0 | Acc], O, B + 1)
    end.

phase_reset(0, _RecTuple, _NRec) -> {[], 0, 0};
phase_reset(N, RecTuple, NRec) -> phase_reset(N, RecTuple, NRec, [], 0, 0).
phase_reset(0, _RecTuple, _NRec, Acc, O, B) -> {Acc, O, B};
phase_reset(I, RecTuple, NRec, Acc, O, B) ->
    K = element(((I - 1) rem NRec) + 1, RecTuple),
    T0 = erlang:monotonic_time(nanosecond),
    ok = login_attempt_ds:reset(K, hot_ip()),
    T1 = erlang:monotonic_time(nanosecond),
    phase_reset(I - 1, RecTuple, NRec, [T1 - T0 | Acc], O + 1, B).

%% -------------------------------------------------------------------------
%% 键空间
%% -------------------------------------------------------------------------

account_keys() ->
    [pad_account(I) || I <- lists:seq(1, ?N_ACCOUNT_KEYS)].

pad_account(I) ->
    N = integer_to_list(I),
    Padded = string:right(N, 6, $0),
    list_to_binary("acct_bench_" ++ Padded).

ip_keys_ordered() ->
    [ip_key(I) || I <- lists:seq(1, ?N_IP_KEYS)].

ip_key(I) ->
    B = 10 + I div 200,
    D = 1 + I rem 200,
    list_to_binary("10.0." ++ integer_to_list(B) ++ "." ++ integer_to_list(D)).

hot_ip() -> <<"10.0.99.99">>.

slice_for(Keys, WorkerId, Workers) ->
    Len = length(Keys) div Workers,
    KeysList = Keys,
    Start = (WorkerId - 1) * Len + 1,
    End =
        case WorkerId =:= Workers of
            true -> length(KeysList);
            false -> WorkerId * Len
        end,
    [lists:nth(I, KeysList) || I <- lists:seq(Start, End)].

%% -------------------------------------------------------------------------
%% 状态复位 / 采集
%% -------------------------------------------------------------------------

reset_state() ->
    ets:delete_all_objects(login_attempt_ets),
    try imboy_cache:flush() catch _:_ -> ok end,
    try ets:delete_all_objects(scope_counters) catch _:_ -> ok end,
    ok.

%% 每轮填充：tuple 索引逐键 record（不计时，由调用方置于计时段外）
fill_tuple_round(_Tuple, _N, I) when I > _N -> ok;
fill_tuple_round(Tuple, N, I) ->
    login_attempt_ds:record_failure(element(I, Tuple), hot_ip()),
    fill_tuple_round(Tuple, N, I + 1).

%% 每轮计时 reset：返回累积 {[Times], Ok, Bad}
reset_tuple_timed(_Tuple, N, I, Acc, O, B) when I > N ->
    {Acc, O, B};
reset_tuple_timed(Tuple, N, I, Acc, O, B) ->
    T0 = erlang:monotonic_time(nanosecond),
    ok = login_attempt_ds:reset(element(I, Tuple), hot_ip()),
    T1 = erlang:monotonic_time(nanosecond),
    reset_tuple_timed(Tuple, N, I + 1, [T1 - T0 | Acc], O + 1, B).

worker_reductions() ->
    {reductions, R} = process_info(self(), reductions),
    {ok, R}.

total_ets_memory() ->
    lists:sum([ets:info(T, memory) || T <- ets:all(), is_atom(T)]).

os_version() ->
    os:cmd("sw_vers -productVersion") -- "\n".

otp_release_s() ->
    case erlang:system_info(otp_release) of
        B when is_binary(B) -> binary_to_list(B);
        L when is_list(L) -> L
    end.

old_head(OldRoot) ->
    Out = os:cmd("git -C " ++ OldRoot ++ " rev-parse --short HEAD"),
    string:trim(Out).

warmup() ->
    reset_state(),
    Keys = account_keys(),
    [login_attempt_ds:record_failure(lists:nth(((I - 1) rem ?N_ACCOUNT_KEYS) + 1, Keys), hot_ip())
     || I <- lists:seq(1, ?N_WARMUP)],
    [login_attempt_ds:check_ip_rate_limit(Ip) || Ip <- ip_keys_ordered()],
    [throttle:check(passport_per_ip, Ip) || Ip <- ip_keys_ordered()],
    reset_state(),
    ok.

%% -------------------------------------------------------------------------
%% 汇总 / JSON
%% -------------------------------------------------------------------------

summarize_runs(Runs) ->
    [Single | _] = Runs,
    PerRun = [summarize_one(R) || R <- Runs],
    #{
        op => maps:get(op, Single, mixed),
        workers => op_workers(Single),
        ds_mailbox_after_each_run => [maps:get(ds_mailbox_after, R, null) || R <- Runs],
        runs => PerRun,
        aggregate => aggregate(PerRun)
    }.

op_workers(R) -> maps:get(workers, R, 1).

summarize_one(R) ->
    Times = maps:get(times_ns, R),
    N = length(Times),
    Sorted = lists:sort(Times),
    TotalNs = lists:sum(Sorted),
    #{
        events => N,
        p50_us => ns_to_us(percentile(Sorted, 0.50)),
        p95_us => ns_to_us(percentile(Sorted, 0.95)),
        p99_us => ns_to_us(percentile(Sorted, 0.99)),
        max_us => ns_to_us(lists:last(Sorted)),
        mean_us => ns_to_us(TotalNs div max(1, N)),
        throughput_eps => round(N / (TotalNs / 1.0e9)),
        reductions_per_event =>
            case maps:find(reductions_delta, R) of
                {ok, Rd} -> round(Rd / max(1, N));
                error -> null
            end,
        allowed => maps:get(allowed, R),
        denied => maps:get(denied, R),
        ets_mem_words_before => maps:get(mem_before, R),
        ets_mem_words_after => maps:get(mem_after, R),
        wall_ms => maps:get(wall_ns, R, 0) div 1000000
    }.

aggregate(PerRun) ->
    Ps = [maps:get(p95_us, R) || R <- PerRun],
    P99s = [maps:get(p99_us, R) || R <- PerRun],
    Mxs = [maps:get(max_us, R) || R <- PerRun],
    Ths = [maps:get(throughput_eps, R) || R <- PerRun],
    #{
        p95_us_range => [lists:min(Ps), lists:max(Ps)],
        p99_us_range => [lists:min(P99s), lists:max(P99s)],
        max_us_range => [lists:min(Mxs), lists:max(Mxs)],
        throughput_eps_range => [lists:min(Ths), lists:max(Ths)]
    }.

percentile(Sorted, Q) ->
    N = length(Sorted),
    Idx = max(1, round(Q * N)),
    lists:nth(Idx, Sorted).

ns_to_us(Ns) when is_integer(Ns) -> round(Ns / 100) / 10;
ns_to_us(X) -> X.

%% 最小 JSON 编码器（键为 atom/binary/string，值为标量/map/list）
json_encode(Term) -> iolist_to_binary(json(Term)).

json(V) when is_map(V) ->
    Inner = lists:join(
        <<",">>,
        [
            [$", key_bin(K), $", $:] ++ json(Val)
         || {K, Val} <- maps:to_list(V)
        ]
    ),
    [${, Inner, $}];
json(V) when is_list(V) ->
    case is_printable_list(V) of
        true -> [$", V, $"];
        false ->
            Inner = lists:join(<<",">>, [json(E) || E <- V]),
            [$[, Inner, $]]
    end;
json(V) when is_binary(V) -> [$", esc(V), $"];
json(V) when is_atom(V) -> [$", atom_to_binary(V, utf8), $"];
json(V) when is_integer(V) -> integer_to_binary(V);
json(V) when is_float(V) -> float_to_binary(V, [{decimals, 2}, compact]);
json(null) -> <<"null">>;
json(V) -> list_to_binary(io_lib:format("~p", [V])).

key_bin(K) when is_atom(K) -> atom_to_binary(K, utf8);
key_bin(K) when is_binary(K) -> K;
key_bin(K) when is_list(K) -> list_to_binary(K);
key_bin(K) -> list_to_binary(io_lib:format("~p", [K])).

is_printable_list([C | Rest]) when is_integer(C), C >= 32, C =< 126 -> is_printable_list(Rest);
is_printable_list([]) -> true;
is_printable_list(_) -> false.

esc(B) ->
    binary:replace(
        binary:replace(B, <<"\\">>, <<"\\\\">>, [global]), <<"\"">>, <<"\\\"">>, [global]
    ).
