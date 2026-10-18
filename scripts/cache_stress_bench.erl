%% Isolated, no-database cache benchmark. One profile per VM; use the identical
%% driver beam with baseline/candidate absolute ebins. Never pad CPU with spin.
%% CACHE_STRESS_{NODES,KEYS,BYTES,WORKERS,SECONDS}=1,10000,64,32,10
%% CACHE_STRESS_{MODE,WORKLOAD,CAPACITY}=local|replicate,read|write|mixed,enlarged|default
%% CACHE_STRESS_OUTPUT=/absolute/result.json; CACHE_STRESS_CPUS overrides the
%% affinity-derived CPU denominator (e.g. when an external quota is known).
-module(cache_stress_bench).
-export([run/0, boot/1, seed/2, verify/2, launch/3, snapshot/0]).

run() ->
    Output = os:getenv("CACHE_STRESS_OUTPUT", "/tmp/cache_stress.json"),
    try
        C = config(),
        distribution(),
        ok = boot(C),
        Peers = peers(maps:get(nodes, C) - 1, C, []),
        try
            Nodes = [node() | [N || {_, N} <- Peers]],
            connect(Nodes),
            Result = profile(C, Nodes),
            ok = file:write_file(Output, jsone:encode(Result)),
            put(stress_result_written, true),
            io:format("CACHE_STRESS ~s~n", [jsone:encode(Result)]),
            case maps:get(errors, Result) of 0 -> ok; _ -> error(measured_errors) end
        after
            [peer:stop(P) || {P, _} <- Peers]
        end,
        halt(0)
    catch Class:Reason:Stack ->
        io:format("CACHE_STRESS_FAILED ~p:~p ~p~n", [Class, Reason, Stack]),
        %% Preserve a measured row even if its errors cause nonzero exit.
        case get(stress_result_written) of
            true -> ok;
            _ -> file:write_file(Output, jsone:encode(#{error => fmt({Class, Reason})}))
        end,
        halt(2)
    end.

config() ->
    C = #{nodes => integer("NODES", 1), keys => integer("KEYS", 10000),
          bytes => integer("BYTES", 64), workers_per_node => integer("WORKERS", 32),
          seconds => integer("SECONDS", 10),
          mode => choice("MODE", "local", [local, replicate]),
          workload => choice("WORKLOAD", "mixed", [read, write, mixed]),
          capacity => choice("CAPACITY", "enlarged", [default, enlarged])},
    true = maps:get(nodes, C) >= 1 andalso maps:get(nodes, C) =< 3,
    true = lists:all(fun(K) -> maps:get(K, C) > 0 end,
                    [keys, bytes, workers_per_node, seconds]),
    true = maps:get(bytes, C) >= 8,
    C.
integer(Name, Default) ->
    list_to_integer(os:getenv("CACHE_STRESS_" ++ Name, integer_to_list(Default))).
choice(Name, Default, Allowed) ->
    V = list_to_existing_atom(os:getenv("CACHE_STRESS_" ++ Name, Default)),
    true = lists:member(V, Allowed), V.
fmt(T) -> iolist_to_binary(io_lib:format("~p", [T])).

%% Reuse the existing test fixture's namespace registration and real sync boot.
%% Only resource limits differ; production config is never edited.
boot(C) ->
    true = os:putenv("CACHE_V2_ARM", "cand"),
    Rep = case maps:get(capacity, C) of
        default -> #{};
        enlarged ->
            N = max(200000, maps:get(keys, C) * 2),
            #{n_max => N, n_max_bytes => max(268435456, N * (maps:get(bytes, C) + 512)),
              gen_max_bytes => N * 256}
    end,
    %% The old baseline has no production shared_cache entry; extend its test
    %% registry explicitly. Candidate must use its real registry entry.
    application:set_env(imboy, cache_registry_overrides, #{}),
    NeedsExtension = imboy_cache_policy:policy({shared_cache, probe}) =/= replicate,
    Overrides = case NeedsExtension of
        true -> #{shared_cache => {replicate, false}, cache_v2_bench_loc => {local, false}};
        false -> #{cache_v2_bench_loc => {local, false}}
    end,
    application:set_env(imboy, cache_stress_baseline_extension, NeedsExtension),
    cache_v2_bench:app_up(#{replication => Rep, memory_max => 1024,
        replication_namespace => shared_cache, registry_overrides => Overrides}).

distribution() ->
    case node() of
        nonode@nohost ->
            {ok, _} = net_kernel:start([list_to_atom("stress_" ++ os:getpid() ++ "@127.0.0.1"), longnames]);
        _ -> ok
    end.
peers(0, _, Acc) -> lists:reverse(Acc);
peers(Count, C, Acc) ->
    Paths = lists:append([["-pa", filename:absname(P)] || P <- lists:reverse(code:get_path())]),
    Schedulers = os:getenv("CACHE_STRESS_PEER_SCHEDULERS", "3"),
    Args = ["+S", Schedulers, "-setcookie", atom_to_list(erlang:get_cookie()),
            "-kernel", "inet_dist_use_interface", "{127,0,0,1}",
            "prevent_overlapping_partitions", "false"] ++ Paths,
    {ok, P, N} = peer:start(#{name => peer:random_name("stress"), host => "127.0.0.1",
                            connection => standard_io, args => Args}),
    try
        ok = peer:call(P, ?MODULE, boot, [C], 60000),
        peers(Count - 1, C, [{P, N} | Acc])
    catch Class:Reason:Stack ->
        peer:stop(P), erlang:raise(Class, Reason, Stack)
    end.
connect(Nodes) ->
    [rpc:call(A, net_kernel, connect_node, [B]) || A <- Nodes, B <- Nodes, A =/= B],
    await(fun() -> lists:all(fun(N) ->
        M = rpc:call(N, syn, members, [imboy_cache, dsync_handler]),
        is_list(M) andalso length(M) =:= length(Nodes)
    end, Nodes) end, 30000),
    [rpc:call(N, erlang, send, [imboy_cache_sync, reconcile_tick]) || N <- Nodes],
    await(fun() -> lists:all(fun(N) ->
        rpc:call(N, cache_v2_bench, members_confirmed, []) =:= true
    end, Nodes) end, 30000).

profile(C, Nodes) ->
    SeedNodes = case maps:get(mode, C) of local -> Nodes; replicate -> [node()] end,
    Seeds = [rpc:call(N, ?MODULE, seed, [C, maps:get(keys, C)], infinity) || N <- SeedNodes],
    SeedErrors = lists:sum([maps:get(errors, S) || S <- Seeds]),
    Base = #{config => C, seed => Seeds,
             baseline_registry_extension => application:get_env(imboy, cache_stress_baseline_extension, false),
             cpu_clock_ticks_per_second => clock_ticks(),
             cpu_available => available_cpus(),
             cpu_denominator_source => case os:getenv("CACHE_STRESS_CPUS") of
                 false -> affinity; _ -> explicit_override end,
             cgroup_cpu_max => read_cpu_max(), runtime => cache_v2_bench:runtime_identity(),
             limits => #{n_max => imboy_cache_policy:n_max(),
                         n_max_bytes => imboy_cache_policy:n_max_bytes()}},
    case SeedErrors of
        0 ->
            await(fun() -> lists:all(fun(N) ->
                rpc:call(N, ?MODULE, verify, [C, sentinel]) =:= 0
            end, Nodes) end, 60000),
            %% Full keyspace read verification is outside the timing window.
            Verify = [rpc:call(N, ?MODULE, verify, [C, all], infinity) || N <- Nodes],
            true = lists:all(fun(E) -> E =:= 0 end, Verify),
            measure(C, Nodes, Base#{warm_verify_errors => Verify});
        _ ->
            Base#{errors => SeedErrors, status => seed_rejected,
                  note => <<"No throughput claimed for a keyspace that could not be populated">>}
    end.

key(C, I) ->
    NS = case maps:get(mode, C) of local -> cache_v2_bench_loc; replicate -> shared_cache end,
    {NS, I}.
value(C, I) -> <<I:64, (binary:copy(<<42>>, maps:get(bytes, C) - 8))/binary>>.
seed(C, Keys) ->
    Start = erlang:monotonic_time(microsecond),
    {OK, Errors, Reasons} = lists:foldl(fun(I, {O, E, R}) ->
        case imboy_cache:put(key(C, I), value(C, I), 600, maps:get(mode, C)) of
            ok -> {O + 1, E, R};
            Other -> {O, E + 1, increment(fmt(Other), R)}
        end
    end, {0, 0, #{}}, lists:seq(1, Keys)),
    #{successful => OK, errors => Errors, reasons => Reasons,
      elapsed_us => erlang:monotonic_time(microsecond) - Start}.
verify(C, Which) ->
    Keys = maps:get(keys, C),
    Is = case Which of all -> lists:seq(1, Keys); sentinel -> [1, Keys div 2 + 1, Keys] end,
    lists:foldl(fun(I, E) ->
        case imboy_cache:get(key(C, I)) of
            {ok, V} -> case V =:= value(C, I) of true -> E; false -> E + 1 end;
            _ -> E + 1
        end
    end, 0, Is).

measure(C, Nodes, Base) ->
    Ref = make_ref(),
    Jobs = lists:append([rpc:call(N, ?MODULE, launch, [self(), Ref, C]) || N <- Nodes]),
    Monitors = [{P, erlang:monitor(process, P)} || P <- Jobs],
    Before = samples(Nodes), Start = erlang:monotonic_time(microsecond),
    [P ! {go, Ref} || P <- Jobs],
    {Rows, Peaks} = try
        collect(Monitors, Ref, Nodes, [], Before,
                erlang:monotonic_time(millisecond) + maps:get(seconds, C) * 1000 + 120000)
    after
        [begin exit(P, kill), erlang:demonitor(M, [flush]) end || {P, M} <- Monitors]
    end,
    Stop = erlang:monotonic_time(microsecond), After = samples(Nodes),
    Wall = (Stop - Start) / 1000000,
    CPU = lists:sum([maps:get(cpu_ticks, maps:get(N, After)) -
                      maps:get(cpu_ticks, maps:get(N, Before)) || N <- Nodes]) / clock_ticks(),
    H = lists:foldl(fun(R, Acc) -> merge_counts(maps:get(histogram, R), Acc) end, #{}, Rows),
    OK = lists:sum([maps:get(successful, R) || R <- Rows]),
    Errors = lists:sum([maps:get(errors, R) || R <- Rows]),
    Misses = lists:sum([maps:get(misses, R) || R <- Rows]),
    Reasons = lists:foldl(fun(R, A) -> merge_counts(maps:get(reasons, R), A) end, #{}, Rows),
    Verify = [rpc:call(N, ?MODULE, verify, [C, all], infinity) || N <- Nodes],
    Fraction = CPU / Wall / available_cpus(),
    Base#{errors => Errors + lists:sum(Verify), operation_errors => Errors, reasons => Reasons,
          post_verify_errors => Verify, successful_operations => OK,
          miss_note => <<"Concurrent revision changes may safely return cache miss. Misses are reported separately, excluded from successful hit/write throughput, and included in completed API throughput.">>, attempted_operations => OK + Misses + Errors,
          read_misses => Misses,
          throughput_completed_api_ops_sec => (OK + Misses) / Wall,
          throughput_successful_ops_sec => OK / Wall, wall_seconds => Wall, cpu_seconds => CPU,
          cpu_available_fraction => Fraction, cpu_two_thirds_target_met => Fraction >= 2 / 3,
          latency_us => #{p50 => percentile(H, 0.50), p95 => percentile(H, 0.95), p99 => percentile(H, 0.99)},
          latency_method => <<"all API attempts only; excludes key/value preparation and read validation; power-of-two microsecond histogram upper bounds">>,
          sampled_peaks => maps:from_list([{atom_to_binary(N), V} || {N, V} <- maps:to_list(Peaks)]),
          sample_interval_ms => 100,
          cpu_note => <<"Aggregate BEAM process user+system CPU, including workload generation, result validation, histogram accounting and sampling overhead, / measured wall / available CPUs. No busy-spin padding. Low utilization is a bottleneck result, not a pass.">>}.

launch(Parent, Ref, C) ->
    Padding = binary:copy(<<42>>, maps:get(bytes, C) - 8),
    [spawn(fun() ->
        receive {go, Ref} -> ok end,
        End = erlang:monotonic_time(microsecond) + maps:get(seconds, C) * 1000000,
        R = loop(C#{padding => Padding}, W, 0, End, 0, 0, 0, #{}, #{}),
        Parent ! {result, Ref, self(), R}
    end) || W <- lists:seq(1, maps:get(workers_per_node, C))].
loop(C, W, Count, End, OK, Misses, Errors, H, Reasons) ->
    case erlang:monotonic_time(microsecond) >= End of
        true -> #{successful => OK, misses => Misses, errors => Errors, histogram => H, reasons => Reasons};
        false ->
            I = ((Count * 104729 + W * 7919) rem maps:get(keys, C)) + 1,
            K = key(C, I), Padding = maps:get(padding, C),
            Write = case maps:get(workload, C) of write -> true; read -> false; mixed -> Count rem 5 =:= 0 end,
            {Raw, Elapsed} = case Write of
                true ->
                    V = <<I:64, Padding/binary>>, Mode = maps:get(mode, C),
                    timed(fun() -> imboy_cache:put(K, V, 600, Mode) end);
                false -> timed(fun() -> imboy_cache:get(K) end)
            end,
            %% Compare the complete payload, using shared padding. Reads never
            %% allocate a replacement expected value, and validation is untimed.
            Result = case {Write, Raw} of
                {true, ok} -> ok;
                {false, {ok, <<I:64, Padding/binary>>}} -> ok;
                {false, undefined} when map_get(workload, C) =:= mixed -> miss;
                {false, undefined} -> unexpected_read_miss;
                {false, Other} -> {read_mismatch, result_tag(Other)};
                {true, Other} -> Other
            end,
            H1 = increment(bucket(Elapsed, 1), H),
            case Result of
                ok -> loop(C, W, Count + 1, End, OK + 1, Misses, Errors, H1, Reasons);
                miss -> loop(C, W, Count + 1, End, OK, Misses + 1, Errors, H1, Reasons);
                _ -> loop(C, W, Count + 1, End, OK, Misses, Errors + 1, H1, increment(fmt(Result), Reasons))
            end
    end.
timed(F) ->
    Start = erlang:monotonic_time(microsecond),
    Result = try F() catch Class:Reason -> {Class, Reason} end,
    {Result, erlang:monotonic_time(microsecond) - Start}.
result_tag({ok, _}) -> wrong_value;
result_tag(T) -> T.
bucket(V, B) when V =< B -> B;
bucket(V, B) -> bucket(V, B * 2).
increment(K, M) -> maps:update_with(K, fun(N) -> N + 1 end, 1, M).
merge_counts(A, B) -> maps:fold(fun(K, V, Acc) -> maps:update_with(K, fun(N) -> N + V end, V, Acc) end, B, A).
percentile(H, Q) ->
    Target = max(1, ceil(lists:sum(maps:values(H)) * Q)),
    percentile_bucket(lists:sort(maps:to_list(H)), Target).
percentile_bucket([], _) -> null;
percentile_bucket([{B, N} | _], T) when N >= T -> B;
percentile_bucket([{_, N} | Rest], T) -> percentile_bucket(Rest, T - N).

collect([], _, _, Rows, Peaks, _) -> {Rows, Peaks};
collect(Jobs, Ref, Nodes, Rows, Peaks, Deadline) ->
    true = erlang:monotonic_time(millisecond) < Deadline,
    receive
        {result, Ref, P, R} ->
            {P, Monitor} = lists:keyfind(P, 1, Jobs),
            erlang:demonitor(Monitor, [flush]),
            collect(lists:keydelete(P, 1, Jobs), Ref, Nodes, [R | Rows], Peaks, Deadline);
        {'DOWN', Monitor, process, P, Reason} ->
            case lists:member({P, Monitor}, Jobs) of
                true -> error({worker_failed, P, Reason});
                false -> collect(Jobs, Ref, Nodes, Rows, Peaks, Deadline)
            end
    after 100 ->
        New = samples(Nodes),
        Peak = maps:map(fun(N, Values) ->
            Old = maps:get(N, Peaks), maps:map(fun(K, V) -> max(V, maps:get(K, Old)) end, Values)
        end, New),
        collect(Jobs, Ref, Nodes, Rows, Peak, Deadline)
    end.
samples(Nodes) -> maps:from_list([{N, rpc:call(N, ?MODULE, snapshot, [])} || N <- Nodes]).
snapshot() ->
    {ok, Stat} = file:read_file("/proc/" ++ os:getpid() ++ "/stat"),
    %% Fields after comm's closing parenthesis start with field 3 (state).
    [_, Tail] = binary:split(Stat, <<") ">>),
    Fields = string:lexemes(binary_to_list(Tail), " \n"),
    CPU = list_to_integer(lists:nth(12, Fields)) + list_to_integer(lists:nth(13, Fields)),
    #{cpu_ticks => CPU, memory_bytes => erlang:memory(total),
      cache_queue => queue(imboy_cache), sync_queue => queue(imboy_cache_sync),
      run_queue => erlang:statistics(run_queue)}.
queue(Name) ->
    case process_info(whereis(Name), message_queue_len) of {message_queue_len, N} -> N; _ -> 0 end.
read_cpu_max() ->
    case file:read_file("/sys/fs/cgroup/cpu.max") of
        {ok, B} -> string:trim(B);
        {error, Reason} -> fmt({unavailable, Reason})
    end.
clock_ticks() -> list_to_integer(string:trim(os:cmd("getconf CLK_TCK"))).
available_cpus() ->
    case os:getenv("CACHE_STRESS_CPUS") of
        false ->
            {ok, Status} = file:read_file("/proc/self/status"),
            [Line] = [L || L <- string:split(binary_to_list(Status), "\n", all), lists:prefix("Cpus_allowed_list:", L)],
            [_, Ranges] = string:split(Line, ":"),
            lists:sum([cpu_range(R) || R <- string:split(string:trim(Ranges), ",", all)]);
        V ->
            N = case string:to_float(V) of {F, []} -> F; _ -> list_to_integer(V) end,
            true = N > 0, N
    end.
cpu_range(R) ->
    case string:split(R, "-") of [A, B] -> list_to_integer(B) - list_to_integer(A) + 1; [_] -> 1 end.
await(F, Timeout) -> await_until(F, erlang:monotonic_time(millisecond) + Timeout).
await_until(F, End) ->
    case F() of true -> ok; false ->
        case erlang:monotonic_time(millisecond) >= End of
            true -> error(readiness_timeout);
            false -> timer:sleep(100), await_until(F, End)
        end
    end.
