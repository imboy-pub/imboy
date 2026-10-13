%% C12 V2 benchmark: same driver/API against separately frozen V2 runtime trees.
%% W0 TTL=3600 is checked, never enabled by raising the product TTL budget.
%% Sampled resource peaks are observations, not hard heap/mailbox bounds.
-module(cache_v2_bench).
-export([
    run/0,
    app_up/0,
    reset_resource_sample/0,
    resource_sample/0,
    applied/2,
    runtime_identity/0,
    members_confirmed/0
]).
-define(REP_NS, cache_v2_bench_rep).
-define(LOC_NS, cache_v2_bench_loc).
-define(KEYSPACE, 1024).

run() ->
    Output = os:getenv("CACHE_V2_OUTPUT"),
    true = is_list(Output),
    Ops = list_to_integer(os:getenv("CACHE_V2_OPERATIONS", "10000")),
    true = Ops > 0 andalso Ops =< 100000,
    Started = erlang:monotonic_time(millisecond),
    try
        ok = start_distribution(),
        ok = app_up(),
        Checks = self_checks(),
        Sizes = [{16, 64}, {64, 1024}, {256, 8192}, {32, 256}],
        Local = lists:append([local_profile(Ops, KB, VB) || {KB, VB} <- Sizes]),
        Remote = three_node_rows(Ops, Sizes),
        Rows = Local ++ Remote,
        Errors = lists:sum([maps:get(errors, R) || R <- Rows]),
        %% TTL profile incompatibility remains independently visible, even with zero measured errors.
        Compatible = maps:get(w0_ttl_compatible, Checks),
        %% Both V2 arms use the legal TTL600 profile; W0 TTL3600 is a separate environment anchor.
        Payload = result(Rows, Checks, Ops, Started),
        Exit = maps:get(exit_code, Payload),
        ok = write_result(Output, Payload),
        io:format("cache_v2_bench rows=~p errors=~p profile_compatible=~p exit=~p~n", [
            length(Rows), Errors, Compatible, Exit
        ]),
        ok = print_counts(Payload),
        halt(Exit)
    catch
        Class:Reason:Stack ->
            Failure = #{
                task_id => <<"CACHE_V221_BACKEND_CLOSEOUT_V1">>,
                candidate_sha256 => candidate(),
                status => <<"fail">>,
                exit => 2,
                exit_code => 2,
                utc => list_to_binary(
                    calendar:system_time_to_rfc3339(erlang:system_time(millisecond), [
                        {unit, millisecond}, {offset, "Z"}
                    ])
                ),
                counts => #{passed => 0, failed => 1, cancelled => 0, skipped => 0},
                scenarios => [
                    #{
                        name => <<"driver_exception">>,
                        status => <<"fail">>,
                        detail => iolist_to_binary(io_lib:format("~p:~p", [Class, reason_tag(Reason)]))
                    }
                ],
                error_class => Class,
                error => reason_tag(Reason),
                elapsed_ms => erlang:monotonic_time(millisecond) - Started
            },
            _ = write_result(Output, Failure),
            io:format("cache_v2_bench failed ~p:~p at ~p~n", [
                Class, reason_tag(Reason), lists:sublist(Stack, 2)
            ]),
            _ = print_counts(Failure),
            halt(2)
    end.

write_result(Output, Payload) ->
    file:write_file(Output, jsone:encode(Payload)).

print_counts(Payload) ->
    #{cancelled := Cancelled, failed := Failed, passed := Passed, skipped := Skipped} =
        maps:get(counts, Payload),
    io:format("CLOSEOUT_COUNTS {\"cancelled\":~B,\"failed\":~B,\"passed\":~B,\"skipped\":~B}~n", [
        Cancelled, Failed, Passed, Skipped
    ]),
    ok.

start_distribution() ->
    {ok, _} = application:ensure_all_started(crypto),
    case node() of
        nonode@nohost ->
            Name = list_to_atom(
                "cacheperf_" ++ integer_to_list(erlang:unique_integer([positive])) ++ "@127.0.0.1"
            ),
            {ok, _} = net_kernel:start([Name, longnames]),
            true = erlang:set_cookie(
                node(),
                list_to_atom(
                    binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(24), lowercase))
                )
            ),
            ok;
        _ ->
            ok
    end.

app_up() ->
    ok = deps_on_path(),
    case application:load(imboy) of
        ok -> ok;
        {error, {already_loaded, imboy}} -> ok
    end,
    application:set_env(imboy, cache_registry_overrides, #{
        ?REP_NS => {replicate, false}, ?LOC_NS => {local, false}
    }),
    application:set_env(imboy, cache_replication, #{whitelist => [?REP_NS, ?LOC_NS]}),
    application:set_env(imboy, cache_fenced_namespaces, [?REP_NS, ?LOC_NS]),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(syn),
    ok = syn:add_node_to_scopes([imboy_cache]),
    {ok, D} = depcache:start_link(imboy_cache, #{memory_max => 64}),
    true = unlink(D),
    {ok, M} = elib_metric:start_link(),
    true = unlink(M),
    {ok, S} = imboy_cache_sync:start_link(),
    true = unlink(S),
    await(fun() -> imboy_cache_sync:status() =:= ready end, 20000).

self_checks() ->
    RequiredChecks = [
        memo_single_flight, invalidation_race, loader_exception, writer_crash, get_wait_handoff
    ],
    AllChecksPassed = maps:from_list([{Key, true} || Key <- RequiredChecks]),
    false = self_checks_complete(#{checks => maps:put(loader_exception, false, AllChecksPassed)}),
    true = self_checks_complete(#{checks => AllChecksPassed}),
    Key = {?REP_NS, profile_ttl},
    TTLResult = imboy_cache:put(Key, <<"ttl-check">>, 3600, replicate),
    Compatible =
        case TTLResult of
            ok -> true;
            {error, ttl_out_of_range} -> false;
            Other -> error({ttl_check_failed, Other})
        end,
    Counter = atomics:new(1, []),
    Parent = self(),
    MemoKey = {?LOC_NS, single_flight},
    Loader = fun() ->
        atomics:add(Counter, 1, 1),
        Parent ! {loader_started, self()},
        receive
            release_loader -> <<"single-flight">>
        after 5000 -> error(loader_release_timeout)
        end
    end,
    Jobs = [
        spawn_monitor(fun() ->
            Parent ! {memo_attempt, self()},
            V = imboy_cache:memo(Loader, MemoKey, 600, []),
            Parent ! {sample_result, self(), V}
        end)
     || _ <- lists:seq(1, 32)
    ],
    LoaderPid =
        receive
            {loader_started, P} -> P
        after 5000 -> error(loader_not_started)
        end,
    [
        receive
            {memo_attempt, Pid} -> ok
        after 5000 -> error(memo_attempt_timeout)
        end
     || {Pid, _} <- Jobs
    ],
    try
        ok = await(
            fun() ->
                lists:all(
                    fun({Pid, _}) ->
                        Pid =:= LoaderPid orelse process_info(Pid, status) =:= {status, waiting}
                    end,
                    Jobs
                )
            end,
            3000
        )
    after
        LoaderPid ! release_loader
    end,
    Values =
        try
            collect(Jobs, [], erlang:monotonic_time(millisecond) + 10000)
        after
            [exit(Pid, kill) || {Pid, _} <- Jobs, is_process_alive(Pid)]
        end,
    true = lists:all(fun(V) -> V =:= <<"single-flight">> end, Values),
    1 = atomics:get(Counter, 1),
    Checks = #{
        memo_single_flight => true,
        invalidation_race => invalidation_race_check(),
        loader_exception => loader_exception_check(),
        writer_crash => writer_crash_check(),
        get_wait_handoff => get_wait_handoff_check()
    },
    #{
        checks => Checks,
        memo_single_flight_workers => 32,
        loader_executions => 1,
        w0_requested_ttl_sec => 3600,
        v2_measured_ttl_sec => 600,
        w0_ttl_compatible => Compatible
    }.

invalidation_race_check() ->
    Parent = self(),
    Key = {?LOC_NS, {invalidation_race, erlang:unique_integer([positive])}},
    Counter = atomics:new(1, []),
    Release = make_ref(),
    Loader = fun() ->
        case atomics:add_get(Counter, 1, 1) of
            1 ->
                Parent ! {invalidation_loader_started, self()},
                receive Release -> stale end;
            _ -> fresh
        end
    end,
    First = spawn(fun() -> Parent ! {invalidation_result, self(), imboy_cache:memo(Loader, Key, 600, [])} end),
    receive
        {invalidation_loader_started, First} -> ok
    after 5000 -> error(invalidation_loader_timeout)
    end,
    Second = spawn(fun() ->
        Parent ! {invalidation_waiter_attempt, self()},
        Parent ! {invalidation_waiter_result, self(), imboy_cache:memo(Loader, Key, 600, [])}
    end),
    try
        receive
            {invalidation_waiter_attempt, Second} -> ok
        after 5000 -> error(invalidation_waiter_timeout)
        end,
        ok = await(fun() -> process_info(Second, status) =:= {status, waiting} end, 3000),
        ok = imboy_cache:invalidate(Key)
    after
        First ! Release
    end,
    fresh = receive_result(invalidation_result, First),
    fresh = receive_result(invalidation_waiter_result, Second),
    2 = atomics:get(Counter, 1),
    true.

loader_exception_check() ->
    Parent = self(),
    Key = {?LOC_NS, {loader_exception, erlang:unique_integer([positive])}},
    Ball = {cache_v2_loader_exception, make_ref()},
    Release = make_ref(),
    Loader = fun() ->
        Parent ! {exception_loader_started, self()},
        receive Release -> throw(Ball) end
    end,
    LoaderResult = spawn(fun() -> Parent ! {exception_result, self(), memo_exception(Loader, Key, Ball)} end),
    receive
        {exception_loader_started, LoaderResult} -> ok
    after 5000 -> error(exception_loader_timeout)
    end,
    Waiters = [spawn(fun() ->
        Parent ! {exception_waiter_attempt, self()},
        Parent ! {exception_waiter_result, self(), memo_exception(Loader, Key, Ball)}
    end) || _ <- lists:seq(1, 3)],
    try
        [receive {exception_waiter_attempt, Pid} -> ok after 5000 -> error(exception_waiter_timeout) end || Pid <- Waiters],
        ok = await(fun() -> all_waiting(Waiters) end, 3000)
    after
        LoaderResult ! Release
    end,
    [thrown = receive_result(exception_waiter_result, Pid) || Pid <- Waiters],
    thrown = receive_result(exception_result, LoaderResult),
    undefined = imboy_cache:get(Key),
    true.

memo_exception(Loader, Key, Ball) ->
    try imboy_cache:memo(Loader, Key, 600, []) of
        _ -> returned
    catch
        throw:Ball -> thrown
    end.

writer_crash_check() ->
    Parent = self(),
    Key = {cache_v2_bench_wait, {writer_crash, erlang:unique_integer([positive])}},
    Cache = whereis(imboy_cache),
    1 = erlang:trace(Cache, true, ['receive', {tracer, Parent}]),
    Writer = spawn(fun() ->
        Parent ! {writer_claim, self(), imboy_cache:get_wait(Key)},
        receive crash -> ok end
    end),
    try
        receive {writer_claim, Writer, undefined} -> ok after 5000 -> error(writer_claim_timeout) end,
        Waiter = spawn(fun() -> Parent ! {writer_waiter_result, self(), imboy_cache:get_wait(Key)} end),
        ok = expect_get_wait(Cache, Waiter, Key),
        Memo = spawn(fun() ->
            Parent ! {writer_memo_attempt, self()},
            Parent ! {writer_memo_result, self(), imboy_cache:memo(fun() -> retried_fresh end, Key, 600, [])}
        end),
        receive {writer_memo_attempt, Memo} -> ok after 5000 -> error(writer_memo_timeout) end,
        ok = await(fun() -> process_info(Memo, status) =:= {status, waiting} end, 3000),
        exit(Writer, kill),
        {error, premature_exit} = receive_result(writer_waiter_result, Waiter),
        retried_fresh = receive_result(writer_memo_result, Memo),
        true
    after
        1 = erlang:trace(Cache, false, ['receive']),
        case is_process_alive(Writer) of true -> exit(Writer, kill); false -> ok end
    end.

get_wait_handoff_check() ->
    Parent = self(),
    Key = {cache_v2_bench_wait, {get_wait_handoff, erlang:unique_integer([positive])}},
    Cache = whereis(imboy_cache),
    1 = erlang:trace(Cache, true, ['receive', {tracer, Parent}]),
    try
        undefined = imboy_cache:get_wait(Key),
        Waiter = spawn(fun() -> Parent ! {handoff_result, self(), imboy_cache:get_wait(Key)} end),
        ok = expect_get_wait(Cache, Waiter, Key),
        ok = imboy_cache:put(Key, handed_off, 600, local),
        {ok, handed_off} = receive_result(handoff_result, Waiter),
        true
    after
        1 = erlang:trace(Cache, false, ['receive'])
    end.

expect_get_wait(Cache, Pid, Key) ->
    receive
        {trace, Cache, 'receive', {'$gen_call', {Pid, _}, {get_wait, Key}}} -> ok
    after 5000 -> error(get_wait_call_not_observed)
    end.

receive_result(Tag, Pid) ->
    receive
        {Tag, Pid, Result} -> Result
    after 10000 -> error({self_check_worker_timeout, Tag})
    end.

all_waiting(Pids) ->
    lists:all(fun(Pid) -> process_info(Pid, status) =:= {status, waiting} end, Pids).

local_profile(Ops, KB, VB) ->
    lists:append([
        begin
            ok = reset_components(),
            [bench(Kind, W, Ops, KB, VB, [node()]) || W <- [1, 8, 32]]
        end
     || Kind <- [rep_put, rep_get, invalidate_then_memo, mixed]
    ]).

reset_components() ->
    %% Only this isolated VM's explicitly constructed benchmark processes are reset.
    [
        case whereis(Name) of
            undefined -> ok;
            Pid -> ok = gen_server:stop(Pid, normal, 5000)
        end
     || Name <- [imboy_cache_sync, imboy_cache, elib_metric]
    ],
    app_up().

three_node_rows(Ops, Sizes) ->
    lists:append([
        begin
            ok = reset_components(),
            three_node_profile(Ops, KB, VB)
        end
     || {KB, VB} <- Sizes
    ]).

three_node_profile(Ops, KB, VB) ->
    Paths = lists:append([
        ["-pa", P]
     || P <- lists:reverse(code:get_path()), filename:pathtype(P) =:= absolute
    ]),
    Args =
        [
            "-setcookie",
            atom_to_list(erlang:get_cookie()),
            "-kernel",
            "inet_dist_use_interface",
            "{127,0,0,1}",
            "prevent_overlapping_partitions",
            "false"
        ] ++ Paths,
    {ok, P1, N1} = peer:start(#{
        name => peer:random_name("cacheperf1"),
        host => "127.0.0.1",
        connection => standard_io,
        args => Args
    }),
    try
        ok = peer:call(P1, ?MODULE, app_up, [], 30000),
        Identity = runtime_identity(),
        Identity = peer:call(P1, ?MODULE, runtime_identity, [], 5000),
        {ok, P2, N2} = peer:start(#{
            name => peer:random_name("cacheperf2"),
            host => "127.0.0.1",
            connection => standard_io,
            args => Args
        }),
        try
            ok = peer:call(P2, ?MODULE, app_up, [], 30000),
            Identity = peer:call(P2, ?MODULE, runtime_identity, [], 5000),
            true = net_kernel:connect_node(N1),
            true = net_kernel:connect_node(N2),
            true = rpc:call(N1, net_kernel, connect_node, [N2], 5000),
            Nodes = [node(), N1, N2],
            ok = await(
                fun() ->
                    lists:all(
                        fun(N) ->
                            M = rpc:call(N, syn, members, [imboy_cache, dsync_handler], 1000),
                            is_list(M) andalso length(M) =:= 3
                        end,
                        Nodes
                    )
                end,
                10000
            ),
            %% Establish real actor handshakes before measuring steady-state replication.
            [
                reconcile_tick = rpc:call(N, erlang, send, [imboy_cache_sync, reconcile_tick])
             || N <- Nodes
            ],
            ok = await(
                fun() ->
                    lists:all(
                        fun(N) ->
                            rpc:call(N, ?MODULE, members_confirmed, [], 1000) =:= true
                        end,
                        Nodes
                    )
                end,
                10000
            ),
            [bench(convergence, W, Ops, KB, VB, Nodes) || W <- [1, 8, 32]]
        after
            stop_peer(P2)
        end
    after
        stop_peer(P1)
    end.

bench(Kind, Workers, Ops, KeyBytes, ValueBytes, Nodes) ->
    Value = binary:copy(<<"v">>, ValueBytes),
    %% Every GET has an asserted warmed value; invalidate never contaminates GET rows.
    case Kind of
        invalidate_then_memo ->
            ok;
        _ ->
            [
                ok = imboy_cache:put(key(rep, I, KeyBytes), Value, 600, replicate)
             || I <- lists:seq(0, ?KEYSPACE - 1)
            ]
    end,
    [ok = rpc:call(N, ?MODULE, reset_resource_sample, [], 1000) || N <- Nodes],
    Parent = self(),
    Sampler = spawn_monitor(fun() -> sample(Parent, Nodes, #{}) end),
    Start = erlang:monotonic_time(microsecond),
    Jobs = [
        spawn_monitor(fun() ->
            put(bench_loader_counter, atomics:new(1, [])),
            Samples = [
                operation(Kind, I, W, Workers, KeyBytes, Value, Nodes)
             || I <- lists:seq(1, Ops)
            ],
            Parent ! {sample_result, self(), Samples}
        end)
     || W <- lists:seq(1, Workers)
    ],
    Samples =
        try
            lists:append(collect(Jobs, [], erlang:monotonic_time(millisecond) + 120000))
        catch
            Class:Reason:Stack ->
                {SamplerPid, _} = Sampler,
                exit(SamplerPid, kill),
                erlang:raise(Class, Reason, Stack)
        after
            [exit(P, kill) || {P, _} <- Jobs, is_process_alive(P)]
        end,
    Elapsed = erlang:monotonic_time(microsecond) - Start,
    {SP, SR} = Sampler,
    SP ! stop,
    Peaks =
        receive
            {resource_peaks, SP, P} ->
                receive
                    {'DOWN', SR, process, SP, normal} -> P
                after 2000 -> error(sampler_down_timeout)
                end;
            {'DOWN', SR, process, SP, Why} ->
                error({sampler_failed, Why})
        after 5000 ->
            exit(SP, kill),
            error(sampler_timeout)
        end,
    Latencies = lists:sort([L || {L, _} <- Samples]),
    Errors = length([bad || {_, S} <- Samples, S =/= ok]),
    #{
        kind => Kind,
        workers => Workers,
        operations_per_worker => Ops,
        operations => length(Samples),
        key_bytes => KeyBytes,
        value_bytes => ValueBytes,
        keyspace => ?KEYSPACE,
        ttl_sec => ttl(Kind),
        throughput_ops_sec => length(Samples) * 1000000 / max(1, Elapsed),
        p50_us => percentile(Latencies, 0.50),
        p95_us => percentile(Latencies, 0.95),
        p99_us => percentile(Latencies, 0.99),
        errors => Errors,
        error_rate => Errors / length(Samples),
        elapsed_us => Elapsed,
        resource_sample_peaks => Peaks,
        resource_sampling_delay_ms => 5,
        resource_peak_is_hard_bound => false,
        receiver_count => length(Nodes)
    }.

ttl(invalidate_then_memo) -> 600;
ttl(_) -> 600.

operation(Kind, I, Worker, Workers, KB, Value, Nodes) ->
    Key = key(rep, (I + Worker) rem ?KEYSPACE, KB),
    Start = erlang:monotonic_time(microsecond),
    Status =
        try
            case Kind of
                rep_put ->
                    ok = imboy_cache:put(Key, Value, 600, replicate);
                rep_get ->
                    {ok, Value} = imboy_cache:get(Key);
                invalidate_then_memo ->
                    Slots = ?KEYSPACE div Workers,
                    MKey = key(local, (Worker - 1) * Slots + I rem Slots, KB),
                    Counter = get(bench_loader_counter),
                    Before = atomics:get(Counter, 1),
                    ok = imboy_cache:invalidate(MKey),
                    Value = imboy_cache:memo(
                        fun() ->
                            atomics:add(Counter, 1, 1),
                            Value
                        end,
                        MKey,
                        600,
                        []
                    ),
                    true = atomics:get(Counter, 1) =:= Before + 1;
                mixed when I rem 10 =:= 0 -> ok = imboy_cache:put(Key, Value, 600, replicate);
                mixed ->
                    {ok, Value} = imboy_cache:get(Key);
                convergence ->
                    %% Each worker owns a key subset: a later competing write cannot satisfy this oracle.
                    Slots = ?KEYSPACE div Workers,
                    CKey = key(rep, (Worker - 1) * Slots + I rem Slots, KB),
                    Tagged = {Worker, I, Value},
                    ok = imboy_cache:put(CKey, Tagged, 600, replicate),
                    {ok, Revision} = revision(CKey),
                    ok = await(
                        fun() ->
                            lists:all(
                                fun(N) ->
                                    rpc:call(N, ?MODULE, applied, [CKey, {Tagged, Revision}], 1000) =:=
                                        true
                                end,
                                Nodes
                            )
                        end,
                        5000
                    )
            end,
            ok
        catch
            Class:Reason -> {error, Class, reason_tag(Reason)}
        end,
    {erlang:monotonic_time(microsecond) - Start, Status}.

key(Type, Index, Bytes) ->
    Namespace =
        case Type of
            rep -> ?REP_NS;
            local -> ?LOC_NS
        end,
    Encoded =
        case Index of
            N when is_integer(N) -> <<0, N:32/unsigned>>;
            {Worker, N} -> <<1, Worker:32/unsigned, N:32/unsigned>>
        end,
    {Namespace, <<Encoded/binary, 0:(max(0, Bytes - byte_size(Encoded)) * 8)>>}.

revision(Key) ->
    case ets:lookup(imboy_cache_meta, {rev, Key}) of
        [{{rev, Key}, Tuple}] -> {ok, element(1, Tuple)};
        [] -> undefined
    end.

applied(Key, {Value, Rev}) ->
    imboy_cache:get(Key) =:= {ok, Value} andalso revision(Key) =:= {ok, Rev}.

collect([], Acc, _) ->
    lists:reverse(Acc);
collect([{Pid, Ref} | Rest], Acc, Deadline) ->
    Left = max(1, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {sample_result, Pid, Result} ->
            receive
                {'DOWN', Ref, process, Pid, normal} -> ok
            after Left -> error(worker_down_timeout)
            end,
            collect(Rest, [Result | Acc], Deadline);
        {'DOWN', Ref, process, Pid, Why} ->
            error({worker_failed, Why})
    after Left -> error(worker_timeout)
    end.

sample(Parent, Nodes, Peaks) ->
    Current = [{atom_to_binary(N), rpc:call(N, ?MODULE, resource_sample, [], 1000)} || N <- Nodes],
    Next = lists:foldl(
        fun({N, Row}, Acc) ->
            true = is_map(Row),
            Old = maps:get(N, Acc, #{}),
            Acc#{N => maps:map(fun(K, V) -> peak_value(V, maps:get(K, Old, null)) end, Row)}
        end,
        Peaks,
        Current
    ),
    receive
        stop -> Parent ! {resource_peaks, self(), Next}
    after 5 -> sample(Parent, Nodes, Next)
    end.

reset_resource_sample() ->
    gen_server:call(imboy_cache_sync, benchmark_reset_snapshot_page_peak, 1000).

resource_sample() ->
    SyncPid = whereis(imboy_cache_sync),
    Info = process_info(SyncPid, [message_queue_len, memory, total_heap_size]),
    Tables = [T || T <- ets:all(), lists:prefix("imboy_cache_", atom_to_list(ets:info(T, name)))],
    MetaWords = lists:sum([ets:info(T, memory) || T <- Tables]),
    maps:merge(logical_usage(), #{
        mailbox => proplists:get_value(message_queue_len, Info),
        buffer_count => null,
        buffer_bytes => null,
        page_count => snapshot_page_peak(1),
        page_bytes => snapshot_page_peak(2),
        journal_count => journal_usage(3),
        journal_bytes => journal_usage(4),
        grace_count => ets:select_count(imboy_cache_meta, [{{{grace_incarnation, '_'}, '_'}, [], [true]}]),
        peer_count => ets:select_count(imboy_cache_meta, [{{{peer_incarnation, '_'}, '_'}, [], [true]}]),
        process_memory_bytes => proplists:get_value(memory, Info),
        heap_bytes => proplists:get_value(total_heap_size, Info) * erlang:system_info(wordsize),
        metadata_entries => lists:sum([ets:info(T, size) || T <- Tables]),
        metadata_bytes => MetaWords * erlang:system_info(wordsize),
        cache_ets_bytes => depcache:size(imboy_cache) * erlang:system_info(wordsize)
    }).

snapshot_page_peak(Index) -> page_peak(snapshot_page_peak, Index).

page_peak(Key, Index) ->
    case ets:lookup(imboy_cache_meta, Key) of
        [{Key, Peak}] -> element(Index, Peak);
        [] -> 0
    end.

journal_usage(Index) ->
    case ets:lookup(imboy_cache_meta, journal_window) of
        [{journal_window, Window}] -> element(Index, Window);
        [] ->
            case ets:select_count(imboy_cache_meta, [{{{journal, '_'}, '_'}, [], [true]}]) of
                0 -> 0;
                _ -> null
            end
    end.

logical_usage() ->
    Current = code:which(imboy_cache_sync_coverage) =/= non_existing,
    Usage =
        case ets:lookup(imboy_cache_meta, metadata_usage) of
            [{metadata_usage, U}] ->
                U;
            [] when Current ->
                %% No fabricated counter: directly prove there are no managed rows before reporting zero.
                Empty =
                    ets:select_count(imboy_cache_meta, [
                        {
                            {{'$1', '_'}, '_'},
                            [{'orelse', {'=:=', '$1', rev}, {'=:=', '$1', rindex}}],
                            [true]
                        }
                    ]) =:= 0,
                case Empty of
                    true -> {0, 0, 0, 0};
                    false -> unavailable
                end;
            [] ->
                unavailable
        end,
    {N, NB, T, TB} =
        case Usage of
            {_, _, _, _} -> Usage;
            unavailable -> {null, null, null, null}
        end,
    Fields = #{
        logical_n => N,
        logical_n_bytes => NB,
        logical_t => T,
        logical_t_bytes => TB,
        logical_gen => metric_counter(gen_count),
        logical_gen_bytes => metric_counter(gen_bytes)
    },
    Pairs = [
        {N, policy_limit(n_max)},
        {NB, policy_limit(n_max_bytes)},
        {T, policy_limit(t_max)},
        {TB, policy_limit(t_max_bytes)},
        {maps:get(logical_gen, Fields), policy_limit(n_max)},
        {maps:get(logical_gen_bytes, Fields), policy_limit(gen_max_bytes)}
    ],
    Available = [{V, L} || {V, L} <- Pairs, is_integer(V), is_integer(L)],
    Violation =
        case Available of
            [] ->
                null;
            _ ->
                case lists:any(fun({V, L}) -> V > L end, Available) of
                    true -> 1;
                    false -> 0
                end
        end,
    Fields#{logical_budget_violation => Violation}.

metric_counter(Key) ->
    case ets:lookup(imboy_cache_meta, Key) of
        [{Key, V}] when is_integer(V) -> V;
        [] -> null
    end.

policy_limit(F) ->
    case erlang:function_exported(imboy_cache_policy, F, 0) of
        true -> apply(imboy_cache_policy, F, []);
        false -> null
    end.

peak_value(null, Old) -> Old;
peak_value(Value, null) -> Value;
peak_value(Value, Old) -> max(Value, Old).

await(Pred, Timeout) -> await_until(Pred, erlang:monotonic_time(millisecond) + Timeout).
await_until(Pred, Deadline) ->
    case Pred() of
        true ->
            ok;
        false ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true ->
                    error(oracle_timeout);
                false ->
                    timer:sleep(1),
                    await_until(Pred, Deadline)
            end
    end.

percentile(Samples, Fraction) -> lists:nth(max(1, ceil(length(Samples) * Fraction)), Samples).
reason_tag(Reason) when is_atom(Reason) -> Reason;
reason_tag(Reason) when is_tuple(Reason) -> element(1, Reason);
reason_tag(_) -> operation_failed.

candidate() ->
    case os:getenv("CANDIDATE_SHA256") of
        false ->
            null;
        Sha when length(Sha) =:= 64 ->
            true = lists:all(
                fun(C) -> (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f) end, Sha
            ),
            list_to_binary(Sha);
        _ ->
            error(malformed_candidate_sha256)
    end.

result(Rows, Checks, Ops, Started) ->
    Errors = lists:sum([maps:get(errors, R) || R <- Rows]),
    Absolute = lists:all(fun absolute_threshold/1, Rows),
    Compatible = maps:get(w0_ttl_compatible, Checks),
    Scenarios = [
        scenario(
            <<"ttl_protocol_admission">>,
            not Compatible,
            io_lib:format("TTL 3600 rejected by V2 profile; measured memo TTL=~B", [
                maps:get(v2_measured_ttl_sec, Checks)
            ])
        ),
        scenario(
            <<"measured_operations">>,
            Errors =:= 0,
            io_lib:format("~B rows, ~B operations, ~B errors", [
                length(Rows), lists:sum([maps:get(operations, R) || R <- Rows]), Errors
            ])
        ),
        scenario(
            <<"frozen_absolute_thresholds">>,
            Absolute,
            io_lib:format("~B rows checked against frozen latency and logical budget limits", [
                length(Rows)
            ])
        ),
        scenario(
            <<"self_checks">>,
            self_checks_complete(Checks),
            io_lib:format("implemented=~p required=~p", [
                maps:keys(maps:get(checks, Checks, #{})),
                [memo_single_flight_workers, invalidation_race, loader_exception,
                    writer_crash, get_wait_handoff]
            ])
        )
    ],
    Counts = scenario_counts(Scenarios),
    Exit = case maps:get(failed, Counts) of 0 -> 0; _ -> 2 end,
    #{
        task_id => <<"CACHE_V221_BACKEND_CLOSEOUT_V1">>,
        candidate_sha256 => candidate(),
        utc => list_to_binary(
            calendar:system_time_to_rfc3339(erlang:system_time(millisecond), [
                {unit, millisecond}, {offset, "Z"}
            ])
        ),
        elapsed_ms => erlang:monotonic_time(millisecond) - Started,
        exit => Exit,
        exit_code => Exit,
        status => case Exit of 0 -> <<"pass">>; _ -> <<"fail">> end,
        counts => Counts,
        profile => <<"v2-contract-ttl600-w0-anchor-ttl3600-not-equivalent">>,
        performance_scope => <<"single_arm_measurement_receipt_only">>,
        invalidate_then_memo_path => <<"includes_invalidation_and_refill">>,
        arm => list_to_binary(os:getenv("CACHE_V2_ARM", "candidate")),
        operations_per_worker => Ops,
        env => #{
            otp => list_to_binary(erlang:system_info(otp_release)),
            schedulers => erlang:system_info(schedulers_online)
        },
        runtime_beam_hashes => runtime_hashes(),
        config_hash => binary:encode_hex(imboy_cache_sync_proto:config_hash(), lowercase),
        registry_hash => binary:encode_hex(imboy_cache_sync_proto:registry_hash(), lowercase),
        self_checks => Checks,
        rows => Rows,
        scenarios => Scenarios,
        frozen_thresholds_us => #{get_p99 => 2000, set_p99 => 5000, convergence_p99 => 1000000},
        relative_gate =>
            <<"requires independently frozen V2 baseline and three interleaved pairs; get degradation<=10%, throughput>=95%">>
    }.

scenario(Name, Passed, Detail) ->
    #{
        name => Name,
        status => case Passed of true -> <<"pass">>; false -> <<"fail">> end,
        detail => iolist_to_binary(Detail)
    }.

scenario_counts(Scenarios) ->
    Passed = length([ok || #{status := <<"pass">>} <- Scenarios]),
    Failed = length(Scenarios) - Passed,
    #{passed => Passed, failed => Failed, cancelled => 0, skipped => 0}.

self_checks_complete(Checks) ->
    Results = maps:get(checks, Checks, #{}),
    lists:all(
        fun(Key) -> maps:get(Key, Results, false) =:= true end,
        [memo_single_flight, invalidation_race, loader_exception, writer_crash, get_wait_handoff]
    ).

absolute_threshold(Row) ->
    Limit =
        case maps:get(kind, Row) of
            rep_get -> 2000;
            rep_put -> 5000;
            convergence -> 1000000;
            _ -> infinity
        end,
    Latency =
        case Limit of
            infinity -> true;
            _ -> maps:get(p99_us, Row) =< Limit
        end,
    Latency andalso
        lists:all(
            fun(Peak) -> maps:get(logical_budget_violation, Peak, null) =/= 1 end,
            maps:values(maps:get(resource_sample_peaks, Row))
        ).

runtime_hashes() -> maps:get(hashes, runtime_identity()).

runtime_identity() ->
    Required = [
        imboy_cache,
        imboy_cache_policy,
        imboy_cache_sync,
        imboy_cache_sync_proto,
        imboy_cache_sync_snapshot
    ],
    Optional = [imboy_cache_sync_apply, imboy_cache_sync_recover, imboy_cache_sync_coverage],
    Present = [M || M <- Optional, code:which(M) =/= non_existing],
    true = lists:member(Present, [[], [imboy_cache_sync_apply, imboy_cache_sync_recover], Optional]),
    Modules = Required ++ Present ++ [elib_metric, depcache, syn, jsone],
    Objects = [
        begin
            {M, Beam, Path} = code:get_object_code(M),
            Path = code:which(M),
            {M, Beam, Path}
        end
     || M <- Modules
    ],
    CoreRoot = filename:dirname(code:which(imboy_cache)),
    true = lists:all(
        fun(M) -> filename:dirname(code:which(M)) =:= CoreRoot end, Required ++ Present
    ),
    #{
        core_modules => Required ++ Present,
        hashes => maps:from_list([
            {atom_to_binary(M), binary:encode_hex(crypto:hash(sha256, B), lowercase)}
         || {M, B, _} <- Objects
        ]),
        paths => maps:from_list([{atom_to_binary(M), list_to_binary(P)} || {M, _, P} <- Objects])
    }.

deps_on_path() ->
    case code:which(imboy_cache) of
        Path when is_list(Path) ->
            Root = filename:dirname(filename:dirname(filename:absname(Path))),
            [
                _ = code:add_patha(filename:join([Root, "deps", D, "ebin"]))
             || D <- ["jsone", "depcache", "syn"]
            ],
            ok;
        _ ->
            error(imboy_ebin_not_on_path)
    end.

%% Read only the registration written by actual version reports, never synthesize it.
members_confirmed() ->
    imboy_cache_sync:status() =:= ready andalso
        lists:all(
            fun({Pid, _}) ->
                case node(Pid) =:= node() of
                    true ->
                        true;
                    false ->
                        {Origin, Boot} = rpc:call(node(Pid), imboy_cache_sync, node_boot, [], 1000),
                        ets:lookup(imboy_cache_meta, {peer_incarnation, Origin}) =:=
                            [{{peer_incarnation, Origin}, Boot}]
                end
            end,
            syn:members(imboy_cache, dsync_handler)
        ).

stop_peer(Pid) ->
    try
        peer:stop(Pid)
    catch
        exit:noproc -> ok
    end.
