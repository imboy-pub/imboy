-module(cache_w0_bench).
-export([run/0, receiver/1, setup/0]).

run() ->
    Output = os:getenv("CACHE_W0_OUTPUT"),
    Operations = list_to_integer(os:getenv("CACHE_W0_OPERATIONS", "1000")),
    true = Operations > 0 andalso Operations =< 100000,
    setup(),
    Checks = self_check(),
    Local = spawn(?MODULE, receiver, [self()]),
    receive {ready, Local} -> ok after 10000 -> error(receiver_timeout) end,
    Single = [bench(Kind, Workers, Operations, [Local]) ||
        Kind <- [get, get_wait, set, mixed, memo, syn], Workers <- [1, 8, 32]],
    Three = three_nodes(Operations, Local),
    Result = #{otp => erlang:system_info(otp_release),
        schedulers => erlang:system_info(schedulers_online),
        wordsize_bytes => erlang:system_info(wordsize),
        operations_per_worker => Operations, keyspace => 1024, value_bytes => 256,
        ttl_seconds => 3600, queue_sample_ms => 5,
        self_checks => Checks, single_node => Single, three_node => Three},
    ok = file:write_file(Output, jsone:encode(Result)),
    Local ! stop,
    halt(0).

self_check() ->
    Cold = cold_memo_check(),
    Race = invalidation_race_check(),
    Exc = loader_exception_check(),
    Crash = writer_crash_check(),
    Handfill = handfill_get_wait_check(),
    maps:merge(maps:merge(maps:merge(Cold, Race), Exc),
               maps:merge(Crash, Handfill)).

%% depcache 原生单飞：同 Key 32 并发冷 miss 只允许一次 loader。
cold_memo_check() ->
    Parent = self(),
    Counter = atomics:new(1, []),
    Workers = [spawn(fun() ->
        receive go -> ok end,
        fresh = depcache:memo(fun() ->
            atomics:add(Counter, 1, 1), timer:sleep(100), fresh
        end, cold_memo, 60, cache_w0_data),
        Parent ! {checked, self()}
    end) || _ <- lists:seq(1, 32)],
    [Worker ! go || Worker <- Workers],
    [receive {checked, Worker} -> ok after 10000 -> error(memo_check_timeout) end ||
        Worker <- Workers],
    1 = atomics:get(Counter, 1),
    #{cold_memo_workers => 32, loader_executions => 1}.

%% 缺陷复现：flush 不与在途 loader 协调（deps/depcache/src/depcache.erl:1301-1305
%% 只删 ETS 与进程字典，不触碰 wait_pids），旧 loader 完成后 set 回填 stale，
%% 且 join 过等待队列的 memo 等待者也经 handle_call_set 回复收到 stale 值。
invalidation_race_check() ->
    Parent = self(),
    Loader = spawn(fun() ->
        stale = depcache:memo(fun() ->
            Parent ! loading, receive continue -> stale end
        end, invalidation_race, 60, cache_w0_data),
        Parent ! finished
    end),
    receive loading -> ok after 10000 -> error(loader_timeout) end,
    Waiter = spawn(fun() ->
        Result = depcache:memo(fun() -> waiter_must_not_run end,
                               invalidation_race, 60, cache_w0_data),
        Parent ! {waiter_result, self(), Result}
    end),
    timer:sleep(200),
    ok = depcache:flush(invalidation_race, cache_w0_data),
    Loader ! continue,
    receive finished -> ok after 10000 -> error(loader_finish_timeout) end,
    receive {waiter_result, Waiter, stale} -> ok
    after 10000 -> error(waiter_result_timeout) end,
    {ok, stale} = depcache:get(invalidation_race, cache_w0_data),
    #{stale_refill_after_flush => reproduced,
      stale_waiter_value_after_flush => stale}.

%% loader 异常：memo_key 捕获后向等待者回 {throw,R} 并重抛
%% （depcache.erl:345-348 memo_send_errors / memo:306-307 rethrow）。
loader_exception_check() ->
    Parent = self(),
    Ball = w0_demo_ball,
    LoaderFun = fun() -> Parent ! loading, receive continue -> erlang:throw(Ball) end end,
    Loader = spawn(fun() ->
        Result = try depcache:memo(LoaderFun, exception_race, 60, cache_w0_data),
                       no_throw
                 catch throw:Ball -> rethrown_ok end,
        Parent ! {exc_loader, self(), Result}
    end),
    receive loading -> ok after 10000 -> error(exc_loader_timeout) end,
    Waiters = [spawn(fun() ->
        Result = try depcache:memo(LoaderFun, exception_race, 60, cache_w0_data),
                       no_throw
                 catch throw:Ball -> rethrown_ok end,
        Parent ! {exc_waiter, self(), Result}
    end) || _ <- lists:seq(1, 3)],
    timer:sleep(200),
    Loader ! continue,
    [receive {exc_waiter, Waiter, rethrown_ok} -> ok
     after 10000 -> error({exc_waiter_timeout, Waiter}) end || Waiter <- Waiters],
    receive {exc_loader, Loader, rethrown_ok} -> ok
    after 10000 -> error(exc_loader_result_timeout) end,
    undefined = depcache:get(exception_race, cache_w0_data),
    #{loader_exception_waiters => 3, loader_exception_reraised => true,
      exception_key_after_failure => undefined}.

%% writer 崩溃：手工 get_wait 模式 writer 死亡后，depcache 经 'DOWN'
%% 向等待者回 {error, premature_exit}（depcache.erl:922-935）；
%% memo 等待者在 premature_exit 后重试并自跑 loader（depcache.erl:310-311）。
writer_crash_check() ->
    Parent = self(),
    Writer = spawn(fun() ->
        undefined = depcache:get_wait(crash_race, cache_w0_data),
        receive die -> ok end
    end),
    timer:sleep(200),
    Waiter = spawn(fun() ->
        Result = case depcache:get_wait(crash_race, cache_w0_data) of
            {error, premature_exit} -> premature_exit_ok;
            Other -> {unexpected, Other}
        end,
        Parent ! {crash_waiter, self(), Result}
    end),
    timer:sleep(200),
    RetryCounter = atomics:new(2, []),
    RetryMemo = spawn(fun() ->
        Value = depcache:memo(fun() ->
            atomics:add(RetryCounter, 1, 1), retried_fresh
        end, crash_race, 60, cache_w0_data),
        Parent ! {crash_memo, self(), Value}
    end),
    timer:sleep(200),
    exit(Writer, kill),
    receive {crash_waiter, Waiter, premature_exit_ok} -> ok
    after 10000 -> error(crash_waiter_timeout) end,
    receive {crash_memo, RetryMemo, retried_fresh} -> ok
    after 10000 -> error(crash_memo_timeout) end,
    1 = atomics:get(RetryCounter, 1),
    #{premature_exit_waiter => ok, memo_retry_after_premature_exit => retried_fresh,
      memo_retry_loaders => 1}.

%% 手工 get_wait 语义：miss 时首个调用者收到 undefined 并成为被监控的 writer，
%% 并发等待者在 writer set 后收到 {ok, Value}（depcache.erl:991-1025, 1130-1142）。
%% 该 API 的 writer 身份不是原子领取权（undefined 即丢失 ownership），仅记录现状。
handfill_get_wait_check() ->
    Parent = self(),
    undefined = depcache:get_wait(handfill_race, cache_w0_data),
    Waiter = spawn(fun() ->
        Result = depcache:get_wait(handfill_race, cache_w0_data),
        Parent ! {handfill_waiter, self(), Result}
    end),
    timer:sleep(200),
    ok = depcache:set(handfill_race, handfilled, 60, cache_w0_data),
    receive {handfill_waiter, Waiter, {ok, handfilled}} -> ok
    after 10000 -> error(handfill_waiter_timeout) end,
    #{first_caller_becomes_writer => undefined,
      waiter_value_after_handfill_set => handfilled}.

setup() ->
    {ok, _} = application:ensure_all_started(syn),
    ok = syn:add_node_to_scopes([cache_w0]),
    {ok, _} = depcache:start_link(cache_w0_data, #{memory_max => 64}),
    Value = binary:copy(<<"x">>, 256),
    [ok = depcache:set({w0, Index}, Value, 3600, cache_w0_data) ||
        Index <- lists:seq(1, 1024)],
    ok.

receiver(Parent) ->
    ok = syn:join(cache_w0, cache_w0_group, self()),
    Parent ! {ready, self()},
    receive_loop().

receive_loop() ->
    receive
        {write, Sender, Reference, Key, Value} ->
            ok = depcache:set(Key, Value, 3600, cache_w0_data),
            Sender ! {applied, Reference, self()},
            receive_loop();
        stop -> ok
    end.

three_nodes(Operations, Local) ->
    Paths = ["-setcookie", atom_to_list(erlang:get_cookie()),
        "-kernel", "inet_dist_use_interface", "{127,0,0,1}"] ++
        lists:append([["-pa", Path] || Path <- code:get_path(),
            filename:pathtype(Path) =:= absolute]),
    try
        {ok, PeerOne, NodeOne} = peer:start_link(#{name => peer:random_name("cache_w0_one"),
            host => "127.0.0.1", longnames => true, connection => standard_io,
            args => Paths}),
        try
            {ok, PeerTwo, NodeTwo} = peer:start_link(#{name => peer:random_name("cache_w0_two"),
                host => "127.0.0.1", longnames => true, connection => standard_io,
                args => Paths}),
            try
                true = net_adm:ping(NodeOne) =:= pong,
                true = net_adm:ping(NodeTwo) =:= pong,
                true = peer:call(PeerOne, net_kernel, connect_node, [NodeTwo]),
                ok = peer:call(PeerOne, ?MODULE, setup, []),
                ok = peer:call(PeerTwo, ?MODULE, setup, []),
                RemoteOne = spawn(NodeOne, ?MODULE, receiver, [self()]),
                RemoteTwo = spawn(NodeTwo, ?MODULE, receiver, [self()]),
                await_ready([RemoteOne, RemoteTwo]),
                await_members(3, 100),
                #{status => measured, nodes => 3,
                    results => [bench(syn, Workers, Operations,
                        [Local, RemoteOne, RemoteTwo]) || Workers <- [1, 8, 32]]}
            after peer:stop(PeerTwo) end
        after peer:stop(PeerOne) end
    catch Class:Reason:Stack ->
        #{status => blocked, reason => iolist_to_binary(io_lib:format("~p:~p", [Class, Reason])),
            stack => iolist_to_binary(io_lib:format("~p", [Stack]))}
    end.

await_ready([]) -> ok;
await_ready([Receiver | Rest]) ->
    receive {ready, Receiver} -> await_ready(Rest)
    after 10000 -> error(remote_receiver_timeout) end.

await_members(Count, Remaining) when Remaining > 0 ->
    case length(syn:members(cache_w0, cache_w0_group)) of
        Count -> ok;
        _ -> timer:sleep(50), await_members(Count, Remaining - 1)
    end;
await_members(_, _) -> error(membership_timeout).

bench(Kind, Workers, Operations, Receivers) ->
    Parent = self(),
    Monitor = spawn(fun() -> sample(Parent, Receivers, 0, 0) end),
    Start = erlang:monotonic_time(microsecond),
    Processes = [spawn_monitor(fun() ->
        Value = binary:copy(<<"x">>, 256),
        Samples = [operation(Kind, Index, Worker, Value, Receivers) ||
            Index <- lists:seq(1, Operations)],
        Parent ! {samples, self(), Samples}
    end) || Worker <- lists:seq(1, Workers)],
    Samples = collect(Processes, []),
    Elapsed = erlang:monotonic_time(microsecond) - Start,
    Monitor ! stop,
    {Queue, PeakMemory} = receive {metrics, Monitor, Metrics} -> Metrics
        after 10000 -> error(metrics_timeout) end,
    Latencies = lists:sort([Latency || {Latency, _} <- Samples]),
    Errors = length([error || {_, error} <- Samples]),
    #{kind => Kind, workers => Workers, operations => length(Samples),
        throughput_ops_sec => length(Samples) * 1000000 / max(Elapsed, 1),
        p50_us => percentile(Latencies, 0.50), p95_us => percentile(Latencies, 0.95),
        p99_us => percentile(Latencies, 0.99), errors => Errors,
        error_rate => Errors / length(Samples), elapsed_us => Elapsed,
        sampled_queue_max => Queue, sampled_beam_memory_peak_bytes => PeakMemory,
        beam_memory_end_bytes => erlang:memory(total),
        cache_ets_words => depcache:size(cache_w0_data),
        cache_ets_bytes => depcache:size(cache_w0_data) * erlang:system_info(wordsize),
        node_memory_end => [#{node => Node, bytes => rpc:call(Node, erlang, memory, [total])} ||
            Node <- lists:usort([node(Receiver) || Receiver <- Receivers])],
        receiver_count => length(Receivers)}.

operation(Kind, Index, Worker, Value, Receivers) ->
    Key = {w0, (Index + Worker) rem 1024 + 1},
    Start = erlang:monotonic_time(microsecond),
    Status = try
        case Kind of
            get -> {ok, Value} = depcache:get(Key, cache_w0_data);
            get_wait -> {ok, Value} = depcache:get_wait(Key, cache_w0_data);
            set -> ok = depcache:set(Key, Value, 3600, cache_w0_data);
            mixed when Index rem 10 =:= 0 ->
                ok = depcache:set(Key, Value, 3600, cache_w0_data);
            mixed -> {ok, Value} = depcache:get(Key, cache_w0_data);
            memo -> Value = depcache:memo(fun() -> Value end, Key, 3600, cache_w0_data);
            syn ->
                Reference = make_ref(),
                {ok, Count} = syn:publish(cache_w0, cache_w0_group,
                    {write, self(), Reference, Key, Value}),
                true = Count =:= length(Receivers),
                await_applied(Reference, Receivers)
        end,
        ok
    catch _:_ -> error end,
    {erlang:monotonic_time(microsecond) - Start, Status}.

await_applied(_, []) -> ok;
await_applied(Reference, [Receiver | Rest]) ->
    receive {applied, Reference, Receiver} -> await_applied(Reference, Rest)
    after 5000 -> error(apply_timeout) end.

collect([], Accumulator) -> lists:append(Accumulator);
collect([{Process, Reference} | Rest], Accumulator) ->
    receive
        {samples, Process, Samples} ->
            receive {'DOWN', Reference, process, Process, normal} -> ok end,
            collect(Rest, [Samples | Accumulator]);
        {'DOWN', Reference, process, Process, Reason} -> error({worker_failed, Reason})
    after 120000 -> error(worker_timeout) end.

sample(Parent, Receivers, QueueMax, MemoryMax) ->
    CacheProcesses = [rpc:call(Node, erlang, whereis, [cache_w0_data]) ||
        Node <- lists:usort([node(Receiver) || Receiver <- Receivers])],
    Queue = lists:sum([queue(Process) || Process <- Receivers ++ CacheProcesses]),
    Memory = erlang:memory(total),
    receive stop -> Parent ! {metrics, self(), {max(Queue, QueueMax), max(Memory, MemoryMax)}}
    after 5 -> sample(Parent, Receivers, max(Queue, QueueMax), max(Memory, MemoryMax)) end.

queue(Process) when node(Process) =:= node() ->
    case process_info(Process, message_queue_len) of
        {message_queue_len, Count} -> Count;
        undefined -> 0
    end;
queue(Process) ->
    case rpc:call(node(Process), erlang, process_info, [Process, message_queue_len], 1000) of
        {message_queue_len, Count} -> Count;
        _ -> 0
    end.

percentile(Values, Fraction) ->
    lists:nth(max(1, ceil(length(Values) * Fraction)), Values).
