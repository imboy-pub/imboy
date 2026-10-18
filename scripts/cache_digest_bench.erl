%% Microbenchmark for fingerprint CPU cost and retained caller memory.
%% Run the same driver beam against original/candidate policy ebins. This is
%% not a cache throughput or distributed-convergence acceptance benchmark.
-module(cache_digest_bench).
-export([run/0]).

run() ->
    {ok, _} = application:ensure_all_started(crypto),
    Callers = list_to_integer(os:getenv("CACHE_DIGEST_CALLERS", "100")),
    Calls = list_to_integer(os:getenv("CACHE_DIGEST_CALLS", "1000")),
    true = Callers > 0 andalso Calls > 0,
    Rows = [profile(Name, Config, Callers, Calls) || {Name, Config} <- [
        {small, #{}},
        {below_cutoff, #{unused => lists:duplicate(3000, $x)}},
        {above_cutoff, #{unused => lists:duplicate(5000, $x)}}
    ]],
    ok = file:write_file(os:getenv("CACHE_DIGEST_OUTPUT", "/tmp/cache_digest.json"),
        jsone:encode(#{callers => Callers, calls_per_caller => Calls, profiles => Rows})),
    io:format("~p~n", [Rows]),
    halt(0).

profile(Name, Config, Callers, Calls) ->
    application:set_env(imboy, cache_replication, Config),
    application:set_env(imboy, cache_registry_overrides, #{}),
    application:set_env(imboy, cache_fenced_namespaces, []),
    BeforeMemory = erlang:memory(total),
    Parent = self(), Ref = make_ref(),
    Jobs = [spawn_monitor(fun() -> caller(Parent, Ref, Calls) end)
        || _ <- lists:seq(1, Callers)],
    Row = try
        Ready = collect(ready, Ref, Jobs, []),
        Start = erlang:monotonic_time(microsecond),
        [Pid ! {go, Ref} || {Pid, _} <- Jobs],
        Finished = collect(finished, Ref, Jobs, []),
        Wall = (erlang:monotonic_time(microsecond) - Start) / 1000000,
        #{profile => Name, config_serialized_bytes => erlang:external_size(Config),
          wall_seconds => Wall, digest_calls => Callers * Calls * 3,
          digest_calls_per_second => Callers * Calls * 3 / Wall,
          idle_caller_memory_bytes => lists:sum(Ready),
          retained_caller_memory_bytes => lists:sum(Finished),
          retained_caller_delta_bytes => lists:sum(Finished) - lists:sum(Ready)}
    after
        [exit(Pid, kill) || {Pid, _} <- Jobs],
        [receive {'DOWN', Monitor, process, Pid, _} -> ok
            after 10000 -> error({cleanup_timeout, Pid}) end || {Pid, Monitor} <- Jobs]
    end,
    erlang:garbage_collect(),
    Row#{vm_memory_before_callers => BeforeMemory,
        vm_memory_after_callers_exit => erlang:memory(total),
        callers_still_alive => length([P || {P, _} <- Jobs, is_process_alive(P)])}.

caller(Parent, Ref, Calls) ->
    erlang:garbage_collect(),
    Parent ! {ready, Ref, self(), memory()},
    receive {go, Ref} -> ok end,
    repeat(Calls),
    erlang:garbage_collect(),
    Parent ! {finished, Ref, self(), memory()},
    receive release -> ok end.

repeat(0) -> ok;
repeat(N) ->
    _ = imboy_cache_policy:budget_digest(),
    _ = imboy_cache_policy:config_hash(),
    _ = imboy_cache_policy:registry_hash(),
    repeat(N - 1).

memory() -> {memory, Bytes} = process_info(self(), memory), Bytes.
collect(_, _, [], Values) -> Values;
collect(Tag, Ref, Jobs, Values) ->
    receive
        {Tag, Ref, Pid, Value} ->
            collect(Tag, Ref, lists:keydelete(Pid, 1, Jobs), [Value | Values]);
        {'DOWN', _, process, Pid, Reason} -> error({caller_failed, Pid, Reason})
    after 120000 -> error({caller_timeout, Tag, length(Jobs)})
    end.
