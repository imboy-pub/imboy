-module(imboy_router_registry_bench_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% P2-V2: 路由表 ETS 读延迟基准测试
%%%
%%% 验收标准：plugin_routes/1 单次 lookup P99 < 10μs（测试环境宽松阈值）
%%% 生产环境 read_concurrency=true + decentralized_counters 更优
%%%
%%% 覆盖 / Coverage:
%%%   1. 单次 ets:lookup P99 < 10μs
%%%   2. all_routes/0 全扫描正确
%%%   3. 多进程并发读取不崩溃
%%% @end
%%%-------------------------------------------------------------------

-define(WARMUP_RUNS, 1000).
-define(BENCH_RUNS, 10000).

setup() ->
    {ok, Pid} = imboy_router_registry:start_link(),
    Plugins = [channel, moment, location, group_collab],
    lists:foreach(fun(Plugin) ->
        Routes = [make_route(Plugin, N) || N <- lists:seq(1, 5)],
        ok = imboy_router_registry:register(Plugin, Routes)
    end, Plugins),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true -> unlink(Pid), gen_server:stop(Pid);
        false -> ok
    end.

make_route(Plugin, N) ->
    Path = list_to_binary(
        "/v1/" ++ atom_to_list(Plugin) ++ "/action" ++ integer_to_list(N)
    ),
    #{
        method => <<"GET">>,
        path => Path,
        handler => list_to_atom(atom_to_list(Plugin) ++ "_handler"),
        action => list
    }.

%% ===================================================================
%% 1. 单次 plugin_routes/1 lookup P99 < 10μs
%% ===================================================================

single_lookup_latency_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_Pid) ->
         ?_test(begin
             lists:foreach(fun(_) ->
                 imboy_router_registry:plugin_routes(channel)
             end, lists:seq(1, ?WARMUP_RUNS)),

             Times = [begin
                 T1 = erlang:monotonic_time(nanosecond),
                 _ = imboy_router_registry:plugin_routes(channel),
                 T2 = erlang:monotonic_time(nanosecond),
                 T2 - T1
             end || _ <- lists:seq(1, ?BENCH_RUNS)],

             Sorted = lists:sort(Times),
             P99 = lists:nth(round(0.99 * ?BENCH_RUNS), Sorted),

             ?assert(P99 < 10000,
                     io_lib:format("P99 ~p ns too slow (threshold 10us)", [P99]))
         end)
     end}.

%% ===================================================================
%% 2. all_routes/0 全扫描正确
%% ===================================================================

all_routes_correct_count_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_Pid) ->
         ?_test(begin
             All = imboy_router_registry:all_routes(),
             ?assertEqual(20, length(All))
         end)
     end}.

%% ===================================================================
%% 3. 多进程并发读取不崩溃
%% ===================================================================

concurrent_read_no_crash_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_Pid) ->
         ?_test(begin
             Self = self(),
             N = 10,
             _Pids = [spawn_monitor(fun() ->
                 lists:foreach(fun(_) ->
                     _ = imboy_router_registry:plugin_routes(channel),
                     _ = imboy_router_registry:all_routes(),
                     _ = imboy_router_registry:plugin_names()
                 end, lists:seq(1, 1000)),
                 Self ! {done, self()}
             end) || _ <- lists:seq(1, N)],

             Done = [receive {done, P} -> P after 5000 -> timeout end
                     || _ <- lists:seq(1, N)],

             ?assertEqual(N, length([P || P <- Done, P =/= timeout]))
         end)
     end}.
