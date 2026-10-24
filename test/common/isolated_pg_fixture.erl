-module(isolated_pg_fixture).
-export([run/1, run/2, run_case/1, test/1, test/2, setup_single_node_cache/2]).
-behaviour(eunit_listener).
-export([start/1, init/1, handle_begin/3, handle_end/3, handle_cancel/3, terminate/2]).

%% 整个生成器在子 VM 执行，不能把描述符返回父 VM 或当作普通结果丢弃。
%% Execute setup, tests and cleanup in the peer; require actual passing tests.
test(Generator) when is_function(Generator, 0) -> test(Generator, 900).

%% Seconds include peer boot/shutdown; existing callers keep the 900/880 budget.
test(Generator, Seconds) when is_function(Generator, 0), is_integer(Seconds), Seconds > 20 ->
    {timeout, Seconds, fun() ->
        {Result, Summary} = run(fun() -> run_tests(Generator) end, Seconds),
        {module, Module} = erlang:fun_info(Generator, module),
        {name, Name} = erlang:fun_info(Generator, name),
        io:format(user, "[isolated_pg_fixture] ~p:~p result=~p counts=~0p~n", [
            Module, Name, Result, Summary
        ]),
        case {Result, proplists:get_value(pass, Summary, 0)} of
            {ok, Passed} when Passed > 0 -> ok;
            _ -> error({isolated_eunit_failed, Result, Summary})
        end
    end}.

run_tests(Generator) ->
    Ref = make_ref(),
    Result = eunit:test(Generator(), [verbose, {report, {?MODULE, [{owner, {self(), Ref}}]}}]),
    receive
        {Ref, {ok, Summary}} -> {Result, Summary};
        {Ref, {error, Reason}} -> error({isolated_eunit_report_failed, Reason})
    after 1000 -> error(isolated_eunit_report_missing)
    end.

%% 原生缓存仅在明确拥有它的 standalone fixture 中使用单节点恢复屏障。
%% Only cache-owning standalone setups select single-node topology and await READY.
setup_single_node_cache(Setup, Cleanup) ->
    false = lists:keymember(imboy, 1, application:which_applications()),
    undefined = whereis(imboy_cache_sync),
    ok = application:set_env(imboy, cluster_nodes, []),
    ok = application:set_env(imboy, dsync_enabled, false),
    State = Setup(),
    try
        ok = await_cache_ready(erlang:monotonic_time(millisecond) + 5000),
        State
    catch
        Class:Reason:Stack ->
            Cleanup(State),
            erlang:raise(Class, Reason, Stack)
    end.

await_cache_ready(Deadline) ->
    case imboy_cache_sync:status() of
        ready ->
            ok;
        degraded ->
            error({standalone_cache_not_ready, degraded});
        Status ->
            Remaining = Deadline - erlang:monotonic_time(millisecond),
            case Remaining > 0 of
                true ->
                    timer:sleep(erlang:min(25, Remaining)),
                    await_cache_ready(Deadline);
                false ->
                    error({standalone_cache_not_ready, Status})
            end
    end.

%% EUnit owns the counts; the parent logs them even when its test output is captured.
start(Options) -> eunit_listener:start(?MODULE, Options).
init(Options) -> proplists:get_value(owner, Options).
handle_begin(_Type, _Data, State) -> State.
handle_end(_Type, _Data, State) -> State.
handle_cancel(_Type, _Data, State) -> State.
terminate(Result, {Owner, Ref}) ->
    Owner ! {Ref, Result},
    ok.

%% Each PG case owns a fresh BEAM: TSID and named pools cannot reuse App state.
run(TestFun) when is_function(TestFun, 0) -> run(TestFun, 900).

run(TestFun, Seconds) when is_function(TestFun, 0), is_integer(Seconds), Seconds > 20 ->
    Paths = [filename:absname(P) || P <- code:get_path()],
    ConfigArgs =
        case init:get_argument(config) of
            {ok, Groups} ->
                lists:append([["-config", filename:absname(P)] || Group <- Groups, P <- Group]);
            error ->
                []
        end,
    {ok, Peer, _} = peer:start_link(#{
        connection => standard_io,
        args => ["-no_dot_erlang", "-pa" | lists:reverse(Paths)] ++ ConfigArgs,
        wait_boot => 10000
    }),
    try
        %% Leave time for boot and shutdown inside the outer 900-second budget.
        peer:call(Peer, ?MODULE, run_case, [TestFun], (Seconds - 20) * 1000)
    after
        peer:stop(Peer)
    end.

run_case(TestFun) when is_function(TestFun, 0) ->
    %% Load the inherited -config values without starting imboy or its pools.
    case application:load(imboy) of
        ok -> ok;
        {error, {already_loaded, imboy}} -> ok
    end,
    TestFun().
