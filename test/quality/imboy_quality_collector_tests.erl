%% 合成 collector 协议测试；不启动产品资源或长时测试。
-module(imboy_quality_collector_tests).
-include_lib("eunit/include/eunit.hrl").
-export([start/1, init/1, handle_begin/3, handle_end/3, handle_cancel/3, terminate/2]).

collector_protocol_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(#{dir := Dir}) ->
        [
            fun idle_then_real_terminal/0,
            fun terminal_then_stop/0,
            fun stop_before_terminal/0,
            fun concurrent_senders/0,
            fun collector_down_fails/0,
            fun missing_terminal_fails/0,
            fun incomplete_events_fail/0,
            fun duplicate_and_late_events_fail/0,
            fun empty_auxiliary_root_is_valid/0,
            fun listener_counts_mismatch_fails/0,
            fun real_root_cancel/0,
            {timeout, 15, fun large_event_stream/0},
            fun failed_listener_fails_single_and_group/0,
            fun actual_single_and_group/0,
            %% 两次 peer 启动、RPC（组模式另有环境探针）及回收均须留余量。
            {timeout, 120, fun() -> actual_peer_single_and_group(Dir) end}
        ]
    end}.

setup() ->
    Repo =
        case os:getenv("IMBOY_QUALITY_REPO") of
            false -> element(2, file:get_cwd());
            Path -> Path
        end,
    Tmp =
        case os:getenv("TMPDIR") of
            false -> "/tmp";
            P -> P
        end,
    Dir = filename:join(
        Tmp,
        "imboy-collector-" ++ os:getpid() ++ "-" ++
            integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:make_dir(Dir),
    Fixtures = [
        quality_fixture_pass_tests, quality_fixture_fail_tests, quality_fixture_cancel_tests
    ],
    Context = #{dir => Dir},
    try
        Base = filename:join(Repo, "tools/imboy_quality"),
        Files =
            [
                filename:join(Base, atom_to_list(M) ++ ".erl")
             || M <-
                    [imboy_quality_runner, imboy_quality_listener]
            ] ++
                [
                    filename:join([Base, "test_fixtures", atom_to_list(M) ++ ".erl"])
                 || M <- Fixtures
                ],
        [compile_source(F, Dir) || F <- Files],
        true = code:add_patha(Dir),
        RunnerSource = filename:join(Base, "imboy_quality_runner.erl"),
        {ok, Forms0} = epp:parse_file(RunnerSource, [], []),
        load_alias(imboy_quality_runner_isolated, [alias_form(F) || F <- Forms0], Dir),
        load_alias(imboy_quality_runner_probe, [probe_form(F) || F <- Forms0], Dir),
        {ok, ListenerForms} = epp:parse_file(
            filename:join(Base, "imboy_quality_listener.erl"), [], []
        ),
        load_alias(imboy_quality_listener_isolated, [listener_alias(F) || F <- ListenerForms], Dir),
        [
            begin
                Source = filename:join([Base, "test_fixtures", atom_to_list(M) ++ ".erl"]),
                {ok, FForms} = epp:parse_file(Source, [], []),
                load_alias(fixture_alias(M), [fixture_form(F, M) || F <- FForms], Dir)
            end
         || M <- Fixtures
        ],
        Context
    catch
        Class:Reason:Stack ->
            cleanup(Context),
            erlang:raise(Class, Reason, Stack)
    end.

compile_source(File, Dir) ->
    case compile:file(File, [{outdir, Dir}, return_errors]) of
        {ok, _} -> ok;
        {error, Errors, Warnings} -> error({compile_failed, File, Errors, Warnings})
    end.

load_alias(Module, Forms, Dir) ->
    {ok, Module, Bin} = compile:forms(Forms, [binary, export_all, nowarn_export_all]),
    {module, Module} = code:load_binary(Module, filename:join(Dir, atom_to_list(Module)), Bin),
    ok.

alias_form({attribute, Line, module, imboy_quality_runner}) ->
    {attribute, Line, module, imboy_quality_runner_isolated};
alias_form(Form) ->
    replace_listener(Form).

%% ?MODULE 展开后的 peer RPC 仍指向真正 runner；仅 framework_path 的
%% code:which 查询改为别名，确保 peer 只使用本次编译的干净目录。
replace_listener(
    {call, L, {remote, R, {atom, A, code}, {atom, B, which}}, [{atom, C, imboy_quality_runner}]}
) ->
    {call, L, {remote, R, {atom, A, code}, {atom, B, which}}, [
        {atom, C, imboy_quality_runner_isolated}
    ]};
replace_listener({atom, Line, imboy_quality_listener}) ->
    {atom, Line, imboy_quality_listener_isolated};
replace_listener(T) when is_tuple(T) ->
    list_to_tuple([replace_listener(X) || X <- tuple_to_list(T)]);
replace_listener(L) when is_list(L) -> [replace_listener(X) || X <- L];
replace_listener(X) ->
    X.

listener_alias({attribute, Line, module, imboy_quality_listener}) ->
    {attribute, Line, module, imboy_quality_listener_isolated};
listener_alias(Form) ->
    Form.

fixture_alias(quality_fixture_pass_tests) -> quality_collector_pass_tests;
fixture_alias(quality_fixture_fail_tests) -> quality_collector_fail_tests;
fixture_alias(quality_fixture_cancel_tests) -> quality_collector_cancel_tests.

fixture_form({attribute, Line, module, Module}, Module) ->
    {attribute, Line, module, fixture_alias(Module)};
fixture_form(Form, _) ->
    Form.

probe_form({attribute, Line, module, imboy_quality_runner}) ->
    {attribute, Line, module, imboy_quality_runner_probe};
probe_form({function, Line, collect, 3, Clauses}) ->
    {function, Line, collect, 3, replace_integer(Clauses, 30000, 20)};
probe_form({function, Line, worker_main, 2, Clauses}) ->
    {function, Line, worker_main, 2, failing_listener(Clauses)};
probe_form(Form) ->
    Form.

failing_listener({atom, Line, imboy_quality_listener}) ->
    {atom, Line, imboy_quality_collector_tests};
failing_listener({integer, Line, 10000}) ->
    {integer, Line, 30};
failing_listener(T) when is_tuple(T) ->
    list_to_tuple([failing_listener(X) || X <- tuple_to_list(T)]);
failing_listener(L) when is_list(L) -> [failing_listener(X) || X <- L];
failing_listener(X) ->
    X.

replace_integer({integer, Line, Old}, Old, New) ->
    {integer, Line, New};
replace_integer(T, Old, New) when is_tuple(T) ->
    list_to_tuple([replace_integer(X, Old, New) || X <- tuple_to_list(T)]);
replace_integer(L, Old, New) when is_list(L) -> [replace_integer(X, Old, New) || X <- L];
replace_integer(X, _, _) ->
    X.

start_collector() ->
    Parent = self(),
    Ref = make_ref(),
    {Pid, Monitor} = spawn_monitor(fun() ->
        imboy_quality_runner_probe:collect(Parent, Ref, [])
    end),
    {Pid, Monitor, Ref}.

stop_collector(Pid, Monitor) ->
    exit(Pid, kill),
    receive
        {'DOWN', Monitor, process, Pid, _} -> ok
    after 1000 -> error(collector_leak)
    end.

idle_then_real_terminal() ->
    {Pid, Monitor, Ref} = start_collector(),
    try
        Pid ! {test_begin, [1]},
        timer:sleep(80),
        ?assert(is_process_alive(Pid)),
        Pid ! {test_end, [1], ok, 42},
        Pid ! {group_end, [], 42},
        Pid ! result(1, 0),
        Pid ! {stop, self(), Ref},
        {ok, Counts, Events} = imboy_quality_runner_probe:await_collector(Pid, Monitor, Ref, 1000),
        ?assertEqual(1, maps:get(pass, Counts)),
        ?assertEqual(ok, imboy_quality_runner_probe:validate_events(Events, Counts))
    after
        stop_collector(Pid, Monitor)
    end.

terminal_then_stop() ->
    {Pid, Monitor, Ref} = start_collector(),
    try
        Pid ! {group_end, [], 0},
        Pid ! result(0, 0),
        timer:sleep(20),
        ?assert(is_process_alive(Pid)),
        Pid ! {stop, self(), Ref},
        ?assertMatch(
            {ok, _, _}, imboy_quality_runner_probe:await_collector(Pid, Monitor, Ref, 1000)
        )
    after
        stop_collector(Pid, Monitor)
    end.

stop_before_terminal() ->
    {Pid, Monitor, Ref} = start_collector(),
    try
        Pid ! {stop, self(), Ref},
        Pid ! {group_end, [], 0},
        timer:sleep(30),
        ?assert(is_process_alive(Pid)),
        Pid ! result(0, 0),
        ?assertMatch(
            {ok, _, _}, imboy_quality_runner_probe:await_collector(Pid, Monitor, Ref, 1000)
        )
    after
        stop_collector(Pid, Monitor)
    end.

%% 模拟 stop 与 listener 从不同发送方并发到达，覆盖两种受控顺序。
concurrent_senders() ->
    [concurrent_case(Order) || Order <- [stop_first, terminal_first]].

concurrent_case(Order) ->
    {Pid, Monitor, Ref} = start_collector(),
    Parent = self(),
    {Stopper, StopMonitor} = spawn_monitor(fun() ->
        receive
            go ->
                Pid ! {stop, Parent, Ref},
                Parent ! stop_sent
        end
    end),
    {Listener, ListenerMonitor} = spawn_monitor(fun() ->
        receive
            go ->
                Pid ! {test_begin, [1]},
                Pid ! {test_end, [1], ok, 0},
                Pid ! {group_end, [], 0},
                Pid ! result(1, 0),
                Parent ! terminal_sent
        end
    end),
    try
        case Order of
            stop_first ->
                Stopper ! go,
                receive
                    stop_sent -> ok
                after 1000 -> error(stop_timeout)
                end,
                Listener ! go,
                receive
                    terminal_sent -> ok
                after 1000 -> error(terminal_timeout)
                end;
            terminal_first ->
                Listener ! go,
                receive
                    terminal_sent -> ok
                after 1000 -> error(terminal_timeout)
                end,
                Stopper ! go,
                receive
                    stop_sent -> ok
                after 1000 -> error(stop_timeout)
                end
        end,
        {ok, Counts, Events} = imboy_quality_runner_probe:await_collector(Pid, Monitor, Ref, 1000),
        ?assertEqual(1, maps:get(pass, Counts)),
        ?assertEqual(ok, imboy_quality_runner_probe:validate_events(Events, Counts))
    after
        stop_collector(Stopper, StopMonitor),
        stop_collector(Listener, ListenerMonitor),
        stop_collector(Pid, Monitor)
    end.

collector_down_fails() ->
    {Pid, Monitor} = spawn_monitor(fun() -> exit(synthetic_failure) end),
    ?assertEqual(
        {error, {collector_down, synthetic_failure}},
        imboy_quality_runner_probe:await_collector(Pid, Monitor, make_ref(), 1000)
    ).

missing_terminal_fails() ->
    ?assertMatch(
        {error, _}, imboy_quality_runner_probe:validate_events([{group_end, [], 0}], zero())
    ),
    {Pid, Monitor, Ref} = start_collector(),
    try
        Pid ! {stop, self(), Ref},
        Pid ! {group_end, [], 0},
        ?assertEqual(
            {error, collector_missing_terminal},
            imboy_quality_runner_probe:await_collector(Pid, Monitor, Ref, 30)
        )
    after
        stop_collector(Pid, Monitor)
    end.

incomplete_events_fail() ->
    ?assertMatch({error, _}, validate([{test_begin, [1]}, {group_end, [], 0}, result(0, 0)])),
    ?assertMatch({error, _}, validate([result(0, 0)])),
    ?assertMatch({error, _}, validate([{test_end, [1], ok, 0}, {group_end, [], 0}, result(1, 0)])),
    ?assertMatch({error, _}, validate([{group_end, [], 0}, {group_end, [], 0}, result(0, 0)])).

duplicate_and_late_events_fail() ->
    Leaf = [{test_begin, [1]}, {test_end, [1], ok, 0}],
    ?assertMatch({error, _}, validate(Leaf ++ Leaf ++ [{group_end, [], 0}, result(2, 0)])),
    ?assertMatch({error, _}, validate([{group_end, [], 0}] ++ Leaf ++ [result(1, 0)])),
    ?assertMatch(
        {error, _}, validate([{test_cancel, group, [], aborted}] ++ Leaf ++ [result(1, 1)])
    ),
    ?assertMatch({error, _}, validate([{group_end, [], 0}, {group_end, [1], 0}, result(0, 0)])).

empty_auxiliary_root_is_valid() ->
    ?assertEqual(ok, validate([{group_end, [], 0}, result(0, 0)])).

listener_counts_mismatch_fails() ->
    ?assertMatch({error, _}, validate([{group_end, [], 0}, result(1, 0)])).

real_root_cancel() ->
    Parent = self(),
    Pid = imboy_quality_listener_isolated:start([{collector, Parent}]),
    Monitor = monitor(process, Pid),
    Pid ! {status, [], {progress, 'begin', {group, []}}},
    Pid ! {status, [], {cancel, synthetic_root_cancel}},
    Events = receive_events(Monitor, Pid, []),
    ?assertMatch([{test_cancel, group, [], synthetic_root_cancel}, {result, {ok, _}}], Events),
    ?assertEqual(ok, validate(Events)),
    Counts = imboy_quality_runner_probe:counts(Events, zero()),
    ?assertEqual(1, maps:get(cancel, Counts)),
    ?assertEqual(
        test_failed,
        maps:get(
            exit,
            imboy_quality_runner_probe:finish(error, Counts, Events, 0, node())
        )
    ).

receive_events(Monitor, Pid, Acc) ->
    receive
        {'DOWN', Monitor, process, Pid, normal} -> lists:reverse(Acc);
        Event -> receive_events(Monitor, Pid, [Event | Acc])
    after 1000 ->
        exit(Pid, kill),
        error(listener_timeout)
    end.

large_event_stream() ->
    {Pid, Monitor, Ref} = start_collector(),
    N = 20000,
    try
        Pid ! {stop, self(), Ref},
        [
            begin
                Pid ! {test_begin, [I]},
                Pid ! {test_end, [I], ok, 0}
            end
         || I <- lists:seq(1, N)
        ],
        Pid ! {group_end, [], 0},
        Pid ! result(N, 0),
        {ok, Counts, Events} = imboy_quality_runner_probe:await_collector(Pid, Monitor, Ref, 10000),
        ?assertEqual(N, maps:get(pass, Counts)),
        ?assertEqual(ok, imboy_quality_runner_probe:validate_events(Events, Counts))
    after
        stop_collector(Pid, Monitor)
    end.

%% 注入的 listener 通知测试进程，证明失败路径确实执行了回调。
start(Opts) -> eunit_listener:start(?MODULE, Opts).
init(_) -> persistent_term:get({?MODULE, target}).
handle_begin(test, _, Target) ->
    Target ! fault_callback_invoked,
    error(synthetic_listener_failure);
handle_begin(group, _, State) ->
    State.
handle_end(_, _, State) -> State.
handle_cancel(_, _, State) -> State.
terminate(_, State) -> State.

failed_listener_fails_single_and_group() ->
    persistent_term:put({?MODULE, target}, self()),
    try
        ?assertMatch(
            {{error, collector_missing_terminal}, _, _},
            imboy_quality_runner_probe:worker_main(quality_collector_pass_tests)
        ),
        receive
            fault_callback_invoked -> ok
        after 1000 -> error(callback_not_invoked)
        end,
        Group = imboy_quality_runner_probe:worker_main_group([quality_collector_pass_tests]),
        receive
            fault_callback_invoked -> ok
        after 1000 -> error(callback_not_invoked)
        end,
        ?assertEqual(test_failed, maps:get(exit, maps:get(quality_collector_pass_tests, Group)))
    after
        persistent_term:erase({?MODULE, target})
    end.

actual_single_and_group() ->
    {ok, Counts, Events} = imboy_quality_runner_isolated:worker_main(quality_collector_pass_tests),
    ?assertEqual(1, maps:get(pass, Counts)),
    ?assertMatch({result, {ok, _}}, lists:last(Events)),
    {ok, EmptyCounts, _} = imboy_quality_runner_isolated:worker_main(
        imboy_quality_listener_isolated
    ),
    ?assertEqual(zero(), EmptyCounts),
    {CancelRet, CancelCounts, CancelEvents} = imboy_quality_runner_isolated:worker_main(
        quality_collector_cancel_tests
    ),
    ?assertEqual(error, CancelRet),
    ?assert(maps:get(cancel, CancelCounts) > 0),
    ?assertEqual(ok, validate(CancelEvents)),
    Group = imboy_quality_runner_isolated:worker_main_group([
        quality_collector_pass_tests,
        quality_collector_fail_tests,
        quality_collector_cancel_tests,
        imboy_quality_listener_isolated
    ]),
    ?assertEqual(ok, maps:get(exit, maps:get(quality_collector_pass_tests, Group))),
    ?assertEqual(test_failed, maps:get(exit, maps:get(quality_collector_fail_tests, Group))),
    ?assertEqual(test_failed, maps:get(exit, maps:get(quality_collector_cancel_tests, Group))),
    ?assertEqual(ok, maps:get(exit, maps:get(imboy_quality_listener_isolated, Group))).

actual_peer_single_and_group(Dir) ->
    ?assert(filelib:is_regular(filename:join(Dir, "quality_fixture_pass_tests.beam"))),
    ?assert(filelib:is_regular(filename:join(Dir, "imboy_quality_runner.beam"))),
    Single = imboy_quality_runner_isolated:run_in_worker(
        quality_fixture_pass_tests, #{code_path => Dir, schedulers => 2}, 10000
    ),
    ?assertEqual(ok, maps:get(exit, Single)),
    ?assertEqual(1, maps:get(pass, maps:get(counts, Single))),
    ?assertMatch({result, {ok, _}}, lists:last(maps:get(events, Single))),
    Group = imboy_quality_runner_isolated:run_group_in_worker(
        [quality_fixture_pass_tests, quality_fixture_fail_tests],
        #{schedulers => 2},
        10000,
        [Dir]
    ),
    Roots = maps:get(per_root, Group),
    ?assertEqual(ok, maps:get(exit, maps:get(quality_fixture_pass_tests, Roots))),
    ?assertEqual(test_failed, maps:get(exit, maps:get(quality_fixture_fail_tests, Roots))),
    ?assertEqual([], nodes()).

result(Pass, Cancel) ->
    {result, {ok, [{pass, Pass}, {fail, 0}, {skip, 0}, {cancel, Cancel}]}}.
zero() -> #{pass => 0, fail => 0, skip => 0, cancel => 0}.
validate(Events) ->
    imboy_quality_runner_probe:validate_events(
        Events, imboy_quality_runner_probe:counts(Events, zero())
    ).

cleanup(#{dir := Dir}) ->
    [
        begin
            code:delete(M),
            code:purge(M)
        end
     || M <- [
            imboy_quality_runner_probe,
            imboy_quality_runner_isolated,
            imboy_quality_listener_isolated,
            quality_collector_pass_tests,
            quality_collector_fail_tests,
            quality_collector_cancel_tests
        ]
    ],
    code:del_path(Dir),
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok
    end.
