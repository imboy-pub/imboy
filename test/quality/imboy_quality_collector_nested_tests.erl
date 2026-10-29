%% 从真实 runner 嵌套执行 collector 测试，防止测试 cleanup 杀死外层 worker。
-module(imboy_quality_collector_nested_tests).
-include_lib("eunit/include/eunit.hrl").

nested_collector_test_() ->
    {timeout, 240, fun nested_collector/0}.

nested_collector() ->
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
        "imboy-collector-nested-" ++ os:getpid() ++ "-" ++
            integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:make_dir(Dir),
    try
        Files =
            [
                filename:join([Repo, "tools/imboy_quality", atom_to_list(M) ++ ".erl"])
             || M <-
                    [imboy_quality_runner, imboy_quality_listener]
            ] ++
                [filename:join([Repo, "test/quality/imboy_quality_collector_tests.erl"])],
        [{ok, _} = compile:file(File, [{outdir, Dir}, return_errors]) || File <- Files],
        %% 使用独立控制 VM，让被测根在真实 worker 内嵌套运行且不替换宿主模块。
        {ok, Peer, _} = peer:start_link(#{
            connection => {{127, 0, 0, 1}, 0},
            env => [{"IMBOY_QUALITY_REPO", Repo}],
            args => ["-noshell", "-noinput", "-boot", "no_dot_erlang", "+S", "2", "-pa", Dir]
        }),
        try
            Result = peer:call(
                Peer,
                imboy_quality_runner,
                run_in_worker,
                [imboy_quality_collector_tests, #{code_path => Dir, schedulers => 2}, 180000],
                200000
            ),
            ?assertEqual(ok, maps:get(exit, Result)),
            ?assert(maps:get(pass, maps:get(counts, Result)) >= 15),
            ?assertMatch({result, {ok, _}}, lists:last(maps:get(events, Result)))
        after
            peer:stop(Peer)
        end
    after
        ok = file:del_dir_r(Dir)
    end.
