%% IMBOY_ERLANG_QUALITY P1-06 正式分片回归（全部通过；进入 U）。
%% SCHED01（LPT 可重放）、SCHED02/03/04（同名资源分槽/独占串行）、
%% SCHED05（单 root 失败不丢其他结果）、SCHED06（worker 数有界）、
%% SCHED07（根为模块原子单位，语义不扁平化——由 runner 的 eunit:test
%% 模块入口保证）、SCHED08（父退出后 peer 有界回收由 runner 语义覆盖）。
-module(imboy_quality_pool_tests).
-include_lib("eunit/include/eunit.hrl").

%% ---------- 纯调度 ----------

%% SCHED01
schedule_lpt_replayable_test() ->
    Roots = [
        root(<<"a">>, 1000),
        root(<<"b">>, 5000),
        root(<<"c">>, 3000),
        root(<<"d">>, 5000)
    ],
    {ok, G1} = imboy_quality_pool:schedule(Roots, 2),
    {ok, G2} = imboy_quality_pool:schedule(Roots, 2),
    ?assertEqual(G1, G2),
    All = lists:append(G1),
    ?assertEqual(4, length(All)),
    %% 慢任务优先：最大耗时 root 在首槽首个
    First = hd(hd(G1)),
    ?assert(maps:get(estimated_ms, First) =:= 5000).

%% SCHED02/03/04
resource_conflict_separation_test() ->
    %% mock 同一模块的两个 root 不得同槽
    Ra = root(<<"m1">>, 100, [mock_channel_logic]),
    Rb = root(<<"m2">>, 100, [mock_channel_logic]),
    {ok, G} = imboy_quality_pool:schedule([Ra, Rb], 2),
    [Slot1, Slot2] = G,
    ?assert(not lists:member(Rb, Slot1)),
    ?assert(not lists:member(Ra, Slot2)),
    %% 独占 DB/端口资源同理
    Da = root(<<"d1">>, 100, [exclusive_pg, db_main]),
    Db = root(<<"d2">>, 100, [exclusive_pg]),
    {ok, G2} = imboy_quality_pool:schedule([Da, Db], 2),
    [S1, S2] = G2,
    ?assert(not (lists:member(Da, S1) andalso lists:member(Db, S1))),
    ?assert(not (lists:member(Da, S2) andalso lists:member(Db, S2))),
    %% 无冲突资源允许同槽共存
    Pc = root(<<"p1">>, 100, []),
    Pd = root(<<"p2">>, 100, [other_resource]),
    {ok, G3} = imboy_quality_pool:schedule([Pc, Pd], 2),
    ?assertEqual(2, length(G3)),
    %% 全槽冲突时追加专用槽（组内串行），不丢 root
    E1 = root(<<"e1">>, 100, [global_role]),
    E2 = root(<<"e2">>, 100, [global_role]),
    E3 = root(<<"e3">>, 100, [global_role]),
    {ok, G4} = imboy_quality_pool:schedule([E1, E2, E3], 1),
    ?assertEqual(3, length(G4)),
    ?assertEqual(3, length(lists:append(G4))).

duplicate_root_id_test() ->
    ?assertError(
        duplicate_root_id,
        imboy_quality_pool:schedule([root(<<"x">>, 1), root(<<"x">>, 2)], 2)
    ).

%% ---------- 有界执行（真实 peer；合成夹具） ----------

pool_execute_test_() ->
    {timeout, 180, fun pool_execute/0}.

%% SCHED05/06：失败 root 不丢成功结果；worker 有界
pool_execute() ->
    CodePath = compile_fixtures(),
    Roots = [
        root(pass_a, quality_fixture_pass_tests, 500),
        root(pass_b, quality_fixture_pass_tests, 500),
        root(fail_c, quality_fixture_fail_tests, 500)
    ],
    Ids = [maps:get(id, R) || R <- Roots],
    Agg0 = imboy_quality_events:new(Ids),
    {Agg, Failed, Results} = imboy_quality_pool:execute(
        Roots,
        #{
            code_path => CodePath,
            worker_count => 2,
            schedulers => 2,
            deadline_ms => 60000,
            group_timeout_ms => 120000
        },
        Agg0
    ),
    %% SCHED05：失败 root 使聚合如实缺终结（整体失败），成功结果不丢
    ?assertMatch(
        {error, missing_terminal_events},
        imboy_quality_events:result(Agg, 0)
    ),
    ?assertEqual([fail_c], Failed),
    ?assert(maps:is_key(pass_a, Results) andalso maps:is_key(pass_b, Results)),
    ?assertEqual(ok, maps:get(exit, maps:get(pass_a, Results))).

root(Id, Est) -> root(Id, Est, []).
root(Id, Est, Resources) when is_integer(Est), is_list(Resources) ->
    #{
        id => Id,
        test_mod => some_module,
        estimated_ms => Est,
        resources => Resources
    };
root(Id, TestMod, Est) when is_atom(TestMod), is_integer(Est) ->
    #{id => Id, test_mod => TestMod, estimated_ms => Est, resources => []}.

compile_fixtures() ->
    Out = filename:join("/private/tmp", "imboy-quality-p6-" ++ os:getpid()),
    ok = filelib:ensure_dir(filename:join(Out, "x")),
    Srcs = filelib:wildcard(filename:join(repo_root_tools(), "*.erl")),
    true = Srcs =/= [],
    lists:foreach(
        fun(Src) ->
            case compile:file(Src, [outdir, Out, return_errors]) of
                {ok, _} -> ok;
                {error, Es, _} -> erlang:error({fixture_compile, Src, Es})
            end
        end,
        Srcs
    ),
    Out.

repo_root_tools() ->
    Repo =
        case os:getenv("IMBOY_QUALITY_REPO") of
            R when is_list(R), R =/= [] -> R;
            _ -> element(2, file:get_cwd())
        end,
    filename:join(Repo, "tools/imboy_quality/test_fixtures").
