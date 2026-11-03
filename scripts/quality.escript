#!/usr/bin/env escript
%%! -noshell +A 4
%% IMBOY_ERLANG_QUALITY P1-03：单一 CLI 入口（纯 Erlang，escript）。
%% 上下文取自控制目录 STATE.json（可信），命令行不能抬权（PAR24）。
%% selftest 仅合成夹具：LP 期与正式期同口径，不链接项目 ebin/deps。
-mode(compile).

main(Args) ->
    try
        CtxDir = resolve_control_dir(),
        %% 先编译并加载框架模块（atom 域就绪后再解析上下文）
        BuildDir0 = filename:join(CtxDir, "prep/build"),
        ok = filelib:ensure_dir(filename:join(BuildDir0, "x")),
        compile_all(BuildDir0),
        true = code:add_patha(BuildDir0),
        [
            begin
                {module, _} = code:ensure_loaded(M)
            end
         || M <- [
                imboy_quality_build,
                imboy_quality_cli,
                imboy_quality_runner,
                imboy_quality_listener,
                imboy_quality_report
            ]
        ],
        {Ctx, _StatePath} = load_context(CtxDir),
        Op = parse(Args),
        enforce(Ctx, Op),
        {Result, OutPath} = dispatch(Op, Ctx, CtxDir),
        Report = undefined,
        io:put_chars(encode(Result)),
        io:nl(),
        case OutPath of
            undefined -> ok;
            P -> io:format("artifact: ~ts~n", [P])
        end,
        erlang:halt(exit_code(Result, Report))
    catch
        Class:Reason:Stack ->
            io:format(
                standard_error,
                "quality.escript error ~tp:~tp~n~tp~n",
                [Class, Reason, Stack]
            ),
            erlang:halt(2)
    end.

%% ---- 控制目录与上下文 ----

resolve_control_dir() ->
    GitOut = os:cmd("git rev-parse --git-common-dir 2>/dev/null"),
    Common = string:trim(GitOut, both, "\n"),
    case filelib:is_dir(Common) of
        true -> filename:join(Common, "zcode-tasks/IMBOY_ERLANG_QUALITY");
        false -> fallback_ctrl()
    end.

fallback_ctrl() ->
    case os:getenv("IMBOY_QUALITY_CTRL") of
        Dir when is_list(Dir), Dir =/= [] -> Dir;
        _ -> erlang:error(no_control_dir)
    end.

load_context(Ctrl) ->
    StatePath = filename:join(Ctrl, "STATE.json"),
    {ok, State, _Sha} = imboy_state_read(StatePath),
    Ctx = trusted(State),
    {Ctx, StatePath}.

trusted(State) ->
    #{
        task_id => maps:get(<<"task_id">>, State, <<>>),
        mode => mode_atom(maps:get(<<"execution_mode">>, State, undefined)),
        goal => mode_atom(maps:get(<<"execution_goal">>, State, undefined)),
        owner_verified => is_map(maps:get(<<"owner">>, State, null)),
        %% G-HEAVY 由 STATE.resource_admission 真实证据决定，命令行不能抬权
        heavy_admitted =>
            case maps:get(<<"resource_admission">>, State, #{}) of
                #{<<"status">> := <<"ADMITTED_FOR_HEAVY">>} -> true;
                _ -> false
            end
    }.

mode_atom(<<"LIMITED_PARALLEL">>) -> limited_parallel;
mode_atom(<<"INTEGRATED_EXECUTION">>) -> integrated_execution;
mode_atom(<<"PHASE1_COMPLETE">>) -> phase1_complete;
mode_atom(<<"ALL_PHASES">>) -> all_phases;
mode_atom(Other) when is_binary(Other) -> {unknown, Other};
mode_atom(Other) -> Other.

%% ---- 参数与许可 ----

parse(["inspect"]) -> inspect;
parse(["pbt", "--profile=" ++ P | _]) -> {pbt, list_to_atom(P)};
parse(["replay", "--next"]) -> replay_next;
parse(["selftest"]) -> selftest;
parse(["build-check"]) -> build_check;
parse(["build-seal"]) -> build_seal;
parse(["admission", "--check-only"]) -> admission_check_only;
parse(["guard"]) -> guard;
parse(["eunit", "--full" | _]) -> eunit_full;
parse(["eunit", "--group=" ++ ModsStr | _]) -> {eunit_group, ModsStr};
parse(["ct", "--profile=" ++ P | _]) -> {ct, list_to_atom(P)};
parse(["ct", "--selftest"]) -> ct_selftest;
parse(["report"]) -> report;
parse(_) -> erlang:error(usage).

enforce(Ctx, Op) ->
    ok = imboy_quality_cli:permit(
        Ctx#{
            execution_mode => maps:get(mode, Ctx),
            execution_goal => maps:get(goal, Ctx)
        },
        permit_op(Op)
    ).

permit_op(selftest) -> synthetic_selftest;
permit_op(admission_check_only) -> admission_check_only;
permit_op({eunit_group, _}) -> eunit_group;
permit_op({ct, _}) -> ct;
permit_op(ct_selftest) -> ct;
permit_op({pbt, _}) -> pbt;
permit_op(replay_next) -> replay;
permit_op(Op) -> Op.

%% ---- 执行 ----

dispatch(inspect, _Ctx, Ctrl) ->
    Info = #{
        op => inspect,
        control_dir => Ctrl,
        modules => [M || {M, _} <- sources()],
        cli => imboy_quality_cli:describe()
    },
    {Info, undefined};
dispatch(selftest, _Ctx, Ctrl) ->
    BuildDir = filename:join(Ctrl, "prep/build"),
    ok = filelib:ensure_dir(filename:join(BuildDir, "x")),
    compile_all(BuildDir),
    true = code:add_patha(BuildDir),
    Tests = test_modules(BuildDir),
    true = Tests =/= [],
    {Pass, Fail} = run_selftests(Tests),
    C = erase(selftest_counts),
    Result = #{
        <<"op">> => <<"selftest">>,
        <<"scope">> => <<"synthetic_only">>,
        <<"top_level_cases">> => length(Tests),
        <<"pass">> => Pass,
        <<"fail">> => Fail,
        <<"cancel">> => maps:get(cancel, C, 0),
        <<"skip">> => maps:get(skip, C, 0),
        <<"business_tests_started">> => 0
    },
    Out = filename:join(Ctrl, "evidence/selftest-latest.json"),
    {ok, _} = imboy_report_write(Out, Result),
    {Result, Out};
dispatch(build_check, _Ctx, _Ctrl) ->
    %% P1-05 BUILD01 hit/miss 判定：计算内容键 → 收据匹配 + 完整性校验
    %% （逐 BEAM SHA256 重算）。hit 退出码 0（Make 跳过硬复位，仅增量
    %% test-build）；miss 退出码 1（Make 走 A1c 硬复位 + 全量重编 + seal）。
    {ok, Repo} = file:get_cwd(),
    #{key := Key} = imboy_quality_build:build_key(Repo, build_profile()),
    case imboy_quality_build:receipt_verify(receipt_path(), Repo, Key) of
        {hit, Key} ->
            io:format("== quality-test-build HIT key=~s ==~n", [hex(Key)]),
            {#{op => build_check, verdict => hit, key => hex(Key)}, undefined};
        {miss, Reason} ->
            io:format("== quality-test-build MISS: ~tp ==~n", [Reason]),
            {
                #{
                    op => build_check,
                    verdict => miss,
                    key => hex(Key),
                    reason =>
                        list_to_binary(io_lib:format("~tp", [Reason]))
                },
                undefined
            }
    end;
dispatch(build_seal, _Ctx, _Ctrl) ->
    %% P1-05 BUILD01：miss 路径在 test-build 成功后调用。采集输出目录快照
    %% （输出路径 / 模块集 / 各 BEAM SHA256 / .app modules）并原子写收据。
    {ok, Repo} = file:get_cwd(),
    Project = project_name(),
    OutDir = "ebin",
    AppFile = filename:join(OutDir, Project ++ ".app"),
    %% 收据按仓库相对路径登记（见 imboy_quality_build:receipt_collect 文档）；
    %% escript 由 Make 配方在仓库根调用，相对路径即解析到 Repo。
    case imboy_quality_build:receipt_collect(OutDir, AppFile) of
        {ok, Collect} ->
            #{key := Key} = imboy_quality_build:build_key(Repo, build_profile()),
            Receipt = Collect#{key => Key},
            Path = receipt_path(),
            ok = imboy_quality_build:write_receipt(Path, Receipt),
            io:format(
                "== quality-test-build SEALED key=~s modules=~p app_modules=~p -> ~ts ==~n",
                [
                    hex(Key),
                    length(maps:get(modules, Collect)),
                    length(maps:get(app_modules, Collect)),
                    Path
                ]
            ),
            {
                #{
                    op => build_seal,
                    ok => true,
                    key => hex(Key),
                    modules => length(maps:get(modules, Collect)),
                    app_modules => length(maps:get(app_modules, Collect))
                },
                undefined
            };
        {error, Reason} ->
            erlang:error({receipt_collect_failed, Reason})
    end;
dispatch(admission_check_only, _Ctx, Ctrl) ->
    StatePath = filename:join(Ctrl, "STATE.json"),
    {ok, State, Sha} = imboy_state_read(StatePath),
    Result = #{
        op => admission_check_only,
        state_sha256 => Sha,
        lifecycle_status => maps:get(<<"lifecycle_status">>, State, null),
        implementation_admission =>
            maps:get(<<"implementation_admission">>, State, null)
    },
    {Result, undefined};
dispatch(report, _Ctx, Ctrl) ->
    Latest = filename:join(Ctrl, "evidence/selftest-latest.json"),
    case file:read_file(Latest) of
        {ok, B} -> {json:decode(B), Latest};
        {error, _} -> {#{op => report, artifact => none}, undefined}
    end;
dispatch(guard, _Ctx, Ctrl) ->
    %% P1-11 真实接线：对当前候选（worktree HEAD 相对 auto 基线）执行
    %% erlang-ci 规则的 Erlang 移植；并校验上游 provenance 摘要。
    case provenance_ok(Ctrl) of
        ok -> ok;
        {mismatch, Detail} -> erlang:error({provenance_mismatch, Detail})
    end,
    {ok, Cwd} = file:get_cwd(),
    {0, HeadB} = imboy_quality_guard:git(Cwd, ["rev-parse", "HEAD"]),
    Head = string:trim(binary_to_list(HeadB)),
    Result =
        case imboy_quality_guard:check(Cwd, auto, Head) of
            {ok, []} ->
                #{
                    <<"op">> => <<"guard">>,
                    <<"verdict">> => <<"clean">>,
                    <<"head">> => Head,
                    <<"base">> => <<"auto">>
                };
            {error, {violations, Vs}} ->
                #{
                    <<"op">> => <<"guard">>,
                    <<"verdict">> => <<"violations">>,
                    <<"head">> => Head,
                    <<"base">> => <<"auto">>,
                    <<"violations">> => [format_violation(V) || V <- Vs]
                };
            {error, {infrastructure, C, R}} ->
                erlang:error({guard_infrastructure, C, R})
        end,
    Out = filename:join(Ctrl, "evidence/guard-latest.json"),
    {ok, _} = imboy_report_write(Out, Result),
    {Result, Out};
dispatch({pbt, Profile}, _Ctx, Ctrl) ->
    %% P2-01：PropEr 执行底座真实接线。manifest 由 P2-03..P2-08 填充真实
    %% 12 性质；缺失/非法时 fail-closed（PBT02：启动前拒绝，不默认掩盖）。
    %% 反例完整 term 的 ETF 落盘属 P2-02，此处仅计数摘要。
    {ok, Repo} = file:get_cwd(),
    %% proper 运行时必须可加载（性质体展开为 proper:forall 等调用）；
    %% 缺依赖时 fail-closed，不产生误导性的 verdict=error 绿灯。
    ProperEbin = filename:join([Repo, "deps", "proper", "ebin"]),
    case filelib:is_dir(ProperEbin) of
        true -> true = code:add_patha(ProperEbin);
        false -> erlang:error({proper_dependency_missing, ProperEbin})
    end,
    %% P2-03 起，真实性质引用生产模块（如 imboy_frame）：项目 ebin 与全部
    %% deps ebin（被测运行时依赖闭包，如 jsone）加入 path；缺失即 error。
    ProjectEbin = filename:join([Repo, "ebin"]),
    case filelib:is_dir(ProjectEbin) of
        true -> true = code:add_patha(ProjectEbin);
        false -> erlang:error({project_ebin_missing, ProjectEbin})
    end,
    lists:foreach(
        fun(D) -> true = code:add_patha(D) end,
        lists:sort(filelib:wildcard(filename:join([Repo, "deps", "*", "ebin"])))
    ),
    ManifestPath = filename:join([Repo, "test", "pbt", "pbt-manifest.term"]),
    case imboy_quality_pbt:load_manifest(ManifestPath) of
        {ok, Manifest} ->
            {ok, Summary} = imboy_quality_pbt:run(Manifest, Profile, #{}),
            Total = maps:get(total, Summary),
            Passed = maps:get(passed, Summary),
            Result = #{
                <<"op">> => <<"pbt">>,
                <<"profile">> => atom_to_binary(Profile),
                <<"total">> => Total,
                <<"passed">> => Passed,
                <<"verdict">> =>
                    case Passed =:= Total of
                        true -> <<"all_passed">>;
                        false -> <<"properties_failed">>
                    end,
                <<"results">> => [format_pbt_result(R) || R <- maps:get(results, Summary)]
            },
            Out = filename:join(Ctrl, "evidence/pbt-latest.json"),
            {ok, _} = imboy_report_write(Out, Result),
            {Result, Out};
        {error, Reason} ->
            erlang:error({pbt_manifest_invalid, ManifestPath, Reason})
    end;
dispatch(replay_next, _Ctx, Ctrl) ->
    %% P2-02：重放最旧未重放反例。allowlist 来自 manifest（若存在）；无
    %% manifest 时显式拒绝（不能无白名单 apply 任意 MFA）。无待重放如实
    %% exit 2（REPLAY06）。
    {ok, Repo} = file:get_cwd(),
    ProperEbin0 = filename:join([Repo, "deps", "proper", "ebin"]),
    true = code:add_patha(ProperEbin0),
    ManifestPath = filename:join([Repo, "test", "pbt", "pbt-manifest.term"]),
    Allowed =
        case imboy_quality_pbt:load_manifest(ManifestPath) of
            {ok, #{properties := Props}} ->
                [maps:get(mfa, P) || P <- Props];
            {error, _} ->
                erlang:error({replay_requires_manifest_allowlist, ManifestPath})
        end,
    case imboy_quality_pbt:replay_next(Ctrl, Allowed) of
        {ok, #{path := P, replay := R, entry := E}} ->
            Result = #{
                <<"op">> => <<"replay">>,
                <<"verdict">> => <<"replayed">>,
                <<"path">> => unicode:characters_to_binary(P),
                <<"id">> => atom_to_binary(maps:get(id, E)),
                <<"replay">> => format_replay(R)
            },
            {Result, undefined};
        {error, no_pending_counterexamples} ->
            erlang:error(no_pending_counterexamples);
        {error, R2} ->
            erlang:error({replay_failed, R2})
    end;
dispatch(eunit_full, _Ctx, Ctrl) ->
    %% P1-10/P1-12 真实接线：分片执行完整 U（普通根 + 独占车道）。G-HEAVY 已由
    %% enforce 校验。环境：IMBOY_QUALITY_WORKERS（默认 2）、EUNIT_CONFIG
    %% （默认 config/sys.local）。每 worker 独立 http 端口副本（19980+K / 19970+K）。
    %% 资源准备（模板/克隆/独占资源/Garage 等）在整条命令计时之内（EUNIT_120S §2）。
    {ok, Repo} = file:get_cwd(),
    require_test_build(Repo),
    RunId = run_id(),
    Workers = env_int("IMBOY_QUALITY_WORKERS", 2),
    #{ordinary := Ordinary, excluded := Excl} = imboy_quality_full:inventory(Repo),
    Est = load_estimates(Ctrl),
    Prepared = imboy_quality_env:prepare(Repo, Ctrl, Workers),
    Opts = run_opts(Repo, Ctrl, Workers, Prepared, Excl, #{estimates => Est}),
    with_resources(Repo, Prepared, fun() ->
        {Result0, OutDir} = imboy_quality_full:run(Ordinary, Opts, RunId),
        Cleanup = imboy_quality_env:release(Prepared),
        Result = Result0#{
            <<"resources">> => maps:get(report, Prepared),
            <<"cleanup">> => Cleanup
        },
        {ok, _} = imboy_report_write(filename:join(OutDir, "result.json"), Result),
        {Result, OutDir}
    end);
dispatch({eunit_group, ModsStr}, _Ctx, Ctrl) ->
    %% P1-10 诊断入口（FRAMEWORK_DESIGN §2：--group 仅诊断，报告标 PARTIAL，
    %% 不产生 full PASS）。普通根在单 worker 单 VM 顺序执行；独占根走独占车道
    %% （各自 fresh VM）。用于修复验证与定向取证；不消耗 full 预算。
    {ok, Repo} = file:get_cwd(),
    require_test_build(Repo),
    Mods = parse_group(ModsStr),
    #{ordinary := Ordinary, excluded := Excl} = imboy_quality_full:inventory(Repo),
    Unknown = [M || M <- Mods, not lists:member(M, Ordinary ++ Excl)],
    case Unknown of
        [] -> ok;
        _ -> erlang:error({roots_not_in_u, Unknown})
    end,
    Workers = env_int("IMBOY_QUALITY_WORKERS", 1),
    Prepared = imboy_quality_env:prepare(Repo, Ctrl, Workers),
    Opts = run_opts(Repo, Ctrl, Workers, Prepared, [M || M <- Mods, lists:member(M, Excl)], #{
        estimates => load_estimates(Ctrl), out_prefix => "group-"
    }),
    OrdPart = [M || M <- Mods, lists:member(M, Ordinary)],
    with_resources(Repo, Prepared, fun() ->
        {Result0, OutDir} = imboy_quality_full:run(OrdPart, Opts, run_id()),
        Cleanup = imboy_quality_env:release(Prepared),
        Result = Result0#{
            <<"scope">> => <<"PARTIAL">>,
            <<"op">> => <<"eunit_group">>,
            <<"resources">> => maps:get(report, Prepared),
            <<"cleanup">> => Cleanup
        },
        {ok, _} = imboy_report_write(filename:join(OutDir, "result.json"), Result),
        {Result, OutDir}
    end);
dispatch(eunit_group, _Ctx, _Ctrl) ->
    erlang:error(eunit_group_requires_modlist);
dispatch({ct, Profile}, _Ctx, Ctrl) ->
    ct_run_profile(Profile, Ctrl);
dispatch(ct_selftest, _Ctx, Ctrl) ->
    ct_run_profile(ct04, Ctrl).

%% P2-09：CT 合同入口（CT01-04）。判据器 fail-closed（imboy_quality_ct:classify）：
%% 零案例/auto-skip/init 失败/cleanup 异常一律不通过；ct04 为 drafts 合成
%% 负向自测，不进仓库 U。
ct_run_profile(Profile, Ctrl) ->
    {ok, Repo} = file:get_cwd(),
    require_test_build(Repo),
    {Suites, TestDir} = ct_plan(Profile, Repo, Ctrl),
    case Profile of
        ct04 ->
            %% 合成 SUITE（drafts）无 app/PG 依赖，主 VM 直跑
            Results = imboy_quality_ct:run_suites(Suites, TestDir),
            ct_report(Profile, Results, Ctrl);
        _ ->
            %% 真实 SUITE 复用 P1 worker 链：私有 PG 模板/克隆 + per-worker
            %% overlay config + peer VM（boot 同 eunit_setup 源）。
            Prepared = imboy_quality_env:prepare(Repo, Ctrl, 1),
            AliasState = imboy_quality_full:ensure_lib_alias(Repo),
            try
                CfgFile = imboy_quality_env:worker_config(Repo, 0, Prepared),
                CodePaths =
                    filelib:wildcard(filename:join([Repo, "deps", "*", "ebin"])) ++
                    [
                        filename:join(Repo, "imboy/ebin"),
                        filename:join(Repo, "ebin"),
                        filename:join(Repo, "test"),
                        filename:join(Ctrl, "prep/build")
                    ],
                PaArgs = lists:append([["-pa", CP] || CP <- CodePaths]),
                {ok, Peer, _Node} = peer:start_link(#{
                    connection => {{127, 0, 0, 1}, 0},
                    args =>
                        [
                            "-noshell",
                            "-noinput",
                            "-boot",
                            "no_dot_erlang",
                            "-config",
                            CfgFile
                        ] ++ PaArgs
                }),
                try
                    Results = peer:call(
                        Peer, imboy_quality_ct, run_suites, [Suites, TestDir], 900000
                    ),
                    ct_report(Profile, Results, Ctrl)
                after
                    peer:stop(Peer)
                end
            after
                imboy_quality_full:maybe_remove_lib_alias(Repo, AliasState),
                _ = imboy_quality_env:release(Prepared)
            end
    end.

ct_report(Profile, Results, Ctrl) ->
    Failed = [R || R = {_, V} <- Results, element(1, V) =:= fail],
    %% ct04（CT04 负向自测）：判定标准 = 判据分类与期望一一相符
    %% （正控 pass，init 失败/auto-skip/cleanup 异常必须 fail），而非套件通过。
    Kind = fun(pass) -> pass; ({fail, _}) -> fail end,
    Check =
        case ct_expect(Profile) of
            undefined -> Failed =:= [];
            Expect -> lists:all(fun({S, V}) -> maps:get(S, Expect) =:= Kind(V) end, Results)
        end,
    Verdict =
        case Check of
            true -> <<"all_passed">>;
            false -> <<"suites_failed">>
        end,
    Result = #{
        <<"op">> => <<"ct">>,
        <<"profile">> => atom_to_binary(Profile),
        <<"results">> =>
            [
                #{
                    <<"suite">> => atom_to_binary(S),
                    <<"verdict">> => ct_verdict_bin(V)
                }
             || {S, V} <- Results
            ],
        <<"total">> => length(Results),
        <<"verdict">> => Verdict
    },
    Out = filename:join(Ctrl, "evidence/ct-latest.json"),
    {ok, _} = imboy_report_write(Out, Result),
    {Result, Out}.

ct_plan(ct01, Repo, _Ctrl) ->
    {[msg_delivery_SUITE, msg_ack_logic_SUITE], filename:join(Repo, "test")};
ct_plan(ct02, Repo, _Ctrl) ->
    {[imboy_quality_ct02_org_boundary_SUITE], filename:join(Repo, "test")};
ct_plan(ct03, Repo, _Ctrl) ->
    {[imboy_quality_ct03_migration_SUITE], filename:join(Repo, "test")};
ct_plan(critical, Repo, _Ctrl) ->
    {[msg_delivery_SUITE, msg_ack_logic_SUITE, imboy_quality_ct02_org_boundary_SUITE,
        imboy_quality_ct03_migration_SUITE], filename:join(Repo, "test")};
ct_plan(ct04, _Repo, Ctrl) ->
    Dir = filename:join(Ctrl, "drafts/ct-selftest"),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    ok = write_ct_selftest_suites(Dir),
    {[qct_ok_SUITE, qct_initfail_SUITE, qct_autoskip_SUITE, qct_cleanupfail_SUITE], Dir}.

ct_expect(ct04) ->
    #{
        qct_ok_SUITE => pass,
        qct_initfail_SUITE => fail,
        qct_autoskip_SUITE => fail,
        qct_cleanupfail_SUITE => fail
    };
ct_expect(_) ->
    undefined.

ct_verdict_bin(pass) -> <<"pass">>;
ct_verdict_bin({fail, Reason}) ->
    Rb = iolist_to_binary(io_lib:format("~0p", [Reason])),
    <<"fail: ", Rb/binary>>.

write_ct_selftest_suites(Dir) ->
    Suits = [
        {qct_ok_SUITE,
            "-module(qct_ok_SUITE).\n"
            "-export([all/0, passes/1]).\n"
            "all() -> [passes].\n"
            "passes(_C) -> ok.\n"},
        {qct_initfail_SUITE,
            "-module(qct_initfail_SUITE).\n"
            "-export([all/0, broken/1, init_per_suite/1]).\n"
            "all() -> [broken].\n"
            "init_per_suite(_C) -> {fail, injected_init_failure}.\n"
            "broken(_C) -> ok.\n"},
        {qct_autoskip_SUITE,
            "-module(qct_autoskip_SUITE).\n"
            "-export([all/0, s1/1]).\n"
            "all() -> [s1].\n"
            "s1(_C) -> {skip, \"injected_auto_skip\"}.\n"},
        {qct_cleanupfail_SUITE,
            "-module(qct_cleanupfail_SUITE).\n"
            "-export([all/0, c1/1, end_per_suite/1]).\n"
            "all() -> [c1].\n"
            "c1(_C) -> ok.\n"
            "end_per_suite(_C) -> ok = injected_cleanup_failure.\n"}
    ],
    lists:foreach(
        fun({Name, Src}) ->
            Path = filename:join(Dir, atom_to_list(Name) ++ ".erl"),
            ok = file:write_file(Path, Src)
        end,
        Suits
    ),
    ok.

%% 资源生命周期包裹：正常/异常路径的 after 回收 + SIGTERM 信号路径回收（被 TERM 的控制器
%% 不会执行 after，2026-10-29 实证遗留 16 库/10 配置/1 Garage 守护进程）。
with_resources(Repo, Prepared, Fun) ->
    ok = imboy_quality_signal:install(fun() ->
        imboy_quality_env:cleanup_configs(Repo),
        imboy_quality_env:release_once(Prepared)
    end),
    try
        Fun()
    after
        imboy_quality_env:cleanup_configs(Repo),
        imboy_quality_env:release_once(Prepared),
        imboy_quality_signal:uninstall()
    end.

%% full/group 共用执行选项：独占根按根 env 与按根 -config（无专属配置则沿用 worker 0）。
%% deadline：普通组默认 90 分钟——talk_record 网关根真实等待 ~1h 的上传能力过期
%% （imboy_quality_full ?LONG_ROOT_HINTS）；独占根默认 30 分钟。均可由环境变量覆盖。
run_opts(Repo, Ctrl, Workers, Prepared, Excl, Extra) ->
    maps:merge(
        #{
            repo => Repo,
            ctrl => Ctrl,
            worker_count => Workers,
            schedulers => 2,
            deadline_ms => env_int("IMBOY_QUALITY_DEADLINE_MS", 5400000),
            exclusive_deadline_ms => env_int("IMBOY_QUALITY_EXCLUSIVE_DEADLINE_MS", 1800000),
            process_limit => 32768,
            env => full_env(Repo) ++ maps:get(env, Prepared),
            spec_wrapper => spec_wrapper(Repo),
            exclusive_roots => Excl,
            exclusive_env_fn => fun(M) -> maps:get(M, maps:get(exclusive_env, Prepared), []) end,
            exclusive_config_fn => fun(M) ->
                case maps:find(M, maps:get(exclusive_config, Prepared, #{})) of
                    {ok, Path} -> Path;
                    error -> imboy_quality_env:worker_config(Repo, 0, Prepared)
                end
            end,
            worker_config_fn => fun(K) -> imboy_quality_env:worker_config(Repo, K, Prepared) end
        },
        Extra
    ).

%% ---- full/group 辅助 ----

run_id() ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:universal_time(),
    lists:flatten(
        io_lib:format(
            "~4..0B~2..0B~2..0BT~2..0B~2..0B~2..0BZ",
            [Y, Mo, D, H, Mi, S]
        )
    ).

parse_group(ModsStr) ->
    Mods = [
        list_to_atom(string:trim(P))
     || P <- string:split(ModsStr, ",", all),
        string:trim(P) =/= []
    ],
    case Mods of
        [] -> erlang:error(empty_group);
        _ -> Mods
    end.

%% test-build 完整性预检：legacy 入口 eunit 目标以 test-build 为前置条件；
%% 标记缺失说明 ebin 可能是非 -DTEST 构建（白盒导出缺失→大面积 undef）。
require_test_build(Repo) ->
    Marker = filename:join(Repo, ".erlang.mk/imboy.test"),
    case filelib:is_regular(Marker) of
        true ->
            ok;
        false ->
            erlang:error(
                {test_build_marker_missing, Marker,
                    {hint, <<"run: IMBOYENV=local make test-build">>}}
            )
    end.

sys_config_base() ->
    case os:getenv("EUNIT_CONFIG") of
        false -> "config/sys.local";
        B -> B
    end.

%% legacy 由 erlang.mk export ERL_LIBS=$(APPS_DIR):$(DEPS_DIR)（相对 cwd），
%% deps 以最低优先级进路径且 code:lib_dir(Dep) 可解析。此处显式绝对路径等价。
%% Makefile:349-357 export TEST_HTTP_PORT=0：eunit_setup 以 test_http_port/0
%% 覆盖 -config 的 http_port（默认 19800，被同机 VM 占用即 eaddrinuse 连锁），
%% 0 = 内核随机端口；Makefile:3-4 export PROJECT_VERSION；Makefile:44-62 的
%% LLM key 导出按 .env.local > .env 首个非空定义（文件缺失则不导出，与 legacy 同）。
full_env(Repo) ->
    Version =
        case file:read_file(filename:join(Repo, "VERSION")) of
            {ok, V} -> string:trim(binary_to_list(V));
            _ -> ""
        end,
    Args =
        try
            imboy_quality_fixture:pg_args_from_config(
                filename:join(Repo, sys_config_base() ++ ".config")
            )
        catch
            _:_ -> #{port => 4323}
        end,
    Host = maps:get(host, Args, "127.0.0.1"),
    PortS = integer_to_list(maps:get(port, Args, 4323)),
    User = maps:get(username, Args, "imboy_user"),
    Pass = maps:get(password, Args, ""),
    [
        {"ERL_LIBS", filename:join(Repo, "apps") ++ ":" ++ filename:join(Repo, "deps")},
        %% eunit 轨道在 VM 内恒为 "test"（eunit_setup 首次执行即 putenv）；legacy 的
        %% "local" 只在 Make/配置选择层有意义。分片后某些根可能先于任何 eunit_setup 运行
        %% （intbe02 前置 os:getenv("IMBOYENV") =:= "test"），故 VM 自启动即为 test。
        {"IMBOYENV", "test"},
        {"TEST_HTTP_PORT", "0"},
        {"PROJECT_VERSION", Version},
        %% 门运行环境（scripts/ci/native-eunit-run.sh）导出的 PG 指向型旋钮在
        %% 直连 PG 下的等价值：marker 服务器与本 PG 是同一服务器；目录同步隔离
        %% 夹具连同一真实 PG。非 PG 指向的旋钮（W403/TG16-20/HIRD/Garage）不在
        %% 此伪造，仍按独占归属硬失败。
        {"OA_CLOUD_MARKER_PORT", PortS},
        {"DIRECTORY_SYNC_INTTEST_PG_HOST", Host},
        {"DIRECTORY_SYNC_INTTEST_PG_PORT", PortS},
        {"DIRECTORY_SYNC_INTTEST_PG_USER", User},
        {"DIRECTORY_SYNC_INTTEST_PG_PASSWORD", Pass}
    ] ++ llm_keys(Repo).

llm_keys(Repo) ->
    Files = [filename:join(Repo, F) || F <- [".env.local", ".env"]],
    [
        {K, V}
     || K <- ["BIGMODEL_API_KEY", "ARK_API_KEY", "BAILIAN_API_KEY"],
        V <- [env_key(K, Files)],
        V =/= undefined
    ].

env_key(_K, []) ->
    undefined;
env_key(K, [F | Rest]) ->
    case file:read_file(F) of
        {ok, B} ->
            Prefix = list_to_binary(K ++ "="),
            Size = byte_size(Prefix),
            Vals = [
                binary:part(L, Size, byte_size(L) - Size)
             || L <- binary:split(B, <<"\n">>, [global]),
                byte_size(L) >= Size,
                binary:part(L, 0, Size) =:= Prefix
            ],
            case Vals of
                [V | _] when V =/= <<>> -> binary_to_list(V);
                _ -> env_key(K, Rest)
            end;
        _ ->
            env_key(K, Rest)
    end.

%% 项目 Makefile:188 EUNIT_TEST_SPEC 以 talk_record_owned_eunit_fixture:entry/1
%% 包裹每个根（budgets 内根变单叶超时包裹，isolated_pg_fixture 子节点执行）。
%% 等价对照（EQ01）必须同规格；合成自测环境无该模块时回退裸模块。
spec_wrapper(Repo) ->
    case
        filelib:is_regular(
            filename:join(Repo, "test/talk_record_owned_eunit_fixture.beam")
        )
    of
        true -> {talk_record_owned_eunit_fixture, entry};
        false -> undefined
    end.

env_int(Name, Default) ->
    case os:getenv(Name) of
        false -> Default;
        V -> list_to_integer(V)
    end.

%% ---- build-check / build-seal 辅助（P1-05 BUILD01） ----
%% Make→escript 的键输入通道全部走环境变量（Makefile export 直传，不经 shell
%% 引号折叠——ERLC_COMPILE_OPTS 内含单引号，命令行拼接会破引号）。

project_name() -> os:getenv("IMBOY_QUALITY_PROJECT", "imboy").

receipt_path() ->
    case os:getenv("IMBOY_QUALITY_BUILD_RECEIPT") of
        P when is_list(P), P =/= [] -> P;
        _ ->
            filename:join(
                ".erlang.mk", project_name() ++ ".quality-test-build.receipt"
            )
    end.

build_profile() ->
    #{
        compiler_opts => split_opts(os:getenv("IMBOY_QUALITY_TEST_ERLC_OPTS")),
        erlc_opts => split_opts(os:getenv("IMBOY_QUALITY_ERLC_OPTS")),
        features => split_opts(os:getenv("IMBOY_QUALITY_FEATURES")),
        test_mode => true
    }.

split_opts(false) ->
    [];
split_opts(S) when is_list(S) ->
    [P || P <- string:split(S, " ", all), P =/= []].

hex(Bin) when is_binary(Bin) -> binary_to_list(binary:encode_hex(Bin)).

load_estimates(Ctrl) ->
    %% 优先读取上一次分片流（group_*.stream，ETF）的实测 wall_ms 作为 LPT 估计
    %% （无则保守 3000ms）。2026-10-30 修复：曾读 per_root.term，但其为
    %% pretty-print map（含 pid 字面量），file:consult 恒失败 → 估计静默退化
    %% 为平坦 3000ms；改读权威 ETF 流（imboy_quality_runner:stream_read 自带
    %% 长度前缀解析、容忍尾部截断），取 {done, M, Res} 的 wall_ms。独占根不经
    %% 流写盘（run_exclusive 无 stream 键），但其逐根独占执行、不参与普通道
    %% LPT 打包，估计只需覆盖普通根。
    Dirs = lists:sort(
        [
            D
         || D <- filelib:wildcard(filename:join([Ctrl, "evidence", "full-*"])),
            filelib:is_dir(D)
        ]
    ),
    Streams =
        case Dirs of
            [] ->
                [];
            _ ->
                filelib:wildcard(filename:join([lists:last(Dirs), "group_*.stream"]))
        end,
    Est = load_estimates_from_streams(Streams, #{}),
    case map_size(Est) of
        0 ->
            io:format(
                "WARNING: no per-root wall_ms recovered from prior run; "
                "LPT degrades to flat 3000ms estimates~n"
            ),
            #{};
        _ ->
            Est
    end.

load_estimates_from_streams([], Acc) ->
    Acc;
load_estimates_from_streams([S | Rest], Acc) ->
    case imboy_quality_runner:stream_read(S) of
        {ok, Events} ->
            load_estimates_from_streams(
                Rest,
                lists:foldl(
                    fun
                        ({done, M, #{wall_ms := Ms}}, A) when is_atom(M), is_integer(Ms) ->
                            A#{M => max(500, Ms)};
                        (_, A) ->
                            A
                    end,
                    Acc,
                    Events
                )
            );
        _ ->
            io:format("WARNING: unreadable stream ~ts; estimates may be partial~n", [S]),
            load_estimates_from_streams(Rest, Acc)
    end.

%% ---- 合成自测 ----

run_selftests(Tests) ->
    Parent = self(),
    Ref = make_ref(),
    Collector = spawn(fun() -> collect(Parent, Ref, []) end),
    Return = eunit:test(Tests, [{report, {imboy_quality_listener, [{collector, Collector}]}}]),
    Collector ! {stop, Parent, Ref},
    {Counts, _Events} =
        receive
            {collected, Ref, C, E} -> {C, E}
        after 10000 -> {#{}, []}
        end,
    %% 任何合成失败都让 selftest 非零退出
    ok = Return,
    put(selftest_counts, Counts),
    {maps:get(pass, Counts, 0), maps:get(fail, Counts, 0)}.

collect(Parent, Ref, Acc) ->
    receive
        {stop, Parent, Ref} ->
            Parent ! {collected, Ref, counts(Acc), lists:reverse(Acc)};
        Msg ->
            collect(Parent, Ref, [Msg | Acc])
    after 30000 ->
        Parent ! {collected, Ref, counts(Acc), lists:reverse(Acc)}
    end.

counts(Es) ->
    lists:foldl(
        fun
            ({test_end, _, ok, _}, C) -> C#{pass => maps:get(pass, C, 0) + 1};
            ({test_end, _, _, _}, C) -> C#{fail => maps:get(fail, C, 0) + 1};
            ({test_cancel, _, _, _}, C) -> C#{cancel => maps:get(cancel, C, 0) + 1};
            (_, C) -> C
        end,
        #{},
        Es
    ).

test_modules(BuildDir) ->
    %% 合成 selftest 仅收集无项目依赖的模块；fixture 回归需要 epgsql+PG
    %% （business env），由项目 make eunit（U 全量）真实执行。
    [
        list_to_atom(filename:rootname(F))
     || F <- filelib:wildcard("imboy_quality_*_tests.beam", BuildDir),
        F =/= "imboy_quality_fixture_tests.beam"
    ].

%% ---- 编译 ----

sources() ->
    [
        {
            filename:rootname(filename:basename(P)),
            filename:join(["tools/imboy_quality", filename:basename(P)])
        }
     || P <- filelib:wildcard("tools/imboy_quality/*.erl")
    ] ++
        [
            {
                filename:rootname(filename:basename(P)),
                filename:join(["test/pbt", filename:basename(P)])
            }
         || P <- filelib:wildcard("test/pbt/*.erl")
        ] ++
        [
            {
                filename:rootname(filename:basename(P)),
                filename:join(["test/quality", filename:basename(P)])
            }
         || P <- filelib:wildcard("test/quality/*.erl")
        ].

compile_all(BuildDir) ->
    %% 编译经原生 erlc（open_port + 独立 argv，FRAMEWORK_DESIGN §3 允许边界）；
    %% 本机 OTP29 的 escript 内 compile:file 不落盘，erlc 为确定路径。
    lists:foreach(
        fun({_Mod, Src}) ->
            case needs_compile(Src, any_beam(BuildDir, _Mod)) of
                true ->
                    case erlc(Src, BuildDir) of
                        0 ->
                            {ok, Bin2} = file:read_file(Src),
                            Sidecar =
                                BuildDir ++ "/" ++
                                    filename:rootname(filename:basename(Src)) ++ ".beam.srcmd5",
                            ok = file:write_file(Sidecar, crypto:hash(md5, Bin2));
                        Code ->
                            erlang:error({compile_failed, Src, Code})
                    end;
                false ->
                    ok
            end
        end,
        sources()
    ).

any_beam(BuildDir, Mod) ->
    filename:join(BuildDir, Mod ++ ".beam").

erlc(Src, OutDir) ->
    Erlc = os:find_executable("erlc"),
    %% P2-01：test/pbt 源含 include_lib("proper/include/proper.hrl")，
    %% erlc 需 proper 在 code path 解析（deps/proper 由 TEST_DEPS 就位）。
    ProperPa =
        case filelib:is_dir("deps/proper/ebin") of
            true -> ["-pa", "deps/proper/ebin"];
            false -> []
        end,
    Port = open_port(
        {spawn_executable, Erlc},
        [
            exit_status,
            use_stdio,
            stderr_to_stdout,
            {args, ["-o", OutDir] ++ ProperPa ++ [Src]}
        ]
    ),
    collect_port(Port, []).

collect_port(Port, Acc) ->
    receive
        {Port, {data, _D}} -> collect_port(Port, Acc);
        {Port, {exit_status, Code}} -> Code
    after 60000 ->
        port_close(Port),
        erlang:error(erlc_timeout)
    end.

%% P2-04 fix: mtime comparison missed same-session re-edits twice (resolution
%% / same-second writes); replaced by source-md5 sidecar (<mod>.beam.srcmd5):
%% recompile iff content changed or beam missing (BUILD03-class guard).
needs_compile(Src, Out) ->
    case file:read_file(Src) of
        {ok, Bin} ->
            Md5 = crypto:hash(md5, Bin),
            Sidecar = Out ++ ".srcmd5",
            Recorded =
                case file:read_file(Sidecar) of
                    {ok, Prev} -> Prev;
                    _ -> <<>>
                end,
            Recorded =/= Md5 orelse not filelib:is_regular(Out);
        {error, _} ->
            erlang:error({missing_source, Src})
    end.

%% ---- guard 辅助 ----

provenance_ok(Ctrl) ->
    Path = filename:join(Ctrl, "evidence/erlang-ci-provenance.json"),
    {ok, Bytes} = file:read_file(Path),
    #{<<"files">> := Files} = json:decode(Bytes),
    Checks = [
        begin
            %% 上游 path 的 basename 在控制目录 prep/ 中以 upstream-<name>.audit 保存
            BaseName = filename:basename(binary_to_list(P)),
            Local = filename:join(Ctrl, "prep/upstream-" ++ BaseName ++ ".audit"),
            case file:read_file(Local) of
                {ok, B} ->
                    {
                        binary_to_list(P),
                        string:lowercase(
                            binary:encode_hex(crypto:hash(sha256, B))
                        ) =:= H
                    };
                {error, _} ->
                    {binary_to_list(P), missing}
            end
        end
     || #{<<"path">> := P, <<"sha256">> := H} <- Files
    ],
    Bad = [C || C <- Checks, element(2, C) =/= true],
    case Bad of
        [] -> ok;
        _ -> {mismatch, Bad}
    end.

format_violation(#{rule := Rule, path := Path}) ->
    #{rule => atom_to_binary(Rule), path => Path};
format_violation(V) when is_map(V) -> V.

%% ---- 薄封装 ----

imboy_state_read(Path) ->
    {ok, B} = file:read_file(Path),
    Map = json:decode(B),
    {ok, Map, crypto:hash(sha256, B)}.

%% PBT 结果摘要（JSON 化；counterexample/stack 等 term 类不在此落盘，
%% 由 P2-02 的 ETF 反例机制承接）。
format_replay({ok, still_fails}) -> <<"still_fails">>;
format_replay({ok, now_passes}) -> <<"now_passes">>;
format_replay({error, R}) -> unicode:characters_to_binary(io_lib:format("~0p", [R])).
format_pbt_result(R) ->
    M = maps:merge(
        #{completion_events => 0, performed => 0, requested => 0, random_started => 0},
        R
    ),
    #{
        <<"id">> => atom_to_binary(maps:get(id, M)),
        <<"verdict">> => atom_to_binary(maps:get(verdict, M)),
        <<"completion_events">> => maps:get(completion_events, M),
        <<"performed">> => maps:get(performed, M),
        <<"requested">> => maps:get(requested, M),
        <<"random_started">> => maps:get(random_started, M)
    }.

imboy_report_write(Path, Map) ->
    Bytes = iolist_to_binary(json:encode(Map)),
    Temp = Path ++ ".tmp." ++ os:getpid(),
    {ok, H} = file:open(Temp, [write, binary, raw, exclusive]),
    try
        ok = file:write(H, Bytes),
        ok = file:sync(H)
    after
        ok = file:close(H)
    end,
    ok = file:rename(Temp, Path),
    {ok, #{bytes => byte_size(Bytes), sha256 => crypto:hash(sha256, Bytes)}}.

%% ---- 退出码 ----

exit_code(#{<<"op">> := <<"guard">>, <<"verdict">> := <<"clean">>}, _) ->
    0;
exit_code(#{<<"op">> := <<"guard">>, <<"verdict">> := <<"violations">>}, _) ->
    1;
%% PBT：全性质 pass=0；任一性质非 pass=1（INCOMPLETE/失败同等对待）
exit_code(#{<<"op">> := <<"ct">>, <<"verdict">> := <<"all_passed">>}, _) ->
    0;
exit_code(#{<<"op">> := <<"ct">>}, _) ->
    1;
exit_code(#{<<"op">> := <<"pbt">>, <<"verdict">> := <<"all_passed">>}, _) ->
    0;
exit_code(#{<<"op">> := <<"pbt">>}, _) ->
    1;
%% P1-05 BUILD01：hit=0（Make 跳过硬复位）；miss=1（触发重建路径）；
%% seal 成功=0；异常路径经 main 的 catch → halt(2)。
%% 本组 dispatch 结果为原子键（仅打印，不落盘，与 admission/report 同风格）。
exit_code(#{op := build_check, verdict := hit}, _) ->
    0;
exit_code(#{op := build_check, verdict := miss}, _) ->
    1;
exit_code(#{op := build_seal, ok := true}, _) ->
    0;
exit_code(#{op := build_seal}, _) ->
    2;
%% full：资源回收失败 → 2（协议/资源异常，优先于测试结果）；根缺失 → 3；
%% 测试失败/取消 → 1；全绿 → 0
exit_code(#{<<"run_id">> := _, <<"cleanup">> := #{<<"ok">> := false}}, _) ->
    2;
exit_code(#{<<"run_id">> := _, <<"roots_missing">> := M}, _) when M > 0 -> 3;
exit_code(#{<<"run_id">> := _, <<"fail">> := F, <<"cancel">> := C, <<"roots_failed">> := R}, _) when
    F > 0; C > 0; R > 0
->
    1;
exit_code(#{<<"run_id">> := _}, _) ->
    0;
exit_code(#{<<"fail">> := 0, <<"cancel">> := _}, _) ->
    0;
exit_code(#{<<"fail">> := F}, _) when F > 0 -> 1;
exit_code(#{<<"cancel">> := C}, _) when C > 0 -> 3;
exit_code(_, _) ->
    0.

encode(Term) -> unicode:characters_to_binary(io_lib:format("~tp~n", [Term])).
