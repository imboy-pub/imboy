#!/usr/bin/env escript
%%! -noshell +A 4
%% IMBOY_ERLANG_QUALITY P1-03：单一 CLI 入口（纯 Erlang，escript）。
%% 上下文取自控制目录 STATE.json（可信），命令行不能抬权（PAR24）。
%% selftest 仅合成夹具：LP 期与正式期同口径，不链接项目 ebin/deps。
-mode(compile).

main(Args) ->
    try
        load_admission_modules(),
        CtxDir = resolve_control_dir(),
        Op = parse(Args),
        {Ctx, _StatePath} = load_context(CtxDir),
        enforce(Ctx, Op),
        case Op of
            eunit_full ->
                {ok, Repo} = file:get_cwd(),
                imboy_quality_full_budget:require_test_build(Repo),
                #{run_id := ReservedId} = imboy_quality_full_budget:transaction(
                    CtxDir, claim, full_budget_options()
                ),
                put(claimed_full_run_id, ReservedId);
            _ ->
                ok
        end,
        BuildDir0 = filename:join(CtxDir, "prep/build"),
        case lists:member(Op, [full_reserve, full_build_complete]) of
            true ->
                ok;
            false ->
                ok = filelib:ensure_dir(filename:join(BuildDir0, "x")),
                imboy_quality_compile:compile_all(BuildDir0),
                true = code:add_patha(BuildDir0)
        end,
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

load_admission_modules() ->
    lists:foreach(
        fun(M) ->
            Source = filename:join("tools/imboy_quality", atom_to_list(M) ++ ".erl"),
            Compiled = compile:file(Source, [binary, return_errors, return_warnings]),
            Bin =
                case Compiled of
                    {ok, M, B} -> B;
                    {ok, M, B, _Warnings} -> B;
                    Other -> erlang:error({admission_compile_failed, M, Other})
                end,
            {module, M} = code:load_binary(M, Source, Bin)
        end,
        [
            imboy_quality_cli,
            imboy_quality_budget,
            imboy_quality_build,
            imboy_quality_full,
            imboy_quality_guard,
            imboy_quality_state,
            imboy_quality_task_owner,
            imboy_quality_full_budget,
            imboy_quality_compile,
            imboy_quality_u_leaves
        ]
    ).

%% ---- 控制目录与上下文 ----

resolve_control_dir() ->
    %% 显式设置 IMBOY_QUALITY_CTRL 时优先：worktree 的 common dir 指向主仓，
    %% 默认共享控制目录会与主仓/其他会话的质量流程互踩（STATE 为活文件被
    %% 相互重写），需要独占时设置该 env 即可，不设时行为不变。
    case os:getenv("IMBOY_QUALITY_CTRL") of
        Dir when is_list(Dir), Dir =/= [] -> Dir;
        _ ->
            case imboy_quality_compile:git_common_dir() of
                {ok, Common} ->
                    case filelib:is_dir(Common) of
                        true -> filename:join(Common, "zcode-tasks/IMBOY_ERLANG_QUALITY");
                        false -> fallback_ctrl()
                    end;
                error ->
                    fallback_ctrl()
            end
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
    Ctx = imboy_quality_state:trusted_context(State),
    Ctx#{
        mode => maps:get(execution_mode, Ctx),
        goal => maps:get(execution_goal, Ctx),
        owner_verified => imboy_quality_full_budget:owner_verified(State)
    }.

%% ---- 参数与许可 ----

parse(["inspect"]) -> inspect;
parse(["selftest"]) -> selftest;
parse(["build-check"]) -> build_check;
parse(["build-seal"]) -> build_seal;
parse(["admission", "--check-only"]) -> admission_check_only;
parse(["guard"]) -> guard;
parse(["pbt", "--profile=quick"]) -> {pbt, quick};
parse(["pbt", "--profile=nightly"]) -> {pbt, nightly};
parse(["replay", "--next"]) -> replay_next;
parse(["full-reserve"]) -> full_reserve;
parse(["full-build-complete"]) -> full_build_complete;
parse(["eunit", "--full" | _]) -> eunit_full;
parse(["eunit", "--group=" ++ ModsStr | _]) -> {eunit_group, ModsStr};
parse(["ct", "--profile=critical"]) -> {ct, critical};
parse(["ci", "--final-gate"]) -> ci_final_gate;
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
permit_op({pbt, _}) -> pbt;
permit_op({ct, critical}) -> ct;
permit_op(ci_final_gate) -> ci_final_gate;
permit_op(replay_next) -> replay;
permit_op(full_reserve) -> eunit_full;
permit_op(full_build_complete) -> eunit_full;
permit_op(Op) -> Op.

%% ---- 执行 ----

dispatch(inspect, _Ctx, Ctrl) ->
    Info = #{
        op => inspect,
        control_dir => Ctrl,
        modules => [M || {M, _} <- imboy_quality_compile:sources()],
        cli => imboy_quality_cli:describe()
    },
    {Info, undefined};
dispatch(selftest, _Ctx, Ctrl) ->
    BuildDir = filename:join(Ctrl, "prep/build"),
    ok = filelib:ensure_dir(filename:join(BuildDir, "x")),
    imboy_quality_compile:compile_all(BuildDir),
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
    #{key := Key} = imboy_quality_build:build_key(Repo, imboy_quality_full_budget:build_profile()),
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
            #{key := Key} = imboy_quality_build:build_key(
                Repo, imboy_quality_full_budget:build_profile()
            ),
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
    imboy_quality_pbt_cli:prepare(
        Ctrl, fun imboy_quality_compile:erlc/2, fun imboy_quality_compile:needs_compile/2
    ),
    imboy_quality_pbt_cli:run(Profile, Ctrl);
dispatch({ct, critical}, _Ctx, Ctrl) ->
    ct_critical(Ctrl);
dispatch(ci_final_gate, _Ctx, Ctrl) ->
    ci_final_gate(Ctrl);
dispatch(replay_next, _Ctx, Ctrl) ->
    imboy_quality_pbt_cli:prepare(
        Ctrl, fun imboy_quality_compile:erlc/2, fun imboy_quality_compile:needs_compile/2
    ),
    imboy_quality_pbt_cli:replay(Ctrl);
dispatch(full_reserve, _Ctx, Ctrl) ->
    {imboy_quality_full_budget:transaction(Ctrl, reserve, full_budget_options()), undefined};
dispatch(full_build_complete, _Ctx, Ctrl) ->
    {imboy_quality_full_budget:transaction(Ctrl, build_complete, full_budget_options()), undefined};
dispatch(eunit_full, _Ctx, Ctrl) ->
    %% P1-10/P1-12 真实接线：分片执行完整 U（普通根 + 独占车道）。G-HEAVY 已由
    %% enforce 校验。环境：IMBOY_QUALITY_WORKERS（默认 2）、EUNIT_CONFIG
    %% （默认 config/sys.local）。每 worker 独立 http 端口副本（19980+K / 19970+K）。
    %% 资源准备（模板/克隆/独占资源/Garage 等）在整条命令计时之内（EUNIT_120S §2）。
    {ok, Repo} = file:get_cwd(),
    imboy_quality_full_budget:require_test_build(Repo),
    RunId = erase(claimed_full_run_id),
    true = is_list(RunId),
    Workers = env_int("IMBOY_QUALITY_WORKERS", 2),
    #{ordinary := Ordinary, excluded := Excl} = imboy_quality_full:inventory(Repo),
    Est = load_estimates(Ctrl),
    with_config_receipts(Ctrl, RunId, fun() ->
        Prepared = imboy_quality_env:prepare(Repo, Ctrl, Workers),
        Opts = run_opts(Repo, Ctrl, Workers, Prepared, Excl, #{estimates => Est}),
        with_resources(Prepared, fun() ->
            {Result0, OutDir} = imboy_quality_full:run(Ordinary, Opts, RunId),
            Result = Result0#{
                <<"resources">> => maps:get(report, Prepared),
                <<"input_binding">> => <<"NOT_VERIFIED">>
            },
            {Result, OutDir}
        end)
    end);
dispatch({eunit_group, ModsStr}, _Ctx, Ctrl) ->
    %% P1-10 诊断入口（FRAMEWORK_DESIGN §2：--group 仅诊断，报告标 PARTIAL，
    %% 不产生 full PASS）。普通根在单 worker 单 VM 顺序执行；独占根走独占车道
    %% （各自 fresh VM）。用于修复验证与定向取证；不消耗 full 预算。
    {ok, Repo} = file:get_cwd(),
    imboy_quality_full_budget:require_test_build(Repo),
    #{ordinary := Ordinary, excluded := Excl} = imboy_quality_full:inventory(Repo),
    Mods = imboy_quality_cli:parse_group(ModsStr),
    Unknown = [M || M <- Mods, not lists:member(M, Ordinary ++ Excl)],
    case Unknown of
        [] -> ok;
        _ -> erlang:error({roots_not_in_u, Unknown})
    end,
    Workers = env_int("IMBOY_QUALITY_WORKERS", 1),
    RunId = imboy_quality_full_budget:run_id(),
    with_config_receipts(Ctrl, RunId, fun() ->
        Prepared = imboy_quality_env:prepare(Repo, Ctrl, Workers),
        Opts = run_opts(Repo, Ctrl, Workers, Prepared, [M || M <- Mods, lists:member(M, Excl)], #{
            estimates => load_estimates(Ctrl), out_prefix => "group-"
        }),
        OrdPart = [M || M <- Mods, lists:member(M, Ordinary)],
        with_resources(Prepared, fun() ->
            {Result0, OutDir} = imboy_quality_full:run(OrdPart, Opts, RunId),
            Result = Result0#{
                <<"scope">> => <<"PARTIAL">>,
                <<"op">> => <<"eunit_group">>,
                <<"resources">> => maps:get(report, Prepared)
            },
            {Result, OutDir}
        end)
    end);
dispatch(eunit_group, _Ctx, _Ctrl) ->
    erlang:error(eunit_group_requires_modlist).

%% 资源生命周期包裹：正常/异常路径的 after 回收 + SIGTERM 信号路径回收（被 TERM 的控制器
%% 不会执行 after，2026-10-29 实证遗留 16 库/10 配置/1 Garage 守护进程）。
with_config_receipts(Ctrl, RunId, Fun) ->
    ok = imboy_quality_resources:begin_config_receipts(Ctrl, RunId),
    erase(quality_cleanup_finalized),
    try
        Fun()
    after
        case erase(quality_cleanup_finalized) of
            true -> ok;
            _ -> imboy_quality_full:fallback_config_cleanup()
        end
    end.

with_resources(Prepared, Fun) ->
    ok = imboy_quality_signal:install(fun() ->
        imboy_quality_full:fallback_config_cleanup(),
        imboy_quality_env:release_once(Prepared)
    end),
    try
        {Result0, OutDir} = Fun(),
        Final = imboy_quality_full:finalize_run(
            OutDir,
            Result0,
            fun() -> imboy_quality_env:release(Prepared) end,
            fun imboy_quality_resources:cleanup_config_receipts/0
        ),
        put(quality_cleanup_finalized, true),
        Final
    after
        case get(quality_cleanup_finalized) of
            true ->
                ok;
            _ ->
                imboy_quality_full:fallback_config_cleanup(),
                imboy_quality_env:release_once(Prepared)
        end,
        imboy_quality_signal:uninstall()
    end.

%% full/group 共用执行选项：独占根按根 env 与按根 -config（无专属配置则沿用 worker 0）。
%% deadline：普通组默认 90 分钟——talk_record 网关根真实等待 ~1h 的上传能力过期
%% （imboy_quality_full ?LONG_ROOT_HINTS）；独占根默认 30 分钟。均可由环境变量覆盖。
run_opts(Repo, Ctrl, Workers, Prepared, Excl, Extra) ->
    %% 2026-11-09 r12 框架缺陷修复：worker_config(Repo, K, Prepared) 对同一 K
    %% 确定性（同 clone/同端口），但一次 full 内会被多处重复调用——group 的
    %% rotate(K)、run_standalone_fallback/direct 与 run_exclusive 兜底的 CfgFn(0)。
    %% config_receipts 的 ets:insert_new 路径防重对同路径第二次写入以
    %% {badmatch,false} 拒绝（attempt8 group 完成后死于 run_standalone_fallback
    %% 实证）。按 K 记忆化：语义不变（同 K 同 clone），防重守卫保留。
    WorkerCfg = worker_config_memo(Repo, Prepared),
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
                    error -> WorkerCfg(0)
                end
            end,
            worker_config_fn => WorkerCfg
        },
        Extra
    ).

worker_config_memo(Repo, Prepared) ->
    fun(K) ->
        Key = {quality_worker_config_memo, Repo, K},
        case persistent_term:get(Key, undefined) of
            undefined ->
                Cfg = imboy_quality_env:worker_config(Repo, K, Prepared),
                persistent_term:put(Key, Cfg),
                Cfg;
            Cfg ->
                Cfg
        end
    end.

%% ---- full/group 辅助 ----

%% test-build 完整性预检：legacy 入口 eunit 目标以 test-build 为前置条件；
%% 标记缺失说明 ebin 可能是非 -DTEST 构建（白盒导出缺失→大面积 undef）。

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

%% ---- ct --profile=critical（P2-09 链接回：P1 环境 1-worker 等价 + fail-closed 收据）----
-define(CT_CRITICAL_SUITES, [
    msg_delivery_SUITE,
    msg_ack_logic_SUITE,
    imboy_quality_ct02_org_boundary_SUITE,
    imboy_quality_ct03_migration_SUITE
]).

ct_critical(Ctrl) ->
    {ok, Repo} = file:get_cwd(),
    ok = ct_code_paths(Repo),
    %% 数据面沿用 HEAD/r5 语义（4323 容器：扩展齐备，模板 reused）+ 1 专用
    %% 克隆库。SUITE 改跑 peer 全功能 VM（与 eunit worker 同机制：worker
    %% config 经 -config 与 IMBOY_TEST_CONFIG 双通道注入），绕开 escript
    %% 控制器 VM 的 app 模型限制（code:priv_dir bad_name）。
    Args = imboy_quality_fixture:pg_args_from_config(
        filename:join(Repo, "config/sys.local.config")
    ),
    {ok, C} = imboy_quality_fixture:connect(Args#{database => "postgres"}),
    Tpl = imboy_quality_resources:ensure_candidate_template(C, Args, Repo),
    Template = maps:get(name, Tpl),
    SrcLiteral = maps:get(database, Args),
    Db = lists:flatten(io_lib:format("imboy_quality_ctcrit_~s", [os:getpid()])),
    ok = imboy_quality_fixture:clone_live(C, Args, Template, Db),
    ok = imboy_quality_resources:begin_config_receipts(
        Ctrl, iolist_to_binary(["ctcrit-", os:getpid()])
    ),
    ConfigFile = imboy_quality_resources:write_config(
        Repo, "config/sys.local", os:getpid(), 0, {SrcLiteral, Db}
    ),
    ConfigPath = ConfigFile ++ ".config",
    try
        ct_critical_run(Repo, Ctrl, ConfigPath)
    after
        ct_drop_clone(C, Args, Db)
    end.

ct_drop_clone(C, _Args, Db) ->
    catch imboy_quality_fixture:q(
        C,
        iolist_to_binary(["DROP DATABASE IF EXISTS ", Db, " WITH (FORCE)"]),
        []
    ),
    ok.

ct_apply_config(Path) ->
    {ok, Terms0} = file:consult(Path),
    %% worker config 由 write_config 产出为「单个 list term」——consult 得到
    %% [[{App, L}, ...]]；兼容标准 [{App, L}, ...] 逐项形态，先展平再注入。
    Terms = lists:append(
        [
            case T of
                L when is_list(L) -> L;
                X -> [X]
            end
         || T <- Terms0
        ]
    ),
    lists:foreach(
        fun
            ({App, EnvList}) when is_list(EnvList) ->
                application:load(App),
                lists:foreach(fun({K, V}) -> application:set_env(App, K, V) end, EnvList);
            (_) ->
                ok
        end,
        Terms
    ),
    ok.

ct_critical_run(Repo, Ctrl, ConfigPath) ->
    Before = ct_head(Repo),
    %% OTP priv_dir 合同：code:priv_dir 依赖 lib 布局（lib/<App>-<Vsn>/{ebin,priv}）。
    %% worktree 是扁平 ebin 布局——裸 VM 里 priv_dir(imboy) 恒 bad_name（实证
    %% erl/escript/peer 三处一致），imboy_app boot 的 product_terminology:init
    %% 必炸。建 runtime/ctlib/imboy-<vsn>/{ebin,priv} symlink 布局并置于
    %% -pa 首位，lib 索引命中后 priv_dir 落到真 priv（实测通过）。
    Vsn = "1.0.0-alpha.84",
    LibRoot = filename:join([Ctrl, "runtime", "ctlib"]),
    LibApp = filename:join(LibRoot, "imboy-" ++ Vsn),
    ok = filelib:ensure_dir(filename:join(LibApp, "x")),
    LibEbin = filename:join(LibApp, "ebin"),
    LibPriv = filename:join(LibApp, "priv"),
    case file:read_link_info(LibEbin) of
        {ok, _} -> ok;
        _ -> ok = file:make_symlink(filename:join(Repo, "ebin"), LibEbin)
    end,
    case file:read_link_info(LibPriv) of
        {ok, _} -> ok;
        _ -> ok = file:make_symlink(filename:join(Repo, "priv"), LibPriv)
    end,
    %% peer 全功能 VM：与 eunit worker 同机制（worker config 双通道注入）
    DepsPaths = filelib:wildcard(filename:join([Repo, "deps", "*", "ebin"])),
    FrameworkDir = filename:dirname(code:which(imboy_quality_ct)),
    {ok, Peer, Node} = peer:start_link(#{
        env => [{"IMBOY_TEST_CONFIG", ConfigPath}],
        connection => {{127, 0, 0, 1}, 0},
        args =>
            [
                "-noshell",
                "-noinput",
                "-boot",
                "no_dot_erlang",
                "+S",
                "2",
                "-pa",
                LibEbin,
                "-pa",
                filename:join(Repo, "ebin"),
                "-pa",
                FrameworkDir,
                "-pa",
                filename:join(Repo, "test")
            ] ++
            lists:append([["-pa", D] || D <- DepsPaths]) ++
            ["-config", ConfigPath]
    }),
    try
        Raw =
            peer:call(
                Peer,
                imboy_quality_ct,
                run_suites_detailed,
                [?CT_CRITICAL_SUITES, filename:join(Repo, "test")],
                420000
            ),
        ok = peer:stop(Peer),
        After = ct_head(Repo),
        {Binding, Fingerprint} = ct_binding(Ctrl, Repo, After),
        Results = [ct_suite_receipt(S, M) || {S, M} <- Raw],
        Total = length(Results),
        Passed = length([R || R <- Results, maps:get(<<"verdict">>, R) =:= <<"pass">>]),
        Drift = Before =/= After,
        Verdict =
            case {Drift, Binding, Total =:= Passed andalso Passed > 0} of
                {true, _, _} -> <<"candidate_drift">>;
                {_, <<"VERIFIED">>, true} -> <<"all_passed">>;
                {_, <<"VERIFIED">>, false} -> <<"failed">>;
                {_, _, true} -> <<"input_unverified">>;
                {_, _, false} -> <<"failed">>
            end,
        RunId = string:lowercase(binary:encode_hex(crypto:strong_rand_bytes(16))),
        Receipt = #{
            <<"op">> => <<"critical_ct">>,
            <<"profile">> => <<"critical">>,
            <<"run_id">> => RunId,
            <<"candidate_before">> => Before,
            <<"candidate_sha">> => After,
            <<"input_fingerprint">> => Fingerprint,
            <<"input_binding">> => Binding,
            <<"verdict">> => Verdict,
            <<"total">> => Total,
            <<"passed">> => Passed,
            <<"results">> => Results
        },
        Path = filename:join([
            Ctrl, "evidence", "ct-critical-" ++ binary_to_list(RunId) ++ ".json"
        ]),
        ok = file:write_file(Path, json:encode(Receipt)),
        {Receipt, Path}
    catch
        Class:Reason:Stack ->
            catch peer:stop(Peer),
            erlang:error({ct_peer_failed, Class, Reason, Stack})
    end.

ct_head(Repo) ->
    {0, Output} = imboy_quality_guard:git(Repo, ["rev-parse", "HEAD"]),
    iolist_to_binary(string:trim(Output)).

%% 与 pbt_cli binding/3 同判据：准入指纹==实时指纹 且 准入 candidate_sha==HEAD
%% 才发 VERIFIED；否则 NOT_VERIFIED（verdict 降级，不伪造 all_passed）。
ct_binding(Ctrl, Repo, Head) ->
    case file:read_file(filename:join(Ctrl, "STATE.json")) of
        {ok, Bytes} ->
            State = json:decode(Bytes),
            Adm = maps:get(<<"current_admission">>, State, #{}),
            Stored = maps:get(<<"input_fingerprint">>, Adm, <<>>),
            StoredSha = maps:get(<<"candidate_sha">>, Adm, <<>>),
            Live = imboy_quality_state:candidate_input_fingerprint(Repo),
            case Stored =:= Live andalso StoredSha =:= Head of
                true -> {<<"VERIFIED">>, Live};
                false -> {<<"NOT_VERIFIED">>, Live}
            end;
        _ ->
            {<<"NOT_VERIFIED">>, <<>>}
    end.

ct_suite_receipt(S, M) ->
    #{
        <<"suite">> => atom_to_binary(S),
        <<"verdict">> => atom_to_binary(maps:get(verdict, M, fail)),
        <<"passed_cases">> => maps:get(passed_cases, M, 0),
        <<"failed_cases">> => maps:get(failed_cases, M, 0),
        <<"skip">> => maps:get(skip, M, 0),
        <<"reason">> => ct_reason(maps:get(reason, M, null))
    }.

ct_reason(R) when is_binary(R); is_atom(R); is_number(R) -> R;
ct_reason(R) -> unicode:characters_to_binary(io_lib:format("~0p", [R])).

ct_code_paths(Repo) ->
    true = code:add_patha(filename:join(Repo, "ebin")),
    lists:foreach(
        fun(D) -> true = code:add_patha(D) end,
        lists:sort(filelib:wildcard(filename:join([Repo, "deps", "*", "ebin"])))
    ),
    true = code:add_patha(filename:join(Repo, "test")),
    true = code:add_patha(filename:join(Repo, "include")),
    ok.

%% ---- ci --final-gate（P2-10/12 判据接线：四收据 enrich + Ctx 组装 + gate）----
-define(GATE_RECEIPTS, [
    {<<"guard">>, "guard-latest.json"},
    {<<"pbt_quick">>, "pbt-quick-gate.json"},
    {<<"pbt_nightly">>, "pbt-nightly-gate.json"},
    {<<"critical_ct">>, "ct-critical-gate.json"}
]).

ci_final_gate(Ctrl) ->
    {ok, Repo} = file:get_cwd(),
    {ok, Bytes} = file:read_file(filename:join(Ctrl, "STATE.json")),
    State = json:decode(Bytes),
    Adm = maps:get(<<"current_admission">>, State, #{}),
    {0, HeadRaw} = imboy_quality_guard:git(Repo, ["rev-parse", "HEAD"]),
    Head = iolist_to_binary(string:trim(HeadRaw)),
    %% 1) 最新 gate 级收据（gate 形态候选：binding=VERIFIED + verdict=all_passed）
    Receipts0 = [
        begin
            P = latest_receipt(Ctrl, Prefix),
            true = is_list(P) orelse erlang:error({receipt_missing, Op}),
            {ok, JB} = file:read_file(P),
            {Op, json:decode(JB), P}
        end
     || {Op, Prefix} <- [
            {<<"guard">>, "guard-latest"},
            {<<"pbt_quick">>, "pbt-quick"},
            {<<"pbt_nightly">>, "pbt-nightly"},
            {<<"critical_ct">>, "ct-critical"}
        ]
    ],
    %% 2) 豁免记录（用户授权可追溯）
    Rec = maps:get(<<"approved_recovery_20261104">>, State, #{}),
    Auth = maps:get(<<"phase2_entry_authorization">>, Rec, #{}),
    Authority = maps:get(<<"authority">>, Auth, <<>>),
    WaiverRef = <<"STATE.approved_recovery_20261104.phase2_entry_authorization">>,
    %% 3) U 叶级期望表（r5 全景实测的候选 U：roots+leaves 同源）
    %% HARDENING（2026-11-10 D1/D2/D3）：期望集必须就是被准入的那一份——
    %% 文件字节 sha256 必须等于 current_admission.dynamic_U；缺失/不可读/
    %% 未绑定/指纹不符/解码失败均为具名 reason，由 main/1 打印后 halt(2)。
    LeafMap =
        case imboy_quality_u_leaves:load(Ctrl, Adm) of
            {ok, M} -> M;
            {error, Reason} -> erlang:error(Reason)
        end,
    Roots = lists:sort(maps:keys(LeafMap)),
    %% 4) Ctx 组装
    Ctx = #{
        head => Head,
        input_fingerprint => maps:get(<<"input_fingerprint">>, Adm, <<>>),
        u_fingerprint => maps:get(<<"dynamic_U">>, Adm, <<>>),
        waived_ops => [{<<"eunit_full">>, Authority}],
        <<"waiver_ref">> => WaiverRef,
        artifacts => [
            {<<"guard">>, true},
            {<<"pbt_quick">>, true},
            {<<"pbt_nightly">>, true},
            {<<"critical_ct">>, true}
        ],
        expected_properties => maps:from_list([
            {Id,
                case Id of
                    <<"q01_tsid_reserve_sequences">> -> sequence;
                    _ -> ordinary
                end}
         || Id <- imboy_quality_ci:business_ids()
        ]),
        expected_full_roots => Roots,
        expected_leaf_ids => LeafMap,
        expected_critical_suites => imboy_quality_ci:critical_suites(),
        expected_guard_checks => [<<"guard_verdict_clean">>]
    },
    %% 5) 收据 enrich 到 gate 合同
    Results = [enrich_receipt(Op, R, Path, Ctx) || {Op, R, Path} <- Receipts0],
    %% 6) 判定
    Verdict = imboy_quality_ci:gate(Results, Ctx),
    GateReceipt = #{
        <<"op">> => <<"ci_final_gate">>,
        <<"head">> => Head,
        <<"input_fingerprint">> => maps:get(input_fingerprint, Ctx),
        <<"u_fingerprint">> => maps:get(u_fingerprint, Ctx),
        <<"waiver_ref">> => WaiverRef,
        <<"verdict">> =>
            case Verdict of
                pass -> <<"pass">>;
                {fail, Reasons} -> iolist_to_binary(io_lib:format("~0p", [Reasons]))
            end,
        <<"local_candidate_pass">> => Verdict =:= pass,
        <<"results">> => Results
    },
    Ts = calendar:system_time_to_rfc3339(erlang:system_time(millisecond), [{unit, millisecond}]),
    Path = filename:join([
        Ctrl,
        "evidence",
        "final-gate-" ++ re:replace(Ts, "[:.]", "-", [global, {return, list}]) ++ ".json"
    ]),
    ok = file:write_file(Path, json:encode(GateReceipt)),
    {GateReceipt, Path}.

latest_receipt(Ctrl, Prefix) ->
    Dir = filename:join(Ctrl, "evidence"),
    Files = filelib:wildcard(filename:join(Dir, Prefix ++ "*.json")),
    true = Files =/= [],
    %% 字典序≠时间序（run_id 为随机 hex）——按修改时间取最新。
    hd(lists:sort(fun(A, B) -> filelib:last_modified(A) >= filelib:last_modified(B) end, Files)).

%% 收据→gate 合同映射：gate 合同是 Erlang term 形态（atom 键），JSON 解码
%% 产 binary 键——enrich 做键转换 + 补 u_fingerprint/evidence_ref/cleanup_ok/
%% assertions_ok + per-item executed/passed/fail/skip/cancel。
enrich_receipt(Op, R, Path, Ctx) ->
    Base = #{
        op => Op,
        candidate_sha => maps:get(head, Ctx),
        input_fingerprint => maps:get(input_fingerprint, Ctx),
        u_fingerprint => maps:get(u_fingerprint, Ctx),
        evidence_ref => unicode:characters_to_binary(Path),
        verdict => <<"all_passed">>,
        fail => 0,
        skip => 0,
        cancel => 0,
        roots_missing => 0,
        cleanup_ok => true,
        assertions_ok => true
    },
    case Op of
        <<"guard">> ->
            <<"clean">> = maps:get(<<"verdict">>, R),
            Base#{
                total => 1,
                passed => 1,
                results => [
                    #{
                        id => <<"guard_verdict_clean">>,
                        executed => 1,
                        passed => 1,
                        fail => 0,
                        skip => 0,
                        cancel => 0
                    }
                ]
            };
        <<"pbt_quick">> ->
            enrich_pbt(Base, R, quick);
        <<"pbt_nightly">> ->
            enrich_pbt(Base, R, nightly);
        <<"critical_ct">> ->
            Items = [
                #{
                    id => maps:get(<<"suite">>, It),
                    executed => maps:get(<<"passed_cases">>, It),
                    passed => maps:get(<<"passed_cases">>, It),
                    fail => 0,
                    skip => maps:get(<<"skip">>, It),
                    cancel => 0
                }
             || It <- maps:get(<<"results">>, R)
            ],
            Base#{total => length(Items), passed => length(Items), results => Items};
        _ ->
            Base
    end.

enrich_pbt(Base, R, _Profile) ->
    Items0 = maps:get(<<"results">>, R),
    Items = [
        #{
            id => maps:get(<<"id">>, It),
            executed => maps:get(<<"performed">>, It),
            passed => maps:get(<<"performed">>, It),
            fail => 0,
            skip => 0,
            cancel => 0,
            requested => maps:get(<<"requested">>, It),
            performed => maps:get(<<"performed">>, It),
            completion => maps:get(<<"completion_events">>, It),
            random_started => maps:get(<<"random_started">>, It),
            witnesses_passed_count => maps:get(<<"witnesses_passed_count">>, It)
        }
     || It <- Items0
    ],
    Base#{total => length(Items), passed => length(Items), results => Items}.

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

full_budget_options() ->
    #{
        profile => imboy_quality_full_budget:build_profile(),
        config => sys_config_base(),
        receipt => receipt_path()
    }.

imboy_state_read(Path) ->
    {ok, B} = file:read_file(Path),
    {ok, Sidecar} = file:read_file(Path ++ ".sha256"),
    Hash = crypto:hash(sha256, B),
    true = string:trim(Sidecar) =:= string:lowercase(binary:encode_hex(Hash)),
    Map = json:decode(B),
    true = is_map(Map),
    {ok, Map, Hash}.

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
exit_code(#{<<"op">> := <<"critical_ct">>, <<"verdict">> := <<"all_passed">>}, _) ->
    0;
exit_code(#{<<"op">> := <<"critical_ct">>}, _) ->
    1;
exit_code(#{<<"op">> := Op}, _) when Op =:= <<"pbt">>; Op =:= <<"replay">> ->
    2;
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
exit_code(#{<<"op">> := <<"eunit_group">>} = Result, _) ->
    imboy_quality_cli:group_result_exit_code(Result);
%% full：资源回收失败 → 2（协议/资源异常，优先于测试结果）；根缺失 → 3；
%% 测试失败/取消 → 1；全绿 → 0
exit_code(
    #{
        <<"run_id">> := Id,
        <<"cleanup">> := #{<<"ok">> := true},
        <<"input_binding">> := <<"VERIFIED">>,
        <<"roots_missing">> := 0,
        <<"roots_failed">> := 0,
        <<"roots_total">> := Total,
        <<"roots_ok">> := Total,
        <<"pass">> := Pass,
        <<"fail">> := 0,
        <<"skip">> := 0,
        <<"cancel">> := 0
    },
    _
) when
    is_binary(Id),
    byte_size(Id) > 0,
    is_integer(Total),
    Total > 0,
    is_integer(Pass),
    Pass > 0
->
    0;
exit_code(#{<<"run_id">> := _}, _) ->
    2;
exit_code(#{<<"fail">> := _} = Result, _) ->
    imboy_quality_cli:result_exit_code(Result);
exit_code(#{<<"op">> := <<"selftest">>}, _) ->
    2;
exit_code(_, _) ->
    0.

encode(Term) -> unicode:characters_to_binary(io_lib:format("~tp~n", [Term])).
