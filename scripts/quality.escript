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
parse(["selftest"]) -> selftest;
parse(["admission", "--check-only"]) -> admission_check_only;
parse(["guard"]) -> guard;
parse(["eunit", "--full" | _]) -> eunit_full;
parse(["eunit", "--group=" ++ ModsStr | _]) -> {eunit_group, ModsStr};
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
dispatch(eunit_full, _Ctx, Ctrl) ->
    %% P1-10/P1-12 真实接线：分片执行完整 U（普通根 + 独占车道）。G-HEAVY 已由
    %% enforce 校验。环境：IMBOY_QUALITY_WORKERS（默认 2）、EUNIT_CONFIG
    %% （默认 config/sys.local）。每 worker 独立 http 端口副本（19980+K / 19970+K）。
    %% 资源准备（TALK_OWNED 维护库等）在整条命令计时之内（EUNIT_120S §2）。
    {ok, Repo} = file:get_cwd(),
    require_test_build(Repo),
    RunId = run_id(),
    Workers = env_int("IMBOY_QUALITY_WORKERS", 2),
    #{ordinary := Ordinary, excluded := Excl} = imboy_quality_full:inventory(Repo),
    Est = load_estimates(Ctrl),
    Prepared = prepare_resources(Repo, Ctrl, Workers),
    Opts = #{
        repo => Repo,
        ctrl => Ctrl,
        worker_count => Workers,
        schedulers => 2,
        deadline_ms => 3000000,
        process_limit => 32768,
        estimates => Est,
        env => full_env(Repo) ++ maps:get(env, Prepared),
        spec_wrapper => spec_wrapper(Repo),
        exclusive_roots => Excl,
        exclusive_env_fn => fun(M) -> maps:get(M, maps:get(exclusive_env, Prepared), []) end,
        worker_config_fn => fun(K) -> config_for_worker(Repo, K, Prepared) end
    },
    try
        {Result0, OutDir} = imboy_quality_full:run(Ordinary, Opts, RunId),
        Cleanup = release_resources(Prepared),
        Result = Result0#{
            <<"resources">> => maps:get(report, Prepared),
            <<"cleanup">> => Cleanup
        },
        {ok, _} = imboy_report_write(filename:join(OutDir, "result.json"), Result),
        {Result, OutDir}
    after
        cleanup_worker_configs(Repo),
        release_resources_once(Prepared)
    end;
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
    Prepared = prepare_resources(Repo, Ctrl, 1),
    Opts = #{
        repo => Repo,
        ctrl => Ctrl,
        worker_count => 1,
        schedulers => 2,
        deadline_ms => 3000000,
        process_limit => 32768,
        estimates => #{},
        out_prefix => "group-",
        env => full_env(Repo) ++ maps:get(env, Prepared),
        spec_wrapper => spec_wrapper(Repo),
        exclusive_roots => [M || M <- Mods, lists:member(M, Excl)],
        exclusive_env_fn => fun(M) -> maps:get(M, maps:get(exclusive_env, Prepared), []) end,
        worker_config_fn => fun(K) -> config_for_worker(Repo, K, Prepared) end
    },
    OrdPart = [M || M <- Mods, lists:member(M, Ordinary)],
    try
        {Result0, OutDir} = imboy_quality_full:run(OrdPart, Opts, run_id()),
        Cleanup = release_resources(Prepared),
        Result = Result0#{
            <<"scope">> => <<"PARTIAL">>,
            <<"op">> => <<"eunit_group">>,
            <<"resources">> => maps:get(report, Prepared),
            <<"cleanup">> => Cleanup
        },
        {ok, _} = imboy_report_write(filename:join(OutDir, "result.json"), Result),
        {Result, OutDir}
    after
        cleanup_worker_configs(Repo),
        release_resources_once(Prepared)
    end;
dispatch(eunit_group, _Ctx, _Ctrl) ->
    erlang:error(eunit_group_requires_modlist).

%% ---- 资源准备（纯 OTP + epgsql，直连项目配置的真实 PG） ----

%% TALK_OWNED 合同（test/common/talk_record_owned_eunit_fixture.erl）：维护库的
%% database COMMENT 为 marker、PG system_identifier 与 env 一致、RUN_ROOT 位于
%% SCRATCH_ROOT 之下且全链无 symlink。legacy 入口从不提供这些变量（26 个 budgets
%% 根在 legacy 中只能 owned_talk_missing_env 失败）；本入口在计时内自动准备，
%% 使这些义务真实执行。失败 fail-closed（不以空环境假绿）。
prepare_resources(Repo, Ctrl, Workers) ->
    true = code:add_patha(filename:join(Repo, "deps/epgsql/ebin")),
    {ok, _} = application:ensure_all_started(epgsql),
    Base =
        case os:getenv("EUNIT_CONFIG") of
            false -> "config/sys.local";
            B -> B
        end,
    Args = imboy_quality_fixture:pg_args_from_config(filename:join(Repo, Base ++ ".config")),
    Maint = "imboy_quality_talk_owned_maint",
    Marker = "IMBOY_ERLANG_QUALITY:talk_owned:maint",
    {ok, C} = imboy_quality_fixture:connect(Args#{database => "postgres"}),
    try
        ok = imboy_quality_fixture:ensure_marked_database(C, Maint, Marker),
        SysId = imboy_quality_fixture:system_identifier(C),
        Scratch = filename:join(Ctrl, "runtime/talk_owned"),
        Run = filename:join(Scratch, "run"),
        ok = filelib:ensure_dir(filename:join(Run, "x")),
        %% 每 worker 私有克隆库（组间 DB 隔离；opt-run5 共用库交叉干扰的修复）：
        %% 从已迁移到 head 的配置库 TEMPLATE 克隆，结束后 release_resources 回收。
        SrcDb = maps:get(database, Args),
        Clones = maps:from_list([
            begin
                Name = lists:flatten(io_lib:format("imboy_quality_w~p_~s", [K, os:getpid()])),
                ok = imboy_quality_fixture:clone_live(C, Args, SrcDb, Name),
                {K, Name}
            end
         || K <- lists:seq(0, Workers - 1)
        ]),
        %% 独占车道按根资源（仅注入对应根的 fresh VM，见 imboy_quality_full:run_exclusive）：
        %%  - group_schedule_remind_pg_tests：独占库（从已迁移源库克隆）+ 合同 COMMENT
        %%    'imboy-delivery-v3.2 merge schedule%exclusive%' + PG*/IMBOY_TEST_* 变量；
        %%  - AG31（agent_grant_pg / agent_run_pg / agent_recovery_pg_tests /
        %%    agent_tool_authorizer_pg_tests）：各一个一次性隔离库（迁移头 >= 00000132）
        %%    + AG31_PG_* 变量。attv5/W403 清单与 T20 冻结注册表属专用控制器，不在此伪造。
        Pid = os:getpid(),
        GsDb = "imboy_quality_gsremind_" ++ Pid,
        ok = imboy_quality_fixture:clone_live(C, Args, SrcDb, GsDb),
        ok = imboy_quality_fixture:comment_database(
            C, GsDb, "imboy-delivery-v3.2 merge schedule IMBOY_ERLANG_QUALITY exclusive " ++ Pid
        ),
        Ag31 = [
            {R, "imboy_quality_ag31_" ++ Tag ++ "_" ++ Pid}
         || {R, Tag} <- [
                {agent_grant_pg, "grant"},
                {agent_run_pg, "run"},
                {agent_recovery_pg_tests, "recovery"},
                {agent_tool_authorizer_pg_tests, "toolauth"}
            ]
        ],
        [ok = imboy_quality_fixture:clone_live(C, Args, SrcDb, N) || {_, N} <- Ag31],
        Host = maps:get(host, Args),
        PortS = integer_to_list(maps:get(port, Args)),
        User = maps:get(username, Args),
        Pass = maps:get(password, Args),
        ExclusiveEnv = maps:from_list(
            [
                {group_schedule_remind_pg_tests, [
                    {"PGHOST", Host},
                    {"PGPORT", PortS},
                    {"PGUSER", User},
                    {"PGPASSWORD", Pass},
                    {"PGDATABASE", GsDb},
                    {"IMBOY_TEST_EXCLUSIVE_DATABASE", GsDb},
                    {"IMBOY_TEST_PG_SYSTEM_ID", SysId}
                ]}
            ] ++
                [
                    {R, [
                        {"AG31_PG_HOST", Host},
                        {"AG31_PG_PORT", PortS},
                        {"AG31_PG_USER", User},
                        {"AG31_PG_PASSWORD", Pass},
                        {"AG31_PG_DB", N}
                    ]}
                 || {R, N} <- Ag31
                ]
        ),
        DropDbs = [GsDb | [N || {_, N} <- Ag31]],
        Env = [
            {"TALK_OWNED_PG_HOST", maps:get(host, Args)},
            {"TALK_OWNED_PG_PORT", integer_to_list(maps:get(port, Args))},
            {"TALK_OWNED_PG_USER", maps:get(username, Args)},
            {"TALK_OWNED_PG_PASSWORD", maps:get(password, Args)},
            {"TALK_OWNED_MAINT_DB", Maint},
            {"TALK_OWNED_PG_MARKER", Marker},
            {"TALK_OWNED_PG_SYSTEM_ID", SysId},
            {"TALK_OWNED_RUN_ROOT", Run},
            {"TALK_OWNED_SCRATCH_ROOT", Scratch}
        ],
        Report = #{
            <<"talk_owned">> => #{
                <<"maint_db">> => list_to_binary(Maint),
                <<"marker">> => list_to_binary(Marker),
                <<"pg">> => list_to_binary(
                    maps:get(host, Args) ++ ":" ++ integer_to_list(maps:get(port, Args))
                ),
                <<"system_id">> => list_to_binary(SysId),
                <<"run_root">> => list_to_binary(Run)
            },
            <<"worker_db_clones">> => #{
                <<"source">> => list_to_binary(SrcDb),
                <<"clones">> => [list_to_binary(N) || {_, N} <- lists:sort(maps:to_list(Clones))]
            },
            <<"exclusive_resources">> => #{
                <<"group_schedule_remind_pg_tests">> => list_to_binary(GsDb),
                <<"ag31">> => [list_to_binary(N) || {_, N} <- Ag31]
            }
        },
        #{
            env => Env,
            report => Report,
            pg_args => Args,
            source_db => SrcDb,
            clones => Clones,
            exclusive_env => ExclusiveEnv,
            drop_dbs => DropDbs
        }
    after
        try
            epgsql:close(C)
        catch
            _:_ -> ok
        end
    end.

%% 回收每 worker 克隆库（worker VM 已退出，后端断开有界等待）。回收失败不掩盖：
%% 记入结果并由退出码体现（FRAMEWORK_DESIGN §5：无法证明回收成功则本轮非通过）。
release_resources(#{pg_args := Args, clones := Clones} = Prepared) ->
    put(resources_released, true),
    {ok, C} = imboy_quality_fixture:connect(Args#{database => "postgres"}),
    try
        Names =
            [N || {_, N} <- lists:sort(maps:to_list(Clones))] ++ maps:get(drop_dbs, Prepared, []),
        Results = [
            {Name,
                try imboy_quality_fixture:drop_owned(C, Args, Name) of
                    ok -> ok
                catch
                    Class:Reason -> {Class, Reason}
                end}
         || Name <- Names
        ],
        Failed = [
            iolist_to_binary(io_lib:format("~s: ~0p", [N, R]))
         || {N, R} <- Results, R =/= ok
        ],
        #{
            <<"clones_dropped">> => [list_to_binary(N) || {N, ok} <- Results],
            <<"clone_drop_failed">> => Failed,
            <<"ok">> => Failed =:= []
        }
    after
        try
            epgsql:close(C)
        catch
            _:_ -> ok
        end
    end;
release_resources(_) ->
    #{<<"clones_dropped">> => [], <<"clone_drop_failed">> => [], <<"ok">> => true}.

%% after 子句的兜底回收：正常路径已回收则跳过；异常路径尽力回收不再抛错。
release_resources_once(Prepared) ->
    case get(resources_released) of
        true ->
            ok;
        _ ->
            try
                _ = release_resources(Prepared),
                ok
            catch
                _:_ -> ok
            end
    end.

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

%% 与 Makefile eunit-local 相同口径：私有 http 端口替换副本，按调用唯一命名，
%% PG 直连（EUNIT_USE_RELAY=0 语义），不写 git。
%% 每 worker K（0 起）一份独立配置副本：http 19980+K / adm 19970+K，
%% 文件名含 worker 索引与 pid，防并发覆写（与 eunit-local relay 同口径，直连 PG）。
config_for_worker(Repo, K, Prepared) ->
    Base =
        case os:getenv("EUNIT_CONFIG") of
            false -> "config/sys.local";
            B -> B
        end,
    {ok, Bin} = file:read_file(filename:join(Repo, Base ++ ".config")),
    Sub = fun(Bin0, From, To) -> binary:replace(Bin0, From, To, [global]) end,
    Out0 = Sub(
        Bin,
        <<"{http_port, 9800}">>,
        list_to_binary(io_lib:format("{http_port, ~p}", [19980 + K]))
    ),
    Out1 = Sub(
        Out0,
        <<"{http_port_adm, 9706}">>,
        list_to_binary(io_lib:format("{http_port_adm, ~p}", [19970 + K]))
    ),
    %% 每 worker 私有克隆库：配置里所有 database => "<源库>" 指向本 worker 克隆
    Out =
        case Prepared of
            #{source_db := Src, clones := Clones} when is_map_key(K, Clones) ->
                Sub(
                    Out1,
                    list_to_binary("database => \"" ++ Src ++ "\""),
                    list_to_binary("database => \"" ++ maps:get(K, Clones) ++ "\"")
                );
            _ ->
                Out1
        end,
    Path = filename:join(
        Repo,
        "config/sys.quality." ++ os:getpid() ++ "." ++
            integer_to_list(K)
    ),
    ok = file:write_file(Path ++ ".config", Out),
    Path.

cleanup_worker_configs(Repo) ->
    %% glob 绝对化（cwd 无关）；异常路径（含被杀进程遗留）一并回收
    [
        file:delete(filename:join([Repo, "config", F]))
     || F <- filelib:wildcard(filename:join(Repo, "config/sys.quality.*"))
    ],
    ok.

load_estimates(Ctrl) ->
    %% 优先读取上一次分片 per_root.term 的实测 wall_ms 作为 LPT 估计（无则保守 3000ms）
    case filelib:wildcard(filename:join([Ctrl, "evidence", "full-*", "per_root.term"])) of
        [] ->
            #{};
        Files ->
            Latest = lists:last(lists:sort(Files)),
            case file:consult(Latest) of
                {ok, [PerRoot]} when is_map(PerRoot) ->
                    maps:from_list([
                        {M, max(500, maps:get(wall_ms, R, 3000))}
                     || {M, R} <- maps:to_list(PerRoot), is_atom(M), is_map(R)
                    ]);
                _ ->
                    #{}
            end
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
                        0 -> ok;
                        Code -> erlang:error({compile_failed, Src, Code})
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
    Port = open_port(
        {spawn_executable, Erlc},
        [
            exit_status,
            use_stdio,
            stderr_to_stdout,
            {args, ["-o", OutDir, Src]}
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

needs_compile(Src, Out) ->
    case {filelib:last_modified(Src), filelib:last_modified(Out)} of
        {Zero, _} when element(1, Zero) =:= {0, 0, 0} ->
            erlang:error({missing_source, Src});
        {SrcM, OutM} ->
            SrcM >= OutM
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
