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
    erlang:error(eunit_group_requires_modlist).

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

load_estimates(Ctrl) ->
    %% 优先读取上一次分片流（group_*.stream，ETF）的实测 wall_ms 作为 LPT 估计
    %% （无则保守 3000ms）。2026-10-30 修复：曾读 per_root.term，但其为
    %% pretty-print map（含 pid 字面量），file:consult 恒失败 → 估计静默退化
    %% 为平坦 3000ms；改读权威 ETF 流（imboy_quality_runner:stream_read 自带
    %% 长度前缀解析、容忍尾部截断），取 {done, M, Res} 的 wall_ms。独占根不经
    %% 流写盘（run_exclusive 无 stream 键），但其逐根独占执行、不参与普通道
    %% LPT 打包，估计只需覆盖普通根。
    Dirs = lists:sort(
        [D
         || D <- filelib:wildcard(filename:join([Ctrl, "evidence", "full-*"])),
            filelib:is_dir(D)]
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
