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
        [begin {module, _} = code:ensure_loaded(M) end
         || M <- [imboy_quality_cli, imboy_quality_runner,
                  imboy_quality_listener, imboy_quality_report]],
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
            io:format(standard_error, "quality.escript error ~tp:~tp~n~tp~n",
                      [Class, Reason, Stack]),
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
    #{task_id => maps:get(<<"task_id">>, State, <<>>),
      mode => mode_atom(maps:get(<<"execution_mode">>, State, undefined)),
      goal => mode_atom(maps:get(<<"execution_goal">>, State, undefined)),
      owner_verified => is_map(maps:get(<<"owner">>, State, null))}.

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
parse(["eunit", "--group=" ++ _ | _]) -> eunit_group;
parse(["report"]) -> report;
parse(_) -> erlang:error(usage).

enforce(Ctx, Op) ->
    ok = imboy_quality_cli:permit(
        Ctx#{execution_mode => maps:get(mode, Ctx),
             execution_goal => maps:get(goal, Ctx)}, permit_op(Op)).

permit_op(selftest) -> synthetic_selftest;
permit_op(admission_check_only) -> admission_check_only;
permit_op(Op) -> Op.

%% ---- 执行 ----

dispatch(inspect, _Ctx, Ctrl) ->
    Info = #{op => inspect, control_dir => Ctrl,
             modules => [M || {M, _} <- sources()],
             cli => imboy_quality_cli:describe()},
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
    Result = #{<<"op">> => <<"selftest">>, <<"scope">> => <<"synthetic_only">>,
               <<"top_level_cases">> => length(Tests), <<"pass">> => Pass,
               <<"fail">> => Fail, <<"cancel">> => maps:get(cancel, C, 0),
               <<"skip">> => maps:get(skip, C, 0),
               <<"business_tests_started">> => 0},
    Out = filename:join(Ctrl, "evidence/selftest-latest.json"),
    {ok, _} = imboy_report_write(Out, Result),
    {Result, Out};

dispatch(admission_check_only, _Ctx, Ctrl) ->
    StatePath = filename:join(Ctrl, "STATE.json"),
    {ok, State, Sha} = imboy_state_read(StatePath),
    Result = #{op => admission_check_only, state_sha256 => Sha,
               lifecycle_status => maps:get(<<"lifecycle_status">>, State, null),
               implementation_admission =>
                   maps:get(<<"implementation_admission">>, State, null)},
    {Result, undefined};

dispatch(report, _Ctx, Ctrl) ->
    Latest = filename:join(Ctrl, "evidence/selftest-latest.json"),
    case file:read_file(Latest) of
        {ok, B} -> {json:decode(B), Latest};
        {error, _} -> {#{op => report, artifact => none}, undefined}
    end;

dispatch(guard, _Ctx, _Ctrl) ->
    %% 真实候选 guard 在 P1-11 完整接线；此处不产生通过证据。
    erlang:error(guard_wiring_delivered_by_P1_11);

dispatch(eunit_full, _Ctx, _Ctrl) ->
    %% G-HEAVY 许可由 enforce 校验；执行体按卡接线（P1-04 基线 / P1-12 pilot）。
    erlang:error(eunit_full_wiring_per_card);

dispatch(eunit_group, _Ctx, _Ctrl) ->
    erlang:error(eunit_group_wiring_per_card).

%% ---- 合成自测 ----

run_selftests(Tests) ->
    Parent = self(),
    Ref = make_ref(),
    Collector = spawn(fun() -> collect(Parent, Ref, []) end),
    Return = eunit:test(Tests, [{report, {imboy_quality_listener,
                                           [{collector, Collector}]}}]),
    Collector ! {stop, Parent, Ref},
    {Counts, _Events} = receive {collected, Ref, C, E} -> {C, E}
                         after 10000 -> {#{}, []} end,
    ok = Return,   %% 任何合成失败都让 selftest 非零退出
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
    lists:foldl(fun
        ({test_end, _, ok, _}, C) -> C#{pass => maps:get(pass, C, 0) + 1};
        ({test_end, _, _, _}, C) -> C#{fail => maps:get(fail, C, 0) + 1};
        ({test_cancel, _, _, _}, C) -> C#{cancel => maps:get(cancel, C, 0) + 1};
        (_, C) -> C
    end, #{}, Es).

test_modules(BuildDir) ->
    [list_to_atom(filename:rootname(F))
     || F <- filelib:wildcard("imboy_quality_*_tests.beam", BuildDir)].

%% ---- 编译 ----

sources() ->
    [{filename:rootname(filename:basename(P)),
      filename:join(["tools/imboy_quality", filename:basename(P)])}
     || P <- filelib:wildcard("tools/imboy_quality/*.erl")]
    ++ [{filename:rootname(filename:basename(P)),
         filename:join(["test/quality", filename:basename(P)])}
        || P <- filelib:wildcard("test/quality/*.erl")].

compile_all(BuildDir) ->
    %% 编译经原生 erlc（open_port + 独立 argv，FRAMEWORK_DESIGN §3 允许边界）；
    %% 本机 OTP29 的 escript 内 compile:file 不落盘，erlc 为确定路径。
    lists:foreach(fun({_Mod, Src}) ->
        case needs_compile(Src, any_beam(BuildDir, _Mod)) of
            true ->
                case erlc(Src, BuildDir) of
                    0 -> ok;
                    Code -> erlang:error({compile_failed, Src, Code})
                end;
            false -> ok
        end
    end, sources()).

any_beam(BuildDir, Mod) ->
    filename:join(BuildDir, Mod ++ ".beam").

erlc(Src, OutDir) ->
    Erlc = os:find_executable("erlc"),
    Port = open_port({spawn_executable, Erlc},
                     [exit_status, use_stdio, stderr_to_stdout,
                      {args, ["-o", OutDir, Src]}]),
    collect_port(Port, []).

collect_port(Port, Acc) ->
    receive
        {Port, {data, _D}} -> collect_port(Port, Acc);
        {Port, {exit_status, Code}} -> Code
    after 60000 -> port_close(Port), erlang:error(erlc_timeout)
    end.

needs_compile(Src, Out) ->
    case {filelib:last_modified(Src), filelib:last_modified(Out)} of
        {Zero, _} when element(1, Zero) =:= {0, 0, 0} ->
            erlang:error({missing_source, Src});
        {SrcM, OutM} -> SrcM >= OutM
    end.

%% ---- 薄封装 ----

imboy_state_read(Path) ->
    {ok, B} = file:read_file(Path),
    Map = json:decode(B),
    {ok, Map, crypto:hash(sha256, B)}.

imboy_report_write(Path, Map) ->
    Bytes = iolist_to_binary(json:encode(Map)),
    Temp = Path ++ ".tmp." ++ os:getpid(),
    {ok, H} = file:open(Temp, [write, binary, raw, exclusive]),
    try ok = file:write(H, Bytes), ok = file:sync(H)
    after ok = file:close(H) end,
    ok = file:rename(Temp, Path),
    {ok, #{bytes => byte_size(Bytes), sha256 => crypto:hash(sha256, Bytes)}}.

%% ---- 退出码 ----

exit_code(#{<<"fail">> := 0, <<"cancel">> := _}, _) -> 0;
exit_code(#{<<"fail">> := F}, _) when F > 0 -> 1;
exit_code(#{<<"cancel">> := C}, _) when C > 0 -> 3;
exit_code(_, _) -> 0.

encode(Term) -> unicode:characters_to_binary(io_lib:format("~tp~n", [Term])).
