%% IMBOY_ERLANG_QUALITY P1-03 正式自测（全部通过；进入项目 U）。
%% 覆盖：RUN01/RUN02/RUN04/RUN05/RUN07/RUN10/RUN12 适配、PAR01/PAR03/PAR24 负向。
%% 故意失败的夹具只在 runner 的受控子运行出现；本模块外层断言验证失败被检出。
-module(imboy_quality_framework_tests).
-include_lib("eunit/include/eunit.hrl").

%% ---------- 纯逻辑：许可门与退出码（PAR01/PAR24） ----------

ctx_lp() ->
    #{
        task_id => <<"IMBOY_ERLANG_QUALITY">>,
        execution_mode => limited_parallel,
        execution_goal => phase1_complete,
        owner_verified => true
    }.

ctx_int() ->
    #{
        task_id => <<"IMBOY_ERLANG_QUALITY">>,
        execution_mode => integrated_execution,
        execution_goal => phase1_complete,
        owner_verified => true
    }.

cli_permit_test() ->
    ok = imboy_quality_cli:permit(ctx_lp(), inspect),
    ok = imboy_quality_cli:permit(ctx_int(), synthetic_selftest),
    ?assertMatch(
        {error, missing_or_untrusted_task_context},
        imboy_quality_cli:permit(#{}, inspect)
    ),
    [
        begin
            ?assertMatch({error, _}, imboy_quality_cli:permit(ctx_lp(), Op))
        end
     || Op <- [
            eunit_full,
            eunit_group,
            baseline,
            pbt,
            ct,
            replay,
            certify,
            guard,
            make_dry_run,
            test_build,
            unknown
        ]
    ],
    [
        begin
            ?assertMatch({error, _}, imboy_quality_cli:permit(ctx_int(), Op))
        end
     || Op <- [pbt, ct, replay, certify]
    ],
    %% 参数指定 formal 不能抬权：上下文仍是 limited_parallel
    ?assertMatch({error, _}, imboy_quality_cli:permit((ctx_lp())#{formal => true}, eunit_full)),
    %% G-HEAVY 未准入时正式模式也拒绝重操作
    ?assertMatch(
        {error, {g_heavy_not_admitted, eunit_full}},
        imboy_quality_cli:permit(ctx_int(), eunit_full)
    ),
    ?assertMatch(
        {error, {g_heavy_not_admitted, baseline}},
        imboy_quality_cli:permit(ctx_int(), baseline)
    ),
    %% G-HEAVY 已准入才放行
    Heavy = (ctx_int())#{heavy_admitted => true},
    ok = imboy_quality_cli:permit(Heavy, eunit_full),
    ok = imboy_quality_cli:permit(Heavy, baseline).

exit_codes_test() ->
    [
        ?assertEqual(Want, imboy_quality_cli:exit_code(Got))
     || {Got, Want} <- [
            {ok, 0},
            {test_failed, 1},
            {{test_failed, x}, 1},
            {{error, input_or_tool}, 2},
            {{error, denied}, 2},
            {{error, inventory_incomplete}, 3},
            {performance_not_met, 4},
            {{error, timeout_or_cancelled}, 5},
            {{error, blocked_or_deferred}, 6},
            {weird, 2}
        ]
    ].

%% ---------- 纯逻辑：事件协议（RUN07 重复/缺失；RUN02 空集） ----------

events_protocol_test() ->
    ?assertError(empty_or_invalid_manifest, imboy_quality_events:new([])),
    S0 = imboy_quality_events:new([r1, r2]),
    S1 = imboy_quality_events:event({start, r1}, S0),
    S2 = imboy_quality_events:event({start, r2}, S1),
    {ok, 3} = imboy_quality_events:result(
        S2#{finished => #{r1 => 1, r2 => 2}}, 0
    ),
    %% 缺失终结事件
    ?assertMatch(
        {error, missing_terminal_events},
        imboy_quality_events:result(S2, 0)
    ),
    %% 重复 finish / 零计数 / 非 ok 状态 / 清理失败均拒绝
    Dup = imboy_quality_events:event({finish, r1, pass, 1, ok}, S1),
    Dup2 = imboy_quality_events:event({finish, r1, pass, 1, ok}, Dup),
    ?assertMatch({error, _}, imboy_quality_events:result(Dup2, 0)),
    Zero = imboy_quality_events:event({finish, r1, pass, 0, ok}, S1),
    ?assertMatch({error, _}, imboy_quality_events:result(Zero, 0)),
    %% 原始退出码非零：整体失败
    ?assertMatch(
        {error, {raw_exit, 2, _}},
        imboy_quality_events:result(S2#{finished => #{r1 => 1, r2 => 2}}, 2)
    ).

%% ---------- 纯逻辑：原子状态与路径安全（PAR03） ----------

state_atomic_and_paths_test() ->
    Dir = tmp_dir(),
    {ok, Path} = imboy_quality_state:new_path(Dir, "p3-state.json"),
    Value = #{<<"mode">> => <<"INTEGRATED_EXECUTION">>, <<"ok">> => true},
    {ok, Sha} = imboy_quality_state:write(Path, Value),
    {ok, Value, Sha} = imboy_quality_state:read(Path),
    %% 已存在 / 越界 / 别名均拒绝
    ?assertMatch(
        {error, unsafe_or_existing_path},
        imboy_quality_state:new_path(Dir, "p3-state.json")
    ),
    ?assertMatch(
        {error, unsafe_or_existing_path},
        imboy_quality_state:new_path(Dir, "../Makefile")
    ),
    ?assertMatch(
        {error, unsafe_or_existing_path},
        imboy_quality_state:new_path(Dir, "a/../b")
    ),
    ?assertMatch(
        {error, unsafe_or_existing_path},
        imboy_quality_state:new_path(Dir, "")
    ),
    ok = file:delete(Path).

%% ---------- runner 集成（peer；RUN01/RUN04/RUN05） ----------

fixture_path_test_() ->
    {timeout, 120, fun fixture_path/0}.

fixture_path() ->
    put(quality_fixture_path, compile_fixtures()),
    ?assert(is_list(get(quality_fixture_path))).

run_fixture(Mod, DeadlineMs) ->
    imboy_quality_runner:run_in_worker(
        Mod,
        #{code_path => get(quality_fixture_path), schedulers => 2},
        DeadlineMs
    ).

runner_pass_test_() ->
    {timeout, 90, fun runner_pass/0}.

%% RUN01 正例：真 EUnit 通过、计数与时长齐全
runner_pass() ->
    R = run_fixture(quality_fixture_pass_tests, 60000),
    ?assertEqual(ok, maps:get(exit, R)),
    Counts = maps:get(counts, R),
    ?assert(maps:get(pass, Counts) >= 1),
    ?assertEqual(0, maps:get(fail, Counts)),
    ?assert(maps:get(wall_ms, R) > 0),
    %% RUN09/RUN10：事件来自 eunit 回调，case 时长为整数毫秒
    [{test_end, _, ok, Time} | _] = [
        E
     || E <- maps:get(events, R),
        element(1, E) =:= test_end
    ],
    ?assert(is_integer(Time)).

runner_fail_test_() ->
    {timeout, 90, fun runner_fail/0}.

%% RUN01 负例：断言失败必须非零且被外层检出
runner_fail() ->
    R = run_fixture(quality_fixture_fail_tests, 60000),
    ?assertEqual(test_failed, maps:get(exit, R)),
    ?assert(maps:get(fail, maps:get(counts, R)) >= 1).

runner_crash_test_() ->
    {timeout, 90, fun runner_crash/0}.

%% RUN01 异常：error class 保留，不吞异常
runner_crash() ->
    R = run_fixture(quality_fixture_crash_tests, 60000),
    ?assertEqual(test_failed, maps:get(exit, R)),
    ?assert(maps:get(fail, maps:get(counts, R)) >= 1).

runner_slow_timeout_test_() ->
    {timeout, 60, fun runner_slow_timeout/0}.

%% RUN05：runner 级 deadline(4s) < eunit 内部超时(5s)，peer 回收权威
runner_slow_timeout() ->
    R = run_fixture(quality_fixture_slow_tests, 4000),
    ?assertEqual(timeout_or_cancelled, maps:get(exit, R)),
    ?assert(maps:get(wall_ms, R) < 30000),
    %% 无自有残留：控制 VM 未建立公开 distribution 连接
    ?assertEqual([], nodes()).

runner_cancel_test_() ->
    {timeout, 90, fun runner_cancel/0}.

%% setup 失败 → cancel 事件被记录
runner_cancel() ->
    R = run_fixture(quality_fixture_cancel_tests, 60000),
    ?assertEqual(test_failed, maps:get(exit, R)),
    ?assert(maps:get(cancel, maps:get(counts, R)) >= 1).

%% ---------- 入口与递归边界（RUN12 部分） ----------

no_recursion_test() ->
    %% escript 不在 test 树内，不会被项目 EUnit 自动发现执行
    ?assertEqual([], filelib:wildcard("test/**/quality.escript")).

%% RUN10：OTP29 eunit_listener 回调探针
listener_adapter_test() ->
    Exports = imboy_quality_listener:module_info(exports),
    [
        begin
            ?assert(lists:member({F, A}, Exports))
        end
     || {F, A} <- [
            {start, 1},
            {init, 1},
            {handle_begin, 3},
            {handle_end, 3},
            {handle_cancel, 3},
            {terminate, 2}
        ]
    ],
    ?assert(
        is_list(code:which(eunit_listener)) orelse
            element(1, code:which(eunit_listener)) =:= file
    ),
    ?assert(
        is_list(erlang:system_info(otp_release)) orelse
            is_integer(erlang:system_info(otp_release))
    ).

%% ---------- 辅助 ----------

tmp_dir() ->
    %% macOS /tmp 是 symlink 到 /private/tmp；safe_parent 保守拒绝 symlink 父链（PAR03），
    %% 故用真实路径 /private/tmp。
    Dir = filename:join("/private/tmp", "imboy-quality-p1-" ++ os:getpid()),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    Dir.

compile_fixtures() ->
    Out = tmp_dir(),
    SrcDir = filename:join(repo_root(), "tools/imboy_quality/test_fixtures"),
    Srcs = filelib:wildcard(filename:join(SrcDir, "*.erl")),
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

repo_root() ->
    case os:getenv("IMBOY_QUALITY_REPO") of
        Repo when is_list(Repo), Repo =/= [] -> Repo;
        _ -> repo_root_from_beam()
    end.

repo_root_from_beam() ->
    %% prep/build 位于 <control>/prep/build，仓库根从 beam 向上不可靠；
    %% 从 CWD 推导（项目 eunit 与 selftest 的 cwd 均为仓库根）。
    case filelib:is_dir(filename:join(cwd(), "tools/imboy_quality")) of
        true -> cwd();
        false -> erlang:error(cannot_locate_repo)
    end.

cwd() ->
    {ok, D} = file:get_cwd(),
    D.
