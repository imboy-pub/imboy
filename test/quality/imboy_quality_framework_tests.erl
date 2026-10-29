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

%% ---------- 组模式流式落盘与有界 VM 轮换（SCHED05/SCHED08） ----------

%% 流文件尾部截断（VM 死亡时最后一条不完整）必须被容忍：只返回完整事件。
stream_read_truncated_test() ->
    Path = filename:join(tmp_dir(), "trunc.stream"),
    B1 = term_to_binary({start, a}),
    B2 = term_to_binary({done, a, #{exit => ok}}),
    ok = file:write_file(Path, [
        integer_to_binary(byte_size(B1)),
        $\n,
        B1,
        $\n,
        integer_to_binary(byte_size(B2)),
        $\n,
        binary:part(B2, 0, byte_size(B2) - 3)
    ]),
    ?assertEqual({ok, [{start, a}]}, imboy_quality_runner:stream_read(Path)),
    ?assertEqual({ok, []}, imboy_quality_runner:stream_read(Path ++ ".missing")).

rotation_vm_killer_test_() ->
    {timeout, 240, fun rotation_vm_killer/0}.

%% 负向自测：组内一根直接终止 VM。期望：先行根结果从流恢复；致死根首次重试、
%% 二次致死后永久排除并记 vm_killer；其余根在新 VM 完成；共 3 次 VM 尝试。
rotation_vm_killer() ->
    OutDir = tmp_dir(),
    Mods = [
        quality_fixture_pass_tests,
        quality_fixture_vmkill_tests,
        quality_fixture_fail_tests
    ],
    Opts = #{schedulers => 2, deadline_ms => 180000},
    {PR, Rot} = imboy_quality_full:run_group_rotating(
        7, Mods, Opts, [get(quality_fixture_path)], fun(_) -> undefined end, OutDir
    ),
    ?assertEqual(ok, maps:get(exit, maps:get(quality_fixture_pass_tests, PR))),
    ?assertEqual(test_failed, maps:get(exit, maps:get(quality_fixture_fail_tests, PR))),
    ?assertEqual(vm_killer, maps:get(exit, maps:get(quality_fixture_vmkill_tests, PR))),
    ?assertEqual(ok, maps:get(exit, Rot)),
    Attempts = maps:get(attempts, Rot),
    ?assertEqual(3, length(Attempts)),
    [A1, A2, A3] = Attempts,
    ?assertEqual([quality_fixture_vmkill_tests], maps:get(inflight, A1)),
    ?assertEqual([quality_fixture_vmkill_tests], maps:get(excluded_as_killer, A2)),
    ?assertEqual(ok, maps:get(exit, A3)),
    ?assert(filelib:is_regular(filename:join(OutDir, "group_7_try_1.stream"))),
    %% 无自有残留 peer
    ?assertEqual([], nodes()).

%% ---------- 入口与递归边界（RUN12 部分） ----------

%% PERF04 负例时钟：119999/120000 通过、120001 不通过；不接受浮点四舍五入。
perf04_negative_clock_test() ->
    ?assertMatch(#{verdict := within_target}, imboy_quality_cli:perf_verdict(119999)),
    ?assertMatch(#{verdict := within_target}, imboy_quality_cli:perf_verdict(120000)),
    ?assertMatch(
        #{verdict := over_target, label := <<"PILOT_OVER_TARGET">>},
        imboy_quality_cli:perf_verdict(120001)
    ),
    ?assertError(function_clause, imboy_quality_cli:perf_verdict(120000.4)),
    ?assertError(function_clause, imboy_quality_cli:perf_verdict(-1)).

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

%% ---------- 资源准备纯逻辑（候选模板 / 私有 Garage / HIRD / 归属前缀 / 独占集） ----------

resources_parse_garage_outputs_test() ->
    Out = <<"Key name: imboy-quality-1\nKey ID: GK0123456789abcdef\nSecret key: s3cr3tvalue\n">>,
    ?assertEqual(
        #{key_id => "GK0123456789abcdef", secret => "s3cr3tvalue"},
        imboy_quality_resources:parse_key_output(Out)
    ),
    ?assertError(
        {garage_key_output_unparsed, _}, imboy_quality_resources:parse_key_output(<<"nope">>)
    ),
    ?assertEqual(
        "4a5b6c7d8e9f0a1b2c3d4e5f60718293",
        imboy_quality_resources:parse_node_id(
            <<"4a5b6c7d8e9f0a1b2c3d4e5f60718293@127.0.0.1:3901\n">>
        )
    ),
    ?assertError({garage_node_id_unparsed, _}, imboy_quality_resources:parse_node_id(<<"no id">>)).

resources_garage_toml_test() ->
    Toml = iolist_to_binary(
        imboy_quality_resources:garage_toml(#{
            dir => "/tmp/g",
            s3_port => 39001,
            rpc_port => 39002,
            admin_port => 39003,
            rpc_secret => "ab",
            admin_token => "cd"
        })
    ),
    [
        ?assert(binary:match(Toml, S) =/= nomatch)
     || S <- [
            <<"db_engine = \"lmdb\"">>,
            <<"replication_factor = 1">>,
            <<"api_bind_addr = \"127.0.0.1:39001\"">>,
            <<"rpc_bind_addr = \"127.0.0.1:39002\"">>,
            <<"api_bind_addr = \"127.0.0.1:39003\"">>,
            <<"s3_region = \"garage\"">>,
            <<"metadata_dir = \"/tmp/g/meta\"">>
        ]
    ],
    P = imboy_quality_resources:free_port(),
    ?assert(is_integer(P) andalso P > 1024).

%% 并行诊断控制器各自 worker 0 不能同绑 19980/19970：IMBOY_QUALITY_PORT_BASE 调基址，
%% adm 恒为基址-10；默认不变；非法值（非整数/特权端口）硬错不静默回退。
resources_port_base_test() ->
    Repo = scratch_dir("portbase"),
    ok = filelib:ensure_dir(filename:join([Repo, "config", "x"])),
    ok = file:write_file(
        filename:join(Repo, "sys.base.config"),
        <<
            "[{imboy, [{http_port, 9800}, {http_port_adm, 9706}]},\n"
            " {pg, #{database => \"imboy_test_v1\"}}].\n"
        >>
    ),
    Saved = os:getenv("IMBOY_QUALITY_PORT_BASE"),
    Has = fun(Bin, S) -> ?assertNotEqual(nomatch, binary:match(Bin, S)) end,
    try
        true = os:unsetenv("IMBOY_QUALITY_PORT_BASE"),
        ?assertEqual(19980, imboy_quality_resources:port_base()),
        P0 = imboy_quality_resources:write_config(
            Repo, "sys.base", "t0", 1, {"imboy_test_v1", "clone1"}
        ),
        {ok, B0} = file:read_file(P0 ++ ".config"),
        Has(B0, <<"{http_port, 19981}">>),
        Has(B0, <<"{http_port_adm, 19971}">>),
        Has(B0, <<"database => \"clone1\"">>),
        true = os:putenv("IMBOY_QUALITY_PORT_BASE", "21980"),
        ?assertEqual(21980, imboy_quality_resources:port_base()),
        P1 = imboy_quality_resources:write_config(
            Repo, "sys.base", "t1", 2, {"imboy_test_v1", "clone2"}
        ),
        {ok, B1} = file:read_file(P1 ++ ".config"),
        Has(B1, <<"{http_port, 21982}">>),
        Has(B1, <<"{http_port_adm, 21972}">>),
        Has(B1, <<"database => \"clone2\"">>),
        true = os:putenv("IMBOY_QUALITY_PORT_BASE", "abc"),
        ?assertError({bad_port_base, _}, imboy_quality_resources:port_base()),
        true = os:putenv("IMBOY_QUALITY_PORT_BASE", "80"),
        ?assertError({bad_port_base, _}, imboy_quality_resources:port_base())
    after
        case Saved of
            false -> os:unsetenv("IMBOY_QUALITY_PORT_BASE");
            V -> os:putenv("IMBOY_QUALITY_PORT_BASE", V)
        end,
        file:del_dir_r(Repo)
    end.

resources_template_digest_test() ->
    Repo = scratch_dir("tpl"),
    Mig = filename:join(Repo, "priv/migrations"),
    ok = filelib:ensure_dir(filename:join(Mig, "x")),
    ok = file:write_file(filename:join(Mig, "00000001_a.up.sql"), <<"-- a\n">>),
    ok = file:write_file(filename:join(Mig, "00000001_a.down.sql"), <<"-- a down\n">>),
    ok = file:write_file(filename:join(Mig, "00000002_b.up.sql"), <<"-- b\n">>),
    try
        ?assertEqual(2, imboy_quality_resources:migration_head(Repo)),
        D1 = imboy_quality_resources:template_digest(Repo, "180001"),
        ?assertEqual(12, length(D1)),
        ?assert(lists:all(fun(C) -> lists:member(C, "0123456789abcdef") end, D1)),
        %% 迁移内容或 PG 版本变化都必须改变键（§3.5：模板键含迁移 hash 与 PG 版本）
        ok = file:write_file(filename:join(Mig, "00000002_b.up.sql"), <<"-- b2\n">>),
        ?assertNotEqual(D1, imboy_quality_resources:template_digest(Repo, "180001")),
        ?assertNotEqual(D1, imboy_quality_resources:template_digest(Repo, "170001"))
    after
        file:del_dir_r(Repo)
    end,
    Empty = scratch_dir("empty"),
    try
        ?assertError({no_migration_files, _}, imboy_quality_resources:migration_head(Empty))
    after
        file:del_dir_r(Empty)
    end.

resources_hird_bin_test() ->
    Root = scratch_dir("hird"),
    Bin = filename:join([Root, "hird", "target", "release", "hird"]),
    ok = filelib:ensure_dir(Bin),
    ok = file:write_file(Bin, <<"#!/bin/sh\n">>),
    Repo = filename:join([Root, "imboy", "worktree"]),
    ok = filelib:ensure_dir(filename:join(Repo, "x")),
    Prev = os:getenv("IMBOY_QUALITY_HIRD_BIN"),
    try
        os:unsetenv("IMBOY_QUALITY_HIRD_BIN"),
        %% 祖先目录定位：<root>/hird/target/release/hird
        ?assertEqual({ok, Bin}, imboy_quality_resources:hird_bin(Repo)),
        %% 显式覆盖优先，且只接受存在的常规文件
        Override = filename:join(Root, "custom-hird"),
        ok = file:write_file(Override, <<"x">>),
        os:putenv("IMBOY_QUALITY_HIRD_BIN", Override),
        ?assertEqual({ok, Override}, imboy_quality_resources:hird_bin(Repo)),
        os:putenv("IMBOY_QUALITY_HIRD_BIN", filename:join(Root, "missing")),
        ?assertEqual({ok, Bin}, imboy_quality_resources:hird_bin(Repo))
    after
        case Prev of
            false -> os:unsetenv("IMBOY_QUALITY_HIRD_BIN");
            V -> os:putenv("IMBOY_QUALITY_HIRD_BIN", V)
        end,
        file:del_dir_r(Root)
    end.

fixture_owned_name_test() ->
    ?assert(imboy_quality_fixture:is_owned_name("imboy_quality_w0_1")),
    ?assert(imboy_quality_fixture:is_owned_name(<<"imboy_quality_tpl_abc">>)),
    ?assert(imboy_quality_fixture:is_owned_name("attv5_chain_imboy_quality_x_1")),
    ?assertNot(imboy_quality_fixture:is_owned_name("attv5_chain_other")),
    ?assertNot(imboy_quality_fixture:is_owned_name("imboy_test_v1")),
    ?assertNot(imboy_quality_fixture:is_owned_name("postgres")),
    ?assertNot(imboy_quality_fixture:is_owned_name(undefined)).

full_isolated_roots_excluded_test() ->
    Excl = imboy_quality_full:excluded(),
    [
        ?assert(lists:member(M, Excl))
     || M <- [throttle_middleware, oa_direct_cloud_retention_pg_tests]
    ].

%% 真实时钟等待根的 LPT 下界必须覆盖其 ≤3605s 的过期等待（见 ?LONG_ROOT_HINTS 注释）。
full_long_root_hints_test() ->
    Hints = imboy_quality_full:long_root_hints(),
    [
        ?assert(maps:get(M, Hints) >= 3605000)
     || M <- [talk_record_gateway_http_tests, talk_record_client_audio_http_pg_tests]
    ].

%% 有序亲和：回读根必须移到写入根同组紧随其后；同组逆序时调整；任一端缺席不动。
full_ordered_affinity_test() ->
    ?assertEqual(
        [
            {ac04_e2ee_off_archive_db_tests, ac04_s5_readback_tests},
            {enterprise_oa_login_http_tests, enterprise_oa_context_binding_tests}
        ],
        imboy_quality_full:ordered_affinity()
    ),
    Mk = fun(M) -> #{id => M, test_mod => M, resources => [], estimated_ms => 1} end,
    %% 跨组：R 从 G2 移入 G1 紧随 W。
    Gs0 = [[Mk(w0a), Mk(writer)], [Mk(reader), Mk(g2b)]],
    Gs1 = imboy_quality_full:apply_ordered_affinity(Gs0, [{writer, reader}]),
    Mods = fun(G) -> [maps:get(test_mod, X) || X <- G] end,
    ?assertEqual([w0a, writer, reader], Mods(lists:nth(1, Gs1))),
    ?assertEqual([g2b], Mods(lists:nth(2, Gs1))),
    %% 同组逆序：R 移到 W 之后。
    Gs2 = [[Mk(reader), Mk(x), Mk(writer)]],
    ?assertEqual(
        [[x, writer, reader]], [
            Mods(G)
         || G <- imboy_quality_full:apply_ordered_affinity(Gs2, [{writer, reader}])
        ]
    ),
    %% 任一端缺席：原样返回。
    ?assertEqual(Gs0, imboy_quality_full:apply_ordered_affinity(Gs0, [{ghost, reader}])),
    ?assertEqual(Gs0, imboy_quality_full:apply_ordered_affinity(Gs0, [{writer, ghost}])).

%% 所有计划库名 ≤ 63 字符（PG NAMEDATALEN-1；超长名被 CREATE DATABASE 静默截断，
%% 清单/配置仍写全名 → W403 assert_database 失败，2026-10-29 实证）。
env_planned_db_names_fit_pg_limit_test() ->
    Names = imboy_quality_env:planned_db_names(8, "99999"),
    ?assert(length(Names) >= 17),
    [?assert(length(N) =< 63) || N <- Names],
    [?assert(imboy_quality_fixture:is_owned_name(N)) || N <- Names],
    ?assertEqual(
        "attv5_chain_imboy_quality_w403gw_123",
        imboy_quality_env:attv5_db_name("w403gw", "123")
    ).

%% ---------- 辅助 ----------

scratch_dir(Tag) ->
    D = filename:join(
        "/private/tmp",
        "imboy-quality-selftest-" ++ Tag ++ "-" ++
            integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = filelib:ensure_dir(filename:join(D, "x")),
    D.

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
