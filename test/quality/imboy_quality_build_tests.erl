%% IMBOY_ERLANG_QUALITY P1-05 构建键回归（全部通过；进入 U）。
%% BUILD01（同输入同键：指纹命中无需重编）、BUILD04（公共输入变化键变）、
%% BUILD05（依赖声明变化键变）、BUILD06（新增/删除源文件键变）、
%% BUILD02（TEST 模式切换键变）、BUILD03（up_to_date 缺 BEAM 检出）。
%% P1-05 BUILD01 收据接线（2026-11-01）：build_receipt_hit_and_integrity_test
%% 以真实编译 BEAM + 真实文件 IO 锁定 hit/miss 全语义（命中键报告、mtime
%% 只读、键失效/缺 BEAM/BEAM 漂移/收据损坏缺失/模块集与 .app modules 变化
%% 一律 miss、原子写无残留）；
%% build_recipe_real_make_twice_test 以独立 erlang.mk 夹具跑真实构建管线：
%% 同输入连续两轮 miss→hit（第二轮 0 条 ERLC 行=不全仓重编且报告命中键）、
%% 改测试文件内容键失效触发第三轮重建。PLAN311 入口路由以
%% entry_route_branches_test 覆盖全分支（Makefile 路由块是 entry_route/1 的
%% 纯 Make 投影，分支顺序两侧同构）。
-module(imboy_quality_build_tests).
-include_lib("eunit/include/eunit.hrl").

build_key_semantics_test() ->
    Dir = tmp_project(),
    try
        Profile = #{test_mode => true, features => [f1]},
        #{key := K1} = imboy_quality_build:build_key(Dir, Profile),
        %% BUILD01：同输入同键（内容指纹命中）
        #{key := K2} = imboy_quality_build:build_key(Dir, Profile),
        ?assertEqual(K1, K2),
        %% BUILD02：TEST 模式切换键变
        #{key := K3} = imboy_quality_build:build_key(
            Dir, Profile#{test_mode => false}
        ),
        ?assertNotEqual(K1, K3),
        %% BUILD04：修改公共 include 变键
        ok = file:write_file(
            filename:join(Dir, "include/shared.hrl"),
            <<"-define(NEW, 1).\n">>
        ),
        #{key := K4} = imboy_quality_build:build_key(Dir, Profile),
        ?assertNotEqual(K1, K4),
        %% BUILD06：新增源文件变键；删除变键
        ok = file:write_file(
            filename:join(Dir, "src/extra.erl"),
            <<"-module(extra).\n">>
        ),
        #{key := K5} = imboy_quality_build:build_key(Dir, Profile),
        ?assertNotEqual(K4, K5),
        ok = file:delete(filename:join(Dir, "src/extra.erl")),
        #{key := K6} = imboy_quality_build:build_key(Dir, Profile),
        ?assertNotEqual(K5, K6),
        %% BUILD05：依赖声明变化变键
        ok = file:write_file(
            filename:join(Dir, "include/deps.mk"),
            <<"dep_x = git x 1.0.1\n">>
        ),
        #{key := K7} = imboy_quality_build:build_key(Dir, Profile),
        ?assertNotEqual(K6, K7),
        %% BUILD01 扩展：test/**/*.erl 内容变化键变（P1-05 收据接线新增键域）
        ok = file:write_file(
            filename:join(Dir, "test/a_tests.erl"),
            <<"-module(a_tests).\n">>
        ),
        #{key := K8} = imboy_quality_build:build_key(Dir, Profile),
        ?assertNotEqual(K7, K8)
    after
        file:del_dir_r(Dir)
    end.

%% BUILD03
up_to_date_missing_beam_test() ->
    Dir = tmp_project(),
    try
        BeamDir = filename:join(Dir, "ebin"),
        ok = file:write_file(filename:join(BeamDir, "mod_a.beam"), <<"fake">>),
        ?assert(imboy_quality_build:up_to_date(BeamDir, [mod_a])),
        %% 缺失 BEAM 检出（不得 test module not found 后仍绿）
        ?assertNot(imboy_quality_build:up_to_date(BeamDir, [mod_a, mod_b]))
    after
        file:del_dir_r(Dir)
    end.

%% PLAN311 入口路由：全分支（权威表 entry_route/1；Makefile 投影同序）。
entry_route_branches_test() ->
    %% native：内层调用（IMBOY_EUNIT_INNER 非空）优先于一切触发变量
    ?assertEqual(native, imboy_quality_build:entry_route(#{inner => command_line})),
    ?assertEqual(native, imboy_quality_build:entry_route(#{inner => environment})),
    ?assertEqual(
        native,
        imboy_quality_build:entry_route(#{
            inner => command_line, t => command_line, eunit_config => environment
        })
    ),
    %% quality_full：裸调用（全部 undefined）——`make eunit` 与裸 `make eunit-local`
    ?assertEqual(quality_full, imboy_quality_build:entry_route(#{})),
    ?assertEqual(
        quality_full, imboy_quality_build:entry_route(#{inner => undefined})
    ),
    %% legacy：t / EUNIT_CONFIG / EUNIT_MODS / EUNIT_USE_RELAY 任一 origin≠undefined
    ?assertEqual(legacy, imboy_quality_build:entry_route(#{t => command_line})),
    ?assertEqual(
        legacy, imboy_quality_build:entry_route(#{eunit_config => command_line})
    ),
    ?assertEqual(
        legacy, imboy_quality_build:entry_route(#{eunit_mods => command_line})
    ),
    ?assertEqual(
        legacy, imboy_quality_build:entry_route(#{eunit_use_relay => environment})
    ),
    ?assertEqual(
        legacy, imboy_quality_build:entry_route(#{eunit_use_relay => file})
    ),
    %% CI 形态逐字节对照（backend-ci.yml:154 / nightly.yml:197 的变量集）
    ?assertEqual(
        legacy,
        imboy_quality_build:entry_route(#{
            eunit_config => command_line, eunit_mods => command_line
        })
    ),
    ?assertEqual(
        legacy,
        imboy_quality_build:entry_route(#{
            eunit_config => command_line, eunit_use_relay => command_line
        })
    ).

%% BUILD01 收据回归：真实编译 BEAM + 真实文件 IO（命中键报告 / mtime 只读 /
%% 原子写无残留 / 键失效 / 缺 BEAM→重建恢复 / BEAM 漂移 / 收据损坏缺失 /
%% 模块集变化 / .app modules 变化）。
build_receipt_hit_and_integrity_test() ->
    Dir = tmp_receipt_project(),
    try
        Ebin = filename:join(Dir, "ebin"),
        AppAbs = filename:join(Dir, "ebin/p5probe.app"),
        ReceiptPath = filename:join(Dir, "receipt.term"),
        KeyOf =
            fun() ->
                #{key := K} = imboy_quality_build:build_key(Dir, #{
                    test_mode => true, features => []
                }),
                K
            end,
        Seal =
            fun() ->
                {ok, R} = imboy_quality_build:receipt_collect(Ebin, AppAbs),
                K = KeyOf(),
                ok = imboy_quality_build:write_receipt(ReceiptPath, R#{key => K}),
                K
            end,
        K1 = KeyOf(),
        SealK = Seal(),
        ?assertEqual(K1, SealK),
        %% hit：命中且报告命中键；产物 mtime 不被触碰（只读校验）
        M0 = beam_mtimes(Ebin),
        {hit, K1} = imboy_quality_build:receipt_verify(ReceiptPath, Dir, K1),
        ?assertEqual(M0, beam_mtimes(Ebin)),
        %% 原子写收据：无临时残留
        ?assertEqual([], filelib:wildcard(ReceiptPath ++ ".tmp.*")),
        %% 键失效：改一个测试文件内容 → miss(key_changed)；还原后恢复命中
        TestFile = filename:join(Dir, "test/p5_probe_a_tests.erl"),
        {ok, OrigTest} = file:read_file(TestFile),
        ok =
            file:write_file(TestFile, <<OrigTest/binary, "%% p5 key invalidate\n">>),
        K2 = KeyOf(),
        ?assertNotEqual(K1, K2),
        {miss, key_changed} =
            imboy_quality_build:receipt_verify(ReceiptPath, Dir, K2),
        ok = file:write_file(TestFile, OrigTest),
        {hit, K1} = imboy_quality_build:receipt_verify(ReceiptPath, Dir, K1),
        %% 缺 BEAM 被完整性检出 → 重建（重编 + re-seal）后恢复命中
        SrcA = filename:join(Dir, "src/p5_receipt_probe_a.erl"),
        BeamA = filename:join(Ebin, "p5_receipt_probe_a.beam"),
        ok = file:delete(BeamA),
        {miss, {beam_missing, p5_receipt_probe_a}} =
            imboy_quality_build:receipt_verify(ReceiptPath, Dir, K1),
        {ok, _} = compile:file(SrcA, [{outdir, Ebin}, report_errors]),
        {hit, K1} = imboy_quality_build:receipt_verify(ReceiptPath, Dir, K1),
        %% BEAM 内容漂移检出（SHA256 重算）
        OkBeam = {ok, file:read_file(BeamA)},
        ok = file:write_file(BeamA, <<"not-a-beam">>),
        {miss, {beam_mismatch, p5_receipt_probe_a}} =
            imboy_quality_build:receipt_verify(ReceiptPath, Dir, K1),
        {ok, {ok, BeamBytes}} = OkBeam,
        ok = file:write_file(BeamA, BeamBytes),
        {hit, K1} = imboy_quality_build:receipt_verify(ReceiptPath, Dir, K1),
        %% 收据损坏 / 缺失一律 miss
        ok = file:write_file(ReceiptPath, <<"garbage-not-a-term">>),
        {miss, receipt_corrupt} =
            imboy_quality_build:receipt_verify(ReceiptPath, Dir, K1),
        ok = file:delete(ReceiptPath),
        {miss, receipt_missing} =
            imboy_quality_build:receipt_verify(ReceiptPath, Dir, K1),
        %% 模块集变化：ebin 多出未入收据的 BEAM → miss；移除后恢复
        _ = Seal(),
        SrcB = filename:join(Dir, "src/p5_receipt_probe_b.erl"),
        ok = file:write_file(
            SrcB,
            <<"-module(p5_receipt_probe_b).\n-export([g/0]).\ng() -> 2.\n">>
        ),
        {ok, _} = compile:file(SrcB, [{outdir, Ebin}, report_errors]),
        {miss, beam_set_changed} =
            imboy_quality_build:receipt_verify(ReceiptPath, Dir, K1),
        ok = file:delete(filename:join(Ebin, "p5_receipt_probe_b.beam")),
        ok = file:delete(SrcB),
        {hit, K1} = imboy_quality_build:receipt_verify(ReceiptPath, Dir, K1),
        %% .app modules 变化 → miss；还原后恢复
        {ok, OrigApp} = file:read_file(AppAbs),
        ok =
            file:write_file(AppAbs, <<"{application, p5probe, [{modules, []}]}.">>),
        {miss, app_modules_changed} =
            imboy_quality_build:receipt_verify(ReceiptPath, Dir, K1),
        ok = file:write_file(AppAbs, OrigApp),
        {hit, K1} = imboy_quality_build:receipt_verify(ReceiptPath, Dir, K1)
    after
        file:del_dir_r(Dir)
    end.

%% BUILD01 真实构建管线回归（独立 erlang.mk 夹具，真 make 三轮）：
%% 同输入连续两轮 quality-test-build 语义——首轮 miss 全量（ERLC 行 > 0）+ seal，
%% 次轮 hit（报告命中键且 make 零 ERLC 行 = 不全仓重编）；改一个测试文件内容
%% 键失效 → 第三轮重建（ERLC 行 > 0）。make 子进程实跑，超时放宽。
build_recipe_real_make_twice_test_() ->
    {timeout, 300, ?_test(build_recipe_real_make_twice())}.

build_recipe_real_make_twice() ->
    RepoRoot = find_erlang_mk_root(),
    Dir = filename:join("/private/tmp", "imboy-quality-p5make-" ++ os:getpid()),
    ok = filelib:ensure_dir(filename:join(Dir, "src/x")),
    ok = filelib:ensure_dir(filename:join(Dir, "test/x")),
    ok = file:write_file(
        filename:join(Dir, "Makefile"),
        ["PROJECT = p5probe\ninclude ", RepoRoot, "/erlang.mk\n"]
    ),
    ok = file:write_file(
        filename:join(Dir, "src/p5_probe_a.erl"),
        <<"-module(p5_probe_a).\n-export([f/0]).\nf() -> 1.\n">>
    ),
    TestFile = filename:join(Dir, "test/p5_probe_a_tests.erl"),
    ok = file:write_file(
        TestFile,
        <<
            "-module(p5_probe_a_tests).\n-include_lib(\"eunit/include/eunit.hrl\").\n"
            "f_test() -> ?assertEqual(1, p5_probe_a:f()).\n"
        >>
    ),
    try
        Ebin = filename:join(Dir, "ebin"),
        AppAbs = filename:join(Ebin, "p5probe.app"),
        ReceiptPath = filename:join(Dir, "receipt.term"),
        KeyOf =
            fun() ->
                #{key := K} = imboy_quality_build:build_key(Dir, #{
                    test_mode => true, features => []
                }),
                K
            end,
        %% 第一轮：miss（无收据）→ 配方 miss 分支语义：硬复位 + test-build + seal
        K1 = KeyOf(),
        Out1 = fixture_miss_build(Dir),
        true = erlc_lines(Out1) > 0,
        {ok, R1} = imboy_quality_build:receipt_collect(Ebin, AppAbs),
        ok = imboy_quality_build:write_receipt(ReceiptPath, R1#{key => K1}),
        %% 第二轮：hit——命中键报告，且 make 增量零 ERLC 行（不全仓重编）
        {hit, K1} = imboy_quality_build:receipt_verify(ReceiptPath, Dir, K1),
        Out2 = fixture_make(Dir),
        ?assertEqual(0, erlc_lines(Out2)),
        {hit, K1} = imboy_quality_build:receipt_verify(ReceiptPath, Dir, K1),
        %% 键失效：改一个测试文件内容 → miss → 第三轮重建 + re-seal 恢复命中
        {ok, OrigTest} = file:read_file(TestFile),
        ok =
            file:write_file(TestFile, <<OrigTest/binary, "%% p5 invalidate\n">>),
        K3 = KeyOf(),
        ?assertNotEqual(K1, K3),
        {miss, key_changed} =
            imboy_quality_build:receipt_verify(ReceiptPath, Dir, K3),
        Out3 = fixture_miss_build(Dir),
        true = erlc_lines(Out3) > 0,
        {ok, R3} = imboy_quality_build:receipt_collect(Ebin, AppAbs),
        ok = imboy_quality_build:write_receipt(ReceiptPath, R3#{key => K3}),
        {hit, K3} = imboy_quality_build:receipt_verify(ReceiptPath, Dir, K3)
    after
        file:del_dir_r(Dir)
    end.

%% ---- 夹具与辅助 ----

fixture_miss_build(Dir) ->
    %% 配方 miss 分支的硬复位三连（.erlang.mk 顶层收据不在清理范围，与
    %% Makefile quality-test-build 一致）；首轮文件可能尚不存在（rm -f 语义）
    _ = file:delete(filename:join([Dir, ".erlang.mk", "p5probe.test"])),
    _ = file:del_dir_r(filename:join([Dir, ".erlang.mk", "beam-cache", "p5probe"])),
    _ = file:delete(filename:join(Dir, "ebin/p5probe.app")),
    fixture_make(Dir).

fixture_make(Dir) ->
    os:cmd("cd '" ++ Dir ++ "' && make test-build 2>&1").

erlc_lines(Out) ->
    length(string:split(Out, " ERLC ", all)) - 1.

find_erlang_mk_root() ->
    {ok, Cwd} = file:get_cwd(),
    find_erlang_mk_root(Cwd).

find_erlang_mk_root(Dir) ->
    Candidate = filename:join(Dir, "erlang.mk"),
    case filelib:is_regular(Candidate) of
        true ->
            Dir;
        false ->
            Parent = filename:dirname(Dir),
            case Parent of
                Dir -> erlang:error(erlang_mk_not_found);
                _ -> find_erlang_mk_root(Parent)
            end
    end.

beam_mtimes(Dir) ->
    [
        filelib:last_modified(F)
     || F <- lists:sort(filelib:wildcard(filename:join(Dir, "*.beam")))
    ].

tmp_project() ->
    Dir = filename:join("/private/tmp", "imboy-quality-p5-" ++ os:getpid()),
    ok = filelib:ensure_dir(filename:join(Dir, "src/x")),
    ok = filelib:ensure_dir(filename:join(Dir, "include/x")),
    ok = filelib:ensure_dir(filename:join(Dir, "test/x")),
    ok = filelib:ensure_dir(filename:join(Dir, "ebin/x")),
    ok = file:write_file(filename:join(Dir, "src/a.erl"), <<"-module(a).\n">>),
    ok = file:write_file(
        filename:join(Dir, "include/shared.hrl"),
        <<"-define(V, 1).\n">>
    ),
    ok = file:write_file(
        filename:join(Dir, "include/deps.mk"),
        <<"dep_x = git x 1.0.0\n">>
    ),
    Dir.

tmp_receipt_project() ->
    Dir = filename:join("/private/tmp", "imboy-quality-p5rcpt-" ++ os:getpid()),
    ok = filelib:ensure_dir(filename:join(Dir, "src/x")),
    ok = filelib:ensure_dir(filename:join(Dir, "include/x")),
    ok = filelib:ensure_dir(filename:join(Dir, "test/x")),
    ok = filelib:ensure_dir(filename:join(Dir, "ebin/x")),
    SrcA = filename:join(Dir, "src/p5_receipt_probe_a.erl"),
    ok = file:write_file(
        SrcA,
        <<"-module(p5_receipt_probe_a).\n-export([f/0]).\nf() -> 1.\n">>
    ),
    ok = file:write_file(
        filename:join(Dir, "test/p5_probe_a_tests.erl"),
        <<"-module(p5_probe_a_tests).\n">>
    ),
    ok = file:write_file(
        filename:join(Dir, "include/shared.hrl"),
        <<"-define(V, 1).\n">>
    ),
    ok = file:write_file(
        filename:join(Dir, "include/deps.mk"),
        <<"dep_x = git x 1.0.0\n">>
    ),
    {ok, _} = compile:file(SrcA, [{outdir, filename:join(Dir, "ebin")}, report_errors]),
    ok = file:write_file(
        filename:join(Dir, "ebin/p5probe.app"),
        <<
            "{application, p5probe, [{description, \"p5 receipt probe\"}, "
            "{vsn, \"1\"}, {modules, [p5_receipt_probe_a]}]}.\n"
        >>
    ),
    Dir.
