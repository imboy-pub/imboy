%% IMBOY_ERLANG_QUALITY P1-05 构建键回归（全部通过；进入 U）。
%% BUILD01（同输入同键：指纹命中无需重编）、BUILD04（公共输入变化键变）、
%% BUILD05（依赖声明变化键变）、BUILD06（新增/删除源文件键变）、
%% BUILD02（TEST 模式切换键变）、BUILD03（up_to_date 缺 BEAM 检出）。
%% 真实 erlang.mk 构建管线的探针接线在 P1-05 验收运行时补充（需要
%% 独占构建 owner），本模块锁定键语义与完整性判定的确定性。
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
        ?assertNotEqual(K6, K7)
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

tmp_project() ->
    Dir = filename:join("/private/tmp", "imboy-quality-p5-" ++ os:getpid()),
    ok = filelib:ensure_dir(filename:join(Dir, "src/x")),
    ok = filelib:ensure_dir(filename:join(Dir, "include/x")),
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
