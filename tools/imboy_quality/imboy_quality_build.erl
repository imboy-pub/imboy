%% IMBOY_ERLANG_QUALITY P1-05：内容指纹构建键（纯 OTP）。
%% 以内容指纹替代 eunit-local 每轮强制清缓存（A1c 硬复位的防失步目的
%% 由完整性校验承担）：键覆盖 OTP/ERTS、编译器选项、TEST 宏、特性、
%% 源码与 include 的逐文件 SHA、以及依赖声明 SHA。任何输入新增/修改/
%% 删除都改变键；产物缺失由 up_to_date 的逐 BEAM 校验检出（BUILD03）。
-module(imboy_quality_build).
-export([build_key/2, up_to_date/2]).

%% build_key(ProjectRoot, Profile) -> #{key => binary(), inputs => map()}
%% Profile :: #{otp => string(), compiler_opts => [string()],
%%              test_mode => boolean(), features => [atom()]}.
build_key(ProjectRoot, Profile) ->
    All = #{
        otp => maps:get(otp, Profile, otp_string()),
        erts => erts_string(),
        compiler_opts => maps:get(compiler_opts, Profile, []),
        test_mode => maps:get(test_mode, Profile, true),
        features => maps:get(features, Profile, []),
        src => file_shas(ProjectRoot, "src"),
        include => file_shas(ProjectRoot, "include"),
        deps => dependency_digest(ProjectRoot)
    },
    #{key => crypto:hash(sha256, term_to_binary(All)), inputs => All}.

%% up_to_date(BuildDir, ExpectedMods)：产物完整性——每个期望模块的
%% BEAM 存在（缺失判定需重建；BUILD03 白盒导出真实可用）。
up_to_date(BuildDir, ExpectedMods) ->
    lists:all(
        fun(Mod) ->
            Beam = filename:join(BuildDir, atom_to_list(Mod) ++ ".beam"),
            case file:read_file(Beam) of
                {ok, _} -> true;
                {error, _} -> false
            end
        end,
        ExpectedMods
    ).

%% ---- 内部 ----

file_shas(ProjectRoot, Dir) ->
    Path = filename:join(ProjectRoot, Dir),
    Files =
        filelib:wildcard(filename:join(Path, "**/*.erl")) ++
            filelib:wildcard(filename:join(Path, "**/*.hrl")),
    maps:from_list([{relative(ProjectRoot, F), file_sha(F)} || F <- Files]).

dependency_digest(ProjectRoot) ->
    DepsFile = filename:join(ProjectRoot, "include/deps.mk"),
    case file:read_file(DepsFile) of
        {ok, B} -> crypto:hash(sha256, B);
        {error, _} -> none
    end.

relative(Root, Path) -> lists:nthtail(length(Root) + 1, Path).

file_sha(Path) ->
    {ok, B} = file:read_file(Path),
    binary:encode_hex(crypto:hash(sha256, B)).

otp_string() -> erlang:system_info(otp_release).
erts_string() -> erlang:system_info(version).
