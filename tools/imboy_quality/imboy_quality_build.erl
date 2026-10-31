%% IMBOY_ERLANG_QUALITY P1-05：内容指纹构建键（纯 OTP）。
%% 以内容指纹替代 eunit-local 每轮强制清缓存（A1c 硬复位的防失步目的
%% 由完整性校验承担）：键覆盖 OTP/ERTS、架构、编译器选项、TEST 宏、特性、
%% 源码/include/test 的逐文件 SHA、以及依赖声明 SHA。任何输入新增/修改/
%% 删除都改变键；产物缺失/漂移由收据的逐 BEAM SHA 校验检出（BUILD01
%% hit/miss：键相同且收据完整性通过才命中，否则按 miss 走硬复位重建）。
%% 本模块另承载 PLAN311 入口路由的权威判定表 entry_route/1（纯函数，
%% Makefile 路由块是其纯 Make 投影，两处注释互指）。
-module(imboy_quality_build).
-export([
    build_key/2,
    up_to_date/2,
    entry_route/1,
    receipt_collect/2,
    receipt_verify/3,
    write_receipt/2
]).

%% build_key(ProjectRoot, Profile) -> #{key => binary(), inputs => map()}
%% Profile :: #{otp => string(), compiler_opts => [string()],
%%              erlc_opts => [string()], test_mode => boolean(),
%%              features => [atom() | string()]}.
%% compiler_opts/erlc_opts 来自 Make 展开后的 TEST_ERLC_OPTS / ERLC_OPTS
%% （Makefile 经 export 直传环境，escript 读取后按空格切分传入；
%% 缺省 [] 保持既有合成用法兼容）。
build_key(ProjectRoot, Profile) ->
    All = #{
        otp => maps:get(otp, Profile, otp_string()),
        erts => erts_string(),
        arch => erlang:system_info(system_architecture),
        compiler_opts => maps:get(compiler_opts, Profile, []),
        erlc_opts => maps:get(erlc_opts, Profile, []),
        test_mode => maps:get(test_mode, Profile, true),
        features => maps:get(features, Profile, []),
        src => file_shas(ProjectRoot, "src"),
        include => file_shas(ProjectRoot, "include"),
        test => file_shas(ProjectRoot, "test"),
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

%% ---- PLAN311 入口路由（权威判定表；Makefile 路由块是其纯 Make 投影） ----

%% entry_route(Org) -> native | legacy | quality_full
%% Org :: #{inner => origin(), t => origin(), eunit_config => origin(),
%%          eunit_mods => origin(), eunit_use_relay => origin()}
%% origin() :: make 的 $(origin VAR) 语义值；undefined 之外的任何值
%% （environment / command_line / file / override / automatic / default）
%% 一律视为「已定义」。缺键等价 undefined。
%% 判定顺序（与 Makefile 投影一致）：
%%   1) inner 已定义（IMBOY_EUNIT_INNER 非空）→ native（erlang.mk 原生；
%%      eunit-local-legacy 内层 $(MAKE) eunit 即此形态）
%%   2) t / EUNIT_CONFIG / EUNIT_MODS / EUNIT_USE_RELAY 任一已定义 → legacy
%%      （CI 显式参数调用即此形态，旧路径逐字节保持）
%%   3) 其余（裸 make eunit / 裸 make eunit-local）→ quality_full
%% eunit-legacy 目标不进入本函数：它在 Makefile 两条分支都落
%% eunit-local-legacy（强制 legacy，无需读取 MAKECMDGOALS）。
entry_route(Org) when is_map(Org) ->
    case defined(inner, Org) of
        true ->
            native;
        false ->
            Legacy =
                [t, eunit_config, eunit_mods, eunit_use_relay],
            case lists:any(fun(K) -> defined(K, Org) end, Legacy) of
                true -> legacy;
                false -> quality_full
            end
    end.

defined(K, Org) -> maps:get(K, Org, undefined) =/= undefined.

%% ---- 构建收据（BUILD01 hit/miss） ----

%% receipt_collect(OutDir, AppFile) -> {ok, Receipt} | {error, Reason}
%% 采集输出目录快照：输出路径、模块集、各 BEAM SHA256、.app modules。
%% OutDir/AppFile 以仓库相对路径传入（如 "ebin"、"ebin/imboy.app"），
%% 原样入收据；校验时由 receipt_verify 与 Repo 拼接。
%% AppFile 缺失/损坏即失败——绝不给不完整构建发收据（fail-closed）。
receipt_collect(OutDir, AppFile) ->
    case app_modules(AppFile) of
        {ok, AppMods} ->
            Files = lists:sort(filelib:wildcard(filename:join(OutDir, "*.beam"))),
            Beams = maps:from_list(
                [
                    {list_to_atom(filename:rootname(filename:basename(F))), file_sha(F)}
                 || F <- Files
                ]
            ),
            {ok, #{
                vsn => 1,
                out_dir => OutDir,
                app_file => AppFile,
                modules => lists:sort(maps:keys(Beams)),
                beams => Beams,
                app_modules => AppMods
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% receipt_verify(ReceiptPath, Repo, Key) -> {hit, Key} | {miss, Reason}
%% 命中需键相同且收据完整性通过；收据不存在/损坏/BEAM 缺失或漂移/
%% 模块集变化/.app modules 变化一律 miss。每次命中都重算全部 BEAM 摘要
%% （完整性校验在 hit 路径同样执行，不以键相同为由跳过）。
receipt_verify(ReceiptPath, Repo, Key) ->
    case read_receipt(ReceiptPath) of
        {ok, R} -> verify_receipt(Repo, Key, R);
        {error, Reason} -> {miss, Reason}
    end.

verify_receipt(Repo, Key, R) ->
    case maps:get(key, R) =:= Key of
        false ->
            {miss, key_changed};
        true ->
            OutDir = filename:join(Repo, maps:get(out_dir, R)),
            case beams_intact(OutDir, maps:get(beams, R)) of
                ok -> verify_set(OutDir, Repo, R);
                {error, Reason} -> {miss, Reason}
            end
    end.

verify_set(OutDir, Repo, R) ->
    case actual_modules(OutDir) of
        {ok, Mods} ->
            case Mods =:= maps:get(modules, R) of
                false ->
                    {miss, beam_set_changed};
                true ->
                    case app_intact(Repo, R) of
                        ok -> {hit, maps:get(key, R)};
                        {error, Reason} -> {miss, Reason}
                    end
            end;
        {error, Reason} ->
            {miss, Reason}
    end.

beams_intact(OutDir, Beams) ->
    try
        maps:foreach(
            fun(Mod, Sha) ->
                Path = filename:join(OutDir, atom_to_list(Mod) ++ ".beam"),
                case file:read_file(Path) of
                    {ok, B} ->
                        case binary:encode_hex(crypto:hash(sha256, B)) of
                            Sha -> ok;
                            _ -> throw({beam_mismatch, Mod})
                        end;
                    {error, _} ->
                        throw({beam_missing, Mod})
                end
            end,
            Beams
        ),
        ok
    catch
        throw:Reason -> {error, Reason}
    end.

actual_modules(OutDir) ->
    case filelib:is_dir(OutDir) of
        false ->
            {error, out_dir_missing};
        true ->
            {ok, [
                list_to_atom(filename:rootname(filename:basename(F)))
             || F <- lists:sort(filelib:wildcard(filename:join(OutDir, "*.beam")))
            ]}
    end.

app_intact(Repo, R) ->
    AppFile = filename:join(Repo, maps:get(app_file, R)),
    case app_modules(AppFile) of
        {ok, Mods} ->
            case Mods =:= maps:get(app_modules, R) of
                true -> ok;
                false -> {error, app_modules_changed}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

app_modules(AppFile) ->
    case file:consult(AppFile) of
        {ok, [{application, _Name, Props}]} ->
            {ok, lists:sort(proplists:get_value(modules, Props, []))};
        {ok, _} ->
            {error, {bad_app_file, AppFile}};
        {error, Reason} ->
            {error, {app_file_read, AppFile, Reason}}
    end.

read_receipt(Path) ->
    case file:read_file(Path) of
        {ok, B} ->
            try
                R = binary_to_term(B),
                case is_receipt(R) of
                    true -> {ok, R};
                    false -> {error, receipt_corrupt}
                end
            catch
                _:_ -> {error, receipt_corrupt}
            end;
        {error, enoent} ->
            {error, receipt_missing};
        {error, Reason} ->
            {error, {receipt_unreadable, Reason}}
    end.

is_receipt(R) ->
    is_map(R) andalso
        maps:get(vsn, R, 0) =:= 1 andalso
        is_binary(maps:get(key, R, <<>>)) andalso
        is_list(maps:get(out_dir, R, x)) andalso
        is_list(maps:get(app_file, R, x)) andalso
        is_list(maps:get(modules, R, x)) andalso
        is_map(maps:get(beams, R, x)) andalso
        is_list(maps:get(app_modules, R, x)).

%% write_receipt(Path, Receipt)：原子写（临时文件 + rename，中断不留半收据；
%% 读者只会看到旧收据或新收据，二者都能被完整性校验裁决）。
write_receipt(Path, Receipt) ->
    Bytes = term_to_binary(Receipt),
    Temp = Path ++ ".tmp." ++ os:getpid(),
    _ = file:delete(Temp),
    ok = filelib:ensure_dir(Temp),
    {ok, H} = file:open(Temp, [write, binary, raw, exclusive]),
    try
        ok = file:write(H, Bytes),
        ok = file:sync(H)
    after
        ok = file:close(H)
    end,
    ok = file:rename(Temp, Path),
    ok.

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
