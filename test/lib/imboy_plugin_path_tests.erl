-module(imboy_plugin_path_tests).

-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% SEC-02: imboy_plugin_path 路径白名单收口的负向矩阵测试
%%%
%%% 覆盖 / Coverage（审计 #43）:
%%%   正向（唯一放行面）:
%%%     1. 插件根内合法目录（含多级子目录、尾斜杠、根本身）
%%%   负向（fail-closed，全部拒绝）:
%%%     2. `..` 单级穿越出根
%%%     3. `..` 多级 / 嵌套 / 混入 `.` 的编码穿越变体
%%%     4. symlink 目录指向插件根外（逃逸）
%%%     5. 绝对路径越界（根外任意绝对路径）
%%%     6. 非目录（普通文件）
%%%     7. 不存在的路径
%%%     8. 插件根本身无效（不存在 / 非目录）→ 拒绝一切
%%%     9. ensure_file_within: 根内文件 symlink 指向根外 → 拒；
%%%        不存在文件 → 放行（缺席由签名/manifest 步骤处理）
%%% @end
%%%-------------------------------------------------------------------

%% ===================================================================
%% Fixture — 全部本地 tmp 目录构造
%% ===================================================================
%%
%% layout:
%%   <base>/plugins/            <- plugin_root（受控插件根）
%%     good/                    <- 正向合法插件目录
%%     good/sub/                <- 多级子目录
%%     nested/a/b/              <- 深层合法目录
%%     afile                    <- 根内普通文件（非目录）
%%     link_evil -> ../outside  <- symlink 逃逸（目录）
%%     good/plugin.config -> ../outside/secret.config  <- 文件级 symlink 逃逸
%%   <base>/plugins_fake_file   <- 非目录根
%%   <base>/outside/            <- 根外目录（穿越/symlink 目标）
%%     secret.config

fixture_setup() ->
    Base = integer_to_binary(erlang:unique_integer([positive])),
    Tmp = filename:join(<<"/tmp">>, <<"imboy_plugin_path_", Base/binary>>),
    Root = filename:join(Tmp, <<"plugins">>),
    Good = filename:join(Root, <<"good">>),
    ok = filelib:ensure_path(filename:join(Good, <<"sub">>)),
    ok = filelib:ensure_path(filename:join([Root, <<"nested">>, <<"a">>, <<"b">>])),
    ok = file:write_file(filename:join(Root, <<"afile">>), <<"not a dir">>),
    Outside = filename:join(Tmp, <<"outside">>),
    ok = filelib:ensure_path(Outside),
    SecretConfig = filename:join(Outside, <<"secret.config">>),
    ok = file:write_file(SecretConfig, <<"stolen manifest">>),
    %% symlink: plugins/link_evil -> ../outside
    ok = file:make_symlink(Outside, filename:join(Root, <<"link_evil">>)),
    %% 文件级 symlink: plugins/good/plugin.config -> ../../outside/secret.config
    ok = file:make_symlink(
        SecretConfig, filename:join(Good, <<"plugin.config">>)
    ),
    %% 真实签名的 plugin.config（非 symlink）放在 nested 下用于文件级正向
    RealConfig = filename:join([Root, <<"nested">>, <<"a">>, <<"b">>, <<"plugin.config">>]),
    ok = file:write_file(RealConfig, <<"name = \"ok\"">>),
    %% 非目录根
    ok = file:write_file(filename:join(Tmp, <<"plugins_fake_file">>), <<"fake">>),
    SavedRoot = application:get_env(imboy, plugin_root),
    ok = application:set_env(imboy, plugin_root, Root),
    {SavedRoot, Tmp, Root}.

fixture_cleanup({SavedRoot, Tmp, _Root}) ->
    restore_env(plugin_root, SavedRoot),
    os:cmd("rm -rf " ++ binary_to_list(Tmp)),
    ok.

restore_env(_Key, undefined) -> ok;
restore_env(Key, {ok, V}) -> application:set_env(imboy, Key, V).

%% 根无效场景的独立 fixture（plugin_root 指向不存在路径 / 普通文件）
root_invalid_setup() ->
    {SavedRoot, _Tmp, _Root} = Ctx = fixture_setup(),
    application:set_env(
        imboy,
        plugin_root,
        filename:join([
            <<"/tmp">>,
            <<"imboy_no_such_root_",
                (integer_to_binary(
                    erlang:unique_integer([positive])
                ))/binary>>
        ])
    ),
    {Ctx, SavedRoot}.

root_file_setup() ->
    {SavedRoot, Tmp, Root} = Ctx = fixture_setup(),
    application:set_env(imboy, plugin_root, filename:join(Root, <<"afile">>)),
    {Ctx, Tmp, SavedRoot}.

%% ===================================================================
%% 正向：插件根内合法路径（唯一放行面）
%% ===================================================================

positive_paths_test_() ->
    {foreach, fun fixture_setup/0, fun fixture_cleanup/1, [
        fun({_, _, Root}) -> ok_dir_within_root(Root) end,
        fun({_, _, Root}) -> ok_deep_subdir(Root) end,
        fun({_, _, Root}) -> ok_trailing_slash(Root) end,
        fun({_, _, Root}) -> ok_root_itself(Root) end
    ]}.

ok_dir_within_root(Root) ->
    ?_test(begin
        Dir = filename:join(Root, <<"good">>),
        ?assertMatch({ok, _}, imboy_plugin_path:resolve(Dir))
    end).

ok_deep_subdir(Root) ->
    ?_test(begin
        Dir = filename:join([Root, <<"nested">>, <<"a">>, <<"b">>]),
        ?assertMatch({ok, _}, imboy_plugin_path:resolve(Dir))
    end).

ok_trailing_slash(Root) ->
    ?_test(begin
        Dir = <<(filename:join(Root, <<"good">>))/binary, "/">>,
        ?assertMatch({ok, _}, imboy_plugin_path:resolve(Dir))
    end).

%% 插件根本身视为“根内”（读 root/plugin.config 仍在受控面内）
ok_root_itself(Root) ->
    ?_test(begin
        ?assertMatch({ok, _}, imboy_plugin_path:resolve(Root))
    end).

%% resolve 返回 canonical 路径（realpath 消解 symlink 后仍在根内时，
%% 返回的是解析后的真实路径）
resolve_returns_canonical_test_() ->
    {foreach, fun fixture_setup/0, fun fixture_cleanup/1, [
        fun({_, _, Root}) -> canonical_result(Root) end
    ]}.

canonical_result(Root) ->
    ?_test(begin
        Dir = filename:join(Root, <<"good">>),
        {ok, Real} = imboy_plugin_path:resolve(Dir),
        %% canonical 特征：绝对路径、无 `..`/`.` 组件
        ?assertEqual(false, lists:member(<<"..">>, filename:split(Real))),
        ?assertEqual(false, lists:member(<<".">>, filename:split(Real))),
        %% 根内 alias（嵌套 ./）canonical 化为同一路径
        Dotted = filename:join([Root, <<".">>, <<"good">>]),
        ?assertEqual({ok, Real}, imboy_plugin_path:resolve(Dotted)),
        %% 物理 `..` 消解后仍在根内 → 放行且 canonical 等价
        UpDown = filename:join([Root, <<"good">>, <<"sub">>, <<"..">>]),
        ?assertEqual({ok, Real}, imboy_plugin_path:resolve(UpDown))
    end).

%% ===================================================================
%% 负向矩阵：fail-closed 全部拒绝
%% ===================================================================

negative_paths_test_() ->
    {foreach, fun fixture_setup/0, fun fixture_cleanup/1, [
        fun({_, _, Root}) -> reject_dotdot_escape(Root) end,
        fun({_, _, Root}) -> reject_multi_level_dotdot(Root) end,
        fun({_, _, Root}) -> reject_dotted_variants(Root) end,
        fun({_, _, Root}) -> reject_symlink_escape(Root) end,
        fun({_, _, Root}) -> reject_absolute_outside(Root) end,
        fun({_, _, Root}) -> reject_not_directory(Root) end,
        fun({_, _, Root}) -> reject_not_found(Root) end
    ]}.

%% `..` 单级穿越出根
reject_dotdot_escape(Root) ->
    ?_test(begin
        %% Root/good/../../../<outside-of-root>（Tmp 下根外）
        Tmp = filename:dirname(Root),
        Evil = filename:join([Root, <<"good">>, <<"..">>, <<"..">>, <<"outside">>]),
        ?assertEqual(
            {error, path_outside_plugin_root},
            imboy_plugin_path:resolve(Evil)
        ),
        Evil2 = filename:join([Root, <<"..">>, <<"outside">>]),
        ?assertEqual(
            {error, path_outside_plugin_root},
            imboy_plugin_path:resolve(Evil2)
        ),
        %% Tmp 是根外绝对路径
        ?assert(Tmp =/= Root)
    end).

%% 多级嵌套 `..`（穿过根再回来仍越界：root/../../<parent>/outside）
reject_multi_level_dotdot(Root) ->
    ?_test(begin
        Tmp = filename:dirname(Root),
        Parent = filename:dirname(Tmp),
        Evil = filename:join([Root, <<"..">>, <<"..">>, filename:basename(Tmp), <<"outside">>]),
        ?assert(Parent =/= Root),
        ?assertEqual(
            {error, path_outside_plugin_root},
            imboy_plugin_path:resolve(Evil)
        )
    end).

%% 混入 `.` 与多余斜杠的编码穿越变体
reject_dotted_variants(Root) ->
    ?_test(begin
        %% /./good/../.. 落到根外已存在的 outside
        Evil1 = <<Root/binary, "/./good/../../outside">>,
        ?assertEqual(
            {error, path_outside_plugin_root},
            imboy_plugin_path:resolve(Evil1)
        ),
        %% 双斜杠 + ..
        Evil2 = <<Root/binary, "//good/../../../outside">>,
        %% 物理解析后 = /private/<tmp-parent>/outside（不存在）→ fail-closed 拒绝
        ?assertMatch(
            {error, _},
            imboy_plugin_path:resolve(Evil2)
        )
    end).

%% symlink 目录指向插件根外
reject_symlink_escape(Root) ->
    ?_test(begin
        Link = filename:join(Root, <<"link_evil">>),
        ?assertEqual(
            {error, path_outside_plugin_root},
            imboy_plugin_path:resolve(Link)
        )
    end).

%% 绝对路径越界（根外任意绝对路径）
reject_absolute_outside(Root) ->
    ?_test(begin
        Tmp = filename:dirname(Root),
        Outside = filename:join(Tmp, <<"outside">>),
        ?assertEqual(
            {error, path_outside_plugin_root},
            imboy_plugin_path:resolve(Outside)
        ),
        ?assertEqual(
            {error, path_outside_plugin_root},
            imboy_plugin_path:resolve(<<"/etc">>)
        ),
        ?assertEqual(
            {error, path_outside_plugin_root},
            imboy_plugin_path:resolve(<<"/">>)
        )
    end).

%% 非目录（根内普通文件）
reject_not_directory(Root) ->
    ?_test(begin
        ?assertEqual(
            {error, path_not_directory},
            imboy_plugin_path:resolve(filename:join(Root, <<"afile">>))
        )
    end).

%% 不存在的路径（含根内不存在与含 .. 的不存在路径）
reject_not_found(Root) ->
    ?_test(begin
        ?assertEqual(
            {error, path_not_found},
            imboy_plugin_path:resolve(filename:join(Root, <<"nope">>))
        ),
        %% 含 .. 但整体不存在 → 无法解析 = 拒绝（fail-closed）
        ?assertMatch(
            {error, _},
            imboy_plugin_path:resolve(filename:join([Root, <<"good">>, <<"..">>, <<"nope">>]))
        )
    end).

%% ===================================================================
%% 插件根无效 → 拒绝一切（fail-closed）
%% ===================================================================

plugin_root_missing_rejects_all_test_() ->
    {setup, fun root_invalid_setup/0,
        fun({Ctx, SavedRoot}) ->
            restore_env(plugin_root, SavedRoot),
            fixture_cleanup(Ctx)
        end,
        fun(_) ->
            ?_test(begin
                ?assertEqual(
                    {error, plugin_root_invalid},
                    imboy_plugin_path:resolve(<<"/tmp">>)
                ),
                ?assertEqual(
                    {error, plugin_root_invalid},
                    imboy_plugin_path:ensure_file_within(<<"/tmp/any">>)
                )
            end)
        end}.

plugin_root_is_file_rejects_all_test_() ->
    {setup, fun root_file_setup/0,
        fun({Ctx, _Tmp, SavedRoot}) ->
            restore_env(plugin_root, SavedRoot),
            fixture_cleanup(Ctx)
        end,
        fun(_) ->
            ?_test(begin
                ?assertEqual(
                    {error, plugin_root_invalid},
                    imboy_plugin_path:resolve(<<"/tmp">>)
                )
            end)
        end}.

%% ===================================================================
%% ensure_file_within：文件级收口（plugin.config / SIGNATURE）
%% ===================================================================

ensure_file_within_test_() ->
    {foreach, fun fixture_setup/0, fun fixture_cleanup/1, [
        fun({_, _, Root}) -> file_ok_within(Root) end,
        fun({_, _, Root}) -> file_symlink_escape_rejected(Root) end,
        fun({_, _, Root}) -> file_absent_allowed(Root) end,
        fun({_, _, Root}) -> file_dotdot_rejected(Root) end
    ]}.

%% 根内真实文件放行
file_ok_within(Root) ->
    ?_test(begin
        Config = filename:join([Root, <<"nested">>, <<"a">>, <<"b">>, <<"plugin.config">>]),
        ?assertEqual(ok, imboy_plugin_path:ensure_file_within(Config))
    end).

%% 根内文件是指向根外的 symlink → 拒绝（防 plugin.config/SIGNATURE 外读）
file_symlink_escape_rejected(Root) ->
    ?_test(begin
        Config = <<(filename:join(Root, <<"good">>))/binary, "/plugin.config">>,
        ?assertEqual(
            {error, path_outside_plugin_root},
            imboy_plugin_path:ensure_file_within(Config)
        )
    end).

%% 文件不存在 → 放行（缺席语义由签名校验/manifest 解析步骤处理）
file_absent_allowed(Root) ->
    ?_test(begin
        Sig = filename:join([Root, <<"nested">>, <<"a">>, <<"b">>, <<"SIGNATURE">>]),
        ?assertEqual(ok, imboy_plugin_path:ensure_file_within(Sig))
    end).

%% 文件路径带 .. 指向根外存在文件 → 拒绝
file_dotdot_rejected(Root) ->
    ?_test(begin
        Tmp = filename:dirname(Root),
        Secret = <<(filename:join(Tmp, <<"outside">>))/binary, "/secret.config">>,
        ?assert(filelib:is_file(Secret)),
        ViaDotdot = filename:join([Root, <<"nested">>, <<"..">>, <<"..">>, <<"outside">>]),
        ViaDotdotBin = <<ViaDotdot/binary, "/secret.config">>,
        ?assertEqual(
            {error, path_outside_plugin_root},
            imboy_plugin_path:ensure_file_within(ViaDotdotBin)
        )
    end).
