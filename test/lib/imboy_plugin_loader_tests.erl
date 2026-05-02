-module(imboy_plugin_loader_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin_loader 的 EUnit 测试（切片 1：核心 gen_server）
%%% EUnit tests for imboy_plugin_loader (slice 1: core gen_server)
%%%
%%% 覆盖 / Coverage:
%%%   1. loads_multiple_valid_plugins: 加载多个合法插件
%%%   2. isolates_invalid_plugin: 单个非法插件不影响其他
%%%   3. empty_dir: 空目录不报错
%%%   4. nonexistent_dir: 不存在的目录被记录为失败
%%%   5. rescan_overwrites: 重新扫描覆盖旧数据
%%%   6. handles_unknown_messages: handle_info 优雅处理未知消息
%%%
%%% 测试隔离 / Isolation:
%%%   - 每个 test 自创临时目录 + cleanup
%%%   - 每个 test 显式 gen_server:stop 并清理 persistent_term
%%%   - 测试间串行（gen_server 注册全局名 ?MODULE）
%%% @end
%%%-------------------------------------------------------------------

%% ===================================================================
%% Test helpers
%% ===================================================================

unique_dir() ->
    filename:join(
        "/tmp",
        "imboy_plugin_loader_test_" ++
            integer_to_list(erlang:unique_integer([positive]))
    ).

mkdir_p(Path) ->
    ok = filelib:ensure_dir(filename:join(Path, "x")),
    case filelib:is_dir(Path) of
        true -> ok;
        false -> ok = file:make_dir(Path)
    end.

%% 递归删除目录
rm_rf(Path) ->
    case filelib:is_dir(Path) of
        true ->
            {ok, Entries} = file:list_dir(Path),
            lists:foreach(
                fun(E) -> rm_rf(filename:join(Path, E)) end,
                Entries
            ),
            file:del_dir(Path);
        false ->
            file:delete(Path)
    end.

%% 在 BaseDir 下写一个名为 Name 的合法插件
write_valid_plugin(BaseDir, Name) ->
    PluginDir = filename:join(BaseDir, atom_to_list(Name)),
    mkdir_p(PluginDir),
    Base = imboy_plugin_dummy:manifest(),
    BasePrefix = <<(atom_to_binary(Name, utf8))/binary, "_">>,
    M = Base#{
        name := Name,
        migrations := (maps:get(migrations, Base))#{
            table_prefix := BasePrefix
        }
    },
    Path = filename:join(PluginDir, "plugin.config"),
    ok = file:write_file(Path, io_lib:format("~p.~n", [M])),
    ok.

%% 在 BaseDir 下写一个非法插件（manifest name 大写不合规）
write_invalid_plugin(BaseDir, SubdirName) ->
    PluginDir = filename:join(BaseDir, SubdirName),
    mkdir_p(PluginDir),
    Bad = (imboy_plugin_dummy:manifest())#{name := 'BadName'},
    Path = filename:join(PluginDir, "plugin.config"),
    ok = file:write_file(Path, io_lib:format("~p.~n", [Bad])),
    ok.

%% 停止 loader、清理 persistent_term + 目录
stop_and_cleanup(Pid, Dir, NamesToErase) ->
    case is_process_alive(Pid) of
        true -> gen_server:stop(Pid);
        false -> ok
    end,
    lists:foreach(
        fun(N) -> _ = persistent_term:erase({imboy_plugin_manifest, N}) end,
        NamesToErase
    ),
    rm_rf(Dir).

%% ===================================================================
%% 1. loads_multiple_valid_plugins
%% ===================================================================

loader_loads_multiple_valid_plugins_test_() ->
    {setup,
     fun() ->
         Dir = unique_dir(),
         mkdir_p(Dir),
         write_valid_plugin(Dir, foo_plugin),
         write_valid_plugin(Dir, bar_plugin),
         {ok, Pid} = imboy_plugin_loader:start_link(Dir),
         {Dir, Pid}
     end,
     fun({Dir, Pid}) ->
         stop_and_cleanup(Pid, Dir, [foo_plugin, bar_plugin])
     end,
     fun(_) ->
         [
             ?_assertEqual(
                 lists:sort([foo_plugin, bar_plugin]),
                 lists:sort(imboy_plugin_loader:list_plugins())
             ),
             ?_assertMatch(
                 #{name := foo_plugin},
                 imboy_plugin_loader:get_manifest(foo_plugin)
             ),
             ?_assertMatch(
                 #{name := bar_plugin},
                 imboy_plugin_loader:get_manifest(bar_plugin)
             ),
             ?_assertEqual([], imboy_plugin_loader:list_failed())
         ]
     end}.

%% ===================================================================
%% 2. isolates_invalid_plugin
%% ===================================================================

loader_isolates_invalid_plugin_test_() ->
    {setup,
     fun() ->
         Dir = unique_dir(),
         mkdir_p(Dir),
         write_valid_plugin(Dir, good_plugin),
         write_invalid_plugin(Dir, "bad_plugin"),
         {ok, Pid} = imboy_plugin_loader:start_link(Dir),
         {Dir, Pid}
     end,
     fun({Dir, Pid}) ->
         stop_and_cleanup(Pid, Dir, [good_plugin])
     end,
     fun(_) ->
         [
             ?_assertEqual([good_plugin], imboy_plugin_loader:list_plugins()),
             ?_assertMatch(
                 #{name := good_plugin},
                 imboy_plugin_loader:get_manifest(good_plugin)
             ),
             ?_assertEqual(
                 undefined,
                 imboy_plugin_loader:get_manifest('BadName')
             ),
             ?_assertMatch(
                 [{_, {name, invalid_format}}],
                 imboy_plugin_loader:list_failed()
             )
         ]
     end}.

%% ===================================================================
%% 3. empty_dir
%% ===================================================================

loader_empty_dir_test_() ->
    {setup,
     fun() ->
         Dir = unique_dir(),
         mkdir_p(Dir),
         {ok, Pid} = imboy_plugin_loader:start_link(Dir),
         {Dir, Pid}
     end,
     fun({Dir, Pid}) ->
         stop_and_cleanup(Pid, Dir, [])
     end,
     fun(_) ->
         [
             ?_assertEqual([], imboy_plugin_loader:list_plugins()),
             ?_assertEqual([], imboy_plugin_loader:list_failed())
         ]
     end}.

%% ===================================================================
%% 4. nonexistent_dir 被记录为失败
%% ===================================================================

loader_nonexistent_dir_test_() ->
    {setup,
     fun() ->
         %% 不创建目录
         Dir = unique_dir(),
         {ok, Pid} = imboy_plugin_loader:start_link(Dir),
         {Dir, Pid}
     end,
     fun({_Dir, Pid}) ->
         case is_process_alive(Pid) of
             true -> gen_server:stop(Pid);
             false -> ok
         end
     end,
     fun(_) ->
         [
             ?_assertEqual([], imboy_plugin_loader:list_plugins()),
             ?_assertMatch([{_, enoent}], imboy_plugin_loader:list_failed())
         ]
     end}.

%% ===================================================================
%% 5. rescan_overwrites - scan/0 重新扫描覆盖旧数据
%% ===================================================================

loader_rescan_overwrites_test_() ->
    {setup,
     fun() ->
         Dir = unique_dir(),
         mkdir_p(Dir),
         write_valid_plugin(Dir, alpha_plugin),
         {ok, Pid} = imboy_plugin_loader:start_link(Dir),
         %% 在 loader 启动后再写一个新插件 + 删除旧的
         write_valid_plugin(Dir, beta_plugin),
         rm_rf(filename:join(Dir, "alpha_plugin")),
         ok = imboy_plugin_loader:scan(),
         {Dir, Pid}
     end,
     fun({Dir, Pid}) ->
         stop_and_cleanup(Pid, Dir, [alpha_plugin, beta_plugin])
     end,
     fun(_) ->
         [
             ?_assertEqual([beta_plugin], imboy_plugin_loader:list_plugins()),
             %% alpha 已被 cleanup_persistent_terms 清除
             ?_assertEqual(
                 undefined,
                 imboy_plugin_loader:get_manifest(alpha_plugin)
             ),
             ?_assertMatch(
                 #{name := beta_plugin},
                 imboy_plugin_loader:get_manifest(beta_plugin)
             )
         ]
     end}.

%% ===================================================================
%% 6. handle_info 优雅处理未知消息（actor-model instinct）
%% ===================================================================

loader_handles_unknown_message_test_() ->
    {setup,
     fun() ->
         Dir = unique_dir(),
         mkdir_p(Dir),
         {ok, Pid} = imboy_plugin_loader:start_link(Dir),
         {Dir, Pid}
     end,
     fun({Dir, Pid}) ->
         stop_and_cleanup(Pid, Dir, [])
     end,
     fun({_Dir, Pid}) ->
         [
             ?_test(begin
                 %% 发送未知消息：进程仍存活，list_plugins 仍工作
                 Pid ! {unknown, message, 42},
                 ?assert(is_process_alive(Pid)),
                 ?assertEqual([], imboy_plugin_loader:list_plugins())
             end),
             ?_test(begin
                 %% 未知 cast 不应崩溃
                 gen_server:cast(Pid, {unknown_cast, msg}),
                 ?assert(is_process_alive(Pid))
             end),
             ?_test(begin
                 %% 未知 call 返回结构化错误
                 ?assertEqual(
                     {error, unknown_call},
                     gen_server:call(Pid, {unknown_call, x})
                 )
             end)
         ]
     end}.
