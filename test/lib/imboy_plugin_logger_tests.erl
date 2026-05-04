-module(imboy_plugin_logger_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin_logger 的 EUnit 测试（P6-T1）
%%%
%%% 覆盖 / Coverage:
%%%   1. add_remove_handler: 动态添加和移除 lager file handler
%%%   2. log_levels: 各级别日志正常写入
%%%   3. log_file: 路径生成正确
%%%   4. log_without_handler: 无 handler 时不崩溃
%%% @end
%%%-------------------------------------------------------------------

unique_log_dir() ->
    filename:join("/tmp",
        "imboy_plugin_logger_test_" ++
            integer_to_list(erlang:unique_integer([positive]))
    ).

rm_rf(Path) ->
    case filelib:is_dir(Path) of
        true ->
            {ok, Entries} = file:list_dir(Path),
            lists:foreach(fun(E) -> rm_rf(filename:join(Path, E)) end, Entries),
            file:del_dir(Path);
        false ->
            file:delete(Path)
    end.

%% ===================================================================
%% 1. add_remove_handler: 动态添加和移除
%% ===================================================================

logger_add_remove_handler_test_() ->
    {setup,
     fun() ->
         Dir = unique_log_dir(),
         ok = filelib:ensure_dir(filename:join(Dir, "x")),
         {Dir, beta_plugin}
     end,
     fun({Dir, _Name}) ->
         rm_rf(Dir)
     end,
     fun({_Dir, Name}) ->
         LogDir = unique_log_dir(),
         [
             ?_assertMatch(ok, imboy_plugin_logger:add_handler(Name, LogDir)),
             ?_assertMatch(ok, imboy_plugin_logger:remove_handler(Name)),
             ?_assertMatch(ok, imboy_plugin_logger:remove_handler(Name))
         ]
     end}.

%% ===================================================================
%% 2. log_levels: 各级别日志正常写入
%% ===================================================================

logger_log_levels_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(ok, imboy_plugin_logger:log(some_plugin, info, "info ~p", [1])),
        ?assertEqual(ok, imboy_plugin_logger:log(some_plugin, warning, "warn ~p", [2])),
        ?assertEqual(ok, imboy_plugin_logger:log(some_plugin, error, "error ~p", [3]))
    end).

%% ===================================================================
%% 3. log_file: 路径生成正确
%% ===================================================================

logger_log_file_path_test_() ->
    ?TEST_SIMPLE(fun() ->
        Path = imboy_plugin_logger:log_file(channel, "/var/log/imboy/plugins"),
        ?assertEqual("/var/log/imboy/plugins/channel.log", Path)
    end).

%% ===================================================================
%% 4. log_without_handler: 无 handler 时不崩溃
%% ===================================================================

logger_log_without_handler_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(ok, imboy_plugin_logger:log(unknown_plugin, info, "no handler", []))
    end).

%% ===================================================================
%% 5. log_actually_writes: 日志实际写入文件
%% ===================================================================

logger_log_actually_writes_test_() ->
    {setup,
     fun() ->
         Dir = unique_log_dir(),
         ok = filelib:ensure_dir(filename:join(Dir, "x")),
         ok = imboy_plugin_logger:add_handler(gamma_plugin, Dir),
         ok = imboy_plugin_logger:log(gamma_plugin, info, "test message ~p", [42]),
         timer:sleep(50),
         {Dir, gamma_plugin}
     end,
     fun({Dir, Name}) ->
         imboy_plugin_logger:remove_handler(Name),
         rm_rf(Dir)
     end,
     fun({Dir, Name}) ->
         FilePath = filename:join(Dir, "gamma_plugin.log"),
         [
             ?_assert(filelib:is_file(FilePath)),
             ?_assertMatch({ok, _}, file:read_file(FilePath)),
             ?_assertEqual({ok, Dir ++ "/gamma_plugin.log"}, imboy_plugin_logger:get_log_file(Name))
         ]
     end}.
