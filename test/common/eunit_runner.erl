-module(eunit_runner).
-export([
    run/0,
    run/1,
    run_fast/0,
    ct_suite_setup/1,
    ct_suite_cleanup/1,
    eunit_setup/0,
    eunit_cleanup/1,
    eunit_cleanup_db/1,
    eunit_try_db/0,
    eunit_setup_with_db/0,
    eunit_setup_db_or_skip/0
]).

%%%===================================================================
%%% @doc
%%% 简化的 EUnit 运行器
%%% 可以直接在 erlang shell 中调用
%%%===================================================================

%% @doc 运行所有测试
run() ->
    run([]).

%% @doc 运行指定模块的测试
%% @param Modules 模块列表，如 [user_repo_tests, group_repo_tests]
run(Modules) when is_list(Modules) ->
    % 使用 eunit_setup 启动应用
    State = eunit_setup(),
    try
        case Modules of
            [] -> eunit:test([], [verbose]);
            _ -> eunit:test(Modules, [verbose])
        end
    after
        eunit_cleanup(State)
    end.

%% @doc 快速测试（只测试不需要数据库的模块）
run_fast() ->
    % 不启动应用，只测试纯函数模块
    FastTestModules = [
        elib_pg_sql_tests
    ],
    eunit:test(FastTestModules, [verbose]).

%% @doc 为 Common Test suite 设置统一的项目根目录和应用启动流程
ct_suite_setup(Config) ->
    {ok, OldCwd} = file:get_cwd(),
    ProjectRoot = project_root_dir(OldCwd),
    case file:set_cwd(ProjectRoot) of
        ok ->
            SetupState = eunit_setup(),
            [{setup_state, SetupState}, {old_cwd, OldCwd} | Config];
        {error, Reason} ->
            {skip, io_lib:format("Unable to set cwd to project root (~p): ~p", [ProjectRoot, Reason])}
    end.

%% @doc 清理 Common Test suite 运行状态
ct_suite_cleanup(Config) ->
    case lists:keyfind(setup_state, 1, Config) of
        {setup_state, SetupState} ->
            eunit_cleanup(SetupState);
        false ->
            ok
    end,
    case lists:keyfind(old_cwd, 1, Config) of
        {old_cwd, OldCwd} ->
            _ = file:set_cwd(OldCwd),
            ok;
        false ->
            ok
    end.

%% ===================================================================
%% 内部函数
%% ===================================================================

%% @doc 启动所有必要的应用（测试环境）
%% @return {app_started, imboy} | {app_not_started, test_continues}
%% 为测试环境设置必要的配置并启动应用
eunit_setup() ->
    % 设置测试环境变量
    case application:load(imboy) of
        ok -> ok;
        {error, {already_loaded, imboy}} -> ok;
        _ -> ok
    end,

    load_test_config(),
    application:set_env(imboy, sql_driver, pgsql),
    application:set_env(imboy, env, test),
    application:set_env(imboy, http_port, test_http_port()),
    application:set_env(imboy, dsync_enabled, false),

    % 启动核心依赖应用
    CoreApps = [crypto, asn1, public_key, ssl, inets, jsone, lager, depcache],
    lists:foreach(fun(App) ->
        case application:ensure_all_started(App) of
            {ok, _} -> ok;
            {error, {already_started, _}} -> ok;
            _ -> ok
        end
    end, CoreApps),

    % 启动 imboy 应用
    case application:ensure_all_started(imboy) of
        {ok, _} ->
            {app_started, imboy};
        {error, {already_started, imboy}} ->
            {app_already_started, imboy};
        {error, StartReason} ->
            io:format("Warning: Failed to start imboy app: ~p~n", [StartReason]),
            io:format("Tests that require app will be skipped~n"),
            {app_not_started, test_continues}
    end.

%% @doc 清理资源
%% @param State setup 返回的状态
eunit_cleanup({app_started, imboy}) ->
    % 停止 imboy 应用
    application:stop(imboy),
    ok;
eunit_cleanup({app_already_started, imboy}) ->
    % 应用已经在运行，不需要停止
    ok;
eunit_cleanup({app_not_started, test_continues}) ->
    % 应用没有启动，不需要清理
    ok;
eunit_cleanup(_State) ->
    ok.

%% @doc 尝试建立数据库连接
%% @return {ok, Conn} | {error, Reason}
eunit_try_db() ->
    eunit_try_db(30).

eunit_try_db(0) ->
    {error, no_connection};
eunit_try_db(AttemptsLeft) ->
    Driver = test_sql_driver(),
    try pooler:take_member(Driver, 100) of
        Pid when is_pid(Pid) ->
            {ok, Driver, Pid};
        error_no_members ->
            timer:sleep(100),
            case AttemptsLeft of
                1 -> {error, no_members};
                _ -> eunit_try_db(AttemptsLeft - 1)
            end
    catch
        exit:{noproc, _} ->
            timer:sleep(100),
            case AttemptsLeft of
                1 -> {error, no_pool};
                _ -> eunit_try_db(AttemptsLeft - 1)
            end;
        _:_ ->
            timer:sleep(100),
            case AttemptsLeft of
                1 -> {error, no_connection};
                _ -> eunit_try_db(AttemptsLeft - 1)
            end
    end.

%% @doc 启动应用并尝试连接数据库
%% @return {ok, Conn} | {error, Reason}
eunit_setup_with_db() ->
    SetupState = eunit_setup(),
    case eunit_try_db() of
        {ok, Driver, ConnPid} ->
            persistent_term:put({?MODULE, db_conn, ConnPid}, {Driver, SetupState}),
            {ok, ConnPid};
        {error, Reason} ->
            eunit_cleanup(SetupState),
            {error, Reason}
    end.

%% @doc 归还测试数据库连接并清理应用状态
eunit_cleanup_db(ConnPid) when is_pid(ConnPid) ->
    Key = {?MODULE, db_conn, ConnPid},
    case persistent_term:get(Key, undefined) of
        {Driver, SetupState} ->
            persistent_term:erase(Key),
            _ = catch pooler:return_member(Driver, ConnPid, ok),
            eunit_cleanup(SetupState);
        undefined ->
            ok
    end;
eunit_cleanup_db(_) ->
    ok.

%% @doc 启动应用，如果数据库不可用则返回 skip
%% @return ok | {skip, Reason}
eunit_setup_db_or_skip() ->
    case eunit_setup_with_db() of
        {ok, _Conn} -> ok;
        {error, Reason} -> {skip, "Database connection not available", Reason}
    end.

load_test_config() ->
    ConfigPath = test_config_path(),
    case file:consult(ConfigPath) of
        {ok, [ConfigList]} when is_list(ConfigList) ->
            load_config_entries(ConfigList);
        {ok, ConfigList} when is_list(ConfigList) ->
            load_config_entries(ConfigList);
        {error, Reason} ->
            io:format("Warning: Failed to load config ~p: ~p~n", [ConfigPath, Reason])
    end.

load_config_entries(ConfigList) ->
    lists:foreach(
      fun({App, Env}) when is_atom(App) andalso is_list(Env) ->
              lists:foreach(
                fun({Key, Value}) ->
                        application:set_env(App, Key, Value)
                end, Env);
         (_) ->
              ok
      end, ConfigList).

test_config_path() ->
    case os:getenv("IMBOY_TEST_CONFIG") of
        false ->
            filename:join([project_root_dir(), "config", "sys.config"]);
        "" ->
            filename:join([project_root_dir(), "config", "sys.config"]);
        Path ->
            filename:absname(Path)
    end.

test_http_port() ->
    case os:getenv("TEST_HTTP_PORT") of
        false ->
            19800;
        "" ->
            19800;
        Value ->
            try list_to_integer(Value) of
                Port -> Port
            catch
                _:_ -> 19800
            end
    end.

test_sql_driver() ->
    case application:get_env(imboy, sql_driver) of
        {ok, Driver} when is_atom(Driver) ->
            Driver;
        _ ->
            pgsql
    end.

project_root_dir() ->
    project_root_dir(".").

project_root_dir(StartDir) ->
    find_project_root(filename:absname(StartDir), 10).

find_project_root(Dir, 0) ->
    Dir;
find_project_root(Dir, N) ->
    ConfigPath = filename:join([Dir, "config", "sys.config"]),
    MakefilePath = filename:join([Dir, "Makefile"]),
    case filelib:is_regular(ConfigPath) andalso filelib:is_regular(MakefilePath) of
        true ->
            Dir;
        false ->
            Parent = filename:dirname(Dir),
            case Parent =:= Dir of
                true ->
                    Dir;
                false ->
                    find_project_root(Parent, N - 1)
            end
    end.
