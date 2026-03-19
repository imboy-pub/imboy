-module(eunit_runner).
-export([
    run/0,
    run/1,
    run_fast/0,
    eunit_setup/0,
    eunit_cleanup/1,
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

%% ===================================================================
%% 内部函数
%% ===================================================================

%% @doc 启动所有必要的应用（测试环境）
%% @return {app_started, imboy} | {app_not_started, test_continues}
%% 为测试环境设置必要的配置并启动应用
eunit_setup() ->
    % 设置测试环境变量
    application:set_env(imboy, sql_driver, pgsql),
    application:set_env(imboy, env, test),

    % 加载本地开发配置文件
    ConfigPath = filename:absname("config/sys.local.config"),
    case file:consult(ConfigPath) of
        {ok, [ConfigList]} ->
            % 加载所有应用的配置
            lists:foreach(fun({App, Env}) when is_atom(App) andalso is_list(Env) ->
                lists:foreach(fun({Key, Value}) ->
                    application:set_env(App, Key, Value)
                end, Env);
               (_) ->
                    ok
            end, ConfigList);
        {error, _Reason} ->
            io:format("Warning: Failed to load config ~p~n", [ConfigPath])
    end,

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
    eunit_try_db(100).

eunit_try_db(0) ->
    {error, no_connection};
eunit_try_db(AttemptsLeft) ->
    try pooler:take_member() of
        {ok, Pid} when is_pid(Pid) ->
            % 成功获取连接，立即归还
            pooler:return_member(Pid, ok),
            {ok, Pid};
        {error, Reason} ->
            timer:sleep(100),
            case AttemptsLeft of
                1 -> {error, Reason};
                _ -> eunit_try_db(AttemptsLeft - 1)
            end
    catch
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
    % 先启动应用
    eunit_setup(),
    % 然后尝试连接数据库
    eunit_try_db().

%% @doc 启动应用，如果数据库不可用则返回 skip
%% @return ok | {skip, Reason}
eunit_setup_db_or_skip() ->
    case eunit_setup_with_db() of
        {ok, _Conn} -> ok;
        {error, Reason} -> {skip, "Database connection not available", Reason}
    end.
