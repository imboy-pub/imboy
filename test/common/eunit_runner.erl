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
    % 启动应用
    start_applications(),
    try
        case Modules of
            [] -> eunit:test([], [verbose]);
            _ -> eunit:test(Modules, [verbose])
        end
    after
        % 不需要停止应用
        ok
    end.

%% @doc 快速测试（只测试不需要数据库的模块）
run_fast() ->
    % 启动应用
    start_applications(),
    try
        % 只测试 SQL 构造相关的模块
        FastTestModules = [
            imboy_pg_sql_tests
        ],
        eunit:test(FastTestModules, [verbose])
    after
        ok
    end.

%% ===================================================================
%% 内部函数
%% ===================================================================

start_applications() ->
    application:set_env(imboy, sql_driver, pgsql),
    application:set_env(imboy, env, test),

    % 启动核心依赖
    CoreApps = [crypto, asn1, public_key, ssl, inets, jsone],
    lists:foreach(fun(App) ->
        case application:ensure_all_started(App) of
            {ok, _} -> ok;
            {error, {already_started, _}} -> ok;
            _ -> ok
        end
    end, CoreApps),

    ensure_cache_started(),
    ok.

%% @doc 启动所有必要的应用
%% @return {app_started, imboy} | {app_already_started, imboy} | {app_not_started, test_continues}
eunit_setup() ->
    % 设置测试环境变量
    application:set_env(imboy, sql_driver, pgsql),
    application:set_env(imboy, env, test),

    % 加载配置文件
    ConfigPath = filename:absname("config/sys.config"),
    io:format("Loading config from: ~p~n", [ConfigPath]),
    case file:consult(ConfigPath) of
        {ok, ConfigList} ->
            io:format("Config loaded: ~p items~n", [length(ConfigList)]),
            [application:set_env(App, Env) || {App, Env} <- ConfigList];
        {error, Reason} ->
            io:format("Failed to load config: ~p~n", [Reason])
    end,

    % 验证配置是否加载成功
    PgConf = application:get_env(imboy, pg_conf),
    io:format("pg_conf: ~p~n", [PgConf]),

    % 启动核心依赖应用
    CoreApps = [crypto, asn1, public_key, ssl, inets, jsone],
    lists:foreach(fun(App) ->
        case application:ensure_all_started(App) of
            {ok, _} -> ok;
            {error, {already_started, _}} -> ok;
            _ -> ok
        end
    end, CoreApps),

    % 确保 depcache 已启动并初始化
    case whereis(depcache) of
        undefined ->
            % 尝试启动 depcache
            case application:ensure_all_started(depcache) of
                ok -> ok;
                {error, {already_started, depcache}} -> ok;
                _ -> ok
            end;
        _ -> ok
    end,

    ensure_cache_started(),
    {app_not_started, test_continues}.

ensure_cache_started() ->
    case whereis(imboy_cache) of
        undefined ->
            _ = imboy_cache:start_link([]),
            ok;
        _ ->
            ok
    end.

%% @doc 清理资源
%% @param _State setup 返回的状态
eunit_cleanup(_State) ->
    % 通常不需要停止应用
    % 让 EUnit 自然结束
    ok.

%% @doc 尝试建立数据库连接
%% @return {ok, Conn} | {error, Reason}
eunit_try_db() ->
    % 先检查 pooler 是否已启动
    case whereis(pooler) of
        undefined ->
            {error, pooler_not_started};
        _Pid ->
            % pooler 已启动，尝试快速获取连接
            try pooler:take_member() of
                {ok, Pid} when is_pid(Pid) ->
                    % 成功获取连接，立即归还
                    pooler:return_member(Pid, ok),
                    {ok, Pid};
                {error, Reason} ->
                    {error, Reason}
            catch
                _:_ ->
                    {error, no_connection}
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
