-module(eunit_runner).
-export([
    run/0,
    run/1,
    run_fast/0,
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
-spec run() -> any().
run() ->
    run([]).

%% @doc 运行指定模块的测试
%% @param Modules 模块列表，如 [user_repo_tests, group_repo_tests]
-spec run(any()) -> ok.
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
-spec run_fast() -> ok.
run_fast() ->
    % 启动应用
    start_applications(),
    try
        % 只测试 SQL 构造相关的模块
        FastTestModules = [
            elib_pg_sql_tests
        ],
        eunit:test(FastTestModules, [verbose])
    after
        ok
    end.

%% ===================================================================
%% 内部函数
%% ===================================================================

%% @doc 确保配置已加载（从已加载的配置文件中读取）
%% @private
ensure_config_loaded() ->
    % 配置文件已通过 Makefile 的 -config 参数加载
    % 这里检查必需的配置是否存在，不存在则报错

    % 检查 pg_conf 是否已加载
    case application:get_env(imboy, pg_conf) of
        {ok, _PgConf} ->
            ok; % 配置已从文件加载
        undefined ->
            error({missing_config, pg_conf,
                "配置文件中未找到 pg_conf，请确保通过 -config 参数指定正确的配置文件\n"
                "使用示例: make eunit CONFIG=config/sys"})
    end,

    % 确保其他必要配置存在
    case application:get_env(imboy, http_port) of
        {ok, _} -> ok;
        undefined ->
            error({missing_config, http_port,
                "配置文件中未找到 http_port，请检查配置文件"})
    end,
    ok.

start_applications() ->
    % ⚠️ 重要：必须在启动应用之前设置配置
    % 因为 imboy_sup:init/1 会在应用启动时被调用

    % 设置测试环境（必须在启动应用之前）
    application:set_env(imboy, sql_driver, pgsql),
    application:set_env(imboy, env, test),

    % 确保配置已加载（从通过 -config 参数加载的配置文件中读取）
    ensure_config_loaded(),

    % 测试环境特别设置：关闭分布式缓存同步
    application:set_env(imboy, dsync_enabled, false),

    % 启动核心依赖
    CoreApps = [crypto, asn1, public_key, ssl, inets, jsone],
    lists:foreach(fun(App) ->
        case application:ensure_all_started(App) of
            {ok, _} -> ok;
            {error, {already_started, _}} -> ok;
            _ -> ok
        end
    end, CoreApps),

    % 尝试启动主应用
    case application:ensure_all_started(imboy) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} ->
            io:format("Warning: Failed to start imboy app: ~p~n", [Reason]),
            ok
    end,
    ok.

%% @doc 启动所有必要的应用
%% @return {app_started, imboy} | {app_already_started, imboy} | {app_not_started, test_continues}
-spec eunit_setup() -> {app_started, imboy} | {app_already_started, imboy} | {app_not_started, test_continues}.
eunit_setup() ->
    % ⚠️ 重要：必须在启动应用之前设置配置
    % 设置测试环境变量
    application:set_env(imboy, sql_driver, pgsql),
    application:set_env(imboy, env, test),

    % 确保配置已加载（从通过 -config 参数加载的配置文件中读取）
    ensure_config_loaded(),

    % 测试环境特别设置：关闭分布式缓存同步
    application:set_env(imboy, dsync_enabled, false),

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
            case application:start(depcache) of
                ok -> ok;
                {error, {already_started, depcache}} -> ok;
                _ -> ok
            end;
        _ -> ok
    end,

    % 尝试启动主应用
    case application:ensure_all_started(imboy) of
        {ok, _StartedApps} ->
            {app_started, imboy};
        {error, {already_started, imboy}} ->
            {app_already_started, imboy};
        {error, _Reason} ->
            % 即使主应用启动失败，也可以继续测试
            % 很多单元测试不需要完整的应用
            {app_not_started, test_continues}
    end.

%% @doc 清理资源
%% @param _State setup 返回的状态
-spec eunit_cleanup(any()) -> ok.
eunit_cleanup(_State) ->
    % 通常不需要停止应用
    % 让 EUnit 自然结束
    ok.

%% @doc 尝试建立数据库连接
%% @return {ok, Conn} | {error, Reason}
-spec eunit_try_db() -> {ok, any()} | {error, any()}.
eunit_try_db() ->
    eunit_try_db(30).

-spec eunit_try_db(non_neg_integer()) -> {ok, any()} | {error, any()}.
eunit_try_db(0) ->
    {error, no_connection};
eunit_try_db(AttemptsLeft) ->
    Driver = test_sql_driver(),
    try pooler:take_member(Driver, 100) of
        ConnPid when is_pid(ConnPid) ->
            {ok, Driver, ConnPid};
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
-spec eunit_setup_with_db() -> {ok, any()} | {error, any()}.
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
-spec eunit_cleanup_db(any()) -> ok.
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
-spec eunit_setup_db_or_skip() -> ok | {skip, binary(), binary(), any()}.
eunit_setup_db_or_skip() ->
    case eunit_setup_with_db() of
        {ok, _Conn} -> ok;
        {error, Reason} -> {skip, <<"Database connection not available">>, <<>>, Reason}
    end.

test_sql_driver() ->
    case application:get_env(imboy, sql_driver) of
        {ok, Driver} when is_atom(Driver) ->
            Driver;
        _ ->
            pgsql
    end.
