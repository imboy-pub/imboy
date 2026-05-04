-module(imboy_plugin_logger).

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin_logger — 每插件独立日志文件（P6-T1）
%%% Per-plugin independent log files
%%%
%%% 职责 / Responsibilities:
%%%   - 为每个插件动态创建 lager_file_backend handler
%%%   - 插件通过 log/4 写日志，自动路由到对应文件
%%%   - 支持运行时添加/移除 handler（热插拔）
%%%
%%% 日志文件位置 / Log file location:
%%%   <log_dir>/<plugin_name>.log
%%%
%%% 用法 / Usage:
%%%   imboy_plugin_logger:add_handler(channel, "/var/log/imboy/plugins"),
%%%   imboy_plugin_logger:log(channel, info, "subscriber count: ~p", [42]),
%%%   imboy_plugin_logger:remove_handler(channel).
%%%
%%% Source of truth: roadmap P6-T1 + contract.md §10
%%%
%%% @author Imboy Team
%%% @copyright 2026 Imboy Project
%%% @end
%%%-------------------------------------------------------------------

-export([
    add_handler/2,
    remove_handler/1,
    log/4,
    log_file/2,
    get_log_file/1
]).

%% ETS 表名
-define(ETS_TABLE, imboy_plugin_logger).
%% Lager handler ID 格式: {imboy_plugin_logger, PluginName}
-define(HANDLER_ID(Name), {imboy_plugin_logger, Name}).

%% ===================================================================
%% ETS 初始化（惰性创建）
%% ===================================================================

ensure_table() ->
    case ets:info(?ETS_TABLE) of
        undefined ->
            ets:new(?ETS_TABLE, [named_table, public, set, {read_concurrency, true}]);
        _ ->
            ok
    end.

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc 为插件添加 lager file backend handler。
%% LogDir: 日志文件所在目录（如 "/var/log/imboy/plugins"）。
%% handler 不会重复添加（幂等）。
-spec add_handler(atom(), file:filename_all()) -> ok | {error, term()}.
add_handler(PluginName, LogDir) ->
    ensure_table(),
    HandlerId = ?HANDLER_ID(PluginName),
    FilePath = log_file(PluginName, LogDir),
    case ets:lookup(?ETS_TABLE, PluginName) of
        [{PluginName, FilePath}] ->
            ok;
        _ ->
            ok = filelib:ensure_dir(FilePath),
            HandlerConfig = [
                {file, FilePath},
                {level, info},
                {size, 10485760},
                {date, "$D0"},
                {count, 30}
            ],
            try
                lager:add_sink(HandlerId, [{handlers, [{lager_file_backend, HandlerConfig}]}])
            catch
                _:_ ->
                    %% lager 不可用时仅记录到 ETS，日志写入降级为直接文件追加
                    ok
            end,
            ets:insert(?ETS_TABLE, {PluginName, FilePath}),
            ok
    end.

%% @doc 移除插件的 lager handler。
-spec remove_handler(atom()) -> ok.
remove_handler(PluginName) ->
    ensure_table(),
    HandlerId = ?HANDLER_ID(PluginName),
    try
        lager:remove_sink(HandlerId)
    catch
        _:_ -> ok
    end,
    ets:delete(?ETS_TABLE, PluginName),
    ok.

%% @doc 写日志到插件的专属文件。
%% 如果插件没有注册 handler，日志静默丢弃（不崩溃）。
-spec log(atom(), lager:log_level() | atom(), string(), [term()]) -> ok.
log(PluginName, Level, Format, Args) ->
    ensure_table(),
    case ets:lookup(?ETS_TABLE, PluginName) of
        [{PluginName, FilePath}] ->
            write_log(FilePath, PluginName, Level, Format, Args);
        [] ->
            ok
    end.

%% @doc 返回插件日志文件路径（纯计算，不创建文件）。
-spec log_file(atom(), file:filename_all()) -> file:filename_all().
log_file(PluginName, LogDir) ->
    filename:join(LogDir, atom_to_list(PluginName) ++ ".log").

%% @doc 返回已注册插件的日志文件路径。
-spec get_log_file(atom()) -> {ok, file:filename_all()} | {error, not_found}.
get_log_file(PluginName) ->
    ensure_table(),
    case ets:lookup(?ETS_TABLE, PluginName) of
        [{PluginName, FilePath}] -> {ok, FilePath};
        [] -> {error, not_found}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 写入日志到文件。优先用 lager sink，降级为直接文件追加。
write_log(FilePath, PluginName, Level, Format, Args) ->
    HandlerId = ?HANDLER_ID(PluginName),
    try
        lager:log(HandlerId, Level, Format, Args)
    catch
        _:_ ->
            %% lager 不可用，直接追加到文件
            Ts = format_timestamp(),
            Msg = io_lib:format(Format, Args),
            Line = io_lib:format("~s [~s] [~s] ~s~n", [Ts, Level, atom_to_list(PluginName), Msg]),
            _ = file:write_file(FilePath, Line, [append]),
            ok
    end.

format_timestamp() ->
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [Y, M, D, H, Min, S]).
