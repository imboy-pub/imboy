-module(elib_log).
-export([
    debug/1, debug/2,
    info/1, info/2,
    notice/1, notice/2,
    warning/1, warning/2,
    error/1, error/2
]).

%% 实际处理函数（导出但仅供内部使用）
-export([internal_log/4, internal_log/5]).

%% @doc 内部日志函数（4参数版本）
%% 注意：虽然 spec 声明为 term()，但实际会通过 ensure_string/1 转换为字符串
-spec internal_log(debug | info | notice | warning | error, term(), module(), non_neg_integer()) ->
    ok.

%% @doc 内部日志函数（5参数版本）
%% 注意：Fmt 参数会被 io_lib:format/2 处理，Args 必须是 list()
-spec internal_log(
    debug | info | notice | warning | error, iodata(), list(), module(), non_neg_integer()
) -> ok.

%% 注意：不使用 lager_transform parse_transform，因为我们已经手动传递了 Module 和 Line 参数
%% 这样可以避免 Dialyzer 无法提取 Core Erlang 的问题

%% 日志级别阈值
-define(LOG_LEVEL, debug).

internal_log(Level, Msg, Module, Line) ->
    safe_log(Level, Msg, Module, Line).
internal_log(Level, Fmt, Args, Module, Line) ->
    safe_log(Level, Fmt, Args, Module, Line).

%% ===================================================================
%% API
%% ===================================================================
%% 级别过滤在 safe_log/level_enabled 中运行时完成：低于 ?LOG_LEVEL 阈值的
%% 调用会被静默丢弃（返回 ok），而非因函数子句 guard 不匹配抛 function_clause。
%% 这样调整 ?LOG_LEVEL 阈值是安全运维操作，不会让既有 debug/info 调用点崩溃。

%% @doc Debug 级别日志（1参数）
%% 实参经 ensure_string/1 转字符串，接受任意 term（与 internal_log/4 一致）
-spec debug(term()) -> ok.
debug(Msg) ->
    safe_log(debug, Msg, ?MODULE, ?LINE).

%% @doc Debug 级别日志（2参数）
-spec debug(iodata(), list()) -> ok.
debug(Fmt, Args) ->
    safe_log(debug, Fmt, Args, ?MODULE, ?LINE).

%% @doc Info 级别日志（1参数）
%% 实参经 ensure_string/1 转字符串，接受任意 term（与 internal_log/4 一致）
-spec info(term()) -> ok.
info(Msg) ->
    safe_log(info, Msg, ?MODULE, ?LINE).

%% @doc Info 级别日志（2参数）
-spec info(iodata(), list()) -> ok.
info(Fmt, Args) ->
    safe_log(info, Fmt, Args, ?MODULE, ?LINE).

%% @doc Notice 级别日志（1参数）
%% 实参经 ensure_string/1 转字符串，接受任意 term（与 internal_log/4 一致）
-spec notice(term()) -> ok.
notice(Msg) ->
    safe_log(notice, Msg, ?MODULE, ?LINE).

%% @doc Notice 级别日志（2参数）
-spec notice(iodata(), list()) -> ok.
notice(Fmt, Args) ->
    safe_log(notice, Fmt, Args, ?MODULE, ?LINE).

%% @doc Warning 级别日志（1参数）
%% 实参经 ensure_string/1 转字符串，接受任意 term（与 internal_log/4 一致）
-spec warning(term()) -> ok.
warning(Msg) ->
    safe_log(warning, Msg, ?MODULE, ?LINE).

%% @doc Warning 级别日志（2参数）
-spec warning(iodata(), list()) -> ok.
warning(Fmt, Args) ->
    safe_log(warning, Fmt, Args, ?MODULE, ?LINE).

%% @doc Error 级别日志（1参数）
%% 实参经 ensure_string/1 转字符串，接受任意 term（与 internal_log/4 一致）
-spec error(term()) -> ok.
error(Msg) ->
    safe_log(error, Msg, ?MODULE, ?LINE).

%% @doc Error 级别日志（2参数）
-spec error(iodata(), list()) -> ok.
error(Fmt, Args) ->
    safe_log(error, Fmt, Args, ?MODULE, ?LINE).

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 级别权重，数值越大级别越高（越严重）
level_weight(debug) -> 10;
level_weight(info) -> 20;
level_weight(notice) -> 30;
level_weight(warning) -> 40;
level_weight(error) -> 50.

%% @doc 判断某级别是否达到 ?LOG_LEVEL 阈值（达到才实际输出）
level_enabled(Level) ->
    level_weight(Level) >= level_weight(?LOG_LEVEL).

%% @doc 构造 lager metadata；独立导出以便 Gradualizer 从 spec 提取 [{atom(), term()}] 类型，
%% 绕过其对 lager:log/3 metadata 参数的 spec 误提取（空列表/具体 proplist 均被误判）。
-spec build_metadata(module(), non_neg_integer(), pid()) -> [{atom(), term()}].
build_metadata(Module, Line, Pid) ->
    [{module, Module}, {line, Line}, {pid, Pid}].

safe_log(Level, Msg, Module, Line) ->
    case level_enabled(Level) of
        false ->
            ok;
        true ->
            Pid = self(),
            Message =
                try
                    ensure_string(Msg)
                catch
                    _:_ ->
                        "INVALID_MESSAGE"
                end,
            _ =
                try
                    %% 经 erlang:apply 调用以绕过 Gradualizer 对 lager:log/3 metadata 参数的 spec 误提取
                    %% （lager 是 parse_transform 库，Gradualizer 从其 beam 提取的 spec 有误）
                    erlang:apply(lager, log, [Level, build_metadata(Module, Line, Pid), Message])
                catch
                    _:_ ->
                        ok
                end,
            ok
    end.

safe_log(Level, Fmt, Args, Module, Line) ->
    case level_enabled(Level) of
        false ->
            ok;
        true ->
            Pid = self(),
            Message =
                try
                    io_lib:format(Fmt, sanitize_args(Args))
                catch
                    _:_ ->
                        io_lib:format("INVALID_FORMAT: ~ts ARGS: ~p", [Fmt, Args])
                end,
            _ =
                try
                    erlang:apply(lager, log, [Level, build_metadata(Module, Line, Pid), Message])
                catch
                    _:_ ->
                        ok
                end,
            ok
    end.

ensure_string(Msg) when is_binary(Msg) ->
    unicode:characters_to_list(Msg);
ensure_string(Msg) when is_list(Msg) ->
    case io_lib:char_list(Msg) of
        true -> Msg;
        false -> io_lib:format("~p", [Msg])
    end;
ensure_string(Msg) ->
    io_lib:format("~p", [Msg]).

sanitize_args(Args) ->
    lists:map(
        fun
            (Arg) when is_binary(Arg) -> unicode:characters_to_list(Arg);
            (Arg) -> Arg
        end,
        Args
    ).
