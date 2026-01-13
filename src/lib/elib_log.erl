-module(elib_log).
-export([debug/1, debug/2,
         info/1, info/2,
         notice/1, notice/2,
         warning/1, warning/2,
         error/1, error/2]).

%% 实际处理函数（导出但仅供内部使用）
-export([internal_log/4, internal_log/5]).

%% @doc 内部日志函数（4参数版本）
%% 注意：虽然 spec 声明为 term()，但实际会通过 ensure_string/1 转换为字符串
-spec internal_log(debug | info | notice | warning | error, term(), module(), non_neg_integer()) -> ok.

%% @doc 内部日志函数（5参数版本）
%% 注意：Fmt 参数会被 io_lib:format/2 处理，Args 必须是 list()
-spec internal_log(debug | info | notice | warning | error, iodata(), list(), module(), non_neg_integer()) -> ok.

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

%% @doc Debug 级别日志（1参数）
-spec debug(iodata()) -> ok.
debug(Msg) when ?LOG_LEVEL =:= debug ->
    safe_log(debug, Msg, ?MODULE, ?LINE).

%% @doc Debug 级别日志（2参数）
-spec debug(iodata(), list()) -> ok.
debug(Fmt, Args) when ?LOG_LEVEL =:= debug ->
    safe_log(debug, Fmt, Args, ?MODULE, ?LINE).

%% @doc Info 级别日志（1参数）
-spec info(iodata()) -> ok.
info(Msg) when ?LOG_LEVEL =:= debug; ?LOG_LEVEL =:= info ->
    safe_log(info, Msg, ?MODULE, ?LINE).

%% @doc Info 级别日志（2参数）
-spec info(iodata(), list()) -> ok.
info(Fmt, Args) when ?LOG_LEVEL =:= debug; ?LOG_LEVEL =:= info ->
    safe_log(info, Fmt, Args, ?MODULE, ?LINE).

%% @doc Notice 级别日志（1参数）
-spec notice(iodata()) -> ok.
notice(Msg) when ?LOG_LEVEL =:= debug; ?LOG_LEVEL =:= info; ?LOG_LEVEL =:= notice ->
    safe_log(notice, Msg, ?MODULE, ?LINE).

%% @doc Notice 级别日志（2参数）
-spec notice(iodata(), list()) -> ok.
notice(Fmt, Args) when ?LOG_LEVEL =:= debug; ?LOG_LEVEL =:= info; ?LOG_LEVEL =:= notice ->
    safe_log(notice, Fmt, Args, ?MODULE, ?LINE).

%% @doc Warning 级别日志（1参数）
-spec warning(iodata()) -> ok.
warning(Msg) when ?LOG_LEVEL =:= debug; ?LOG_LEVEL =:= info;
                  ?LOG_LEVEL =:= notice; ?LOG_LEVEL =:= warning ->
    safe_log(warning, Msg, ?MODULE, ?LINE).

%% @doc Warning 级别日志（2参数）
-spec warning(iodata(), list()) -> ok.
warning(Fmt, Args) when ?LOG_LEVEL =:= debug; ?LOG_LEVEL =:= info;
                        ?LOG_LEVEL =:= notice; ?LOG_LEVEL =:= warning ->
    safe_log(warning, Fmt, Args, ?MODULE, ?LINE).

%% @doc Error 级别日志（1参数）
-spec error(iodata()) -> ok.
error(Msg) when ?LOG_LEVEL =:= debug; ?LOG_LEVEL =:= info;
                ?LOG_LEVEL =:= notice; ?LOG_LEVEL =:= warning; ?LOG_LEVEL =:= error ->
    safe_log(error, Msg, ?MODULE, ?LINE).

%% @doc Error 级别日志（2参数）
-spec error(iodata(), list()) -> ok.
error(Fmt, Args) when ?LOG_LEVEL =:= debug; ?LOG_LEVEL =:= info;
                      ?LOG_LEVEL =:= notice; ?LOG_LEVEL =:= warning; ?LOG_LEVEL =:= error ->
    safe_log(error, Fmt, Args, ?MODULE, ?LINE).


%% ===================================================================
%% Internal Functions
%% ===================================================================
safe_log(Level, Msg, Module, Line) ->
    Pid = self(),
    try
        Message = ensure_string(Msg),
        lager:log(Level, [{module, Module}, {line, Line}, {pid, Pid}], Message)
    catch
        _:_ -> lager:log(Level, [{module, Module}, {line, Line}, {pid, Pid}], "INVALID_MESSAGE")
    end.

safe_log(Level, Fmt, Args, Module, Line) ->
    Pid = self(),
    try
        Message = io_lib:format(Fmt, sanitize_args(Args)),
        lager:log(Level, [{module, Module}, {line, Line}, {pid, Pid}], Message)
    catch
        _:_ ->
            ErrorMsg = io_lib:format("INVALID_FORMAT: ~ts ARGS: ~p", [Fmt, Args]),
            lager:log(Level, [{module, Module}, {line, Line}, {pid, Pid}], ErrorMsg)
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
    lists:map(fun
        (Arg) when is_binary(Arg) -> unicode:characters_to_list(Arg);
        (Arg) -> Arg
    end, Args).
