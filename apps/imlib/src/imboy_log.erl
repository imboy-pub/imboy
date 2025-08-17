-module(imboy_log).
-export([debug/1, debug/2,
         info/1, info/2,
         notice/1, notice/2,
         warning/1, warning/2,
         error/1, error/2]).

%% 实际处理函数（导出但仅供内部使用）
-export([internal_log/4, internal_log/5]).

%% 使用parse_transform自动注入模块和行号
-compile({parse_transform, lager_transform}).
%% 日志级别阈值
-define(LOG_LEVEL, debug).

internal_log(Level, Msg, Module, Line) ->
    safe_log(Level, Msg, Module, Line).
internal_log(Level, Fmt, Args, Module, Line) ->
    safe_log(Level, Fmt, Args, Module, Line).

%% ===================================================================
%% API
%% ===================================================================
debug(Msg) when ?LOG_LEVEL =:= debug ->
    safe_log(debug, Msg, ?MODULE, ?LINE).
debug(Fmt, Args) when ?LOG_LEVEL =:= debug ->
    safe_log(debug, Fmt, Args, ?MODULE, ?LINE).
info(Msg) when ?LOG_LEVEL =:= debug; ?LOG_LEVEL =:= info ->
    safe_log(info, Msg, ?MODULE, ?LINE).
info(Fmt, Args) when ?LOG_LEVEL =:= debug; ?LOG_LEVEL =:= info ->
    safe_log(info, Fmt, Args, ?MODULE, ?LINE).
notice(Msg) when ?LOG_LEVEL =:= debug; ?LOG_LEVEL =:= info; ?LOG_LEVEL =:= notice ->
    safe_log(notice, Msg, ?MODULE, ?LINE).
notice(Fmt, Args) when ?LOG_LEVEL =:= debug; ?LOG_LEVEL =:= info; ?LOG_LEVEL =:= notice ->
    safe_log(notice, Fmt, Args, ?MODULE, ?LINE).
warning(Msg) when ?LOG_LEVEL =:= debug; ?LOG_LEVEL =:= info;
                  ?LOG_LEVEL =:= notice; ?LOG_LEVEL =:= warning ->
    safe_log(warning, Msg, ?MODULE, ?LINE).
warning(Fmt, Args) when ?LOG_LEVEL =:= debug; ?LOG_LEVEL =:= info;
                        ?LOG_LEVEL =:= notice; ?LOG_LEVEL =:= warning ->
    safe_log(warning, Fmt, Args, ?MODULE, ?LINE).
error(Msg) when ?LOG_LEVEL =:= debug; ?LOG_LEVEL =:= info;
                ?LOG_LEVEL =:= notice; ?LOG_LEVEL =:= warning; ?LOG_LEVEL =:= error ->
    safe_log(error, Msg, ?MODULE, ?LINE).
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
