-module(imboy_log).

-export([
    print/1,
    debug/1, debug/2,
    info/1, info/2,
    notice/1, notice/2,
    warning/1, warning/2,
    error/1, error/2
]).

-include("log.hrl").

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

print(Message) ->
    ?LOG(Message).
debug(Message) ->
    log(debug, Message).
debug(Format, Args) ->
    log(debug, Format, Args).


info(Message) ->
    log(info, Message).
info(Format, Args) ->
    log(info, Format, Args).


notice(Message) ->
    log(notice, Message).
notice(Format, Args) ->
    log(notice, Format, Args).

warning(Message) ->
    log(warning, Message).
warning(Format, Args) ->
    log(warning, Format, Args).


error(Message) ->
    log(error, Message).
error(Format, Args) ->
    log(error, Format, Args).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


%% @doc Manually log a message into lager without using the parse transform.
-spec log(lager:log_level(), list()) -> ok | {error, lager_not_running}.
log(Level, Message) ->
    Pid = self(),
    lager:log(Level, Pid, Message).


-spec log(lager:log_level(), string(), list()) -> ok | {error, lager_not_running}.
log(Level, Format, Args) ->
    Pid = self(),
    lager:log(Level, Pid, Format, Args).
