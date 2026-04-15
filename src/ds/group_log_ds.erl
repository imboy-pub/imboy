-module(group_log_ds).
%%%
% group_log_ds — G3 架构治理：group_logic 不应直调 group_log_repo
% G3: thin DS wrapper for group operation logs
%%%

-include("log.hrl").

%% ==================== API ====================
-export([add/2]).

%% @doc 写入群操作日志（pass-through 到 repo）
-spec add(epgsql:connection() | pid(), map()) -> {ok, integer()} | {error, term()}.
add(Conn, Data) ->
    group_log_repo:add(Conn, Data).
