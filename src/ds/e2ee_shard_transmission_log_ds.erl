-module(e2ee_shard_transmission_log_ds).
%%%
% e2ee_shard_transmission_log_ds — E2EE 分片传输日志 DS 层
% G3 架构治理：e2ee_shard_validator 不再直调 repo / G3: thin DS wrapper
%%%

-include("log.hrl").

%% ==================== API ====================
-export([insert/1]).

%% @doc 写入分片传输日志（pass-through 到 repo）
-spec insert(map()) -> {ok, integer()} | {error, term()}.
insert(Data) ->
    e2ee_shard_transmission_log_repo:insert(Data).
