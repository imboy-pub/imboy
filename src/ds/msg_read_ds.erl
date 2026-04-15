-module(msg_read_ds).
%%%
% msg_read_ds — G3 架构治理：msg_c2c_logic 不应直调 msg_read_repo
% G3: thin DS wrapper for message read receipts
%%%

-include("log.hrl").

%% ==================== API ====================
-export([save_read/5]).

%% @doc 保存已读回执（pass-through 到 repo）
-spec save_read(binary(), integer(), integer(), binary(), binary()) -> ok | {error, term()}.
save_read(MsgId, ToId, CurrentUid, ToDid, ReadAtRfc) ->
    msg_read_repo:save_read(MsgId, ToId, CurrentUid, ToDid, ReadAtRfc).
