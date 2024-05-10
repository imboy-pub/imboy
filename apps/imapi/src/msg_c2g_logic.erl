-module(msg_c2g_logic).
%%%
% msg_c2g 业务逻辑模块
%%%
-export([check_msg/3]).

-include_lib("imlib/include/chat.hrl").
-include_lib("imlib/include/log.hrl").


%% ===================================================================
%% API
%% ===================================================================
check_msg(Uid, Pid, _DID) ->
    GMsgs = msg_c2g_ds:read_msg(Uid),
    sent_offline_msg(Uid, Pid, GMsgs, 0),
    ok.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


sent_offline_msg(_Uid, _Pid, [], _Index) ->
    ok;
sent_offline_msg(Uid, Pid, [Row | Tail], Index) ->
    {<<"payload">>, Msg} = lists:keyfind(<<"payload">>, 1, Row),
    Delay = 100 + Index * 100,
    erlang:start_timer(Delay, Pid, Msg),
    sent_offline_msg(Uid, Pid, Tail, Index + 1).
