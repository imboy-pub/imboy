-module(logic_msg_c2g).
%%%
% msg_c2g 业务逻辑模块
%%%
-export([check_msg/2]).

-include("chat.hrl").
-include("common.hrl").


%%
check_msg(Uid, Pid) ->
    GMsgs = ds_msg_c2g:read_msg(Uid, ?SAVE_MSG_LIMIT),
    sent_offline_msg(Uid, Pid, GMsgs, 0),
    ok.


sent_offline_msg(_Uid, _Pid, [], _Index) ->
    ok;
sent_offline_msg(Uid, Pid, [Row | Tail], Index) ->
    {<<"payload">>, Msg} = lists:keyfind(<<"payload">>, 1, Row),
    Delay = 100 + Index * 100,
    erlang:start_timer(Delay, Pid, Msg),
    sent_offline_msg(Uid, Pid, Tail, Index + 1).
