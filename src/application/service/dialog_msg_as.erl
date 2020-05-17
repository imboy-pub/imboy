-module (dialog_msg_as).
%%%
% dialog_msg_as 是 dialog_msg application service 缩写
%%%
-export ([check_msg/2]).

-include("imboy.hrl").

%% 检查离线消息
check_msg(Uid, Pid) ->
    Msgs = dialog_msg_ds:read_msg(Uid, ?OFFLINE_MSG_LIMIT),
    % 发送单聊离线消息
    sent_offline_msg(Pid, Msgs, 0),
    ok.

sent_offline_msg(_Pid, [], _Index) ->
    ok;
sent_offline_msg(Pid, [Row|Tail], Index) ->
    {<<"id">>, Id} = lists:keyfind(<<"id">>, 1, Row),
    dialog_msg_ds:delete_msg(Id),
    {<<"payload">>, Msg} = lists:keyfind(<<"payload">>, 1, Row),
    Delay = 3000 + Index * 100,
    erlang:start_timer(Delay, Pid, Msg),
    sent_offline_msg(Pid, Tail, Index + 1).
