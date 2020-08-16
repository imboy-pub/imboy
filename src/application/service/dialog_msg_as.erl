-module (dialog_msg_as).
%%%
% dialog_msg_as 是 dialog_msg application service 缩写
%%%
-export ([check_msg/2]).

-include("chat.hrl").
-include("common.hrl").

%% 检查离线消息
% 单聊离线消息，每个离线用户的消息获取10条（差不多一屏幕多），如果多以10条，再返回消除总数量
%%
check_msg(Uid, Pid) ->
    Msgs = dialog_msg_ds:read_msg(Uid, ?SAVE_MSG_LIMIT, undefined),
    % 发送单聊离线消息
    sent_offline_msg(Pid, Msgs, 0),
    ok.

sent_offline_msg(_Pid, [], _Index) ->
    ok;
sent_offline_msg(Pid, [Row|Tail], Index) ->
    {<<"payload">>, Msg} = lists:keyfind(<<"payload">>, 1, Row),
    Delay = 100 + Index * 100,
    erlang:start_timer(Delay, Pid, Msg),
    sent_offline_msg(Pid, Tail, Index + 1).
