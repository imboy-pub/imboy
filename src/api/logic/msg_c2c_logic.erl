-module (msg_c2c_logic).
%%%
%  msg_c2c 业务逻辑模块
%%%
-export ([check_msg/2]).

-include("chat.hrl").
-include("common.hrl").

%% 检查离线消息
% 单聊离线消息，每个离线用户的消息获取10条（差不多一屏幕多），如果多以10条，再返回消除总数量
%%
check_msg(Uid, Pid) ->
    % ?LOG(["msg_c2c_logic/check_msg/2", Uid, Pid]),
    Msgs = msg_c2c_ds:read_msg(Uid, ?SAVE_MSG_LIMIT, undefined),
    % 发送单聊离线消息
    sent_offline_msg(Pid, Msgs, 0),
    ok.

sent_offline_msg(_Pid, [], _Index) ->
    ok;
sent_offline_msg(Pid, [Row|Tail], Index) ->
    {<<"msg_id">>, MsgId} = lists:keyfind(<<"msg_id">>, 1, Row),
    {<<"from_id">>, FromId} = lists:keyfind(<<"from_id">>, 1, Row),
    {<<"to_id">>, ToId} = lists:keyfind(<<"to_id">>, 1, Row),
    {<<"payload">>, Payload} = lists:keyfind(<<"payload">>, 1, Row),
    % ?LOG(["Row", Row, "; Payload: ", Payload]),
    Delay = 100 + Index * 100,
    Msg = [
        {<<"id">>, MsgId},
        {<<"type">>,<<"C2C">>},
        {<<"from">>, hashids_translator:uid_encode(FromId)},
        {<<"to">>, hashids_translator:uid_encode(ToId)},
        {<<"payload">>, jsone:decode(Payload, [{object_format, proplist}])},
        lists:keyfind(<<"created_at">>, 1, Row),
        lists:keyfind(<<"server_ts">>, 1, Row)
    ],
    % ?LOG([Delay, "Msg: ", Msg]),
    erlang:start_timer(Delay, Pid, jsone:encode(Msg)),
    sent_offline_msg(Pid, Tail, Index + 1).
