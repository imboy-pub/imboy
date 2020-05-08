-module (group_chat_message_as).
%%%
% group_chat_message_as 是 group_chat_message application service 缩写
%%%
-export ([check_msg/2]).

-include("imboy.hrl").

%%
check_msg(Uid, Pid) ->
    GMsgs = group_chat_message_ds:read_msg(Uid, 1000),
    sent_offline_msg(Uid, Pid, GMsgs, 0),
    ok.

sent_offline_msg(_Uid, _Pid, [], _Index) ->
    ok;
sent_offline_msg(Uid, Pid, [Row|Tail], Index) ->
    {<<"id">>, Id} = lists:keyfind(<<"id">>, 1, Row),
    case group_chat_message_timeline_repo:check_msg(Id) of
        {ok, _, [[0]]} ->
            group_chat_message_ds:delete_msg(Id);
        {ok, _, _} ->
            ok
    end,
    {<<"payload">>, Msg} = lists:keyfind(<<"payload">>, 1, Row),
    Delay = 3000 + Index * 100,
    erlang:start_timer(Delay, Pid, Msg),
    sent_offline_msg(Uid, Pid, Tail, Index + 1).
