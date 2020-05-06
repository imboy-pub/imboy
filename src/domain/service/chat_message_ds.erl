-module (chat_message_ds).
%%%
% chat_message_ds 是 chat_message domain service 缩写
%%%
-export ([write_msg/3]).
-export ([read_msg/2]).
-export ([sent_offline_msg/3]).
-export ([delete_msg/1]).

-include("imboy.hrl").

-spec write_msg(binary(), integer(), integer()) -> any().

%% 存储离线消息
write_msg(Payload, FromId, ToId) ->
    % TODO 检查离线消息数量，如果数量大于limit 删除旧数据，插入新数据
    chat_message_repo:write_msg(Payload, FromId, ToId).

%% 读取离线消息
read_msg(ToUid, Limit) ->
    Column = <<"`id`, `payload`, `from_id`, `created_at`">>,
    {ok, ColumnList, Rows} = chat_message_repo:read_msg(ToUid, Column, Limit),
    if
        length(Rows) == 0  ->
            {error, "暂无离线消息"};
        true ->
            [lists:zipwith(fun(X, Y) -> {X,Y} end, ColumnList, Row) || Row <- Rows]
    end.

delete_msg(Id) ->
    chat_message_repo:delete_msg(Id).

sent_offline_msg(_Pid, [], _Index) ->
    ok;
sent_offline_msg(Pid, [Row|Tail], Index) ->
    {<<"id">>, Id} = lists:keyfind(<<"id">>, 1, Row),
    chat_message_ds:delete_msg(Id),
    {<<"payload">>, Msg} = lists:keyfind(<<"payload">>, 1, Row),
    Delay = 4000 + Index*300,
    erlang:start_timer(Delay, Pid, Msg),
    sent_offline_msg(Pid, Tail, Index + 1).
