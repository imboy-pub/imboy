-module (group_chat_message_ds).
%%%
% group_chat_message_ds 是 group_chat_message domain service 缩写
%%%
-export ([write_msg/4]).
-export ([read_msg/2]).
-export ([delete_msg/1]).

-include("imboy.hrl").

-spec write_msg(binary(), integer(), integer(), integer()) -> any().

%% 存储离线消息
write_msg(Payload, FromId, ToId, Gid) ->
    % TODO 检查离线消息数量，如果数量大于limit 删除旧数据，插入新数据
    group_chat_message_repo:write_msg(Payload, FromId, ToId, Gid).

%% 读取离线消息
read_msg(ToUid, Limit) ->
    Column = <<"`id`, `msg_id`">>,
    {ok, _CoLi, Rows} = group_chat_message_timeline_repo:find_by_uid(ToUid, Column, Limit),
    MsgIds = lists:map(fun([Id, MsgId]) ->
        group_chat_message_timeline_repo:delete_msg(Id),
        MsgId
    end, Rows),
    Column2 = <<"`id`, `payload`">>,
    case group_chat_message_repo:find_by_ids(MsgIds, Column2) of
        {ok, _, []}  ->
            [];
        {ok, ColumnList2, Rows2} ->
            [lists:zipwith(fun(X, Y) -> {X,Y} end, ColumnList2, Row) || Row <- Rows2]
    end.

delete_msg(Id) ->
    group_chat_message_repo:delete_msg(Id).
