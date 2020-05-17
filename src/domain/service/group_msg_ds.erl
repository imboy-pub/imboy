-module (group_msg_ds).
%%%
% group_msg_ds 是 group_msg domain service 缩写
%%%
-export ([write_msg/4]).
-export ([read_msg/2]).
-export ([delete_msg/1]).

-include("imboy.hrl").

-spec write_msg(binary(), integer(), integer(), integer()) -> any().

%% 存储离线消息
write_msg(Payload, FromId, ToId, Gid) ->
    group_msg_repo:write_msg(Payload, FromId, ToId, Gid).

%% 读取离线消息
read_msg(ToUid, Limit) ->
    Column = <<"`id`, `msg_id`">>,
    {ok, _CoLi, Rows} = group_msg_timeline_repo:find_by_uid(ToUid, Column, Limit),
    MsgIds = lists:map(fun([Id, MsgId]) ->
        group_msg_timeline_repo:delete_timeline(Id),
        MsgId
    end, Rows),
    Column2 = <<"`id`, `payload`">>,
    case group_msg_repo:find_by_ids(MsgIds, Column2) of
        {ok, _, []}  ->
            [];
        {ok, ColumnList2, Rows2} ->
            [lists:zipwith(fun(X, Y) -> {X,Y} end, ColumnList2, Row) || Row <- Rows2]
    end.

delete_msg(Id) ->
    group_msg_repo:delete_msg(Id).
