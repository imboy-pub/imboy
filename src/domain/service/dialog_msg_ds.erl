-module (dialog_msg_ds).
%%%
% dialog_msg_ds 是 dialog_msg domain service 缩写
%%%
-export ([write_msg/3]).
-export ([read_msg/2]).
-export ([delete_msg/1]).

-include("imboy.hrl").

-spec write_msg(binary(), integer(), integer()) -> any().

%% 存储离线消息
write_msg(Payload, FromId, ToId) ->
    % 检查离线消息数量，如果数量大于limit 删除旧数据、插入新数据
    case dialog_msg_repo:count_by_to_id(ToId) of
        {ok, _, [[Count]]} when Count >= ?OFFLINE_MSG_LIMIT ->
            Limit = Count - ?OFFLINE_MSG_LIMIT + 1,
            dialog_msg_repo:delete_overflow_msg(ToId, Limit);
        _ ->
        ok
    end,
    dialog_msg_repo:write_msg(Payload, FromId, ToId).

%% 读取离线消息
read_msg(ToUid, Limit) ->
    Column = <<"`id`, `payload`, `from_id`, `created_at`">>,
    {ok, ColumnList, Rows} = dialog_msg_repo:read_msg(ToUid, Column, Limit),
    [lists:zipwith(fun(X, Y) -> {X,Y} end, ColumnList, Row) || Row <- Rows].

delete_msg(Id) ->
    dialog_msg_repo:delete_msg(Id).
