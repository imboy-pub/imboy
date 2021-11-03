-module (dialog_msg_ds).
%%%
% dialog_msg_ds 是 dialog_msg domain service 缩写
%%%
-export ([write_msg/5]).
-export ([read_msg/3]).
-export ([delete_msg/1]).

-include("chat.hrl").
-include("common.hrl").

-spec write_msg(integer(), binary(), binary(), integer(), integer()) -> any().

%% 存储消息
write_msg(CreatedAt, Id, Payload, FromId, ToId) ->
    % 检查消息存储数量，如果数量大于limit 删除旧数据、插入新数据
    case dialog_msg_repo:count_by_to_id(ToId) of
        {ok, _, [[Count]]} when Count >= ?SAVE_MSG_LIMIT ->
            Limit = Count - ?SAVE_MSG_LIMIT + 1,
            dialog_msg_repo:delete_overflow_msg(ToId, Limit);
        _ ->
        ok
    end,
    dialog_msg_repo:write_msg(CreatedAt, Id, Payload, FromId, ToId).

%% 读取消息
read_msg(ToUid, Limit, undefined) ->
    Column = <<"`id`, `payload`, `from_id`, `created_at`, `msg_id`">>,
    Where = <<"WHERE `to_id` = ?">>,
    Vals = [ToUid],
    {ok, ColumnList, Rows} = dialog_msg_repo:read_msg(Where, Vals, Column, Limit),
    [lists:zipwith(fun(X, Y) -> {X,Y} end, ColumnList, Row) || Row <- Rows];
read_msg(ToUid, Limit, Ts) when is_binary(Ts) ->
    read_msg(ToUid, Limit, binary_to_integer(Ts));
read_msg(ToUid, Limit, Ts) ->
    Column = <<"`id`, `payload`, `from_id`, `created_at`, `msg_id`">>,
    Where = <<"WHERE `to_id` = ? AND `created_at` > ?">>,
    Vals = [ToUid, Ts],
    {ok, ColumnList, Rows} = dialog_msg_repo:read_msg(Where, Vals, Column, Limit),
    [lists:zipwith(fun(X, Y) -> {X,Y} end, ColumnList, Row) || Row <- Rows].

delete_msg(Id) ->
    dialog_msg_repo:delete_msg(Id).
