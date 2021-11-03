-module (dialog_msg_repo).
%%%
% dialog_msg_repo 是 dialog_msg repository 缩写
%%%
-export ([read_msg/4]).
-export ([write_msg/5]).
-export ([delete_msg/1]).
-export ([count_by_to_id/1]).
-export ([delete_overflow_msg/2]).

-include("common.hrl").

read_msg(Where, Vals, Column, Limit) ->
    % use index i_ToId
    Sql = <<"SELECT ", Column/binary,
        " FROM `dialog_msg` ",
        Where/binary,
        " ORDER BY `id` ASC LIMIT ?">>,
    % ?LOG(Sql),
    mysql_pool:query(Sql, Vals ++ [Limit]).

write_msg(CreatedAt, Id, Payload, FromId, ToId) when is_integer(FromId) ->
    FromId2 = list_to_binary(integer_to_list(FromId)),
    write_msg(CreatedAt, Id, Payload, FromId2, ToId);
write_msg(CreatedAt, Id, Payload, FromId, ToId) when is_integer(ToId) ->
    ToId2 = list_to_binary(integer_to_list(ToId)),
    write_msg(CreatedAt, Id, Payload, FromId, ToId2);
write_msg(CreatedAt, Id, Payload, FromId, ToId) ->
    Table = <<"`dialog_msg`">>,
    Column = <<"(`payload`, `from_id`, `to_id`, `created_at`, `msg_id`)">>,
    CreatedAt2 = integer_to_binary(CreatedAt),
    Value = <<"('",
        Payload/binary, "', '",
        FromId/binary, "', '",
        ToId/binary, "', '",
        CreatedAt2/binary, "', '",
        Id/binary, "')">>,
    mysql_pool:replace_into(Table, Column, Value).

delete_msg(Id) when is_integer(Id) ->
    Where = <<"WHERE `id` = ?">>,
    delete_msg(Where, Id);
delete_msg(Id) ->
    Where = <<"WHERE `msg_id` = ?">>,
    delete_msg(Where, Id).

delete_msg(Where, Val) ->
    Sql = <<"DELETE FROM `dialog_msg` ",
        Where/binary>>,
    mysql_pool:query(Sql, [Val]).


count_by_to_id(ToUid) ->
    % use index i_ToId
    Sql = <<"SELECT count(*) as count FROM `dialog_msg` WHERE `to_id` = ?">>,
    mysql_pool:query(Sql, [ToUid]).

delete_overflow_msg(ToUid, Limit) ->
    Sql = <<"SELECT `id` FROM `dialog_msg` WHERE `to_id` = ? ORDER BY `id` ASC LIMIT ?">>,
    case mysql_pool:query(Sql, [ToUid, Limit]) of
        {ok, _, []} ->
            ok;
        {ok, _, Rows} ->
            [delete_msg(Id) || [Id] <- Rows],
            ok
    end.
