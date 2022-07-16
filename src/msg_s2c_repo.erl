-module(msg_s2c_repo).
%%%
% msg_s2c_repo 是 msg_s2c repository 缩写
%%%

-include_lib("imboy/include/log.hrl").

-export([read_msg/4]).
-export([write_msg/6]).
-export([delete_msg/1]).
-export([count_by_to_id/1]).
-export([delete_overflow_msg/2]).


read_msg(Where, Vals, Column, Limit) ->
    % use index i_ToId
    Sql = <<"SELECT ", Column/binary, " FROM `msg_s2c` ", Where/binary,
            " ORDER BY `id` ASC LIMIT ?">>,
    % ?LOG(Sql),
    mysql_pool:query(Sql, Vals ++ [Limit]).


write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS)
  when is_integer(FromId) ->
    FromId2 = list_to_binary(integer_to_list(FromId)),
    write_msg(CreatedAt, Id, Payload, FromId2, ToId, ServerTS);
write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS)
  when is_integer(ToId) ->
    ToId2 = list_to_binary(integer_to_list(ToId)),
    write_msg(CreatedAt, Id, Payload, FromId, ToId2, ServerTS);
write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS) ->
    % ?LOG([CreatedAt, Id, Payload, FromId, ToId, ServerTS]),
    Table = <<"`msg_s2c`">>,
    Column = <<"(`payload`, `from_id`, `to_id`,
        `created_at`, `server_ts`, `msg_id`)">>,
    CreatedAt2 = integer_to_binary(CreatedAt),
    ServerTS2 = integer_to_binary(ServerTS),
    Value = <<"('", Payload/binary, "', '", FromId/binary, "', '",
              ToId/binary, "', '", CreatedAt2/binary, "', '",
              ServerTS2/binary, "', '", Id/binary, "')">>,
    mysql_pool:replace_into(Table, Column, Value).


delete_msg(Id) when is_integer(Id) ->
    Where = <<"WHERE `id` = ?">>,
    delete_msg(Where, Id);
delete_msg(Id) ->
    Where = <<"WHERE `msg_id` = ?">>,
    delete_msg(Where, Id).


delete_msg(Where, Val) ->
    Sql = <<"DELETE FROM `msg_s2c` ", Where/binary>>,
    mysql_pool:query(Sql, [Val]).

% msg_s2c_repo:count_by_to_id(1).
count_by_to_id(ToUid) ->
    % use index i_ToId
    Sql = <<"SELECT count(*) as count
        FROM `msg_s2c` WHERE `to_id` = ?">>,
    mysql_pool:query(Sql, [ToUid]).


delete_overflow_msg(ToUid, Limit) ->
    Sql = <<"SELECT `id` FROM `msg_s2c`
        WHERE `to_id` = ? ORDER BY `id` ASC LIMIT ?">>,
    case mysql_pool:query(Sql, [ToUid, Limit]) of
        {ok, _, []} ->
            ok;
        {ok, _, Rows} ->
            [delete_msg(Id) || [Id] <- Rows],
            ok
    end.
