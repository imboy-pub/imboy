-module (chat_message_repo).
%%%
% chat_message_repo 是 chat_message repository 缩写
%%%
-export ([read_msg/3]).
-export ([write_msg/3]).
-export ([find_by_uid/2]).
-export ([delete_msg/1]).

-include("imboy.hrl").

read_msg(ToUid, Column, Limit) ->
    Where = <<"WHERE `to_id` = ? AND `status` = 2">>,
    Sql = <<"SELECT ", Column/binary,
        " FROM `chat_message` ",
        Where/binary,
        " ORDER BY ID ASC LIMIT ?">>,
    % ?LOG(Sql),
    imboy_db:query(Sql, [ToUid, Limit]).

write_msg(Payload, FromId, ToId) when is_integer(FromId) ->
    FromId2 = list_to_binary(integer_to_list(FromId)),
    write_msg(Payload, FromId2, ToId);
write_msg(Payload, FromId, ToId) when is_integer(ToId) ->
    ToId2 = list_to_binary(integer_to_list(ToId)),
    write_msg(Payload, FromId, ToId2);
write_msg(Payload, FromId, ToId) ->
    Table = <<"`chat_message`">>,
    Column = <<"(`payload`, `from_id`, `to_id`, `read_at`, `status`, `created_at`, `payload_md5`)">>,
    Now = list_to_binary(integer_to_list(imboy_func:milliseconds())),
    Pmd5 = imboy_func:md5(Payload),
    Value = <<"('", Payload/binary, "', '", FromId/binary, "', '", ToId/binary, "',", "0,", "2", ",'", Now/binary, "', '", Pmd5/binary, "')">>,
    imboy_db:insert_into(Table, Column, Value).

find_by_uid(Uid, Column) ->
    find_by_uid(Uid, Column, 1000).

find_by_uid(Uid, Column, Limit) ->
    Where = <<"WHERE `owner_user_id` = ? AND `status` = 1 LIMIT ?">>,
    Sql = <<"SELECT ", Column/binary,
        " FROM `chat_message`",
        Where/binary>>,
    imboy_db:query(Sql, [Uid, Limit]).

delete_msg(Id) ->
    Where = <<"WHERE `id` = ?">>,
    Sql = <<"DELETE FROM `chat_message` ",
        Where/binary>>,
    imboy_db:query(Sql, [Id]).
