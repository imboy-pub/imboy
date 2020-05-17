-module (dialog_msg_repo).
%%%
% dialog_msg_repo 是 dialog_msg repository 缩写
%%%
-export ([read_msg/3]).
-export ([write_msg/3]).
-export ([delete_msg/1]).
-export ([count_by_to_id/1]).
-export ([delete_overflow_msg/2]).

-include("imboy.hrl").

read_msg(ToUid, Column, Limit) ->
    % use index i_ToId
    Where = <<"WHERE `to_id` = ?">>,
    Sql = <<"SELECT ", Column/binary,
        " FROM `dialog_msg` ",
        Where/binary,
        " ORDER BY `id` ASC LIMIT ?">>,
    % ?LOG(Sql),
    imboy_db:query(Sql, [ToUid, Limit]).

write_msg(Payload, FromId, ToId) when is_integer(FromId) ->
    FromId2 = list_to_binary(integer_to_list(FromId)),
    write_msg(Payload, FromId2, ToId);
write_msg(Payload, FromId, ToId) when is_integer(ToId) ->
    ToId2 = list_to_binary(integer_to_list(ToId)),
    write_msg(Payload, FromId, ToId2);
write_msg(Payload, FromId, ToId) ->
    Table = <<"`dialog_msg`">>,
    Column = <<"(`payload`, `from_id`, `to_id`, `created_at`, `payload_md5`)">>,
    Now = list_to_binary(integer_to_list(imboy_func:milliseconds())),
    Pmd5 = imboy_func:md5(Payload),
    Value = <<"('", Payload/binary, "', '", FromId/binary, "', '", ToId/binary, "', '", Now/binary, "', '", Pmd5/binary, "')">>,
    imboy_db:insert_into(Table, Column, Value).

delete_msg(Id) ->
    Where = <<"WHERE `id` = ?">>,
    Sql = <<"DELETE FROM `dialog_msg` ",
        Where/binary>>,
    imboy_db:query(Sql, [Id]).

count_by_to_id(ToUid) ->
    % use index i_ToId
    Sql = <<"SELECT count(*) as count FROM `dialog_msg` WHERE `to_id` = ?">>,
    imboy_db:query(Sql, [ToUid]).

delete_overflow_msg(ToUid, Limit) ->
    Sql = <<"SELECT `id` FROM `dialog_msg` WHERE `to_id` = ? ORDER BY `id` ASC LIMIT ?">>,
    case imboy_db:query(Sql, [ToUid, Limit]) of
        {ok, _, []} ->
            ok;
        {ok, _, Rows} ->
            [delete_msg(Id) || [Id] <- Rows],
            ok
    end.
