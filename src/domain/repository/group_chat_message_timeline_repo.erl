-module (group_chat_message_timeline_repo).
%%%
% group_chat_message_timeline_repo 是 group_chat_message_timeline repository 缩写
%%%
-export ([find_by_uid/2, find_by_uid/3]).
-export ([delete_msg/1]).
-export ([check_msg/1]).

-include("imboy.hrl").

find_by_uid(Uid, Column) ->
    find_by_uid(Uid, Column, 1000).

find_by_uid(Uid, Column, Limit) ->
    Where = <<"WHERE `to_id` = ? LIMIT ?">>,
    Sql = <<"SELECT ", Column/binary,
        " FROM `group_chat_message_timeline`",
        Where/binary>>,
    imboy_db:query(Sql, [Uid, Limit]).

delete_msg(Id) ->
    Where = <<"WHERE `id` = ?">>,
    Sql = <<"DELETE FROM `group_chat_message_timeline` ",
        Where/binary>>,
    imboy_db:query(Sql, [Id]).

check_msg(MsgId) ->
    Sql = <<"SELECT count(*) as count FROM `group_chat_message_timeline` WHERE `msg_id` = ?">>,
    imboy_db:query(Sql, [MsgId]).
