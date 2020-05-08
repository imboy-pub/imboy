-module (group_chat_message_repo).
%%%
% group_chat_message_repo 是 group_chat_message repository 缩写
%%%
-export ([write_msg/4]).
-export ([find_by_ids/2]).
-export ([delete_msg/1]).

-include("imboy.hrl").

write_msg(Payload, FromId, ToUids, Gid) when is_integer(FromId) ->
    FromId2 = list_to_binary(integer_to_list(FromId)),
    write_msg(Payload, FromId2, ToUids, Gid);
write_msg(Payload, FromId, ToUids, Gid) when is_integer(Gid) ->
    Gid2 = list_to_binary(integer_to_list(Gid)),
    write_msg(Payload, FromId, ToUids, Gid2);
% 批量插入群离线消息表 及 时间线表
write_msg(Payload, FromId, ToUids, Gid) ->
    % ?LOG([Payload, FromId, ToUids, Gid]),
    poolboy:transaction(mysql, fun(ConnectPid) ->
        Column = <<"(`payload`,`to_groupid`,`from_id`,`created_at`,`payload_md5`)">>,
        Now = list_to_binary(integer_to_list(imboy_func:milliseconds())),
        Pmd5 = imboy_func:md5(Payload),
        Sql = <<"REPLACE INTO `group_chat_message` ",
            Column/binary,
            " VALUES('",
            Payload/binary, "', '",
            Gid/binary, "', '",
            FromId/binary, "', '",
            Now/binary, "', '",
            Pmd5/binary, "')">>,
        % ?LOG(Sql),
        mysql:query(ConnectPid, Sql),
        MsgId = mysql:insert_id(ConnectPid),
        % ?LOG(MsgId),
        MsgId2 = list_to_binary(integer_to_list(MsgId)),

        Vals = lists:map(fun(ToId) ->
            ToId2 = list_to_binary(integer_to_list(ToId)),
            binary_to_list(<<"('",
            Gid/binary, "', '",
            ToId2/binary, "', '",
            MsgId2/binary, "', '",
            Now/binary, "')">>)
         end, ToUids),
        L1 = lists:flatmap(fun(Val)->[Val , ","] end, Vals),
        [_|L2] = lists:reverse(L1),
        Values = list_to_binary(lists:concat(L2)),
        Column2 = <<"(`to_groupid`,`to_id`,`msg_id`,`created_at`)">>,
        Sql2 = <<"REPLACE INTO `group_chat_message_timeline` ",
            Column2/binary,
            " VALUES", Values/binary>>,
        % ?LOG([Sql, Sql2]),
        mysql:query(ConnectPid, Sql2)
    end),
    ok.


find_by_ids([], _Column) ->
    {ok, [], []};
find_by_ids(Ids, Column) ->
    L1 = lists:flatmap(fun(Id)->[Id, ","] end, Ids),
    [_|L2] = lists:reverse(L1),
    Ids2 = list_to_binary(lists:concat(L2)),
    Where = <<"WHERE `id` IN (", Ids2/binary, ")">>,
    Sql = <<"SELECT ", Column/binary, " FROM `group_chat_message` ", Where/binary, " order by `id` ASC">>,
    imboy_db:query(Sql, no_params).

delete_msg(Id) ->
    Where = <<"WHERE `id` = ?">>,
    Sql = <<"DELETE FROM `group_chat_message` ",
        Where/binary>>,
    imboy_db:query(Sql, [Id]).
