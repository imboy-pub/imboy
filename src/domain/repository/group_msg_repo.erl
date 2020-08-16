-module (group_msg_repo).
%%%
% group_msg_repo 是 group_msg repository 缩写
%%%
-export ([write_msg/6]).
-export ([find_by_ids/2]).
-export ([delete_msg/1]).

-include("chat.hrl").
-include("common.hrl").

write_msg(CreatedAt, MsgMd5, Payload, FromId, ToUids, Gid) when is_integer(FromId) ->
    FromId2 = list_to_binary(integer_to_list(FromId)),
    write_msg(CreatedAt, MsgMd5, Payload, FromId2, ToUids, Gid);
write_msg(CreatedAt, MsgMd5, Payload, FromId, ToUids, Gid) when is_integer(Gid) ->
    Gid2 = list_to_binary(integer_to_list(Gid)),
    write_msg(CreatedAt, MsgMd5, Payload, FromId, ToUids, Gid2);
% 批量插入群离线消息表 及 时间线表
write_msg(CreatedAt, MsgMd5, Payload, FromId, ToUids, Gid) ->
    % ?LOG([CreatedAt, Payload, FromId, ToUids, Gid]),
    poolboy:transaction(mysql, fun(ConnectPid) ->
        CreatedAt2 = integer_to_binary(CreatedAt),
        % ?LOG(CreatedAt2),
        Column = <<"(`payload`,`to_groupid`,`from_id`,`created_at`,`msg_md5`)">>,
        Sql = <<"REPLACE INTO `group_msg` ",
            Column/binary,
            " VALUES('",
            Payload/binary, "', '",
            Gid/binary, "', '",
            FromId/binary, "', '",
            CreatedAt2/binary, "', '",
            MsgMd5/binary, "');">>,
        % ?LOG(Sql),
        mysql:query(ConnectPid, Sql),
        MsgId = mysql:insert_id(ConnectPid),
        % ?LOG(MsgId),
        MsgId2 = list_to_binary(integer_to_list(MsgId)),

        Vals = lists:map(fun(ToId) ->
            % 检查离线消息数量，如果数量大于limit 删除旧数据、插入新数据
            case group_msg_timeline_repo:count_by_to_id(ToId) of
                {ok, _, [[Count]]} when Count >= ?SAVE_MSG_LIMIT ->
                    Limit = Count - ?SAVE_MSG_LIMIT + 1,
                    case
                    group_msg_timeline_repo:delete_overflow_timeline(ToId, Limit) of
                        {msg_ids, MsgIds} ->
                            [
                                case group_msg_timeline_repo:check_msg(CheckMsgId) of
                                    {ok, _, [[0]]} ->
                                        group_msg_ds:delete_msg(CheckMsgId);
                                    {ok, _, _} ->
                                        ok
                                end || CheckMsgId <- MsgIds
                            ];
                        _ ->
                            ok
                    end;
                _ ->
                    ok
            end,
            ToId2 = list_to_binary(integer_to_list(ToId)),
            binary_to_list(<<"('",
            Gid/binary, "', '",
            ToId2/binary, "', '",
            MsgId2/binary, "', '",
            CreatedAt2/binary, "')">>)
         end, ToUids),
        L1 = lists:flatmap(fun(Val)->[Val , ","] end, Vals),
        [_|L2] = lists:reverse(L1),
        Values = list_to_binary(lists:concat(L2)),
        Column2 = <<"(`to_groupid`,`to_id`,`msg_id`,`created_at`)">>,
        Sql2 = <<"REPLACE INTO `group_msg_timeline` ",
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
    Sql = <<"SELECT ", Column/binary, " FROM `group_msg` ",
        Where/binary, " order by `id` ASC">>,
    mysql_pool:query(Sql, no_params).

delete_msg(Id) when is_integer(Id) ->
    Where = <<"WHERE `id` = ?">>,
    delete_msg(Where, Id);
delete_msg(MsgMd5) ->
    Where = <<"WHERE `msg_md5` = ?">>,
    delete_msg(Where, MsgMd5).
delete_msg(Where, Val) ->
    Sql = <<"DELETE FROM `group_msg` ",
        Where/binary>>,
    mysql_pool:query(Sql, [Val]).
