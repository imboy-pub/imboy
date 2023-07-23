-module(msg_c2g_repo).
%%%
% msg_c2g_repo 是 msg_c2g repository 缩写
%%%

-include_lib("imboy/include/chat.hrl").
-include_lib("imboy/include/log.hrl").

-export([tablename/0]).
-export([write_msg/6]).
-export([find_by_ids/2]).
-export([delete_msg/1]).

%% ===================================================================
%% API
%% ===================================================================

tablename() ->
    imboy_db:public_tablename(<<"msg_c2g">>).

write_msg(CreatedAt, Id, Payload, FromId, ToUids, Gid)
  when is_integer(FromId) ->
    FromId2 = integer_to_binary(FromId),
    write_msg(CreatedAt, Id, Payload, FromId2, ToUids, Gid);
write_msg(CreatedAt, Id, Payload, FromId, ToUids, Gid)
  when is_integer(Gid) ->
    Gid2 = integer_to_binary(Gid),
    write_msg(CreatedAt, Id, Payload, FromId, ToUids, Gid2);
% 批量插入群离线消息表 及 时间线表
write_msg(CreatedAt, MsgId, Payload, FromId, ToUids, Gid) ->
    Tb = tablename(),
    % ?LOG([CreatedAt, Payload, FromId, ToUids, Gid]),
    imboy_db:with_transaction(fun(Conn) ->
        CreatedAt2 = integer_to_binary(CreatedAt),
        Payload2 = imboy_hasher:encoded_val(Payload),
        % ?LOG(CreatedAt2),
        Column = <<"(payload,to_groupid,from_id,created_at,msg_id)">>,
        Sql = <<"INSERT INTO ", Tb/binary," ",
               Column/binary, " VALUES(",
               Payload2/binary, ", '",
               Gid/binary, "', '",
               FromId/binary, "', '",
               CreatedAt2/binary, "', '",
               MsgId/binary, "') RETURNING id;">>,
        % ?LOG(Sql),
        {ok, Stmt} = epgsql:parse(Conn, Sql),
        [{ok,1,[{_Msg_c2g_id}]}] = epgsql:execute_batch(Conn, [{Stmt, []}]),

        Vals = lists:map(fun(ToId) ->
            % 检查离线消息数量，如果数量大于limit 删除旧数据、插入新数据
            case msg_c2g_timeline_repo:count_by_to_id(ToId) of
               Count when Count >= ?SAVE_MSG_LIMIT ->
                   Limit = Count - ?SAVE_MSG_LIMIT + 1,
                   case msg_c2g_timeline_repo:delete_overflow_timeline(ToId, Limit) of
                       {msg_ids, MsgIds} ->
                           [case msg_c2g_timeline_repo:check_msg(CheckMsgId) of
                                0 ->
                                    msg_c2g_ds:delete_msg(CheckMsgId);
                                _ ->
                                    ok
                            end ||
                               CheckMsgId <- MsgIds];
                       _ ->
                           ok
                   end;
               _ ->
                   ok
            end,
            ToId2 = list_to_binary(integer_to_list(ToId)),
            Val = <<"('"
            , MsgId/binary
            , "', '", ToId2/binary
            , "', '", Gid/binary
            , "', '", CreatedAt2/binary,
            "')">>,
           binary_to_list(Val)
        end, ToUids),
        L1 = lists:flatmap(fun(Val) -> [Val, ","] end, Vals),
        [_ | L2] = lists:reverse(L1),
        Values = list_to_binary(lists:concat(L2)),
        Column2 = <<"(msg_id,to_uid,to_gid,created_at)">>,
        Tb2 = msg_c2g_timeline_repo:tablename(),
        Sql2 = <<"INSERT INTO ", Tb2/binary, " ",
             Column2/binary, " VALUES",
             Values/binary>>,
        % ?LOG([Sql, Sql2]),

        {ok, Stmt2} = epgsql:parse(Conn, Sql2),
        epgsql:execute_batch(Conn, [{Stmt2, []}]),
        ok
    end),
    ok.


% msg_c2g_repo:find_by_ids(MsgIds, <<"payload">>).
find_by_ids([], _Column) ->
    {ok, [], []};
find_by_ids(Ids, Column) ->
    Tb = tablename(),
    L1 = lists:flatmap(fun(Id) -> [Id, "','"] end, Ids),
    [_ | L2] = lists:reverse(L1),
    Ids2 = erlang:iolist_to_binary(L2),
    Where = <<" WHERE msg_id IN ('", Ids2/binary, "')">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary,
            " order by created_at ASC">>,
    % ?LOG(Sql),
    imboy_db:query(Sql).


% msg_c2g_repo:delete_msg(6).
delete_msg(Id) ->
    Where = <<"WHERE msg_id = $1">>,
    delete_msg(Where, Id).


delete_msg(Where, Val) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " ", Where/binary>>,
    imboy_db:execute(Sql, [Val]).
