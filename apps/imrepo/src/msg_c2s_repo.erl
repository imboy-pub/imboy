-module(msg_c2s_repo).
%%%
% msg_c2s_repo 是 msg_c2s repository 缩写
%%%

-include_lib("imlib/include/chat.hrl").
-include_lib("imlib/include/log.hrl").

-export([tablename/0]).
-export([write_msg/6]).
-export([list_by_ids/2]).
-export([delete_msg/1]).

%% ===================================================================
%% API
%% ===================================================================


tablename() ->
    imboy_db:public_tablename(<<"msg_c2s">>).


write_msg(CreatedAt, Id, Payload, FromId, ToUids, Gid) when is_integer(FromId) ->
    FromId2 = integer_to_binary(FromId),
    write_msg(CreatedAt, Id, Payload, FromId2, ToUids, Gid);
write_msg(CreatedAt, Id, Payload, FromId, ToUids, Gid) when is_integer(Gid) ->
    Gid2 = integer_to_binary(Gid),
    write_msg(CreatedAt, Id, Payload, FromId, ToUids, Gid2);
% 批量插入群离线消息表 及 时间线表
write_msg(CreatedAt, MsgId, Payload, FromId, ToUids, Gid) ->
    Tb = tablename(),
    % ?DEBUG_LOG([CreatedAt, Payload, FromId, ToUids, Gid]),
    imboy_db:with_transaction(fun(Conn) ->
        Payload2 = imboy_hasher:encoded_val(Payload),
        % ?DEBUG_LOG(CreatedAt),
        Column = <<"(payload,to_groupid,from_id,created_at,msg_id)">>,
        Sql = <<"INSERT INTO ", Tb/binary, " ", Column/binary, " VALUES(", Payload2/binary,
              ", '", Gid/binary, "', '", FromId/binary, "', '", CreatedAt/binary,
              "', '", MsgId/binary, "');">>,
        % ?DEBUG_LOG(Sql),
        %% 使用统一封装的执行接口，避免直接依赖 epgsql
        ok = imboy_db:execute(Conn, Sql, []),
        % Res = epgsql:execute_batch(Conn, [{Stmt, []}]),
        % ?DEBUG_LOG(["Res", Res]), % [{ok,1}]
         % [{ok, 1, _}] = Res,
        Vals = lists:map(fun(ToId) ->
                ToId2 = ec_cnv:to_binary(ToId),
               Val = <<"('", MsgId/binary, "', '", ToId2/binary, "', '",
                       Gid/binary, "', '", CreatedAt/binary, "')">>,
               binary_to_list(Val)
           end,
           ToUids),
        L1 = lists:flatmap(fun(Val) -> [Val, ","] end, Vals),
        [_ | L2] = lists:reverse(L1),
        Values = list_to_binary(lists:concat(L2)),
        Column2 = <<"(msg_id,to_uid,to_gid,created_at)">>,
        Tb2 = msg_c2g_timeline_repo:tablename(),
        Sql2 = <<"INSERT INTO ", Tb2/binary, " ", Column2/binary, " VALUES",
               Values/binary>>,
        % ?DEBUG_LOG([Sql, Sql2]),
        %% 使用统一封装的执行接口，避免直接依赖 epgsql
        ok = imboy_db:execute(Conn, Sql2, []),
        ok
    end),
    ok.


% msg_c2s_repo:list_by_ids(MsgIds, <<"payload">>).
list_by_ids([], _Column) ->
    {ok, [], []};
list_by_ids(Ids, Column) ->
    Tb = tablename(),
    L1 = lists:flatmap(fun(Id) -> [Id, "','"] end, Ids),
    [_ | L2] = lists:reverse(L1),
    Ids2 = erlang:iolist_to_binary(L2),
    Where = <<" WHERE msg_id IN ('", Ids2/binary, "')">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary, " order by created_at ASC">>,
    % ?DEBUG_LOG(Sql),
    imboy_db:query(Sql).


% msg_c2s_repo:delete_msg(6).
delete_msg(Id) ->
    Where = <<"WHERE msg_id = $1">>,
    delete_msg(Where, Id).


delete_msg(Where, Val) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " ", Where/binary>>,
    imboy_db:execute(Sql, [Val]).
