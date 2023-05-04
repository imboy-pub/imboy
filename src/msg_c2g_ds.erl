-module(msg_c2g_ds).
%%%
% msg_c2g_ds 是 msg_c2g domain service 缩写
%%%
-export([write_msg/6]).
-export([read_msg/2]).
-export([delete_msg/1]).

-include_lib("imboy/include/log.hrl").


-spec write_msg(integer(),
                binary(),
                binary(),
                integer(),
                integer(),
                integer()) -> any().
%% 存储消息
% msg_c2g_ds:write_msg(1, <<"msg_id_1">>,  <<"{}">>,  1, [2,3,4], 1).
write_msg(CreatedAt, Id, Payload, FromId, ToIds, Gid) ->
    msg_c2g_repo:write_msg(CreatedAt, Id, Payload, FromId, ToIds, Gid).


%% 读取离线消息
% msg_c2g_ds:read_msg(3, 10).
read_msg(ToUid, Limit) ->
    Column = <<"msg_id">>,
    {ok, _CoLi, Rows} = msg_c2g_timeline_repo:find_by_uid(
        ToUid
        , Column
        , Limit
    ),
    MsgIds = lists:map(
        fun({MsgId}) ->
            % TODO 2023-05-05 17:56:43 ,需要在ACK的时候清理
            % msg_c2g_timeline_repo:delete_timeline(ToUid, MsgId),
            MsgId
        end
        , Rows
    ),
    Column2 = <<"payload">>,
    case msg_c2g_repo:find_by_ids(MsgIds, Column2) of
        {ok, _, []} ->
            [];
        {ok, ColumnList2, Rows2} ->
            [lists:zipwith(fun(X, Y) -> {X, Y} end, ColumnList2, tuple_to_list(Row)) ||
                Row <- Rows2]
    end.


delete_msg(Id) ->
    msg_c2g_repo:delete_msg(Id).
