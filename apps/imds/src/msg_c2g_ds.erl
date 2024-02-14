-module(msg_c2g_ds).
%%%
% msg_c2g_ds 是 msg_c2g domain service 缩写
%%%
-export([write_msg/6]).
-export([read_msg/2]).
-export([delete_msg/1]).

-include_lib("imlib/include/log.hrl").


-spec write_msg(integer(), binary(), binary(), integer(), integer(), integer()) -> any().
%% 存储消息
% msg_c2g_ds:write_msg(1707686743435, <<"msg_id_1">>,  <<"{}">>,  1, [2,3,4], 1).
% msg_c2g_ds:write_msg(1707686743435, <<"msg_id_1">>,  <<"{\"a\":1}">>,  1, [2,3,107], 7).
write_msg(CreatedAt, Id, Payload, FromId, ToUids, Gid) ->
    Count = imboy_db:pluck(
        msg_c2g_repo:tablename()
        , <<"msg_id = '", Id/binary, "'">>
        , <<"count(*)">>
        , 0),
    % ?LOG([Count]),
    case Count of
        0 ->
            msg_c2g_repo:write_msg(CreatedAt, Id, Payload, FromId, ToUids, Gid);
        _ ->
            ok
    end.


%% 读取离线消息
% msg_c2g_ds:read_msg(3, 10).
read_msg(ToUid, Limit) ->
    Column = <<"msg_id">>,
    {ok, _CoLi, Rows} = msg_c2g_timeline_repo:list_by_uid(ToUid, Column, Limit),
    MsgIds = lists:map(fun({MsgId}) ->
                               MsgId
                       end,
                       Rows),
    % Column2 = <<"payload">>,
    Column2 = imboy_hasher:decoded_payload(),
    case msg_c2g_repo:list_by_ids(MsgIds, Column2) of
        {ok, _, []} ->
            [];
        {ok, ColumnList2, Rows2} ->
            [ lists:zipwith(fun(X, Y) -> {X, Y} end, ColumnList2, tuple_to_list(Row)) || Row <- Rows2 ]
    end.


delete_msg(Id) ->
    msg_c2g_repo:delete_msg(Id).
