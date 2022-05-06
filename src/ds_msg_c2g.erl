-module(ds_msg_c2g).
%%%
% ds_msg_c2g 是 msg_c2g domain service 缩写
%%%
-export([write_msg/6]).
-export([read_msg/2]).
-export([delete_msg/1]).

-include("common.hrl").


-spec write_msg(integer(),
                binary(),
                binary(),
                integer(),
                integer(),
                integer()) -> any().
%% 存储消息
write_msg(CreatedAt, Id, Payload, FromId, ToIds, Gid) ->
    repo_msg_c2g:write_msg(CreatedAt, Id, Payload, FromId, ToIds, Gid).


%% 读取离线消息
read_msg(ToUid, Limit) ->
    Column = <<"`id`, `msg_id`">>,
    {ok, _CoLi, Rows} = repo_msg_c2g_timeline:find_by_uid(ToUid,
                                                          Column,
                                                          Limit),
    MsgIds = lists:map(fun([Id, MsgId]) ->
                              repo_msg_c2g_timeline:delete_timeline(Id),
                              MsgId
                       end,
                       Rows),
    Column2 = <<"`id`, `payload`">>,
    case repo_msg_c2g:find_by_ids(MsgIds, Column2) of
        {ok, _, []} ->
            [];
        {ok, ColumnList2, Rows2} ->
            [lists:zipwith(fun(X, Y) -> {X, Y} end, ColumnList2, Row) ||
                Row <- Rows2]
    end.


delete_msg(Id) ->
    repo_msg_c2g:delete_msg(Id).
