-module(msg_c2g_timeline_repo).
%%%
% msg_c2g_timeline_repo 是 msg_c2g_timeline repository 缩写
%%%

-include_lib("imboy/include/log.hrl").

-export([find_by_uid/2, find_by_uid/3]).
-export([delete_timeline/1]).
-export([check_msg/1]).
-export([count_by_to_id/1]).
-export([delete_overflow_timeline/2]).

find_by_uid(Uid, Column) ->
    find_by_uid(Uid, Column, 1000).


find_by_uid(Uid, Column, Limit) ->
    % use index i_ToId
    Where = <<"WHERE `to_id` = ? LIMIT ?">>,
    Sql = <<"SELECT ", Column/binary, " FROM `msg_c2g_timeline`",
            Where/binary>>,
    mysql_pool:query(Sql, [Uid, Limit]).


delete_timeline(Id) ->
    Where = <<"WHERE `id` = ?">>,
    Sql = <<"DELETE FROM `msg_c2g_timeline` ", Where/binary>>,
    mysql_pool:query(Sql, [Id]).


check_msg(MsgId) ->
    Sql = <<"SELECT count(*) as count
        FROM `msg_c2g_timeline` WHERE `msg_id` = ?">>,
    mysql_pool:query(Sql, [MsgId]).

% msg_c2g_timeline_repo:count_by_to_id(1).
count_by_to_id(ToUid) ->
    % use index i_ToId
    Sql = <<"SELECT count(*) as count
        FROM `msg_c2g_timeline` WHERE `to_id` = ?">>,
    mysql_pool:query(Sql, [ToUid]).


delete_overflow_timeline(ToUid, Limit) ->
    % use index i_ToId
    Sql = <<"SELECT `id`,`msg_id` FROM `msg_c2g_timeline`
        WHERE `to_id` = ? ORDER BY `id` ASC LIMIT ?">>,
    case mysql_pool:query(Sql, [ToUid, Limit]) of
        {ok, _, []} ->
            ok;
        {ok, _, Rows} ->
            [delete_timeline(Id) || [Id, _MsgId] <- Rows],
            {msg_ids, [MsgId || [_Id, MsgId] <- Rows]}
    end.
