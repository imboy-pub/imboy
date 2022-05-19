-module(group_member_repo).
%%%
% group_member_repo 是 group_member repository 缩写
%%%
-export([find_by_group_id/2, find_by_group_id/3]).
-export([find_by_uid/2, find_by_uid/3]).


find_by_group_id(Gid, Column) ->
    find_by_group_id(Gid, Column, 10000).


find_by_group_id(Gid, Column, Limit) ->
    Where = <<"WHERE `group_id` = ? AND `status` = 1 LIMIT ?">>,
    Sql = <<"SELECT ", Column/binary, " FROM `group_member` ",
            Where/binary>>,
    mysql_pool:query(Sql, [Gid, Limit]).


find_by_uid(Uid, Column) ->
    find_by_uid(Uid, Column, 10000).


find_by_uid(Uid, Column, Limit) ->
    Where = <<"WHERE `user_id` = ? AND `status` = 1 LIMIT ?">>,
    Sql = <<"SELECT ", Column/binary, " FROM `group_member` ",
            Where/binary>>,
    mysql_pool:query(Sql, [Uid, Limit]).
