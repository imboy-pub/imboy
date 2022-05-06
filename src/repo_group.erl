-module(repo_group).
%%%
% repo_group 是 group repository 缩写
%%%
-export([find_by_ids/2]).
-export([find_by_uid/2, find_by_uid/3]).


find_by_ids(Ids, Column) ->
    L1 = lists:flatmap(fun(Id) -> [Id, ","] end, Ids),
    [_ | L2] = lists:reverse(L1),
    Ids2 = list_to_binary(lists:concat(L2)),
    Where = <<"WHERE `id` IN (", Ids2/binary, ")">>,
    Sql = <<"SELECT ", Column/binary, " FROM `group` ", Where/binary>>,
    mysql_pool:query(Sql, no_params).


find_by_uid(Uid, Column) ->
    find_by_uid(Uid, Column, 10000).


find_by_uid(Uid, Column, Limit) ->
    Where = <<"WHERE `owner_uid` = ? AND `status` = 1 LIMIT ?">>,
    Sql = <<"SELECT ", Column/binary, " FROM `group` ", Where/binary>>,
    mysql_pool:query(Sql, [Uid, Limit]).
