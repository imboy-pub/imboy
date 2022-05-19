-module(user_repo).
%%%
% user_repo 是 user repository 缩写
%%%

-include_lib("imboy/include/common.hrl").

-export([find_by_email/2,
         find_by_mobile/2,
         find_by_account/2]).
-export([find_by_id/1, find_by_id/2]).
-export([find_by_ids/2]).


find_by_email(Mobile, Column) ->
    Sql = <<"SELECT ", Column/binary,
            " FROM `user` WHERE `email` = ?">>,
    mysql_pool:query(Sql, [Mobile]).


find_by_mobile(Mobile, Column) ->
    Sql = <<"SELECT ", Column/binary,
            " FROM `user` WHERE `mobile` = ?">>,
    mysql_pool:query(Sql, [Mobile]).


find_by_account(Username, Column) ->
    Sql = <<"SELECT ", Column/binary,
            " FROM `user` WHERE `account` = ?">>,
    mysql_pool:query(Sql, [Username]).


find_by_id(Uid) ->
    Column = <<"`id`,`account`,`avatar`,`sign`">>,
    find_by_id(Uid, Column).


find_by_id(Uid, Column) ->
    Where = <<"WHERE `id` = ?">>,
    Sql = <<"SELECT ", Column/binary, " FROM `user` ", Where/binary>>,
    mysql_pool:query(Sql, [Uid]).


find_by_ids(Uids, Column) ->
    L1 = lists:flatmap(fun(Uid) -> [Uid, ","] end, Uids),
    [_ | L2] = lists:reverse(L1),
    Ids = list_to_binary(lists:concat(L2)),
    Where = <<"WHERE `id` IN (", Ids/binary, ")">>,
    Sql = <<"SELECT ", Column/binary, " FROM `user` ", Where/binary>>,
    mysql_pool:query(Sql, no_params).
