-module (user_repo).
%%%
% user_repo 是 user repository 缩写
%%%
-export ([find_by_mobile/1, find_by_account/1]).
-export ([find_by_id/1, find_by_id/2]).
-export ([find_by_ids/2]).

-include("imboy.hrl").

% -spec find_by_mobile(Mobile::list()) ->

find_by_mobile(Mobile) ->
    Sql = <<"SELECT `id`,`account`, `password`, `nickname`,`avatar` FROM `user` WHERE `mobile` = ?">>,
    imboy_db:query(Sql, [Mobile]).

find_by_account(Username) ->
    Sql = <<"SELECT `id`,`account`, `password`,`nickname`,`avatar` FROM `user` WHERE `account` = ?">>,
    imboy_db:query(Sql, [Username]).

find_by_id(Uid) ->
    Column = <<"`id`,`account`,`avatar`,`sign`">>,
    find_by_id(Uid, Column).

find_by_id(Uid, Column) ->
    Where = <<"WHERE `id` = ?">>,
    Sql = <<"SELECT ", Column/binary, " FROM `user` ", Where/binary>>,
    imboy_db:query(Sql, [Uid]).

find_by_ids(Uids, Column) ->
    L1 = lists:flatmap(fun(Uid)->[Uid, ","] end, Uids),
    [_|L2] = lists:reverse(L1),
    Ids = list_to_binary(lists:concat(L2)),
    Where = <<"WHERE `id` IN (", Ids/binary,")">>,
    Sql = <<"SELECT ", Column/binary, " FROM `user` ", Where/binary>>,
    imboy_db:query(Sql, no_params).
