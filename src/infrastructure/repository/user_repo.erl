-module (user_repo).

-export ([find_by_mobile/1, find_by_username/1]).
-export ([find_by_id/1, find_by_id/2]).

-export ([find_by_ids/2]).

% -spec find_by_mobile(Mobile::list()) ->

find_by_mobile(Mobile) ->
    Sql = <<"SELECT `id`,`username`, `password`,`avatar` FROM `user` WHERE `mobile` = ?">>,
    imboy_db:query(Sql, [Mobile]).

find_by_username(Username) ->
    Sql = <<"SELECT `id`,`username`, `password`,`avatar` FROM `user` WHERE `username` = ?">>,
    imboy_db:query(Sql, [Username]).

find_by_id(Uid) ->
    Column = <<"`id`,`username`,`avatar`,`sign`">>,
    find_by_id(Uid, Column).

find_by_id(Uid, Column) ->
    Where = <<"WHERE `id` = ?">>,
    Sql = <<"SELECT ", Column/binary, " FROM `user` ", Where/binary>>,
    imboy_db:query(Sql, [Uid]).

find_by_ids(Uids, Column) ->
    L1 = lists:flatmap(fun(X)->[X, ","] end, Uids),
    [_|L2] = lists:reverse(L1),
    Ids = list_to_binary(lists:concat(L2)),
    Where = <<"WHERE `id` IN (", Ids/binary,")">>,
    Sql = <<"SELECT ", Column/binary, " FROM `user` ", Where/binary>>,
    imboy_db:query(Sql, no_params).
