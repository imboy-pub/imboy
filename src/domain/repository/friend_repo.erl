-module (friend_repo).

-export ([is_friend/2]).
-export ([find_by_uid/2]).

is_friend(FromUid, ToUid) ->
    Where = <<"WHERE `from_user_id` = ? AND `to_user_id` = ? AND `status` = 1">>,
    Sql = <<"SELECT count(*) as count FROM `user_friend` ",
        Where/binary>>,
    imboy_db:query(Sql, [FromUid, ToUid]).


find_by_uid(Uid, Column) ->
    find_by_uid(Uid, Column, 10000).

find_by_uid(Uid, Column, Limit) ->
    Where = <<"WHERE `from_user_id` = ? AND `status` = 1 LIMIT ?">>,
    Sql = <<"SELECT ", Column/binary,
        " FROM `user_friend` ",
        Where/binary>>,
    imboy_db:query(Sql, [Uid, Limit]).
