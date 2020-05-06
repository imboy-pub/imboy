-module (friend_category_repo).
%%%
% friend_category_repo 是 friend_category repository 缩写
%%%
-export ([find_by_uid/2]).

find_by_uid(Uid, Column) ->
    find_by_uid(Uid, Column, 1000).

find_by_uid(Uid, Column, Limit) ->
    Where = <<"WHERE `owner_user_id` = ? AND `status` = 1 LIMIT ?">>,
    Sql = <<"SELECT ", Column/binary,
        " FROM `user_friend_category`",
        Where/binary>>,
    imboy_db:query(Sql, [Uid, Limit]).
