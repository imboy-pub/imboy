-module (friend_category_repo).
%%%
% friend_category_repo 是 friend_category repository 缩写
%%%
-export ([find_by_uid/2]).
-export ([add/2]).
-export ([delete/2]).

find_by_uid(Uid, Column) ->
    find_by_uid(Uid, Column, 1000).

add(Uid, Name) ->
    Sql = <<"INSERT INTO `user_friend_category` (`name`, `owner_user_id`) VALUES (?, ?)">>,
    mysql_pool:execute(Sql, [Name, Uid]).

delete(Uid, Id) ->
    Sql = <<"DELETE FROM `user_friend_category` WHERE `id` = ? AND `owner_user_id` = ?">>,
    mysql_pool:query(Sql, [Id, Uid]).

%% Internal.

find_by_uid(Uid, Column, Limit) ->
    Where = <<"WHERE `owner_user_id` = ? LIMIT ?">>,
    Sql = <<"SELECT ", Column/binary,
        " FROM `user_friend_category`",
        Where/binary>>,
    mysql_pool:query(Sql, [Uid, Limit]).
