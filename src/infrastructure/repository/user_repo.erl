-module (user_repo).

-export ([find_by_mobile/1, find_by_username/1]).

% -spec find_by_mobile(Mobile::list()) ->

find_by_mobile(Mobile) ->
    Sql = <<"SELECT `id`,`username`, `password`,`avatar` FROM `user` WHERE `mobile` = ?">>,
    imboy_db:query(Sql, [Mobile]).

find_by_username(Username) ->
    Sql = <<"SELECT `id`,`username`, `password`,`avatar` FROM `user` WHERE `username` = ?">>,
    imboy_db:query(Sql, [Username]).
