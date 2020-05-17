-module (user_setting_repo).
%%%
% user_setting_repo 是 user_setting repository 缩写
%%%
-export ([find_by_uid/2]).

-include("imboy.hrl").


find_by_uid(Uid, Column) ->
    Where = <<"WHERE `user_id` = ?">>,
    Sql = <<"SELECT ", Column/binary, " FROM `user_setting` ", Where/binary>>,
    imboy_db:query(Sql, [Uid]).
