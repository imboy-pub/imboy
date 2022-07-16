-module(user_setting_repo).
%%%
% user_setting_repo 是 user_setting repository 缩写
%%%

-include_lib("imboy/include/log.hrl").

-export([find_by_uid/2]).
-export([update/2]).


find_by_uid(Uid, Column) ->
    Where = <<"WHERE `user_id` = ?">>,
    Sql = <<"SELECT ", Column/binary, " FROM `user_setting` ",
            Where/binary>>,
    mysql_pool:query(Sql, [Uid]).


update(Uid, Setting) ->
    Sql = <<"REPLACE INTO `user_setting`
        (`user_id`, `setting`, `updated_at`) VALUES (?, ?, ?)">>,
    % ?LOG([Sql, [Uid, jsone:encode(Setting), imboy_dt:milliseconds()]]),
    mysql_pool:query(Sql,
                     [Uid,
                      jsone:encode(Setting, [native_utf8]),
                      imboy_dt:milliseconds()]).
