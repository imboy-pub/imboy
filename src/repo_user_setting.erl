-module(repo_user_setting).
%%%
% repo_user_setting 是 user_setting repository 缩写
%%%
-export([find_by_uid/2]).
-export([update/2]).

-include("common.hrl").


find_by_uid(Uid, Column) ->
    Where = <<"WHERE `user_id` = ?">>,
    Sql = <<"SELECT ", Column/binary, " FROM `user_setting` ",
            Where/binary>>,
    mysql_pool:query(Sql, [Uid]).


update(Uid, Setting) ->
    Sql = <<"REPLACE INTO `user_setting`
        (`user_id`, `setting`, `updated_at`) VALUES (?, ?, ?)">>,
    % ?LOG([Sql, [Uid, jsone:encode(Setting), util_dt:milliseconds()]]),
    mysql_pool:query(Sql,
                     [Uid,
                      jsone:encode(Setting, [native_utf8]),
                      util_dt:milliseconds()]).
