-module(user_setting_repo).
%%%
% user_setting_repo 是 user_setting repository 缩写
%%%

-include_lib("imboy/include/log.hrl").

-export([find_by_uid/2]).
-export([update/2]).


find_by_uid(Uid, Column) when is_binary(Uid) ->
    find_by_uid(imboy_hashids:uid_decode(Uid), Column);
find_by_uid(Uid, Column) when is_integer(Uid) ->
    Where = <<"WHERE `user_id` = ?">>,
    Sql = <<"SELECT ", Column/binary, " FROM `user_setting` ",
            Where/binary>>,
    mysql_pool:query(Sql, [Uid]).


update(Uid, Setting) when is_binary(Uid) ->
    update(imboy_hashids:uid_decode(Uid), Setting);
update(Uid, Setting) when is_integer(Uid) ->
    Sql = <<"REPLACE INTO `user_setting`
        (`user_id`, `setting`, `updated_at`) VALUES (?, ?, ?)">>,
    UpAt = imboy_dt:millisecond(),
    % ?LOG([Sql, [Uid, jsone:encode(Setting), UpAt]]),
    mysql_pool:query(
        Sql,
        [Uid, jsone:encode(Setting, [native_utf8]), UpAt]
    ).
