-module(user_setting_repo).
%%%
% user_setting_repo 是 user_setting repository 缩写
%%%

-include_lib("imboy/include/log.hrl").

-export([tablename/0]).
-export([find_by_uid/2]).
-export([update/2]).

%% ===================================================================
%% API
%% ===================================================================
tablename() ->
    imboy_db:public_tablename(<<"user_setting">>).

find_by_uid(Uid, Column) when is_binary(Uid) ->
    find_by_uid(imboy_hashids:uid_decode(Uid), Column);
find_by_uid(Uid, Column) when is_integer(Uid) ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_db:query(Sql, [Uid]).


update(Uid, Setting) when is_binary(Uid) ->
    update(imboy_hashids:uid_decode(Uid), Setting);
update(Uid, Setting) when is_integer(Uid) ->
    Tb = tablename(),
    Sql = <<"REPLACE INTO ", Tb/binary, "
        (user_id, setting, updated_at) VALUES ($1, $2, $3)">>,
    UpAt = imboy_dt:millisecond(),
    % ?LOG([Sql, [Uid, jsone:encode(Setting), UpAt]]),
    imboy_db:execute(
        Sql,
        [Uid, jsone:encode(Setting, [native_utf8]), UpAt]
    ).
