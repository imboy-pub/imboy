-module(user_repo).
%%%
% user_repo 是 user repository 缩写
%%%

-include_lib("imboy/include/log.hrl").

-export([tablename/0]).
-export([
    find_by_email/2
    , find_by_mobile/2
    , find_by_account/2
]).
-export([find_by_id/1, find_by_id/2]).
-export([find_by_ids/2]).

%% ===================================================================
%% API
%% ===================================================================

tablename() ->
    imboy_db:public_tablename(<<"user">>).

find_by_email(Mobile, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE email = $1">>,
    imboy_db:query(Sql, [Mobile]).


find_by_mobile(Mobile, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE mobile = $1">>,
    imboy_db:query(Sql, [Mobile]).


find_by_account(Username, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE account = $1">>,
    imboy_db:query(Sql, [Username]).


find_by_id(Uid) ->
    Column = <<"id,account,avatar,sign">>,
    find_by_id(Uid, Column).


find_by_id(Uid, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE id = $1">>,
    % ?LOG([Sql]),
    imboy_db:query(Sql, [Uid]).


find_by_ids(Uids, Column) ->
    Tb = tablename(),
    L1 = lists:flatmap(fun(Uid) -> [Uid, ","] end, Uids),
    [_ | L2] = lists:reverse(L1),
    Ids = list_to_binary(lists:concat(L2)),
    Where = <<" WHERE id IN (", Ids/binary, ")">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_db:query(Sql).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
