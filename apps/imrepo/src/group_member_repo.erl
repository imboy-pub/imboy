-module(group_member_repo).
%%%
% group_member_repo 是 group_member repository 缩写
%%%
-export([tablename/0]).
-export([find_by_gid/2, find_by_gid/3]).
-export([find_by_uid/2, find_by_uid/3]).

%% ===================================================================
%% API
%% ===================================================================


tablename() ->
    imboy_db:public_tablename(<<"group_member">>).


find_by_gid(Gid, Column) ->
    find_by_gid(Gid, Column, 10000).


find_by_gid(Gid, Column, Limit) ->
    Tb = tablename(),
    Where = <<" WHERE group_id = $1 AND status = 1 LIMIT $2">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_db:query(Sql, [Gid, Limit]).


find_by_uid(Uid, Column) ->
    find_by_uid(Uid, Column, 10000).


find_by_uid(Uid, Column, Limit) ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1 AND status = 1 LIMIT $2">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_db:query(Sql, [Uid, Limit]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
