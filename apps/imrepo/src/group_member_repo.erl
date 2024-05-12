-module(group_member_repo).
%%%
% group_member_repo 是 group_member repository 缩写
%%%
-export([tablename/0]).
-export ([add/1]).
-export ([add/2]).
-export ([find/3]).
-export([list_by_gid/2, list_by_gid/3]).
-export([list_by_uid/2, list_by_uid/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================


tablename() ->
    imboy_db:public_tablename(<<"group_member">>).


add(Data) ->
    Tb = tablename(),
    imboy_db:insert_into(Tb, Data).

add(Conn, Data) ->
    Tb = tablename(),
    imboy_db:add(Conn, Tb, Data).

% group_member_repo:find(6, 1, <<"*">>).
find(Gid, Uid, Column) ->
    Tb = tablename(),
    % use index uk_Gid_Uid
    Where = <<"WHERE group_id = ", (ec_cnv:to_binary(Gid))/binary, " AND user_id = ", (ec_cnv:to_binary(Uid))/binary>>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " ", Where/binary>>,
    % ?LOG([Sql]),
    imboy_db:find(Sql).

list_by_gid(Gid, Column) ->
    list_by_gid(Gid, Column, 10000).


list_by_gid(Gid, Column, Limit) ->
    Tb = tablename(),
    Where = <<" WHERE group_id = $1 AND status = 1 LIMIT $2">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_db:query(Sql, [Gid, Limit]).


list_by_uid(Uid, Column) ->
    list_by_uid(Uid, Column, 10000).


list_by_uid(Uid, Column, Limit) ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1 AND status = 1 LIMIT $2">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_db:query(Sql, [Uid, Limit]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
