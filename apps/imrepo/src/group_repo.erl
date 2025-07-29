-module(group_repo).
%%%
% group_repo 是 group repository 缩写
%%%
-export([tablename/0]).
-export ([add/2]).
-export([find_by_id/2]).
-export([list_by_ids/2]).
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
    imboy_db:public_tablename(<<"group">>).

add(Conn, Data) ->
    Tb = tablename(),
    imboy_db:add(Conn, Tb, Data).


% group_repo:find_by_id(1, <<"*">>).
find_by_id(Gid, Column) ->
    Tb = tablename(),
    Where = <<"WHERE id =", (ec_cnv:to_binary(Gid))/binary>>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " ", Where/binary>>,
    % ?DEBUG_LOG([Sql]),
    % imboy_db:query(Sql).
    imboy_db:find(Sql).


% group_repo:list_by_ids([1,2], <<"*">>).
list_by_ids(Ids, Column) ->
    Tb = tablename(),
    L1 = lists:flatmap(fun(Id) -> [Id, ","] end, Ids),
    [_ | L2] = lists:reverse(L1),
    Ids2 = list_to_binary(lists:concat(L2)),
    Where = <<"WHERE id IN (", Ids2/binary, ")">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " ", Where/binary>>,
    % ?DEBUG_LOG([Sql]),
    imboy_db:proplists(Sql).


% group_repo:list_by_uid(1, <<"*">>).
list_by_uid(Uid, Column) ->
    list_by_uid(Uid, Column, 10000).


list_by_uid(Uid, Column, Limit) ->
    Tb = tablename(),
    Where = <<"WHERE owner_uid = $1 AND status = 1 LIMIT $2">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " ", Where/binary>>,
    imboy_db:query(Sql, [Uid, Limit]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
