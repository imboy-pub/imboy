-module(group_repo).
%%%
% group_repo 是 group repository 缩写
%%%
-export([tablename/0]).
-export([find_by_ids/2]).
-export([find_by_uid/2, find_by_uid/3]).

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


% group_repo:find_by_ids([1,2], <<"*">>).
find_by_ids(Ids, Column) ->
    Tb = tablename(),
    L1 = lists:flatmap(fun(Id) -> [Id, ","] end, Ids),
    [_ | L2] = lists:reverse(L1),
    Ids2 = list_to_binary(lists:concat(L2)),
    Where = <<"WHERE id IN (", Ids2/binary, ")">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " ", Where/binary>>,
    % ?LOG([Sql]),
    imboy_db:query(Sql).


% group_repo:find_by_uid(1, <<"*">>).
find_by_uid(Uid, Column) ->
    find_by_uid(Uid, Column, 10000).


find_by_uid(Uid, Column, Limit) ->
    Tb = tablename(),
    Where = <<"WHERE owner_uid = $1 AND status = 1 LIMIT $2">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " ", Where/binary>>,
    imboy_db:query(Sql, [Uid, Limit]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
