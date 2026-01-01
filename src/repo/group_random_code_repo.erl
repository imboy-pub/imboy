-module (group_random_code_repo).
%%%
% group_random_code 相关操作都放到该模块，存储库模块
% group_random_code related operations are put in this module, repository module
%%%

-export ([tablename/0]).
-export ([find_by_gid/2]).
-export ([add/2]).


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

tablename() ->
    imboy_pg_sql:public_tablename(<<"group_random_code">>).

find_by_gid(Gid, Column) when is_binary(Gid);is_list(Gid) ->
    find_by_gid(ec_cnv:to_integer(Gid), Column);
find_by_gid(Gid, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE group_id = $1 ORDER BY id DESC">>,
    case imboy_pg:one(Sql, [Gid]) of
        {ok, Row} -> Row;
        _ -> #{}
    end.

add(Conn, Data) ->
    Tb = tablename(),
    {Sql, Params} = imboy_pg_sql:insert(Tb, Data, <<>>),
    imboy_pg:execute(Conn, Sql, Params).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
