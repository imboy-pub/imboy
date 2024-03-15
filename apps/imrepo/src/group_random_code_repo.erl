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
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

tablename() ->
    imboy_db:public_tablename(<<"group_random_code">>).

find_by_gid(Gid, Column) when is_integer(Gid) ->
    find_by_gid(ec_cnv:to_binary(Gid), Column);
find_by_gid(Gid, Column) ->
    Where = <<"group_id = ", Gid/binary>>,
    OrderBy = <<"id desc">>,
    Tb = tablename(),
    imboy_db:find(Tb, Where, OrderBy, Column).

add(Conn, Data) ->
    Tb = tablename(),
    imboy_db:add(Conn, Tb, Data).

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
