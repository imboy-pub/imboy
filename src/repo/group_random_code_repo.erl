-module (group_random_code_repo).
%%%
% group_random_code 相关操作都放到该模块，存储库模块
% group_random_code related operations are put in this module, repository module
% 群随机码数据仓库层，提供群随机码信息的基础数据库操作
%%%

-export ([tablename/0]).
-export ([find_by_gid/2]).
-export ([add/2]).


-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取群随机码表的表名
%% @return 返回群随机码表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"group_random_code">>).

%% @doc 根据群组ID查找群随机码
%% @param Gid 群组ID（支持binary、list或integer类型）
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @return Map 查询成功返回群随机码信息map，未找到返回空map
-spec find_by_gid(binary() | list() | integer(), binary()) -> map().
find_by_gid(Gid, Column) when is_binary(Gid);is_list(Gid) ->
    find_by_gid(ec_cnv:to_integer(Gid), Column);
find_by_gid(Gid, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE group_id = $1 ORDER BY id DESC">>,
    case elib_pg:one(Sql, [Gid]) of
        {ok, Row} -> Row;
        _ -> #{}
    end.

%% @doc 添加群随机码
%% @param Conn 数据库连接
%% @param Data 包含群随机码信息的map
%% @return {ok, Result} | {ok, Count, Result} | {error, Reason}
-spec add(epgsql:connection() | pid(), map()) -> {ok, integer()} | {error, term()}.
add(Conn, Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(group_random_code),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

