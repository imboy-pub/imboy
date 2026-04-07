-module (app_ddl_repo).
%%%
% app_ddl 相关操作都放到该模块，存储库模块
% app_ddl related operations are put in this module, repository module
% 应用DDL数据仓库层，提供应用DDL信息的基础数据库操作
%%%

-export ([tablename/0]).
-export ([add/1]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取应用DDL表的表名
%% @return 返回应用DDL表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"app_ddl">>).

%% @doc 添加应用DDL记录
%% @param Data 包含DDL信息的map
%% @return {ok, Result} | {error, Reason}
-spec add(map()) -> {ok, integer()} | {error, any()}.
add(Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(app_ddl),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Sql, Params) of
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

