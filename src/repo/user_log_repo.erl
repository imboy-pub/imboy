-module (user_log_repo).
%%%
% user_log 相关操作都放到该模块，存储库模块
% user_log related operations are put in this module, repository module
% 用户日志数据仓库层，提供用户日志信息的基础数据库操作
%%%

-export ([tablename/0]).
-export ([add/1]).
-export ([add/2]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取用户日志表的表名
%% @return 返回用户日志表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"user_log">>).

%% @doc 添加用户日志（无连接版本，使用事务）
%% @param Data 日志数据map
%% @return {ok, Result} | {error, Reason}
-spec add(map()) -> {ok, term()} | {error, term()}.
add(Data) ->
    elib_pg:with_tx(fun(Conn) -> add(Conn, Data) end).

%% @doc 添加用户日志
%% @param Conn 数据库连接
%% @param Data 日志数据map
%% @return {ok, Result} | {ok, Count, Result} | {error, Reason}
-spec add(epgsql:connection() | pid(), map()) -> {ok, term()} | {ok, term(), term()} | {error, term()}.
add(Conn, Data) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:insert(Tb, Data, <<>>),
    elib_pg:execute(Conn, Sql, Params).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

