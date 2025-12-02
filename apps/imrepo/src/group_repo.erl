-module(group_repo).
%%%
% group_repo 是 group repository 缩写
% 群组数据仓库层，提供群组数据的基础数据库操作
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
%% API functions
%% ===================================================================

%% @doc 获取群组表的表名
%% @return 返回群组表的完整表名
-spec tablename() -> binary().
tablename() ->
    imboy_db:public_tablename(<<"group">>).

%% @doc 添加新群组
%% @param Conn 数据库连接
%% @param Data 包含群组信息的map
%% @return {ok, Result} | {error, Reason}
-spec add(any(), map()) -> {ok, any()} | {error, any()}.
add(Conn, Data) ->
    Tb = tablename(),
    imboy_db:add(Conn, Tb, Data).

%% @doc 根据群组ID查找群组信息
%% @param Gid 群组ID
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return {ok, Columns, Rows} 查询成功返回列和行数据 | {error, Reason} 查询失败
%% @example group_repo:find_by_id(1, <<"*">>).
-spec find_by_id(integer() | binary(), binary()) -> {ok, list(), list()} | {error, any()}.
find_by_id(Gid, Column) ->
    Tb = tablename(),
    Where = <<"WHERE id =", (ec_cnv:to_binary(Gid))/binary>>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " ", Where/binary>>,
    % ?DEBUG_LOG([Sql]),
    % imboy_db:query(Sql).
    imboy_db:find(Sql).


%% @doc 根据群组ID列表批量查询群组信息
%% @param Ids 群组ID列表
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return {ok, List} 查询成功返回proplist列表 | {error, Reason} 查询失败
%% @example group_repo:list_by_ids([1,2], <<"*">>).
-spec list_by_ids(list(integer() | binary()), binary()) -> {ok, list()} | {error, any()}.
list_by_ids(Ids, Column) ->
    Tb = tablename(),
    L1 = lists:flatmap(fun(Id) -> [integer_to_binary(Id), ","] end, Ids),
    [_ | L2] = lists:reverse(L1),
    Ids2 = list_to_binary(lists:concat(L2)),
    Where = <<"WHERE id IN (", Ids2/binary, ")">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " ", Where/binary>>,
    % ?DEBUG_LOG([Sql]),
    imboy_db:proplists(Sql).

%% @doc 查询用户创建的群组列表（使用默认限制10000）
%% @param Uid 用户ID（群组所有者）
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @return {ok, Columns, Rows} 查询成功返回列和行数据 | {error, Reason} 查询失败
%% @example group_repo:list_by_uid(1, <<"*">>).
-spec list_by_uid(integer(), binary()) -> {ok, list(), list()} | {error, any()}.
list_by_uid(Uid, Column) ->
    list_by_uid(Uid, Column, 10000).

%% @doc 查询用户创建的群组列表（指定限制数量）
%% @param Uid 用户ID（群组所有者）
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @param Limit 查询结果数量限制
%% @return {ok, Columns, Rows} 查询成功返回列和行数据 | {error, Reason} 查询失败
-spec list_by_uid(integer(), binary(), integer()) -> {ok, list(), list()} | {error, any()}.
list_by_uid(Uid, Column, Limit) ->
    Tb = tablename(),
    Where = <<"WHERE owner_uid = $1 AND status = 1 LIMIT $2">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " ", Where/binary>>,
    imboy_db:query(Sql, [Uid, Limit]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
