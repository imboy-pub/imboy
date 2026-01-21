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
-export([page/2, page/4]).
-export([update/1]).
-export([delete/1]).
-export([count/0]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取群组表的表名
%% @return 返回群组表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"group">>).

%% @doc 添加新群组
%% @doc 添加群组（使用连接）
%% @param Conn 数据库连接（未使用，保留用于API兼容性）
%% @param Data 包含群组信息的map
%% @return {ok, GroupId, #{}} | {error, Reason} (返回插入的群组ID)
-spec add(any(), map()) -> {ok, integer(), map()} | {error, term()}.
add(Conn, Data) ->
    Tb = tablename(),
    elib_pg_sql:parse_result(elib_pg:insert(Conn, Tb, Data, <<"RETURNING id">>)).

%% @doc 根据群组ID查找群组信息
%% @param Gid 群组ID
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return Row 查询成功返回行数据（map） | {error, Reason} 查询失败
%% @example group_repo:find_by_id(1, <<"*">>).
-spec find_by_id(integer() | binary(), binary()) -> map() | {error, any()}.
find_by_id(Gid, Column) when is_list(Gid); is_binary(Gid) ->
    find_by_id(ec_cnv:to_integer(Gid), Column);
find_by_id(Gid, Column) ->
    Tb = tablename(),
    % 使用安全的参数化查询，避免SQL注入
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{id => Gid}, #{limit => 1}),
    case elib_pg:one(Sql, Params) of
        {ok, Row} -> Row;
        {error, Reason} -> {error, Reason}
    end.


%% @doc 根据群组ID列表批量查询群组信息
%% @param Ids 群组ID列表
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return {ok, Rows} 查询成功返回 map 列表 | {error, Reason} 查询失败
%% @example group_repo:list_by_ids([1,2], <<"*">>).
-spec list_by_ids(list(integer() | binary()), binary()) -> {ok, list(map())} | {error, any()}.
list_by_ids(Ids, Column) when length(Ids) > 0 ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{id => {in, Ids}}, #{}),
    case elib_pg:query(Sql, Params) of
        {ok, Rows} ->
            {ok, Rows};
        {error, Reason} ->
            {error, Reason}
    end;
list_by_ids([], _Column) ->
    {ok, []}.

%% @doc 查询用户创建的群组列表（使用默认限制10000）
%% @param Uid 用户ID（群组所有者）
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
%% @example group_repo:list_by_uid(1, <<"*">>).
-spec list_by_uid(integer(), binary()) -> {ok, list(map())} | {error, any()}.
list_by_uid(Uid, Column) ->
    list_by_uid(Uid, Column, 10000).

%% @doc 查询用户创建的群组列表（指定限制数量）
%% @param Uid 用户ID（群组所有者）
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @param Limit 查询结果数量限制
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
%% @example group_repo:list_by_uid(1, <<"*">>).
-spec list_by_uid(integer(), binary(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_uid(Uid, Column, Limit) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{owner_uid => Uid, status => 1}, #{limit => Limit}),
    elib_pg:query(Sql, Params).

%% @doc 分页查询群组
-spec page(integer(), integer()) -> {ok, map()} | {error, any()}.
page(Page, Size) ->
    page(Page, Size, #{}, <<"created_at DESC">>).

%% @doc 分页查询群组（带条件）
-spec page(integer(), integer(), map(), binary()) -> {ok, map()} | {error, any()}.
page(Page, Size, Where, OrderBy) ->
    Tb = tablename(),
    Column = <<"id,title,avatar,owner_uid,creator_uid,type,join_limit,member_count,introduction,status,created_at">>,
    elib_pg:page_with_total(Tb, Column, Where, OrderBy, Page, Size).

%% @doc 更新群组信息
-spec update(map()) -> {ok, non_neg_integer()} | {error, any()}.
update(Data) ->
    Tb = tablename(),
    Id = maps:get(<<"id">>, Data),
    UpdateData = maps:without([<<"id">>], Data),
    elib_pg:update(Tb, UpdateData, <<"id = $1">>, [Id]).

%% @doc 删除群组（软删除，设置 status 为 0）
-spec delete(integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete(Id) ->
    Tb = tablename(),
    elib_pg:update(Tb, #{status => 0}, <<"id = $1">>, [Id]).

%% @doc 统计群组总数
-spec count() -> {ok, non_neg_integer()} | {error, any()}.
count() ->
    Tb = tablename(),
    Sql = <<"SELECT COUNT(*) as count FROM ", Tb/binary, " WHERE status = 1">>,
    case elib_pg:one(Sql, []) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
