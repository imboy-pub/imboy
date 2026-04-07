-module(group_category_repo).
%%%
% group_category_repo 是 group_category repository 缩写
% 群组分类数据仓库层，提供群组分类的基础数据库操作
%%%

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% 表名操作
-export([tablename/0]).

%% 查询操作
-export([list_by_uid/2]).
-export([find_by_name/2]).

%% 增删改操作
-export([add/2]).
-export([update_name/3]).
-export([update_sort_order/3]).
-export([delete/2]).

%% 群组成员分类操作
-export([update_group_category/3]).
-export([list_groups_by_category/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% API Functions
%% ===================================================================

%% @doc 获取群组分类表的表名
%% @return 返回群组分类表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"user_group_category">>).

%% @doc 查询指定用户的群组分类列表
%% @param Uid 用户ID
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
-spec list_by_uid(integer(), binary()) -> {ok, list(map())} | {error, term()}.
list_by_uid(Uid, Column) ->
    list_by_uid(Uid, Column, 10000).

%% @doc 查询指定用户的群组分类列表（指定限制数量）
%% @param Uid 用户ID
%% @param Column 要查询的列名
%% @param Limit 查询结果数量限制
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
-spec list_by_uid(integer(), binary(), integer()) -> {ok, list(map())} | {error, term()}.
list_by_uid(Uid, Column, Limit) ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1 ORDER BY sort_order ASC, id ASC LIMIT $2">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    elib_pg:query(Sql, [Uid, Limit]).

%% @doc 根据用户ID和分类名称查找群组分类
%% @param Uid 用户ID
%% @param Name 分类名称
%% @return {ok, map()} 查询成功返回分类信息 | {ok, #{}} 未找到 | {error, Reason} 查询失败
-spec find_by_name(integer(), binary()) -> {ok, map()} | {ok, #{}} | {error, term()}.
find_by_name(Uid, Name) ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1 AND category_name = $2">>,
    Sql = <<"SELECT id, user_id, category_name, sort_order FROM ", Tb/binary, Where/binary, " LIMIT 1">>,
    case elib_pg:one(Sql, [Uid, Name], #{}) of
        {ok, Row} when map_size(Row) > 0 ->
            {ok, Row};
        {ok, EmptyMap} ->
            {ok, EmptyMap};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 添加新的群组分类
%% @param Uid 用户ID
%% @param Name 分类名称
%% @return {ok, CategoryId} 添加成功返回分类ID | {error, Reason} 添加失败
-spec add(integer(), binary()) -> {ok, integer()} | {error, term()}.
add(Uid, Name) ->
    Tb = tablename(),
    Id = elib_tsid:generate(group_category),
    Sql = <<"INSERT INTO ", Tb/binary,
            " (id, user_id, category_name, sort_order) "
            "VALUES ($1, $2, $3, 0)">>,
    case elib_pg:execute(Sql, [Id, Uid, Name]) of
        {ok, _Count} ->
            {ok, Id};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 更新群组分类名称
%% @param Uid 用户ID
%% @param CategoryId 分类ID
%% @param NewName 新的分类名称
%% @return {ok, Count} 更新成功返回影响行数 | {error, Reason} 更新失败
-spec update_name(integer(), integer(), binary()) -> {ok, integer()} | {error, term()}.
update_name(Uid, CategoryId, NewName) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET category_name = $1 "
            "WHERE id = $2 AND user_id = $3">>,
    elib_pg:execute(Sql, [NewName, CategoryId, Uid]).

%% @doc 更新群组分类排序
%% @param Uid 用户ID
%% @param CategoryId 分类ID
%% @param SortOrder 排序顺序
%% @return {ok, Count} 更新成功返回影响行数 | {error, Reason} 更新失败
-spec update_sort_order(integer(), integer(), integer()) -> {ok, integer()} | {error, term()}.
update_sort_order(Uid, CategoryId, SortOrder) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET sort_order = $1 "
            "WHERE id = $2 AND user_id = $3">>,
    elib_pg:execute(Sql, [SortOrder, CategoryId, Uid]).

%% @doc 删除群组分类
%% @param Uid 用户ID
%% @param CategoryId 分类ID
%% @return {ok, Count} 删除成功返回影响行数 | {error, Reason} 删除失败
-spec delete(integer(), integer()) -> {ok, integer()} | {error, term()}.
delete(Uid, CategoryId) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary,
            " WHERE id = $1 AND user_id = $2">>,
    elib_pg:execute(Sql, [CategoryId, Uid]).

%% @doc 更新群组成员的分类ID
%% @param Uid 用户ID
%% @param Gid 群组ID
%% @param CategoryId 分类ID（0表示未分类）
%% @return {ok, Count} 更新成功返回影响行数 | {error, Reason} 更新失败
-spec update_group_category(integer(), integer(), integer()) -> {ok, integer()} | {error, term()}.
update_group_category(Uid, Gid, CategoryId) ->
    Tb = group_member_repo:tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET category_id = $1, updated_at = $2 "
            "WHERE group_id = $3 AND user_id = $4">>,
    elib_pg:execute(Sql, [CategoryId, elib_dt:now(), Gid, Uid]).

%% @doc 查询指定分类下的群组列表
%% @param Uid 用户ID
%% @param CategoryId 分类ID（0表示未分类）
%% @param Column 要查询的列名
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
-spec list_groups_by_category(integer(), integer(), binary()) -> {ok, list(map())} | {error, term()}.
list_groups_by_category(Uid, CategoryId, Column) ->
    Tb = group_member_repo:tablename(),
    GTb = group_repo:tablename(),
    Where = <<" WHERE gm.user_id = $1 AND gm.category_id = $2 AND gm.status = 1">>,
    Sql = <<"SELECT ", Column/binary,
            " FROM ", Tb/binary, " gm "
            "LEFT JOIN ", GTb/binary, " g ON gm.group_id = g.id ",
            Where/binary,
            " ORDER BY gm.updated_at DESC">>,
    elib_pg:query(Sql, [Uid, CategoryId]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
