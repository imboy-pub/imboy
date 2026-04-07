-module(group_tag_repo).
%%%
% group_tag_repo 是 group_tag repository 缩写
% 群组标签数据仓库层，提供群组标签数据的基础数据库操作
%%%

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% 导出函数
-export([tablename/0]).
-export([add/2]).
-export([find_by_id/2]).
-export([list_by_group/2]).
-export([list_by_tag_name/2]).
-export([delete/2]).
-export([delete_by_group_id/1]).
-export([exists/2]).
-export([count/0]).
-export([count_by_group/1]).
-export([hot_tags/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取群标签表的表名
%% @return 返回群标签表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"group_tag">>).

%% @doc 添加群组标签
%% @param Conn 数据库连接（undefined 表示使用连接池）
%% @param Data 包含标签信息的map
%% @return {ok, TagId} | {error, Reason}
-spec add(any(), map()) -> {ok, integer()} | {error, term()}.
add(undefined, Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(group_tag),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end;
add(Conn, Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(group_tag),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Conn, Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 根据标签ID查找标签信息
%% @param Id 标签ID
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return Row 查询成功返回行数据（map） | {error, Reason} 查询失败
-spec find_by_id(integer() | binary(), binary()) -> map() | {error, any()}.
find_by_id(Id, Column) when is_list(Id); is_binary(Id) ->
    find_by_id(ec_cnv:to_integer(Id), Column);
find_by_id(Id, Column) when Id > 0 ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{id => Id}, #{limit => 1}),
    case elib_pg:one(Sql, Params) of
        {ok, Row} -> Row;
        {error, Reason} -> {error, Reason}
    end;
find_by_id(_, _Column) ->
    {error, invalid_id}.

%% @doc 根据群组ID查询标签列表
%% @param GroupId 群组ID
%% @param Column 要查询的列名
%% @return {ok, Rows} 查询成功返回 map 列表 | {error, Reason} 查询失败
-spec list_by_group(integer(), binary()) -> {ok, list(map())} | {error, any()}.
list_by_group(GroupId, Column) when GroupId > 0 ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{group_id => GroupId}, #{}),
    case elib_pg:query(Sql, Params) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end;
list_by_group(_GroupId, _Column) ->
    {ok, []}.

%% @doc 根据标签名查询使用该标签的群组列表
%% @param TagName 标签名称
%% @param Column 要查询的列名
%% @return {ok, Rows} 查询成功返回 map 列表 | {error, Reason} 查询失败
-spec list_by_tag_name(binary(), binary()) -> {ok, list(map())} | {error, any()}.
list_by_tag_name(TagName, Column) when is_binary(TagName), TagName =/= <<>> ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{tag_name => TagName}, #{}),
    case elib_pg:query(Sql, Params) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end;
list_by_tag_name(_TagName, _Column) ->
    {ok, []}.

%% @doc 删除指定群组的指定标签
%% @param GroupId 群组ID
%% @param TagName 标签名称
%% @return {ok, AffectedRows} | {error, Reason}
-spec delete(integer(), binary()) -> {ok, non_neg_integer()} | {error, any()}.
delete(GroupId, TagName) when GroupId > 0, is_binary(TagName), TagName =/= <<>> ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE group_id = $1 AND tag_name = $2">>,
    elib_pg:execute(Sql, [GroupId, TagName]);
delete(_, _) ->
    {ok, 0}.

%% @doc 删除指定群组的所有标签
%% @param GroupId 群组ID
%% @return {ok, AffectedRows} | {error, Reason}
-spec delete_by_group_id(integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete_by_group_id(GroupId) when GroupId > 0 ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE group_id = $1">>,
    elib_pg:execute(Sql, [GroupId]);
delete_by_group_id(_) ->
    {ok, 0}.

%% @doc 检查指定群组是否存在指定标签
%% @param GroupId 群组ID
%% @param TagName 标签名称
%% @return true | false
-spec exists(integer(), binary()) -> boolean().
exists(GroupId, TagName) when GroupId > 0, is_binary(TagName), TagName =/= <<>> ->
    Tb = tablename(),
    Count = elib_pg:pluck_value(Tb, <<"COUNT(*)">>, #{group_id => GroupId, tag_name => TagName}, 0),
    Count > 0;
exists(_, _) ->
    false.

%% @doc 统计标签总数
%% @return {ok, Count} | {error, Reason}
-spec count() -> {ok, non_neg_integer()} | {error, any()}.
count() ->
    Tb = tablename(),
    Sql = <<"SELECT COUNT(*) as count FROM ", Tb/binary>>,
    case elib_pg:one(Sql, []) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 统计指定群组的标签数量
%% @param GroupId 群组ID
%% @return {ok, Count} | {error, Reason}
-spec count_by_group(integer()) -> {ok, non_neg_integer()} | {error, any()}.
count_by_group(GroupId) when GroupId > 0 ->
    Tb = tablename(),
    Count = elib_pg:pluck_value(Tb, <<"COUNT(*)">>, #{group_id => GroupId}, 0),
    {ok, Count};
count_by_group(_) ->
    {ok, 0}.

%% @doc 获取热门标签（按使用频率排序）
%% @param Limit 返回数量限制
%% @return {ok, [{tag_name, count}]} | {error, Reason}
-spec hot_tags(integer()) -> {ok, list(map())} | {error, any()}.
hot_tags(Limit) when Limit > 0 ->
    Tb = tablename(),
    Sql = <<"SELECT tag_name, COUNT(*) as count FROM ", Tb/binary,
            " GROUP BY tag_name ORDER BY count DESC LIMIT $1">>,
    case elib_pg:query(Sql, [Limit]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end;
hot_tags(_) ->
    {ok, []}.
