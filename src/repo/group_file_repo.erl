-module(group_file_repo).
%%%
% group_file_repo 是 group file repository 缩写
% 群文件数据仓库层，提供群文件数据的基础数据库操作
%%%

-export([tablename/0]).
-export([insert/1]).
-export([find_by_id/1]).
-export([find_by_file_id/1]).
-export([list_by_group/4]).
-export([search_by_name/4]).
-export([list_by_category/4]).
-export([soft_delete/1]).
-export([increment_download/1]).
-export([count_by_group/1]).
-export([sum_size_by_group/1]).
-export([category_stats/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取群文件表的表名
%% @return 返回群文件表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"group_file">>).

%% @doc 插入群文件记录
%% @param Data 包含文件信息的map
%% @return {ok, FileId, #{}} | {error, Reason}
-spec insert(map()) -> {ok, integer(), map()} | {error, term()}.
insert(Data) ->
    Tb = tablename(),
    elib_pg_sql:parse_result(elib_pg:insert(undefined, Tb, Data, <<"RETURNING id">>)).

%% @doc 根据文件ID（主键）查找文件
%% @param FileId 文件主键ID
%% @return FileMap 文件信息map | 空map（未找到）
-spec find_by_id(integer() | binary()) -> map().
find_by_id(FileId) when is_list(FileId); is_binary(FileId) ->
    find_by_id(ec_cnv:to_integer(FileId));
find_by_id(FileId) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, <<"*">>, #{id => FileId}, #{limit => 1}),
    case elib_pg:one(Sql, Params) of
        {ok, Row} -> Row;
        {error, _Reason} -> #{}
    end.

%% @doc 根据文件ID（业务ID）查找文件
%% @param FileId 文件业务ID（如 "file_123"）
%% @return FileMap 文件信息map | 空map（未找到）
-spec find_by_file_id(binary()) -> map().
find_by_file_id(FileId) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, <<"*">>, #{file_id => FileId}, #{limit => 1}),
    case elib_pg:one(Sql, Params) of
        {ok, Row} -> Row;
        {error, _Reason} -> #{}
    end.

%% @doc 查询群文件列表（分页）
%% @param Gid 群组ID
%% @param Page 页码（从1开始）
%% @param Size 每页数量
%% @param Options 选项 #{category => binary()}
%% @return {ok, [FileMap]} | {error, Reason}
-spec list_by_group(integer(), integer(), integer(), map()) -> {ok, list(map())} | {error, term()}.
list_by_group(Gid, Page, Size, Options) ->
    Tb = tablename(),
    Offset = (Page - 1) * Size,
    Where = case maps:get(category, Options, undefined) of
        undefined ->
            #{group_id => Gid, status => 1};
        Category ->
            #{group_id => Gid, status => 1, file_category => Category}
    end,
    {Sql, Params} = elib_pg_sql:build_select(
        Tb,
        <<"*">>,
        Where,
        #{order_by => <<"created_at DESC">>, limit => Size, offset => Offset}
    ),
    elib_pg:query(Sql, Params).

%% @doc 按文件名搜索群文件
%% @param Gid 群组ID
%% @param Keyword 关键词
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, [FileMap]} | {error, Reason}
-spec search_by_name(integer(), binary(), integer(), integer()) -> {ok, list(map())} | {error, term()}.
search_by_name(Gid, Keyword, Page, Size) ->
    Tb = tablename(),
    Offset = (Page - 1) * Size,
    Sql = <<"SELECT * FROM ", Tb/binary,
            " WHERE group_id = $1 ",
            " AND status = 1 ",
            " AND file_name LIKE $2 ",
            " ORDER BY created_at DESC ",
            " LIMIT $3 OFFSET $4">>,
    Params = [Gid, <<"%", Keyword/binary, "%">>, Size, Offset],
    elib_pg:query(Sql, Params).

%% @doc 按分类查询群文件
%% @param Gid 群组ID
%% @param Category 文件分类
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, [FileMap]} | {error, Reason}
-spec list_by_category(integer(), binary(), integer(), integer()) -> {ok, list(map())} | {error, term()}.
list_by_category(Gid, Category, Page, Size) ->
    Tb = tablename(),
    Offset = (Page - 1) * Size,
    {Sql, Params} = elib_pg_sql:build_select(
        Tb,
        <<"*">>,
        #{group_id => Gid, status => 1, file_category => Category},
        #{order_by => <<"created_at DESC">>, limit => Size, offset => Offset}
    ),
    elib_pg:query(Sql, Params).

%% @doc 软删除文件
%% @param FileId 文件主键ID
%% @return {ok, AffectedRows} | {error, Reason}
-spec soft_delete(integer()) -> {ok, integer()} | {error, term()}.
soft_delete(FileId) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET status = 0 WHERE id = $1">>,
    elib_pg:execute(Sql, [FileId]).

%% @doc 增加文件下载计数
%% @param FileId 文件主键ID
%% @return {ok, AffectedRows} | {error, Reason}
-spec increment_download(integer()) -> {ok, integer()} | {error, term()}.
increment_download(FileId) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET download_count = download_count + 1 ",
            " WHERE id = $1 AND status = 1">>,
    elib_pg:execute(Sql, [FileId]).

%% @doc 统计群文件数量
%% @param Gid 群组ID
%% @return {ok, Count} | {error, Reason}
-spec count_by_group(integer()) -> {ok, integer()} | {error, term()}.
count_by_group(Gid) ->
    Tb = tablename(),
    Sql = <<"SELECT COUNT(*) as count FROM ", Tb/binary,
            " WHERE group_id = $1 AND status = 1">>,
    case elib_pg:one(Sql, [Gid]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 统计群文件总大小
%% @param Gid 群组ID
%% @return {ok, TotalSize} | {error, Reason}
-spec sum_size_by_group(integer()) -> {ok, integer()} | {error, term()}.
sum_size_by_group(Gid) ->
    Tb = tablename(),
    Sql = <<"SELECT COALESCE(SUM(file_size), 0) as total_size FROM ", Tb/binary,
            " WHERE group_id = $1 AND status = 1">>,
    case elib_pg:one(Sql, [Gid]) of
        {ok, #{<<"total_size">> := TotalSize}} -> {ok, TotalSize};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 获取群文件分类统计
%% @param Gid 群组ID
%% @return {ok, [{Category, Count, TotalSize}]} | {error, Reason}
-spec category_stats(integer()) -> {ok, list({binary(), integer(), integer()})} | {error, term()}.
category_stats(Gid) ->
    Tb = tablename(),
    Sql = <<"SELECT file_category, ",
            " COUNT(*) as count, ",
            " SUM(file_size) as total_size ",
            " FROM ", Tb/binary,
            " WHERE group_id = $1 AND status = 1 ",
            " GROUP BY file_category ",
            " ORDER BY count DESC">>,
    case elib_pg:query(Sql, [Gid]) of
        {ok, Rows} ->
            Stats = [{maps:get(<<"file_category">>, R, <<>>),
                      maps:get(<<"count">>, R, 0),
                      maps:get(<<"total_size">>, R, 0)} || R <- Rows],
            {ok, Stats};
        {error, Reason} ->
            {error, Reason}
    end.
