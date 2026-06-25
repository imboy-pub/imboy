-module(group_member_repo).
%%%
% group_member_repo 是 group_member repository 缩写
% 群组成员数据仓库层，提供群组成员信息的基础数据库操作
%%%
-export([tablename/0]).
-export([add/1]).
-export([add/2]).
-export([find/3]).
-export([list_same_group/2]).
% -export ([list_same_group/2]).
-export([list_by_gid/2, list_by_gid/3]).
-export([page_by_gid/4]).
-export([count_by_uid/1]).
-export([list_by_uid/2, list_by_uid/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取群组成员表的表名
%% @return 返回群组成员表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"group_member">>).

%% @doc 添加群组成员
%% @param Data 包含群组成员信息的map
%% @return {ok, integer()} | {error, Reason}
-spec add(map()) -> {ok, integer()} | {error, term()}.
add(Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(group_member),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 添加群组成员（使用连接）
%% @param Conn 数据库连接
%% @param Data 包含群组成员信息的map
%% @return {ok, integer()} | {error, Reason}
-spec add(any(), map()) -> {ok, integer()} | {error, term()}.
add(Conn, Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(group_member),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Conn, Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 查找群组成员
%% @param Gid 群组ID
%% @param Uid 用户ID
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return Map 查询成功返回成员信息map，未找到返回空map
%% @example group_member_repo:find(6, 1, <<"*">>).
-spec find(integer(), integer(), binary()) -> map().
find(Gid, Uid, Column) ->
    Tb = tablename(),
    % use index uk_Gid_Uid
    % 使用安全的参数化查询，避免SQL注入
    Sql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE group_id = $1 AND user_id = $2">>,
    % ?DEBUG_LOG([Sql]),
    case elib_pg:one(Sql, [Gid, Uid]) of
        {ok, Row} -> Row;
        _ -> #{}
    end.

%% @doc 查询群组成员列表（使用默认限制10000）
%% @param Gid 群组ID
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
-spec list_by_gid(integer(), binary()) -> {ok, list(map())} | {error, any()}.
list_by_gid(Gid, Column) ->
    list_by_gid(Gid, Column, 10000).

%% @doc 查询群组成员列表（指定限制数量）
%% @param Gid 群组ID
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @param Limit 查询结果数量限制
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
-spec list_by_gid(integer(), binary(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_gid(Gid, Column, Limit) ->
    Tb = tablename(),
    Where = <<" WHERE group_id = $1 AND status = 1 LIMIT $2">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    elib_pg:query(Sql, [Gid, Limit]).

%% @doc 分页查询群组成员列表
%% @param Gid 群组ID
%% @param Page 页码（从1开始）
%% @param Size 每页大小
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @return {ok, #{list => Rows, page => Page, size => Size, total => Total, total_page => TotalPage}}
-spec page_by_gid(integer(), integer(), integer(), binary()) -> {ok, map()} | {error, any()}.
page_by_gid(Gid, Page, Size, Column) ->
    Tb = tablename(),
    % 计算偏移量
    Offset = (Page - 1) * Size,
    % 查询总数
    CountSql =
        <<"SELECT COUNT(*) as count FROM ", Tb/binary, " WHERE group_id = $1 AND status = 1">>,
    Total =
        case elib_pg:one(CountSql, [Gid]) of
            {ok, #{<<"count">> := C}} -> C;
            _ -> 0
        end,
    % 查询数据
    DataSql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " WHERE group_id = $1 AND status = 1 ORDER BY role DESC, joined_at ASC LIMIT $2 OFFSET $3">>,
    case elib_pg:query(DataSql, [Gid, Size, Offset]) of
        {ok, Items} ->
            TotalPage =
                case Total > 0 of
                    true -> ((Total - 1) div Size) + 1;
                    false -> 0
                end,
            {ok, #{
                list => Items,
                page => Page,
                size => Size,
                total => Total,
                total_page => TotalPage
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 统计用户加入的群组数量
%% @param Uid 用户ID
%% @return non_neg_integer() 群组数量
-spec count_by_uid(integer()) -> non_neg_integer().
count_by_uid(Uid) ->
    Tb = tablename(),
    Sql = <<"SELECT COUNT(*) as count FROM ", Tb/binary, " WHERE user_id = $1 AND status = 1">>,
    case elib_pg:one(Sql, [Uid]) of
        {ok, #{<<"count">> := Count}} -> Count;
        _ -> 0
    end.

%% @doc 查询用户加入的群组列表（使用默认限制10000）
%% @param Uid 用户ID
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
-spec list_by_uid(integer(), binary()) -> {ok, list(map())} | {error, any()}.
list_by_uid(Uid, Column) ->
    list_by_uid(Uid, Column, 10000).

%% @doc 查询用户加入的群组列表（指定限制数量）
%% @param Uid 用户ID
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @param Limit 查询结果数量限制
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
-spec list_by_uid(integer(), binary(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_uid(Uid, Column, Limit) ->
    Tb = tablename(),
    % use index i_Uid_Status
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{user_id => Uid, status => 1}, #{
        limit => Limit
    }),
    elib_pg:query(Sql, Params).

%% @doc 查询两个用户共同加入的群组ID列表
%% @param Uid1 用户1的ID
%% @param Uid2 用户2的ID
%% @return GroupIds 群组ID列表，任一用户ID为0或无共同群组时返回空列表
%% @example group_member_repo:list_same_group(108, 25).
-spec list_same_group(integer(), integer()) -> [integer()].
list_same_group(0, _) ->
    [];
list_same_group(_, 0) ->
    [];
list_same_group(Uid1, Uid2) ->
    % use index i_Uid_Status
    % T1 = elib_dt:microsecond(),
    Sql = <<
        "SELECT group_id\n"
        "        FROM (\n"
        "            SELECT group_id\n"
        "            FROM public.group_member\n"
        "            WHERE user_id = $1 AND status = 1\n"
        "        ) AS subquery\n"
        "        WHERE EXISTS (\n"
        "            SELECT 1\n"
        "              FROM public.group_member gm2\n"
        "             WHERE gm2.group_id = subquery.group_id\n"
        "               AND gm2.user_id = $2 AND gm2.status = 1\n"
        "        );"
    >>,
    case elib_pg:query(Sql, [Uid1, Uid2]) of
        {ok, []} ->
            [];
        {ok, Rows} ->
            [Gid || #{<<"group_id">> := Gid} <- Rows]
    end.
% T2 = elib_dt:microsecond(),
    % {T2-T1, Res}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
