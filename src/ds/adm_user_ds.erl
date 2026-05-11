-module(adm_user_ds).
%%%
% adm_user_ds 是管理员用户数据服务层
% 封装管理员用户的数据操作
%%%

-include("log.hrl").
-include("common.hrl").

%% ==================== API ====================

-export([find_by_id/1, find_by_id/2]).
-export([find_by_mobile/2, find_by_account/2]).
-export([list/2, count/0]).
-export([save/1, update/2, delete/1]).
-export([page_with_where_sql/4]).

%% ===================================================================
%% API Functions
%% ===================================================================

%% @doc 根据ID查找管理员用户（默认列）
-spec find_by_id(integer()) -> map().
find_by_id(Uid) ->
    adm_user_repo:find_by_id(Uid).

%% @doc 根据ID和指定列查找管理员用户
-spec find_by_id(integer(), binary()) -> map().
find_by_id(Uid, Column) ->
    adm_user_repo:find_by_id(Uid, Column).

%% @doc 根据邮箱查找管理员用户
-spec find_by_email(binary(), binary()) -> map().
find_by_email(Email, Column) ->
    adm_user_repo:find_by_email(Email, Column).

%% @doc 根据手机号查找管理员用户
-spec find_by_mobile(binary() | string(), binary()) -> map().
find_by_mobile(Mobile, Column) ->
    adm_user_repo:find_by_mobile(Mobile, Column).

%% @doc 根据账号查找管理员用户
-spec find_by_account(binary() | string(), binary()) -> map().
find_by_account(Account, Column) ->
    adm_user_repo:find_by_account(Account, Column).

%% @doc 批量查找管理员用户
-spec list_by_ids(list(integer() | binary()), binary()) -> {ok, [map()]} | {error, any()}.
list_by_ids(Uids, Column) ->
    adm_user_repo:list_by_ids(Uids, Column).

%% @doc 获取管理员用户列表（分页）
-spec list(pos_integer(), pos_integer()) -> {ok, [map()]} | {error, term()}.
list(Page, Size) ->
    Tb = adm_user_repo:tablename(),
    Column = <<"id,account,mobile,email,nickname,avatar,role_id,login_count,last_login_ip,last_login_at,status,created_at">>,
    OrderBy = <<"created_at DESC">>,
    elib_pg:page(Tb, Column, #{}, OrderBy, Page, Size).

%% @doc 统计管理员用户总数
-spec count() -> {ok, integer()}.
count() ->
    Tb = adm_user_repo:tablename(),
    Sql = <<"SELECT COUNT(*) AS count FROM ", Tb/binary, " WHERE status >= 0">>,
    case elib_pg:query(Sql, []) of
        {ok, [#{<<"count">> := Count}]} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 保存管理员用户
-spec save(map()) -> {ok, 1} | {error, any()}.
save(Data) ->
    adm_user_repo:save(Data).

%% @doc 更新管理员用户
-spec update(integer(), map()) -> {ok, 1} | {error, any()}.
update(Id, Data) ->
    adm_user_repo:update(Id, Data).

%% @doc 删除管理员用户（软删除）
-spec delete(integer()) -> {ok, 1} | {error, any()}.
delete(Id) ->
    adm_user_repo:delete(Id).

%% @doc 统计指定角色的用户数量
-spec count_by_role_id(integer()) -> {ok, integer()} | {error, any()}.
count_by_role_id(RoleId) ->
    adm_user_repo:count_by_role_id(RoleId).

%% @doc 根据WHERE条件查询管理员用户列表（使用默认列）
-spec select_by_where(binary() | map(), integer(), integer(), binary()) -> {ok, [map()]} | {error, any()}.
select_by_where(Where, Limit, Offset, OrderBy) ->
    adm_user_repo:select_by_where(Where, Limit, Offset, OrderBy).

%% @doc 根据WHERE条件查询管理员用户列表（指定列）
-spec select_by_where(binary(), binary() | map(), integer(), integer(), binary()) -> {ok, [map()]} | {error, any()}.
select_by_where(Column, Where, Limit, Offset, OrderBy) ->
    adm_user_repo:select_by_where(Column, Where, Limit, Offset, OrderBy).

%% @doc 根据WHERE条件查询（安全排序）
-spec select_by_where_safe(binary(), map(), integer(), integer(), [{atom() | binary(), asc | desc}], [binary()]) ->
    {ok, [map()]} | {error, any()}.
select_by_where_safe(Column, WhereMap, Limit, Offset, OrderSpec, ValidFields) ->
    adm_user_repo:select_by_where_safe(Column, WhereMap, Limit, Offset, OrderSpec, ValidFields).

%% @doc 带动态 WHERE SQL 的分页查询管理员列表
-spec page_with_where_sql(binary(), list(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page_with_where_sql(WhereSql, Params, Page, Size) ->
    Tb = adm_user_repo:tablename(),
    BaseWhere = <<" WHERE status >= -1">>,
    CountSql = iolist_to_binary([
        <<"SELECT COUNT(*) AS count FROM ">>, Tb, BaseWhere, WhereSql
    ]),
    case elib_pg:one(CountSql, Params) of
        {ok, CountRow} when is_map(CountRow) ->
            Total = case maps:get(<<"count">>, CountRow, 0) of
                V when is_integer(V) -> V;
                _ -> 0
            end,
            Offset = (Page - 1) * Size,
            LimitPos = integer_to_binary(length(Params) + 1),
            OffsetPos = integer_to_binary(length(Params) + 2),
            DataSql = iolist_to_binary([
                <<"SELECT id,account,mobile,email,nickname,avatar,role_id,">>,
                <<"login_count,last_login_ip,last_login_at,status,created_at ">>,
                <<"FROM ">>, Tb, BaseWhere, WhereSql,
                <<" ORDER BY created_at DESC, id DESC LIMIT $">>,
                LimitPos, <<" OFFSET $">>, OffsetPos
            ]),
            case elib_pg:query(DataSql, Params ++ [Size, Offset]) of
                {ok, Rows} ->
                    {ok, #{list => Rows, total => Total, page => Page, size => Size}};
                {error, _} = Err ->
                    Err
            end;
        {ok, _} ->
            {ok, #{list => [], total => 0, page => Page, size => Size}};
        {error, _} = Err ->
            Err
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================
