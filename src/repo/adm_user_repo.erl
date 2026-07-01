-module(adm_user_repo).
%%%
% adm_user_repo 是 admin user repository 缩写
% 管理员用户数据仓库层，提供管理员用户数据的基础数据库操作
%%%

-include("log.hrl").
-include("common.hrl").

-export([tablename/0]).
-export([save/1, update/2, delete/1]).
-export([count/0]).
-export([count_by_role_id/1]).

-export([
    find_by_email/2,
    find_by_mobile/2,
    find_by_account/2
]).
-export([find_by_id/1, find_by_id/2]).
-export([list_by_ids/2]).
-export([select_by_where/4]).
-export([select_by_where/5]).
-export([select_by_where_safe/6]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取管理员用户表的表名
%% @return 返回管理员用户表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"adm_user">>).

%% @doc 统计指定角色的用户数量
%% @param RoleId 角色ID
%% @return {ok, Count} 用户数量 | {error, Reason} 查询失败
-spec count() -> {ok, integer()} | {error, any()}.
count() ->
    Tb = tablename(),
    Sql = <<"SELECT COUNT(*) AS count FROM ", Tb/binary, " WHERE status >= 0">>,
    case elib_pg:query(Sql, []) of
        {ok, [#{<<"count">> := Count}]} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% @example adm_user_repo:count_by_role_id(1).
-spec count_by_role_id(integer()) -> {ok, integer()} | {error, any()}.
count_by_role_id(RoleId) ->
    Tb = tablename(),
    Sql = <<"SELECT COUNT(*) AS count FROM ", Tb/binary, " WHERE role_id = $1 AND status >= 0">>,
    case elib_pg:query(Sql, [RoleId]) of
        {ok, [#{<<"count">> := Count}]} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 根据WHERE条件查询管理员用户列表（使用默认列）
%% @param Where SQL WHERE子句条件
%% @param Limit 查询结果数量限制
%% @param Offset 查询结果偏移量
%% @param OrderBy 排序字段
%% @return {ok, [map()]} 查询成功返回map列表 | {error, Reason} 查询失败
-spec select_by_where(binary() | map(), integer(), integer(), binary()) ->
    {ok, [map()]} | {error, any()}.
select_by_where(Where, Limit, Offset, OrderBy) when is_binary(Where) ->
    select_by_where(?DEF_USER_COLUMN, Where, Limit, Offset, OrderBy);
select_by_where(WhereMap, Limit, Offset, OrderBy) when is_map(WhereMap) ->
    select_by_where(?DEF_USER_COLUMN, WhereMap, Limit, Offset, OrderBy).

%% @doc 根据WHERE条件查询管理员用户列表（指定列）
%% @param Column 要查询的列名
%% @param Where SQL WHERE子句条件
%% @param Limit 查询结果数量限制
%% @param Offset 查询结果偏移量
%% @param OrderBy 排序字段
%% @return {ok, [map()]} 查询成功返回map列表 | {error, Reason} 查询失败
-spec select_by_where(binary(), binary() | map(), integer(), integer(), binary()) ->
    {ok, [map()]} | {error, any()}.
select_by_where(Column, Where, Limit, Offset, OrderBy) when is_binary(Where) ->
    case Limit > 0 of
        false ->
            {ok, []};
        true ->
            Tb = tablename(),
            Page = (Offset div Limit) + 1,
            elib_pg:page(Tb, Column, #{<<"__raw">> => Where}, OrderBy, Page, Limit)
    end;
select_by_where(Column, WhereMap, Limit, Offset, OrderBy) when is_map(WhereMap) ->
    case Limit > 0 of
        false ->
            {ok, []};
        true ->
            Tb = tablename(),
            Page = (Offset div Limit) + 1,
            elib_pg:page(Tb, Column, WhereMap, OrderBy, Page, Limit)
    end.

%% @doc 根据WHERE条件查询（安全排序）
%% @param Column 列集合
%% @param WhereMap 参数化条件
%% @param Limit 条数
%% @param Offset 偏移
%% @param OrderSpec 排序规范 [{Field, asc|desc}]
%% @param ValidFields 白名单字段 [binary()]
%% @return {ok, [map()]} | {error, Reason}
-spec select_by_where_safe(
    binary(), map(), integer(), integer(), [{atom() | binary(), asc | desc}], [binary()]
) ->
    {ok, [map()]} | {error, any()}.
select_by_where_safe(Column, WhereMap, Limit, Offset, OrderSpec, ValidFields) ->
    case Limit > 0 of
        false ->
            {ok, []};
        true ->
            Tb = tablename(),
            {Sql, Params} =
                elib_pg_sql:build_select_safe(
                    Tb,
                    Column,
                    WhereMap,
                    OrderSpec,
                    ValidFields,
                    #{limit => Limit, offset => Offset}
                ),
            elib_pg:query(Sql, Params)
    end.

%% @doc 根据邮箱查找管理员用户
%% @param Email 用户邮箱地址
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @example adm_user_repo:find_by_email(<<"admin@example.com">>, <<"id,account,mobile,password,nickname,avatar,role_id,email,status">>).
-spec find_by_email(binary(), binary()) -> map().
find_by_email(Email, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE email = $1">>,
    elib_pg_sql:value_or_empty(elib_pg:one(Sql, [Email])).

%% @doc 根据手机号查找管理员用户
%% @param Mobile 用户手机号码，支持binary或string类型
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @example adm_user_repo:find_by_mobile(<<"13692177080">>, <<"id,account,mobile,password,nickname,avatar,role_id,email,status">>).
%% @example adm_user_repo:find_by_mobile("13692177080", <<"id,account,mobile,password,nickname,avatar,role_id,email,status">>).
-spec find_by_mobile(binary() | string(), binary()) -> map().
find_by_mobile(Mobile, Column) when is_binary(Mobile); is_list(Mobile) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE mobile = $1">>,
    elib_pg_sql:value_or_empty(elib_pg:one(Sql, [Mobile])).

%% @doc 根据用户账号查找管理员用户
%% @param Account 用户账号（字符串或binary类型）
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @example adm_user_repo:find_by_account("admin", <<"id,account,mobile,password,nickname,avatar,role_id,email,status">>).
-spec find_by_account(binary() | string(), binary()) -> map().
find_by_account(Account, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE account = $1">>,
    elib_pg_sql:value_or_empty(elib_pg:one(Sql, [Account])).

%% @doc 根据管理员用户ID查找用户基本信息（使用默认列）
%% @param Uid 管理员用户ID
%% @return {ok, Columns, Rows} 查询成功返回列和行数据 | {error, Reason} 查询失败
-spec find_by_id(integer()) -> map().
find_by_id(Uid) ->
    Column = <<"id,account,avatar,role_id,email,status">>,
    find_by_id(Uid, Column).

%% @doc 根据管理员用户ID查找用户（指定列）
%% @param Uid 管理员用户ID
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return 查询成功返回用户信息 map；查询失败或不存在返回空 map
-spec find_by_id(integer(), binary()) -> map().
find_by_id(Uid, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE id = $1">>,
    elib_pg_sql:value_or_empty(elib_pg:one(Sql, [Uid])).

%% @doc 根据管理员用户ID列表批量查询用户信息
%% @param Uids 管理员用户ID列表，元素类型为integer或binary
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return {ok, [map()]} 查询成功返回map列表 | {error, Reason} 查询失败
-spec list_by_ids(list(integer() | binary()), binary()) -> {ok, [map()]} | {error, any()}.
list_by_ids(Uids, Column) ->
    case Uids of
        [] ->
            {ok, []};
        _ ->
            Tb = tablename(),
            % 构建参数化占位符: $1, $2, $3, ...
            Placeholders = iolist_to_binary(
                lists:join(
                    <<",">>,
                    [<<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(1, length(Uids))]
                )
            ),
            Sql =
                <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE id IN (",
                    Placeholders/binary, ")">>,
            elib_pg:query(Sql, Uids)
    end.

%% @doc 保存新管理员用户记录
%% @param Data 包含管理员用户信息的map，必须包含mobile、password、account等必要字段
%% @return {ok, 1} 保存成功 | {ok, 1, ReturnData} 保存成功并返回数据 | {error, Reason} 保存失败
%% @example adm_user_repo:save(#{mobile => <<"13692177080">>, password => elib_password:generate(<<"admin888">>), account => "admin", status => 1, role_id => 1, nickname => <<"管理员"/utf8>>, created_at => elib_dt:now()}).
-spec save(map()) -> {ok, integer()} | {error, any()}.
save(Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(adm_user),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 更新管理员用户信息
%% @param Id 管理员用户ID
%% @param Data 包含要更新字段的map
%% @return {ok, 1} 更新成功 | {error, Reason} 更新失败
%% @example adm_user_repo:update(1, #{nickname => <<"修改后的昵称"/utf8>>}).
-spec update(integer(), map()) -> {ok, 1} | {error, any()}.
update(Id, Data) ->
    Tb = tablename(),
    elib_pg:update(Tb, Data, <<"id = $1">>, [Id]).

%% @doc 删除管理员用户（软删除）
%% @param Id 管理员用户ID
%% @return {ok, 1} 删除成功 | {error, Reason} 删除失败
%% @details 实际上是软删除，将用户状态更新为 -1
%% @example adm_user_repo:delete(1).
-spec delete(integer()) -> {ok, 1} | {error, any()}.
delete(Id) ->
    Tb = tablename(),
    % 使用安全的参数化查询，避免SQL注入
    % 软删除，更新状态为 -1
    elib_pg:update(Tb, #{status => -1}, <<"id = $1">>, [Id]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
