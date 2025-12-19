-module(user_repo).
%%%
% user_repo 是 user repository 缩写
% 用户数据仓库层，提供用户数据的基础数据库操作
%%%

-include_lib("imlib/include/log.hrl").
-include_lib("imlib/include/def_column.hrl").

-export([tablename/0]).
-export ([save/1, update/2, delete/1]).

-export([find_by_email/2,
         find_by_mobile/2,
         find_by_account/2]).
-export([find_by_id/1, find_by_id/2]).
-export([list_by_ids/2]).
-export([select_by_where/4]).
-export([select_by_where/5]).

-export([update_friends_last_seen_at/2]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取用户表的表名
%% @return 返回用户表的完整表名
-spec tablename() -> binary().
tablename() ->
    imboy_db:public_tablename(<<"user">>).


%% @doc 根据WHERE条件查询用户列表（使用默认列）
%% @param Where SQL WHERE子句条件
%% @param Limit 查询结果数量限制
%% @param Offset 查询结果偏移量
%% @param OrderBy 排序字段
%% @return {ok, Columns, Rows} 查询成功返回列和行数据 | {error, Reason} 查询失败
-spec select_by_where(binary(), integer(), integer(), binary()) -> {ok, list(), list()} | {error, any()}.
select_by_where(Where, Limit, Offset, OrderBy) ->
    select_by_where(?DEF_USER_COLUMN, Where, Limit, Offset, OrderBy).

%% @doc 根据WHERE条件查询用户列表（支持全文搜索）
%% @param Column 要查询的列名
%% @param Where SQL WHERE子句条件
%% @param Limit 查询结果数量限制
%% @param Offset 查询结果偏移量
%% @param OrderBy 排序字段
%% @return {ok, Columns, Rows} 查询成功返回列和行数据 | {error, Reason} 查询失败
%% @doc 内部函数实现，会关联全文搜索表进行查询
select_by_where(Column, Where, Limit, Offset, OrderBy) ->
    Tb = tablename(),
    FtsTb = fts_user_repo:tablename(),
    Limit2 = integer_to_binary(Limit),
    Offset2 = integer_to_binary(Offset),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " u LEFT JOIN ", FtsTb/binary,
            " fts ON fts.user_id = u.id
     WHERE ", Where/binary, " order by ", OrderBy/binary, " LIMIT ", Limit2/binary, " OFFSET ", Offset2/binary>>,
    imboy_log:info(io_lib:format("user_repo/select_by_where/5: Sql ~p ~n", [Sql])),
    imboy_db:query(Sql, []).


%% @doc 根据邮箱查找用户
%% @param Email 用户邮箱地址
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return {ok, Columns, Rows} 查询成功返回列和行数据 | {error, Reason} 查询失败
%% @example user_repo:find_by_email(<<"100000@imboy.pub">>, <<"id,account,mobile,password,nickname,avatar,gender,region,sign">>).
-spec find_by_email(binary(), binary()) -> {ok, list(), list()} | {error, any()}.
find_by_email(Email, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE email = $1">>,
    % ?DEBUG_LOG(["sql ", Sql]),
    imboy_db:find(Sql, [Email]).


%% @doc 根据手机号查找用户
%% @param Mobile 用户手机号码，支持binary或string类型
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return {ok, Columns, Rows} 查询成功返回列和行数据 | {error, Reason} 查询失败
%% @example user_repo:find_by_mobile(<<"13692177080">>, <<"*">>).
%% @example user_repo:find_by_mobile("13692177080", <<"*">>).
-spec find_by_mobile(binary() | string(), binary()) -> {ok, list(), list()} | {error, any()}.
find_by_mobile(Mobile, Column) when is_binary(Mobile); is_list(Mobile) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE mobile = $1">>,
    % ?DEBUG_LOG(["sql ", Sql]),
    imboy_db:find(Sql, [Mobile]).


%% @doc 根据用户账号查找用户
%% @param Account 用户账号（字符串或binary类型）
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return {ok, Columns, Rows} 查询成功返回列和行数据 | {error, Reason} 查询失败
%% @example user_repo:find_by_account("550138", <<"id,account,mobile,password,nickname,avatar,gender,region,sign">>).
-spec find_by_account(binary() | string(), binary()) -> {ok, list(), list()} | {error, any()}.
find_by_account(Account, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE account = $1">>,
    imboy_db:find(Sql, [Account]).


%% @doc 根据用户ID查找用户基本信息（使用默认列）
%% @param Uid 用户ID
%% @return {ok, Columns, Rows} 查询成功返回列和行数据 | {error, Reason} 查询失败
-spec find_by_id(integer()) -> {ok, list(), list()} | {error, any()}.
find_by_id(Uid) ->
    Column = <<"id,account,avatar,sign">>,
    find_by_id(Uid, Column).


%% @doc 根据用户ID查找用户（指定列）
%% @param Uid 用户ID
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return {ok, Columns, Rows} 查询成功返回列和行数据 | {error, Reason} 查询失败
-spec find_by_id(integer(), binary()) -> {ok, list(), list()} | {error, any()}.
find_by_id(Uid, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE id = $1">>,
    % ?DEBUG_LOG([Sql]),
    imboy_db:find(Sql, [Uid]).


%% @doc 根据用户ID列表批量查询用户信息
%% @param Uids 用户ID列表，元素类型为integer或binary
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return {ok, Columns, Rows} 查询成功返回列和行数据 | {error, Reason} 查询失败
-spec list_by_ids(list(integer() | binary()), binary()) -> {ok, list(), list()} | {error, any()}.
list_by_ids(Uids, Column) ->
    Tb = tablename(),
    L1 = lists:flatmap(fun(Uid) ->
        case is_binary(Uid) of
            true -> [Uid, ","];
            false -> [integer_to_binary(Uid), ","]
        end
    end, Uids),
    [_ | L2] = lists:reverse(L1),
    Ids = list_to_binary(lists:concat(L2)),
    Where = <<" WHERE id IN (", Ids/binary, ")">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_db:query(Sql).


%% @doc 更新指定用户的所有好友关系中的最后在线时间
%% @param Uid 用户ID
%% @param Timestamp 要更新的时间戳（timestamptz格式）
%% @return ok
%% @details 该函数会更新用户作为from_user_id和to_user_id的所有好友关系记录
update_friends_last_seen_at(Uid, Timestamp) ->
    % 更新我是from_user_id的记录
    update_last_seen_at_by_from_uid(Uid, Timestamp),
    % 更新我是to_user_id的记录
    update_last_seen_at_by_to_uid(Uid, Timestamp).



%% @doc 保存新用户记录
%% @param Data 包含用户信息的map，必须包含mobile、password、account等必要字段
%% @return {ok, 1} 保存成功 | {ok, 1, ReturnData} 保存成功并返回数据 | {error, Reason} 保存失败
%% @example user_repo:save(#{mobile => <<"13692177080">>, password => imboy_password:generate(imboy_hasher:md5("admin888")), account => "13692177080A", status => 1, role_id => {1,3}, nickname => <<"大大大"/utf8>>, created_at => imboy_dt:now()}).
-spec save(map()) -> {ok, 1} | {ok, 1, term()} | {error, any()}.
save(Data) ->
    Tb = tablename(),
    imboy_db:insert_into(Tb, Data).

%% @doc 更新用户信息
%% @param Id 用户ID
%% @param Data 包含要更新字段的map
%% @return {ok, 1} 更新成功 | {error, Reason} 更新失败
%% @example user_repo:update(1, #{role_name => <<"修改后的角色名称"/utf8>>}).
-spec update(integer(), map()) -> {ok, 1} | {error, any()}.
update(Id, Data) ->
    Tb = tablename(),
    Where = <<"id = ", (integer_to_binary(Id))/binary>>,
    imboy_db:update(Tb, Where, Data).

%% @doc 删除用户（软删除）
%% @param Id 用户ID
%% @return {ok, 1} 删除成功 | {error, Reason} 删除失败
%% @details 实际上是软删除，将用户状态更新为 -1
%% @example user_repo:delete(1).
-spec delete(integer()) -> {ok, 1} | {error, any()}.
delete(Id) ->
    Tb = tablename(),
    Where = <<"id = ", (integer_to_binary(Id))/binary>>,
    % 软删除，更新状态为 -1
    imboy_db:update_by_id(Tb, Where, #{<<"status">> => -1}).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 更新from_user_id为指定用户的好友关系记录的最后在线时间
%% @param Uid 用户ID
%% @param Timestamp 要更新的时间戳
%% @return {ok, Count} | {error, Reason}
update_last_seen_at_by_from_uid(Uid, Timestamp) ->
    Tb = friend_repo:tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET last_seen_at = $1::timestamptz, updated_at = $2::timestamptz ",
            "WHERE from_user_id = $3 AND status = 1">>,
    imboy_db:execute(Sql, [Timestamp, imboy_dt:now(), Uid]).

%% @doc 更新to_user_id为指定用户的好友关系记录的最后在线时间
%% @param Uid 用户ID
%% @param Timestamp 要更新的时间戳
%% @return {ok, Count} | {error, Reason}
update_last_seen_at_by_to_uid(Uid, Timestamp) ->
    Tb = friend_repo:tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET last_seen_at = $1::timestamptz, updated_at = $2::timestamptz ",
            "WHERE to_user_id = $3 AND status = 1">>,
    imboy_db:execute(Sql, [Timestamp, imboy_dt:now(), Uid]).
