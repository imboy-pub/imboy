-module(friend_category_repo).
%%%
% friend_category_repo 是 friend_category repository 缩写
%%%
-export([tablename/0]).
-export([list_by_uid/2, list_by_uid/3]).
-export([add/2]).
-export([delete/2]).
-export([find_by_name/2]).

%% ===================================================================
%% API
%% ===================================================================


%% @doc 获取好友分类表的表名
%% @return 返回好友分类表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"user_friend_category">>).


%% @doc 查询指定用户的好友分类列表（使用默认限制10000）
%% @param Uid 用户ID
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
-spec list_by_uid(integer(), binary()) -> {ok, list(map())} | {error, term()}.
list_by_uid(Uid, Column) ->
    list_by_uid(Uid, Column, 1000).

%% @doc 根据用户ID和分类名称查找好友分类
%%
%% 查询指定用户下是否存在指定名称的分类
%%
%% @param Uid 用户ID
%% @param Name 分类名称
%% @returns {ok, map()} | {error, any()}
-spec find_by_name(integer(), binary()) -> {ok, map()}  | {error, any()}.
find_by_name(Uid, Name) ->
    Tb = tablename(),
    Where = <<" WHERE owner_user_id = $1 AND name = $2">>,
    Sql = <<"SELECT id, name, owner_user_id FROM ", Tb/binary, Where/binary, " LIMIT 1">>,
    elib_pg:one(Sql, [Uid, Name], #{}).

%% @doc 添加新的好友分类
%% @param Uid 用户ID
%% @param Name 分类名称（支持binary或string类型）
%% @return {ok, Id} 添加成功返回分类ID | {error, Reason} 添加失败
%% @example friend_category_repo:add(1, <<"测试"/utf8>>).
-spec add(integer(), binary() | string()) -> {ok, integer()} | {error, term()}.
add(Uid, Name) ->
    Tb = tablename(),
    Id = elib_tsid:generate(friend_category),
    Sql = <<"INSERT INTO ", Tb/binary, " (id, name, owner_user_id)
        VALUES ($1, $2, $3)">>,
    case elib_pg:execute(Sql, [Id, Name, Uid]) of
        {ok, _Count} ->
            {ok, Id};
        {error, Reason} ->
            {error, Reason}
    end.


%% @doc 删除好友分类
%% @param Uid 用户ID
%% @param Id 分类ID
%% @return {ok, Count} 删除成功返回影响行数 | {error, Reason} 删除失败
%% @example friend_category_repo:delete(1, 1).
-spec delete(integer(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
delete(Uid, Id) ->
    Tb = tablename(),
    Where = <<" WHERE id = $1 AND owner_user_id = $2">>,
    Sql = <<"DELETE FROM ", Tb/binary, Where/binary>>,
    elib_pg:execute(Sql, [Id, Uid]).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


%% @doc 查询指定用户的好友分类列表（指定限制数量）
%% @param Uid 用户ID
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @param Limit 查询结果数量限制
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
%% @example friend_category_repo:list_by_uid(1, <<"name">>, 100).
%% @example friend_category_repo:list_by_uid(1, <<"id,name">>, 100).
-spec list_by_uid(integer(), binary(), integer()) -> {ok, list(map())} | {error, term()}.
list_by_uid(Uid, Column, Limit) ->
    Tb = tablename(),
    Where = <<" WHERE owner_user_id = $1 LIMIT $2">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    elib_pg:query(Sql, [Uid, Limit]).
