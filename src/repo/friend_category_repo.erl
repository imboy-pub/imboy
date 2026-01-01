-module(friend_category_repo).
%%%
% friend_category_repo 是 friend_category repository 缩写
%%%
-export([tablename/0]).
-export([list_by_uid/2]).
-export([add/2]).
-export([delete/2]).
-export([find_by_name/2]).

%% ===================================================================
%% API
%% ===================================================================


tablename() ->
    imboy_pg_sql:public_tablename(<<"user_friend_category">>).


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
    imboy_pg:one(Sql, [Uid, Name], #{}).

% friend_category_repo:add(1, "测试").
% friend_category_repo:add(1, <<"测试2"/utf8>>).
add(Uid, Name) ->
    Tb = tablename(),
    Sql = <<"INSERT INTO ", Tb/binary, " (name, owner_user_id)
        VALUES ($1, $2)  RETURNING id">>,
    case imboy_pg_sql:parse_result(imboy_pg:execute(Sql, [Name, Uid])) of
        {ok, Id, _} ->
            {ok, Id};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, unknown}
    end.


% friend_category_repo:delete(1, 1).
delete(Uid, Id) ->
    Tb = tablename(),
    Where = <<" WHERE id = $1 AND owner_user_id = $2">>,
    Sql = <<"DELETE FROM ", Tb/binary, Where/binary>>,
    imboy_pg:execute(Sql, [Id, Uid]).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


% friend_category_repo:list_by_uid(1, <<"name">>).
% friend_category_repo:list_by_uid(1, <<"id,name">>).
list_by_uid(Uid, Column, Limit) ->
    Tb = tablename(),
    Where = <<" WHERE owner_user_id = $1 LIMIT $2">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_pg:query(Sql, [Uid, Limit]).
