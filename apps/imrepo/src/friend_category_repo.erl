-module(friend_category_repo).
%%%
% friend_category_repo 是 friend_category repository 缩写
%%%
-export([tablename/0]).
-export([find_by_uid/2]).
-export([add/2]).
-export([delete/2]).

%% ===================================================================
%% API
%% ===================================================================


tablename() ->
    imboy_db:public_tablename(<<"user_friend_category">>).


find_by_uid(Uid, Column) ->
    find_by_uid(Uid, Column, 1000).


% friend_category_repo:add(1, "测试").
% friend_category_repo:add(1, <<"测试2"/utf8>>).
add(Uid, Name) ->
    Tb = tablename(),
    Sql = <<"INSERT INTO ", Tb/binary, " (name, owner_user_id)
        VALUES ($1, $2)  RETURNING id">>,
    case imboy_db:execute(Sql, [Name, Uid]) of
        {ok, 1, [{Id}]} ->
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
    imboy_db:execute(Sql, [Id, Uid]).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


% friend_category_repo:find_by_uid(1, <<"name">>).
% friend_category_repo:find_by_uid(1, <<"id,name">>).
find_by_uid(Uid, Column, Limit) ->
    Tb = tablename(),
    Where = <<" WHERE owner_user_id = $1 LIMIT $2">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_db:query(Sql, [Uid, Limit]).
