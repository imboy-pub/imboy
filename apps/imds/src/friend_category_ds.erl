-module(friend_category_ds).
%%%
% friend_category_ds 是 friend_category domain service 缩写
%%%

-include_lib("imlib/include/log.hrl").

-export([add/2]).
-export([find_by_uid/1]).
-export([rename/3]).
-export([delete/2]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 添加好友分类
%%
%% 为指定用户添加一个新的好友分类
%%
%% @param Uid 用户ID
%% @param Name 分类名称
%% @returns {ok, integer()} | {error, any()} 操作结果
-spec add(integer(), binary()) -> {ok, integer()} | {error, any()}.
add(Uid, Name) ->
    case friend_category_repo:add(Uid, Name) of
        {error, ErrorMsg} ->
            {error, ErrorMsg};
        {ok, Num} ->
            {ok, Num}
    end.

%% @doc 根据用户ID查找好友分类
%%
%% 获取指定用户的所有好友分类，包括默认分类
%%
%% return [Id, Username, Avator, Sign].
%% @param Uid 用户ID
%% @returns list() 好友分类列表，包含默认分类
%% friend_category_ds:find_by_uid(1).
-spec find_by_uid(integer()) -> list().
find_by_uid(Uid) ->
    Field = <<"id, name">>,
    {ok, _FieldList, Rows} = friend_category_repo:list_by_uid(Uid, Field),
    % ?DEBUG_LOG({ok, FieldList, Rows}),
    Default = [{<<"id">>, 0}, {<<"groupname">>, <<"default">>}],
    case length(Rows) == 0 of
        true ->
            [Default];
        _ ->
            [Default | [ lists:zipwith(fun(X, Y) -> {X, Y} end, [<<"id">>, <<"groupname">>], [Id, Name]) || {Id, Name} <- Rows ]]
    end.

%% @doc 重命名好友分类
%%
%% 修改指定好友分类的名称
%%
%% @param Uid 用户ID
%% @param Id 分类ID
%% @param Name 新的分类名称
%% @returns ok 表示操作成功
% friend_category_ds:rename(Uid, Id, Name).
-spec rename(integer(), any(), binary()) -> ok.
rename(Uid, Id, Name) ->
    Tb = friend_category_repo:tablename(),
    Where = <<" WHERE owner_user_id = $2 AND id = $3">>,
    Sql = <<"UPDATE ", Tb/binary, " SET name = $1", Where/binary>>,
    imboy_db:execute(Sql, [Name, Uid, Id]),
    ok.

%% @doc 删除好友分类
%%
%% 删除指定的好友分类
%%
%% @param Uid 用户ID
%% @param Id 分类ID
%% @returns ok 表示操作成功
-spec delete(any(), any()) -> ok.
delete(Uid, Id) ->
    friend_category_repo:delete(Uid, Id),
    ok.

%% Internal.
