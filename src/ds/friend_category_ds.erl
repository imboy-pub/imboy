-module(friend_category_ds).
%%%
% friend_category_ds 是 friend_category domain service 缩写
%%%

-include("log.hrl").

-export([add/2]).
-export([find_by_uid/1]).
-export([rename/3]).
-export([delete/2]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 添加好友分类
%%
%% 为指定用户添加一个新的好友分类，如果同名分类已存在则返回错误
%%
%% @param Uid 用户ID
%% @param Name 分类名称
%% @returns {ok, integer()} | {error, any()} 操作结果
-spec add(integer(), binary()) -> {ok, integer()} | {error, any()}.
add(Uid, Name) ->
    % 检查同名分类是否已存在
    case friend_category_repo:find_by_name(Uid, Name) of
        {ok, #{<<"id">> := Cid}} ->
            % 同名分类已存在，直接返回
            {ok, Cid};
        {ok, #{}} ->
            % 未找到同名分类，执行插入
            case friend_category_repo:add(Uid, Name) of
                {error, ErrorMsg} ->
                    {error, ErrorMsg};
                {ok, Cid} ->
                    {ok, Cid}
            end;
        {error, Reason} ->
            {error, Reason}
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
    {ok, Rows} = friend_category_repo:list_by_uid(Uid, Field),
    % ?DEBUG_LOG({ok, Rows}),
    Default = [{<<"id">>, 0}, {<<"groupname">>, <<"default">>}],
    case length(Rows) == 0 of
        true ->
            [Default];
        _ ->
            [Default | [ [{<<"id">>, Id}, {<<"groupname">>, Name}] || #{<<"id">> := Id, <<"name">> := Name} <- Rows ]]
    end.

%% @doc 重命名好友分类
%%
%% 修改指定好友分类的名称
%%
%% @param Uid 用户ID
%% @param Id 分类ID
%% @param Name 新的分类名称
%% @returns {ok, 1} | {error, any()} 操作结果
% friend_category_ds:rename(Uid, Id, Name).
-spec rename(integer(), any(), binary()) -> {ok, 1} | {error, any()}.
rename(_Uid, Id, _Name) when Id =:= undefined; Id =:= <<"">>; Id =:= "" ->
    {error, "invalid_id: Id is required"};
rename(_Uid, _Id, Name) when Name =:= undefined; Name =:= <<"">>; Name =:= "" ->
    {error, "invalid_name: Name is required"};
rename(Uid, Id, Name) ->
    Tb = friend_category_repo:tablename(),
    Where = <<"owner_user_id = $1 AND id = $2">>,
    WhereArg = [Uid, ec_cnv:to_integer(Id)],
    case imboy_pg:update(Tb, #{<<"name">> => Name}, Where, WhereArg) of
        {ok, 1} ->
            {ok, 1};
        {ok, 0} ->
            {error, "not_found: Category not found"};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 删除好友分类
%%
%% 删除指定的好友分类
%%
%% @param Uid 用户ID
%% @param Id 分类ID
%% @returns {ok, 1} | {error, any()} 操作结果
-spec delete(any(), any()) -> {ok, 1} | {error, any()}.
delete(_Uid, Id) when Id =:= undefined; Id =:= <<"">>; Id =:= "" ->
    {error, {"invalid_id", "Id is required"}};
delete(Uid, Id) ->
    case friend_category_repo:delete(Uid, Id) of
        {ok, 1} ->
            {ok, 1};
        {ok, 0} ->
            {error, "not_found Category not found"};
        {error, Reason} ->
            {error, Reason}
    end.

%% Internal.
