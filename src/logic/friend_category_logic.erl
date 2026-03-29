-module(friend_category_logic).
%%%
% friend_category 业务逻辑模块
%%%
-export([add/2]).
-export([save/1]).
-export([list/1]).
-export([delete/2]).

-include("log.hrl").

%% @doc 兼容旧入口：创建好友分组并返回旧 map 结构
%% @param Uid 用户ID
%% @param Name 分组名称
%% @return {ok, map()} | {error, term()}
-spec add(integer(), binary()) -> {ok, map()} | {error, term()}.
add(Uid, Name) ->
    case friend_category_ds:add(Uid, Name) of
        {ok, Id} ->
            {ok, #{
                <<"id">> => Id,
                <<"name">> => Name,
                <<"groupname">> => Name
            }};
        Error ->
            Error
    end.

%% @doc 兼容旧入口：保留 save 名称并返回旧的 {ok, Id} 结构
%% @param Data 包含 user_id 和 name 的请求体
%% @return {ok, integer()} | {error, term()}
-spec save(map()) -> {ok, integer()} | {error, term()}.
save(#{<<"user_id">> := Uid, <<"name">> := Name}) ->
    friend_category_ds:add(Uid, Name).

%% @doc 兼容旧入口：返回指定用户的好友分组列表
%% @param Uid 用户ID
%% @return {ok, list(map())}
-spec list(integer()) -> {ok, list(map())}.
list(Uid) ->
    {ok, friend_category_ds:find_by_uid(Uid)}.

%% @doc 删除好友分组
%% 删除指定分组，并将该分组下的好友移动到默认分组（ID=0）
%% @param Uid 用户ID
%% @param Id 分组ID
%% @return {ok, 1} | {error, Reason} 删除结果
-spec delete(integer(), integer()) -> {ok, 1} | {error, term()}.
delete(Uid, Id) ->
    case friend_ds:set_category_id(Uid, Id, 0) of
        {error, ErrorMsg} ->
            {error, ErrorMsg};
        {ok, _} ->
            friend_category_ds:delete(Uid, Id)
    end.

%% Internal.
