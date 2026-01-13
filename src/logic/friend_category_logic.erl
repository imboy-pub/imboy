-module(friend_category_logic).
%%%
% friend_category 业务逻辑模块
%%%
-export([delete/2]).

-include("log.hrl").

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
