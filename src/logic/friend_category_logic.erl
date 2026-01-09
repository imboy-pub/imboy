-module(friend_category_logic).
%%%
% friend_category 业务逻辑模块
%%%
-export([delete/2]).

-include("log.hrl").

-spec delete(integer(), integer()) -> {ok, 1} | {error, term()}.
delete(Uid, Id) ->
    case friend_ds:set_category_id(Uid, Id, 0) of
        {error, ErrorMsg} ->
            {error, ErrorMsg};
        {ok, _} ->
            friend_category_ds:delete(Uid, Id)
    end.

%% Internal.
