-module(friend_category_logic).
%%%
% friend_category 业务逻辑模块
%%%
-export([add/2]).
-export([delete/2]).

-include_lib("imlib/include/log.hrl").


-spec add(Uid :: any(), Name :: any()) -> {ok, LastInsertId :: integer()} | {error, any()}.
add(Uid, Name) ->
    friend_category_ds:add(Uid, Name).


-spec delete(Uid :: any(), Id :: any()) -> ok | {error, any()}.
delete(Uid, Id) ->
    case friend_ds:set_category_id(Uid, Id, 0) of
        {error, ErrorMsg} ->
            {error, ErrorMsg};
        {ok, _} ->
            friend_category_ds:delete(Uid, Id)
    end.

%% Internal.
