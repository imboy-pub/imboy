-module(logic_friend_category).
%%%
% friend_category 业务逻辑模块
%%%
-export([add/2]).
-export([delete/2]).

-include("common.hrl").


-spec add(Uid :: any(), Name :: any()) ->
          {ok, LastInsertId :: integer()} | {error, any()}.
add(Uid, Name) ->
    ds_friend_category:add(Uid, Name).


-spec delete(Uid :: any(), Id :: any()) ->
          ok | {error, ErrorMsg :: any()}.
delete(Uid, Id) ->
    case ds_friend:set_category_id(Uid, Id, 0) of
        {error, {_, _, ErrorMsg}} ->
            {error, ErrorMsg};
        ok ->
            ds_friend_category:delete(Uid, Id)
    end.

%% Internal.
