-module (friend_category_as).
%%%
% friend_category_as 是 friend_category application service 缩写
%%%
-export ([add/2]).
-export ([delete/2]).

-include("imboy.hrl").

-spec add(Uid::any(), Name::any()) -> {ok, LastInsertId::integer()} | {error, any()}.
add(Uid, Name) ->
    friend_category_ds:add(Uid, Name).

-spec delete(Uid::any(), Id::any()) -> ok | {error, ErrorMsg::any()}.
delete(Uid, Id) ->
    case friend_ds:set_category_id(Uid, Id, 0) of
        {error, {_, _, ErrorMsg}} ->
            {error, ErrorMsg};
        ok ->
            friend_category_ds:delete(Uid, Id)
    end.

%% Internal.
