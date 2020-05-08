-module (friend_ds).
%%%
% friend_ds 是 friend domain service 缩写
%%%
-export ([is_friend/2]).
-export ([find_by_uid/2]).

-include("imboy.hrl").

-spec find_by_uid(integer(), list()) -> list().

%% ToUid 是 FromUid 的好友？
is_friend(FromUid, ToUid) ->
    case friend_repo:is_friend(FromUid, ToUid) of
        {ok, _ColumnLi, [[Count]]} when Count > 0->
            true;
        _ ->
            false
    end.

find_by_uid(Uid, Column) ->
    case friend_repo:find_by_uid(Uid, Column) of
        {ok, _, []} ->
            [];
        {ok, ColumnList, Rows} ->
            [lists:zipwith(fun(X, Y) -> {X,Y} end, ColumnList, Row) || Row <- Rows];
        _ ->
            []
    end.
