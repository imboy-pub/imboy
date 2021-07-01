-module (friend_ds).
%%%
% friend_ds 是 friend domain service 缩写
%%%
-export ([is_friend/2]).
-export ([find_by_uid/2]).
-export ([change_remark/3]).
-export ([set_category_id/3]).

-include("common.hrl").

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

change_remark(FromUid, ToUid, Remark) ->
    Sql = <<"UPDATE `user_friend` SET `remark` = ?, `updated_at` = ? WHERE `status` = 1 AND `from_user_id` = ? AND `to_user_id` = ?">>,
    mysql_pool:query(Sql, [Remark, dt_util:milliseconds(), FromUid, ToUid]).

set_category_id(Uid, CategoryId, NewCid) ->
    Sql = <<"UPDATE `user_friend` SET `category_id` = ?, `updated_at` = ? WHERE `status` = 1 AND `from_user_id` = ? AND `category_id` = ?">>,
    mysql_pool:query(Sql, [NewCid, dt_util:milliseconds(),Uid, CategoryId]).
