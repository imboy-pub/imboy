-module (user_ds).
%%%
% user_ds 是 user domain service 缩写
%%%
-export ([find_by_id/1]).
-export ([find_by_ids/1, find_by_ids/2]).

-include("imboy.hrl").

-spec find_by_id(binary()) -> list().

%% return [Id, Username, Avator, Sign].
find_by_id(Id) ->
    Column = <<"`id`,`username`,`avatar`,`sign`">>,
    {ok, ColumnList, Rows} = user_repo:find_by_id(Id, Column),
    if
        length(Rows) == 0  ->
            {error, "用户不存在"};
        true ->
            [Row|_] = Rows,
            filter_user(ColumnList, Row)
    end.

find_by_ids(Ids) ->
    Column = <<"`id`,`username`,`avatar`,`sign`">>,
    find_by_ids(Ids, Column).

find_by_ids(Ids, Column) ->
    {ok, ColumnList, Rows} = user_repo:find_by_ids(Ids, Column),
    if
        length(Rows) == 0  ->
            {error, "用户不存在"};
        true ->
            [filter_user(ColumnList, Row) || Row <- Rows]
    end.


filter_user(ColumnList, User) ->
    User2 = case User of
        [Uid, Username, <<>>, Sign] ->
            [Uid, Username, <<"/static/image/default_avatar_male_180.gif">>, Sign] ;
        _ ->
            User
    end,
    lists:zipwith(fun(X, Y) -> {X,Y} end, ColumnList, User2).
