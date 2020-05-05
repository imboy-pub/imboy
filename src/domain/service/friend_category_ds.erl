-module (friend_category_ds).
%%%
% friend_category_ds 是 friend_category domain service 缩写
%%%
-export ([find_by_uid/1]).

-include("imboy.hrl").

-spec find_by_uid(integer()) -> list().

%% return [Id, Username, Avator, Sign].
find_by_uid(Uid) ->
    Field = <<"`id`, `name` as groupname">>,
    {ok, FieldList, Rows} = friend_category_repo:find_by_uid(Uid, Field),
    Default = [
        {<<"id">>, 0},
        {<<"groupname">>, <<"default">>}
    ],
    if
        length(Rows) == 0  ->
            [Default];
        true ->
            [Default | [lists:zipwith(fun(X, Y) -> {X,Y} end, FieldList, Row) || Row <- Rows]]
    end.
