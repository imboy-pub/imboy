-module(ds_friend_category).
%%%
% ds_friend_category 是 friend_category domain service 缩写
%%%
-export([add/2]).
-export([find_by_uid/1]).
-export([rename/3]).
-export([delete/2]).

-include("common.hrl").


-spec find_by_uid(integer()) -> list().


-spec add(Uid :: any(), Name :: any()) ->
          {ok, LastInsertId :: integer()} | {error, any()}.
add(Uid, Name) ->
    case repo_friend_category:add(Uid, Name) of
        {error, {_, _, ErrorMsg}} ->
            {error, ErrorMsg};
        {ok, LastInsertId} ->
            {ok, LastInsertId}
    end.


%% return [Id, Username, Avator, Sign].
find_by_uid(Uid) ->
    Field = <<"`id`, `name` as groupname">>,
    {ok, FieldList, Rows} = repo_friend_category:find_by_uid(Uid,
                                                             Field),
    % ?LOG({ok, FieldList, Rows}),
    Default = [{<<"id">>, 0}, {<<"groupname">>, <<"default">>}],
    case length(Rows) == 0 of
        true ->
            [Default];
        _ ->
            [Default |
             [lists:zipwith(fun(X, Y) -> {X, Y} end, FieldList, Row) ||
                 Row <- Rows]]
    end.


rename(Uid, Id, Name) ->
    Sql = <<"UPDATE `user_friend_category` SET `name` = ?
        WHERE `owner_user_id` = ? AND `id` = ?">>,
    mysql_pool:query(Sql, [Name, Uid, Id]).


-spec delete(Uid :: any(), Id :: any()) ->
          ok | {error, ErrorMsg :: any()}.
delete(Uid, Id) ->
    case repo_friend_category:delete(Uid, Id) of
        {error, {_, _, ErrorMsg}} ->
            {error, ErrorMsg};
        ok ->
            ok
    end.

%% Internal.