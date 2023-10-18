-module(friend_category_ds).
%%%
% friend_category_ds 是 friend_category domain service 缩写
%%%

-include_lib("imlib/include/log.hrl").

-export([add/2]).
-export([find_by_uid/1]).
-export([rename/3]).
-export([delete/2]).


%% ===================================================================
%% API
%% ===================================================================

-spec add(integer(), binary()) -> {ok, integer()} | {error, any()}.
add(Uid, Name) ->
    case friend_category_repo:add(Uid, Name) of
        {error, ErrorMsg} ->
            {error, ErrorMsg};
        {ok, Num} ->
            {ok, Num}
    end.


%% return [Id, Username, Avator, Sign].
%% friend_category_ds:find_by_uid(1).
-spec find_by_uid(integer()) -> list().
find_by_uid(Uid) ->
    Field = <<"id, name">>,
    {ok, _FieldList, Rows} = friend_category_repo:find_by_uid(Uid, Field),
    % ?LOG({ok, FieldList, Rows}),
    Default = [{<<"id">>, 0}, {<<"groupname">>, <<"default">>}],
    case length(Rows) == 0 of
        true ->
            [Default];
        _ ->
            [Default |
             [lists:zipwith(fun(X, Y) -> {X, Y} end, [<<"id">>, <<"groupname">>], [Id, Name]) || {Id, Name} <- Rows]]
    end.


% friend_category_ds:rename(Uid, Id, Name).
rename(Uid, Id, Name) ->
    Tb = friend_category_repo:tablename(),
    Where = <<" WHERE owner_user_id = $2 AND id = $3">>,
    Sql = <<"UPDATE ", Tb/binary, " SET name = $1", Where/binary>>,
    imboy_db:execute(Sql, [Name, Uid, Id]),
    ok.


-spec delete(Uid :: any(), Id :: any()) -> ok.
delete(Uid, Id) ->
    friend_category_repo:delete(Uid, Id),
    ok.

%% Internal.
