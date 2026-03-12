-module(moment_like_repo).
%%%
% moment_like repository
%%%

-export([tablename/0]).
-export([add/2, add/3]).
-export([remove/2, remove/3]).
-export([list_by_post/2]).
-export([count_by_post/1]).
-export([has_liked/2]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"moment_like">>).

-spec add(integer(), integer()) -> {ok, boolean()} | {error, any()}.
add(PostId, UserId) ->
    Tb = tablename(),
    Sql = <<"INSERT INTO ", Tb/binary,
            " (post_id, user_id, created_at)"
            " VALUES ($1, $2, NOW())"
            " ON CONFLICT (post_id, user_id) DO NOTHING"
            " RETURNING 1 AS inserted">>,
    case elib_pg:query(Sql, [PostId, UserId]) of
        {ok, []} ->
            {ok, false};
        {ok, [_ | _]} ->
            {ok, true};
        {error, Reason} ->
            {error, Reason}
    end.

-spec add(any(), integer(), integer()) -> {ok, boolean()} | {error, any()}.
add(Conn, PostId, UserId) ->
    Tb = tablename(),
    Sql = <<"INSERT INTO ", Tb/binary,
            " (post_id, user_id, created_at)"
            " VALUES ($1, $2, NOW())"
            " ON CONFLICT (post_id, user_id) DO NOTHING"
            " RETURNING 1 AS inserted">>,
    case elib_pg:query(Conn, Sql, [PostId, UserId]) of
        {ok, []} ->
            {ok, false};
        {ok, [_ | _]} ->
            {ok, true};
        {error, Reason} ->
            {error, Reason}
    end.

-spec remove(integer(), integer()) -> {ok, boolean()} | {error, any()}.
remove(PostId, UserId) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE post_id = $1 AND user_id = $2">>,
    case elib_pg:execute(Sql, [PostId, UserId]) of
        {ok, Count} when Count > 0 ->
            {ok, true};
        {ok, _} ->
            {ok, false};
        {error, Reason} ->
            {error, Reason}
    end.

-spec remove(any(), integer(), integer()) -> {ok, boolean()} | {error, any()}.
remove(Conn, PostId, UserId) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE post_id = $1 AND user_id = $2">>,
    case elib_pg:execute(Conn, Sql, [PostId, UserId]) of
        {ok, Count} when Count > 0 ->
            {ok, true};
        {ok, _} ->
            {ok, false};
        {error, Reason} ->
            {error, Reason}
    end.

-spec list_by_post(integer(), integer()) -> {ok, [map()]} | {error, any()}.
list_by_post(PostId, Limit) ->
    Tb = tablename(),
    Sql = <<"SELECT post_id, user_id, created_at"
            " FROM ", Tb/binary,
            " WHERE post_id = $1"
            " ORDER BY id DESC LIMIT $2">>,
    elib_pg:query(Sql, [PostId, Limit]).

-spec count_by_post(integer()) -> non_neg_integer().
count_by_post(PostId) ->
    Tb = tablename(),
    Sql = <<"SELECT COUNT(*) AS count FROM ", Tb/binary, " WHERE post_id = $1">>,
    case elib_pg:one(Sql, [PostId]) of
        {ok, #{<<"count">> := Count}} ->
            ec_cnv:to_integer(Count);
        _ ->
            0
    end.

-spec has_liked(integer(), integer()) -> boolean().
has_liked(PostId, UserId) ->
    Tb = tablename(),
    Sql = <<"SELECT 1 AS liked FROM ", Tb/binary,
            " WHERE post_id = $1 AND user_id = $2 LIMIT 1">>,
    case elib_pg:one(Sql, [PostId, UserId]) of
        {ok, #{<<"liked">> := 1}} -> true;
        _ -> false
    end.
