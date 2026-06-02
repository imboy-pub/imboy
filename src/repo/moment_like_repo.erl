-module(moment_like_repo).
%%%
% moment_like repository
%%%
%% Internal persistence detail for the moment_social domain.

-export([tablename/0]).
-export([add/2, add/3]).
-export([remove/2, remove/3]).
-export([list_by_post/2]).
-export([has_liked/2]).
-export([liked_post_ids/2]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"moment_like">>).

-spec add(integer(), integer()) -> {ok, boolean()} | {error, any()}.
add(PostId, UserId) ->
    Tb = tablename(),
    GenId = elib_tsid:generate(moment_like),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, post_id, user_id, created_at)"
            " VALUES ($1, $2, $3, NOW())"
            " ON CONFLICT (post_id, user_id) DO NOTHING"
            " RETURNING 1 AS inserted">>,
    case elib_pg:query(Sql, [GenId, PostId, UserId]) of
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
    GenId = elib_tsid:generate(moment_like),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, post_id, user_id, created_at)"
            " VALUES ($1, $2, $3, NOW())"
            " ON CONFLICT (post_id, user_id) DO NOTHING"
            " RETURNING 1 AS inserted">>,
    case elib_pg:query(Conn, Sql, [GenId, PostId, UserId]) of
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
    Sql = <<
        "SELECT post_id, user_id, created_at"
        " FROM ",
        Tb/binary,
        " WHERE post_id = $1"
        " ORDER BY id DESC LIMIT $2"
    >>,
    elib_pg:query(Sql, [PostId, Limit]).

-spec has_liked(integer(), integer()) -> boolean().
has_liked(PostId, UserId) ->
    Tb = tablename(),
    Sql = <<"SELECT 1 AS liked FROM ", Tb/binary, " WHERE post_id = $1 AND user_id = $2 LIMIT 1">>,
    case elib_pg:one(Sql, [PostId, UserId]) of
        {ok, #{<<"liked">> := 1}} -> true;
        _ -> false
    end.

%% @doc 批量查询用户在给定动态集合中点赞过的 post_id 子集（避免 N+1）
-spec liked_post_ids([integer()], integer()) -> {ok, [integer()]} | {error, any()}.
liked_post_ids(PostIds, UserId) when is_list(PostIds), PostIds =/= [] ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(
        Tb, <<"post_id">>, #{post_id => {in, PostIds}, user_id => UserId}, #{}
    ),
    case elib_pg:query(Sql, Params) of
        {ok, Rows} -> {ok, [maps:get(<<"post_id">>, R) || R <- Rows]};
        Err -> Err
    end;
liked_post_ids(_, _) ->
    {ok, []}.
