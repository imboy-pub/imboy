-module(moment_comment_repo).
%%%
% moment_comment repository
%%%

-export([tablename/0]).
-export([add/1, add/2]).
-export([find_by_id/1, find_any_by_id/1]).
-export([page_by_post/3]).
-export([soft_delete/1, soft_delete/2]).
-export([count_by_post/1]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"moment_comment">>).

-spec add(map()) -> {ok, integer(), map()} | {error, term()}.
add(Data) ->
    Tb = tablename(),
    elib_pg_sql:parse_result(elib_pg:insert(Tb, Data, <<"RETURNING id">>)).

-spec add(any(), map()) -> {ok, integer(), map()} | {error, term()}.
add(Conn, Data) ->
    Tb = tablename(),
    elib_pg_sql:parse_result(elib_pg:insert(Conn, Tb, Data, <<"RETURNING id">>)).

-spec find_by_id(integer()) -> map() | {error, any()}.
find_by_id(CommentId) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary, " WHERE id = $1 AND status = 1 LIMIT 1">>,
    case elib_pg:one(Sql, [CommentId]) of
        {ok, Row} -> Row;
        {error, Reason} -> {error, Reason}
    end.

-spec find_any_by_id(integer()) -> map() | {error, any()}.
find_any_by_id(CommentId) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary, " WHERE id = $1 LIMIT 1">>,
    case elib_pg:one(Sql, [CommentId]) of
        {ok, Row} -> Row;
        {error, Reason} -> {error, Reason}
    end.

-spec page_by_post(integer(), integer(), integer()) -> {ok, [map()]} | {error, any()}.
page_by_post(PostId, Cursor, Limit) when is_integer(Cursor), Cursor > 0 ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary,
            " WHERE post_id = $1 AND status = 1 AND id < $2"
            " ORDER BY id DESC LIMIT $3">>,
    elib_pg:query(Sql, [PostId, Cursor, Limit]);
page_by_post(PostId, _Cursor, Limit) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary,
            " WHERE post_id = $1 AND status = 1"
            " ORDER BY id DESC LIMIT $2">>,
    elib_pg:query(Sql, [PostId, Limit]).

-spec soft_delete(integer()) -> {ok, non_neg_integer()} | {error, any()}.
soft_delete(CommentId) ->
    Tb = tablename(),
    Data = #{status => 0, updated_at => elib_dt:now()},
    elib_pg:update(Tb, Data, <<"id = $1 AND status = 1">>, [CommentId]).

-spec soft_delete(any(), integer()) -> {ok, non_neg_integer()} | {error, any()}.
soft_delete(Conn, CommentId) ->
    Tb = tablename(),
    Data = #{status => 0, updated_at => elib_dt:now()},
    elib_pg:update(Conn, Tb, Data, <<"id = $1 AND status = 1">>, [CommentId]).

-spec count_by_post(integer()) -> non_neg_integer().
count_by_post(PostId) ->
    Tb = tablename(),
    Sql = <<"SELECT COUNT(*) AS count FROM ", Tb/binary,
            " WHERE post_id = $1 AND status = 1">>,
    case elib_pg:one(Sql, [PostId]) of
        {ok, #{<<"count">> := Count}} ->
            ec_cnv:to_integer(Count);
        _ ->
            0
    end.
