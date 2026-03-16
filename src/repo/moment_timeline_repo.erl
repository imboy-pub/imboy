-module(moment_timeline_repo).
%%%
% moment_timeline repository
%%%
%% Internal persistence detail for the moment_social domain.

-export([tablename/0]).
-export([upsert_batch/5]).
-export([list_by_recipient/3]).
-export([hide_by_post/1, hide_by_post/2]).
-export([delete_by_post/1, delete_by_post/2]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"moment_timeline">>).

-spec upsert_batch(any(), [integer()], integer(), integer(), binary()) ->
    {ok, non_neg_integer()} | {error, any()}.
upsert_batch(_Conn, [], _PostId, _AuthorUid, _RankAt) ->
    {ok, 0};
upsert_batch(Conn, RecipientUids0, PostId, AuthorUid, RankAt) ->
    RecipientUids = lists:usort([Uid || Uid <- RecipientUids0, is_integer(Uid), Uid > 0]),
    Rows = [[Uid, PostId, AuthorUid, RankAt, 1, RankAt] || Uid <- RecipientUids],
    Tb = tablename(),
    {Sql0, Params} = elib_pg_sql:insert_batch(
        Tb,
        [recipient_uid, post_id, author_uid, rank_at, status, created_at],
        Rows
    ),
    Sql = [
        Sql0,
        <<" ON CONFLICT (recipient_uid, post_id) DO UPDATE SET "
          "author_uid = EXCLUDED.author_uid, "
          "rank_at = EXCLUDED.rank_at, "
          "status = 1">>
    ],
    elib_pg:execute(Conn, Sql, Params).

-spec list_by_recipient(integer(), integer(), integer()) -> {ok, [map()]} | {error, any()}.
list_by_recipient(RecipientUid, Cursor, Limit) when is_integer(Cursor), Cursor > 0 ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary,
            " WHERE recipient_uid = $1 AND status = 1 AND id < $2"
            " ORDER BY id DESC LIMIT $3">>,
    elib_pg:query(Sql, [RecipientUid, Cursor, Limit]);
list_by_recipient(RecipientUid, _Cursor, Limit) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary,
            " WHERE recipient_uid = $1 AND status = 1"
            " ORDER BY id DESC LIMIT $2">>,
    elib_pg:query(Sql, [RecipientUid, Limit]).

-spec hide_by_post(integer()) -> {ok, non_neg_integer()} | {error, any()}.
hide_by_post(PostId) ->
    Tb = tablename(),
    Data = #{status => 0},
    elib_pg:update(Tb, Data, <<"post_id = $1">>, [PostId]).

-spec hide_by_post(any(), integer()) -> {ok, non_neg_integer()} | {error, any()}.
hide_by_post(Conn, PostId) ->
    Tb = tablename(),
    Data = #{status => 0},
    elib_pg:update(Conn, Tb, Data, <<"post_id = $1">>, [PostId]).

-spec delete_by_post(integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete_by_post(PostId) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE post_id = $1">>,
    elib_pg:execute(Sql, [PostId]).

-spec delete_by_post(any(), integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete_by_post(Conn, PostId) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE post_id = $1">>,
    elib_pg:execute(Conn, Sql, [PostId]).
