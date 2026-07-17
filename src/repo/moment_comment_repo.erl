-module(moment_comment_repo).
%%%
% moment_comment repository
%%%
%% Internal persistence detail for the moment_social domain.

-export([add/1, add/2]).
-export([find_by_id/1, find_any_by_id/1]).
-export([page_by_post/3]).
-export([recent_comments_by_posts/2]).
-export([soft_delete/1, soft_delete/2]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"moment_comment">>).

-spec add(map()) -> {ok, integer()} | {error, term()}.
add(Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(moment_comment),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

-spec add(any(), map()) -> {ok, integer()} | {error, term()}.
add(Conn, Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(moment_comment),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Conn, Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

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
    Sql =
        <<"SELECT * FROM ", Tb/binary,
            " WHERE post_id = $1 AND status = 1 AND id < $2"
            " ORDER BY id DESC LIMIT $3">>,
    elib_pg:query(Sql, [PostId, Cursor, Limit]);
page_by_post(PostId, _Cursor, Limit) ->
    Tb = tablename(),
    Sql =
        <<"SELECT * FROM ", Tb/binary,
            " WHERE post_id = $1 AND status = 1"
            " ORDER BY id DESC LIMIT $2">>,
    elib_pg:query(Sql, [PostId, Limit]).

%% @doc 批量查询多个动态各自最新 PerPostLimit 条评论（避免 N+1）。
%% 用 ROW_NUMBER() 窗口函数按 post_id 分组取最新 N 条，输出按帖内 id
%% 升序（时间正序展示，与微信朋友圈一致）。
%% Batch-fetch the latest N comments per post with a window function.
-spec recent_comments_by_posts([integer()], integer()) ->
    {ok, [map()]} | {error, any()}.
recent_comments_by_posts([], _PerPostLimit) ->
    {ok, []};
recent_comments_by_posts(PostIds, PerPostLimit) when
    is_list(PostIds),
    PostIds =/= [],
    is_integer(PerPostLimit),
    PerPostLimit > 0
->
    Tb = tablename(),
    % $1 = post_id 数组，$2 = 每帖评论条数上限
    Sql = <<
        "WITH ranked AS ("
        " SELECT id, post_id, user_id, reply_to_uid, content, created_at,",
        " ROW_NUMBER() OVER (PARTITION BY post_id ORDER BY id DESC) AS rn",
        " FROM ",
        Tb/binary,
        " WHERE post_id = ANY($1) AND status = 1"
        ")",
        " SELECT id, post_id, user_id, reply_to_uid, content, created_at",
        " FROM ranked WHERE rn <= $2",
        " ORDER BY post_id, id ASC"
    >>,
    elib_pg:query(Sql, [PostIds, PerPostLimit]).

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
