-module(moment_post_repo).
%%%
% moment_post repository
%%%

-export([tablename/0]).
-export([add/1, add/2]).
-export([find_by_id/1, find_any_by_id/1]).
-export([page_feed_candidates/2]).
-export([page_by_author/3]).
-export([soft_delete/2, soft_delete_by_admin/1]).
-export([increment_like_count/2, increment_like_count/3]).
-export([increment_comment_count/2, increment_comment_count/3]).
-export([page_admin/5]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"moment_post">>).

-spec add(map()) -> {ok, integer(), map()} | {error, term()}.
add(Data) ->
    Tb = tablename(),
    elib_pg_sql:parse_result(elib_pg:insert(Tb, Data, <<"RETURNING id">>)).

-spec add(any(), map()) -> {ok, integer(), map()} | {error, term()}.
add(Conn, Data) ->
    Tb = tablename(),
    elib_pg_sql:parse_result(elib_pg:insert(Conn, Tb, Data, <<"RETURNING id">>)).

-spec find_by_id(integer() | binary() | list()) -> map() | {error, any()}.
find_by_id(PostId) ->
    find_by_id(PostId, active).

-spec find_any_by_id(integer() | binary() | list()) -> map() | {error, any()}.
find_any_by_id(PostId) ->
    find_by_id(PostId, all).

-spec find_by_id(integer() | binary() | list(), active | all) -> map() | {error, any()}.
find_by_id(PostId, Scope) when is_binary(PostId); is_list(PostId) ->
    find_by_id(ec_cnv:to_integer(PostId), Scope);
find_by_id(PostId, Scope) when is_integer(PostId), PostId > 0 ->
    Tb = tablename(),
    Sql = case Scope of
        active ->
            <<"SELECT * FROM ", Tb/binary, " WHERE id = $1 AND status = 1 LIMIT 1">>;
        all ->
            <<"SELECT * FROM ", Tb/binary, " WHERE id = $1 LIMIT 1">>
    end,
    case elib_pg:one(Sql, [PostId]) of
        {ok, Row} -> Row;
        {error, Reason} -> {error, Reason}
    end;
find_by_id(_, _Scope) ->
    #{}.

-spec page_feed_candidates(integer(), integer()) -> {ok, [map()]} | {error, any()}.
page_feed_candidates(Cursor, Limit) when is_integer(Cursor), Cursor > 0 ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary,
            " WHERE status = 1 AND id < $1"
            " ORDER BY id DESC LIMIT $2">>,
    elib_pg:query(Sql, [Cursor, Limit]);
page_feed_candidates(_Cursor, Limit) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary,
            " WHERE status = 1"
            " ORDER BY id DESC LIMIT $1">>,
    elib_pg:query(Sql, [Limit]).

-spec page_by_author(integer(), integer(), integer()) -> {ok, [map()]} | {error, any()}.
page_by_author(AuthorUid, Cursor, Limit) when is_integer(Cursor), Cursor > 0 ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary,
            " WHERE author_uid = $1 AND status = 1 AND id < $2"
            " ORDER BY id DESC LIMIT $3">>,
    elib_pg:query(Sql, [AuthorUid, Cursor, Limit]);
page_by_author(AuthorUid, _Cursor, Limit) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary,
            " WHERE author_uid = $1 AND status = 1"
            " ORDER BY id DESC LIMIT $2">>,
    elib_pg:query(Sql, [AuthorUid, Limit]).

-spec soft_delete(integer(), integer()) -> {ok, non_neg_integer()} | {error, any()}.
soft_delete(PostId, AuthorUid) ->
    Tb = tablename(),
    Data = #{status => -1, updated_at => elib_dt:now()},
    Where = <<"id = $1 AND author_uid = $2 AND status = 1">>,
    elib_pg:update(Tb, Data, Where, [PostId, AuthorUid]).

-spec soft_delete_by_admin(integer()) -> {ok, non_neg_integer()} | {error, any()}.
soft_delete_by_admin(PostId) ->
    Tb = tablename(),
    Data = #{status => -1, updated_at => elib_dt:now()},
    Where = <<"id = $1 AND status <> -1">>,
    elib_pg:update(Tb, Data, Where, [PostId]).

-spec increment_like_count(integer(), integer()) -> {ok, non_neg_integer()} | {error, any()}.
increment_like_count(PostId, Delta) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET like_count = GREATEST(like_count + $1, 0), updated_at = CURRENT_TIMESTAMP"
            " WHERE id = $2 AND status = 1">>,
    elib_pg:execute(Sql, [Delta, PostId]).

-spec increment_like_count(any(), integer(), integer()) -> {ok, non_neg_integer()} | {error, any()}.
increment_like_count(Conn, PostId, Delta) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET like_count = GREATEST(like_count + $1, 0), updated_at = CURRENT_TIMESTAMP"
            " WHERE id = $2 AND status = 1">>,
    elib_pg:execute(Conn, Sql, [Delta, PostId]).

-spec increment_comment_count(integer(), integer()) -> {ok, non_neg_integer()} | {error, any()}.
increment_comment_count(PostId, Delta) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET comment_count = GREATEST(comment_count + $1, 0), updated_at = CURRENT_TIMESTAMP"
            " WHERE id = $2 AND status = 1">>,
    elib_pg:execute(Sql, [Delta, PostId]).

-spec increment_comment_count(any(), integer(), integer()) -> {ok, non_neg_integer()} | {error, any()}.
increment_comment_count(Conn, PostId, Delta) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET comment_count = GREATEST(comment_count + $1, 0), updated_at = CURRENT_TIMESTAMP"
            " WHERE id = $2 AND status = 1">>,
    elib_pg:execute(Conn, Sql, [Delta, PostId]).

-spec page_admin(binary() | undefined, integer(), integer(), integer(), integer()) ->
    {ok, map()} | {error, any()}.
page_admin(Keyword, AuthorUid, Status, Page, Size) ->
    Tb = tablename(),
    {WhereSql, Params} = build_admin_where(Keyword, AuthorUid, Status),
    BaseSql = <<" FROM ", Tb/binary, WhereSql/binary>>,
    Offset = erlang:max(0, (Page - 1) * Size),

    ListParam1 = length(Params) + 1,
    ListParam2 = length(Params) + 2,
    Column = <<"id, author_uid, content, media, visibility, allow_comment,"
               " like_count, comment_count, status, created_at, updated_at">>,
    ListSql = <<"SELECT ", Column/binary, BaseSql/binary,
                " ORDER BY id DESC"
                " LIMIT $", (integer_to_binary(ListParam1))/binary,
                " OFFSET $", (integer_to_binary(ListParam2))/binary>>,
    ListParams = Params ++ [Size, Offset],

    CountSql = <<"SELECT COUNT(*) AS count", BaseSql/binary>>,
    case {elib_pg:query(ListSql, ListParams), elib_pg:one(CountSql, Params)} of
        {{ok, Rows}, {ok, #{<<"count">> := Total0}}} ->
            Total = ec_cnv:to_integer(Total0),
            {ok, #{
                list => Rows,
                total => Total,
                page => Page,
                size => Size,
                total_pages => calc_total_pages(Total, Size)
            }};
        {{ok, _Rows}, {error, Reason}} ->
            {error, Reason};
        {{error, Reason}, _} ->
            {error, Reason}
    end.

-spec build_admin_where(binary() | undefined, integer(), integer()) -> {binary(), [term()]}.
build_admin_where(Keyword0, AuthorUid, Status) ->
    {Conds1, Params1, Next1} =
        case normalize_keyword(Keyword0) of
            <<>> ->
                {[], [], 1};
            Keyword ->
                Cond1 = <<"content ILIKE $", (integer_to_binary(1))/binary>>,
                {[Cond1], [<<"%", Keyword/binary, "%">>], 2}
        end,
    {Conds2, Params2, Next2} =
        case AuthorUid > 0 of
            true ->
                Cond2 = <<"author_uid = $", (integer_to_binary(Next1))/binary>>,
                {[Cond2 | Conds1], Params1 ++ [AuthorUid], Next1 + 1};
            false ->
                {Conds1, Params1, Next1}
        end,
    {Conds3, Params3} =
        case Status of
            S when is_integer(S), S >= -1 ->
                Cond3 = <<"status = $", (integer_to_binary(Next2))/binary>>,
                {[Cond3 | Conds2], Params2 ++ [S]};
            _ ->
                {Conds2, Params2}
        end,
    WhereSql = case Conds3 of
        [] ->
            <<>>;
        _ ->
            JoinCond = iolist_to_binary(lists:join(<<" AND ">>, lists:reverse(Conds3))),
            <<" WHERE ", JoinCond/binary>>
    end,
    {WhereSql, Params3}.

-spec normalize_keyword(binary() | undefined) -> binary().
normalize_keyword(undefined) ->
    <<>>;
normalize_keyword(Keyword) when is_binary(Keyword) ->
    list_to_binary(string:trim(binary_to_list(Keyword)));
normalize_keyword(Keyword) when is_list(Keyword) ->
    list_to_binary(string:trim(Keyword));
normalize_keyword(_) ->
    <<>>.

-spec calc_total_pages(integer(), integer()) -> integer().
calc_total_pages(_Total, Size) when Size =< 0 ->
    0;
calc_total_pages(Total, _Size) when Total =< 0 ->
    0;
calc_total_pages(Total, Size) ->
    (Total + Size - 1) div Size.
