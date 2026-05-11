-module(moment_report_repo).
%%%
% moment_report repository
%%%
%% Internal persistence detail for the moment_social domain.

-export([upsert/4]).
-export([find_by_id/1]).
-export([list_by_post/2]).
-export([page_admin/3]).
-export([resolve/4]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"moment_report">>).

-spec upsert(integer(), integer(), binary(), binary()) ->
    {ok, integer()} | {error, any()}.
upsert(PostId, ReporterUid, Reason, Desc) ->
    Tb = tablename(),
    Id = elib_tsid:generate(moment_report),
    Sql = <<"INSERT INTO ", Tb/binary,
            " (id, post_id, reporter_uid, reason, description, status, created_at, updated_at)"
            " VALUES ($1, $2, $3, $4, $5, 0, NOW(), NOW())"
            " ON CONFLICT (post_id, reporter_uid) DO UPDATE SET"
            " reason = EXCLUDED.reason,"
            " description = EXCLUDED.description,"
            " status = 0,"
            " handled_by = NULL,"
            " handled_at = NULL,"
            " updated_at = NOW()"
            " RETURNING id">>,
    case elib_pg:one(Sql, [Id, PostId, ReporterUid, Reason, Desc]) of
        {ok, #{<<"id">> := ReportId}} ->
            {ok, ReportId};
        {error, ReasonErr} ->
            {error, ReasonErr}
    end.

-spec find_by_id(integer()) -> map() | {error, any()}.
find_by_id(ReportId) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary, " WHERE id = $1 LIMIT 1">>,
    case elib_pg:one(Sql, [ReportId]) of
        {ok, Row} -> Row;
        {error, Reason} -> {error, Reason}
    end.

-spec list_by_post(integer(), integer()) -> {ok, [map()]} | {error, any()}.
list_by_post(PostId, Limit) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary,
            " WHERE post_id = $1"
            " ORDER BY id DESC LIMIT $2">>,
    elib_pg:query(Sql, [PostId, Limit]).

-spec page_admin(integer(), integer(), integer()) -> {ok, map()} | {error, any()}.
page_admin(Status, Page, Size) ->
    Tb = tablename(),
    Offset = erlang:max(0, (Page - 1) * Size),
    {WhereSql, Params} =
        case Status of
            S when is_integer(S), S >= 0 ->
                {<<" WHERE status = $1">>, [S]};
            _ ->
                {<<>>, []}
        end,
    BaseSql = <<" FROM ", Tb/binary, WhereSql/binary>>,
    ListParam1 = length(Params) + 1,
    ListParam2 = length(Params) + 2,
    Column = <<"id, post_id, reporter_uid, reason, description, status,"
               " handled_by, handled_at, created_at, updated_at">>,
    ListSql = <<"SELECT ", Column/binary, BaseSql/binary,
                " ORDER BY id DESC"
                " LIMIT $", (integer_to_binary(ListParam1))/binary,
                " OFFSET $", (integer_to_binary(ListParam2))/binary>>,
    CountSql = <<"SELECT COUNT(*) AS count", BaseSql/binary>>,
    ListParams = Params ++ [Size, Offset],
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
        {{error, Reason}, _} ->
            {error, Reason};
        {{ok, _}, {error, Reason}} ->
            {error, Reason}
    end.

-spec resolve(integer(), integer(), binary() | undefined, integer()) ->
    {ok, non_neg_integer()} | {error, any()}.
resolve(ReportId, Result, _Note, AdmUid) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET status = $1, handled_by = $2, handled_at = NOW(), updated_at = NOW()"
            " WHERE id = $3">>,
    elib_pg:execute(Sql, [Result, AdmUid, ReportId]).

-spec calc_total_pages(integer(), integer()) -> integer().
calc_total_pages(_Total, Size) when Size =< 0 ->
    0;
calc_total_pages(Total, _Size) when Total =< 0 ->
    0;
calc_total_pages(Total, Size) ->
    (Total + Size - 1) div Size.
