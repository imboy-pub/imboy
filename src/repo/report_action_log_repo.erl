-module(report_action_log_repo).

-export([tablename/0]).
-export([create/4]).
-export([latest_by_report_id/1]).

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"report_action_log">>).

-spec create(integer(), integer(), integer(), binary()) -> {ok, integer()} | {error, any()}.
create(ReportId, OperatorUid, Result, Note) ->
    Tb = tablename(),
    Sql = <<"INSERT INTO ", Tb/binary,
            " (report_id, operator_uid, result, note, created_at)"
            " VALUES ($1, $2, $3, $4, NOW())"
            " RETURNING id">>,
    case elib_pg:one(Sql, [ReportId, OperatorUid, Result, Note]) of
        {ok, #{<<"id">> := Id}} ->
            {ok, Id};
        {error, Reason} ->
            {error, Reason}
    end.

-spec latest_by_report_id(integer()) -> map() | {error, any()}.
latest_by_report_id(ReportId) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary, " WHERE report_id = $1 ORDER BY id DESC LIMIT 1">>,
    case elib_pg:one(Sql, [ReportId]) of
        {ok, Row} ->
            Row;
        {error, Reason} ->
            {error, Reason}
    end.
