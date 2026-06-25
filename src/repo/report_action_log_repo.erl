-module(report_action_log_repo).

-export([create/4]).

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"report_action_log">>).

-spec create(integer(), integer(), integer(), binary()) -> {ok, integer()} | {error, any()}.
create(ReportId, OperatorUid, Result, Note) ->
    Tb = tablename(),
    Id = elib_tsid:generate(report_action_log),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, report_id, operator_uid, result, note, created_at)"
            " VALUES ($1, $2, $3, $4, $5, NOW())">>,
    case elib_pg:execute(Sql, [Id, ReportId, OperatorUid, Result, Note]) of
        {ok, _Count} ->
            {ok, Id};
        {error, Reason} ->
            {error, Reason}
    end.
