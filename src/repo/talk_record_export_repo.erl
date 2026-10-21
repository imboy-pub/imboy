-module(talk_record_export_repo).
-moduledoc "面聊导出元数据与异步任务关联；正文仅在附件系统保存。".
-export([
    insert_queued_tx/2,
    find_tx/3,
    find_by_job_tx/2,
    set_job_tx/3,
    mark_ready_tx/4,
    mark_failed_tx/2,
    expire_tx/2,
    sweep_expired_tx/1,
    lock_record_tx/2,
    grant_export_tx/3,
    lock_job_tx/2,
    artifact_tx/2,
    set_artifact_tx/3,
    due_records_tx/1,
    expired_artifacts_tx/2
]).

-define(COLUMNS, <<
    "e.id,e.record_id,e.requester_id,e.format,e.include_audio,"
    "e.attachment_id,e.job_id,e.state,e.expires_at,e.created_at,e.updated_at"
>>).

insert_queued_tx(Conn, #{id := Id, record_id := Rid, requester_id := Uid, format := Format}) ->
    Sql = <<
        "INSERT INTO talk_record_export(id,record_id,requester_id,format,include_audio,"
        "state,expires_at) VALUES($1,$2,$3,$4,false,'queued',now()+interval '1 day')"
    >>,
    case elib_pg:execute(Conn, Sql, [Id, Rid, Uid, Format]) of
        {ok, 1} -> {ok, Id};
        Error -> Error
    end.

find_tx(Conn, Rid, Eid) ->
    one(
        Conn,
        <<"SELECT ", ?COLUMNS/binary,
            " FROM talk_record_export e"
            " WHERE e.record_id=$1 AND e.id=$2">>,
        [Rid, Eid]
    ).

find_by_job_tx(Conn, JobId) ->
    one(Conn, <<"SELECT ", ?COLUMNS/binary, " FROM talk_record_export e WHERE e.job_id=$1">>, [
        JobId
    ]).

set_job_tx(Conn, Eid, JobId) ->
    update_one(
        Conn,
        <<
            "UPDATE talk_record_export SET job_id=$2 WHERE id=$1"
            " AND state='queued' AND job_id IS NULL RETURNING id"
        >>,
        [Eid, JobId]
    ).

set_artifact_tx(Conn, Eid, Aid) ->
    update_one(
        Conn,
        <<
            "UPDATE talk_record_export SET attachment_id=$2,updated_at=now()"
            " WHERE id=$1 AND state='queued' RETURNING id"
        >>,
        [Eid, Aid]
    ).

mark_ready_tx(Conn, Eid, Aid, Ttl) when is_integer(Aid) ->
    one(
        Conn,
        <<
            "UPDATE talk_record_export e SET state='ready',attachment_id=$2,"
            "expires_at=now()+make_interval(secs=>$3),updated_at=now()"
            " WHERE e.id=$1 AND e.state='queued' RETURNING ",
            ?COLUMNS/binary
        >>,
        [Eid, Aid, Ttl]
    ).

mark_failed_tx(Conn, Eid) ->
    case
        elib_pg:execute(
            Conn,
            <<
                "UPDATE talk_record_export SET state='failed',updated_at=now()"
                " WHERE id=$1 AND state='queued'"
            >>,
            [Eid]
        )
    of
        {ok, _} -> ok;
        Error -> Error
    end.

expire_tx(Conn, Eid) ->
    one(
        Conn,
        <<
            "UPDATE talk_record_export e SET state='expired',updated_at=now()"
            " WHERE e.id=$1 AND e.state IN ('ready','failed','queued')"
            " AND e.expires_at<=now() RETURNING ",
            ?COLUMNS/binary
        >>,
        [Eid]
    ).

%% 元数据过期；引用释放和通用删除意图由 logic 协调，不在仓储内删对象。
sweep_expired_tx(Conn) ->
    elib_pg:execute(
        Conn,
        <<
            "UPDATE talk_record_export SET state='expired',updated_at=now()"
            " WHERE state IN ('ready','failed','queued') AND expires_at<=now()"
        >>,
        []
    ).

lock_record_tx(Conn, Rid) ->
    one(Conn, <<"SELECT * FROM talk_record WHERE id=$1 AND deleted_at IS NULL FOR UPDATE">>, [Rid]).

grant_export_tx(Conn, Rid, Uid) ->
    case
        elib_pg:query(
            Conn,
            <<
                "SELECT allow_export FROM talk_record_grant WHERE record_id=$1"
                " AND grantee_user_id=$2 AND revoked_at IS NULL AND (expires_at IS NULL OR expires_at>now())"
            >>,
            [Rid, Uid]
        )
    of
        {ok, [#{<<"allow_export">> := true}]} -> {ok, true};
        {ok, _} -> {ok, false};
        Error -> Error
    end.

lock_job_tx(Conn, #{<<"id">> := Id, <<"lease_token">> := Token}) ->
    one(
        Conn,
        <<
            "SELECT * FROM talk_record_job WHERE id=$1 AND state='running'"
            " AND lease_token=$2 AND lease_until>clock_timestamp() FOR UPDATE"
        >>,
        [Id, Token]
    ).

artifact_tx(Conn, Aid) ->
    one(Conn, <<"SELECT * FROM attachment WHERE id=$1 AND lifecycle_state='live'">>, [Aid]).

due_records_tx(Conn) ->
    elib_pg:query(
        Conn,
        <<
            "SELECT DISTINCT e.record_id FROM talk_record_export e"
            " JOIN talk_record_attachment_ref r ON r.attachment_id=e.attachment_id"
            " WHERE e.expires_at<=now() AND r.state='bound' ORDER BY e.record_id LIMIT 100"
        >>,
        []
    ).

expired_artifacts_tx(Conn, Rid) ->
    elib_pg:query(
        Conn,
        <<
            "SELECT DISTINCT e.id,e.attachment_id,a.path FROM talk_record_export e"
            " JOIN attachment a ON a.id=e.attachment_id JOIN talk_record_attachment_ref r"
            " ON r.attachment_id=a.id AND r.record_id=e.record_id WHERE e.record_id=$1"
            " AND e.expires_at<=now() AND r.state='bound' ORDER BY e.id"
        >>,
        [Rid]
    ).

one(Conn, Sql, Args) ->
    case elib_pg:query(Conn, Sql, Args) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {ok, _} -> {error, ambiguous_export};
        Error -> Error
    end.

update_one(Conn, Sql, Args) ->
    case one(Conn, Sql, Args) of
        {ok, _} -> ok;
        Error -> Error
    end.
