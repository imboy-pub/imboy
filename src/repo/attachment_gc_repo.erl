-module(attachment_gc_repo).
-moduledoc "Durable object deletion jobs; completed rows reserve retired object keys.".
-export([enqueue_tx/3, pending/0, lock_tx/2, complete_tx/2, defer/1]).

enqueue_tx(Conn, Path, Bucket) ->
    case
        elib_pg:query(
            Conn,
            <<"SELECT pg_advisory_xact_lock(hashtextextended($1,168))">>,
            [Path]
        )
    of
        {ok, _} -> enqueue_locked_tx(Conn, Path, Bucket);
        {error, Reason} -> {error, Reason}
    end.

enqueue_locked_tx(Conn, Path, Bucket) ->
    case
        elib_pg:execute(
            Conn,
            <<"INSERT INTO attachment_object_deletion(path,bucket) VALUES($1,$2)">>,
            [Path, Bucket]
        )
    of
        {ok, 1} -> ok;
        {error, Reason} -> {error, Reason}
    end.

pending() ->
    elib_pg:query(
        <<
            "SELECT path FROM attachment_object_deletion"
            " WHERE completed_at IS NULL AND next_retry_at<=NOW()"
            " ORDER BY next_retry_at,created_at,path LIMIT 500"
        >>,
        []
    ).

lock_tx(Conn, Path) ->
    elib_pg:query(
        Conn,
        <<
            "SELECT path,bucket FROM attachment_object_deletion"
            " WHERE path=$1 AND completed_at IS NULL AND next_retry_at<=NOW()"
            " FOR UPDATE SKIP LOCKED"
        >>,
        [Path]
    ).

complete_tx(Conn, Path) ->
    case
        elib_pg:execute(
            Conn,
            <<
                "UPDATE attachment_object_deletion"
                " SET completed_at=NOW() WHERE path=$1 AND completed_at IS NULL"
            >>,
            [Path]
        )
    of
        {ok, 1} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% Persist retry delay after rollback so failing oldest jobs cannot monopolize scans.
defer(Path) ->
    elib_pg:execute(
        <<
            "UPDATE attachment_object_deletion SET attempts=attempts+1,"
            " next_retry_at=NOW()+LEAST(3600,60*(attempts+1))*INTERVAL '1 second'"
            " WHERE path=$1 AND completed_at IS NULL"
        >>,
        [Path]
    ).
