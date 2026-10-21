-module(attachment_gc_repo).
-moduledoc "Durable object deletion jobs; completed rows reserve retired object keys.".
-export([enqueue_tx/3, pending/0, lock_tx/2, complete_tx/2, defer/1]).
-export([v5_claim/2, v5_ack/4, v5_reap/2, retry_talk_record_tx/9]).

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

%% ===================================================================
%% W4-02 V5 队列协议封装（迁移 179 SECURITY DEFINER 函数；worker 只可
%% claim/ack/reaper）。fn 内 CAS 租约（SKIP LOCKED、lease 120s、attempts<8、
%% 7 档退避、max_attempts→blocked）——本模块只做参数/结果归一。
%% ===================================================================

-include_lib("epgsql/include/epgsql.hrl").

%% claim 一个 ready/retry_wait job（租约 120s；attempts 已由 fn 递增）。
-spec v5_claim(epgsql:connection() | pid(), binary()) ->
    {ok, map()} | {error, empty | term()}.
v5_claim(Conn, Token) ->
    case
        elib_pg:query(
            Conn,
            <<
                "SELECT out_path, out_bucket, out_storage_driver,"
                " out_storage_namespace, out_storage_container,"
                " out_storage_locator_sha256, out_storage_physical_key, out_attempts"
                " FROM fn_v5_claim_ordinary($1)"
            >>,
            [Token]
        )
    of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, empty};
        {error, R} -> {error, R}
    end.

%% ack 删除结果：ok=done；failed=退避重试/8 次终止（fn 裁决）。
-spec v5_ack(epgsql:connection() | pid(), binary(), binary(), boolean()) ->
    ok | {error, term()}.
v5_ack(Conn, Path, Token, Ok) ->
    case
        elib_pg:query(
            Conn,
            <<"SELECT fn_v5_ack_ordinary($1, $2, $3)">>,
            [Path, Token, Ok]
        )
    of
        {ok, [#{<<"fn_v5_ack_ordinary">> := <<"done">>}]} when Ok =:= true -> ok;
        {ok, [#{<<"fn_v5_ack_ordinary">> := State}]} when
            Ok =:= false, (State =:= <<"retry_wait">> orelse State =:= <<"blocked">>)
        ->
            ok;
        {ok, [#{<<"fn_v5_ack_ordinary">> := State}]} ->
            {error, {ack_rejected, State}};
        {ok, Rows} ->
            {error, {invalid_ack_result, Rows}};
        {error, R} ->
            {error, R}
    end.

%% 过期租约回收（返回回收行数）。
-spec v5_reap(epgsql:connection() | pid(), integer()) -> {ok, integer()} | {error, term()}.
v5_reap(Conn, Limit) ->
    case elib_pg:query(Conn, <<"SELECT fn_v5_reaper_ordinary($1)">>, [Limit]) of
        {ok, [#{<<"fn_v5_reaper_ordinary">> := N}]} when is_integer(N) -> {ok, N};
        {ok, _} -> {ok, 0};
        {error, R} -> {error, R}
    end.

%% Narrow function rechecks authenticated actor relationship and frozen ownership.
retry_talk_record_tx(C, Uid, Rid, Rev, Generation, Key, Digest, Reason, JobId) ->
    elib_pg:query(
        C,
        <<"SELECT fn_talk_record_cleanup_retry($1,$2,$3,$4,$5,$6,$7,$8) AS result">>,
        [Uid, Rid, Rev, Generation, Key, Digest, Reason, JobId]
    ).
