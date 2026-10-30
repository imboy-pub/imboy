-module(c2c_action_receipt_repo).

-export([accept/6, consume/6, reconcile_consumed_tx/2]).

-spec accept(binary(), binary(), integer(), integer(), binary(), binary() | null) ->
    {ok, new | duplicate} | {error, conflict | term()}.
accept(RequestId, OriginalMsgId, ActorUid, PeerUid, Action, ContentHash) ->
    elib_pg:with_tx(fun(Conn) ->
        Sql =
            <<"INSERT INTO public.c2c_action_receipt ",
                "(request_id,original_msg_id,actor_uid,peer_uid,action,content_sha256) ",
                "VALUES ($1,$2,$3,$4,$5,$6) ON CONFLICT (request_id) DO NOTHING">>,
        case
            elib_pg:execute_count(Conn, Sql, [
                RequestId, OriginalMsgId, ActorUid, PeerUid, Action, ContentHash
            ])
        of
            {ok, 1} ->
                {ok, new};
            {ok, 0} ->
                verify_duplicate(
                    Conn, RequestId, OriginalMsgId, ActorUid, PeerUid, Action, ContentHash
                );
            {error, Reason} ->
                throw({rollback, Reason})
        end
    end).

-spec consume(binary(), binary(), integer(), binary(), binary() | null, binary()) ->
    ok | {error, not_found | replayed | term()}.
consume(RequestId, OriginalMsgId, AckUid, Action, ContentHash, PayloadJson) ->
    Result = elib_pg:with_tx(fun(Conn) ->
        case lock_receipt(Conn, RequestId) of
            {ok, #{
                <<"original_msg_id">> := OriginalMsgId,
                <<"actor_uid">> := ActorUid,
                <<"peer_uid">> := PeerUid,
                <<"action">> := Action,
                <<"content_sha256">> := StoredHash,
                <<"consumed_at">> := null
            }} when PeerUid =:= AckUid, StoredHash =:= ContentHash ->
                consume_locked(
                    Conn, RequestId, OriginalMsgId, ActorUid, PeerUid, PayloadJson
                );
            {ok, #{<<"consumed_at">> := ConsumedAt}} when ConsumedAt =/= null ->
                {error, replayed};
            {ok, _} ->
                {error, not_found};
            {error, not_found} ->
                {error, not_found};
            {error, Reason} ->
                throw({rollback, Reason})
        end
    end),
    tx_result(Result).

-spec reconcile_consumed_tx(any(), binary()) -> ok | {error, term()}.
reconcile_consumed_tx(Conn, OriginalMsgId) ->
    Sql =
        <<"UPDATE public.msg_c2c m SET payload=r.result_payload::text, ",
            "server_ts=clock_timestamp() FROM (",
            "SELECT actor_uid,peer_uid,result_payload FROM public.c2c_action_receipt ",
            "WHERE original_msg_id=$1 AND consumed_at IS NOT NULL ",
            "ORDER BY consumed_at DESC,request_id DESC LIMIT 1",
            ") r WHERE m.msg_id=$1 AND m.from_id=r.actor_uid AND m.to_id=r.peer_uid">>,
    case elib_pg:execute_count(Conn, Sql, [OriginalMsgId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

verify_duplicate(Conn, RequestId, OriginalMsgId, ActorUid, PeerUid, Action, ContentHash) ->
    Sql =
        <<"SELECT original_msg_id,actor_uid,peer_uid,action,content_sha256 ",
            "FROM public.c2c_action_receipt WHERE request_id=$1 FOR UPDATE">>,
    case elib_pg:query(Conn, Sql, [RequestId]) of
        {ok, [
            #{
                <<"original_msg_id">> := OriginalMsgId,
                <<"actor_uid">> := ActorUid,
                <<"peer_uid">> := PeerUid,
                <<"action">> := Action,
                <<"content_sha256">> := ContentHash
            }
        ]} ->
            {ok, duplicate};
        {ok, [_]} ->
            {error, conflict};
        {ok, []} ->
            {error, conflict};
        {error, Reason} ->
            throw({rollback, Reason})
    end.

lock_receipt(Conn, RequestId) ->
    Sql =
        <<"SELECT original_msg_id,actor_uid,peer_uid,action,content_sha256,consumed_at ",
            "FROM public.c2c_action_receipt WHERE request_id=$1 FOR UPDATE">>,
    case elib_pg:query(Conn, Sql, [RequestId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

consume_locked(Conn, RequestId, OriginalMsgId, ActorUid, PeerUid, PayloadJson) ->
    FormalSql =
        <<"UPDATE public.msg_c2c SET payload=$1,server_ts=clock_timestamp() ",
            "WHERE msg_id=$2 AND from_id=$3 AND to_id=$4">>,
    StagingSql =
        <<"UPDATE public.msg_store_staging SET payload=$1::jsonb,server_ts=clock_timestamp() ",
            "WHERE type='c2c' AND msg_id=$2 AND from_id=$3 AND to_id=$4 ",
            "AND processed_at IS NULL">>,
    FormalCount = execute_count_or_rollback(
        Conn, FormalSql, [PayloadJson, OriginalMsgId, ActorUid, PeerUid]
    ),
    StagingCount = execute_count_or_rollback(
        Conn, StagingSql, [PayloadJson, OriginalMsgId, ActorUid, PeerUid]
    ),
    case FormalCount + StagingCount of
        0 ->
            {error, not_found};
        _ ->
            ConsumeSql =
                <<"UPDATE public.c2c_action_receipt SET consumed_at=clock_timestamp(), ",
                    "result_payload=$1::jsonb WHERE request_id=$2 AND consumed_at IS NULL">>,
            case elib_pg:execute_count(Conn, ConsumeSql, [PayloadJson, RequestId]) of
                {ok, 1} -> ok;
                {ok, 0} -> throw({rollback, replayed});
                {error, Reason} -> throw({rollback, Reason})
            end
    end.

execute_count_or_rollback(Conn, Sql, Params) ->
    case elib_pg:execute_count(Conn, Sql, Params) of
        {ok, Count} -> Count;
        {error, Reason} -> throw({rollback, Reason})
    end.

tx_result({rollback, Reason}) -> {error, Reason};
tx_result(Result) -> Result.
