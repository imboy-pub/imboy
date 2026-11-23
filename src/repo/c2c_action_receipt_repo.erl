-module(c2c_action_receipt_repo).

-export([accept/6, accept_tx/7, consume/6, lock_original_tx/2, reconcile_consumed_tx/2]).

-spec accept(binary(), binary(), integer(), integer(), binary(), binary() | null) ->
    {ok, new | duplicate} | {error, conflict | term()}.
accept(RequestId, OriginalMsgId, ActorUid, PeerUid, Action, ContentHash) ->
    tx_result(
        elib_pg:with_tx(fun(Conn) ->
            accept_tx(Conn, RequestId, OriginalMsgId, ActorUid, PeerUid, Action, ContentHash)
        end)
    ).

accept_tx(Conn, RequestId, OriginalMsgId, ActorUid, PeerUid, Action, ContentHash) ->
    ok = lock_original_tx(Conn, OriginalMsgId),
    %% BE-04 撤回终态守卫：该 original 已被撤回消费（权威终态）后，
    %% 任何新的编辑/撤回受理一律显式拒绝——正文不得复活。
    case consumed_revoke_exists(Conn, OriginalMsgId) of
        true ->
            {error, revoked};
        false ->
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
                    finish_accept(Conn, OriginalMsgId, ActorUid, PeerUid, Action, {ok, new});
                {ok, 0} ->
                    Duplicate = verify_duplicate(
                        Conn, RequestId, OriginalMsgId, ActorUid, PeerUid, Action, ContentHash
                    ),
                    finish_accept(Conn, OriginalMsgId, ActorUid, PeerUid, Action, Duplicate);
                {error, Reason} ->
                    throw({rollback, Reason})
            end
    end.

-spec lock_original_tx(any(), binary()) -> ok.
lock_original_tx(Conn, OriginalMsgId) ->
    Lock = <<"c2c_forward_revoke:", OriginalMsgId/binary>>,
    case elib_pg:query(Conn, <<"SELECT pg_advisory_xact_lock(hashtextextended($1,0))">>, [Lock]) of
        {ok, _} -> ok;
        {error, Reason} -> throw({rollback, {revoke_lock_failed, Reason}})
    end.

finish_accept(Conn, OriginalMsgId, ActorUid, PeerUid, <<"revoke">>, {ok, _} = Accepted) ->
    case personal_history_repo:mark_revoked_tx(Conn, ActorUid, PeerUid, OriginalMsgId) of
        ok -> Accepted;
        {error, Reason} -> throw({rollback, {history_tombstone_failed, Reason}})
    end;
finish_accept(_Conn, _OriginalMsgId, _ActorUid, _PeerUid, _Action, Result) ->
    Result.

-spec consume(binary(), binary(), integer(), binary(), binary() | null, binary()) ->
    ok | {error, not_found | replayed | term()}.
consume(RequestId, OriginalMsgId, AckUid, Action, ContentHash, PayloadJson) ->
    Result = elib_pg:with_tx(fun(Conn) ->
        %% BE-04：与 accept 同键的 per-original 事务级 advisory lock——
        %% 受理序与消费序在库内全串行，终态守卫与 fence 判定无竞态。
        ok = lock_original_tx(Conn, OriginalMsgId),
        case lock_receipt(Conn, RequestId) of
            {ok, #{
                <<"original_msg_id">> := OriginalMsgId,
                <<"actor_uid">> := ActorUid,
                <<"peer_uid">> := PeerUid,
                <<"action">> := Action,
                <<"content_sha256">> := StoredHash,
                <<"consumed_at">> := null
            }} when PeerUid =:= AckUid, StoredHash =:= ContentHash ->
                case edit_consume_guards(Conn, Action, OriginalMsgId, RequestId) of
                    ok ->
                        consume_locked(
                            Conn, RequestId, OriginalMsgId, ActorUid, PeerUid, PayloadJson
                        );
                    {error, _} = GuardError ->
                        GuardError
                end;
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
            "server_ts=clock_timestamp(), action_version=r.action_version FROM (",
            "SELECT DISTINCT ON (actor_uid,peer_uid) actor_uid,peer_uid,result_payload, ",
            "count(*) OVER (PARTITION BY actor_uid,peer_uid) AS action_version ",
            "FROM public.c2c_action_receipt ",
            "WHERE original_msg_id=$1 AND consumed_at IS NOT NULL ",
            "ORDER BY actor_uid,peer_uid,consumed_at DESC,request_id DESC",
            ") r WHERE m.msg_id=$1 AND m.from_id=r.actor_uid AND m.to_id=r.peer_uid ",
            "AND m.action_version<=r.action_version">>,
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
        <<"UPDATE public.msg_c2c SET payload=$1,server_ts=clock_timestamp(),",
            "action_version=action_version+1 ", "WHERE msg_id=$2 AND from_id=$3 AND to_id=$4">>,
    StagingSql =
        <<"UPDATE public.msg_store_staging SET payload=$1::jsonb,server_ts=clock_timestamp() ",
            "WHERE type='c2c' AND msg_id=$2 AND from_id=$3 AND to_id=$4 ",
            "AND processed_at IS NULL">>,
    %% The staging worker completes staging before it locks the formal row.
    StagingCount = execute_count_or_rollback(
        Conn, StagingSql, [PayloadJson, OriginalMsgId, ActorUid, PeerUid]
    ),
    FormalCount = execute_count_or_rollback(
        Conn, FormalSql, [PayloadJson, OriginalMsgId, ActorUid, PeerUid]
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

%% BE-04 编辑消费守卫（仅 edit；revoke 是权威终态动作，不受 fence 约束）：
%% ① 撤回终态：已消费的撤回存在时，编辑消费一律 {error, revoked}（正文不复活）；
%% ② 受理序 fence：同 original 存在受理序更晚的动作时，更早受理的编辑 ACK
%%    视为过期 {error, stale}——权威最新状态 = 受理序最新动作，冲突显式返回
%%    不静默覆盖。两判定均在 per-original advisory lock 串行域内，无竞态。
edit_consume_guards(Conn, Action, OriginalMsgId, RequestId) ->
    case Action of
        <<"edit">> ->
            case consumed_revoke_exists(Conn, OriginalMsgId) of
                true ->
                    {error, revoked};
                false ->
                    FenceSql =
                        <<
                            "SELECT 1 FROM public.c2c_action_receipt "
                            "WHERE original_msg_id=$1 AND request_id<>$2 "
                            "AND (accepted_at, request_id) > "
                            "(SELECT accepted_at, request_id FROM public.c2c_action_receipt "
                            "WHERE request_id=$2) LIMIT 1"
                        >>,
                    case elib_pg:query(Conn, FenceSql, [OriginalMsgId, RequestId]) of
                        {ok, []} -> ok;
                        {ok, [_]} -> {error, stale};
                        {error, Reason} -> throw({rollback, Reason})
                    end
            end;
        _ ->
            ok
    end.

consumed_revoke_exists(Conn, OriginalMsgId) ->
    Sql =
        <<
            "SELECT 1 FROM public.c2c_action_receipt "
            "WHERE original_msg_id=$1 AND action='revoke' AND consumed_at IS NOT NULL LIMIT 1"
        >>,
    case elib_pg:query(Conn, Sql, [OriginalMsgId]) of
        {ok, []} -> false;
        {ok, [_]} -> true;
        {error, Reason} -> throw({rollback, Reason})
    end.

tx_result({rollback, Reason}) -> {error, Reason};
tx_result(Result) -> Result.
