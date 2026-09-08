-module(bot_webhook_delivery_repo).

%%%
% Bot 出站 Webhook outbox 仓库（WH-01，migration 00000092）。
% 主路径只写 outbox；worker 拉取执行；attempt 审计不存响应正文与 secret。
%%%

-export([tablename/0, attempt_tablename/0]).
-export([insert/1, get_delivery/1, claim_due/1]).
-export([mark_success/1, mark_retry/4, mark_dead/2]).
-export([insert_attempt/2]).
-export([list_dead/2, replay/1]).
-export([count_by_status/1]).

-include("log.hrl").

tablename() -> elib_pg_sql:public_tablename(<<"bot_delivery">>).
attempt_tablename() -> elib_pg_sql:public_tablename(<<"bot_delivery_attempt">>).

%% @doc 幂等入队：idempotency_key 唯一约束，重复事件返回 duplicate（不重复投递）。
-spec insert(map()) -> {ok, inserted | duplicate} | {error, term()}.
insert(D) ->
    Tb = tablename(),
    #{
        delivery_id := Did,
        bot_id := BotId,
        correlation_id := Corr,
        idempotency_key := Idem
    } = D,
    EventType = maps:get(event_type, D, <<"message">>),
    Payload = maps:get(payload, D, <<"{}">>),
    ReplyCtx = maps:get(reply_context, D, <<>>),
    Host = maps:get(webhook_host, D, <<>>),
    PinnedIP = maps:get(pinned_ip, D, <<>>),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (delivery_id, bot_id, event_type, payload, reply_context,"
            " correlation_id, idempotency_key, webhook_host, pinned_ip)"
            " VALUES ($1,$2,$3,$4::jsonb,$5,$6,$7,$8,$9)"
            " ON CONFLICT (idempotency_key) DO NOTHING">>,
    case
        elib_pg:query(Sql, [
            Did,
            BotId,
            EventType,
            Payload,
            ReplyCtx,
            Corr,
            Idem,
            Host,
            PinnedIP
        ])
    of
        {ok, [_]} ->
            {ok, inserted};
        {ok, []} ->
            {ok, duplicate};
        {ok, N} when is_integer(N), N > 0 -> {ok, inserted};
        {ok, 0} ->
            {ok, duplicate};
        {ok, N, _} when is_integer(N), N > 0 -> {ok, inserted};
        {ok, 0, []} ->
            {ok, duplicate};
        {error, Reason} ->
            ?ERROR_LOG("bot_delivery_repo:insert error ~p~n", [Reason]),
            {error, Reason}
    end.

-spec get_delivery(binary()) -> {ok, map()} | {error, notfound | term()}.
get_delivery(DeliveryId) ->
    Tb = tablename(),
    case
        elib_pg:query(
            <<
                "SELECT delivery_id, bot_id, event_type, payload::text AS payload,"
                " reply_context, correlation_id, idempotency_key, status,"
                " attempt_count, next_retry_at, webhook_host, pinned_ip,"
                " created_at, updated_at FROM ",
                Tb/binary,
                " WHERE delivery_id = $1"
            >>,
            [DeliveryId]
        )
    of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 认领到期交付（pending/retry 且 next_retry_at <= NOW）。
-spec claim_due(non_neg_integer()) -> {ok, [map()]} | {error, term()}.
claim_due(Limit) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = 'pending', updated_at = NOW()"
            " WHERE delivery_id IN ("
            "   SELECT delivery_id FROM ", Tb/binary,
            "   WHERE status IN ('pending','retry') AND next_retry_at <= NOW()"
            "   ORDER BY next_retry_at LIMIT $1"
            " ) RETURNING delivery_id, bot_id, event_type, payload::text AS payload,"
            " reply_context, correlation_id, attempt_count, webhook_host, pinned_ip">>,
    case elib_pg:query(Sql, [Limit]) of
        {ok, Rows} when is_list(Rows) -> {ok, Rows};
        {ok, _N} when is_integer(_N) -> {ok, []};
        {ok, _N, Rows} when is_list(Rows) -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

mark_success(DeliveryId) ->
    set_status_and_bump(DeliveryId, <<"success">>, <<>>).

%% @doc 失败转重试：RetryAfterSecs 秒后再次到期。
mark_retry(DeliveryId, RetryAfterSecs, AttemptNo, _Note) ->
    Tb = tablename(),
    elib_pg:execute(
        <<"UPDATE ", Tb/binary,
            " SET status = 'retry', attempt_count = $2,"
            " next_retry_at = NOW() + ($3 || ' seconds')::interval, updated_at = NOW()"
            " WHERE delivery_id = $1">>,
        [DeliveryId, AttemptNo, integer_to_binary(RetryAfterSecs)]
    ).

%% @doc 进入死信（不再重试）。
mark_dead(DeliveryId, AttemptNo) ->
    set_status_and_bump(DeliveryId, <<"dead">>, <<>>),
    Tb = tablename(),
    elib_pg:execute(
        <<"UPDATE ", Tb/binary,
            " SET attempt_count = $2, updated_at = NOW()"
            " WHERE delivery_id = $1">>,
        [DeliveryId, AttemptNo]
    ).

set_status_and_bump(DeliveryId, Status, _Extra) ->
    Tb = tablename(),
    elib_pg:execute(
        <<"UPDATE ", Tb/binary, " SET status = $2, updated_at = NOW() WHERE delivery_id = $1">>,
        [DeliveryId, Status]
    ).

%% @doc 尝试审计（attempt、class、http status、latency、截断错误）。
insert_attempt(DeliveryId, A) ->
    Tb = attempt_tablename(),
    #{id := Id, attempt_no := No, status_class := Class} = A,
    HttpStatus = maps:get(http_status, A, null),
    Latency = maps:get(latency_ms, A, null),
    Err = maps:get(error_trunc, A, <<>>),
    case
        elib_pg:execute(
            <<"INSERT INTO ", Tb/binary,
                " (id, delivery_id, attempt_no, status_class, http_status, latency_ms, error_trunc)"
                " VALUES ($1,$2,$3,$4,$5,$6,$7)">>,
            [Id, DeliveryId, No, Class, HttpStatus, Latency, Err]
        )
    of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 死信分页（审计）。
list_dead(Page, Size) ->
    Tb = tablename(),
    elib_pg:page_with_total(
        Tb,
        <<
            "delivery_id, bot_id, event_type, correlation_id, attempt_count,"
            " webhook_host, created_at, updated_at"
        >>,
        #{status => <<"dead">>},
        <<"updated_at DESC">>,
        Page,
        Size
    ).

%% @doc 管理员手工重放：仅 dead 可重放；生成新 attempt，delivery_id 不变。
-spec replay(binary()) -> {ok, reused_delivery} | {error, not_dead | notfound | term()}.
replay(DeliveryId) ->
    case get_delivery(DeliveryId) of
        {ok, #{<<"status">> := <<"dead">>}} ->
            Tb = tablename(),
            case
                elib_pg:execute(
                    <<"UPDATE ", Tb/binary,
                        " SET status = 'pending', next_retry_at = NOW(), updated_at = NOW()"
                        " WHERE delivery_id = $1">>,
                    [DeliveryId]
                )
            of
                {ok, _} -> {ok, reused_delivery};
                {error, Reason} -> {error, Reason}
            end;
        {ok, #{<<"status">> := _}} ->
            {error, not_dead};
        {error, notfound} ->
            {error, notfound};
        {error, Reason} ->
            {error, Reason}
    end.

count_by_status(Status) ->
    Tb = tablename(),
    case
        elib_pg:query(
            <<"SELECT count(*) AS n FROM ", Tb/binary, " WHERE status = $1">>,
            [Status]
        )
    of
        {ok, [#{<<"n">> := N}]} -> {ok, N};
        Other -> Other
    end.
