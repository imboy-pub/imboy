-module(bot_webhook_delivery_repo).

%%%
% Bot 出站 Webhook outbox 仓库（WH-01，migration 00000092）。
% 主路径只写 outbox；worker 拉取执行；attempt 审计不存响应正文与 secret。
%%%

-export([tablename/0, attempt_tablename/0]).
-export([insert/1, get_delivery/1, claim_due/1, claim_due_tx/2]).
-export([consume_reply_context/4]).
-export([mark_success/1, mark_success/2, mark_retry/4, mark_dead/2]).
-export([insert_attempt/2]).
-export([list_dead/2, replay/1]).
-export([count_by_status/1]).

-include("log.hrl").

tablename() -> elib_pg_sql:public_tablename(<<"bot_delivery">>).
attempt_tablename() -> elib_pg_sql:public_tablename(<<"bot_delivery_attempt">>).

%% @doc 幂等入队：idempotency_key 唯一约束，重复事件返回 duplicate（不重复投递）。
-spec insert(map()) -> {ok, inserted | duplicate} | {error, term()}.
insert(D) ->
    case elib_pg:with_tx(fun(Conn) -> insert_tx(Conn, D) end) of
        {rollback, Reason} -> {error, Reason};
        Result -> Result
    end.

insert_tx(Conn, D) ->
    Tb = tablename(),
    #{
        delivery_id := Did,
        bot_id := BotId,
        correlation_id := Corr,
        idempotency_key := Idem,
        webhook_url := WebhookUrl,
        webhook_host := Host,
        pinned_ip := PinnedIP
    } = D,
    EventType = maps:get(event_type, D, <<"message">>),
    Payload = maps:get(payload, D, <<"{}">>),
    ReplyCtx = maps:get(reply_context, D, <<>>),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (delivery_id, bot_id, event_type, payload, reply_context,"
            " correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip)"
            " VALUES ($1,$2,$3,$4::jsonb,$5,$6,$7,$8,$9,$10)"
            " ON CONFLICT (idempotency_key) DO NOTHING">>,
    case
        elib_pg:query(Conn, Sql, [
            Did,
            BotId,
            EventType,
            Payload,
            ReplyCtx,
            Corr,
            Idem,
            WebhookUrl,
            Host,
            PinnedIP
        ])
    of
        {ok, [_]} ->
            inserted_with_audit(Conn, Corr, Did);
        {ok, []} ->
            {ok, duplicate};
        {ok, N} when is_integer(N), N > 0 -> inserted_with_audit(Conn, Corr, Did);
        {ok, 0} ->
            {ok, duplicate};
        {ok, N, _} when is_integer(N), N > 0 -> inserted_with_audit(Conn, Corr, Did);
        {ok, 0, []} ->
            {ok, duplicate};
        {error, Reason} ->
            ?ERROR_LOG("bot_delivery_repo:insert error ~p~n", [Reason]),
            {error, Reason}
    end.

inserted_with_audit(Conn, Corr, DeliveryId) ->
    _ = agent_hub_audit_repo:record_delivery_tx(
        Conn, Corr, DeliveryId, <<"pending">>
    ),
    {ok, inserted}.

-spec get_delivery(binary()) -> {ok, map()} | {error, notfound | term()}.
get_delivery(DeliveryId) ->
    Tb = tablename(),
    case
        elib_pg:query(
            <<
                "SELECT delivery_id, bot_id, event_type, payload::text AS payload,"
                " reply_context, correlation_id, idempotency_key, status,"
                " attempt_count, next_retry_at, webhook_url, webhook_host, pinned_ip,"
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
%% 池化入口（worker 用）；事务化实现在 claim_due_tx/2（同一段 SQL，无第二套认领）。
-spec claim_due(non_neg_integer()) -> {ok, [map()]} | {error, term()}.
claim_due(Limit) ->
    case elib_pg:with_tx(fun(Conn) -> claim_due_tx(Conn, Limit) end) of
        {rollback, Reason} -> {error, Reason};
        Result -> Result
    end.

%% @doc 事务化认领（FULL-03：并发 claim 判定需要调用方自持事务/连接）。
%% 认领语义（单条 SQL = 单次原子认领，不拆分）：
%%   * SELECT ... FOR UPDATE SKIP LOCKED 只锁本事务可见的到期行——并发 claim
%%     双方不会拿到同一行（另一方可跳过的行已被锁）；
%%   * UPDATE 把 next_retry_at 前推 60 秒（租约）——即使两个事务不重叠，
%%     后到的 claim 也看不到已前推的行（next_retry_at > NOW()）；
%%   * ewh_claimed_at 仅对企业行（bot_id LIKE 'eapp:%'）打点，bot 域行保持 NULL。
-spec claim_due_tx(any(), non_neg_integer()) -> {ok, [map()]} | {error, term()}.
claim_due_tx(Conn, Limit) ->
    Tb = tablename(),
    Sql =
        <<
            "WITH due AS ("
            " SELECT delivery_id FROM ",
            Tb/binary,
            " WHERE status IN ('pending','retry') AND next_retry_at <= NOW()"
            " ORDER BY next_retry_at LIMIT $1 FOR UPDATE SKIP LOCKED"
            ") UPDATE ",
            Tb/binary,
            " AS delivery"
            " SET status = 'pending', next_retry_at = NOW() + INTERVAL '60 seconds',"
            " ewh_claimed_at = CASE WHEN delivery.bot_id LIKE 'eapp:%' THEN NOW()"
            " ELSE delivery.ewh_claimed_at END,"
            " updated_at = NOW()"
            " FROM due WHERE delivery.delivery_id = due.delivery_id"
            " RETURNING delivery.delivery_id, delivery.bot_id, delivery.event_type,"
            " delivery.payload::text AS payload, delivery.reply_context,"
            " delivery.correlation_id, delivery.attempt_count, delivery.webhook_url,"
            " delivery.webhook_host, delivery.pinned_ip,"
            " delivery.ewh_owner_organization_id, delivery.ewh_owner_application_id,"
            " delivery.ewh_replay_of, delivery.ewh_endpoint_generation,"
            " delivery.ewh_ledger_version"
        >>,
    case elib_pg:query(Conn, Sql, [Limit]) of
        {ok, Rows} when is_list(Rows) -> {ok, Rows};
        {ok, _N} when is_integer(_N) -> {ok, []};
        {ok, _N, Rows} when is_list(Rows) -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

mark_success(DeliveryId) ->
    update_status(DeliveryId, <<"success">>, undefined, undefined).

%% @doc worker 成功落账时同步记录实际 attempt 序号。
mark_success(DeliveryId, AttemptNo) ->
    update_status(DeliveryId, <<"success">>, AttemptNo, undefined).

%% @doc 失败转重试：RetryAfterSecs 秒后再次到期。
mark_retry(DeliveryId, RetryAfterSecs, AttemptNo, _Note) ->
    update_status(DeliveryId, <<"retry">>, AttemptNo, RetryAfterSecs).

%% @doc 进入死信（不再重试）。
mark_dead(DeliveryId, AttemptNo) ->
    update_status(DeliveryId, <<"dead">>, AttemptNo, undefined).

update_status(DeliveryId, Status, AttemptNo, RetryAfterSecs) ->
    Tb = tablename(),
    {Sql, Params} = status_sql(Tb, DeliveryId, Status, AttemptNo, RetryAfterSecs),
    case
        elib_pg:with_tx(fun(Conn) ->
            case elib_pg:execute(Conn, Sql, Params) of
                {ok, _} = Ok ->
                    _ = agent_hub_audit_repo:set_delivery_status_tx(Conn, DeliveryId, Status),
                    Ok;
                {error, Reason} ->
                    throw({abort_tx, Reason})
            end
        end)
    of
        {rollback, Reason} -> {error, Reason};
        Result -> Result
    end.

status_sql(Tb, DeliveryId, <<"retry">>, AttemptNo, RetryAfterSecs) ->
    {
        <<"UPDATE ", Tb/binary,
            " SET status = 'retry', attempt_count = $2,"
            " next_retry_at = NOW() + ($3 || ' seconds')::interval, updated_at = NOW()"
            " WHERE delivery_id = $1">>,
        [DeliveryId, AttemptNo, integer_to_binary(RetryAfterSecs)]
    };
status_sql(Tb, DeliveryId, Status, undefined, _RetryAfterSecs) ->
    {<<"UPDATE ", Tb/binary, " SET status = $2, updated_at = NOW() WHERE delivery_id = $1">>, [
        DeliveryId, Status
    ]};
status_sql(Tb, DeliveryId, Status, AttemptNo, _RetryAfterSecs) ->
    {
        <<"UPDATE ", Tb/binary,
            " SET status = $2, attempt_count = $3, updated_at = NOW()"
            " WHERE delivery_id = $1">>,
        [DeliveryId, Status, AttemptNo]
    }.

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
    case elib_pg:with_tx(fun(Conn) -> replay_tx(Conn, DeliveryId) end) of
        {rollback, Reason} -> {error, Reason};
        Result -> Result
    end.

replay_tx(Conn, DeliveryId) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = 'pending', next_retry_at = NOW(), updated_at = NOW()"
            " WHERE delivery_id = $1 AND status = 'dead' RETURNING delivery_id">>,
    case elib_pg:query(Conn, Sql, [DeliveryId]) of
        {ok, [_]} -> replayed_with_audit(Conn, DeliveryId);
        {ok, _N, [_]} -> replayed_with_audit(Conn, DeliveryId);
        {ok, []} -> replay_not_updated(Conn, DeliveryId);
        {ok, 0} -> replay_not_updated(Conn, DeliveryId);
        {ok, 0, []} -> replay_not_updated(Conn, DeliveryId);
        {error, Reason} -> throw({abort_tx, Reason});
        Other -> throw({abort_tx, {unexpected_replay_result, Other}})
    end.

replayed_with_audit(Conn, DeliveryId) ->
    _ = agent_hub_audit_repo:set_delivery_status_tx(Conn, DeliveryId, <<"pending">>),
    {ok, reused_delivery}.

replay_not_updated(Conn, DeliveryId) ->
    Tb = tablename(),
    case
        elib_pg:query(Conn, <<"SELECT 1 FROM ", Tb/binary, " WHERE delivery_id = $1">>, [
            DeliveryId
        ])
    of
        {ok, []} -> {error, notfound};
        {ok, 0} -> {error, notfound};
        {ok, [_]} -> {error, not_dead};
        {ok, _N, [_]} -> {error, not_dead};
        {error, Reason} -> throw({abort_tx, Reason});
        Other -> throw({abort_tx, {unexpected_replay_lookup_result, Other}})
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

%% @doc 按 delivery 主键、Bot、correlation 与 token 原子消费 reply context。
%% 成功后清空独立列；payload 保留原始投递审计。第二次消费及伪造输入同形 notfound。
-spec consume_reply_context(binary(), binary(), integer(), binary()) ->
    {ok, consumed} | {error, notfound | term()}.
consume_reply_context(DeliveryId, Token, BotId, Corr) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET reply_context = '', updated_at = NOW()"
            " WHERE delivery_id = $1 AND reply_context = $2"
            " AND bot_id = $3 AND correlation_id = $4"
            " RETURNING delivery_id">>,
    case elib_pg:query(Sql, [DeliveryId, Token, integer_to_binary(BotId), Corr]) of
        {ok, [_]} -> {ok, consumed};
        {ok, []} -> {error, notfound};
        {ok, N} when is_integer(N), N > 0 -> {ok, consumed};
        {ok, 0} -> {error, notfound};
        {ok, N, _} when is_integer(N), N > 0 -> {ok, consumed};
        {ok, 0, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.
