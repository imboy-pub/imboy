%%% @doc 客服会话的 PG 实现（`cs_store_port` 的 session 段；A02 的 DB 裁决点）。
%%%
%%% 铁律 6：session 级 SQL 全部同语句带 `organization_id` + `workspace_id`。
%%% 铁律 7（A02）：`claim_session/7` 在**单事务**内完成——
%%%   1. `SELECT ... FOR UPDATE` 锁 seat 行（同坐席并发 claim 在此串行化）；
%%%   2. 复核 `enabled` 与 active 计数 < `max_concurrent`（检查与写入同锁，窗口关闭）；
%%%   3. `(status='queued', version=expected)` 条件 UPDATE（同会话并发恰好一个 1 行）；
%%%   4. 审计事件同事务落库（状态推进与证据原子）。
%%% 业务失败以 `throw({rollback, {error, _}})` 回滚（elib_pg 捕获该形状，不重试）。
%%%
%%% 会话不存任何消息副本：conversation/message 真源在 enterprise 侧（A03）。
-module(cs_pg_session).

-export([
    insert_session/3,
    fetch_session/3,
    claim_session/7,
    transfer_session/7,
    close_session/7,
    rate_session/7,
    list_sessions_for_contact/3,
    list_sessions_page/5,
    sql_statements/0
]).

-define(SESSION_KEYS, [
    id,
    organization_id,
    workspace_id,
    contact_id,
    conversation_id,
    business_identity_id,
    visit_token_id,
    status,
    rating,
    rating_at,
    queued_at,
    claimed_at,
    closed_at,
    close_reason,
    version,
    updated_at
]).

-define(SQL_INSERT_SESSION, <<
    "INSERT INTO customer_service_session"
    " (id, organization_id, workspace_id, contact_id, conversation_id, visit_token_id,"
    "  queued_at, created_by_user_id)"
    " VALUES ($1, $2, $3, $4, $5, $6, COALESCE(to_timestamp($7), CURRENT_TIMESTAMP), $8)"
>>).

-define(SQL_FETCH_SESSION, <<
    "SELECT s.id, s.organization_id, s.workspace_id, s.contact_id, s.conversation_id,"
    "       s.business_identity_id, s.visit_token_id, s.status, s.rating,"
    "       extract(epoch from s.rating_at)::bigint AS rating_at,"
    "       extract(epoch from s.queued_at)::bigint AS queued_at,"
    "       extract(epoch from s.claimed_at)::bigint AS claimed_at,"
    "       extract(epoch from s.closed_at)::bigint AS closed_at,"
    "       s.close_reason, s.version,"
    "       extract(epoch from s.updated_at)::bigint AS updated_at"
    "  FROM customer_service_session s"
    "  JOIN workspace w ON w.organization_id = $1 AND w.id = $2"
    " WHERE s.organization_id = $1 AND s.workspace_id = $2 AND s.id = $3"
>>).

-define(SQL_LOCK_SEAT, <<
    "SELECT enabled, max_concurrent FROM customer_service_seat"
    " WHERE organization_id = $1 AND business_identity_id = $2 FOR UPDATE"
>>).

-define(SQL_COUNT_ACTIVE, <<
    "SELECT count(*) AS n FROM customer_service_session"
    " WHERE organization_id = $1 AND business_identity_id = $2 AND status = 'active'"
>>).

-define(SQL_CLAIM_UPDATE, <<
    "UPDATE customer_service_session"
    "   SET status = 'active', business_identity_id = $4,"
    "       claimed_at = to_timestamp($5), version = version + 1,"
    "       updated_at = to_timestamp($5)"
    " WHERE organization_id = $1 AND workspace_id = $2 AND id = $3"
    "   AND status = 'queued' AND version = $6"
>>).

-define(SQL_TRANSFER_UPDATE, <<
    "UPDATE customer_service_session"
    "   SET business_identity_id = $4, version = version + 1, updated_at = to_timestamp($6)"
    " WHERE organization_id = $1 AND workspace_id = $2 AND id = $3"
    "   AND status = 'active' AND version = $5"
    "   AND EXISTS (SELECT 1 FROM customer_service_seat cs"
    "                WHERE cs.organization_id = $1 AND cs.business_identity_id = $4)"
>>).

-define(SQL_CLOSE_UPDATE, <<
    "UPDATE customer_service_session"
    "   SET status = 'closed', closed_at = to_timestamp($6), close_reason = $4,"
    "       version = version + 1, updated_at = to_timestamp($6)"
    " WHERE organization_id = $1 AND workspace_id = $2 AND id = $3"
    "   AND status IN ('queued', 'active') AND version = $5"
>>).

-define(SQL_RATE_UPDATE, <<
    "UPDATE customer_service_session"
    "   SET rating = $4, rating_at = to_timestamp($6), version = version + 1,"
    "       updated_at = to_timestamp($6)"
    " WHERE organization_id = $1 AND workspace_id = $2 AND id = $3"
    "   AND status = 'closed' AND version = $5 AND rating IS NULL"
>>).

-define(SQL_LIST_FOR_CONTACT, <<
    "SELECT s.id, s.organization_id, s.workspace_id, s.contact_id, s.conversation_id,"
    "       s.business_identity_id, s.visit_token_id, s.status, s.rating,"
    "       extract(epoch from s.rating_at)::bigint AS rating_at,"
    "       extract(epoch from s.queued_at)::bigint AS queued_at,"
    "       extract(epoch from s.claimed_at)::bigint AS claimed_at,"
    "       extract(epoch from s.closed_at)::bigint AS closed_at,"
    "       s.close_reason, s.version,"
    "       extract(epoch from s.updated_at)::bigint AS updated_at"
    "  FROM customer_service_session s"
    "  JOIN workspace w ON w.organization_id = $1 AND w.id = $2"
    " WHERE s.organization_id = $1 AND s.workspace_id = $2 AND s.contact_id = $3"
    " ORDER BY s.id"
>>).

%% C1（contracts-w2）键集下推：`ORDER BY id DESC LIMIT $5`（模板
%% eb_pg_message_ext.erl:25-34 的 DESC 形态），前两业务参数 OrgId+WorkspaceId
%% 同语句（JOIN workspace 同时证明 workspace 归属）。status 可选过滤：
%% `COALESCE($3, s.status)` 缺省恒真；游标 `$4`（bigint cast 防 int4
%% 溢出——TSID 是 64-bit）：0=首页，否则 `id < $4`
%% 取更早一页（DESC 键集的不重不漏方向——`id >` 在 DESC 排序下无法构造
%% 单调翻页游标）。绝无 OFFSET。
-define(SQL_LIST_SESSIONS_PAGE, <<
    "SELECT s.id, s.organization_id, s.workspace_id, s.contact_id, s.conversation_id,"
    "       s.business_identity_id, s.visit_token_id, s.status, s.rating,"
    "       extract(epoch from s.rating_at)::bigint AS rating_at,"
    "       extract(epoch from s.queued_at)::bigint AS queued_at,"
    "       extract(epoch from s.claimed_at)::bigint AS claimed_at,"
    "       extract(epoch from s.closed_at)::bigint AS closed_at,"
    "       s.close_reason, s.version,"
    "       extract(epoch from s.updated_at)::bigint AS updated_at"
    "  FROM customer_service_session s"
    "  JOIN workspace w ON w.organization_id = $1 AND w.id = $2"
    " WHERE s.organization_id = $1 AND s.workspace_id = $2"
    "   AND s.status = COALESCE($3, s.status)"
    "   AND ($4::bigint = 0 OR s.id < $4)"
    " ORDER BY s.id DESC"
    " LIMIT $5"
>>).

%% @doc 冻结语句（供 cs_pg_tests 的租户键机械断言）。
-spec sql_statements() -> [binary()].
sql_statements() ->
    [
        ?SQL_INSERT_SESSION,
        ?SQL_FETCH_SESSION,
        ?SQL_LOCK_SEAT,
        ?SQL_COUNT_ACTIVE,
        ?SQL_CLAIM_UPDATE,
        ?SQL_TRANSFER_UPDATE,
        ?SQL_CLOSE_UPDATE,
        ?SQL_RATE_UPDATE,
        ?SQL_LIST_FOR_CONTACT,
        ?SQL_LIST_SESSIONS_PAGE
    ].

%% ===================================================================
%% 基础读写
%% ===================================================================

-spec insert_session(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_session(OrgId, WorkspaceId, Draft) when is_map(Draft) ->
    SessionId = maps:get(id, Draft),
    Params = [
        SessionId,
        OrgId,
        WorkspaceId,
        maps:get(contact_id, Draft),
        maps:get(conversation_id, Draft),
        cs_pg_common:nullify(maps:get(visit_token_id, Draft, undefined)),
        cs_pg_common:nullify(maps:get(queued_at, Draft, undefined)),
        cs_pg_common:nullify(maps:get(created_by_user_id, Draft, undefined))
    ],
    case elib_pg:execute(?SQL_INSERT_SESSION, Params) of
        {ok, 1} -> fetch_session(OrgId, WorkspaceId, SessionId);
        {ok, 0} -> {error, no_row};
        {error, Reason} -> {error, map_insert_error(cs_pg_common:normalize_error(Reason))}
    end;
insert_session(_OrgId, _WorkspaceId, _Draft) ->
    {error, invalid_session}.

%% 同一 (Org, conversation) 已有未关闭 session 时，由 DB 部分唯一索引
%% uq_csss_org_conv_open 裁决为业务冲突——映射 conflict（cs_http 409 通道），
%% 不落 500 fail-closed（DEFECT-3，CS-04 E2E 发现）。
map_insert_error({sql, <<"23505">>, <<"uq_csss_org_conv_open">>}) -> conflict;
map_insert_error(Other) -> Other.

%% fetch_one/fetch_many 已完成行归一化（atom 键）；这里**只**把 status 从
%% binary 收敛为 atom。绝不再做第二次 normalize（R2 修复：二次归一化会拿
%% binary 键去查 atom 键 map，全列变 undefined——run5 真库首跑的根因）。
-spec fetch_session(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
fetch_session(OrgId, WorkspaceId, SessionId) ->
    to_status_row(
        cs_pg_common:fetch_one(?SQL_FETCH_SESSION, [OrgId, WorkspaceId, SessionId], ?SESSION_KEYS)
    ).

-spec list_sessions_for_contact(integer(), integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_sessions_for_contact(OrgId, WorkspaceId, ContactId) ->
    case
        cs_pg_common:fetch_many(
            ?SQL_LIST_FOR_CONTACT, [OrgId, WorkspaceId, ContactId], ?SESSION_KEYS
        )
    of
        {ok, Rows} -> {ok, [to_status_value(Row) || Row <- Rows]};
        {error, _} = Err -> Err
    end.

%% @doc C1（contracts-w2）键集分页读取。`Status` 为 binary 白名单值或
%% undefined（nullify 成 NULL，COALESCE 恒真）；行按 id DESC。
-spec list_sessions_page(
    integer(),
    integer(),
    binary() | undefined,
    non_neg_integer(),
    pos_integer()
) -> {ok, [map()]} | {error, term()}.
list_sessions_page(OrgId, WorkspaceId, Status, AfterId, Limit) ->
    Params = [
        OrgId,
        WorkspaceId,
        cs_pg_common:nullify(Status),
        AfterId,
        Limit
    ],
    case cs_pg_common:fetch_many(?SQL_LIST_SESSIONS_PAGE, Params, ?SESSION_KEYS) of
        {ok, Rows} -> {ok, [to_status_value(Row) || Row <- Rows]};
        {error, _} = Err -> Err
    end.

to_status_row({ok, Row}) -> {ok, to_status_value(Row)};
to_status_row({error, _} = Err) -> Err.

to_status_value(Row) ->
    maps:update_with(status, fun cs_pg_common:to_status/1, Row).

%% ===================================================================
%% claim（A02 的 DB 裁决点；单事务 CAS）
%% ===================================================================

-spec claim_session(integer(), integer(), integer(), integer(), integer(), integer(), map()) ->
    {ok, map()} | {error, term()}.
claim_session(OrgId, WorkspaceId, SessionId, IdentityId, ExpectedVersion, ClaimedAt, Event) ->
    Result = elib_pg:with_tx(fun(Conn) ->
        claim_tx(Conn, OrgId, WorkspaceId, SessionId, IdentityId, ExpectedVersion, ClaimedAt, Event)
    end),
    undo_rollback(Result).

claim_tx(Conn, OrgId, WorkspaceId, SessionId, IdentityId, ExpectedVersion, ClaimedAt, Event) ->
    case elib_pg:query(Conn, ?SQL_LOCK_SEAT, [OrgId, IdentityId]) of
        {ok, [SeatRow]} ->
            check_seat_capacity(Conn, OrgId, IdentityId, SeatRow),
            cas_claim_update(
                Conn,
                OrgId,
                WorkspaceId,
                SessionId,
                IdentityId,
                ExpectedVersion,
                ClaimedAt,
                Event
            );
        {ok, []} ->
            throw({rollback, {error, seat_not_found}});
        {error, Reason} ->
            throw({rollback, {error, cs_pg_common:normalize_error(Reason)}})
    end.

%% 检查与写入同锁：enabled=false → seat_disabled；active 计数达上限 → seat_at_capacity。
check_seat_capacity(Conn, OrgId, IdentityId, SeatRow) ->
    Enabled = maps:get(<<"enabled">>, SeatRow),
    Max = maps:get(<<"max_concurrent">>, SeatRow),
    CapacityOk =
        case elib_pg:query(Conn, ?SQL_COUNT_ACTIVE, [OrgId, IdentityId]) of
            {ok, [#{<<"n">> := N}]} -> is_integer(N) andalso N < Max;
            {ok, _} -> false;
            {error, Reason} -> throw({rollback, {error, cs_pg_common:normalize_error(Reason)}})
        end,
    case {Enabled, CapacityOk} of
        {false, _} -> throw({rollback, {error, seat_disabled}});
        {_, false} -> throw({rollback, {error, seat_at_capacity}});
        _ -> ok
    end.

cas_claim_update(
    Conn, OrgId, WorkspaceId, SessionId, IdentityId, ExpectedVersion, ClaimedAt, Event
) ->
    Params = [OrgId, WorkspaceId, SessionId, IdentityId, ClaimedAt, ExpectedVersion],
    case elib_pg:execute(Conn, ?SQL_CLAIM_UPDATE, Params) of
        {ok, 1} ->
            write_event_or_rollback(Conn, OrgId, Event),
            fetch_session_in(Conn, OrgId, WorkspaceId, SessionId);
        {ok, 0} ->
            throw({rollback, {error, conflict}});
        {error, Reason} ->
            throw({rollback, {error, cs_pg_common:normalize_error(Reason)}})
    end.

%% ===================================================================
%% transfer / close / rate（单事务 CAS + 事件）
%% ===================================================================

-spec transfer_session(integer(), integer(), integer(), integer(), integer(), integer(), map()) ->
    {ok, map()} | {error, term()}.
transfer_session(OrgId, WorkspaceId, SessionId, ToIdentityId, ExpectedVersion, At, Event) ->
    Params = [OrgId, WorkspaceId, SessionId, ToIdentityId, ExpectedVersion, At],
    cas_in_tx(?SQL_TRANSFER_UPDATE, Params, OrgId, WorkspaceId, SessionId, Event).

-spec close_session(integer(), integer(), integer(), term(), integer(), integer(), map()) ->
    {ok, map()} | {error, term()}.
close_session(OrgId, WorkspaceId, SessionId, Reason, ExpectedVersion, At, Event) ->
    Params = [OrgId, WorkspaceId, SessionId, cs_pg_common:nullify(Reason), ExpectedVersion, At],
    cas_in_tx(?SQL_CLOSE_UPDATE, Params, OrgId, WorkspaceId, SessionId, Event).

-spec rate_session(integer(), integer(), integer(), pos_integer(), integer(), integer(), map()) ->
    {ok, map()} | {error, term()}.
rate_session(OrgId, WorkspaceId, SessionId, Rating, ExpectedVersion, At, Event) ->
    Params = [OrgId, WorkspaceId, SessionId, Rating, ExpectedVersion, At],
    cas_in_tx(?SQL_RATE_UPDATE, Params, OrgId, WorkspaceId, SessionId, Event).

cas_in_tx(Sql, Params, OrgId, WorkspaceId, SessionId, Event) ->
    Result = elib_pg:with_tx(fun(Conn) ->
        case elib_pg:execute(Conn, Sql, Params) of
            {ok, 1} ->
                write_event_or_rollback(Conn, OrgId, Event),
                fetch_session_in(Conn, OrgId, WorkspaceId, SessionId);
            {ok, 0} ->
                throw({rollback, {error, conflict}});
            {error, Reason} ->
                throw({rollback, {error, cs_pg_common:normalize_error(Reason)}})
        end
    end),
    undo_rollback(Result).

%% ===================================================================
%% 事务内辅助
%% ===================================================================

write_event_or_rollback(Conn, OrgId, Event) ->
    case cs_pg_seat:insert_event_in(Conn, OrgId, Event) of
        {ok, _EventId} -> ok;
        {error, Reason} -> throw({rollback, {error, {event_append_failed, Reason}}})
    end.

fetch_session_in(Conn, OrgId, WorkspaceId, SessionId) ->
    case elib_pg:query(Conn, ?SQL_FETCH_SESSION, [OrgId, WorkspaceId, SessionId]) of
        {ok, [Row]} ->
            %% 事务内拿到的是**原始** binary 键行：这里恰好做一次完整归一化
            {ok, to_status_value(cs_pg_common:normalize_row(Row, ?SESSION_KEYS))};
        {ok, []} ->
            throw({rollback, {error, not_found}});
        {error, Reason} ->
            throw({rollback, {error, cs_pg_common:normalize_error(Reason)}})
    end.

%% elib_pg 的业务回滚信号 `{rollback, Reason}`：Reason 就是调用方的返回形状，
%% 原样透传（不包一层 rollback）。
undo_rollback({rollback, Reason}) -> Reason;
undo_rollback(Result) -> Result.
