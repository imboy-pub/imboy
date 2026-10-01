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
    insert_session/4,
    append_message_event_in/3,
    fetch_session/3,
    claim_session/7,
    transfer_session/7,
    close_session/7,
    rate_session/7,
    list_sessions_for_contact/3,
    list_sessions_page/5,
    seat_session_page/5,
    fetch_session_customer_context/3,
    list_session_history_page/4,
    list_contact_notes_page/3,
    %% CS-BE-04：已读游标（单调 ACK 幂等 + 未读事实现算）
    ack_session_read/6,
    fetch_session_read_state/4,
    %% CS-BE-07：按需统计（窗口聚合 + 当前 status 计数）
    session_stats/4,
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

-define(SQL_LOCK_MESSAGE_SEAT, <<
    "SELECT business_identity_id FROM customer_service_seat"
    " WHERE organization_id=$1 AND business_identity_id=$2 FOR KEY SHARE"
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
%% CSB-02R 坐席工作台页（org-wide，workspace 可选收窄）。行含 contact 掩码
%% 原料（display_name / subject_mask——已是掩码，不是原文）与末条消息**安全
%% 摘要**（id / sender_type / created_at）：密文（body_cipher）、密钥版本、
%% client_msg_id 一概不进本语句，workbench 未解锁 E2EE 前零消息内容出站。
-define(SQL_SEAT_SESSION_PAGE, <<
    "SELECT s.id, s.organization_id, s.workspace_id, s.contact_id, s.conversation_id,"
    "       s.business_identity_id, s.visit_token_id, s.created_by_user_id, s.status,"
    "       s.version,"
    "       extract(epoch from s.queued_at)::bigint AS queued_at,"
    "       extract(epoch from s.claimed_at)::bigint AS claimed_at,"
    "       extract(epoch from s.closed_at)::bigint AS closed_at,"
    "       c.display_name AS contact_display_name,"
    "       ci.subject_mask AS contact_subject_mask,"
    "       lm.last_message_id, lm.last_message_sender_type, lm.last_message_created_at"
    "  FROM customer_service_session s"
    "  JOIN enterprise_contact c"
    "    ON c.organization_id = s.organization_id AND c.id = s.contact_id"
    "  LEFT JOIN LATERAL ("
    "       SELECT i.subject_mask FROM enterprise_contact_identity i"
    "        WHERE i.organization_id = s.organization_id AND i.contact_id = s.contact_id"
    "        ORDER BY i.id LIMIT 1"
    "  ) ci ON true"
    "  LEFT JOIN LATERAL ("
    "       SELECT m.id AS last_message_id,"
    "              m.sender_type AS last_message_sender_type,"
    "              extract(epoch from m.created_at)::bigint AS last_message_created_at"
    "         FROM enterprise_message m"
    "        WHERE m.organization_id = s.organization_id"
    "          AND m.conversation_id = s.conversation_id"
    "        ORDER BY m.id DESC LIMIT 1"
    "  ) lm ON true"
    " WHERE s.organization_id = $1 AND s.status = $2"
    "   AND ($3::bigint = 0 OR s.id < $3)"
    "   AND ($4::bigint = 0 OR s.workspace_id = $4)"
    " ORDER BY s.id DESC"
    " LIMIT $5"
>>).

%% 同作用域 status 计数（与列表页同 Org/workspace 收窄，稳定可对账）。
-define(SQL_SEAT_SESSION_TOTAL, <<
    "SELECT count(*) AS total FROM customer_service_session s"
    " WHERE s.organization_id = $1 AND s.status = $2"
    "   AND ($3::bigint = 0 OR s.workspace_id = $3)"
>>).

-define(SQL_SEAT_SESSION_TOTAL_BY_STATUS, <<
    "SELECT s.status, count(*) AS total FROM customer_service_session s"
    " WHERE s.organization_id = $1"
    "   AND ($2::bigint = 0 OR s.workspace_id = $2)"
    " GROUP BY s.status"
>>).

%% CS-BE-07（按需统计）：窗口聚合三轴 + 当前 status 计数。窗口比较全部
%% 用 to_timestamp($n::double precision)（epoch 秒 → UTC instant），与 DB
%% 会话时区设置无关；Org/Workspace 同语句收窄（铁律 6；WorkspaceId=0 =
%% org-wide）。AVG 走 float8（无样本 AVG 为 NULL → undefined，不用 0 伪装）。
-define(SQL_STATS_NEW_SESSIONS, <<
    "SELECT count(*) AS new_sessions,"
    "       count(claimed_at) AS claimed_in_window,"
    "       AVG(extract(epoch from (claimed_at - queued_at)))::float8 AS first_response_avg_seconds"
    "  FROM customer_service_session s"
    " WHERE s.organization_id = $1"
    "   AND ($2::bigint = 0 OR s.workspace_id = $2)"
    "   AND s.queued_at >= to_timestamp($3::double precision)"
    "   AND s.queued_at < to_timestamp($4::double precision)"
>>).

-define(SQL_STATS_CLOSED, <<
    "SELECT count(*) AS closed_sessions"
    "  FROM customer_service_session s"
    " WHERE s.organization_id = $1"
    "   AND ($2::bigint = 0 OR s.workspace_id = $2)"
    "   AND s.closed_at >= to_timestamp($3::double precision)"
    "   AND s.closed_at < to_timestamp($4::double precision)"
>>).

-define(SQL_STATS_RATING, <<
    "SELECT count(rating_at) AS rated_in_window,"
    "       AVG(s.rating)::float8 AS avg_rating"
    "  FROM customer_service_session s"
    " WHERE s.organization_id = $1"
    "   AND ($2::bigint = 0 OR s.workspace_id = $2)"
    "   AND s.rating_at >= to_timestamp($3::double precision)"
    "   AND s.rating_at < to_timestamp($4::double precision)"
>>).

%% CS-BE-03（CS-DEC-01 冻结）：会话锚定的客户上下文事实行。零白名单外列：
%% 只有来源两列（visit_token_id / created_by_user_id——来源推导输入）、
%% 掩码原料（subject_mask 本就是掩码 / display_name 只做打码输入）与
%% first/last seen（事实派生：first = contact.created_at，
%% last = max(contact.created_at, 同 contact 全部会话活动峰值——
%% queued/claimed/closed/rating 四列的 GREATEST；PG 的 GREATEST 忽略 NULL，
%% MAX 无行时 COALESCE 回 created_at）。联系方式/原始外部身份/密文列
%% （profile_cipher、subject_hmac、body_cipher…）一概不进本语句。
-define(SQL_SESSION_CUSTOMER_CONTEXT, <<
    "SELECT s.contact_id, s.workspace_id, s.visit_token_id, s.created_by_user_id,"
    "       extract(epoch from c.created_at)::bigint AS first_seen,"
    "       extract(epoch from GREATEST("
    "           c.created_at,"
    "           COALESCE((SELECT MAX(GREATEST(s2.queued_at, s2.claimed_at,"
    "                                         s2.closed_at, s2.rating_at))"
    "                       FROM customer_service_session s2"
    "                      WHERE s2.organization_id = c.organization_id"
    "                        AND s2.contact_id = c.id),"
    "                    c.created_at)))::bigint AS last_seen,"
    "       ci.subject_mask,"
    "       c.display_name"
    "  FROM customer_service_session s"
    "  JOIN enterprise_contact c"
    "    ON c.organization_id = s.organization_id AND c.id = s.contact_id"
    "  LEFT JOIN LATERAL ("
    "       SELECT i.subject_mask FROM enterprise_contact_identity i"
    "        WHERE i.organization_id = s.organization_id AND i.contact_id = s.contact_id"
    "        ORDER BY i.id LIMIT 1"
    "  ) ci ON true"
    " WHERE s.organization_id = $1 AND s.workspace_id = $2 AND s.id = $3"
>>).

%% CS-BE-03：同 contact 的同 Org 历史客服会话页（键集下推 `id < $3` +
%% `ORDER BY id DESC LIMIT $4`，C1 冻结口径）；列 = 历史投影白名单，
%% 无 visit_token_id / close_reason / created_by_user_id。
-define(SQL_SESSION_HISTORY_PAGE, <<
    "SELECT s.id, s.conversation_id, s.workspace_id, s.status, s.version, s.rating,"
    "       extract(epoch from s.queued_at)::bigint AS queued_at,"
    "       extract(epoch from s.claimed_at)::bigint AS claimed_at,"
    "       extract(epoch from s.closed_at)::bigint AS closed_at"
    "  FROM customer_service_session s"
    " WHERE s.organization_id = $1 AND s.contact_id = $2"
    "   AND ($3::bigint = 0 OR s.id < $3)"
    " ORDER BY s.id DESC"
    " LIMIT $4"
>>).

%% CS-BE-03：同 contact 的授权备注事实页（active 行，软删排除；按
%% created_at/id DESC 稳定序）。EB 无 note 读面、密文材料禁出站
%% （CS-DEC-01）——只取行事实（id / business_identity_id / created_at），
%% 零正文、零密文列。
-define(SQL_CONTACT_NOTES_PAGE, <<
    "SELECT n.id, n.business_identity_id,"
    "       extract(epoch from n.created_at)::bigint AS created_at"
    "  FROM enterprise_note n"
    " WHERE n.organization_id = $1 AND n.contact_id = $2"
    "   AND n.status = 'active' AND n.deleted_at IS NULL"
    " ORDER BY n.created_at DESC, n.id DESC"
    " LIMIT $3"
>>).

%% CS-BE-04（CS-DEC-02）：ACK 的单调 upsert。DO UPDATE 带 WHERE
%% 「旧值 < 新值」：条件不满足即零行 no-op——重复/乱序后到的旧 ACK 不改
%% 任何列（updated_at 不被刷新），游标永不回退。冲突键是
%% uq_csrc_org_session_identity（游标绑定 org+session+经办 identity，
%% 不是全局 user）。时间由注入的 At（epoch 秒）落库，不读 now()。
-define(SQL_ACK_CURSOR_UPSERT, <<
    "INSERT INTO customer_service_read_cursor"
    " (id, organization_id, workspace_id, session_id, business_identity_id,"
    "  last_read_message_id, created_at, updated_at)"
    " VALUES ($1, $2, $3, $4, $5, $6, to_timestamp($7), to_timestamp($7))"
    " ON CONFLICT (organization_id, session_id, business_identity_id) DO UPDATE"
    "   SET last_read_message_id = EXCLUDED.last_read_message_id,"
    "       updated_at = EXCLUDED.updated_at"
    " WHERE customer_service_read_cursor.last_read_message_id"
    "       < EXCLUDED.last_read_message_id"
>>).

%% CS-BE-04：ACK 目标收敛——候选游标只能指向该会话 conversation 中已存在的
%% 消息 id（不存在的/未来的 id 不越过消息事实上界），无更早消息则 0。
-define(SQL_ACK_EFFECTIVE_CURSOR, <<
    "SELECT COALESCE(MAX(m.id), 0)::bigint AS effective"
    "  FROM enterprise_message m"
    " WHERE m.organization_id = $1 AND m.workspace_id = $2"
    "   AND m.conversation_id = $3 AND m.id <= $4"
>>).

%% CS-BE-04：transfer 边界的取值——transfer 时刻会话内已存在的最大
%% message id（无消息为 0）。
-define(SQL_TRANSFER_BOUNDARY_MAX, <<
    "SELECT COALESCE(MAX(m.id), 0)::bigint AS boundary"
    "  FROM enterprise_message m"
    " WHERE m.organization_id = $1 AND m.workspace_id = $2"
    "   AND m.conversation_id = $3"
>>).

%% CS-BE-04：读状态单语句——游标（LEFT JOIN 本经办 identity 的游标行）
%% 与未读数（enterprise_message 事实现算：visible ∧ sender_type='contact'
%% ∧ id > cursor）。无冗余计数表；游标不存在（尚未读过）从 0 起算。
-define(SQL_SESSION_READ_STATE, <<
    "SELECT COALESCE(c.last_read_message_id, 0)::bigint AS last_read_message_id,"
    "       (SELECT count(*) FROM enterprise_message m"
    "         WHERE m.organization_id = s.organization_id"
    "           AND m.workspace_id = s.workspace_id"
    "           AND m.conversation_id = s.conversation_id"
    "           AND m.id > COALESCE(c.last_read_message_id, 0)"
    "           AND m.sender_type = 'contact'"
    "           AND m.visibility = 'visible') AS unread_count"
    "  FROM customer_service_session s"
    "  LEFT JOIN customer_service_read_cursor c"
    "    ON c.organization_id = s.organization_id"
    "   AND c.session_id = s.id"
    "   AND c.business_identity_id = $3"
    " WHERE s.organization_id = $1 AND s.workspace_id = $2 AND s.id = $4"
>>).

%% CS-BE-04：transfer 边界 upsert（同改绑事务执行）——受让人游标写入
%% 「transfer 时刻会话内已存在的最大 message id」（$6 = 已查得的边界值，
%% 无消息为 0）。无条件覆盖（DO UPDATE 不带 WHERE）：transfer 是新
%% assignment 边界的确立事件，不是 ACK——A→B→A 重受让时，前次游标被
%% 新边界取代（transfer 前的历史对受让人默认 0 unread，CS-DEC-02）。
-define(SQL_TRANSFER_CURSOR_BOUNDARY, <<
    "INSERT INTO customer_service_read_cursor"
    " (id, organization_id, workspace_id, session_id, business_identity_id,"
    "  last_read_message_id, created_at, updated_at)"
    " VALUES ($1, $2, $3, $4, $5, $6, to_timestamp($7), to_timestamp($7))"
    " ON CONFLICT (organization_id, session_id, business_identity_id) DO UPDATE"
    "   SET last_read_message_id = EXCLUDED.last_read_message_id,"
    "       updated_at = EXCLUDED.updated_at"
>>).

-define(SEAT_SESSION_KEYS, [
    id,
    organization_id,
    workspace_id,
    contact_id,
    conversation_id,
    business_identity_id,
    visit_token_id,
    created_by_user_id,
    status,
    version,
    queued_at,
    claimed_at,
    closed_at,
    contact_display_name,
    contact_subject_mask,
    last_message_id,
    last_message_sender_type,
    last_message_created_at
]).

%% CS-BE-03 三段读模型的行键（与各自 SQL 列逐字对应；null → undefined）。
-define(SESSION_CONTEXT_KEYS, [
    contact_id,
    workspace_id,
    visit_token_id,
    created_by_user_id,
    first_seen,
    last_seen,
    subject_mask,
    display_name
]).

-define(SESSION_HISTORY_KEYS, [
    id,
    conversation_id,
    workspace_id,
    status,
    version,
    rating,
    queued_at,
    claimed_at,
    closed_at
]).

-define(CONTACT_NOTES_KEYS, [id, business_identity_id, created_at]).

-spec seat_session_page(
    integer(),
    binary(),
    non_neg_integer(),
    pos_integer(),
    non_neg_integer()
) ->
    {ok, #{rows := [map()], total := non_neg_integer(), total_by_status := map()}}
    | {error, term()}.
seat_session_page(OrgId, Status, AfterId, Limit, WorkspaceId) ->
    case
        cs_pg_common:fetch_many(
            ?SQL_SEAT_SESSION_PAGE,
            [OrgId, Status, AfterId, WorkspaceId, Limit],
            ?SEAT_SESSION_KEYS
        )
    of
        {error, _} = Err ->
            Err;
        {ok, Rows} ->
            case seat_session_total(OrgId, Status, WorkspaceId) of
                {error, _} = Err2 ->
                    Err2;
                {ok, Total} ->
                    case seat_session_total_by_status(OrgId, WorkspaceId) of
                        {error, _} = Err3 ->
                            Err3;
                        {ok, ByStatus} ->
                            {ok, #{rows => Rows, total => Total, total_by_status => ByStatus}}
                    end
            end
    end.

seat_session_total(OrgId, Status, WorkspaceId) ->
    case
        cs_pg_common:fetch_one(
            ?SQL_SEAT_SESSION_TOTAL, [OrgId, Status, WorkspaceId], [total]
        )
    of
        {ok, #{total := Total}} when is_integer(Total) -> {ok, Total};
        {error, _} = Err -> Err
    end.

seat_session_total_by_status(OrgId, WorkspaceId) ->
    case
        cs_pg_common:fetch_many(
            ?SQL_SEAT_SESSION_TOTAL_BY_STATUS, [OrgId, WorkspaceId], [status, total]
        )
    of
        {error, _} = Err ->
            Err;
        {ok, Rows} ->
            ByStatus =
                #{
                    cs_pg_common:to_status(maps:get(status, Row)) => maps:get(total, Row)
                 || Row <- Rows
                },
            %% DF-8：queued/active/closed 三键恒在、零计数显式出 0。GROUP BY
            %% 只出非零行，缺键会把前端 toCounts 的数字断言打成 TypeError，
            %% 队列视图恒「请求失败」；白名单外的 status 键原样保留。
            {ok, maps:merge(#{queued => 0, active => 0, closed => 0}, ByStatus)}
    end.

%% ===================================================================
%% CS-BE-07：按需统计（纯读；三轴窗口聚合 + 当前 status 计数现算）
%% ===================================================================

-spec session_stats(integer(), integer(), non_neg_integer(), non_neg_integer()) ->
    {ok, map()} | {error, term()}.
session_stats(OrgId, WorkspaceId, Start, End) ->
    case
        cs_pg_common:fetch_one(
            ?SQL_STATS_NEW_SESSIONS,
            [OrgId, WorkspaceId, Start, End],
            [new_sessions, claimed_in_window, first_response_avg_seconds]
        )
    of
        {error, _} = Err ->
            Err;
        {ok, NewRow} ->
            session_stats_closed(OrgId, WorkspaceId, Start, End, NewRow)
    end.

session_stats_closed(OrgId, WorkspaceId, Start, End, NewRow) ->
    case
        cs_pg_common:fetch_one(
            ?SQL_STATS_CLOSED, [OrgId, WorkspaceId, Start, End], [closed_sessions]
        )
    of
        {error, _} = Err ->
            Err;
        {ok, ClosedRow} ->
            session_stats_rating(OrgId, WorkspaceId, Start, End, NewRow, ClosedRow)
    end.

session_stats_rating(OrgId, WorkspaceId, Start, End, NewRow, ClosedRow) ->
    case
        cs_pg_common:fetch_one(
            ?SQL_STATS_RATING,
            [OrgId, WorkspaceId, Start, End],
            [rated_in_window, avg_rating]
        )
    of
        {error, _} = Err ->
            Err;
        {ok, RatingRow} ->
            session_stats_status(OrgId, WorkspaceId, NewRow, ClosedRow, RatingRow)
    end.

session_stats_status(OrgId, WorkspaceId, NewRow, ClosedRow, RatingRow) ->
    case seat_session_total_by_status(OrgId, WorkspaceId) of
        {error, _} = Err ->
            Err;
        {ok, ByStatus} ->
            {ok,
                maps:merge(
                    maps:merge(NewRow, ClosedRow),
                    maps:merge(RatingRow, #{status_counts => ByStatus})
                )}
    end.

-spec sql_statements() -> [binary()].
sql_statements() ->
    [
        ?SQL_INSERT_SESSION,
        ?SQL_FETCH_SESSION,
        ?SQL_LOCK_SEAT,
        ?SQL_LOCK_MESSAGE_SEAT,
        ?SQL_COUNT_ACTIVE,
        ?SQL_CLAIM_UPDATE,
        ?SQL_TRANSFER_UPDATE,
        ?SQL_CLOSE_UPDATE,
        ?SQL_RATE_UPDATE,
        ?SQL_LIST_FOR_CONTACT,
        ?SQL_LIST_SESSIONS_PAGE,
        ?SQL_SEAT_SESSION_PAGE,
        ?SQL_SEAT_SESSION_TOTAL,
        ?SQL_SEAT_SESSION_TOTAL_BY_STATUS,
        ?SQL_SESSION_CUSTOMER_CONTEXT,
        ?SQL_SESSION_HISTORY_PAGE,
        ?SQL_CONTACT_NOTES_PAGE,
        %% CS-BE-04：已读游标（单调 ACK upsert / 候选收敛 / 读状态 / transfer 边界）
        ?SQL_ACK_CURSOR_UPSERT,
        ?SQL_ACK_EFFECTIVE_CURSOR,
        ?SQL_TRANSFER_BOUNDARY_MAX,
        ?SQL_SESSION_READ_STATE,
        ?SQL_TRANSFER_CURSOR_BOUNDARY,
        %% CS-BE-07：按需统计（新会话+首响 / 关闭 / 评分；当前 status 计数
        %% 复用上面的 SQL_SEAT_SESSION_TOTAL_BY_STATUS）
        ?SQL_STATS_NEW_SESSIONS,
        ?SQL_STATS_CLOSED,
        ?SQL_STATS_RATING
    ].

%% ===================================================================
%% CS-BE-03：客户上下文只读事实（零写副作用；全部同语句租户裁决）
%% ===================================================================

-spec fetch_session_customer_context(integer(), integer(), integer()) ->
    {ok, map()} | {error, term()}.
fetch_session_customer_context(OrgId, WorkspaceId, SessionId) ->
    cs_pg_common:fetch_one(
        ?SQL_SESSION_CUSTOMER_CONTEXT, [OrgId, WorkspaceId, SessionId], ?SESSION_CONTEXT_KEYS
    ).

-spec list_session_history_page(integer(), integer(), non_neg_integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
list_session_history_page(OrgId, ContactId, AfterId, Limit) ->
    case
        cs_pg_common:fetch_many(
            ?SQL_SESSION_HISTORY_PAGE, [OrgId, ContactId, AfterId, Limit], ?SESSION_HISTORY_KEYS
        )
    of
        {ok, Rows} -> {ok, [to_status_value(Row) || Row <- Rows]};
        {error, _} = Err -> Err
    end.

-spec list_contact_notes_page(integer(), integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
list_contact_notes_page(OrgId, ContactId, Limit) ->
    cs_pg_common:fetch_many(
        ?SQL_CONTACT_NOTES_PAGE, [OrgId, ContactId, Limit], ?CONTACT_NOTES_KEYS
    ).

%% ===================================================================
%% 基础读写
%% ===================================================================

-spec insert_session(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_session(OrgId, WorkspaceId, Draft) when is_map(Draft) ->
    SessionId = maps:get(id, Draft),
    Params = session_insert_params(OrgId, WorkspaceId, Draft),
    case elib_pg:execute(?SQL_INSERT_SESSION, Params) of
        {ok, 1} -> fetch_session(OrgId, WorkspaceId, SessionId);
        {ok, 0} -> {error, no_row};
        {error, Reason} -> {error, map_insert_error(cs_pg_common:normalize_error(Reason))}
    end;
insert_session(_OrgId, _WorkspaceId, _Draft) ->
    {error, invalid_session}.

%% Opening and its audit are atomic, using the same connection as CAS transitions.
-spec insert_session(integer(), integer(), map(), map()) -> {ok, map()} | {error, term()}.
insert_session(OrgId, WorkspaceId, Draft, Event) when is_map(Draft), is_map(Event) ->
    undo_rollback(
        elib_pg:with_tx(fun(Conn) ->
            Params = session_insert_params(OrgId, WorkspaceId, Draft),
            case elib_pg:execute(Conn, ?SQL_INSERT_SESSION, Params) of
                {ok, 1} ->
                    write_event_or_rollback(Conn, OrgId, Event),
                    fetch_session_in(Conn, OrgId, WorkspaceId, maps:get(id, Draft));
                {ok, 0} ->
                    throw({rollback, {error, no_row}});
                {error, Reason} ->
                    throw(
                        {rollback, {error, map_insert_error(cs_pg_common:normalize_error(Reason))}}
                    )
            end
        end)
    );
insert_session(_OrgId, _WorkspaceId, _Draft, _Event) ->
    {error, invalid_session}.

session_insert_params(OrgId, WorkspaceId, Draft) ->
    [
        maps:get(id, Draft),
        OrgId,
        WorkspaceId,
        maps:get(contact_id, Draft),
        maps:get(conversation_id, Draft),
        cs_pg_common:nullify(maps:get(visit_token_id, Draft, undefined)),
        cs_pg_common:nullify(maps:get(queued_at, Draft, undefined)),
        cs_pg_common:nullify(maps:get(created_by_user_id, Draft, undefined))
    ].

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
    lock_available_seat(Conn, OrgId, IdentityId),
    cas_claim_update(
        Conn, OrgId, WorkspaceId, SessionId, IdentityId, ExpectedVersion, ClaimedAt, Event
    ).

%% Claim and transfer share the target-seat lock and capacity decision.
lock_available_seat(Conn, OrgId, IdentityId) ->
    case elib_pg:query(Conn, ?SQL_LOCK_SEAT, [OrgId, IdentityId]) of
        {ok, [SeatRow]} ->
            check_seat_capacity(Conn, OrgId, IdentityId, SeatRow);
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
    %% CS-BE-04：transfer 不再走共用 cas_in_tx——同事务在 CAS 成功后确立
    %% 受让人的读游标边界（transfer 前的历史对受让人默认 0 unread，
    %% CS-DEC-02）；任一步失败全回滚。
    Result = elib_pg:with_tx(fun(Conn) ->
        lock_available_seat(Conn, OrgId, ToIdentityId),
        Params = [OrgId, WorkspaceId, SessionId, ToIdentityId, ExpectedVersion, At],
        case elib_pg:execute(Conn, ?SQL_TRANSFER_UPDATE, Params) of
            {ok, 1} ->
                write_event_or_rollback(Conn, OrgId, Event),
                {ok, Session} = fetch_session_in(Conn, OrgId, WorkspaceId, SessionId),
                ok =
                    transfer_cursor_boundary_in(
                        Conn,
                        OrgId,
                        WorkspaceId,
                        SessionId,
                        ToIdentityId,
                        maps:get(conversation_id, Session),
                        At
                    ),
                {ok, Session};
            {ok, 0} ->
                throw({rollback, {error, conflict}});
            {error, Reason} ->
                throw({rollback, {error, cs_pg_common:normalize_error(Reason)}})
        end
    end),
    undo_rollback(Result).

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
%% CS-BE-04：已读游标（单调 ACK 幂等 + 未读事实现算）
%% ===================================================================

%% @doc 已读游标 ACK（单事务机械单调写；授权由 application 复核后进入）。
%% 步骤：取会话（同语句租户裁决）→ 候选游标收敛到已存在消息事实上界 →
%% 单调 upsert → 返回 ACK 后的读状态。任一步失败全回滚。
-spec ack_session_read(integer(), integer(), integer(), integer(), non_neg_integer(), integer()) ->
    {ok, cs_store_port:read_state()} | {error, term()}.
ack_session_read(OrgId, WorkspaceId, SessionId, IdentityId, LastReadMessageId, At) ->
    Result = elib_pg:with_tx(fun(Conn) ->
        {ok, Session} = fetch_session_in(Conn, OrgId, WorkspaceId, SessionId),
        ConversationId = maps:get(conversation_id, Session),
        Effective = effective_cursor_in(
            Conn, OrgId, WorkspaceId, ConversationId, LastReadMessageId
        ),
        upsert_cursor_in(Conn, OrgId, WorkspaceId, SessionId, IdentityId, Effective, At),
        read_state_in(Conn, OrgId, WorkspaceId, SessionId, IdentityId)
    end),
    undo_rollback(Result).

%% @doc 会话读状态（游标 + 未读数；单语句租户裁决，只读零副作用）。
-spec fetch_session_read_state(integer(), integer(), integer(), integer()) ->
    {ok, cs_store_port:read_state()} | {error, term()}.
fetch_session_read_state(OrgId, WorkspaceId, SessionId, IdentityId) ->
    case
        cs_pg_common:fetch_one(
            ?SQL_SESSION_READ_STATE, [OrgId, WorkspaceId, IdentityId, SessionId], [
                last_read_message_id, unread_count
            ]
        )
    of
        {ok, #{last_read_message_id := Cursor, unread_count := Unread}} ->
            {ok, #{
                session_id => SessionId,
                business_identity_id => IdentityId,
                last_read_message_id => Cursor,
                unread_count => Unread
            }};
        {error, _} = Err ->
            Err
    end.

%% 候选游标收敛：只承认会话 conversation 内已存在的消息 id（<= 候选的最大
%% id）；候选之前无任何消息（或候选 <= 0）则 0。
effective_cursor_in(Conn, OrgId, WorkspaceId, ConversationId, Candidate) ->
    case
        elib_pg:query(Conn, ?SQL_ACK_EFFECTIVE_CURSOR, [
            OrgId, WorkspaceId, ConversationId, Candidate
        ])
    of
        {ok, [#{<<"effective">> := N}]} when is_integer(N), N >= 0 -> N;
        {ok, _} -> throw({rollback, {error, {cursor_effective_unavailable, Candidate}}});
        {error, Reason} -> throw({rollback, {error, cs_pg_common:normalize_error(Reason)}})
    end.

%% 单调 upsert：DO UPDATE 带 WHERE（旧值 < 新值），条件不满足即零行
%% no-op（幂等）；成功与否都以回读状态为准。
upsert_cursor_in(Conn, OrgId, WorkspaceId, SessionId, IdentityId, Effective, At) ->
    Params = [
        cs_tsid:new_id(cs_read_cursor), OrgId, WorkspaceId, SessionId, IdentityId, Effective, At
    ],
    case elib_pg:execute(Conn, ?SQL_ACK_CURSOR_UPSERT, Params) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            throw({rollback, {error, cs_pg_common:normalize_error(Reason)}})
    end.

%% transfer 边界（在改绑事务内调用）：受让人游标 = transfer 时刻会话内
%% 已存在的最大 message id（无消息为 0）；无条件覆盖旧游标行。
transfer_cursor_boundary_in(Conn, OrgId, WorkspaceId, SessionId, ToIdentityId, ConversationId, At) ->
    case elib_pg:query(Conn, ?SQL_TRANSFER_BOUNDARY_MAX, [OrgId, WorkspaceId, ConversationId]) of
        {ok, [#{<<"boundary">> := N}]} when is_integer(N), N >= 0 ->
            Params = [
                cs_tsid:new_id(cs_read_cursor),
                OrgId,
                WorkspaceId,
                SessionId,
                ToIdentityId,
                N,
                At
            ],
            case elib_pg:execute(Conn, ?SQL_TRANSFER_CURSOR_BOUNDARY, Params) of
                {ok, _} ->
                    ok;
                {error, Reason} ->
                    throw({rollback, {error, cs_pg_common:normalize_error(Reason)}})
            end;
        {ok, _} ->
            throw({rollback, {error, cursor_boundary_unavailable}});
        {error, Reason} ->
            throw({rollback, {error, cs_pg_common:normalize_error(Reason)}})
    end.

read_state_in(Conn, OrgId, WorkspaceId, SessionId, IdentityId) ->
    case
        elib_pg:query(Conn, ?SQL_SESSION_READ_STATE, [OrgId, WorkspaceId, IdentityId, SessionId])
    of
        {ok, [Row]} ->
            Normalized = cs_pg_common:normalize_row(
                Row, [last_read_message_id, unread_count]
            ),
            {ok, #{
                session_id => SessionId,
                business_identity_id => IdentityId,
                last_read_message_id => maps:get(last_read_message_id, Normalized),
                unread_count => maps:get(unread_count, Normalized)
            }};
        {ok, []} ->
            throw({rollback, {error, not_found}});
        {error, Reason} ->
            throw({rollback, {error, cs_pg_common:normalize_error(Reason)}})
    end.

%% ===================================================================
%% 事务内辅助
%% ===================================================================

%% Seat before session matches claim/transfer order; KEY SHARE permits concurrent
%% messages while preventing the event FK from reversing that order after the lock.
append_message_event_in(Conn, OrgId, Event) ->
    Identity = maps:get(business_identity_id, Event, undefined),
    case lock_message_seat(Conn, OrgId, Identity) of
        ok ->
            Sql = <<(?SQL_FETCH_SESSION)/binary, " FOR UPDATE">>,
            Params = [OrgId, maps:get(workspace_id, Event), maps:get(session_id, Event)],
            case elib_pg:query(Conn, Sql, Params) of
                {ok, [#{<<"status">> := <<"closed">>}]} ->
                    {error, session_already_closed};
                {ok, [Row]} ->
                    case
                        cs_pg_common:nullify(Identity) =:= maps:get(<<"business_identity_id">>, Row)
                    of
                        true -> cs_pg_seat:insert_event_in(Conn, OrgId, Event);
                        false -> {error, conflict}
                    end;
                {ok, []} ->
                    {error, not_found};
                {error, Reason} ->
                    {error, cs_pg_common:normalize_error(Reason)}
            end;
        {error, _} = Err ->
            Err
    end.

lock_message_seat(_Conn, _OrgId, Identity) when Identity =:= undefined; Identity =:= null -> ok;
lock_message_seat(Conn, OrgId, Identity) ->
    case elib_pg:query(Conn, ?SQL_LOCK_MESSAGE_SEAT, [OrgId, Identity]) of
        {ok, [_]} -> ok;
        {ok, []} -> {error, seat_not_found};
        {error, Reason} -> {error, cs_pg_common:normalize_error(Reason)}
    end.

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
