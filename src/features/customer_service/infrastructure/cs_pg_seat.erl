%%% @doc seat / identity 事实 / 客服事件的 PG 实现（`cs_store_port` 的 seat 段）。
%%%
%%% 铁律 6：Org 级资源的每条 SQL 都同语句带 `organization_id`（`$1`）。
%%% A01 的数据库层：seat INSERT 走复合 FK (organization_id, business_identity_id,
%%% function_key) + CHECK (function_key='customer_service')——引用 sales identity
%%% 会被 23503/fk_css_identity_function 拒绝，绕过应用也进不来。
%%% 事件表是 append-only：本模块只允许 INSERT（UPDATE/DELETE 由触发器拒绝）。
-module(cs_pg_seat).

-export([
    fetch_identity_function/2,
    insert_seat/2,
    fetch_seat/2,
    list_dispatchable_seats/1,
    list_dispatchable_seats_page/3,
    list_all_seats_page/3,
    set_seat_enabled/4,
    insert_event/2,
    insert_event_in/3,
    fetch_event_scope/2,
    list_events_page/4,
    event_watermark/2,
    provision_seat/3,
    list_seat_org_contexts/1,
    list_transfer_targets_page/4,
    %% CS-BE-06：席位 entitlement（组织级人工 seat_limit；并发安全检查）
    seat_limit/2,
    set_seat_limit/3,
    create_seat_limit_tx/6,
    set_enabled_limit_tx/5,
    %% CS-BE-05：presence 心跳 lease（持久/共享可见运行态事实输入）
    heartbeat_seat/4,
    set_seat_manual_status/4,
    fetch_seat_presence/2,
    list_seat_presence/1,
    sql_statements/0
]).

-define(SEAT_KEYS, [
    organization_id,
    business_identity_id,
    function_key,
    enabled,
    max_concurrent,
    version,
    created_at,
    updated_at
]).

-define(SQL_IDENTITY_FUNCTION, <<
    "SELECT function_key FROM organization_business_identity"
    " WHERE organization_id = $1 AND id = $2"
>>).

-define(SQL_INSERT_SEAT, <<
    "INSERT INTO customer_service_seat"
    " (organization_id, business_identity_id, function_key, enabled, max_concurrent,"
    "  created_by_user_id)"
    " VALUES ($1, $2, 'customer_service', $3, $4, $5)"
>>).

-define(SQL_FETCH_SEAT, <<
    "SELECT organization_id, business_identity_id, function_key, enabled, max_concurrent,"
    "       version,"
    "       extract(epoch from created_at)::bigint AS created_at,"
    "       extract(epoch from updated_at)::bigint AS updated_at"
    "  FROM customer_service_seat"
    " WHERE organization_id = $1 AND business_identity_id = $2"
>>).

%% ===================================================================
%% CS-BE-05：presence 心跳 lease（SQL 宏）
%% ===================================================================

%% ===================================================================
%% CS-BE-06：席位 entitlement（组织级人工 seat_limit；CS-DEC-03）
%% ===================================================================

-define(SQL_SEAT_LIMIT_FETCH, <<
    "SELECT seat_limit FROM customer_service_seat_limit"
    " WHERE organization_id = $1"
>>).

-define(SQL_SEAT_LIMIT_UPSERT, <<
    "INSERT INTO customer_service_seat_limit (organization_id, seat_limit, updated_at)"
    " VALUES ($1, $2, CURRENT_TIMESTAMP)"
    " ON CONFLICT (organization_id) DO UPDATE"
    "   SET seat_limit = EXCLUDED.seat_limit, updated_at = CURRENT_TIMESTAMP"
>>).

-define(SQL_SEAT_LIMIT_DELETE, <<
    "DELETE FROM customer_service_seat_limit WHERE organization_id = $1"
>>).

%% enabled 坐席现算计数（used；无冗余计数列，与 CS-BE-04 同纪律）。
-define(SQL_SEAT_ENABLED_COUNT, <<
    "SELECT count(*) AS n FROM customer_service_seat"
    " WHERE organization_id = $1 AND enabled = true"
>>).

%% 心跳 upsert：只刷新 last_heartbeat_at（manual_status 不动——手动 away
%% 不会被周期心跳冲掉）；行不存在则创建（首次心跳）。
-define(SQL_PRESENCE_HEARTBEAT, <<
    "INSERT INTO customer_service_seat_presence"
    " (organization_id, business_identity_id, last_heartbeat_at, updated_at)"
    " VALUES ($1, $2, to_timestamp($3), CURRENT_TIMESTAMP)"
    " ON CONFLICT (organization_id, business_identity_id)"
    " DO UPDATE SET last_heartbeat_at = EXCLUDED.last_heartbeat_at,"
    "               updated_at = CURRENT_TIMESTAMP"
>>).

%% 手动状态 set/clear：ManualStatus 为 'away' 或 NULL（clear）。行不存在
%% 则创建（last_heartbeat_at = At——设置 away 时人以在线事实为锚）。
-define(SQL_PRESENCE_MANUAL_SET, <<
    "INSERT INTO customer_service_seat_presence"
    " (organization_id, business_identity_id, last_heartbeat_at, manual_status,"
    "  updated_at)"
    " VALUES ($1, $2, to_timestamp($3), $4, CURRENT_TIMESTAMP)"
    " ON CONFLICT (organization_id, business_identity_id)"
    " DO UPDATE SET manual_status = EXCLUDED.manual_status,"
    "               updated_at = CURRENT_TIMESTAMP"
>>).

%% 单坐席 presence 快照（含派生输入：seat enabled/max_concurrent + 活跃计数）。
%% LEFT JOIN：无 presence 行时心跳/手动列为 NULL（从未上报 → offline）；
%% seat 行不存在 → 0 行（not_found）。
-define(SQL_PRESENCE_FETCH, <<
    "SELECT p.organization_id, p.business_identity_id,"
    "       extract(epoch from p.last_heartbeat_at)::bigint AS last_heartbeat_at,"
    "       p.manual_status, s.enabled, s.max_concurrent,"
    "       (SELECT count(*) FROM customer_service_session x"
    "         WHERE x.organization_id = s.organization_id"
    "           AND x.business_identity_id = s.business_identity_id"
    "           AND x.status = 'active') AS active_count"
    "  FROM customer_service_seat s"
    "  LEFT JOIN customer_service_seat_presence p"
    "    ON p.organization_id = s.organization_id"
    "   AND p.business_identity_id = s.business_identity_id"
    " WHERE s.organization_id = $1 AND s.business_identity_id = $2"
>>).

%% Org 级 presence 行投影（cs_presence:annotate/3 的输入；仅事实输入，
%% 派生在应用层完成）。
-define(SQL_PRESENCE_LIST, <<
    "SELECT organization_id, business_identity_id,"
    "       extract(epoch from last_heartbeat_at)::bigint AS last_heartbeat_at,"
    "       manual_status"
    "  FROM customer_service_seat_presence"
    " WHERE organization_id = $1"
>>).

-define(SQL_LIST_DISPATCHABLE, <<
    "SELECT s.organization_id, s.business_identity_id, s.function_key, s.enabled,"
    "       s.max_concurrent, s.version,"
    "       (SELECT count(*) FROM customer_service_session x"
    "         WHERE x.organization_id = s.organization_id"
    "           AND x.business_identity_id = s.business_identity_id"
    "           AND x.status = 'active') AS active_count,"
    "       extract(epoch from s.created_at)::bigint AS created_at,"
    "       extract(epoch from s.updated_at)::bigint AS updated_at"
    "  FROM customer_service_seat s"
    " WHERE s.organization_id = $1 AND s.enabled = true"
    " ORDER BY s.business_identity_id"
>>).

%% C4（contracts-w2）键集下推（eb_pg_message_ext 模板口径：`游标 > $2` 升序 +
%% `LIMIT $3`），同语句带 Org 且仅 enabled 坐席；绝无 OFFSET。
-define(SQL_LIST_DISPATCHABLE_PAGE, <<
    "SELECT s.organization_id, s.business_identity_id, s.function_key, s.enabled,"
    "       s.max_concurrent,"
    "       (SELECT count(*) FROM customer_service_session x"
    "         WHERE x.organization_id = s.organization_id"
    "           AND x.business_identity_id = s.business_identity_id"
    "           AND x.status = 'active') AS active_count"
    "  FROM customer_service_seat s"
    " WHERE s.organization_id = $1 AND s.enabled = true"
    "   AND s.business_identity_id > $2"
    " ORDER BY s.business_identity_id"
    " LIMIT $3"
>>).

%% 平台运营面坐席分页（跨企业）：$1=0 全局 / >0 收窄单企业（显式 ::bigint cast
%% 防多使用点参数类型推导不一致）；含已停用坐席；键集 business_identity_id
%% 升序（TSID 全局唯一）。行投影带企业名/坐席显示名/默认 active Workspace
%% （suspend/resume 审计事件的服务端落点——LATERAL 取该 Org 最小 active id）。
-define(SQL_LIST_ALL_SEATS_PAGE, <<
    "SELECT s.organization_id,"
    "       o.name AS organization_name,"
    "       i.display_name,"
    "       s.business_identity_id, s.function_key, s.enabled, s.max_concurrent,"
    "       ws.workspace_id,"
    "       (SELECT count(*) FROM customer_service_session x"
    "         WHERE x.organization_id = s.organization_id"
    "           AND x.business_identity_id = s.business_identity_id"
    "           AND x.status = 'active') AS active_count"
    "  FROM customer_service_seat s"
    "  JOIN organization o ON o.id = s.organization_id"
    "  LEFT JOIN organization_business_identity i"
    "    ON i.organization_id = s.organization_id AND i.id = s.business_identity_id"
    "  LEFT JOIN LATERAL (SELECT w.id AS workspace_id"
    "                       FROM workspace w"
    "                      WHERE w.organization_id = s.organization_id"
    "                        AND w.status = 'active'"
    "                      ORDER BY w.id LIMIT 1) ws ON true"
    " WHERE ($1::bigint = 0 OR s.organization_id = $1::bigint)"
    "   AND s.business_identity_id > $2"
    " ORDER BY s.business_identity_id"
    " LIMIT $3"
>>).

-define(SQL_SET_ENABLED, <<
    "UPDATE customer_service_seat"
    "   SET enabled = $3, version = version + 1, updated_at = to_timestamp($4)"
    " WHERE organization_id = $1 AND business_identity_id = $2"
>>).

%% BE-S01a：用户维度坐席上下文聚合（四张事实表单语句同过滤——member active、
%% org active、assignment active+customer_service、seat enabled；无坐席身份的
%% Org 行 LEFT JOIN 出 NULL，application 投影为 seat_enabled=false）。
-define(SQL_SEAT_ORG_CONTEXTS, <<
    "SELECT m.organization_id,"
    "       o.name AS organization_name,"
    "       a.business_identity_id,"
    "       COALESCE(s.enabled, false) AS seat_enabled,"
    "       (SELECT json_agg(json_build_object('id', w.id, 'name', w.name) ORDER BY w.id)"
    "          FROM workspace w"
    "         WHERE w.organization_id = m.organization_id AND w.status = 'active')"
    "         AS workspaces"
    "  FROM organization_member m"
    "  JOIN organization o ON o.id = m.organization_id AND o.status = 'active'"
    "  LEFT JOIN organization_business_identity_assignment a"
    "    ON a.organization_id = m.organization_id AND a.user_id = m.user_id"
    "   AND a.status = 'active' AND a.function_key = 'customer_service'"
    "  LEFT JOIN customer_service_seat s"
    "    ON s.organization_id = a.organization_id"
    "   AND s.business_identity_id = a.business_identity_id AND s.enabled = true"
    " WHERE m.user_id = $1 AND m.status = 'active'"
    " ORDER BY m.organization_id"
>>).

%% BE-S01a：转接目标分页（键集下推 + 同语句 active 计数；排除调用者本人）。
-define(SQL_TRANSFER_TARGETS_PAGE, <<
    "SELECT s.business_identity_id,"
    "       i.display_name,"
    "       s.max_concurrent,"
    "       (SELECT count(*) FROM customer_service_session x"
    "         WHERE x.organization_id = s.organization_id"
    "           AND x.business_identity_id = s.business_identity_id"
    "           AND x.status = 'active') AS active_count"
    "  FROM customer_service_seat s"
    "  JOIN organization_business_identity i"
    "    ON i.organization_id = s.organization_id AND i.id = s.business_identity_id"
    " WHERE s.organization_id = $1 AND s.enabled = true"
    "   AND s.business_identity_id <> $2"
    "   AND s.business_identity_id > $3"
    " ORDER BY s.business_identity_id"
    " LIMIT $4"
>>).

-define(SQL_INSERT_EVENT, <<
    "INSERT INTO customer_service_event"
    " (id, organization_id, workspace_id, session_id, business_identity_id, actor_user_id,"
    "  actor_kind, action, detail)"
    " VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9::jsonb)"
    " RETURNING id"
>>).

%% BE-S01b（sse-event-contract）：游标作用域裁决——按 id 全局唯一读
%% (organization_id, workspace_id)，跨租户/跨 Workspace 游标由此逐字比对。
-define(SQL_FETCH_EVENT_SCOPE, <<
    "SELECT organization_id, workspace_id FROM customer_service_event WHERE id = $1"
>>).

%% BE-S01b：SSE 键集读页（迁移 135 的 i_cse_org_ws_id 唯一入口；升序 = 乱序
%% 不产生；`id > $3` = 不重）。同语句绑定 (Org, Workspace)。
-define(SQL_LIST_EVENTS_PAGE, <<
    "SELECT id, organization_id, workspace_id, session_id, business_identity_id,"
    "       actor_kind, action, detail,"
    "       extract(epoch from created_at)::bigint AS created_at"
    "  FROM customer_service_event"
    " WHERE organization_id = $1 AND workspace_id = $2 AND id > $3"
    " ORDER BY id ASC"
    " LIMIT $4"
>>).

%% BE-S01b：当前水位（resync 后从这里继续，不重放历史）。
-define(SQL_EVENT_WATERMARK, <<
    "SELECT COALESCE(max(id), 0) AS watermark FROM customer_service_event"
    " WHERE organization_id = $1 AND workspace_id = $2"
>>).

%% BE-S01b（admin provisioning）：事务化开通/修复坐席。
%% 1) workspace 归属门（Org 内 active workspace 必须存在，否则 fail）；
%% 2) (Org, user, customer_service) 的 active identity+assignment 复用查询；
%% 3) 缺失则建 identity（display_name 幂等性由调用方语义承担——重复开通用户
%%    命中步骤 2，不会走到这里）；
%% 4) 建 active assignment；
%% 5) seat upsert（PK=business_identity_id；已存在但 disabled ⇒ 修复 enabled）；
%% 6) 审计事件同事务落库（before/after 进 detail）。
-define(SQL_PROVISION_GUARD_WS, <<
    "SELECT 1 FROM workspace"
    " WHERE organization_id = $1 AND id = $2 AND status = 'active'"
>>).

%% 开通前提（api-surface-freeze authoritative checks）：目标 user 必须已是
%% 本 Org 的 active member——provisioning 是「成员变坐席」，不是成员创建。
-define(SQL_PROVISION_GUARD_MEMBER, <<
    "SELECT 1 FROM organization_member"
    " WHERE organization_id = $1 AND user_id = $2 AND status = 'active'"
>>).

-define(SQL_PROVISION_FIND_IDENTITY, <<
    "SELECT i.id, s.enabled"
    "  FROM organization_business_identity_assignment a"
    "  JOIN organization_business_identity i"
    "    ON i.organization_id = a.organization_id AND i.id = a.business_identity_id"
    "   AND i.function_key = 'customer_service' AND i.status = 'active'"
    "  LEFT JOIN customer_service_seat s"
    "    ON s.business_identity_id = i.id"
    " WHERE a.organization_id = $1 AND a.user_id = $2"
    "   AND a.function_key = 'customer_service' AND a.status = 'active'"
    " ORDER BY i.id"
    " LIMIT 1"
>>).

-define(SQL_PROVISION_INSERT_IDENTITY, <<
    "INSERT INTO organization_business_identity"
    " (id, organization_id, function_key, display_name, status, version, created_by_user_id)"
    " VALUES ($1, $2, 'customer_service', $3, 'active', 1, $4)"
    " RETURNING id"
>>).

-define(SQL_PROVISION_INSERT_ASSIGNMENT, <<
    "INSERT INTO organization_business_identity_assignment"
    " (id, organization_id, business_identity_id, function_key, user_id, status,"
    "  assigned_by, version)"
    " VALUES ($1, $2, $3, 'customer_service', $4, 'active', $5, 1)"
    " RETURNING id"
>>).

-define(SQL_PROVISION_UPSERT_SEAT, <<
    "INSERT INTO customer_service_seat"
    " (organization_id, business_identity_id, function_key, enabled, max_concurrent,"
    "  created_by_user_id)"
    " VALUES ($1, $2, 'customer_service', true, $3, $4)"
    " ON CONFLICT (business_identity_id) DO UPDATE"
    "   SET enabled = true, version = customer_service_seat.version + 1, updated_at = now()"
    " RETURNING enabled, max_concurrent"
>>).

%% @doc 冻结语句（供 cs_pg_tests 的租户键机械断言）。
-spec sql_statements() -> [binary()].
sql_statements() ->
    [
        ?SQL_IDENTITY_FUNCTION,
        ?SQL_INSERT_SEAT,
        ?SQL_FETCH_SEAT,
        ?SQL_SEAT_LIMIT_FETCH,
        ?SQL_SEAT_LIMIT_UPSERT,
        ?SQL_SEAT_LIMIT_DELETE,
        ?SQL_SEAT_ENABLED_COUNT,
        ?SQL_PRESENCE_HEARTBEAT,
        ?SQL_PRESENCE_MANUAL_SET,
        ?SQL_PRESENCE_FETCH,
        ?SQL_PRESENCE_LIST,
        ?SQL_LIST_DISPATCHABLE,
        ?SQL_LIST_DISPATCHABLE_PAGE,
        ?SQL_LIST_ALL_SEATS_PAGE,
        ?SQL_SET_ENABLED,
        ?SQL_SEAT_ORG_CONTEXTS,
        ?SQL_TRANSFER_TARGETS_PAGE,
        ?SQL_INSERT_EVENT,
        ?SQL_FETCH_EVENT_SCOPE,
        ?SQL_LIST_EVENTS_PAGE,
        ?SQL_EVENT_WATERMARK,
        ?SQL_PROVISION_GUARD_WS,
        ?SQL_PROVISION_GUARD_MEMBER,
        ?SQL_PROVISION_FIND_IDENTITY,
        ?SQL_PROVISION_INSERT_IDENTITY,
        ?SQL_PROVISION_INSERT_ASSIGNMENT,
        ?SQL_PROVISION_UPSERT_SEAT
    ].

%% ===================================================================
%% CS-BE-05：presence 心跳 lease（持久/共享可见运行态事实输入）
%% ===================================================================

%% @doc 心跳 upsert：只刷新 last_heartbeat_at；At 是服务端派生时钟
%% （epoch 秒，可注入），客户端不可报时。FK 保证仅对存在坐席可写。
-spec heartbeat_seat(integer(), integer(), integer(), undefined) ->
    {ok, map()} | {error, term()}.
heartbeat_seat(OrgId, IdentityId, AtSec, _Opts) ->
    case elib_pg:execute(?SQL_PRESENCE_HEARTBEAT, [OrgId, IdentityId, AtSec]) of
        {ok, _} -> fetch_seat_presence(OrgId, IdentityId);
        {error, Reason} -> {error, cs_pg_common:normalize_error(Reason)}
    end.

%% @doc 手动状态 set/clear：ManualStatus 为 <<"away">>（设 away）或
%% undefined（clear）；AtSec 同 heartbeat 语义（锚定在线事实）。
-spec set_seat_manual_status(integer(), integer(), integer(), binary() | undefined) ->
    {ok, map()} | {error, term()}.
set_seat_manual_status(OrgId, IdentityId, AtSec, ManualStatus) ->
    Manual =
        case ManualStatus of
            <<"away">> -> <<"away">>;
            _ -> undefined
        end,
    case elib_pg:execute(?SQL_PRESENCE_MANUAL_SET, [OrgId, IdentityId, AtSec, Manual]) of
        {ok, _} -> fetch_seat_presence(OrgId, IdentityId);
        {error, Reason} -> {error, cs_pg_common:normalize_error(Reason)}
    end.

%% @doc 单坐席 presence 快照：心跳/手动事实 + 派生输入（enabled /
%% max_concurrent / active_count）。seat 不存在 → not_found；无 presence
%% 行时心跳列为 undefined（从未上报 → offline）。
-spec fetch_seat_presence(integer(), integer()) -> {ok, map()} | {error, term()}.
fetch_seat_presence(OrgId, IdentityId) ->
    Keys = [
        organization_id,
        business_identity_id,
        last_heartbeat_at,
        manual_status,
        enabled,
        max_concurrent,
        active_count
    ],
    cs_pg_common:fetch_one(?SQL_PRESENCE_FETCH, [OrgId, IdentityId], Keys).

%% @doc Org 级 presence 行投影（cs_presence:annotate/3 的输入）。
-spec list_seat_presence(integer()) -> {ok, [map()]} | {error, term()}.
list_seat_presence(OrgId) ->
    cs_pg_common:fetch_many(
        ?SQL_PRESENCE_LIST,
        [OrgId],
        [organization_id, business_identity_id, last_heartbeat_at, manual_status]
    ).

%% ===================================================================
%% CS-BE-06：席位 entitlement（组织级人工 seat_limit；CS-DEC-03 冻结）
%% ===================================================================

%% @doc 读当前 limit：{ok, unlimited}（无行=现存组织默认）| {ok, pos_integer()}。
-spec seat_limit(integer(), pool | pid()) -> {ok, unlimited | pos_integer()} | {error, term()}.
seat_limit(OrgId, _Opts) ->
    case cs_pg_common:fetch_one(?SQL_SEAT_LIMIT_FETCH, [OrgId], [seat_limit]) of
        {ok, #{seat_limit := N}} when is_integer(N), N >= 1 -> {ok, N};
        {ok, _} -> {ok, unlimited};
        {error, not_found} -> {ok, unlimited};
        {error, _} = Err -> Err
    end.

%% @doc 人工配置/清除 limit（undefined=删行→unlimited）。
-spec set_seat_limit(integer(), pos_integer() | undefined, pool | pid()) ->
    {ok, unlimited | pos_integer()} | {error, term()}.
set_seat_limit(OrgId, undefined, _Opts) ->
    case elib_pg:execute(?SQL_SEAT_LIMIT_DELETE, [OrgId]) of
        {ok, _} -> {ok, unlimited};
        {error, Reason} -> {error, cs_pg_common:normalize_error(Reason)}
    end;
set_seat_limit(OrgId, Limit, _Opts) when is_integer(Limit), Limit >= 1 ->
    case elib_pg:execute(?SQL_SEAT_LIMIT_UPSERT, [OrgId, Limit]) of
        {ok, _} -> {ok, Limit};
        {error, Reason} -> {error, cs_pg_common:normalize_error(Reason)}
    end;
set_seat_limit(_OrgId, _Bad, _Opts) ->
    {error, invalid_seat_limit}.

%% per-org 事务锁：并发「计数+插入/翻转」串行化（N 并发恰 N 成功）。
assert_limit_tx(Conn, OrgId) ->
    %% INT-BE-06/CS-BE-06 实测：PG18 无 pg_advisory_xact_lock(int8, int4)
    %% 两参签名（psql 复现 42883）——用单参版（key=OrgId，org 全局唯一即
    %% 全局无碰撞）。
    {ok, _, _} = epgsql:equery(
        Conn, <<"SELECT pg_advisory_xact_lock($1::bigint)">>, [OrgId]
    ),
    case cs_pg_common:fetch_one_conn(Conn, ?SQL_SEAT_ENABLED_COUNT, [OrgId], [n]) of
        {ok, #{n := N}} when is_integer(N) ->
            case seat_limit_tx(Conn, OrgId) of
                {ok, unlimited} -> ok;
                {ok, Limit} when N >= Limit -> {error, seat_limit_exceeded};
                {ok, _Limit} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

seat_limit_tx(Conn, OrgId) ->
    case cs_pg_common:fetch_one_conn(Conn, ?SQL_SEAT_LIMIT_FETCH, [OrgId], [seat_limit]) of
        {ok, #{seat_limit := N}} when is_integer(N), N >= 1 -> {ok, N};
        {ok, _} -> {ok, unlimited};
        {error, not_found} -> {ok, unlimited};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 创建坐席（limit 感知）：advisory 事务锁内「count enabled + 检查 +
%% INSERT」原子完成——N 并发开第 N+1 个坐席恰一失败（seat_limit_exceeded）。
-spec create_seat_limit_tx(
    pid(), integer(), integer(), boolean(), pos_integer(), term()
) -> {ok, map()} | {error, seat_limit_exceeded | term()}.
create_seat_limit_tx(_Conn, OrgId, IdentityId, Enabled, MaxConcurrent, CreatedBy) ->
    elib_pg:with_tx(fun(Conn1) ->
        case Enabled of
            false ->
                insert_seat_in(Conn1, OrgId, IdentityId, Enabled, MaxConcurrent, CreatedBy);
            true ->
                case assert_limit_tx(Conn1, OrgId) of
                    ok ->
                        insert_seat_in(
                            Conn1, OrgId, IdentityId, Enabled, MaxConcurrent, CreatedBy
                        );
                    {error, Reason} ->
                        throw({rollback, {error, Reason}})
                end
        end
    end).

%% @doc enabled 翻转（limit 感知）：false→true 是「增」（超 limit 拒）；
%% true→false 是「减」（存量超额可减，永不检查）。
-spec set_enabled_limit_tx(
    pid(), integer(), integer(), boolean(), term()
) -> {ok, map()} | {error, seat_limit_exceeded | not_found | term()}.
set_enabled_limit_tx(_Conn, OrgId, IdentityId, Enabled, At) ->
    elib_pg:with_tx(fun(Conn1) ->
        case Enabled of
            false ->
                do_set_enabled(Conn1, OrgId, IdentityId, false, At);
            true ->
                case is_seat_enabled(Conn1, OrgId, IdentityId) of
                    {ok, true} ->
                        %% 已启用：重放（幂等 provisioning），不重复计数不检查。
                        do_set_enabled(Conn1, OrgId, IdentityId, true, At);
                    {ok, false} ->
                        case assert_limit_tx(Conn1, OrgId) of
                            ok ->
                                do_set_enabled(Conn1, OrgId, IdentityId, true, At);
                            {error, Reason} ->
                                throw({rollback, {error, Reason}})
                        end;
                    {error, not_found} = E ->
                        throw({rollback, E});
                    {error, Reason} ->
                        throw({rollback, {error, Reason}})
                end
        end
    end).

is_seat_enabled(Conn, OrgId, IdentityId) ->
    case
        cs_pg_common:fetch_one_conn(
            Conn,
            <<
                "SELECT enabled FROM customer_service_seat"
                " WHERE organization_id = $1 AND business_identity_id = $2"
            >>,
            [OrgId, IdentityId],
            [enabled]
        )
    of
        {ok, #{enabled := Enabled}} -> {ok, Enabled};
        {error, not_found} = E -> E;
        {error, Reason} -> {error, Reason}
    end.

do_set_enabled(Conn, OrgId, IdentityId, Enabled, At) ->
    case elib_pg:execute(Conn, ?SQL_SET_ENABLED, [OrgId, IdentityId, Enabled, At]) of
        {ok, 1} -> fetch_seat_in(Conn, OrgId, IdentityId);
        {ok, 0} -> throw({rollback, {error, not_found}});
        {error, Reason} -> throw({rollback, {error, cs_pg_common:normalize_error(Reason)}})
    end.

%% 事务内 INSERT（create_seat_limit_tx 用；形状同池化 insert_seat）。
insert_seat_in(Conn, OrgId, IdentityId, Enabled, MaxConcurrent, CreatedBy) ->
    Params = [OrgId, IdentityId, Enabled, MaxConcurrent, cs_pg_common:nullify(CreatedBy)],
    case elib_pg:execute(Conn, ?SQL_INSERT_SEAT, Params) of
        {ok, 1} -> fetch_seat_in(Conn, OrgId, IdentityId);
        {ok, 0} -> throw({rollback, {error, insert_failed}});
        {error, Reason} -> throw({rollback, {error, cs_pg_common:normalize_error(Reason)}})
    end.

%% 事务内读回（fetch_seat 的 Conn 版）。
fetch_seat_in(Conn, OrgId, IdentityId) ->
    case cs_pg_common:fetch_one_conn(Conn, ?SQL_FETCH_SEAT, [OrgId, IdentityId], ?SEAT_KEYS) of
        {ok, Row} ->
            {ok, Row#{function_key => cs_pg_common:to_status(maps:get(function_key, Row))}};
        {error, Reason} ->
            throw({rollback, {error, Reason}})
    end.

%% ===================================================================
%% identity 事实（A01 应用侧前置校验数据源）
%% ===================================================================

-spec fetch_identity_function(integer(), integer()) -> {ok, binary()} | {error, term()}.
fetch_identity_function(OrgId, IdentityId) ->
    case cs_pg_common:fetch_one(?SQL_IDENTITY_FUNCTION, [OrgId, IdentityId], [function_key]) of
        {ok, #{function_key := FunctionKey}} -> {ok, FunctionKey};
        {error, _} = Err -> Err
    end.

%% ===================================================================
%% seat
%% ===================================================================

-spec insert_seat(integer(), map()) -> {ok, map()} | {error, term()}.
insert_seat(OrgId, Seat) when is_map(Seat) ->
    IdentityId = maps:get(business_identity_id, Seat),
    Params = [
        OrgId,
        IdentityId,
        maps:get(enabled, Seat, true),
        maps:get(max_concurrent, Seat, 1),
        cs_pg_common:nullify(maps:get(created_by_user_id, Seat, undefined))
    ],
    case elib_pg:execute(?SQL_INSERT_SEAT, Params) of
        {ok, 1} -> fetch_seat(OrgId, IdentityId);
        {ok, 0} -> {error, no_row};
        {error, Reason} -> {error, cs_pg_common:normalize_error(Reason)}
    end;
insert_seat(_OrgId, _Seat) ->
    {error, invalid_seat}.

-spec fetch_seat(integer(), integer()) -> {ok, map()} | {error, term()}.
fetch_seat(OrgId, IdentityId) ->
    case cs_pg_common:fetch_one(?SQL_FETCH_SEAT, [OrgId, IdentityId], ?SEAT_KEYS) of
        {ok, Row} ->
            {ok, Row#{function_key => cs_pg_common:to_status(maps:get(function_key, Row))}};
        {error, _} = Err ->
            Err
    end.

-spec list_dispatchable_seats(integer()) -> {ok, [map()]} | {error, term()}.
list_dispatchable_seats(OrgId) ->
    case cs_pg_common:fetch_many(?SQL_LIST_DISPATCHABLE, [OrgId], ?SEAT_KEYS ++ [active_count]) of
        {ok, Rows} ->
            {ok, [
                Row#{function_key => cs_pg_common:to_status(maps:get(function_key, Row))}
             || Row <- Rows
            ]};
        {error, _} = Err ->
            Err
    end.

%% @doc C4（contracts-w2）：seat 键集分页（business_identity_id 升序；
%% 同语句带 Org，仅 enabled）。行投影字段与派单快照一致（active_count 同语句计数）。
-spec list_dispatchable_seats_page(integer(), non_neg_integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
list_dispatchable_seats_page(OrgId, AfterId, Limit) ->
    PageKeys = [
        organization_id,
        business_identity_id,
        function_key,
        enabled,
        max_concurrent,
        active_count
    ],
    case cs_pg_common:fetch_many(?SQL_LIST_DISPATCHABLE_PAGE, [OrgId, AfterId, Limit], PageKeys) of
        {ok, Rows} ->
            {ok, [
                Row#{function_key => cs_pg_common:to_status(maps:get(function_key, Row))}
             || Row <- Rows
            ]};
        {error, _} = Err ->
            Err
    end.

%% @doc 平台运营面坐席分页（跨企业可选 Org 过滤；含已停用——运营面要能定位
%% 并恢复）。键集 business_identity_id 升序（TSID 全局唯一，跨企业游标不重
%% 不漏）；行投影带 organization_name / display_name / 默认 active workspace_id。
-spec list_all_seats_page(non_neg_integer(), non_neg_integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
list_all_seats_page(OrgFilter, AfterId, Limit) ->
    Keys = [
        organization_id,
        organization_name,
        display_name,
        business_identity_id,
        function_key,
        enabled,
        max_concurrent,
        active_count,
        workspace_id
    ],
    case cs_pg_common:fetch_many(?SQL_LIST_ALL_SEATS_PAGE, [OrgFilter, AfterId, Limit], Keys) of
        {ok, Rows} ->
            {ok, [
                Row#{function_key => cs_pg_common:to_status(maps:get(function_key, Row))}
             || Row <- Rows
            ]};
        {error, _} = Err ->
            Err
    end.

-spec set_seat_enabled(integer(), integer(), boolean(), integer()) -> {ok, map()} | {error, term()}.
set_seat_enabled(OrgId, IdentityId, Enabled, At) ->
    Params = [OrgId, IdentityId, Enabled, At],
    case elib_pg:execute(?SQL_SET_ENABLED, Params) of
        {ok, 1} -> fetch_seat(OrgId, IdentityId);
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, cs_pg_common:normalize_error(Reason)}
    end.

%% ===================================================================
%% BE-S01a：坐席上下文聚合 / 转接目标（主体自身作用域 + 最小投影）
%% ===================================================================

%% @doc 用户维度的坐席上下文聚合（无坐席身份的 Org 行 business_identity_id
%% 为 undefined、seat_enabled=false——application 据此区分「成员未开通」）。
-spec list_seat_org_contexts(integer()) -> {ok, [map()]} | {error, term()}.
list_seat_org_contexts(UserId) when is_integer(UserId) ->
    Keys = [organization_id, organization_name, business_identity_id, seat_enabled, workspaces],
    case cs_pg_common:fetch_many(?SQL_SEAT_ORG_CONTEXTS, [UserId], Keys) of
        {ok, Rows} ->
            {ok, [decode_workspaces(Row) || Row <- Rows]};
        {error, _} = Err ->
            Err
    end;
list_seat_org_contexts(UserId) ->
    {error, {invalid_argument, {user_id, UserId}}}.

%% epgsql 解 json_agg 列：binary JSON → map（null → []——无 active Workspace
%% 的 Org 给空列表，客户端自行提示不可用）。
decode_workspaces(Row) ->
    case maps:get(workspaces, Row, undefined) of
        Bin when is_binary(Bin) ->
            try
                Row#{workspaces := jsone:decode(Bin)}
            catch
                _:_ -> Row#{workspaces := []}
            end;
        null ->
            Row#{workspaces := []};
        List when is_list(List) ->
            Row;
        _Other ->
            Row#{workspaces := []}
    end.

%% @doc 转接目标分页（键集下推 + 同语句 active 计数；排除调用者本人）。
-spec list_transfer_targets_page(integer(), integer(), non_neg_integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
list_transfer_targets_page(OrgId, ExcludeIdentityId, AfterId, Limit) when
    is_integer(OrgId), is_integer(ExcludeIdentityId)
->
    Keys = [business_identity_id, display_name, max_concurrent, active_count],
    cs_pg_common:fetch_many(
        ?SQL_TRANSFER_TARGETS_PAGE, [OrgId, ExcludeIdentityId, AfterId, Limit], Keys
    );
list_transfer_targets_page(OrgId, _Exclude, _After, _Limit) ->
    {error, {invalid_organization_id, OrgId}}.

%% ===================================================================
%% event（append-only；只 INSERT）
%% ===================================================================

%% @doc 独立事务追加客服状态审计事件。
-spec insert_event(integer(), map()) -> {ok, integer()} | {error, term()}.
insert_event(OrgId, Event) when is_integer(OrgId), is_map(Event) ->
    case elib_pg:execute(?SQL_INSERT_EVENT, event_params(OrgId, Event)) of
        {ok, _Count, Rows} -> {ok, first_id(Rows)};
        {error, Reason} -> {error, cs_pg_common:normalize_error(Reason)}
    end;
insert_event(OrgId, _Event) ->
    {error, {invalid_organization_id, OrgId}}.

%% @doc 在调用方事务内追加事件（状态推进 + 审计原子提交）。
-spec insert_event_in(term(), integer(), map()) -> {ok, integer()} | {error, term()}.
insert_event_in(Conn, OrgId, Event) when is_integer(OrgId), is_map(Event) ->
    case elib_pg:execute(Conn, ?SQL_INSERT_EVENT, event_params(OrgId, Event)) of
        {ok, _Count, Rows} -> {ok, first_id(Rows)};
        {error, Reason} -> {error, cs_pg_common:normalize_error(Reason)}
    end;
insert_event_in(_Conn, OrgId, _Event) ->
    {error, {invalid_organization_id, OrgId}}.

%% ===================================================================
%% BE-S01b：SSE 事件读取（游标裁决 / 键集读页 / 水位）
%% ===================================================================

%% @doc 按事件 id 读作用域（sse-event-contract 游标裁决的数据面）：
%% 行存在 ⇒ `{ok, #{organization_id, workspace_id}}`（调用方逐字比对）；
%% 不存在 ⇒ `{error, not_found}`（游标缺失/超窗 ⇒ resync 面）。
-spec fetch_event_scope(integer(), integer()) ->
    {ok, #{organization_id := integer(), workspace_id := integer()}} | {error, term()}.
fetch_event_scope(OrgId, EventId) when is_integer(OrgId), is_integer(EventId) ->
    Keys = [organization_id, workspace_id],
    cs_pg_common:fetch_one(?SQL_FETCH_EVENT_SCOPE, [EventId], Keys);
fetch_event_scope(OrgId, _EventId) ->
    {error, {invalid_organization_id, OrgId}}.

%% @doc SSE 补偿/轮询读页（键集下推，升序 = 乱序不产生，`id >` = 不重）。
%% 行键投影与 append_event 写入面同源；detail 归一为 map（jsonb 读出 binary）。
-spec list_events_page(integer(), integer(), non_neg_integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
list_events_page(OrgId, WorkspaceId, AfterId, Limit) when
    is_integer(OrgId), is_integer(WorkspaceId), is_integer(AfterId), is_integer(Limit)
->
    Keys = [
        id,
        organization_id,
        workspace_id,
        session_id,
        business_identity_id,
        actor_kind,
        action,
        detail,
        created_at
    ],
    case
        cs_pg_common:fetch_many(?SQL_LIST_EVENTS_PAGE, [OrgId, WorkspaceId, AfterId, Limit], Keys)
    of
        {ok, Rows} -> {ok, [decode_detail(Row) || Row <- Rows]};
        {error, _} = Err -> Err
    end;
list_events_page(OrgId, _Ws, _After, _Limit) ->
    {error, {invalid_organization_id, OrgId}}.

decode_detail(Row) ->
    case maps:get(detail, Row, undefined) of
        Bin when is_binary(Bin) ->
            Row#{detail := cs_pg_common:jsonb_read(Bin)};
        Map when is_map(Map) ->
            Row;
        _Other ->
            Row#{detail := #{}}
    end.

%% @doc 当前水位（(Org, Workspace) 内最大事件 id；空域 0）。
-spec event_watermark(integer(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
event_watermark(OrgId, WorkspaceId) when is_integer(OrgId), is_integer(WorkspaceId) ->
    case cs_pg_common:fetch_one(?SQL_EVENT_WATERMARK, [OrgId, WorkspaceId], [watermark]) of
        {ok, #{watermark := W}} when is_integer(W) -> {ok, W};
        {ok, _} -> {ok, 0};
        {error, _} = Err -> Err
    end;
event_watermark(OrgId, _Ws) ->
    {error, {invalid_organization_id, OrgId}}.

%% ===================================================================
%% BE-S01b：Admin provisioning（单事务开通/修复 identity+assignment+seat）
%% ===================================================================

-spec provision_seat(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
provision_seat(OrgId, WorkspaceId, Provision) when
    is_integer(OrgId), is_integer(WorkspaceId), is_map(Provision)
->
    undo_rollback(
        elib_pg:with_tx(fun(Conn) -> provision_tx(Conn, OrgId, WorkspaceId, Provision) end)
    );
provision_seat(OrgId, _Ws, _Provision) ->
    {error, {invalid_organization_id, OrgId}}.

%% elib_pg 的业务回滚信号 `{rollback, Reason}`：Reason 就是调用方的返回形状，
%% 原样透传（cs_pg_session 同款，绝不二次包裹）。
undo_rollback({rollback, Reason}) -> Reason;
undo_rollback(Other) -> Other.

provision_tx(Conn, OrgId, WorkspaceId, Provision) ->
    ok = provision_guard_workspace(Conn, OrgId, WorkspaceId),
    ok = provision_guard_member(Conn, OrgId, maps:get(user_id, Provision)),
    case elib_pg:query(Conn, ?SQL_PROVISION_FIND_IDENTITY, [OrgId, maps:get(user_id, Provision)]) of
        {ok, [Row | _]} ->
            %% CS-BE-06：existing 分支——disabled→enabled 修复是「增」，
            %% 先锁内检查；已 enabled 重放不重复计数不检查。
            case before_seat(Row) of
                enabled ->
                    ok;
                _ ->
                    case assert_limit_tx(Conn, OrgId) of
                        ok -> ok;
                        {error, ReasonL} -> throw({rollback, {error, ReasonL}})
                    end
            end,
            provision_existing(Conn, OrgId, WorkspaceId, Provision, Row);
        {ok, []} ->
            %% CS-BE-06：create 分支（新 identity + enabled seat）按 limit 检查。
            case assert_limit_tx(Conn, OrgId) of
                ok -> ok;
                {error, ReasonL} -> throw({rollback, {error, ReasonL}})
            end,
            provision_create(Conn, OrgId, WorkspaceId, Provision);
        {error, Reason} ->
            throw({rollback, {error, cs_pg_common:normalize_error(Reason)}})
    end.

provision_guard_member(Conn, OrgId, UserId) when is_integer(UserId) ->
    case elib_pg:query(Conn, ?SQL_PROVISION_GUARD_MEMBER, [OrgId, UserId]) of
        {ok, [_ | _]} ->
            ok;
        {ok, []} ->
            throw({rollback, {error, {not_found, member}}});
        {error, Reason} ->
            throw({rollback, {error, cs_pg_common:normalize_error(Reason)}})
    end;
provision_guard_member(_Conn, _OrgId, _Bad) ->
    throw({rollback, {error, {invalid_argument, {user_id, _Bad}}}}).

provision_guard_workspace(Conn, OrgId, WorkspaceId) ->
    case elib_pg:query(Conn, ?SQL_PROVISION_GUARD_WS, [OrgId, WorkspaceId]) of
        {ok, [_ | _]} ->
            ok;
        {ok, []} ->
            throw({rollback, {error, {not_found, workspace}}});
        {error, Reason} ->
            throw({rollback, {error, cs_pg_common:normalize_error(Reason)}})
    end.

%% 既有事实（幂等路径）：不重复创建 identity/assignment；seat 不存在则补建、
%% disabled 则修复 enabled=true（"修复"语义）；审计记 before/after。
provision_existing(Conn, OrgId, WorkspaceId, Provision, Row) ->
    IdentityId = maps:get(<<"id">>, Row),
    Before = before_seat(Row),
    %% upsert_seat_in 失败即内部 throw rollback（见下），不返回 {error, _}。
    {ok, Seat} = upsert_seat_in(Conn, OrgId, IdentityId, Provision),
    finish_provision(Conn, OrgId, WorkspaceId, Provision, IdentityId, Before, Seat, false).

before_seat(Row) ->
    case maps:get(<<"enabled">>, Row, null) of
        null -> absent;
        true -> enabled;
        false -> disabled
    end.

%% 全新开通：identity + assignment + seat 三写（同事务，任一失败全回滚）。
provision_create(Conn, OrgId, WorkspaceId, Provision) ->
    IdentityId = cs_tsid:new_id(business_identity),
    insert_identity_in(Conn, OrgId, Provision, IdentityId),
    insert_assignment_in(Conn, OrgId, Provision, IdentityId),
    {ok, Seat} = upsert_seat_in(Conn, OrgId, IdentityId, Provision),
    finish_provision(Conn, OrgId, WorkspaceId, Provision, IdentityId, absent, Seat, true).

insert_identity_in(Conn, OrgId, Provision, IdentityId) ->
    Params = [
        IdentityId,
        OrgId,
        maps:get(display_name, Provision),
        cs_pg_common:nullify(maps:get(actor_user_id, Provision, undefined))
    ],
    case elib_pg:execute(Conn, ?SQL_PROVISION_INSERT_IDENTITY, Params) of
        {ok, _Count, _Rows} ->
            ok;
        {error, Reason} ->
            throw({rollback, {error, cs_pg_common:normalize_error(Reason)}})
    end.

insert_assignment_in(Conn, OrgId, Provision, IdentityId) ->
    AssignmentId = cs_tsid:new_id(organization_business_identity_assignment),
    Params = [
        AssignmentId,
        OrgId,
        IdentityId,
        maps:get(user_id, Provision),
        cs_pg_common:nullify(maps:get(actor_user_id, Provision, undefined))
    ],
    case elib_pg:execute(Conn, ?SQL_PROVISION_INSERT_ASSIGNMENT, Params) of
        {ok, _Count, _Rows} ->
            ok;
        {error, Reason} ->
            throw({rollback, {error, cs_pg_common:normalize_error(Reason)}})
    end.

upsert_seat_in(Conn, OrgId, IdentityId, Provision) ->
    Params = [
        OrgId,
        IdentityId,
        maps:get(max_concurrent, Provision, 1),
        cs_pg_common:nullify(maps:get(actor_user_id, Provision, undefined))
    ],
    case elib_pg:query(Conn, ?SQL_PROVISION_UPSERT_SEAT, Params) of
        {ok, [Row | _]} ->
            {ok, #{
                enabled => maps:get(<<"enabled">>, Row),
                max_concurrent => maps:get(<<"max_concurrent">>, Row)
            }};
        {ok, []} ->
            throw({rollback, {error, seat_not_found}});
        {error, Reason} ->
            throw({rollback, {error, cs_pg_common:normalize_error(Reason)}})
    end.

%% 审计（不可抵赖：append-only 事件行，trigger 拒绝改写）：actor_kind=
%% platform_admin；adm 身份进 detail（fk_cse_actor 只认 "user"，adm_user_id
%% 不得写 actor_user_id 列）；before/after 进 detail。审计失败 = 全回滚。
finish_provision(Conn, OrgId, WorkspaceId, Provision, IdentityId, Before, Seat, Created) ->
    Event = #{
        business_identity_id => IdentityId,
        actor_kind => <<"platform_admin">>,
        action => <<"platform.provisioned">>,
        detail => #{
            <<"adm_user_id">> => maps:get(adm_user_id, Provision, undefined),
            <<"target_user_id">> => maps:get(user_id, Provision),
            <<"workspace_id">> => WorkspaceId,
            <<"identity_created">> => Created,
            <<"before">> => atom_to_binary(Before, utf8),
            <<"after">> => after_seat(Seat)
        },
        workspace_id => WorkspaceId
    },
    case insert_event_in(Conn, OrgId, Event) of
        {ok, _EventId} ->
            {ok, #{
                organization_id => OrgId,
                workspace_id => WorkspaceId,
                business_identity_id => IdentityId,
                identity_created => Created,
                seat => Seat
            }};
        {error, Reason} ->
            throw({rollback, {error, {event_append_failed, Reason}}})
    end.

after_seat(#{enabled := true}) -> <<"enabled">>;
after_seat(_Other) -> <<"unknown">>.

event_params(OrgId, Event) ->
    [
        maps:get(id, Event, cs_tsid:new_id(cs_event)),
        OrgId,
        %% BE-S01a（迁移 135）：workspace_id NOT NULL——全部写入方
        %% （session/seat/access/widget application）已在事件构造点注入；
        %% 缺失即 23502 由调用方显式失败（不静默猜默认）。
        cs_pg_common:nullify(maps:get(workspace_id, Event, undefined)),
        cs_pg_common:nullify(maps:get(session_id, Event, undefined)),
        cs_pg_common:nullify(maps:get(business_identity_id, Event, undefined)),
        cs_pg_common:nullify(maps:get(actor_user_id, Event, undefined)),
        cs_pg_common:nullify(maps:get(actor_kind, Event, undefined)),
        maps:get(action, Event, <<"unknown">>),
        cs_pg_common:jsonb(maps:get(detail, Event, #{}))
    ].

%% 取首行主键：调用点为 epgsql squery 的 tuple 行 [{Id}]。
%% 不做 is_integer/is_map 多形状宽容——dialyzer 依成功类型判其死代码。
first_id([{Id} | _]) -> Id;
first_id(_) -> undefined.
