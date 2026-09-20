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
    set_seat_enabled/4,
    insert_event/2,
    insert_event_in/3,
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

%% @doc 冻结语句（供 cs_pg_tests 的租户键机械断言）。
-spec sql_statements() -> [binary()].
sql_statements() ->
    [
        ?SQL_IDENTITY_FUNCTION,
        ?SQL_INSERT_SEAT,
        ?SQL_FETCH_SEAT,
        ?SQL_LIST_DISPATCHABLE,
        ?SQL_LIST_DISPATCHABLE_PAGE,
        ?SQL_SET_ENABLED,
        ?SQL_SEAT_ORG_CONTEXTS,
        ?SQL_TRANSFER_TARGETS_PAGE,
        ?SQL_INSERT_EVENT
    ].

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
                Row#{workspaces := jsx:decode(Bin, [return_maps])}
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
