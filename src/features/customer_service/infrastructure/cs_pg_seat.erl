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

-define(SQL_INSERT_EVENT, <<
    "INSERT INTO customer_service_event"
    " (id, organization_id, session_id, business_identity_id, actor_user_id,"
    "  actor_kind, action, detail)"
    " VALUES ($1, $2, $3, $4, $5, $6, $7, $8::jsonb)"
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
