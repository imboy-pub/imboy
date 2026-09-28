%%% @doc 坐席控制台嵌入的 PG 实现（`cs_store_port` 的 seat console 段；
%%% seat-console-embed SC-BE。镜像 `cs_pg_widget` 的口径）。
%%%
%%% 安全不变量：
%%%   * public_seat_console_id 是公开标识（非 secret），嵌入面解析是**全局**
%%%     反查（单占位符 $1，谓词零 Org；organization_id 从命中行输出，权威
%%%     派生租户）——与铁律 6 的租户语句**不同类**，本语句不进 sql_statements/0
%%%     （形状由 cs_seat_console_pg_tests 专属机械断言单独冻结）；
%%%   * 管理面语句每条同语句带 organization_id（`$1`），workspace 同语句
%%%     复核（铁律 6：租户作用域显式贯穿）；
%%%   * 同一 (Org, Workspace) 至多一个 active 控制台：部分唯一索引
%%%     uq_cssc_org_ws_active 裁决，23505 → `{error, conflict}`；
%%%   * 明文 secret 不在本表（表结构上不存在对应列）。
-module(cs_pg_seat_console).

-export([
    insert_seat_console/2,
    fetch_seat_console/3,
    fetch_seat_console_by_public_id_global/1,
    list_seat_consoles_page/4,
    revoke_seat_console/4,
    update_seat_console/5,
    sql_statements/0
]).

-define(CONSOLE_KEYS, [
    id,
    organization_id,
    workspace_id,
    public_seat_console_id,
    allowed_origins,
    status,
    revoked_at,
    version,
    created_at,
    updated_at
]).

-define(SQL_INSERT_CONSOLE, <<
    "INSERT INTO customer_service_seat_console"
    " (id, organization_id, workspace_id, public_seat_console_id,"
    "  allowed_origins, created_by_user_id)"
    " VALUES ($1, $2, $3, $4, $5::jsonb, $6)"
>>).

-define(SQL_FETCH_CONSOLE, <<
    "SELECT id, organization_id, workspace_id, public_seat_console_id,"
    "       allowed_origins, status,"
    "       extract(epoch from revoked_at)::bigint AS revoked_at, version,"
    "       extract(epoch from created_at)::bigint AS created_at,"
    "       extract(epoch from updated_at)::bigint AS updated_at"
    "  FROM customer_service_seat_console"
    " WHERE organization_id = $1 AND workspace_id = $2 AND id = $3"
>>).

%% 嵌入面全局反查（/seat/:public_seat_console_id）：与铁律 6 的租户语句
%% **不同类**——输入只有公开 ID（单占位符 $1 = public_seat_console_id），
%% organization_id 只出现在 SELECT 投影（从行**输出**，权威派生租户），
%% 谓词零 Org（调用方是浏览器，无 Org 可带）。uq_cssc_public_seat_console_id
%% 全局唯一约束保证单行。本语句**不进** sql_statements/0（语义上不适用），
%% 形状由 cs_seat_console_pg_tests 的专属机械断言单独冻结：恰一个占位符且
%% 谓词为 public_seat_console_id = $1。
-define(SQL_FETCH_CONSOLE_BY_PUBLIC_ID_GLOBAL, <<
    "SELECT id, organization_id, workspace_id, public_seat_console_id,"
    "       allowed_origins, status,"
    "       extract(epoch from revoked_at)::bigint AS revoked_at, version,"
    "       extract(epoch from created_at)::bigint AS created_at,"
    "       extract(epoch from updated_at)::bigint AS updated_at"
    "  FROM customer_service_seat_console"
    " WHERE public_seat_console_id = $1"
>>).

-define(SQL_LIST_CONSOLES_PAGE, <<
    "SELECT id, organization_id, workspace_id, public_seat_console_id,"
    "       allowed_origins, status,"
    "       extract(epoch from revoked_at)::bigint AS revoked_at, version,"
    "       extract(epoch from created_at)::bigint AS created_at,"
    "       extract(epoch from updated_at)::bigint AS updated_at"
    "  FROM customer_service_seat_console"
    " WHERE organization_id = $1 AND workspace_id = $2"
    "   AND ($3::bigint = 0 OR id < $3)"
    " ORDER BY id DESC LIMIT $4"
>>).

-define(SQL_REVOKE_CONSOLE, <<
    "UPDATE customer_service_seat_console"
    "   SET status = 'revoked', revoked_at = to_timestamp($4),"
    "       version = version + 1, updated_at = to_timestamp($4)"
    " WHERE organization_id = $1 AND workspace_id = $2 AND id = $3"
    "   AND status = 'active'"
>>).

%% 可编辑投影只含 allowed_origins：public_seat_console_id / workspace_id /
%% status / revoked_at 不在 SET 内（作用域与状态变更没有 HTTP 面）。update
%% 前置的工作区归属复核：同语句 workspace_id 谓词（scope re-verify）。
-define(SQL_UPDATE_CONSOLE, <<
    "UPDATE customer_service_seat_console"
    "   SET allowed_origins = $4::jsonb, version = version + 1,"
    "       updated_at = to_timestamp($5)"
    " WHERE organization_id = $1 AND workspace_id = $2 AND id = $3"
    "   AND status = 'active'"
>>).

%% @doc 冻结语句（供租户键机械断言：每条都同语句带 organization_id）。
-spec sql_statements() -> [binary()].
sql_statements() ->
    [
        ?SQL_INSERT_CONSOLE,
        ?SQL_FETCH_CONSOLE,
        ?SQL_LIST_CONSOLES_PAGE,
        ?SQL_REVOKE_CONSOLE,
        ?SQL_UPDATE_CONSOLE
    ].

%% ===================================================================
%% seat console
%% ===================================================================

-spec insert_seat_console(integer(), map()) -> {ok, map()} | {error, term()}.
insert_seat_console(OrgId, Console) when is_map(Console) ->
    ConsoleId = maps:get(id, Console),
    Params = [
        ConsoleId,
        OrgId,
        maps:get(workspace_id, Console),
        maps:get(public_seat_console_id, Console),
        cs_pg_common:jsonb(maps:get(allowed_origins, Console, [])),
        cs_pg_common:nullify(maps:get(created_by_user_id, Console, undefined))
    ],
    case elib_pg:execute(?SQL_INSERT_CONSOLE, Params) of
        {ok, 1} ->
            fetch_seat_console(OrgId, maps:get(workspace_id, Console), ConsoleId);
        {ok, 0} ->
            {error, no_row};
        {error, Reason} ->
            insert_error(Reason)
    end;
insert_seat_console(_OrgId, _Console) ->
    {error, invalid_seat_console}.

%% 23505 → conflict（uq_cssc_org_ws_active 同 (Org,WS) 活跃槽位唯一 /
%% uq_cssc_public_seat_console_id 公开 ID 全局唯一，同归一 409，不区分泄漏）。
insert_error(Reason) ->
    case cs_pg_common:normalize_error(Reason) of
        {sql, <<"23505">>, _Constraint} -> {error, conflict};
        Normalized -> {error, Normalized}
    end.

-spec fetch_seat_console(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
fetch_seat_console(OrgId, WorkspaceId, ConsoleId) ->
    to_status_row(
        decode_console_jsonb(
            cs_pg_common:fetch_one(
                ?SQL_FETCH_CONSOLE, [OrgId, WorkspaceId, ConsoleId], ?CONSOLE_KEYS
            )
        )
    ).

%% @doc 全局反查（/seat/:id 嵌入面）：无 Org 输入，organization_id 从命中行
%% 输出。不存在 → not_found（application 归一 seat_console_unavailable）。
-spec fetch_seat_console_by_public_id_global(binary()) -> {ok, map()} | {error, term()}.
fetch_seat_console_by_public_id_global(PublicSeatConsoleId) when is_binary(PublicSeatConsoleId) ->
    to_status_row(
        decode_console_jsonb(
            cs_pg_common:fetch_one(
                ?SQL_FETCH_CONSOLE_BY_PUBLIC_ID_GLOBAL,
                [PublicSeatConsoleId],
                ?CONSOLE_KEYS
            )
        )
    );
fetch_seat_console_by_public_id_global(_PublicSeatConsoleId) ->
    {error, {invalid_argument, public_seat_console_id}}.

-spec list_seat_consoles_page(integer(), integer(), non_neg_integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
list_seat_consoles_page(OrgId, WorkspaceId, AfterId, Limit) ->
    case
        cs_pg_common:fetch_many(
            ?SQL_LIST_CONSOLES_PAGE, [OrgId, WorkspaceId, AfterId, Limit], ?CONSOLE_KEYS
        )
    of
        {ok, Rows} ->
            {ok, [finish_console_row(Row) || Row <- Rows]};
        {error, _} = Err ->
            Err
    end.

-spec revoke_seat_console(integer(), integer(), integer(), integer()) -> ok | {error, term()}.
revoke_seat_console(OrgId, WorkspaceId, ConsoleId, At) ->
    update_exactly_one(?SQL_REVOKE_CONSOLE, [OrgId, WorkspaceId, ConsoleId, At]).

%% Updates 投影只含 allowed_origins（不可编辑键不在 SET 内）；status='active'
%% 谓词与 revoke 同口径——已吊销行零命中（0 行 → 应用层 fetch 区分
%% not_found / seat_console_revoked，无存在性枚举差异）。workspace_id 同语句
%% 复核（scope re-verified in same statement）。
-spec update_seat_console(integer(), integer(), integer(), integer(), map()) ->
    {ok, map()} | {error, term()}.
update_seat_console(OrgId, WorkspaceId, ConsoleId, At, Updates) when is_map(Updates) ->
    Params = [
        OrgId,
        WorkspaceId,
        ConsoleId,
        cs_pg_common:jsonb(maps:get(allowed_origins, Updates, [])),
        At
    ],
    case elib_pg:execute(?SQL_UPDATE_CONSOLE, Params) of
        {ok, 1} ->
            fetch_seat_console(OrgId, WorkspaceId, ConsoleId);
        {ok, 0} ->
            %% 0 行 = 不存在 / 错 workspace / 已吊销（status='active' 谓词零
            %% 命中）：fetch 区分，管理面 403 seat_console_revoked 与 404
            %% not_found 语义不混装。
            case fetch_seat_console(OrgId, WorkspaceId, ConsoleId) of
                {ok, #{status := Status}} when Status =/= active ->
                    {error, seat_console_revoked};
                {ok, _} ->
                    {error, not_found};
                {error, _} = Err ->
                    Err
            end;
        {error, Reason} ->
            {error, cs_pg_common:normalize_error(Reason)}
    end;
update_seat_console(_OrgId, _WorkspaceId, _ConsoleId, _At, _Updates) ->
    {error, invalid_seat_console}.

%% ===================================================================
%% 内部辅助
%% ===================================================================

%% fetch_one/fetch_many 只做行归一化；status 列按 cs_pg_common 契约由调用方
%% 转 atom（`active` → active；`revoked` 等 fail-closed 保留 binary）。
to_status_row({ok, Row}) ->
    {ok, maps:update_with(status, fun cs_pg_common:to_status/1, Row)};
to_status_row({error, _} = Err) ->
    Err.

%% console 行终处理：status 归一 + jsonb 列读归一。
%% jsonb 读归一（codec 无关，见 cs_pg_common:jsonb_read/1）：无 json codec 的池
%% 读回文本 binary，须还原为 term（origin 校验只认 list）。
finish_console_row(Row0) ->
    Row = maps:update_with(status, fun cs_pg_common:to_status/1, Row0),
    Row#{
        allowed_origins := cs_pg_common:jsonb_read(maps:get(allowed_origins, Row, []))
    }.

decode_console_jsonb({ok, Row}) ->
    {ok, finish_console_row(Row)};
decode_console_jsonb({error, _} = Err) ->
    Err.

%% 恰写入 1 行才 ok；0 行 = 目标不在本 (Org, Workspace) / 不存在 / 已终态
%% → not_found（幂等裁决由 application 用 fetch 区分）。
update_exactly_one(Sql, Params) ->
    case elib_pg:execute(Sql, Params) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, cs_pg_common:normalize_error(Reason)}
    end.
