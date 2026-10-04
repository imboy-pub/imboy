%%% @doc EB-03R P1/P2/P5/P8：identity / assignment / conversation 的正向持久化能力。
%%%
%%% 依据：R0 的能力反查表（B1「缺 INSERT assignment」、C1「缺 update assignee」、
%%% §5.1 GET conversations、§二「identity list」）。这些能力此前**不存在**，
%%% 调用方只能 `store_capability_missing` fail-closed（负例安全 ≠ 正向完成）。
%%%
%%% 铁律 6：每条 SQL 都同时带 `organization_id` 与 `workspace_id`（`$1`/`$2`）；
%%% 对没有 workspace 列的表（`organization_business_identity` / `..._assignment`），
%%% 用 `workspace` 表在同一语句里做归属校验，绝不退化成「只按 organization_id 查」。
%%% 铁律 7：并发由 DB 唯一索引裁决（`uq_obia_active_identity` /
%%% `uq_obia_active_user_function`），0 行写入一律翻译成 `{error, conflict}`。
-module(eb_pg_identity_ext).

-moduledoc "identity / assignment / conversation 正向持久化能力（EB-03R P1/P2/P5/P8）。".
-export([
    insert_assignment/3,
    list_identities/2,
    list_identities/3,
    update_conversation_assignee/4,
    list_conversations/2,
    sql_statements/0
]).

-define(SQL_INSERT_ASSIGNMENT, <<
    "INSERT INTO organization_business_identity_assignment"
    " (id, organization_id, business_identity_id, function_key, user_id, status,"
    "  assigned_by, version)"
    " SELECT $3, $1, i.id, i.function_key, $6, 'active', $7, 1"
    "   FROM workspace w"
    "   JOIN organization_business_identity i"
    "     ON i.organization_id = $1 AND i.id = $4 AND i.function_key = $5"
    "    AND i.status = 'active'"
    "  WHERE w.organization_id = $1 AND w.id = $2"
    " ON CONFLICT DO NOTHING"
    " RETURNING id"
>>).

-define(SQL_FETCH_ASSIGNMENT, <<
    "SELECT a.id AS assignment_id, a.organization_id, a.business_identity_id, a.function_key,"
    "       a.user_id, a.status, a.assigned_at, a.ended_at, a.version"
    "  FROM organization_business_identity_assignment a"
    "  JOIN workspace w ON w.id = $2 AND w.organization_id = a.organization_id"
    " WHERE a.organization_id = $1 AND a.id = $3"
>>).

%% C5：identity 列表（键集分页下推 + active assignment 同语句 JOIN 投影）。
%%   * 游标：`i.id < $3`（**倒序**键集，与 offboarding 列表同口径），ORDER BY id DESC；
%%     `after_id` 缺省时传 ?TSID_MAX，等价「从最大 id 开始的第一页」。
%%   * LIMIT 是绑定参数（$4），语句里不得出现 OFFSET（TENANCY 机械判据）。
%%   * `uq_obia_active_identity` 保证每 identity 至多一条 active ⇒ LEFT JOIN 不产生
%%     行扇出；无 active 的 identity 该组列全 NULL。
-define(SQL_LIST_IDENTITIES, <<
    "SELECT i.id, i.organization_id, w.id AS workspace_id, i.function_key, i.display_name,"
    "       i.status, i.version, i.created_by_user_id,"
    "       a.id AS aa_assignment_id, a.business_identity_id AS aa_business_identity_id,"
    "       a.user_id AS aa_user_id, a.function_key AS aa_function_key,"
    "       a.status AS aa_status, a.assigned_at AS aa_assigned_at, a.version AS aa_version"
    "  FROM organization_business_identity i"
    "  JOIN workspace w ON w.id = $2 AND w.organization_id = i.organization_id"
    "  LEFT JOIN organization_business_identity_assignment a"
    "         ON a.organization_id = i.organization_id AND a.business_identity_id = i.id"
    "        AND a.status = 'active'"
    " WHERE i.organization_id = $1"
    "   AND i.id < $3"
    " ORDER BY i.id DESC"
    " LIMIT $4"
>>).

%% `after_id` 缺省时的游标哨兵：int64 上界。TSID 生成自毫秒时钟，永远达不到该值，
%% 故 `id < 哨兵` 等价「无游标 = 首页」（与 `id > 0` 在升序模板中的角色对偶）。
-define(TSID_MAX, 9223372036854775807).

%% 分页窗口：缺省 50、上限 200（message / offboarding 同口径）。
-define(DEFAULT_PAGE_LIMIT, 50).
-define(MAX_PAGE_LIMIT, 200).

-define(SQL_UPDATE_CONVERSATION_ASSIGNEE, <<
    "UPDATE enterprise_conversation c"
    "   SET business_identity_id = $4, version = c.version + 1, updated_at = now()"
    " WHERE c.organization_id = $1 AND c.workspace_id = $2 AND c.id = $3"
    "   AND c.status = 'active'"
    "   AND EXISTS ("
    "       SELECT 1 FROM organization_business_identity i"
    "        WHERE i.organization_id = $1 AND i.id = $4 AND i.status = 'active')"
    " RETURNING c.id"
>>).

-define(SQL_LIST_CONVERSATIONS, <<
    "SELECT id, organization_id, workspace_id, contact_id, business_identity_id, status,"
    "       version, notice_version, consent_at, consent_subject"
    "  FROM enterprise_conversation"
    " WHERE organization_id = $1 AND workspace_id = $2"
    "   AND EXISTS (SELECT 1 FROM workspace w WHERE w.organization_id = $1 AND w.id = $2)"
    " ORDER BY id"
>>).

%% @doc 冻结语句（供 `scripts/enterprise_business_db_it.sh` 的双租户键机械判据）。
-spec sql_statements() -> [binary()].
sql_statements() ->
    [
        ?SQL_INSERT_ASSIGNMENT,
        ?SQL_FETCH_ASSIGNMENT,
        ?SQL_LIST_IDENTITIES,
        ?SQL_UPDATE_CONVERSATION_ASSIGNEE,
        ?SQL_LIST_CONVERSATIONS
    ].

%% @doc P1：首次绑定 active 经办。
%%
%% `Assignment` 必含 `id` / `business_identity_id` / `function_key` / `user_id`；
%% `assigned_by` 可选。同一 identity 已有 active 行、或同一 user 已占用同一
%% function_key 时，唯一索引裁决 → `{error, conflict}`（不得静默成功）。
-spec insert_assignment(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_assignment(OrgId, WorkspaceId, Assignment) when is_map(Assignment) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        Id = maps:get(id, Assignment, undefined),
        Params = [
            OrgId,
            WorkspaceId,
            Id,
            maps:get(business_identity_id, Assignment, undefined),
            maps:get(function_key, Assignment, undefined),
            maps:get(user_id, Assignment, undefined),
            eb_pg_store_sql:nullify(maps:get(assigned_by, Assignment, undefined))
        ],
        case eb_pg_exec:insert_returning(?SQL_INSERT_ASSIGNMENT, Params) of
            {ok, _Row} ->
                fetch_assignment(OrgId, WorkspaceId, Id);
            {error, no_row} ->
                case eb_pg_exec:scope_ok(OrgId, WorkspaceId) of
                    true -> {error, conflict};
                    false -> {error, {workspace_not_in_org, WorkspaceId}}
                end;
            {error, _} = Err ->
                Err
        end
    end);
insert_assignment(_OrgId, _WorkspaceId, _Assignment) ->
    {error, invalid_assignment}.

fetch_assignment(OrgId, WorkspaceId, AssignmentId) ->
    eb_pg_exec:fetch_one(
        ?SQL_FETCH_ASSIGNMENT,
        [OrgId, WorkspaceId, AssignmentId],
        eb_pg_store_sql:assignment_fields()
    ).

%% @doc P2：identity 列举（Org 域；每条都带解析出的 workspace_id 供调用方核对租户键）。
%%
%% 冻结契约保留 `/2` 形状（等价默认页）；分页/投影走 `/3`。
-spec list_identities(integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_identities(OrgId, WorkspaceId) ->
    list_identities(OrgId, WorkspaceId, #{}).

%% @doc P2（C5 下推）：identity 列举——键集分页 + active assignment JOIN 投影。
%%
%% `Query`：
%%   after_id  可选（倒序游标，严格 `id < after_id`；缺省 = 首页）
%%   limit     可选（缺省 50，1..200；越界 `{error, {invalid_limit, _}}`，不钳制）
%%
%% 每行附 `active_assignment` 键：无 active 经办时为 `undefined`（store 的 NULL
%% 约定，HTTP 面由 application 层投影成 JSON null），有则为 C5 白名单七键对象
%% （assignment_id / business_identity_id / user_id / function_key / status /
%% assigned_at / version；字段规格见 `eb_pg_store_sql:active_assignment_fields/0`）。
-spec list_identities(integer(), integer(), map()) -> {ok, [map()]} | {error, term()}.
list_identities(OrgId, WorkspaceId, Query) when is_map(Query) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        case page_query(Query) of
            {error, _} = Err ->
                Err;
            {ok, {AfterId, Limit}} ->
                Params = [OrgId, WorkspaceId, AfterId, Limit],
                case eb_pg_exec:fetch_many(?SQL_LIST_IDENTITIES, Params, page_row_fields()) of
                    {ok, Rows} ->
                        {ok, [with_active_assignment(Row) || Row <- Rows]};
                    {error, _} = Err ->
                        Err
                end
        end
    end);
list_identities(_OrgId, _WorkspaceId, _Query) ->
    {error, invalid_query}.

%% 分页参数收敛：游标缺省 = int64 上界（首页）；limit 缺省 50、1..200。
%% 两者的越界/非法取值一律 `{error, _}` fail-closed，不静默钳制、不抛异常。
page_query(Query) ->
    case page_limit(Query) of
        {error, _} = Err ->
            Err;
        {ok, Limit} ->
            case maps:get(after_id, Query, undefined) of
                undefined -> {ok, {?TSID_MAX, Limit}};
                Id when is_integer(Id), Id > 0 -> {ok, {Id, Limit}};
                Other -> {error, {invalid_after_id, Other}}
            end
    end.

page_limit(Query) ->
    case maps:get(limit, Query, ?DEFAULT_PAGE_LIMIT) of
        Limit when is_integer(Limit), Limit >= 1, Limit =< ?MAX_PAGE_LIMIT -> {ok, Limit};
        Other -> {error, {invalid_limit, Other}}
    end.

page_row_fields() ->
    eb_pg_store_sql:identity_fields() ++ page_aa_fields().

%% JOIN 中间列规格：原子键加 `aa_` 前缀。identity 行与 assignment 行在
%% function_key/status/version 上同名——若用 C5 键直接单次归一化，identity 的
%% 自身字段会被 assignment 列覆盖；先以互不冲突的 aa_* 键摊平，再在
%% `with_active_assignment/1` 折叠时映射回 C5 白名单键
%%（`eb_pg_store_sql:active_assignment_fields/0`，键序即配对序）。
page_aa_fields() ->
    [
        {aa_assignment_id, <<"aa_assignment_id">>, int},
        {aa_business_identity_id, <<"aa_business_identity_id">>, int},
        {aa_user_id, <<"aa_user_id">>, int},
        {aa_function_key, <<"aa_function_key">>, bin},
        {aa_status, <<"aa_status">>, atom},
        {aa_assigned_at, <<"aa_assigned_at">>, ts},
        {aa_version, <<"aa_version">>, int}
    ].

aa_key_pairs() ->
    C5 = [Key || {Key, _Alias, _Kind} <- eb_pg_store_sql:active_assignment_fields()],
    lists:zip(C5, [Key || {Key, _Alias, _Kind} <- page_aa_fields()]).

%% 把 JOIN 出的 aa_* 列折叠成嵌套 `active_assignment` 键（键集 = C5 白名单，
%% 由字段规格配对机械导出）；无 active 经办（整组 NULL ⇒ assignment_id 为
%% undefined）时置 `undefined`，不留扁平的 aa_* 键污染 identity 行。
with_active_assignment(Row) ->
    AaKeys = [AaKey || {_C5Key, AaKey} <- aa_key_pairs()],
    Aa = maps:from_list([
        {C5Key, maps:get(AaKey, Row, undefined)}
     || {C5Key, AaKey} <- aa_key_pairs()
    ]),
    Identity = maps:without(AaKeys, Row),
    case maps:get(assignment_id, Aa, undefined) of
        undefined -> Identity#{active_assignment => undefined};
        _Id -> Identity#{active_assignment => Aa}
    end.

%% @doc P5：会话经办交接（只改当前 identity，不动历史 message；CAS 由 version 体现）。
-spec update_conversation_assignee(integer(), integer(), integer(), integer()) ->
    {ok, map()} | {error, term()}.
update_conversation_assignee(OrgId, WorkspaceId, ConversationId, IdentityId) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        Params = [OrgId, WorkspaceId, ConversationId, IdentityId],
        case eb_pg_exec:execute_returning(?SQL_UPDATE_CONVERSATION_ASSIGNEE, Params) of
            {ok, [_ | _]} ->
                eb_pg_store:fetch_conversation(OrgId, WorkspaceId, ConversationId);
            {ok, []} ->
                %% 0 行：会话不存在 / 非 active / 目标 identity 不合法 —— 统一 conflict
                {error, conflict};
            {error, _} = Err ->
                Err
        end
    end).

%% @doc P8：会话列表（§5.1 GET conversations）。
-spec list_conversations(integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_conversations(OrgId, WorkspaceId) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        eb_pg_exec:fetch_many(
            ?SQL_LIST_CONVERSATIONS, [OrgId, WorkspaceId], eb_pg_store_sql:conversation_fields()
        )
    end).
