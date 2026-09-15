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

-export([
    insert_assignment/3,
    list_identities/2,
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

-define(SQL_LIST_IDENTITIES, <<
    "SELECT i.id, i.organization_id, w.id AS workspace_id, i.function_key, i.display_name,"
    "       i.status, i.version, i.created_by_user_id"
    "  FROM organization_business_identity i"
    "  JOIN workspace w ON w.id = $2 AND w.organization_id = i.organization_id"
    " WHERE i.organization_id = $1"
    " ORDER BY i.id"
>>).

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
-spec list_identities(integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_identities(OrgId, WorkspaceId) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        eb_pg_exec:fetch_many(
            ?SQL_LIST_IDENTITIES, [OrgId, WorkspaceId], eb_pg_store_sql:identity_fields()
        )
    end).

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
