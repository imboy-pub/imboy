%%% @doc EB-03R P3/P4/P7：contact 侧的正向持久化能力。
%%%
%%% 依据：R0 的能力反查表（B3「缺 append_note / contact_assignment」、
%%% §5.1 GET/PATCH contacts）。`enterprise_note` / `enterprise_contact_assignment`
%%% **表已建、无写入能力**，`enterprise_contact` 有 fetch 但**无 list / update**。
%%%
%%% 铁律 6：`enterprise_contact` / `enterprise_note` / `enterprise_contact_assignment`
%%% 都没有 workspace 列 ⇒ 用 `workspace` 表在同一条语句里做归属校验（`$1`/`$2`
%%% 同时出现在语句里，`scripts/enterprise_business_db_it.sh` 可机械核对）。
%%% 备注正文是**密文**（`body_cipher` + `body_key_version`），本模块不接触明文。
-module(eb_pg_contact_ext).

-moduledoc "contact 侧正向持久化能力（EB-03R P3/P4/P7）。".
-export([
    insert_note/3,
    insert_contact_assignment/3,
    list_contacts/2,
    update_contact/3,
    sql_statements/0
]).

%% 备注：body 为密文；key_version 与 cipher 成对（DB CHECK 同口径）。
-define(SQL_INSERT_NOTE, <<
    "INSERT INTO enterprise_note"
    " (id, organization_id, contact_id, business_identity_id, actor_user_id,"
    "  body_cipher, body_key_version, status)"
    " SELECT $3, $1, c.id, $5, $6, $7, $8, 'active'"
    "   FROM workspace w"
    "   JOIN enterprise_contact c ON c.organization_id = $1 AND c.id = $4"
    "  WHERE w.organization_id = $1 AND w.id = $2"
    " ON CONFLICT DO NOTHING"
    " RETURNING id"
>>).

-define(SQL_FETCH_NOTE, <<
    "SELECT n.id, n.organization_id, n.contact_id, n.business_identity_id, n.actor_user_id,"
    "       n.body_cipher, n.body_key_version, n.status, n.created_at,"
    "       w.id AS workspace_id"
    "  FROM enterprise_note n"
    "  JOIN workspace w ON w.id = $2 AND w.organization_id = n.organization_id"
    " WHERE n.organization_id = $1 AND n.id = $3"
>>).

-define(SQL_INSERT_CONTACT_ASSIGNMENT, <<
    "INSERT INTO enterprise_contact_assignment"
    " (id, organization_id, contact_id, business_identity_id, role, status, assigned_by)"
    " SELECT $3, $1, c.id, i.id, $6, 'active', $7"
    "   FROM workspace w"
    "   JOIN enterprise_contact c ON c.organization_id = $1 AND c.id = $4"
    "   JOIN organization_business_identity i ON i.organization_id = $1 AND i.id = $5"
    "  WHERE w.organization_id = $1 AND w.id = $2"
    " ON CONFLICT DO NOTHING"
    " RETURNING id"
>>).

-define(SQL_FETCH_CONTACT_ASSIGNMENT, <<
    "SELECT a.id, a.organization_id, a.contact_id, a.business_identity_id, a.role, a.status,"
    "       a.assigned_at, a.ended_at, w.id AS workspace_id"
    "  FROM enterprise_contact_assignment a"
    "  JOIN workspace w ON w.id = $2 AND w.organization_id = a.organization_id"
    " WHERE a.organization_id = $1 AND a.id = $3"
>>).

-define(SQL_LIST_CONTACTS, <<
    "SELECT c.id, c.organization_id, w.id AS workspace_id, c.imboy_user_id, c.status,"
    "       c.display_name, c.profile_cipher, c.profile_key_version,"
    "       c.created_by_business_identity_id, c.version"
    "  FROM enterprise_contact c"
    "  JOIN workspace w ON w.id = $2 AND w.organization_id = c.organization_id"
    " WHERE c.organization_id = $1"
    " ORDER BY c.id"
>>).

%% PATCH：只允许白名单字段（display_name / profile_cipher / profile_key_version）；
%% id / organization_id / created_by_business_identity_id 一律不可变。
-define(SQL_UPDATE_CONTACT, <<
    "UPDATE enterprise_contact c"
    "   SET display_name = COALESCE($4, c.display_name),"
    "       profile_cipher = COALESCE($5, c.profile_cipher),"
    "       profile_key_version = COALESCE($6, c.profile_key_version),"
    "       version = c.version + 1,"
    "       updated_at = now()"
    " WHERE c.organization_id = $1 AND c.id = $3"
    "   AND EXISTS (SELECT 1 FROM workspace w WHERE w.organization_id = $1 AND w.id = $2)"
    " RETURNING c.id"
>>).

%% @doc 冻结语句（双租户键机械判据用）。
-spec sql_statements() -> [binary()].
sql_statements() ->
    [
        ?SQL_INSERT_NOTE,
        ?SQL_FETCH_NOTE,
        ?SQL_INSERT_CONTACT_ASSIGNMENT,
        ?SQL_FETCH_CONTACT_ASSIGNMENT,
        ?SQL_LIST_CONTACTS,
        ?SQL_UPDATE_CONTACT
    ].

%% @doc P3：企业跟进备注落库（密文；`body_cipher` 与 `body_key_version` 成对）。
-spec insert_note(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_note(OrgId, WorkspaceId, Note) when is_map(Note) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        Id = maps:get(id, Note, undefined),
        Params = [
            OrgId,
            WorkspaceId,
            Id,
            maps:get(contact_id, Note, undefined),
            eb_pg_store_sql:nullify(maps:get(business_identity_id, Note, undefined)),
            eb_pg_store_sql:nullify(maps:get(actor_user_id, Note, undefined)),
            eb_pg_store_sql:nullify(maps:get(body_cipher, Note, undefined)),
            eb_pg_store_sql:nullify(maps:get(body_key_version, Note, undefined))
        ],
        case eb_pg_exec:insert_returning(?SQL_INSERT_NOTE, Params) of
            {ok, _Row} ->
                eb_pg_exec:fetch_one(?SQL_FETCH_NOTE, [OrgId, WorkspaceId, Id], note_fields());
            {error, no_row} ->
                eb_pg_exec:conflict_or(OrgId, WorkspaceId, WorkspaceId);
            {error, _} = Err ->
                Err
        end
    end);
insert_note(_OrgId, _WorkspaceId, _Note) ->
    {error, invalid_note}.

%% @doc P4：客户 ↔ 业务身份经办关系。
-spec insert_contact_assignment(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_contact_assignment(OrgId, WorkspaceId, Assignment) when is_map(Assignment) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        Id = maps:get(id, Assignment, undefined),
        Params = [
            OrgId,
            WorkspaceId,
            Id,
            maps:get(contact_id, Assignment, undefined),
            maps:get(business_identity_id, Assignment, undefined),
            maps:get(role, Assignment, <<"primary">>),
            eb_pg_store_sql:nullify(maps:get(assigned_by, Assignment, undefined))
        ],
        case eb_pg_exec:insert_returning(?SQL_INSERT_CONTACT_ASSIGNMENT, Params) of
            {ok, _Row} ->
                eb_pg_exec:fetch_one(
                    ?SQL_FETCH_CONTACT_ASSIGNMENT,
                    [OrgId, WorkspaceId, Id],
                    contact_assignment_fields()
                );
            {error, no_row} ->
                eb_pg_exec:conflict_or(OrgId, WorkspaceId, WorkspaceId);
            {error, _} = Err ->
                Err
        end
    end);
insert_contact_assignment(_OrgId, _WorkspaceId, _Assignment) ->
    {error, invalid_contact_assignment}.

%% @doc P7：客户列表（§5.1 GET contacts）。
-spec list_contacts(integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_contacts(OrgId, WorkspaceId) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        eb_pg_exec:fetch_many(
            ?SQL_LIST_CONTACTS, [OrgId, WorkspaceId], eb_pg_store_sql:contact_fields()
        )
    end).

%% @doc P7：客户资料更新（§5.1 PATCH contacts/:id）。
%%
%% `Patch` 必含 `id`，可选 `display_name` / `profile_cipher` / `profile_key_version`。
%% 未提供的字段不动（`COALESCE`）；白名单外字段一律忽略（不可变字段无法被改动）。
-spec update_contact(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
update_contact(OrgId, WorkspaceId, Patch) when is_map(Patch) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        ContactId = maps:get(id, Patch, undefined),
        Params = [
            OrgId,
            WorkspaceId,
            ContactId,
            eb_pg_store_sql:nullify(maps:get(display_name, Patch, undefined)),
            eb_pg_store_sql:nullify(maps:get(profile_cipher, Patch, undefined)),
            eb_pg_store_sql:nullify(maps:get(profile_key_version, Patch, undefined))
        ],
        case eb_pg_exec:execute_returning(?SQL_UPDATE_CONTACT, Params) of
            {ok, [_ | _]} -> eb_pg_store:fetch_contact(OrgId, WorkspaceId, ContactId);
            {ok, []} -> {error, not_found};
            {error, _} = Err -> Err
        end
    end);
update_contact(_OrgId, _WorkspaceId, _Patch) ->
    {error, invalid_patch}.

%% 备注 / 客户经办关系的行规格（本模块私有；与 eb_pg_store_sql 的字段规格同规）。
note_fields() ->
    [
        {id, <<"id">>, int},
        {organization_id, <<"organization_id">>, int},
        {workspace_id, <<"workspace_id">>, int},
        {contact_id, <<"contact_id">>, int},
        {business_identity_id, <<"business_identity_id">>, int},
        {actor_user_id, <<"actor_user_id">>, int},
        {body_cipher, <<"body_cipher">>, bin},
        {body_key_version, <<"body_key_version">>, int},
        {status, <<"status">>, atom},
        {created_at, <<"created_at">>, ts}
    ].

contact_assignment_fields() ->
    [
        {id, <<"id">>, int},
        {organization_id, <<"organization_id">>, int},
        {workspace_id, <<"workspace_id">>, int},
        {contact_id, <<"contact_id">>, int},
        {business_identity_id, <<"business_identity_id">>, int},
        {role, <<"role">>, bin},
        {status, <<"status">>, atom},
        {assigned_at, <<"assigned_at">>, ts},
        {ended_at, <<"ended_at">>, ts}
    ].
