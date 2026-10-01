%%% @doc EB-03R P12：asset **metadata** 生命周期（pending_confirm → active → deleted）。
%%%
%%% 依据：R0-4——`eb_asset_port` 只有 put/stream/delete 三件 object-store 能力，
%%% **无 metadata 生命周期**；`enterprise_asset` 表已建但没有读写入口。
%%%
%%% ## 铁律级的边界（EB-D06 / §2.1 #13 / §5.4）
%%%
%%%   * 本模块只写**元数据**：绝不把 bucket / endpoint / 对象 key / presigned 链接
%%%     交给调用方；`object_key` 由实现**自行派生**（`enterprise/<Org>/<Ws>/<AssetId>`），
%%%     调用方既不能传入也不能读回可下载语义的值。
%%%   * 每次调用都带 `(OrgId, WorkspaceId)`：跨 Org 一律 `{error, not_found}`，
%%%     不区分「不存在」与「不属于你」（避免枚举）。
%%%   * 状态跃迁用 CAS 形状的 `WHERE status = ...`：并发两次 confirm 只有一个成功。
-module(eb_pg_asset_meta).
-export([key_prefix/2]).

-export([
    insert_asset/3,
    fetch_asset/3,
    confirm_asset/3,
    cleanup_asset/3,
    claim_pending_cleanup/5,
    finish_pending_cleanup/3,
    object_key/3,
    sql_statements/0
]).

-define(ASSET_COLUMNS,
    "a.id, a.organization_id, a.workspace_id, a.conversation_id, a.message_id,"
    " a.business_identity_id, a.uploaded_by_user_id, a.object_key, a.object_hash, a.mime,"
    " a.size_bytes, a.status, a.key_version, a.file_name, a.retain_until, a.version,"
    " a.created_at, a.deleted_at, a.pending_object_delete"
).

%% 登记：status 恒为 pending_confirm（确认只能走 confirm_asset/3）。
%% CS-BE-01：file_name（可空展示文件名，迁移 146 起有列）随登记落库。
-define(SQL_INSERT_ASSET, <<
    "INSERT INTO enterprise_asset"
    " (id, organization_id, workspace_id, conversation_id, message_id, business_identity_id,"
    "  uploaded_by_user_id, object_key, object_hash, mime, size_bytes, status, key_version,"
    "  file_name, retain_until, version)"
    " SELECT $3, $1, $2, $4, $5, $6, $7, $8, $9, $10, $11, 'pending_confirm', $12, $13,"
    "        CASE WHEN $14::bigint IS NULL THEN NULL ELSE to_timestamp($14::bigint/1000) END, 1"
    "   FROM workspace w"
    "  WHERE w.organization_id = $1 AND w.id = $2"
    " ON CONFLICT DO NOTHING"
    " RETURNING id"
>>).

-define(SQL_FETCH_ASSET, <<
    "SELECT "
    ?ASSET_COLUMNS
    "  FROM enterprise_asset a"
    "  JOIN workspace w ON w.id = $2 AND w.organization_id = a.organization_id"
    " WHERE a.organization_id = $1 AND a.workspace_id = $2 AND a.id = $3"
>>).

%% CAS：只有 pending_confirm 能确认（重放不算成功：状态机跃迁必须可审计）。
-define(SQL_CONFIRM_ASSET, <<
    "UPDATE enterprise_asset a"
    "   SET status = 'active', version = a.version + 1, updated_at = now()"
    " WHERE a.organization_id = $1 AND a.workspace_id = $2 AND a.id = $3"
    "   AND a.status = 'pending_confirm'"
    " RETURNING a.id"
>>).

%% 元数据回收：pending_confirm|active → deleted（幂等不重复计一次跃迁）。
-define(SQL_CLEANUP_ASSET, <<
    "UPDATE enterprise_asset a"
    "   SET status = 'deleted', deleted_at = now(), version = a.version + 1, updated_at = now()"
    " WHERE a.organization_id = $1 AND a.workspace_id = $2 AND a.id = $3"
    "   AND a.status <> 'deleted'"
    " RETURNING a.id"
>>).

%% @doc 冻结语句（双租户键机械判据用）。
-spec sql_statements() -> [binary()].
sql_statements() ->
    [?SQL_INSERT_ASSET, ?SQL_FETCH_ASSET, ?SQL_CONFIRM_ASSET, ?SQL_CLEANUP_ASSET].

%% @doc 由作用域**派生**对象 key（调用方不可传入、不可解释）。
%%
%% 形态：`enterprise/<OrgId>/<WorkspaceId>/<AssetId>`——Org/Workspace 是隔离前缀，
%% 跨 Org 的同 id 资产必然落在不同 key 上（A11 的作用域证据）。
-spec object_key(integer(), integer(), integer()) -> binary().
object_key(OrgId, WorkspaceId, AssetId) ->
    <<(key_prefix(OrgId, WorkspaceId))/binary, (integer_to_binary(AssetId))/binary>>.

key_prefix(OrgId, WorkspaceId) ->
    iolist_to_binary([
        "enterprise/",
        integer_to_binary(OrgId),
        "/",
        integer_to_binary(WorkspaceId),
        "/"
    ]).

%% @doc 登记未确认资产元数据。
%%
%% `Descriptor`（原子键 map）：`id` 必填、`object_hash` 必填、`mime` / `size_bytes` /
%% `file_name`（CS-BE-01：可空展示文件名）/ `key_version` / `retain_until`（Unix 秒）/
%% `conversation_id` / `message_id` / `business_identity_id` / `uploaded_by_user_id` 可选。
%% `object_key` **不在**入参白名单里：由 `object_key/3` 派生。
-spec insert_asset(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_asset(OrgId, WorkspaceId, Descriptor) when is_map(Descriptor) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        Id = maps:get(id, Descriptor, undefined),
        Hash = maps:get(object_hash, Descriptor, undefined),
        case is_integer(Id) andalso is_binary(Hash) of
            false ->
                {error, invalid_asset_descriptor};
            true ->
                Params = [
                    OrgId,
                    WorkspaceId,
                    Id,
                    eb_pg_store_sql:nullify(maps:get(conversation_id, Descriptor, undefined)),
                    eb_pg_store_sql:nullify(maps:get(message_id, Descriptor, undefined)),
                    eb_pg_store_sql:nullify(maps:get(business_identity_id, Descriptor, undefined)),
                    eb_pg_store_sql:nullify(maps:get(uploaded_by_user_id, Descriptor, undefined)),
                    object_key(OrgId, WorkspaceId, Id),
                    Hash,
                    eb_pg_store_sql:nullify(maps:get(mime, Descriptor, undefined)),
                    eb_pg_store_sql:nullify(maps:get(size_bytes, Descriptor, undefined)),
                    eb_pg_store_sql:nullify(maps:get(key_version, Descriptor, undefined)),
                    eb_pg_store_sql:nullify(maps:get(file_name, Descriptor, undefined)),
                    eb_pg_store_sql:nullify(maps:get(retain_until, Descriptor, undefined))
                ],
                case guarded_insert(OrgId, WorkspaceId, Id, Params) of
                    {ok, _Row} -> fetch_asset(OrgId, WorkspaceId, Id);
                    {error, no_row} -> eb_pg_exec:conflict_or(OrgId, WorkspaceId, WorkspaceId);
                    {error, _} = Err -> Err
                end
        end
    end);
insert_asset(_OrgId, _WorkspaceId, _Descriptor) ->
    {error, invalid_asset_descriptor}.

%% @doc 读取资产元数据（跨 Org / 不存在一律 `{error, not_found}`）。
-spec fetch_asset(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
fetch_asset(OrgId, WorkspaceId, AssetId) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        eb_pg_exec:fetch_one(?SQL_FETCH_ASSET, [OrgId, WorkspaceId, AssetId], asset_fields())
    end).

%% @doc 确认资产（`pending_confirm` → `active`）；非 pending 状态 → `{error, conflict}`。
-spec confirm_asset(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
confirm_asset(OrgId, WorkspaceId, AssetId) ->
    transition(
        OrgId,
        WorkspaceId,
        AssetId,
        ?SQL_CONFIRM_ASSET,
        fun() ->
            case fetch_asset(OrgId, WorkspaceId, AssetId) of
                {ok, _Row} -> {error, conflict};
                {error, _} = Err -> Err
            end
        end
    ).

%% @doc 回收资产元数据（→ `deleted`）。
-spec cleanup_asset(integer(), integer(), integer()) -> ok | {error, term()}.
cleanup_asset(OrgId, WorkspaceId, AssetId) ->
    case
        transition(OrgId, WorkspaceId, AssetId, ?SQL_CLEANUP_ASSET, fun() ->
            {error, not_found}
        end)
    of
        {ok, _Row} -> ok;
        {error, _} = Err -> Err
    end.

transition(OrgId, WorkspaceId, AssetId, Sql, OnNoRow) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        case eb_pg_exec:execute_returning(Sql, [OrgId, WorkspaceId, AssetId]) of
            {ok, [_ | _]} -> fetch_asset(OrgId, WorkspaceId, AssetId);
            {ok, []} -> OnNoRow();
            {error, _} = Err -> Err
        end
    end).

asset_fields() ->
    [
        {id, <<"id">>, int},
        {organization_id, <<"organization_id">>, int},
        {workspace_id, <<"workspace_id">>, int},
        {conversation_id, <<"conversation_id">>, int},
        {message_id, <<"message_id">>, int},
        {business_identity_id, <<"business_identity_id">>, int},
        {uploaded_by_user_id, <<"uploaded_by_user_id">>, int},
        {object_key, <<"object_key">>, bin},
        {object_hash, <<"object_hash">>, bin},
        {mime, <<"mime">>, bin},
        {size_bytes, <<"size_bytes">>, int},
        {status, <<"status">>, atom},
        {key_version, <<"key_version">>, int},
        {file_name, <<"file_name">>, bin},
        {retain_until, <<"retain_until">>, ts},
        {version, <<"version">>, int},
        {created_at, <<"created_at">>, ts},
        {deleted_at, <<"deleted_at">>, ts},
        {pending_object_delete, <<"pending_object_delete">>, raw}
    ].

%% Workspace FOR UPDATE conflicts with hold INSERT's FK KEY SHARE lock.
%% Claim is committed before network deletion; confirm can never revive this tombstone.
claim_pending_cleanup(Org, Ws, Id, Now, Ttl) ->
    Result = elib_pg:with_tx(fun(Conn) ->
        case
            elib_pg:query(
                Conn,
                <<"SELECT id FROM workspace WHERE organization_id=$1 AND id=$2 FOR UPDATE">>,
                [Org, Ws]
            )
        of
            {ok, [_]} -> claim_pending_in(Conn, Org, Ws, Id, Now, Ttl);
            {ok, []} -> {error, not_found};
            {error, Reason} -> throw({rollback, {error, Reason}})
        end
    end),
    case Result of
        {rollback, Error} -> Error;
        Other -> Other
    end.

claim_pending_in(Conn, Org, Ws, Id, Now, Ttl) ->
    Sql = <<
        "UPDATE enterprise_asset a SET status='deleted',pending_object_delete=true,"
        " deleted_at=now(),updated_at=now(),version=version+1"
        " WHERE a.organization_id=$1 AND a.workspace_id=$2 AND a.id=$3"
        " AND a.status='pending_confirm' AND a.message_id IS NULL"
        " AND a.created_at <= to_timestamp($4::bigint-$5::bigint)"
        " AND (a.retain_until IS NULL OR a.retain_until <= to_timestamp($4::bigint))"
        " AND NOT EXISTS (SELECT 1 FROM enterprise_retention_hold h"
        " WHERE h.organization_id=$1 AND h.workspace_id=$2 AND h.released_at IS NULL"
        " AND (h.scope_type='workspace' OR"
        " (h.scope_type='conversation' AND h.scope_conversation_id=a.conversation_id) OR"
        " (h.scope_type='message' AND h.scope_message_id=a.message_id)))"
        " RETURNING a.object_key"
    >>,
    case elib_pg:query(Conn, Sql, [Org, Ws, Id, Now, Ttl]) of
        {ok, [#{<<"object_key">> := Key}]} -> {ok, Key};
        {ok, []} -> {error, not_eligible};
        {error, Reason} -> throw({rollback, {error, Reason}})
    end.

finish_pending_cleanup(Org, Ws, Id) ->
    case
        elib_pg:execute(
            <<
                "UPDATE enterprise_asset SET pending_object_delete=false,"
                "updated_at=now(),version=version+1 WHERE organization_id=$1 AND workspace_id=$2"
                " AND id=$3 AND status='deleted' AND pending_object_delete"
            >>,
            [Org, Ws, Id]
        )
    of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% A committed deletion intent owns this ID until the old object is gone.
%% KEY SHARE precedes the queue read, so a waiting insert sees a fresh post-purge snapshot.
guarded_insert(Org, Ws, Id, Params) ->
    Result = elib_pg:with_tx(fun(Conn) ->
        case
            elib_pg:query(
                Conn,
                <<"SELECT id FROM workspace WHERE organization_id=$1 AND id=$2 FOR KEY SHARE">>,
                [Org, Ws]
            )
        of
            {ok, [_]} -> insert_without_pending_delete(Conn, Org, Ws, Id, Params);
            {ok, []} -> {error, no_row};
            {error, Reason} -> throw({rollback, {error, eb_pg_store_sql:normalize_error(Reason)}})
        end
    end),
    case Result of
        {rollback, Error} -> Error;
        Other -> Other
    end.

insert_without_pending_delete(Conn, Org, Ws, Id, Params) ->
    case
        elib_pg:query(
            Conn,
            <<"SELECT asset_id FROM enterprise_asset_delete_queue WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3">>,
            [Org, Ws, Id]
        )
    of
        {ok, []} ->
            case elib_pg:query(Conn, ?SQL_INSERT_ASSET, Params) of
                {ok, []} ->
                    {error, no_row};
                {ok, Rows} ->
                    {ok, Rows};
                {error, Reason} ->
                    throw({rollback, {error, eb_pg_store_sql:normalize_error(Reason)}})
            end;
        {ok, [_]} ->
            {error, cleanup_pending};
        {error, Reason} ->
            throw({rollback, {error, eb_pg_store_sql:normalize_error(Reason)}})
    end.
