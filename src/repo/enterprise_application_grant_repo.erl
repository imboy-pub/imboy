-module(enterprise_application_grant_repo).

%%%
% enterprise_application_grant_repo 是 Application Grant 仓储层（迁移 00000139，
% FULL-01 / plan-full §5「数据扩展」+ §3.1）。
%
% 表结构（三表 + 两视图）：
%   enterprise_application_grant(id TSID PK, organization_id, application_id,
%     workspace_scope_kind none|explicit, status active|revoked, valid_from,
%     expires_at, revoked_at, revoked_by_user_id, version CAS, idempotency_key,
%     timestamps；uq_eag_org_id_kind = 子表 kind 闸门，uq_eag_org_app_idempotency)
%   enterprise_application_grant_scope(grant_id, scope；scope 固定枚举 CHECK)
%   enterprise_application_grant_workspace(organization_id, grant_id, workspace_id；
%     三列复合 FK 引用 uq_eag_org_id_kind，kind 列被 CHECK 钉死 'explicit')
%   v_enterprise_effective_application_grant / _scope（只读视图，读时求值）
%
% 授权语义（本层只做数据访问，判定在 enterprise_application_grant_logic）：
%   * **无缓存**：每次调用都是一次 SQL，授权状态在**下一次读取**即反映；
%     撤销/到期/降级没有缓存失效链，视图谓词 CURRENT_TIMESTAMP 在每次查询求值。
%   * **受管即不可回退**：grant_governed_tx/3 只问「是否存在任何 Grant 行」
%     （含 revoked/过期）——授权行被触发器禁止物理删除（migration 00000139），
%     因此受管状态一旦成立就不会因为撤权而退回更宽的广州期边界。
%   * 所有 mutation 走 expected-version CAS（version 列），并发撤销/降级不丢更新。
%%%

-export([
    tablename/0,
    scope_tablename/0,
    workspace_tablename/0,
    effective_view/0,
    effective_scope_view/0,
    next_id/0,
    create_tx/4,
    find_tx/4,
    find_by_idempotency_key_tx/4,
    list_tx/3,
    revoke_tx/6,
    revoke_admin_tx/6,
    replace_scopes_tx/6,
    set_workspace_scope_tx/7,
    grant_governed_tx/3,
    effective_scopes_tx/3,
    effective_grants_tx/3,
    workspace_covered_tx/5,
    org_covered_tx/4
]).

-include_lib("epgsql/include/epgsql.hrl").

-define(COLUMNS, <<
    "id, organization_id, application_id, workspace_scope_kind, status,"
    " valid_from, expires_at, revoked_at, revoked_by_user_id, version,"
    " idempotency_key, created_at, updated_at"
>>).

%% 唯一约束名（幂等键）；单列在这里给出便于 map_error/1 判定。
-define(UQ_IDEMPOTENCY, <<"uq_eag_org_app_idempotency">>).

%% 授权行聚合列（grant 行 + 子表聚合数组）：list/find 共用。
-define(AGGREGATED_COLUMNS, <<
    "g.id, g.organization_id, g.application_id, g.workspace_scope_kind, g.status,"
    " g.valid_from, g.expires_at, g.revoked_at, g.revoked_by_user_id, g.version,"
    " g.idempotency_key, g.created_at, g.updated_at,"
    " COALESCE((SELECT array_agg(s.scope ORDER BY s.scope)"
    "           FROM enterprise_application_grant_scope s WHERE s.grant_id = g.id), '{}')"
    "   AS scopes,"
    " COALESCE((SELECT array_agg(w.workspace_id ORDER BY w.workspace_id)"
    "           FROM enterprise_application_grant_workspace w WHERE w.grant_id = g.id), '{}')"
    "   AS workspace_ids"
>>).

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"enterprise_application_grant">>).

-spec scope_tablename() -> binary().
scope_tablename() ->
    elib_pg_sql:public_tablename(<<"enterprise_application_grant_scope">>).

-spec workspace_tablename() -> binary().
workspace_tablename() ->
    elib_pg_sql:public_tablename(<<"enterprise_application_grant_workspace">>).

%% @doc auth context 读取面：当前生效的 Grant 行视图（读时求值，不物化）。
-spec effective_view() -> binary().
effective_view() ->
    elib_pg_sql:public_tablename(<<"v_enterprise_effective_application_grant">>).

%% @doc auth context 读取面：当前生效的 (Grant, scope) 视图（读时求值，不物化）。
-spec effective_scope_view() -> binary().
effective_scope_view() ->
    elib_pg_sql:public_tablename(<<"v_enterprise_effective_application_grant_scope">>).

%% @doc enterprise_application_grant 命名空间 TSID（惰性注册，镜像 agent_grant 口径）。
-spec next_id() -> pos_integer().
next_id() ->
    case lists:member(enterprise_application_grant, elib_tsid:registered()) of
        true -> ok;
        false -> elib_tsid:register(enterprise_application_grant)
    end,
    elib_tsid:generate(enterprise_application_grant).

%% @doc 事务内创建 Grant（父行 + scope 集合 + 可选显式 workspace 集合）。
%% Spec（map）：
%%   scopes                :: [binary()] 非空（固定枚举由 DB CHECK 兜底，上层先校验）
%%   workspace_scope_kind  :: none | explicit（缺省 none）
%%   workspace_ids         :: [integer()]（kind=explicit 时非空，kind=none 时必须空）
%%   expires_at            :: binary RFC3339（必填）
%%   valid_from            :: binary RFC3339（缺省 = DB 当前事务时间；不传比传
%%                            更安全：应用与数据库时钟偏差不会让刚签发的授权
%%                            在签发事务内"尚未生效"）
%%   idempotency_key       :: binary 非空（(org, app) 内唯一）
%% 返回 {ok, Grant}（含 scopes / workspace_ids 聚合数组）。
%% 语义拒绝：{error, empty_scopes}（无 scope 的 Grant 无意义）、
%%   {error, invalid_workspaces}（explicit 必须非空 / none 必须为空）、
%%   {error, key_conflict}（(org, app, idempotency_key) 撞唯一）、
%%   {error, invalid_scope | term()}（DB 23514 / 23503 等）。
-spec create_tx(any(), integer(), integer(), map()) -> {ok, map()} | {error, atom() | term()}.
create_tx(Conn, OrgId, AppId, Spec) when is_map(Spec) ->
    Scopes = maps:get(scopes, Spec, []),
    Kind = maps:get(workspace_scope_kind, Spec, none),
    WorkspaceIds = maps:get(workspace_ids, Spec, []),
    ExpiresAt = maps:get(expires_at, Spec, undefined),
    ValidFrom = maps:get(valid_from, Spec, undefined),
    IdemKey = maps:get(idempotency_key, Spec, undefined),
    case validate_create(Scopes, Kind, WorkspaceIds, ExpiresAt, IdemKey) of
        ok ->
            insert_grant(
                Conn, OrgId, AppId, Scopes, Kind, WorkspaceIds, ValidFrom, ExpiresAt, IdemKey
            );
        {error, _} = Err ->
            Err
    end;
create_tx(_Conn, _OrgId, _AppId, _Other) ->
    {error, invalid_spec}.

%% @doc 事务内按 (organization_id, application_id, id) 取 Grant（聚合 scopes/workspace_ids）。
-spec find_tx(any(), integer(), integer(), integer()) -> {ok, map()} | {error, not_found | term()}.
find_tx(Conn, OrgId, AppId, GrantId) when
    is_integer(OrgId), is_integer(AppId), is_integer(GrantId)
->
    Sql =
        <<"SELECT ", ?AGGREGATED_COLUMNS/binary, " FROM ", (tablename())/binary,
            " g WHERE g.organization_id = $1 AND g.application_id = $2 AND g.id = $3 LIMIT 1">>,
    one_tx(Conn, Sql, [OrgId, AppId, GrantId]).

%% @doc 事务内按幂等键取 Grant（治理接口的幂等读回）。
-spec find_by_idempotency_key_tx(any(), integer(), integer(), binary()) ->
    {ok, map()} | {error, not_found | term()}.
find_by_idempotency_key_tx(Conn, OrgId, AppId, IdempotencyKey) when is_binary(IdempotencyKey) ->
    Sql =
        <<"SELECT ", ?AGGREGATED_COLUMNS/binary, " FROM ", (tablename())/binary,
            " g WHERE g.organization_id = $1 AND g.application_id = $2"
            " AND g.idempotency_key = $3 LIMIT 1">>,
    one_tx(Conn, Sql, [OrgId, AppId, IdempotencyKey]).

%% @doc 事务内列出一个 Application 的全部 Grant（含 revoked/过期，治理读面）。
-spec list_tx(any(), integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_tx(Conn, OrgId, AppId) when is_integer(OrgId), is_integer(AppId) ->
    Sql =
        <<"SELECT ", ?AGGREGATED_COLUMNS/binary, " FROM ", (tablename())/binary,
            " g WHERE g.organization_id = $1 AND g.application_id = $2 ORDER BY g.id">>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内撤销 Grant（CAS：expected version 且仅 active 可撤销）。
%% 撤权**立即生效**：下一次 auth context 读取（effective 视图）即看不到该 Grant。
%% 既有行为逐字不变（租户 user 通道）：只是委托到带双通道的内部实现。
-spec revoke_tx(any(), integer(), integer(), integer(), integer(), integer()) ->
    ok | {error, not_found | version_conflict | already_revoked | term()}.
revoke_tx(Conn, OrgId, AppId, GrantId, ExpectedVersion, RevokedByUserId) ->
    revoke_with_actor_tx(Conn, OrgId, AppId, GrantId, ExpectedVersion, RevokedByUserId, null).

%% @doc 事务内以**平台管理员**身份撤销 Grant（Admin 治理面 A-11；迁移 00000143）。
%% 语义与 revoke_tx/6 完全对称（CAS + 仅 active 可撤 + 撤权立即生效），唯一差别是
%% 执行者落在新列 revoked_by_adm_user_id（revoked_by_user_id 保持 NULL）。
%% 为什么必须开第二通道：平台管理员是 adm_user，**不是**租户 user，
%% 而 ck_eag_status_revoked_match 要求 revoked 行必须有且仅有一个执行者——
%% 用它顶替租户 user 会伪造归因，撤销不写执行者则等于放弃留痕。
-spec revoke_admin_tx(any(), integer(), integer(), integer(), integer(), integer()) ->
    ok | {error, not_found | version_conflict | already_revoked | term()}.
revoke_admin_tx(Conn, OrgId, AppId, GrantId, ExpectedVersion, AdmUserId) when
    is_integer(AdmUserId), AdmUserId > 0
->
    revoke_with_actor_tx(Conn, OrgId, AppId, GrantId, ExpectedVersion, null, AdmUserId).

%% @doc 事务内整体替换 Grant 的 scope 集合（CAS；scope downgrade 的机制）。
%% Scopes 非空；替换后 version+1。降级对**下一请求**立即生效。
-spec replace_scopes_tx(any(), integer(), integer(), integer(), integer(), [binary()]) ->
    ok | {error, not_found | version_conflict | already_revoked | empty_scopes | term()}.
replace_scopes_tx(_Conn, _OrgId, _AppId, _GrantId, _ExpectedVersion, []) ->
    {error, empty_scopes};
replace_scopes_tx(Conn, OrgId, AppId, GrantId, ExpectedVersion, Scopes) when is_list(Scopes) ->
    case bump_version_tx(Conn, OrgId, AppId, GrantId, ExpectedVersion) of
        ok ->
            case replace_scope_rows_tx(Conn, GrantId, Scopes) of
                ok -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {error, _} = Err ->
            Err
    end.

%% @doc 事务内整体替换 Grant 的 workspace 授权边界（CAS）。
%% TargetKind=none ⇒ 清空 workspace 行（org 全域授权）；explicit ⇒ 必须有非空
%% workspace_ids（kind 闸门 FK 由 DB 兜底：none 类型不允许挂 workspace 行）。
-spec set_workspace_scope_tx(
    any(), integer(), integer(), integer(), integer(), none | explicit, [integer()]
) ->
    ok
    | {error, not_found | version_conflict | already_revoked | invalid_workspaces | term()}.
set_workspace_scope_tx(_Conn, _OrgId, _AppId, _GrantId, _V, explicit, []) ->
    {error, invalid_workspaces};
set_workspace_scope_tx(Conn, OrgId, AppId, GrantId, ExpectedVersion, TargetKind, WorkspaceIds) ->
    {Kind, Ids} =
        case TargetKind of
            none -> {none, []};
            explicit -> {explicit, WorkspaceIds};
            _ -> {invalid, []}
        end,
    case Kind of
        invalid ->
            {error, invalid_workspaces};
        none ->
            %% 先清子行再改 kind（否则 kind 改 'none' 会撞三列复合 FK）
            case delete_workspace_rows_tx(Conn, GrantId) of
                ok ->
                    set_workspace_scope_kind_tx(
                        Conn, OrgId, AppId, GrantId, ExpectedVersion, none, []
                    );
                {error, Reason} ->
                    {error, Reason}
            end;
        explicit ->
            %% 先改 kind 再插子行（'explicit' 是子行可引用的唯一 kind）
            case
                set_workspace_scope_kind_tx(
                    Conn, OrgId, AppId, GrantId, ExpectedVersion, explicit, Ids
                )
            of
                ok -> ok;
                {error, _} = Err -> Err
            end
    end.

%% @doc 该 Application 是否「受 Grant 治理」：只要存在**任何** Grant 行（含 revoked
%% 与已过期）即为 true。授权行被 migration 00000139 的触发器禁止物理删除，故该
%% 判定一旦为真永不回退——撤权只会把生效 scope 压到空集（fail-closed），
%% 绝不会退回未受管（更宽）的边界。
-spec grant_governed_tx(any(), integer(), integer()) -> {ok, boolean()} | {error, term()}.
grant_governed_tx(Conn, OrgId, AppId) ->
    Sql =
        <<"SELECT EXISTS (SELECT 1 FROM ", (tablename())/binary,
            " WHERE organization_id = $1 AND application_id = $2) AS governed">>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId]) of
        {ok, [Row | _]} -> {ok, maps:get(<<"governed">>, Row)};
        {ok, []} -> {ok, false};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 当前生效的 scope 并集（跨全部生效 Grant），经 effective 视图读时求值。
%% 这是 auth context 的 scope 读取面：App allowed_scopes ∩ 本集合 由 logic 求交。
-spec effective_scopes_tx(any(), integer(), integer()) -> {ok, [binary()]} | {error, term()}.
effective_scopes_tx(Conn, OrgId, AppId) ->
    Sql =
        <<"SELECT DISTINCT scope FROM ", (effective_scope_view())/binary,
            " WHERE organization_id = $1 AND application_id = $2 ORDER BY scope">>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId]) of
        {ok, Rows} -> {ok, [maps:get(<<"scope">>, R) || R <- Rows]};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 当前生效的 Grant 行（治理/诊断读面）。
-spec effective_grants_tx(any(), integer(), integer()) -> {ok, [map()]} | {error, term()}.
effective_grants_tx(Conn, OrgId, AppId) ->
    Sql =
        <<"SELECT grant_id, workspace_scope_kind, version, valid_from, expires_at FROM ",
            (effective_view())/binary,
            " WHERE organization_id = $1 AND application_id = $2 ORDER BY grant_id">>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 资源边界读取面：是否存在**同一个**生效 Grant 同时覆盖 scope 与 workspace
%% （kind='none' 覆盖 org 全域；kind='explicit' 需命中显式 workspace 行）。
%% 必须同 Grant 取交集——跨 Grant 的「scope 来自 A、workspace 来自 B」不成立。
%% 目标 workspace 以 (organization_id, id) 复合条件进 SQL：**跨 Org 或不存在
%% 的 workspace 一律 false**（fail-closed，org 全域 Grant 不会因此成为跨租户旁路）。
-spec workspace_covered_tx(any(), integer(), integer(), integer(), binary()) ->
    {ok, boolean()} | {error, term()}.
workspace_covered_tx(Conn, OrgId, AppId, WorkspaceId, Scope) when
    is_integer(WorkspaceId), is_binary(Scope)
->
    Sql =
        <<
            "SELECT EXISTS ("
            " SELECT 1 FROM ",
            (effective_view())/binary,
            " g"
            " JOIN ",
            (scope_tablename())/binary,
            " s ON s.grant_id = g.grant_id"
            " JOIN workspace w ON w.organization_id = g.organization_id AND w.id = $4"
            " LEFT JOIN ",
            (workspace_tablename())/binary,
            " gw ON gw.grant_id = g.grant_id AND gw.workspace_id = w.id"
            " WHERE g.organization_id = $1 AND g.application_id = $2 AND s.scope = $3"
            " AND (g.workspace_scope_kind = 'none' OR gw.grant_id IS NOT NULL)"
            ") AS covered"
        >>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId, Scope, WorkspaceId]) of
        {ok, [Row | _]} -> {ok, maps:get(<<"covered">>, Row)};
        {ok, []} -> {ok, false};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 资源边界读取面（**org 级**资源，FULL-02）：是否存在同一生效 Grant 同时
%% 覆盖 scope 与 Org 全域。org 级资源的边界是「Org 全域 Grant」（kind='none'）
%% ——显式 Workspace Grant（kind='explicit'）只覆盖列出的 workspace，**不**授权
%% org 级操作（否则一份窄 Workspace Grant 会隐式放大成全域权限，fail-open）。
%% 判定在 SQL 内以 (organization_id, application_id, scope) 复合条件完成：
%% 跨 Org / 未授予 scope 一律 false。读取失败由调用方 fail-closed。
-spec org_covered_tx(any(), integer(), integer(), binary()) ->
    {ok, boolean()} | {error, term()}.
org_covered_tx(Conn, OrgId, AppId, Scope) when is_binary(Scope) ->
    Sql =
        <<
            "SELECT EXISTS ("
            " SELECT 1 FROM ",
            (effective_view())/binary,
            " g"
            " JOIN ",
            (scope_tablename())/binary,
            " s ON s.grant_id = g.grant_id"
            " WHERE g.organization_id = $1 AND g.application_id = $2 AND s.scope = $3"
            " AND g.workspace_scope_kind = 'none'"
            ") AS covered"
        >>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId, Scope]) of
        {ok, [Row | _]} -> {ok, maps:get(<<"covered">>, Row)};
        {ok, []} -> {ok, false};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec validate_create(
    [binary()], none | explicit, [integer()], binary() | undefined, binary() | undefined
) ->
    ok
    | {error, empty_scopes | invalid_workspaces | invalid_expires_at | invalid_idempotency_key}.
validate_create([], _Kind, _WorkspaceIds, _ExpiresAt, _IdemKey) ->
    {error, empty_scopes};
validate_create(Scopes, Kind, WorkspaceIds, ExpiresAt, IdemKey) when
    is_list(Scopes), is_list(WorkspaceIds)
->
    case
        {
            is_binary(ExpiresAt) andalso ExpiresAt =/= <<>>,
            is_binary(IdemKey) andalso IdemKey =/= <<>>
        }
    of
        {false, _} ->
            {error, invalid_expires_at};
        {_, false} ->
            {error, invalid_idempotency_key};
        {true, true} ->
            validate_workspaces(Kind, WorkspaceIds)
    end;
validate_create(_Scopes, _Kind, _WorkspaceIds, _ExpiresAt, _IdemKey) ->
    {error, invalid_workspaces}.

-spec validate_workspaces(none | explicit, [integer()]) -> ok | {error, invalid_workspaces}.
validate_workspaces(none, []) ->
    ok;
validate_workspaces(none, _NonEmpty) ->
    {error, invalid_workspaces};
validate_workspaces(explicit, [_ | _] = Ids) ->
    case lists:all(fun is_integer/1, Ids) andalso lists:usort(Ids) =:= Ids of
        true -> ok;
        false -> {error, invalid_workspaces}
    end;
validate_workspaces(explicit, []) ->
    {error, invalid_workspaces};
validate_workspaces(_Kind, _WorkspaceIds) ->
    {error, invalid_workspaces}.

-spec insert_grant(
    any(),
    integer(),
    integer(),
    [binary()],
    none | explicit,
    [integer()],
    binary() | undefined,
    binary(),
    binary()
) -> {ok, map()} | {error, atom() | term()}.
insert_grant(Conn, OrgId, AppId, Scopes, Kind, WorkspaceIds, ValidFrom, ExpiresAt, IdemKey) ->
    Id = next_id(),
    Now = elib_dt:now(),
    %% valid_from 缺省用 DB 侧 CURRENT_TIMESTAMP（同一事务内即刻生效；避免
    %% 应用/数据库时钟偏差导致刚签发的 Grant 在签发事务内「尚未生效」）。
    ValidFromParam =
        case ValidFrom of
            V when is_binary(V), V =/= <<>> -> V;
            _ -> null
        end,
    Sql =
        <<"INSERT INTO ", (tablename())/binary,
            " (id, organization_id, application_id, workspace_scope_kind, status,"
            " valid_from, expires_at, version, idempotency_key, created_at, updated_at)",
            " VALUES ($1, $2, $3, $4, 'active', COALESCE($5::timestamptz, CURRENT_TIMESTAMP),"
            " $6::timestamptz, 1, $7, $8, $8)">>,
    case
        elib_pg:execute(Conn, Sql, [
            Id,
            OrgId,
            AppId,
            atom_to_binary(Kind, utf8),
            ValidFromParam,
            ExpiresAt,
            IdemKey,
            Now
        ])
    of
        {ok, 1} ->
            after_insert_children(Conn, OrgId, AppId, Id, Scopes, Kind, WorkspaceIds);
        {ok, 0} ->
            {error, insert_empty_result};
        {error, Reason} ->
            map_error(Reason)
    end.

-spec after_insert_children(any(), integer(), integer(), integer(), [binary()], atom(), [integer()]) ->
    {ok, map()} | {error, term()}.
after_insert_children(Conn, OrgId, AppId, Id, Scopes, Kind, WorkspaceIds) ->
    case replace_scope_rows_tx(Conn, Id, Scopes) of
        ok ->
            case maybe_insert_workspace_rows_tx(Conn, OrgId, Id, Kind, WorkspaceIds) of
                ok -> find_tx(Conn, OrgId, AppId, Id);
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec maybe_insert_workspace_rows_tx(any(), integer(), integer(), atom(), [integer()]) ->
    ok | {error, term()}.
maybe_insert_workspace_rows_tx(_Conn, _OrgId, _GrantId, none, []) ->
    ok;
maybe_insert_workspace_rows_tx(Conn, OrgId, GrantId, explicit, WorkspaceIds) ->
    insert_workspace_rows_tx(Conn, OrgId, GrantId, WorkspaceIds).

%% scope 行整体替换（先删后插，同一事务内）。
-spec replace_scope_rows_tx(any(), integer(), [binary()]) -> ok | {error, term()}.
replace_scope_rows_tx(Conn, GrantId, Scopes) ->
    Delete =
        <<"DELETE FROM ", (scope_tablename())/binary, " WHERE grant_id = $1">>,
    case elib_pg:execute(Conn, Delete, [GrantId]) of
        {ok, _} ->
            insert_scope_rows_tx(Conn, GrantId, Scopes);
        {error, Reason} ->
            {error, Reason}
    end.

-spec insert_scope_rows_tx(any(), integer(), [binary()]) -> ok | {error, term()}.
insert_scope_rows_tx(_Conn, _GrantId, []) ->
    ok;
insert_scope_rows_tx(Conn, GrantId, Scopes) ->
    Sql =
        <<"INSERT INTO ", (scope_tablename())/binary,
            " (grant_id, scope)"
            " SELECT $1, s FROM unnest($2::text[]) AS t(s)">>,
    case elib_pg:execute(Conn, Sql, [GrantId, Scopes]) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            map_error(Reason)
    end.

-spec delete_workspace_rows_tx(any(), integer()) -> ok | {error, term()}.
delete_workspace_rows_tx(Conn, GrantId) ->
    Sql = <<"DELETE FROM ", (workspace_tablename())/binary, " WHERE grant_id = $1">>,
    case elib_pg:execute(Conn, Sql, [GrantId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec insert_workspace_rows_tx(any(), integer(), integer(), [integer()]) -> ok | {error, term()}.
insert_workspace_rows_tx(Conn, OrgId, GrantId, WorkspaceIds) ->
    Sql =
        <<"INSERT INTO ", (workspace_tablename())/binary,
            " (organization_id, grant_id, workspace_id, workspace_scope_kind)"
            " SELECT $1, $2, w, 'explicit' FROM unnest($3::bigint[]) AS t(w)">>,
    case elib_pg:execute(Conn, Sql, [OrgId, GrantId, WorkspaceIds]) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            map_error(Reason)
    end.

%% CAS 版本自增（Grant 存在性/状态/版本三重判定）。
-spec bump_version_tx(any(), integer(), integer(), integer(), integer()) ->
    ok | {error, not_found | version_conflict | already_revoked | term()}.
bump_version_tx(Conn, OrgId, AppId, GrantId, ExpectedVersion) ->
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", (tablename())/binary, " SET version = version + 1, updated_at = $5",
            " WHERE organization_id = $1 AND application_id = $2 AND id = $3"
            " AND version = $4 AND status = 'active'">>,
    case elib_pg:execute(Conn, Sql, [OrgId, AppId, GrantId, ExpectedVersion, Now]) of
        {ok, 1} -> ok;
        {ok, 0} -> cas_miss(Conn, OrgId, AppId, GrantId);
        {error, Reason} -> {error, Reason}
    end.

%% kind 变更 + （explicit 时）workspace 行整体替换（CAS 在同一事务内）。
-spec set_workspace_scope_kind_tx(any(), integer(), integer(), integer(), integer(), atom(), [
    integer()
]) ->
    ok | {error, not_found | version_conflict | already_revoked | term()}.
set_workspace_scope_kind_tx(Conn, OrgId, AppId, GrantId, ExpectedVersion, TargetKind, WorkspaceIds) ->
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET workspace_scope_kind = $5, version = version + 1, updated_at = $6",
            " WHERE organization_id = $1 AND application_id = $2 AND id = $3"
            " AND version = $4 AND status = 'active'">>,
    case
        elib_pg:execute(Conn, Sql, [
            OrgId, AppId, GrantId, ExpectedVersion, atom_to_binary(TargetKind, utf8), Now
        ])
    of
        {ok, 1} when TargetKind =:= explicit ->
            case delete_workspace_rows_tx(Conn, GrantId) of
                ok -> insert_workspace_rows_tx(Conn, OrgId, GrantId, WorkspaceIds);
                {error, Reason} -> {error, Reason}
            end;
        {ok, 1} ->
            ok;
        {ok, 0} ->
            cas_miss(Conn, OrgId, AppId, GrantId);
        {error, Reason} ->
            {error, Reason}
    end.

%% 撤销的唯一写入点（CAS：expected version 且仅 active 可撤销）。撤权**立即生效**：
%% 下一次 auth context 读取（effective 视图）即看不到该 Grant。
%% RevokedByUserId 与 RevokedByAdmUserId 必须**恰有一个**为 null（另一方为整数）——
%% 由 ck_eag_status_revoked_match（迁移 00000143）在 DB 侧强制，本层不重复校验，
%% 但违反时 DB 抛 23514，map_error/1 归一后上层可见。
-spec revoke_with_actor_tx(
    any(), integer(), integer(), integer(), integer(), integer() | null, integer() | null
) ->
    ok | {error, not_found | version_conflict | already_revoked | term()}.
revoke_with_actor_tx(
    Conn, OrgId, AppId, GrantId, ExpectedVersion, RevokedByUserId, RevokedByAdmUserId
) ->
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET status = 'revoked', revoked_at = $5, revoked_by_user_id = $6,"
            " revoked_by_adm_user_id = $7,"
            " version = version + 1, updated_at = $5",
            " WHERE organization_id = $1 AND application_id = $2 AND id = $3"
            " AND version = $4 AND status = 'active'">>,
    case
        elib_pg:execute(Conn, Sql, [
            OrgId, AppId, GrantId, ExpectedVersion, Now, RevokedByUserId, RevokedByAdmUserId
        ])
    of
        {ok, 1} ->
            ok;
        {ok, 0} ->
            cas_miss(Conn, OrgId, AppId, GrantId);
        {error, Reason} ->
            {error, Reason}
    end.

%% CAS 未命中：区分「不存在」「已撤销」「版本冲突」——失败原因对治理接口可见。
-spec cas_miss(any(), integer(), integer(), integer()) ->
    {error, not_found | version_conflict | already_revoked | term()}.
cas_miss(Conn, OrgId, AppId, GrantId) ->
    Sql =
        <<"SELECT status, version FROM ", (tablename())/binary,
            " WHERE organization_id = $1 AND application_id = $2 AND id = $3 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId, GrantId]) of
        {ok, [#{<<"status">> := <<"revoked">>} | _]} -> {error, already_revoked};
        {ok, [_ | _]} -> {error, version_conflict};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec one_tx(any(), binary(), [term()]) -> {ok, map()} | {error, not_found | term()}.
one_tx(Conn, Sql, Params) ->
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% SQL 错误 → 稳定 atom（约束名取自 epgsql #error.extra，便于上层区分
%% 「幂等键冲突 / 跨 Org 引用 / 固定枚举违约 / 删除守卫」而无需解析 message）。
-spec map_error(term()) -> {error, atom() | {atom(), binary() | undefined} | term()}.
map_error(#error{code = <<"23505">>, extra = Extra}) ->
    case constraint_name(Extra) of
        ?UQ_IDEMPOTENCY -> {error, key_conflict};
        <<"pk_enterprise_application_grant">> -> {error, key_conflict};
        _ -> {error, unique_violation}
    end;
map_error(#error{code = <<"23503">>, extra = Extra}) ->
    {error, {foreign_key_violation, constraint_name(Extra)}};
map_error(#error{code = <<"23514">>, extra = Extra}) ->
    {error, {check_violation, constraint_name(Extra)}};
map_error(#error{code = <<"23502">>, extra = Extra}) ->
    %% NOT NULL 违约（如 valid_from/expires_at 缺省）
    {error, {not_null_violation, constraint_name(Extra)}};
map_error(#error{code = <<"22007">>}) ->
    %% 时间戳文本形态非法（RFC3339 解析失败）
    {error, invalid_expires_at};
map_error(#error{code = <<"22P02">>}) ->
    {error, invalid_argument};
map_error(Other) ->
    {error, Other}.

-spec constraint_name(list()) -> binary() | undefined.
constraint_name(Extra) when is_list(Extra) ->
    proplists:get_value(constraint_name, Extra);
constraint_name(_Extra) ->
    undefined.
