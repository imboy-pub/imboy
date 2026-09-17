%%% @doc Agent Grant 四表（agent_grant / agent_grant_workspace /
%%% agent_grant_capability / agent_grant_event，迁移 00000132，架构合同 §7.2
%%% Frozen Grant Schema Contract）的 epgsql 持久化实现（AG31-03）。
%%%
%%% 命名说明：本模块语义上是派工包 OWNED FILES 中的 "agent_grant_repo"（SQL 层），
%%% 采用 `agent_grant_pg` 后缀是 Feature Slice 铁律 5 的硬约束——
%%% scripts/check_feature_architecture.sh 禁止 application 层静态引用任何
%%% `*_repo` 模块（与 AG31-04B 的 agent_run_pg 同一先例）。
%%%
%%% 职责边界（镜像 agent_run_pg）：
%%%   * 全部 SQL 参数化；连接句柄 Conn 由调用方注入。
%%%   * **发行单事务**（§M.1 + §7.2 L306-307）：INSERT grant + event(issued) +
%%%     workspace 行 + capability 行在同一事务；事务内**锁 Grant 行**
%%%     （SELECT ... FOR UPDATE，刚插入即持有写锁，显式重取固化跨表条件验证）
%%%     并验证 workspace 跨表条件：none→零行、explicit→≥1 行。
%%%   * **撤销 CAS**（§M.3）：UPDATE ... WHERE id AND organization_id AND
%%%     status='active' AND version=expected；与 event(revoked) 同事务；
%%%     0 行更新 = {error, version_conflict}（并发 revoke 一胜一拒）。
%%%   * **append-only**：agent_grant_event 只 INSERT；UPDATE/DELETE 由
%%%     00000132 trg_agent_grant_event_append_only 在 DB 层拒绝（23514）。
%%%   * **约束冲突还原为稳定 reason**：23505 uq_ag_org_delegator_idempotency →
%%%     idempotency_duplicate（发行竞态）；23503 fk_agw_workspace →
%%%     cross_org_workspace（跨 Org workspace，§M.1 映射 + 上层审计）。
%%%   * delegator/Agent 身份权威事实 = user 表 account_type（迁移 00000070
%%%     注释枚举：0=human 1=agent 2=system_bot 3=bot）；本层提供单点读取。
-module(agent_grant_pg).

-export([
    next_id/1,
    get_user_account_type/2,
    get_grant/3,
    find_grant_by_idempotency/4,
    list_grants/2,
    list_workspace_ids/3,
    list_capabilities/3,
    insert_grant_tx/5,
    revoke_cas_tx/6
]).

-export_type([grant_row/0, capability_row/0]).

-type grant_row() :: map().
-type capability_row() :: map().

-define(GRANT_COLS,
    "id, agent_id, organization_id, delegator_user_id, workspace_scope_kind, status, "
    "valid_from, expires_at, revoked_at, revoked_by_user_id, version, idempotency_key, "
    "created_at, updated_at"
).

%% ===================================================================
%% ID 生成（elib_tsid 惰性注册；镜像 agent_run_pg 口径）
%% ===================================================================

%% @doc 按资源类型分域生成 TSID；未就绪时惰性注册（不静默兜底）。
next_id(Kind) when is_atom(Kind) ->
    case lists:member(Kind, elib_tsid:registered()) of
        true -> ok;
        false -> elib_tsid:register(Kind)
    end,
    elib_tsid:generate(Kind).

%% ===================================================================
%% 身份权威事实（user.account_type；delegator Human / Agent=1 的唯一判据）
%% ===================================================================

%% @doc 读取 user 行 account_type；行不存在 → {error, not_found}。
%% 权威事实源（派工包 §M.1）：account_type=1 是 Agent，≠1 视为 Human 可委托
%% 身份（0=human 2=system_bot 3=bot 均非 Agent，迁移 00000070 注释枚举）。
get_user_account_type(Conn, UserId) ->
    case
        epgsql:equery(
            Conn, "SELECT account_type FROM \"user\" WHERE id = $1", [UserId]
        )
    of
        {ok, _Cols, [{AccountType}]} -> {ok, AccountType};
        {ok, _, []} -> {error, not_found}
    end.

%% ===================================================================
%% 读路径
%% ===================================================================

%% @doc org 域内读单张 Grant（org 不匹配视同不存在，铁律 6 贯穿）。
get_grant(Conn, OrgId, GrantId) ->
    Sql = "SELECT " ?GRANT_COLS " FROM agent_grant WHERE id = $1 AND organization_id = $2",
    case epgsql:equery(Conn, Sql, [GrantId, OrgId]) of
        {ok, _Cols, [Row]} -> {ok, grant_row_to_map(Row)};
        {ok, _, []} -> {error, not_found}
    end.

%% @doc 幂等键定位域读取（UNIQUE (organization_id, delegator_user_id,
%% idempotency_key)，§7.2）。
find_grant_by_idempotency(Conn, OrgId, DelegatorId, IdemKey) ->
    Sql =
        "SELECT "
        ?GRANT_COLS
        " FROM agent_grant "
        "WHERE organization_id = $1 AND delegator_user_id = $2 AND idempotency_key = $3",
    case epgsql:equery(Conn, Sql, [OrgId, DelegatorId, IdemKey]) of
        {ok, _Cols, [Row]} -> {ok, grant_row_to_map(Row)};
        {ok, _, []} -> {error, not_found}
    end.

%% @doc org 域内 bounded 列举（新→旧；limit 由调用方收敛，上限 200）。
%% 过滤键：agent_id（可选）。铁律 6：恒带 organization_id 约束。
list_grants(Conn, #{organization_id := OrgId} = Filter) ->
    Limit = min(maps:get(limit, Filter, 50), 200),
    Offset = maps:get(offset, Filter, 0),
    case maps:get(agent_id, Filter, undefined) of
        undefined ->
            Sql =
                "SELECT "
                ?GRANT_COLS
                " FROM agent_grant WHERE organization_id = $1 "
                "ORDER BY created_at DESC, id DESC LIMIT $2 OFFSET $3",
            {ok, _Cols, Rows} = epgsql:equery(Conn, Sql, [OrgId, Limit, Offset]),
            [grant_row_to_map(R) || R <- Rows];
        AgentId ->
            Sql =
                "SELECT "
                ?GRANT_COLS
                " FROM agent_grant WHERE organization_id = $1 "
                "AND agent_id = $2 ORDER BY created_at DESC, id DESC LIMIT $3 OFFSET $4",
            {ok, _Cols, Rows} = epgsql:equery(Conn, Sql, [OrgId, AgentId, Limit, Offset]),
            [grant_row_to_map(R) || R <- Rows]
    end.

%% @doc Grant 的显式 workspace id 集（org 域内；复合 FK 保证同 Org）。
list_workspace_ids(Conn, OrgId, GrantId) ->
    Sql =
        "SELECT workspace_id FROM agent_grant_workspace "
        "WHERE organization_id = $1 AND grant_id = $2 ORDER BY workspace_id",
    {ok, _Cols, Rows} = epgsql:equery(Conn, Sql, [OrgId, GrantId]),
    [WsId || {WsId} <- Rows].

%% @doc Grant 的 capability 行（经 join 固化 org 域，铁律 6 贯穿）。
list_capabilities(Conn, OrgId, GrantId) ->
    Sql =
        "SELECT c.grant_id, c.capability, c.action, c.resource_type, c.constraint_json "
        "FROM agent_grant_capability c "
        "JOIN agent_grant g ON g.id = c.grant_id AND g.organization_id = $1 "
        "WHERE c.grant_id = $2 "
        "ORDER BY c.capability, c.action, c.resource_type",
    {ok, _Cols, Rows} = epgsql:equery(Conn, Sql, [OrgId, GrantId]),
    [
        #{
            capability => Capability,
            action => Action,
            resource_type => ResourceType,
            constraint => jsone:decode(ConstraintJson, [{object_format, map}])
        }
     || {_Gid, Capability, Action, ResourceType, ConstraintJson} <- Rows
    ].

%% ===================================================================
%% 写路径：发行单事务（grant + event + workspace 行 + capability 行 + 锁行验证）
%% ===================================================================

%% @doc 发行四写同事务（§7.2 L339-340：Grant create 与 event 同一事务提交，
%% 审计失败则 mutation 回滚）。事务内锁 Grant 行验证 workspace 跨表条件：
%% none→零行、explicit→≥1 行（§7.2 L305-307）。
%%
%% 错误映射：23503 fk_agw_workspace → {error, cross_org_workspace}；
%% 23505 uq_ag_org_delegator_idempotency → {error, idempotency_duplicate}
%% （并发发行竞态，调用方 re-read 收敛）；其余约束 → {error, {db_constraint, Name}}。
insert_grant_tx(Conn, Grant, WorkspaceIds, Capabilities, Event) ->
    with_tx(Conn, fun(C) ->
        InsertGrant =
            "INSERT INTO agent_grant (id, agent_id, organization_id, delegator_user_id, "
            "workspace_scope_kind, status, valid_from, expires_at, version, idempotency_key, "
            "created_at, updated_at) "
            "VALUES ($1,$2,$3,$4,$5,'active',$6,$7,1,$8,$9,$9)",
        case
            epgsql:equery(C, InsertGrant, [
                maps:get(id, Grant),
                maps:get(agent_id, Grant),
                maps:get(organization_id, Grant),
                maps:get(delegator_user_id, Grant),
                atom_to_binary(maps:get(workspace_scope_kind, Grant), utf8),
                maps:get(valid_from, Grant),
                maps:get(expires_at, Grant),
                maps:get(idempotency_key, Grant),
                maps:get(now, Grant)
            ])
        of
            {ok, 1} ->
                ok;
            {error, {error, _S1, <<"23505">>, _N1, _M1, Extra1}} ->
                %% 发行竞态：UNIQUE(organization_id, delegator_user_id,
                %% idempotency_key) 挡下并发同 key 写；调用方 re-read 收敛
                throw({abort_tx, constraint_abort(Extra1)})
        end,
        %% 跨 Org workspace 由复合 FK fk_agw_workspace 在 DB 层拒绝（§M.1）
        lists:foreach(
            fun(WsId) ->
                Sql =
                    "INSERT INTO agent_grant_workspace (organization_id, grant_id, workspace_id) "
                    "VALUES ($1, $2, $3)",
                case
                    epgsql:equery(C, Sql, [
                        maps:get(organization_id, Grant), maps:get(id, Grant), WsId
                    ])
                of
                    {ok, 1} ->
                        ok;
                    {error, {error, _S, <<"23503">>, _N, _M, _Extra}} ->
                        %% 跨 Org workspace：复合 FK fk_agw_workspace 拒绝（§M.1
                        %% 映射为 cross-org 拒绝，上层审计）
                        throw({abort_tx, cross_org_workspace})
                end
            end,
            WorkspaceIds
        ),
        lists:foreach(
            fun(Cap) ->
                Sql =
                    "INSERT INTO agent_grant_capability (grant_id, capability, action, "
                    "resource_type, constraint_json) VALUES ($1,$2,$3,$4,$5::jsonb)",
                {ok, 1} =
                    epgsql:equery(C, Sql, [
                        maps:get(id, Grant),
                        maps:get(capability, Cap),
                        maps:get(action, Cap),
                        maps:get(resource_type, Cap),
                        constraint_json(maps:get(constraint, Cap, #{}))
                    ])
            end,
            Capabilities
        ),
        ok = insert_event(C, Event),
        %% 同事务内锁 Grant 行 + 跨表条件验证（§7.2 L305-307 原文要求；
        %% 行锁已随 INSERT 持有，显式 FOR UPDATE 固化验证读的行版本）
        LockSql =
            "SELECT workspace_scope_kind FROM agent_grant "
            "WHERE id = $1 AND organization_id = $2 FOR UPDATE",
        {ok, _LCols, [{ScopeKindB}]} =
            epgsql:equery(C, LockSql, [maps:get(id, Grant), maps:get(organization_id, Grant)]),
        CountSql = "SELECT count(*) FROM agent_grant_workspace WHERE grant_id = $1",
        {ok, _CCols, [{Count}]} = epgsql:equery(C, CountSql, [maps:get(id, Grant)]),
        ok = assert_scope_condition(binary_to_atom(ScopeKindB, utf8), Count),
        {ok, maps:get(id, Grant)}
    end).

assert_scope_condition(none, 0) -> ok;
assert_scope_condition(explicit, Count) when Count >= 1 -> ok;
assert_scope_condition(_Scope, _Count) -> throw({abort_tx, workspace_scope_violation}).

%% ===================================================================
%% 写路径：撤销 CAS + event 同事务（§M.3）
%% ===================================================================

%% @doc CAS 撤销：仅 active→revoked；置 revoked_at + revoked_by_user_id
%% （同空/同非空由 ck_ag_status_revoked_match 兜底）；version+1 原子递增；
%% 并发 revoke 谓词重估恰一 winner，败者 {error, version_conflict}；
%% revoker 不存在（fk_ag_revoked_by RESTRICT）→ {error, revoker_not_found}。
revoke_cas_tx(Conn, OrgId, GrantId, ExpectedVersion, RevokedAt, Event) ->
    Sql =
        "UPDATE agent_grant SET status = 'revoked', revoked_at = $4, "
        "revoked_by_user_id = $5, version = version + 1, updated_at = $4 "
        "WHERE id = $2 AND organization_id = $1 AND status = 'active' AND version = $3 "
        "RETURNING version",
    with_tx(Conn, fun(C) ->
        case
            epgsql:equery(C, Sql, [
                OrgId,
                GrantId,
                ExpectedVersion,
                RevokedAt,
                maps:get(revoked_by_user_id, Event)
            ])
        of
            {ok, 1, _Cols, [{NewVersion}]} ->
                ok = insert_event(C, Event),
                {ok, NewVersion};
            {ok, 0, _Cols, []} ->
                throw({abort_tx, version_conflict});
            {error, {error, _S, <<"23503">>, _N, _M, _Extra}} ->
                %% fk_ag_revoked_by：revoker 不存在（RESTRICT）→ 稳定拒绝
                throw({abort_tx, revoker_not_found})
        end
    end).

%% ===================================================================
%% 内部：事务包装 / event 写入 / 错误映射
%% ===================================================================

%% epgsql:with_transaction/2（reraise=false）把 throw 的 {abort_tx, Reason}
%% 原样包成 {rollback, Reason}。
with_tx(Conn, Fun) ->
    case epgsql:with_transaction(Conn, Fun) of
        {rollback, {abort_tx, Reason}} -> {error, Reason};
        {rollback, Reason} -> {rollback, Reason};
        Reply -> Reply
    end.

insert_event(C, Ev) ->
    Sql =
        "INSERT INTO agent_grant_event (id, grant_id, event_type, actor_kind, actor_user_id, "
        "from_version, to_version, detail_json, idempotency_key, created_at) "
        "VALUES ($1,$2,$3,$4,$5,$6,$7,$8::jsonb,$9,$10)",
    {ok, 1} =
        epgsql:equery(C, Sql, [
            maps:get(id, Ev),
            maps:get(grant_id, Ev),
            atom_to_binary(maps:get(event_type, Ev), utf8),
            atom_to_binary(maps:get(actor_kind, Ev), utf8),
            maps:get(actor_user_id, Ev, undefined),
            maps:get(from_version, Ev, undefined),
            maps:get(to_version, Ev),
            detail_json(maps:get(detail, Ev, #{})),
            maps:get(idempotency_key, Ev),
            maps:get(now, Ev)
        ]),
    ok.

%% 已知约束错误 → 稳定 reason（23505 幂等唯一）；未知约束保留约束名。
constraint_abort(Extra) ->
    case lists:keyfind(constraint_name, 1, Extra) of
        {constraint_name, <<"uq_ag_org_delegator_idempotency">>} ->
            idempotency_duplicate;
        {constraint_name, Name} ->
            {db_constraint, Name};
        false ->
            {db_constraint, <<"unknown">>}
    end.

%% ===================================================================
%% 行转换 / JSON 片段
%% ===================================================================

grant_row_to_map(
    {Id, AgentId, OrgId, DelegatorId, ScopeKindB, StatusB, ValidFrom, ExpiresAt, RevokedAt,
        RevokedBy, Version, IdemKey, CreatedAt, UpdatedAt}
) ->
    #{
        id => Id,
        agent_id => AgentId,
        organization_id => OrgId,
        delegator_user_id => DelegatorId,
        workspace_scope_kind => binary_to_atom(ScopeKindB, utf8),
        status => binary_to_atom(StatusB, utf8),
        %% timestamptz 读回秒位为浮点（如 0.0）；归一为整秒保持与请求侧
        %% calendar datetime 的项等比较（幂等指纹/实时有效态判定依赖）
        valid_from => norm_dt(ValidFrom),
        expires_at => norm_dt(ExpiresAt),
        revoked_at => norm_dt(RevokedAt),
        revoked_by_user_id => RevokedBy,
        version => Version,
        idempotency_key => IdemKey,
        created_at => norm_dt(CreatedAt),
        updated_at => norm_dt(UpdatedAt)
    }.

%% 整数秒等值浮点（0.0/5.0）归一为整数；带非零小数的微秒精度保留原值。
%% 注意用数值相等 ==（0.0 =:= 0 为 false，会漏掉全部整秒浮点）。
norm_dt({D, {H, I, S}}) when is_float(S), S == trunc(S) ->
    {D, {H, I, trunc(S)}};
norm_dt(Dt) ->
    Dt.

constraint_json(Map) when map_size(Map) =:= 0 ->
    <<"{}">>;
constraint_json(Map) ->
    jsone:encode(Map).

%% sanitized metadata 只承载扁平 scalar（键值均先规约为 binary/整数再编码，
%% 不嵌套、不存敏感原文）。
detail_json(Map) when map_size(Map) =:= 0 ->
    <<"{}">>;
detail_json(Map) ->
    jsone:encode(maps:from_list([{key_to_b(K), scalar_to_b(V)} || {K, V} <- maps:to_list(Map)])).

key_to_b(K) when is_atom(K) -> atom_to_binary(K, utf8);
key_to_b(K) when is_integer(K) -> integer_to_binary(K);
key_to_b(K) when is_binary(K) -> K.

scalar_to_b(A) when is_atom(A) -> atom_to_binary(A, utf8);
scalar_to_b(I) when is_integer(I) -> I;
scalar_to_b(B) when is_binary(B) -> B;
scalar_to_b(Other) -> iolist_to_binary(io_lib:format("~p", [Other])).
