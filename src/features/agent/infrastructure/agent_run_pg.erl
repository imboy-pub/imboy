%%% @doc AgentRun 三表（agent_run/agent_run_event/agent_effect）的 epgsql 持久化实现
%%% （AG31-04B；架构合同 §9.3 并发策略 + §9.4 Frozen Run/Effect Schema Contract）。
%%%
%%% 职责边界：
%%%   * 全部 SQL 参数化；连接句柄 Conn 由调用方注入（隔离 PG 验收 / 生产装配皆然）。
%%%   * **CAS 迁移**：每条 FSM 边为 `UPDATE ... WHERE id=? AND status=? AND version=?`
%%%     （§9.3），且与 agent_run_event 在同一事务提交（epgsql:with_transaction，
%%%     失败 throw {abort_tx, _} 整体回滚——镜像 channel_webhook_ds 口径）。
%%%   * **lease**：获取/续租/过期接管均为 DB 条件更新；进程锁/ETS/全局注册不作真源。
%%%     过期接管是同态字段更新（非 FSM 边，不写 event），谓词为
%%%     `status='running' AND lease_expires_at < now`——行锁串行化下并发竞争恰一 winner。
%%%   * **append-only**：agent_run_event 只 INSERT；UPDATE/DELETE 由 00000133
%%%     trg_agent_run_event_append_only 在 DB 层拒绝（23514）。
%%%   * 错误形状稳定：{error, Reason}；约束冲突还原为稳定 reason（duplicate_effect 等）。
-module(agent_run_pg).

-export([
    next_id/1,
    get_run/2,
    find_run_by_trigger/6,
    get_grant/2,
    get_effect/2,
    find_effect_by_external_key/3,
    count_run_events/2,
    get_agent_identity/2,
    next_effect_sequence/2,
    insert_run_tx/3,
    cas_transition_tx/8,
    lease_acquire_tx/7,
    insert_effect_guarded_tx/2,
    lease_renew/6,
    lease_take_over/5,
    insert_effect_tx/3,
    effect_to_tx/7,
    combined_effect_run_tx/8
]).

-export_type([run/0, effect/0, grant_row/0]).

-type conn() :: pid().
-type dt() :: calendar:datetime().
-type run() :: map().
-type effect() :: map().
-type grant_row() :: map().

%% ===================================================================
%% ID 生成（elib_tsid 惰性注册；镜像 cs_tsid 口径）
%% ===================================================================

%% @doc 按资源类型分域生成 TSID；未就绪时惰性注册（不静默兜底）。
-spec next_id(atom()) -> pos_integer().
next_id(Kind) when is_atom(Kind) ->
    case lists:member(Kind, elib_tsid:registered()) of
        true -> ok;
        false -> elib_tsid:register(Kind)
    end,
    elib_tsid:generate(Kind).

%% ===================================================================
%% 读路径
%% ===================================================================

-define(RUN_COLS,
    "id, agent_id, organization_id, workspace_id, grant_id, grant_version_at_start, "
    "delegating_principal_id, trigger_type, trigger_id, runtime_type, status, reason_code, "
    "version, context_digest, idempotency_key, lease_owner, lease_expires_at, attempt, "
    "created_at, queued_at, started_at, finished_at, updated_at"
).

-define(EFFECT_COLS,
    "id, run_id, sequence, tool_id, capability, action, resource_digest, args_digest, "
    "status, authorization_reason, approval_ref, grant_version_checked, "
    "external_idempotency_key, result_digest, failure_code, version, created_at, updated_at"
).

-spec get_run(conn(), non_neg_integer()) -> {ok, run()} | {error, not_found}.
get_run(Conn, RunId) ->
    Sql = "SELECT " ?RUN_COLS " FROM agent_run WHERE id = $1",
    case epgsql:equery(Conn, Sql, [RunId]) of
        {ok, _Cols, [Row]} -> {ok, run_row_to_map(Row)};
        {ok, _, []} -> {error, not_found}
    end.

-spec find_run_by_trigger(conn(), integer(), integer(), atom(), binary(), binary()) ->
    {ok, run()} | {error, not_found}.
find_run_by_trigger(Conn, AgentId, OrgId, TriggerType, TriggerId, IdemKey) ->
    Sql =
        "SELECT "
        ?RUN_COLS
        " FROM agent_run "
        "WHERE agent_id = $1 AND organization_id = $2 AND trigger_type = $3 "
        "AND trigger_id = $4 AND idempotency_key = $5",
    Params = [AgentId, OrgId, atom_to_binary(TriggerType, utf8), TriggerId, IdemKey],
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _Cols, [Row]} -> {ok, run_row_to_map(Row)};
        {ok, _, []} -> {error, not_found}
    end.

-spec get_grant(conn(), non_neg_integer()) -> {ok, grant_row()} | {error, not_found}.
get_grant(Conn, GrantId) ->
    Sql = "SELECT id, status, valid_from, expires_at, version FROM agent_grant WHERE id = $1",
    case epgsql:equery(Conn, Sql, [GrantId]) of
        {ok, _Cols, [{Id, StatusB, ValidFrom, ExpiresAt, Version}]} ->
            {ok, #{
                id => Id,
                status => binary_to_atom(StatusB, utf8),
                valid_from => ValidFrom,
                expires_at => ExpiresAt,
                version => Version
            }};
        {ok, _, []} ->
            {error, not_found}
    end.

-spec get_effect(conn(), non_neg_integer()) -> {ok, effect()} | {error, not_found}.
get_effect(Conn, EffectId) ->
    Sql = "SELECT " ?EFFECT_COLS " FROM agent_effect WHERE id = $1",
    case epgsql:equery(Conn, Sql, [EffectId]) of
        {ok, _Cols, [Row]} -> {ok, effect_row_to_map(Row)};
        {ok, _, []} -> {error, not_found}
    end.

-spec find_effect_by_external_key(conn(), binary(), binary()) ->
    {ok, effect()} | {error, not_found}.
find_effect_by_external_key(Conn, ToolId, ExternalKey) ->
    Sql =
        "SELECT "
        ?EFFECT_COLS
        " FROM agent_effect "
        "WHERE tool_id = $1 AND external_idempotency_key = $2",
    case epgsql:equery(Conn, Sql, [ToolId, ExternalKey]) of
        {ok, _Cols, [Row]} -> {ok, effect_row_to_map(Row)};
        {ok, _, []} -> {error, not_found}
    end.

-spec count_run_events(conn(), non_neg_integer()) -> non_neg_integer().
count_run_events(Conn, RunId) ->
    {ok, _, [{Count}]} =
        epgsql:equery(Conn, "SELECT count(*) FROM agent_run_event WHERE run_id = $1", [RunId]),
    Count.

%% ===================================================================
%% 读路径：Agent 身份 + effect 序号（AG31-05 严格附加；R7 披露项）
%% ===================================================================

%% AG31-05 R1：enabled 权威事实=user 行存在 ∧ account_type=1，另加核
%% 显式 status 列（00000001 注释：-1 删除/0 禁用/1 启用/2 注销中；
%% 1=启用才视为 enabled）。既有行为的纯附加读，不改任何既有函数。
-spec get_agent_identity(conn(), non_neg_integer()) ->
    {ok, #{account_type := integer(), status := integer()}} | {error, not_found}.
get_agent_identity(Conn, UserId) ->
    Sql = "SELECT account_type, status FROM \"user\" WHERE id = $1",
    case epgsql:equery(Conn, Sql, [UserId]) of
        {ok, _Cols, [{AccountType, Status}]} ->
            {ok, #{account_type => AccountType, status => Status}};
        {ok, _, []} ->
            {error, not_found}
    end.

%% AG31-05：authorize/3 自持 effect 单调序号（UNIQUE(run_id,sequence) 兜底
%% 并发碰撞为 23505）。既有行为的纯附加读，不改任何既有函数。
-spec next_effect_sequence(conn(), non_neg_integer()) -> pos_integer().
next_effect_sequence(Conn, RunId) ->
    Sql = "SELECT COALESCE(MAX(sequence), 0) + 1 FROM agent_effect WHERE run_id = $1",
    {ok, _, [{Seq}]} = epgsql:equery(Conn, Sql, [RunId]),
    Seq.

%% ===================================================================
%% 写路径：Run 创建（insert + 创建事件同事务）
%% ===================================================================

-spec insert_run_tx(conn(), map(), map()) -> {ok, pos_integer()} | {rollback, term()}.
insert_run_tx(Conn, Run, CreationEvent) ->
    with_tx(Conn, fun(C) ->
        Sql =
            "INSERT INTO agent_run (id, agent_id, organization_id, workspace_id, grant_id, "
            "grant_version_at_start, delegating_principal_id, trigger_type, trigger_id, "
            "runtime_type, status, reason_code, version, context_digest, idempotency_key, "
            "created_at, updated_at) "
            "VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,'created',NULL,1,$11,$12,$13,$13)",
        {ok, 1} =
            epgsql:equery(C, Sql, [
                maps:get(id, Run),
                maps:get(agent_id, Run),
                maps:get(organization_id, Run),
                maps:get(workspace_id, Run),
                maps:get(grant_id, Run),
                maps:get(grant_version_at_start, Run),
                maps:get(delegating_principal_id, Run),
                atom_to_binary(maps:get(trigger_type, Run), utf8),
                maps:get(trigger_id, Run),
                atom_to_binary(maps:get(runtime_type, Run), utf8),
                maps:get(context_digest, Run),
                maps:get(idempotency_key, Run),
                maps:get(now, Run)
            ]),
        ok = insert_event(C, CreationEvent),
        {ok, maps:get(id, Run)}
    end).

%% ===================================================================
%% 写路径：CAS FSM 迁移 + event 同事务（§9.3 L456-457）
%% ===================================================================

%% lifecycle 列按目标态定：queued→queued_at / running→started_at / 终态→finished_at；
%% waiting_approval/unknown/created→updated_at（由 updated_at 覆写，无独立 lifecycle 列）。
-spec cas_transition_tx(
    conn(), integer(), atom(), atom(), integer(), dt(), atom() | undefined, map()
) ->
    {ok, pos_integer()}
    | {error, cas_conflict | illegal_transition}
    | {rollback, term()}.
cas_transition_tx(Conn, RunId, From, To, ExpectedVersion, Now, ReasonCode, Event) ->
    ok = agent_run_fsm:assert_transition(From, To),
    with_tx(Conn, fun(C) ->
        case cas_transition_equery(C, RunId, From, To, ExpectedVersion, Now, ReasonCode) of
            {ok, NewVersion} ->
                ok = insert_event(C, Event),
                {ok, NewVersion};
            {error, cas_conflict} ->
                throw({abort_tx, cas_conflict})
        end
    end).

%% CAS FSM 迁移的纯 equery 形态（无事务边界；供 cas_transition_tx 与
%% 组合事务复用）。调用方必须已 assert_transition。
-spec cas_transition_equery(pid(), integer(), atom(), atom(), integer(), dt(), atom() | undefined) ->
    {ok, pos_integer()} | {error, cas_conflict}.
cas_transition_equery(C, RunId, From, To, ExpectedVersion, Now, ReasonCode) ->
    %% waiting_approval/unknown/created 无独立 lifecycle 列（lifecycle_col 回退
    %% updated_at），与无条件 updated_at 赋值合并，避免 42601 multiple assignments。
    ExtraLifecycle =
        case lifecycle_col(To) of
            "updated_at" -> [];
            Col -> ", " ++ Col ++ " = $5"
        end,
    Sql =
        "UPDATE agent_run SET status = $3, reason_code = $4, version = version + 1, "
        "updated_at = $5" ++
            ExtraLifecycle ++
            " WHERE id = $1 AND status = $2 AND version = $6 "
            "RETURNING version",
    case
        epgsql:equery(C, Sql, [
            RunId,
            atom_to_binary(From, utf8),
            atom_to_binary(To, utf8),
            reason_to_b(ReasonCode),
            Now,
            ExpectedVersion
        ])
    of
        {ok, 1, _Cols, [{NewVersion}]} -> {ok, NewVersion};
        {ok, 0, _Cols, []} -> {error, cas_conflict}
    end.

%% ===================================================================
%% 写路径：lease（DB 条件更新为唯一真源，§9.3 L458-459）
%% ===================================================================

%% queued→running：attempt+1、写 started_at/lease 字段，与 E04 事件同事务。
-spec lease_acquire_tx(conn(), integer(), integer(), binary(), dt(), dt(), map()) ->
    {ok, #{version := pos_integer(), attempt := non_neg_integer()}}
    | {error, lease_not_acquired}
    | {rollback, term()}.
lease_acquire_tx(Conn, RunId, ExpectedVersion, Worker, ExpiresAt, Now, Event) ->
    Sql =
        "UPDATE agent_run SET status = 'running', started_at = $3, lease_owner = $4, "
        "lease_expires_at = $5, attempt = attempt + 1, version = version + 1, updated_at = $3 "
        "WHERE id = $1 AND status = 'queued' AND version = $2 "
        "RETURNING version, attempt",
    with_tx(Conn, fun(C) ->
        case epgsql:equery(C, Sql, [RunId, ExpectedVersion, Now, Worker, ExpiresAt]) of
            {ok, 1, _Cols, [{V, A}]} ->
                ok = insert_event(C, Event),
                {ok, #{version => V, attempt => A}};
            {ok, 0, _Cols, []} ->
                throw({abort_tx, lease_not_acquired})
        end
    end).

%% running 续租：仅 lease_owner 本人（未被他人接管前），过期与否皆可续。
-spec lease_renew(conn(), integer(), binary(), dt(), dt(), integer()) ->
    {ok, pos_integer()} | {error, lease_not_acquired}.
lease_renew(Conn, RunId, Worker, ExpiresAt, Now, ExpectedVersion) ->
    Sql =
        "UPDATE agent_run SET lease_owner = $2, lease_expires_at = $3, version = version + 1, "
        "updated_at = $4 "
        "WHERE id = $1 AND status = 'running' AND lease_owner = $2 AND version = $5 "
        "RETURNING version",
    case epgsql:equery(Conn, Sql, [RunId, Worker, ExpiresAt, Now, ExpectedVersion]) of
        {ok, 1, _Cols, [{V}]} -> {ok, V};
        {ok, 0, _Cols, []} -> {error, lease_not_acquired}
    end.

%% 过期接管：同态字段更新（非 FSM 边，不写 event；04A §3.1 认定）。
%% 并发竞争时行锁串行化 + 谓词重估，恰一 winner。
-spec lease_take_over(conn(), integer(), binary(), dt(), dt()) ->
    {ok, #{version := pos_integer(), attempt := non_neg_integer()}} | {error, lease_not_acquired}.
lease_take_over(Conn, RunId, Worker, ExpiresAt, Now) ->
    Sql =
        "UPDATE agent_run SET lease_owner = $2, lease_expires_at = $3, attempt = attempt + 1, "
        "version = version + 1, updated_at = $4 "
        "WHERE id = $1 AND status = 'running' AND lease_expires_at < $4 "
        "RETURNING version, attempt",
    case epgsql:equery(Conn, Sql, [RunId, Worker, ExpiresAt, Now]) of
        {ok, 1, _Cols, [{V, A}]} -> {ok, #{version => V, attempt => A}};
        {ok, 0, _Cols, []} -> {error, lease_not_acquired}
    end.

%% ===================================================================
%% 写路径：agent_effect（insert + CAS 子状态迁移）
%% ===================================================================

%% INSERT(status='created') + CAS created→decided（及可选的同事务 Run CAS 迁移，
%% 如 approval_required 时的 E07 running→waiting_approval）在同一事务
%% （账本诚实记录 created 态；run 迁移与 event 同事务提交，§9.3 L456-457）。
%% UNIQUE(tool_id, external_idempotency_key) 冲突还原为
%% {error, {duplicate_effect, ConstraintName}}（§14 duplicate effect 不重发）。
-spec insert_effect_tx(
    conn(), map(), none | {cas_run, integer(), atom(), atom(), integer(), atom() | undefined, map()}
) ->
    {ok, pos_integer(), pos_integer() | undefined}
    | {error, {duplicate_effect, binary()}}
    | {rollback, term()}.
insert_effect_tx(Conn, Effect, RunOp) ->
    with_tx(Conn, fun(C) ->
        Sql =
            "INSERT INTO agent_effect (id, run_id, sequence, tool_id, capability, action, "
            "resource_digest, args_digest, status, authorization_reason, approval_ref, "
            "grant_version_checked, external_idempotency_key, version, created_at, updated_at) "
            "VALUES ($1,$2,$3,$4,$5,$6,$7,$8,'created',NULL,NULL,NULL,$9,1,$10,$10) "
            "RETURNING id",
        Params = [
            maps:get(id, Effect),
            maps:get(run_id, Effect),
            maps:get(sequence, Effect),
            maps:get(tool_id, Effect),
            maps:get(capability, Effect),
            maps:get(action, Effect),
            maps:get(resource_digest, Effect),
            maps:get(args_digest, Effect),
            maps:get(external_idempotency_key, Effect),
            maps:get(now, Effect)
        ],
        EffectId =
            case epgsql:equery(C, Sql, Params) of
                {ok, 1, _InsCols, [{Id}]} ->
                    Id;
                {error, {error, _S, <<"23505">>, _N, _M, Extra}} ->
                    throw({abort_tx, {duplicate_effect, constraint_name(Extra)}})
            end,
        Decided = maps:get(decided_status, Effect),
        RunVersion =
            case RunOp of
                none ->
                    undefined;
                {cas_run, RunId, RFrom, RTo, RExpVersion, RReason, Event} ->
                    {ok, RV} =
                        cas_transition_equery(
                            C, RunId, RFrom, RTo, RExpVersion, maps:get(now, Effect), RReason
                        ),
                    ok = insert_event(C, Event),
                    RV
            end,
        case Decided of
            created ->
                {ok, EffectId, RunVersion};
            _Decided ->
                %% 同一外层事务内的 CAS（禁止嵌套 BEGIN，故不走 effect_to_tx）
                case
                    effect_to_equery(
                        C,
                        EffectId,
                        created,
                        Decided,
                        1,
                        maps:get(now, Effect),
                        decision_extras(Effect, Decided)
                    )
                of
                    {ok, 1, _C1, [{_V}]} -> {ok, EffectId, RunVersion};
                    {ok, 0, _C2, []} -> throw({abort_tx, cas_conflict})
                end
        end
    end).

%% ===================================================================
%% AG31-05 MEDIUM-2 加固（A2 review）：allow 决策持久化的 run 终态守卫。
%% 与 insert_effect_tx 同一 INSERT created→Decided CAS 流程，但在同一事务内
%% 先 SELECT ... FOR UPDATE 锁 run 行并要求 status='running'——堵住步骤 1
%% 无锁读与持久化之间的并发终态迁移（cancel/timeout）窗口（§14 Run
%% terminal → deny all new effects）。R7 严格附加：不改 insert_effect_tx
%% 既有行为（deny/approval 路径继续走原函数；approval 的 E07 CAS 自带
%% 版本守卫）。
%% ===================================================================

-spec insert_effect_guarded_tx(conn(), map()) ->
    {ok, pos_integer()}
    | {error, {run_not_active, binary() | not_found}}
    | {error, {duplicate_effect, binary()}}
    | {rollback, term()}.
insert_effect_guarded_tx(Conn, Effect) ->
    with_tx(Conn, fun(C) ->
        RunId = maps:get(run_id, Effect),
        case epgsql:equery(C, "SELECT status FROM agent_run WHERE id = $1 FOR UPDATE", [RunId]) of
            {ok, _Cols, [{<<"running">>}]} ->
                guarded_insert_decided(C, Effect);
            {ok, _Cols, [{OtherStatus}]} ->
                throw({abort_tx, {run_not_active, OtherStatus}});
            {ok, _Cols, []} ->
                throw({abort_tx, {run_not_active, not_found}})
        end
    end).

%% guarded 变体的 created→Decided 落账：与 insert_effect_tx 主体同流程
%% （R7 纯附加约束下的受控重复；行为差异仅事务开头的 run 行锁守卫）。
guarded_insert_decided(C, Effect) ->
    Sql =
        "INSERT INTO agent_effect (id, run_id, sequence, tool_id, capability, action, "
        "resource_digest, args_digest, status, authorization_reason, approval_ref, "
        "grant_version_checked, external_idempotency_key, version, created_at, updated_at) "
        "VALUES ($1,$2,$3,$4,$5,$6,$7,$8,'created',NULL,NULL,NULL,$9,1,$10,$10) "
        "RETURNING id",
    Params = [
        maps:get(id, Effect),
        maps:get(run_id, Effect),
        maps:get(sequence, Effect),
        maps:get(tool_id, Effect),
        maps:get(capability, Effect),
        maps:get(action, Effect),
        maps:get(resource_digest, Effect),
        maps:get(args_digest, Effect),
        maps:get(external_idempotency_key, Effect),
        maps:get(now, Effect)
    ],
    EffectId =
        case epgsql:equery(C, Sql, Params) of
            {ok, 1, _InsCols, [{Id}]} ->
                Id;
            {error, {error, _S, <<"23505">>, _N, _M, Extra}} ->
                throw({abort_tx, {duplicate_effect, constraint_name(Extra)}})
        end,
    Decided = maps:get(decided_status, Effect),
    case
        effect_to_equery(
            C,
            EffectId,
            created,
            Decided,
            1,
            maps:get(now, Effect),
            decision_extras(Effect, Decided)
        )
    of
        {ok, 1, _C1, [{_V}]} -> {ok, EffectId};
        {ok, 0, _C2, []} -> throw({abort_tx, cas_conflict})
    end.

%% Effect CAS 子状态迁移（§10.3 链；ExtraCols 白名单限定可写列）。
-spec effect_to_tx(pid(), integer(), atom(), atom(), integer(), dt(), #{
    atom() => binary() | integer()
}) ->
    {ok, pos_integer()} | {error, cas_conflict} | {rollback, term()}.
effect_to_tx(Conn, EffectId, From, To, ExpectedVersion, Now, ExtraCols) ->
    with_tx(Conn, fun(C) ->
        case effect_to_equery(C, EffectId, From, To, ExpectedVersion, Now, ExtraCols) of
            {ok, 1, _Cols, [{V}]} -> {ok, V};
            {ok, 0, _Cols, []} -> throw({abort_tx, cas_conflict})
        end
    end).

%% effect_to 的纯 equery 形态（无事务边界；供 effect_to_tx / insert_effect_tx 复用）。
%% UPDATE...RETURNING 的 equery 形状是 {ok, Count, Columns, Rows}（4 元组）。
-spec effect_to_equery(pid(), integer(), atom(), atom(), integer(), dt(), #{
    atom() => binary() | integer()
}) ->
    {ok, 1, list(), [{pos_integer()}]} | {ok, 0, list(), []}.
effect_to_equery(C, EffectId, From, To, ExpectedVersion, Now, ExtraCols) ->
    {SetFrag, ExtraVals} = extra_set_frag(maps:to_list(ExtraCols), 5),
    %% WHERE 的 version 占位符排在 extras 之后（extras 从 $5 起），
    %% 且参数严格按占位符序排列——version 与 extras 类型不同，不得共用占位符。
    VPos = 5 + length(ExtraVals),
    Sql =
        "UPDATE agent_effect SET status = $3, version = version + 1, updated_at = $4" ++
            SetFrag ++
            " WHERE id = $1 AND status = $2 AND version = $" ++
            integer_to_list(VPos) ++
            " RETURNING version",
    Params =
        [EffectId, atom_to_binary(From, utf8), atom_to_binary(To, utf8), Now] ++
            ExtraVals ++ [ExpectedVersion],
    epgsql:equery(C, Sql, Params).

%% 组合事务：Effect CAS + （可选的）Run CAS 迁移 + event，三者同一事务提交
%% （approve/reject 场景：effect waiting_approval→authorized|denied 与
%% run E12/E13 原子生效；任一 CAS 失败整体回滚）。
-spec combined_effect_run_tx(
    conn(),
    dt(),
    integer(),
    atom(),
    atom(),
    integer(),
    #{atom() => binary() | integer()},
    none | {cas_run, integer(), atom(), atom(), integer(), atom() | undefined, map()}
) ->
    {ok, pos_integer(), pos_integer() | undefined}
    | {error, cas_conflict}
    | {rollback, term()}.
combined_effect_run_tx(Conn, Now, EffectId, EFrom, ETo, EExpV, Extras, RunOp) ->
    with_tx(Conn, fun(C) ->
        RunVersion =
            case RunOp of
                none ->
                    undefined;
                {cas_run, RunId, RFrom, RTo, RExpV, RReason, Event} ->
                    {ok, RV} =
                        cas_transition_equery(C, RunId, RFrom, RTo, RExpV, Now, RReason),
                    ok = insert_event(C, Event),
                    RV
            end,
        case effect_to_equery(C, EffectId, EFrom, ETo, EExpV, Now, Extras) of
            {ok, 1, _Cols, [{EV}]} ->
                {ok, EV, RunVersion};
            {ok, 0, _Cols, []} ->
                throw({abort_tx, cas_conflict})
        end
    end).

%% ===================================================================
%% 内部：事务包装 / event 写入
%% ===================================================================

%% epgsql:with_transaction/2（reraise=false）把 throw 的 {abort_tx, Reason}
%% 原样包成 {rollback, Reason}。
with_tx(Conn, Fun) ->
    case epgsql:with_transaction(Conn, Fun) of
        {rollback, {abort_tx, Reason}} -> {error, Reason};
        {rollback, Reason} -> {rollback, Reason};
        Reply -> Reply
    end.

-spec insert_event(pid(), map()) -> ok.
insert_event(C, Ev) ->
    Sql =
        "INSERT INTO agent_run_event (id, run_id, from_status, to_status, reason_code, "
        "actor_kind, actor_id, detail_json, idempotency_key, created_at) "
        "VALUES ($1,$2,$3,$4,$5,$6,$7,$8::jsonb,$9,$10)",
    {ok, 1} =
        epgsql:equery(C, Sql, [
            maps:get(id, Ev),
            maps:get(run_id, Ev),
            reason_to_b(maps:get(from_status, Ev, undefined)),
            atom_to_binary(maps:get(to_status, Ev), utf8),
            reason_to_b(maps:get(reason_code, Ev, undefined)),
            atom_to_binary(maps:get(actor_kind, Ev), utf8),
            maps:get(actor_id, Ev),
            detail_json(maps:get(detail, Ev, #{})),
            maps:get(idempotency_key, Ev),
            maps:get(now, Ev)
        ]),
    ok.

%% ===================================================================
%% 内部：SQL 片段 / 行转换
%% ===================================================================

lifecycle_col(queued) -> "queued_at";
lifecycle_col(running) -> "started_at";
lifecycle_col(S) when S =:= succeeded; S =:= failed; S =:= cancelled -> "finished_at";
lifecycle_col(_) -> "updated_at".

%% 白名单列的 SET 片段；参数序号从 N 起与返回的值列表一一对应。
extra_set_frag([], _N) ->
    {[], []};
extra_set_frag([{K, V} | Rest], N) ->
    {FragRest, ValsRest} = extra_set_frag(Rest, N + 1),
    Frag = ", " ++ extra_col(K) ++ " = $" ++ integer_to_list(N),
    {[Frag | FragRest], [V | ValsRest]}.

extra_col(result_digest) -> "result_digest";
extra_col(failure_code) -> "failure_code";
extra_col(authorization_reason) -> "authorization_reason";
extra_col(grant_version_checked) -> "grant_version_checked";
extra_col(approval_ref) -> "approval_ref".

constraint_name(Extra) ->
    case lists:keyfind(constraint_name, 1, Extra) of
        {constraint_name, Name} -> Name;
        false -> <<"unknown">>
    end.

decision_extras(Effect, authorized) ->
    #{
        authorization_reason => <<"allow">>,
        grant_version_checked => maps:get(grant_version_checked, Effect, undefined)
    };
decision_extras(Effect, denied) ->
    #{authorization_reason => reason_to_b(maps:get(denial_reason, Effect, undefined))};
decision_extras(_Effect, waiting_approval) ->
    #{authorization_reason => <<"approval_required">>};
decision_extras(_Effect, created) ->
    #{}.

detail_json(Map) when map_size(Map) =:= 0 ->
    <<"{}">>;
detail_json(Map) ->
    %% sanitized metadata 只承载扁平 scalar（不嵌套、不存敏感原文）。
    Pairs = lists:map(
        fun({K, V}) ->
            KBin =
                case K of
                    KA when is_atom(KA) -> atom_to_binary(KA, utf8);
                    KI when is_integer(KI) -> integer_to_binary(KI);
                    KB when is_binary(KB) -> KB
                end,
            VBin =
                case V of
                    A when is_atom(A) -> atom_to_binary(A, utf8);
                    I when is_integer(I) -> integer_to_binary(I);
                    B when is_binary(B) -> B
                end,
            [$", KBin, $", ":", $", VBin, $"]
        end,
        lists:sort(maps:to_list(Map))
    ),
    iolist_to_binary(["{", lists:join(",", Pairs), "}"]).

run_row_to_map(
    {Id, AgentId, OrgId, WorkspaceId, GrantId, GrantVersionAtStart, DelegatingPrincipalId,
        TriggerTypeB, TriggerId, RuntimeTypeB, StatusB, ReasonCode, Version, ContextDigest, IdemKey,
        LeaseOwner, LeaseExpiresAt, Attempt, CreatedAt, QueuedAt, StartedAt, FinishedAt, UpdatedAt}
) ->
    #{
        id => Id,
        agent_id => AgentId,
        organization_id => OrgId,
        workspace_id => WorkspaceId,
        grant_id => GrantId,
        grant_version_at_start => GrantVersionAtStart,
        delegating_principal_id => DelegatingPrincipalId,
        trigger_type => binary_to_atom(TriggerTypeB, utf8),
        trigger_id => TriggerId,
        runtime_type => binary_to_atom(RuntimeTypeB, utf8),
        status => binary_to_atom(StatusB, utf8),
        reason_code => ReasonCode,
        version => Version,
        context_digest => ContextDigest,
        idempotency_key => IdemKey,
        lease_owner => LeaseOwner,
        lease_expires_at => LeaseExpiresAt,
        attempt => Attempt,
        created_at => CreatedAt,
        queued_at => QueuedAt,
        started_at => StartedAt,
        finished_at => FinishedAt,
        updated_at => UpdatedAt
    }.

effect_row_to_map(
    {Id, RunId, Sequence, ToolId, Capability, Action, ResourceDigest, ArgsDigest, StatusB,
        AuthorizationReason, ApprovalRef, GrantVersionChecked, ExternalKey, ResultDigest,
        FailureCode, Version, CreatedAt, UpdatedAt}
) ->
    #{
        id => Id,
        run_id => RunId,
        sequence => Sequence,
        tool_id => ToolId,
        capability => Capability,
        action => Action,
        resource_digest => ResourceDigest,
        args_digest => ArgsDigest,
        status => binary_to_atom(StatusB, utf8),
        authorization_reason => AuthorizationReason,
        approval_ref => ApprovalRef,
        grant_version_checked => GrantVersionChecked,
        external_idempotency_key => ExternalKey,
        result_digest => ResultDigest,
        failure_code => FailureCode,
        version => Version,
        created_at => CreatedAt,
        updated_at => UpdatedAt
    }.

reason_to_b(undefined) -> undefined;
reason_to_b(R) when is_atom(R) -> atom_to_binary(R, utf8);
reason_to_b(R) when is_binary(R) -> R.
