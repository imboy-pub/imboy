%%% @doc EB-03R P11：离职交接（offboarding）case / item / CAS 的持久化能力。
%%%
%%% 依据：用户裁决 §三原文——「为 EB-08 预先补齐 offboarding case/item/CAS 持久化
%%% 能力，**或**明确扩展 EB-08 infrastructure 租约；不允许再到 EB-08 才发现无 SQL owner」。
%%% 本卡采取**前者**：能力在 EB-03R 一次补齐，租约仍归 EB-03R（单一写所有者）。
%%%
%%% `enterprise_offboarding_case` / `_item` 是 **Org 级**表（没有 workspace 列），
%%% 但 Port 的信封仍是 `(OrgId, WorkspaceId, ...)`（铁律 6）：`workspace` 在每条
%%% 语句里做归属校验，workspace 不属于该 Org 时**一行都写不进去**，也不会写错租户。
%%%
%%% 幂等与重试（EB-08-A04）：
%%%   * 建档重放由 `uq_eoi_org_idempotency_key` 裁决（0 行 → `{error, conflict}`）；
%%%   * 重试走 `update_item/3`：`status` 置回 `pending`、`attempt` 递增、
%%%     `idempotency_key` **逐字不变**（重试不得产生第二条消息/审计）。
%%%
%%% CAS（铁律 7）：`advance_case/6` 的 `version` 不匹配即 `{error, conflict}`，
%%% 仅当影响行数为 1 时返回 `ok`；合法迁移由 domain `eb_offboarding:transition/2` 裁决，
%%% 本模块只执行、不自行发明状态机。
-module(eb_pg_offboarding_ext).

-moduledoc "离职交接（offboarding）case / item / CAS 持久化能力（EB-03R P11）。".
-export([
    insert_case/3,
    fetch_case/3,
    list_cases/2,
    insert_item/3,
    list_items/3,
    update_item/3,
    advance_case/6,
    update_case_counts/6,
    sql_statements/0
]).

-define(CASE_COLUMNS,
    "c.id, c.organization_id, c.leaver_user_id, c.successor_user_id, c.status, c.version,"
    " c.item_total, c.item_success, c.item_failed, c.created_by_user_id, c.reason,"
    " c.created_at, c.updated_at, c.completed_at"
).

-define(ITEM_COLUMNS,
    "i.id, i.organization_id, i.case_id, i.business_identity_id, i.function_key,"
    " i.from_user_id, i.to_user_id, i.status, i.idempotency_key, i.attempt,"
    " i.failure_reason, i.audit_event_id, i.created_at"
).

-define(SQL_INSERT_CASE, <<
    "INSERT INTO enterprise_offboarding_case"
    " (id, organization_id, leaver_user_id, successor_user_id, status, version,"
    "  created_by_user_id, reason)"
    " SELECT $3, $1, $4, $5, 'draft', 1, $6, $7"
    "   FROM workspace w"
    "  WHERE w.organization_id = $1 AND w.id = $2"
    " ON CONFLICT DO NOTHING"
    " RETURNING id"
>>).

-define(SQL_FETCH_CASE, <<
    "SELECT "
    ?CASE_COLUMNS
    "  FROM enterprise_offboarding_case c"
    "  JOIN workspace w ON w.id = $2 AND w.organization_id = c.organization_id"
    " WHERE c.organization_id = $1 AND c.id = $3"
>>).

-define(SQL_LIST_CASES, <<
    "SELECT "
    ?CASE_COLUMNS
    "  FROM enterprise_offboarding_case c"
    "  JOIN workspace w ON w.id = $2 AND w.organization_id = c.organization_id"
    " WHERE c.organization_id = $1"
    " ORDER BY c.id"
>>).

-define(SQL_INSERT_ITEM, <<
    "INSERT INTO enterprise_offboarding_item"
    " (id, organization_id, case_id, business_identity_id, function_key, from_user_id,"
    "  to_user_id, status, idempotency_key, attempt)"
    " SELECT $3, $1, c.id, i.id, i.function_key, $6, $7, 'pending', $8, 0"
    "   FROM workspace w"
    "   JOIN enterprise_offboarding_case c ON c.organization_id = $1 AND c.id = $4"
    "   JOIN organization_business_identity i ON i.organization_id = $1 AND i.id = $5"
    "  WHERE w.organization_id = $1 AND w.id = $2"
    " ON CONFLICT DO NOTHING"
    " RETURNING id"
>>).

-define(SQL_LIST_ITEMS, <<
    "SELECT "
    ?ITEM_COLUMNS
    "  FROM enterprise_offboarding_item i"
    "  JOIN workspace w ON w.id = $2 AND w.organization_id = i.organization_id"
    " WHERE i.organization_id = $1 AND i.case_id = $3"
    " ORDER BY i.id"
>>).

%% 重试：status/attempt/failure_reason 可改；idempotency_key 与 case 归属永不可改。
-define(SQL_UPDATE_ITEM, <<
    "UPDATE enterprise_offboarding_item i"
    "   SET status = $4,"
    "       attempt = $5,"
    "       failure_reason = $6,"
    "       audit_event_id = COALESCE($7, i.audit_event_id),"
    "       updated_at = now()"
    " WHERE i.organization_id = $1 AND i.id = $3"
    "   AND EXISTS (SELECT 1 FROM workspace w WHERE w.organization_id = $1 AND w.id = $2)"
    " RETURNING i.id"
>>).

%% 计数专用 CAS（FND-6）：只写三个计数控，不推状态、不递增 version ——
%% 供「状态不变但 items 终值已定」的时点（execute 全部成功后、verify 前）把
%% case 行计数落成真实值；version 不匹配同样 conflict。
-define(SQL_UPDATE_CASE_COUNTS, <<
    "UPDATE enterprise_offboarding_case c"
    "   SET item_total = $5,"
    "       item_success = $6,"
    "       item_failed = $7,"
    "       updated_at = now()"
    " WHERE c.organization_id = $1 AND c.id = $3 AND c.version = $4"
    "   AND c.status = $8"
    "   AND EXISTS (SELECT 1 FROM workspace w WHERE w.organization_id = $1 AND w.id = $2)"
    " RETURNING c.id, c.version"
>>).

%% CAS：version 必须精确匹配；仅当影响行数为 1 时提交。
%% FND-6（RULING-2026-09-15 §五）：item_total/item_success/item_failed 与 status
%% 在**同一条 CAS 语句**内更新 —— 计数来自真实 item 状态（调用方现算），与状态
%% 推进同事务同边界，不可能出现「状态已推进而计数滞留 0」或并发双写。
-define(SQL_ADVANCE_CASE, <<
    "UPDATE enterprise_offboarding_case c"
    "   SET status = $5,"
    "       item_total = $6,"
    "       item_success = $7,"
    "       item_failed = $8,"
    "       version = c.version + 1,"
    "       updated_at = now(),"
    "       completed_at = CASE WHEN $5 = 'completed' THEN now() ELSE c.completed_at END"
    " WHERE c.organization_id = $1 AND c.id = $3 AND c.version = $4"
    "   AND EXISTS (SELECT 1 FROM workspace w WHERE w.organization_id = $1 AND w.id = $2)"
    " RETURNING c.id"
>>).

%% @doc 冻结语句（双租户键机械判据用）。
-spec sql_statements() -> [binary()].
sql_statements() ->
    [
        ?SQL_INSERT_CASE,
        ?SQL_FETCH_CASE,
        ?SQL_LIST_CASES,
        ?SQL_INSERT_ITEM,
        ?SQL_LIST_ITEMS,
        ?SQL_UPDATE_ITEM,
        ?SQL_ADVANCE_CASE,
        ?SQL_UPDATE_CASE_COUNTS
    ].

%% @doc case 建档（输入只含 leaver/successor/reason/created_by；status 恒为 `draft`，
%% 状态机推进只能走 `advance_case/5`）。
-spec insert_case(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_case(OrgId, WorkspaceId, Case) when is_map(Case) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        Id = maps:get(id, Case, undefined),
        Params = [
            OrgId,
            WorkspaceId,
            Id,
            eb_pg_store_sql:nullify(maps:get(leaver_user_id, Case, undefined)),
            eb_pg_store_sql:nullify(maps:get(successor_user_id, Case, undefined)),
            eb_pg_store_sql:nullify(maps:get(created_by_user_id, Case, undefined)),
            eb_pg_store_sql:nullify(maps:get(reason, Case, undefined))
        ],
        case eb_pg_exec:insert_returning(?SQL_INSERT_CASE, Params) of
            {ok, _Row} -> fetch_case(OrgId, WorkspaceId, Id);
            {error, no_row} -> eb_pg_exec:conflict_or(OrgId, WorkspaceId, WorkspaceId);
            {error, _} = Err -> Err
        end
    end);
insert_case(_OrgId, _WorkspaceId, _Case) ->
    {error, invalid_case}.

%% @doc case 详情。
-spec fetch_case(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
fetch_case(OrgId, WorkspaceId, CaseId) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        eb_pg_exec:fetch_one(?SQL_FETCH_CASE, [OrgId, WorkspaceId, CaseId], case_fields())
    end).

%% @doc case 列表（Org 域）。
-spec list_cases(integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_cases(OrgId, WorkspaceId) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        eb_pg_exec:fetch_many(?SQL_LIST_CASES, [OrgId, WorkspaceId], case_fields())
    end).

%% @doc 交接项建档（幂等键 `uq_eoi_org_idempotency_key` 裁决重放）。
-spec insert_item(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_item(OrgId, WorkspaceId, Item) when is_map(Item) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        Id = maps:get(id, Item, undefined),
        Params = [
            OrgId,
            WorkspaceId,
            Id,
            maps:get(case_id, Item, undefined),
            maps:get(business_identity_id, Item, undefined),
            eb_pg_store_sql:nullify(maps:get(from_user_id, Item, undefined)),
            eb_pg_store_sql:nullify(maps:get(to_user_id, Item, undefined)),
            maps:get(idempotency_key, Item, undefined)
        ],
        case eb_pg_exec:insert_returning(?SQL_INSERT_ITEM, Params) of
            {ok, _Row} -> fetch_item(OrgId, WorkspaceId, Id);
            {error, no_row} -> eb_pg_exec:conflict_or(OrgId, WorkspaceId, WorkspaceId);
            {error, _} = Err -> Err
        end
    end);
insert_item(_OrgId, _WorkspaceId, _Item) ->
    {error, invalid_item}.

%% @doc case 下的交接项列表。
-spec list_items(integer(), integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_items(OrgId, WorkspaceId, CaseId) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        eb_pg_exec:fetch_many(?SQL_LIST_ITEMS, [OrgId, WorkspaceId, CaseId], item_fields())
    end).

%% @doc 交接项状态推进（含 EB-08-A04 的 failed → pending 重试）。
%%
%% `Item` 必含 `id`；可选 `status`（默认 `pending`）、`attempt`（默认取现值的下一次）、
%% `failure_reason`、`audit_event_id`。**`idempotency_key` 与 case 归属不可改**。
-spec update_item(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
update_item(OrgId, WorkspaceId, Item) when is_map(Item) ->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        Id = maps:get(id, Item, undefined),
        case fetch_item(OrgId, WorkspaceId, Id) of
            {ok, Current} ->
                Status = status_atom(maps:get(status, Item, pending)),
                Attempt = attempt_of(Item, Current),
                Params = [
                    OrgId,
                    WorkspaceId,
                    Id,
                    status_bin(Status),
                    Attempt,
                    failure_reason_arg(Status, Item),
                    eb_pg_store_sql:nullify(maps:get(audit_event_id, Item, undefined))
                ],
                case eb_pg_exec:execute_returning(?SQL_UPDATE_ITEM, Params) of
                    {ok, [_ | _]} -> fetch_item(OrgId, WorkspaceId, Id);
                    {ok, []} -> {error, conflict};
                    {error, _} = Err -> Err
                end;
            {error, _} = Err ->
                Err
        end
    end);
update_item(_OrgId, _WorkspaceId, _Item) ->
    {error, invalid_item}.

%% @doc case 状态 CAS 推进（FND-6：计数随状态同语句更新）。
%% `ExpectedVersion` 不匹配 ⇒ `{error, conflict}`；`Counts` 必须是
%% `#{total => N, success => N, failed => N}`（来自真实 item 状态的现算值）。
-spec advance_case(integer(), integer(), integer(), integer(), atom(), map()) ->
    ok | {error, term()}.
advance_case(OrgId, WorkspaceId, CaseId, ExpectedVersion, NextStatus, Counts) when
    is_integer(ExpectedVersion), is_atom(NextStatus), is_map(Counts)
->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        Params = [
            OrgId,
            WorkspaceId,
            CaseId,
            ExpectedVersion,
            status_bin(NextStatus),
            maps:get(total, Counts, 0),
            maps:get(success, Counts, 0),
            maps:get(failed, Counts, 0)
        ],
        case eb_pg_exec:execute_returning(?SQL_ADVANCE_CASE, Params) of
            {ok, [_ | _]} -> ok;
            {ok, []} -> {error, conflict};
            {error, _} = Err -> Err
        end
    end);
advance_case(_OrgId, _WorkspaceId, _CaseId, _ExpectedVersion, _NextStatus, _Counts) ->
    {error, invalid_cas_args}.

%% @doc 计数专用 CAS（FND-6）：不推状态、不递增 version，只落真实计数值。
%% `ExpectedVersion`/`ExpectedStatus` 不匹配 ⇒ conflict（并发或状态漂移即拒）。
-spec update_case_counts(integer(), integer(), integer(), integer(), atom(), map()) ->
    ok | {error, term()}.
update_case_counts(OrgId, WorkspaceId, CaseId, ExpectedVersion, ExpectedStatus, Counts) when
    is_integer(ExpectedVersion), is_atom(ExpectedStatus), is_map(Counts)
->
    eb_pg_exec:with_tenant(OrgId, WorkspaceId, fun() ->
        Params = [
            OrgId,
            WorkspaceId,
            CaseId,
            ExpectedVersion,
            maps:get(total, Counts, 0),
            maps:get(success, Counts, 0),
            maps:get(failed, Counts, 0),
            status_bin(ExpectedStatus)
        ],
        case eb_pg_exec:execute_returning(?SQL_UPDATE_CASE_COUNTS, Params) of
            {ok, [_ | _]} -> ok;
            {ok, []} -> {error, conflict};
            {error, _} = Err -> Err
        end
    end);
update_case_counts(_OrgId, _WorkspaceId, _CaseId, _ExpectedVersion, _ExpectedStatus, _Counts) ->
    {error, invalid_cas_args}.

%% ===================================================================
%% 内部
%% ===================================================================

fetch_item(OrgId, WorkspaceId, ItemId) ->
    eb_pg_exec:fetch_one(
        <<
            "SELECT "
            ?ITEM_COLUMNS
            "  FROM enterprise_offboarding_item i"
            "  JOIN workspace w ON w.id = $2 AND w.organization_id = i.organization_id"
            " WHERE i.organization_id = $1 AND i.id = $3"
        >>,
        [OrgId, WorkspaceId, ItemId],
        item_fields()
    ).

%% 失败原因列受 `ck_eoi_failure_reason` 约束：`failure_reason IS NULL OR status = 'failed'`。
%% 因此**重试（回到 pending）必须清空 failure_reason**——失败事实经 `audit_event_id` 保留，
%% 而不是留在这一列里制造「pending 行带失败原因」的伪状态。
failure_reason_arg(failed, Item) ->
    eb_pg_store_sql:nullify(maps:get(failure_reason, Item, undefined));
failure_reason_arg(_OtherStatus, _Item) ->
    null.

%% 重试语义：status 回到 pending 时 attempt 递增；其余情况沿用现值（除非显式给出）。
attempt_of(Item, Current) ->
    case maps:get(attempt, Item, undefined) of
        Attempt when is_integer(Attempt), Attempt >= 0 -> Attempt;
        _ ->
            case {status_atom(maps:get(status, Item, pending)), maps:get(attempt, Current, 0)} of
                {pending, CurrentAttempt} -> CurrentAttempt + 1;
                {_Other, CurrentAttempt} -> CurrentAttempt
            end
    end.

status_atom(pending) -> pending;
status_atom(success) -> success;
status_atom(failed) -> failed;
status_atom(Other) -> Other.

status_bin(draft) -> <<"draft">>;
status_bin(frozen) -> <<"frozen">>;
status_bin(transferring) -> <<"transferring">>;
status_bin(verifying) -> <<"verifying">>;
status_bin(completed) -> <<"completed">>;
status_bin(failed) -> <<"failed">>;
status_bin(pending) -> <<"pending">>;
status_bin(success) -> <<"success">>;
status_bin(Value) when is_binary(Value) -> Value.

case_fields() ->
    [
        {id, <<"id">>, int},
        {organization_id, <<"organization_id">>, int},
        {leaver_user_id, <<"leaver_user_id">>, int},
        {successor_user_id, <<"successor_user_id">>, int},
        {status, <<"status">>, atom},
        {version, <<"version">>, int},
        {item_total, <<"item_total">>, int},
        {item_success, <<"item_success">>, int},
        {item_failed, <<"item_failed">>, int},
        {created_by_user_id, <<"created_by_user_id">>, int},
        {reason, <<"reason">>, bin},
        {created_at, <<"created_at">>, ts},
        {completed_at, <<"completed_at">>, ts}
    ].

item_fields() ->
    [
        {id, <<"id">>, int},
        {organization_id, <<"organization_id">>, int},
        {case_id, <<"case_id">>, int},
        {business_identity_id, <<"business_identity_id">>, int},
        {function_key, <<"function_key">>, bin},
        {from_user_id, <<"from_user_id">>, int},
        {to_user_id, <<"to_user_id">>, int},
        {status, <<"status">>, atom},
        {idempotency_key, <<"idempotency_key">>, bin},
        {attempt, <<"attempt">>, int},
        {failure_reason, <<"failure_reason">>, raw},
        {audit_event_id, <<"audit_event_id">>, int},
        {created_at, <<"created_at">>, ts}
    ].
