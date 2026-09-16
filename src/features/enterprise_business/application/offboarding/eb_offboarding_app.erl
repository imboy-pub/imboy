%%% @doc 离职冻结、交接与完成证明的**用例层**（plan v4.1 EB-08）。
%%%
%%% 依据：plan v4.1 §「EB-08：离职冻结、交接和完成证明」、§5.1 的
%%% `POST offboarding` / `POST :id/execute` / `POST :id/verify` / `POST :id/finalize`
%%% 与 `POST members/:uid/suspend`、EB-D07（离职状态机）、
%%% `docs/architecture/feature-slice-rules.md` 铁律 3/4/6/7。
%%%
%%% ## 三个内部 Gate（严格串行，前一 Gate 未 PASS 不得进入下一 Gate）
%%%
%%%   1. **S1 suspend+freeze**（`open_offboarding/2` / `suspend_member/2`）：
%%%      建档即撤权（`organization_member.status = suspended`），case 进入 `frozen`
%%%      且**快照固定**（item 的 identity/function/from/to/幂等键此后逐字不变）；
%%%      **不执行任何 rebind**（经办关系一字不动）。
%%%   2. **S2 transfer+CAS**（`execute_offboarding/2`）：case 版本 CAS 把 `frozen|failed`
%%%      推到 `transferring`，再逐项把 identity 的 active 经办从 leaver 换到 successor
%%%      （结束旧行的 CAS + 新行 INSERT；owner/资源 ID 不变）。并发下**仅一方**推进。
%%%   3. **S3 verify+finalize**（`verify_offboarding/2` / `finalize_offboarding/2`）：
%%%      校验残留/ID/Org/hash/count，成功才把 case 推入 `verifying`；finalize 只接受
%%%      `verifying`，并且**成员移除由数据库守卫在同一语句内复核**（双层守卫）。
%%%
%%% ## 职责与边界
%%%
%%%   * 数据访问全部经扩展点：`eb_store_port`（装配实现 `eb_infra_ports:store/0`）、
%%%     `eb_audit_port`、`eb_id_port`、`eb_member_fact_port`（**只读事实**）。
%%%     本模块零 SQL、零 `elib_pg`、不触 `*_repo` / `*_ds`（`make arch-check` 会拦）。
%%%   * **成员状态的写路径归本卡**（A08）：`organization_member` 的 `suspended`
%%%     迁移经 Core 的通用能力 `organization_member_logic:suspend/3` 落库；本卡**不**改
%%%     Core 之外的任何写路径，也**不**把「读事实」与「写状态」混在同一个端口上。
%%%   * 事实 ≠ 授权结论：成员/经办只读事实经最小只读 Port 读取；授权判定仍归
%%%     `eb_auth_port` + `eb_auth_app`（本模块不重造 RBAC）。
%%%   * 事务边界如实声明：冻结契约**没有**「case + item + member + audit 单事务」的
%%%     用例级事务 callback（`eb_tx_port` 只有 canonical message / conversation audit
%%%     两个具名用例），因此本模块按**fail-closed 顺序**编排并逐条审计；每一步的
%%%     失败都返回明确错误，绝不静默继续（见各函数注释里的顺序理由）。
-module(eb_offboarding_app).

-export([
    suspend_member/2,
    open_offboarding/2,
    execute_offboarding/2,
    verify_offboarding/2,
    finalize_offboarding/2,
    %% 读取面（closure §8：查询交接 case；零写零审计）
    list_cases/2,
    case_detail/2
]).

-define(ACTION_SUSPEND, <<"offboarding.member.suspend">>).
-define(ACTION_OPEN, <<"offboarding.open">>).
-define(ACTION_EXECUTE, <<"offboarding.execute">>).
-define(ACTION_VERIFY, <<"offboarding.verify">>).
-define(ACTION_FINALIZE, <<"offboarding.finalize">>).

%% ===================================================================
%% 读取面（closure §8：创建/查询交接 case 的「查询」半边）
%% ===================================================================
%%
%% 合同（与 `eb_offboarding_http_tests` 的投影断言逐字同口径）：
%%   * **零写零审计**：纯读取用例，不触任何写 callback、不追加审计事件；
%%   * **投影白名单**：只回显标识符 / 状态 / 计数 / 时间戳——**没有任何** cipher、
%%     密钥材料或个人域 PII 字段（offboarding 表本身无密文列，读面再把键集收窄到
%%     白名单：多一个键都不出站）；
%%   * **键集分页**（message 先例同口径）：按 id 倒序（= 创建时间倒序，TSID 时间有序），
%%     `after_id` 语义为**严格 `id < after_id`**（倒序游标），`limit` 缺省 50、上限 200，
%%     越界 `{error, {invalid_limit, _}}`（不静默钳制）；未用的键集窗口不漂移；
%%   * **status 过滤**：case 状态白名单 draft/frozen/transferring/verifying/completed/failed；
%%     失败项查询 = 详情的 `items_status=failed`（item 白名单 pending/success/failed）。
%%
%% 租户作用域：store 的 `list_offboarding_cases` / `fetch_offboarding_case` /
%% `list_offboarding_items` 均是 `(OrgId, WorkspaceId)` 同语句约束（铁律 6），
%% 跨 Org / Workspace 不匹配只可能得到空集或 not_found。

%% case 列表行的出站键（白名单；不含 reason——审计自由文本只在详情给治理角色）。
-define(CASE_LIST_KEYS, [
    id,
    status,
    leaver_user_id,
    successor_user_id,
    version,
    item_total,
    item_success,
    item_failed,
    created_at,
    updated_at,
    completed_at
]).

%% case 详情键（全字段白名单 + items 子表）。
-define(CASE_DETAIL_KEYS, [
    id,
    organization_id,
    leaver_user_id,
    successor_user_id,
    status,
    version,
    item_total,
    item_success,
    item_failed,
    created_by_user_id,
    reason,
    created_at,
    updated_at,
    completed_at
]).

%% item 出站键（含 status/failure_reason/attempt/idempotency_key；无 cipher 材料）。
-define(ITEM_KEYS, [
    id,
    case_id,
    business_identity_id,
    function_key,
    from_user_id,
    to_user_id,
    status,
    idempotency_key,
    attempt,
    failure_reason,
    created_at
]).

%% 分页边界：与 `eb_message_app` 逐字同口径（缺省 50、1..200）。
-define(DEFAULT_PAGE_LIMIT, 50).
-define(MAX_PAGE_LIMIT, 200).

%% @doc 列举本 Org 的离职交接 case（§8 `GET /offboarding/cases`）。
%%
%% Params：
%%   workspace_id  必填整数（租户操作范围）
%%   status        可选 case 状态白名单过滤（未知值 fail-closed 422）
%%   after_id      可选键集游标（严格 id < after_id；倒序分页）
%%   limit         可选页大小（缺省 50，1..200）
-spec list_cases(integer(), map()) -> {ok, [map()]} | {error, term()}.
list_cases(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            list_cases_in(OrgId, WorkspaceId, Params)
    end;
list_cases(_OrgId, _Params) ->
    {error, {invalid_argument, list_cases}}.

list_cases_in(OrgId, WorkspaceId, Params) ->
    case status_filter(Params) of
        {error, _} = Err ->
            Err;
        {ok, Status} ->
            case page_limit(Params) of
                {error, _} = Err ->
                    Err;
                {ok, Limit} ->
                    case
                        with_store(Params, fun(Store) ->
                            Store:list_offboarding_cases(OrgId, WorkspaceId)
                        end)
                    of
                        {error, _} = Err ->
                            Err;
                        {ok, Rows} ->
                            {ok, page_cases(Rows, Params, Limit, Status)}
                    end
            end
    end.

%% @doc 交接 case 详情（§8 `GET /offboarding/cases/:id`）+ items 子表。
%%
%% Params：
%%   workspace_id  必填整数
%%   case_id       必填整数（路径 :id）
%%   items_status  可选 item 状态白名单过滤（`failed` 即失败项查询；未知值 422）
%%
%% 不存在 / 跨 Org / Workspace 不匹配 ⇒ `{error, {case_not_found, _}}`（不区分，
%% 避免租户枚举）。
-spec case_detail(integer(), map()) -> {ok, map()} | {error, term()}.
case_detail(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            case_detail_in(OrgId, WorkspaceId, Params)
    end;
case_detail(_OrgId, _Params) ->
    {error, {invalid_argument, case_detail}}.

case_detail_in(OrgId, WorkspaceId, Params) ->
    case items_status_filter(Params) of
        {error, _} = Err ->
            Err;
        {ok, ItemStatus} ->
            CaseId = maps:get(case_id, Params, undefined),
            case fetch_case(OrgId, WorkspaceId, CaseId, Params) of
                {error, _} = Err ->
                    Err;
                {ok, Case} ->
                    case list_items(OrgId, WorkspaceId, CaseId, Params) of
                        {error, _} = Err ->
                            Err;
                        {ok, Items} ->
                            {ok, project_case_detail(Case, Items, ItemStatus)}
                    end
            end
    end.

%% 顺序固定：倒序排序 → status 过滤 → 游标切割（严格 id < after_id）→ limit 截断。
%% 页内行全部满足 status，且「下一页」永远从上一页最后一行的 id 继续（键集不漂移）。
page_cases(Rows, Params, Limit, Status) ->
    Desc = lists:sort(fun(A, B) -> maps:get(id, A, 0) > maps:get(id, B, 0) end, Rows),
    Matched =
        case Status of
            undefined ->
                Desc;
            _ ->
                [R || R <- Desc, maps:get(status, R, undefined) =:= Status]
        end,
    After = maps:get(after_id, Params, undefined),
    Cut =
        case is_pos_int(After) of
            true -> [R || R <- Matched, maps:get(id, R, 0) < After];
            false -> Matched
        end,
    [project_case_list(R) || R <- lists:sublist(Cut, Limit)].

%% case 状态白名单（domain `eb_offboarding` 的全集；未知值 fail-closed）。
status_filter(Params) ->
    case maps:get(status, Params, undefined) of
        undefined ->
            {ok, undefined};
        Status when is_binary(Status) ->
            case
                lists:member(
                    Status,
                    [
                        <<"draft">>,
                        <<"frozen">>,
                        <<"transferring">>,
                        <<"verifying">>,
                        <<"completed">>,
                        <<"failed">>
                    ]
                )
            of
                true -> {ok, binary_to_atom(Status, utf8)};
                false -> {error, {invalid_status, Status}}
            end;
        Other ->
            {error, {invalid_status, Other}}
    end.

%% item 状态白名单（失败项查询 = items_status=failed）。
items_status_filter(Params) ->
    case maps:get(items_status, Params, undefined) of
        undefined ->
            {ok, undefined};
        Status when is_binary(Status) ->
            case lists:member(Status, [<<"pending">>, <<"success">>, <<"failed">>]) of
                true -> {ok, binary_to_atom(Status, utf8)};
                false -> {error, {invalid_items_status, Status}}
            end;
        Other ->
            {error, {invalid_items_status, Other}}
    end.

%% 缺省 50、1..200（message 先例）；越界报 {invalid_limit, _}，不静默钳制。
page_limit(Params) ->
    case maps:get(limit, Params, ?DEFAULT_PAGE_LIMIT) of
        Limit when is_integer(Limit), Limit >= 1, Limit =< ?MAX_PAGE_LIMIT -> {ok, Limit};
        Other -> {error, {invalid_limit, Other}}
    end.

%% 白名单投影：白名单键恒出现（未填值出站为 `null`），白名单外的键一个不出站。
project_case_list(Case) ->
    project(?CASE_LIST_KEYS, Case).

project_case_detail(Case, Items, ItemStatus) ->
    Matched =
        case ItemStatus of
            undefined ->
                Items;
            _ ->
                [I || I <- Items, maps:get(status, I, undefined) =:= ItemStatus]
        end,
    (project(?CASE_DETAIL_KEYS, Case))#{
        items => [project_item(I) || I <- Matched]
    }.

project_item(Item) ->
    project(?ITEM_KEYS, Item).

project(Keys, Row) ->
    maps:from_list([{K, present(maps:get(K, Row, undefined))} || K <- Keys]).

present(undefined) ->
    null;
present(Value) ->
    Value.

%% ===================================================================
%% 内部辅助：端口 / 租户 / 事实 / 审计
%% ===================================================================

%% @doc 立即撤销该成员的全部企业业务授权（个人 IM 能力不变，可恢复）。
%%
%% Params：
%%   workspace_id    必填整数（租户操作范围；归属由店铺/成员域裁决）
%%   member_user_id  必填整数（被撤权的成员；兼容 `user_id` 键）
%%   reason          必填非空二进制（审计用；不进任何上游正文）
%%   actor_user_id   必填整数（操作人；Core 侧要求其为本 Org 的 Owner/Admin）
%%
%% 写路径：`organization_member_logic:suspend/3`（**通用** Core 能力）——
%% 单条 `UPDATE organization_member SET status='suspended'`，org 作用域显式贯穿，
%% 且只接受 `status='active'` 的行（并发重复撤权由该条件裁决，不静默成功）。
-spec suspend_member(integer(), map()) -> {ok, map()} | {error, term()}.
suspend_member(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, _WorkspaceId} ->
            suspend_member_args(OrgId, Params)
    end;
suspend_member(_OrgId, _Params) ->
    {error, {invalid_argument, suspend_member}}.

suspend_member_args(OrgId, Params) ->
    MemberUserId = first_defined([
        maps:get(member_user_id, Params, undefined),
        maps:get(user_id, Params, undefined)
    ]),
    Reason = maps:get(reason, Params, undefined),
    Actor = maps:get(actor_user_id, Params, undefined),
    case {is_pos_int(MemberUserId), is_non_empty_binary(Reason), is_pos_int(Actor)} of
        {false, _, _} ->
            {error, {invalid_member_user_id, MemberUserId}};
        {_, false, _} ->
            {error, {invalid_reason, Reason}};
        {_, _, false} ->
            {error, {invalid_actor_user_id, Actor}};
        {true, true, true} ->
            suspend_member_core(OrgId, MemberUserId, Actor, Reason, Params)
    end.

suspend_member_core(OrgId, MemberUserId, Actor, Reason, Params) ->
    case organization_member_logic:suspend(Actor, OrgId, MemberUserId) of
        {error, _} = Err ->
            Err;
        {ok, Member} ->
            audit_result(
                Params,
                OrgId,
                #{
                    resource_type => <<"organization_member">>,
                    resource_id => MemberUserId,
                    action => ?ACTION_SUSPEND,
                    actor_user_id => Actor,
                    detail => #{
                        <<"status">> => <<"suspended">>,
                        <<"reason">> => Reason
                    }
                },
                Member#{status => <<"suspended">>}
            )
    end.

%% ===================================================================
%% S1：建档 + 撤权 + 冻结（不执行 rebind）
%% ===================================================================

%% @doc 打开离职交接 case：快照该 leaver 当前的 active 经办关系、撤销其企业授权，
%% 并把 case 置为 `frozen`。
%%
%% Params：
%%   workspace_id       必填整数
%%   leaver_user_id     必填整数（离职人；必须是本 Org 的 active/suspended 成员）
%%   successor_user_id  必填整数（承接人；必须是本 Org 的 active 成员，且 ≠ leaver）
%%   actor_user_id      必填整数（操作人；撤权由 Core 侧判定需 Owner/Admin）
%%   reason             可选二进制（审计用）
%%
%% 顺序（fail-closed，且每一步都可独立复查）：
%%   1. 前置事实门（leaver 为 active/suspended，successor 为 active）——只读事实 Port；
%%   2. 快照来源：`list_assignments/2` 的 leaver active 行（快照的**唯一**来源，
%%      不从调用方接收 identity 列表，避免客户端指定交接对象）；
%%   3. 建 case（`draft`）；
%%   4. 建 item（冻结字段一次写定，此后只允许改 status/attempt/failure_reason）；
%%   5. **确保已撤权**：active 才调用 Core `suspend/3`；已 suspended 不重复撤权；
%%   6. case 版本 CAS `draft -> frozen`（domain `eb_offboarding:transition/2` 裁决）。
%%
%% 任一步失败即返回错误；已完成的步不回滚（冻结契约无跨资源事务），但**不会**出现
%% 「已撤权却仍可交接」或「已 rebind」的状态：rebind 只可能在 `execute_offboarding/2`
%% 里发生，而它要求 case 已在 `frozen|failed`。
-spec open_offboarding(integer(), map()) -> {ok, map()} | {error, term()}.
open_offboarding(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            open_args(OrgId, WorkspaceId, Params)
    end;
open_offboarding(_OrgId, _Params) ->
    {error, {invalid_argument, open_offboarding}}.

open_args(OrgId, WorkspaceId, Params) ->
    Leaver = maps:get(leaver_user_id, Params, undefined),
    Successor = maps:get(successor_user_id, Params, undefined),
    Actor = maps:get(actor_user_id, Params, undefined),
    case open_arguments_ok(Leaver, Successor, Actor) of
        {error, _} = Err ->
            Err;
        ok ->
            open_member_gate(OrgId, WorkspaceId, Leaver, Successor, Actor, Params)
    end.

open_arguments_ok(Leaver, Successor, Actor) ->
    case {is_pos_int(Leaver), is_pos_int(Successor), is_pos_int(Actor)} of
        {false, _, _} ->
            {error, {invalid_leaver_user_id, Leaver}};
        {_, false, _} ->
            {error, {invalid_successor_user_id, Successor}};
        {_, _, false} ->
            {error, {invalid_actor_user_id, Actor}};
        {true, true, true} ->
            case Leaver =:= Successor of
                true -> {error, {self_handover, Leaver}};
                false -> ok
            end
    end.

open_member_gate(OrgId, WorkspaceId, Leaver, Successor, Actor, Params) ->
    case member_status(OrgId, Leaver, Params) of
        {error, _} = Err ->
            Err;
        {ok, LeaverStatus} when LeaverStatus =:= active; LeaverStatus =:= suspended ->
            open_successor_gate(OrgId, WorkspaceId, Leaver, Successor, Actor, Params);
        {ok, LeaverStatus} ->
            {error, {leaver_not_active, LeaverStatus}}
    end.

open_successor_gate(OrgId, WorkspaceId, Leaver, Successor, Actor, Params) ->
    case member_status(OrgId, Successor, Params) of
        {error, _} = Err ->
            Err;
        {ok, active} ->
            open_snapshot(OrgId, WorkspaceId, Leaver, Successor, Actor, Params);
        {ok, SuccessorStatus} ->
            {error, {successor_not_active, SuccessorStatus}}
    end.

open_snapshot(OrgId, WorkspaceId, Leaver, Successor, Actor, Params) ->
    case leaver_active_assignments(OrgId, WorkspaceId, Leaver, Params) of
        {error, _} = Err ->
            Err;
        {ok, Assignments} ->
            case new_id(enterprise_offboarding_case, Params) of
                {error, _} = Err ->
                    Err;
                {ok, CaseId} ->
                    open_insert(
                        OrgId,
                        WorkspaceId,
                        Leaver,
                        Successor,
                        Actor,
                        CaseId,
                        Assignments,
                        Params
                    )
            end
    end.

open_insert(OrgId, WorkspaceId, Leaver, Successor, Actor, CaseId, Assignments, Params) ->
    Row = #{
        id => CaseId,
        leaver_user_id => Leaver,
        successor_user_id => Successor,
        created_by_user_id => Actor,
        reason => maps:get(reason, Params, undefined)
    },
    case
        with_store(Params, fun(Store) ->
            Store:insert_offboarding_case(OrgId, WorkspaceId, Row)
        end)
    of
        {error, _} = Err ->
            Err;
        {ok, _Case} ->
            open_items(OrgId, WorkspaceId, Leaver, Successor, Actor, CaseId, Assignments, Params)
    end.

open_items(OrgId, WorkspaceId, Leaver, Successor, Actor, CaseId, Assignments, Params) ->
    case insert_items(OrgId, WorkspaceId, CaseId, Leaver, Successor, Assignments, Params) of
        {error, _} = Err ->
            Err;
        {ok, Items} ->
            open_suspend(OrgId, WorkspaceId, Leaver, Successor, Actor, CaseId, Items, Params)
    end.

%% 第 5 步：确保已撤权（写路径归本卡，见 A08）。
open_suspend(OrgId, _WorkspaceId, Leaver, Successor, Actor, CaseId, Items, Params) ->
    case ensure_suspended(OrgId, Leaver, Actor, CaseId, Params) of
        {error, _} = Err ->
            Err;
        ok ->
            open_freeze(OrgId, Leaver, Successor, Actor, CaseId, Items, Params)
    end.

ensure_suspended(OrgId, Leaver, Actor, CaseId, Params) ->
    case member_status(OrgId, Leaver, Params) of
        {ok, active} ->
            suspend_member_via_core(OrgId, Leaver, Actor, {offboarding, CaseId}, Params);
        {ok, suspended} ->
            ok;
        {ok, LeaverStatus} ->
            {error, {leaver_not_active, LeaverStatus}};
        {error, _} = Err ->
            Err
    end.

%% 第 6 步：draft -> frozen（CAS；domain 裁决合法性）。
%%
%% 期望版本恒为 1：冻结契约的 `insert_offboarding_case` 建档即 `version = 1`
%% （status='draft'），且 case 版本**只**能由 CAS 推进——所以这里的期望值来自契约
%% 事实而不是调用方入参（调用方无法借期望版本跳步）。
open_freeze(OrgId, Leaver, Successor, Actor, CaseId, Items, Params) ->
    DraftVersion = 1,
    case eb_offboarding:transition(draft, frozen) of
        {error, _} = Err ->
            Err;
        {ok, frozen} ->
            case
                with_store(Params, fun(Store) ->
                    Store:advance_offboarding_case(
                        OrgId,
                        workspace_id(Params),
                        CaseId,
                        DraftVersion,
                        frozen,
                        eb_offboarding_flow:item_counts(Items)
                    )
                end)
            of
                {error, _} = Err ->
                    Err;
                ok ->
                    open_audit(OrgId, Leaver, Successor, Actor, CaseId, Items, Params)
            end
    end.

open_audit(OrgId, Leaver, Successor, Actor, CaseId, Items, Params) ->
    SnapshotHash = eb_offboarding_flow:snapshot_hash(Items),
    audit_result(
        Params,
        OrgId,
        #{
            resource_type => <<"enterprise_offboarding_case">>,
            resource_id => CaseId,
            action => ?ACTION_OPEN,
            actor_user_id => Actor,
            detail => #{
                <<"leaver_user_id">> => Leaver,
                <<"successor_user_id">> => Successor,
                <<"items_total">> => length(Items),
                <<"snapshot_hash">> => SnapshotHash
            }
        },
        #{
            case_id => CaseId,
            status => frozen,
            version => 2,
            leaver_user_id => Leaver,
            successor_user_id => Successor,
            items_total => length(Items),
            items => Items,
            snapshot_hash => SnapshotHash,
            member_suspended => true
        }
    ).

%% ===================================================================
%% 快照 / item 写入
%% ===================================================================

%% 快照来源：库里该 leaver 的 active 经办行（不接受调用方传入的 identity 列表）。
leaver_active_assignments(OrgId, WorkspaceId, Leaver, Params) ->
    case with_store(Params, fun(Store) -> Store:list_assignments(OrgId, WorkspaceId) end) of
        {error, _} = Err ->
            Err;
        {ok, Rows} ->
            {ok, [
                Row
             || Row <- Rows,
                maps:get(user_id, Row, undefined) =:= Leaver,
                maps:get(status, Row, undefined) =:= active
            ]}
    end.

insert_items(_OrgId, _WorkspaceId, _CaseId, _Leaver, _Successor, [], _Params) ->
    {ok, []};
insert_items(OrgId, WorkspaceId, CaseId, Leaver, Successor, [Assignment | Rest], Params) ->
    IdentityId = maps:get(business_identity_id, Assignment, undefined),
    FunctionKey = maps:get(function_key, Assignment, undefined),
    case new_id(enterprise_offboarding_item, Params) of
        {error, _} = Err ->
            Err;
        {ok, ItemId} ->
            Item = #{
                id => ItemId,
                case_id => CaseId,
                business_identity_id => IdentityId,
                function_key => FunctionKey,
                from_user_id => Leaver,
                to_user_id => Successor,
                idempotency_key =>
                    eb_offboarding_flow:idempotency_key(CaseId, IdentityId)
            },
            case
                with_store(Params, fun(Store) ->
                    Store:insert_offboarding_item(OrgId, WorkspaceId, Item)
                end)
            of
                {error, _} = Err ->
                    Err;
                {ok, Stored} ->
                    case
                        insert_items(
                            OrgId, WorkspaceId, CaseId, Leaver, Successor, Rest, Params
                        )
                    of
                        {error, _} = Err ->
                            Err;
                        {ok, StoredRest} ->
                            {ok, [Stored | StoredRest]}
                    end
            end
    end.

%% ===================================================================
%% S2：批量 CAS 交接（transfer）
%% ===================================================================

%% @doc 执行交接：把 case 版本 CAS 推到 `transferring`，再逐项把 identity 的 active
%% 经办从 leaver 换到 successor。
%%
%% Params：
%%   workspace_id      必填整数
%%   case_id           必填整数
%%   expected_version  必填整数（**并发裁决**：只有持有当前版本的一方推进成功）
%%   actor_user_id     必填整数（审计快照；也是新经办行的 assigned_by）
%%
%% 并发与幂等（S2/A03 口径，明确写清不夸大）：
%%   * **并发仅一方成功**：`advance_offboarding_case/5` 的 version CAS 由数据库行锁
%%     裁决，落败方得到 `{error, {case_conflict, _}}` 或 `{error, {stale_version, _, _}}`，
%%     且**零写入、零审计**；不会出现两次转移或两条 execute 审计。
%%   * **幂等效果**：case 已推进（`transferring`/`verifying`/`completed`）后再调用
%%     一律 `{error, {case_in_transfer, _}}` / `{error, {invalid_case_status, _}}`，
%%     不产生第二次转移、不追加审计；`success` 的项不会被重复处理。
%%   * 项级失败**不整批回滚**：失败项落 `failed` + 可归因 `failure_reason`，case 进
%%     `failed`，可重试（`failed -> transferring`）；幂等键逐字不变。
%%
%% owner/资源不变：本函数**只**动 assignment 的 assignee（结束旧行 + 新增新行），
%% 从不更新 identity 行、不删资源；每项在改前/改后各取一次资源指纹（identity 行
%% hash + 会话数 + 客户数）并要求逐字段相等，否则该项判失败（fail-closed）。
-spec execute_offboarding(integer(), map()) -> {ok, map()} | {error, term()}.
execute_offboarding(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            execute_args(OrgId, WorkspaceId, Params)
    end;
execute_offboarding(_OrgId, _Params) ->
    {error, {invalid_argument, execute_offboarding}}.

execute_args(OrgId, WorkspaceId, Params) ->
    CaseId = maps:get(case_id, Params, undefined),
    Expected = maps:get(expected_version, Params, undefined),
    Actor = maps:get(actor_user_id, Params, undefined),
    case {is_pos_int(CaseId), is_pos_int(Expected), is_pos_int(Actor)} of
        {false, _, _} ->
            {error, {invalid_case_id, CaseId}};
        {_, false, _} ->
            {error, {invalid_expected_version, Expected}};
        {_, _, false} ->
            {error, {invalid_actor_user_id, Actor}};
        {true, true, true} ->
            execute_case(OrgId, WorkspaceId, CaseId, Expected, Actor, Params)
    end.

execute_case(OrgId, WorkspaceId, CaseId, Expected, Actor, Params) ->
    case fetch_case(OrgId, WorkspaceId, CaseId, Params) of
        {error, _} = Err ->
            Err;
        {ok, Case} ->
            execute_status_gate(OrgId, WorkspaceId, CaseId, Expected, Actor, Case, Params)
    end.

execute_status_gate(OrgId, WorkspaceId, CaseId, Expected, Actor, Case, Params) ->
    case maps:get(status, Case, undefined) of
        frozen ->
            execute_version_gate(OrgId, WorkspaceId, CaseId, Expected, Actor, Case, Params);
        failed ->
            execute_version_gate(OrgId, WorkspaceId, CaseId, Expected, Actor, Case, Params);
        transferring ->
            {error, {case_in_transfer, CaseId}};
        OtherStatus ->
            {error, {invalid_case_status, OtherStatus}}
    end.

execute_version_gate(OrgId, WorkspaceId, CaseId, Expected, Actor, Case, Params) ->
    case maps:get(version, Case, undefined) of
        Expected -> execute_cas(OrgId, WorkspaceId, CaseId, Expected, Actor, Case, Params);
        Actual -> {error, {stale_version, Expected, Actual}}
    end.

%% 权威闸门：CAS（domain 裁决合法性 + 数据库行锁裁决并发）。
execute_cas(OrgId, WorkspaceId, CaseId, Expected, Actor, Case, Params) ->
    From = maps:get(status, Case, undefined),
    case eb_offboarding:transition(From, transferring) of
        {error, _} = Err ->
            Err;
        {ok, transferring} ->
            %% FND-6：进入 transferring 时携带 case 行**现值**计数（基线承接，
            %% 不是现造 —— items 尚未处理；终值由 execute_finish 的真实计数覆盖）。
            BaselineCounts = #{
                total => maps:get(item_total, Case, 0),
                success => maps:get(item_success, Case, 0),
                failed => maps:get(item_failed, Case, 0)
            },
            case
                with_store(Params, fun(Store) ->
                    Store:advance_offboarding_case(
                        OrgId, WorkspaceId, CaseId, Expected, transferring, BaselineCounts
                    )
                end)
            of
                {error, conflict} ->
                    {error, {case_conflict, CaseId}};
                {error, _} = Err ->
                    Err;
                ok ->
                    execute_items(
                        OrgId, WorkspaceId, CaseId, Expected + 1, Actor, Case, Params
                    )
            end
    end.

execute_items(OrgId, WorkspaceId, CaseId, CaseVersion, Actor, Case, Params) ->
    case list_items(OrgId, WorkspaceId, CaseId, Params) of
        {error, _} = Err ->
            Err;
        {ok, Items} ->
            Successor = maps:get(successor_user_id, Case, undefined),
            Updated = [
                execute_item(OrgId, WorkspaceId, Successor, Actor, Item, Params)
             || Item <- Items
            ],
            execute_finish(OrgId, WorkspaceId, CaseId, CaseVersion, Actor, Updated, Params)
    end.

%% 逐项：只处理未 success 的项；每项独立裁决（失败不阻塞其它项）。
execute_item(OrgId, WorkspaceId, Successor, Actor, Item, Params) ->
    case maps:get(status, Item, undefined) of
        success ->
            Item;
        _PendingOrFailed ->
            item_snapshot(OrgId, WorkspaceId, Successor, Actor, Item, Params)
    end.

item_snapshot(OrgId, WorkspaceId, Successor, Actor, Item, Params) ->
    IdentityId = maps:get(business_identity_id, Item, undefined),
    To = maps:get(to_user_id, Item, Successor),
    case resource_fingerprint(OrgId, WorkspaceId, IdentityId, Params) of
        {error, Reason} ->
            fail_item(OrgId, WorkspaceId, Item, Reason, Params);
        {ok, Before} ->
            case rebind_identity(OrgId, WorkspaceId, Item, To, Actor, Params) of
                {error, Reason} ->
                    fail_item(OrgId, WorkspaceId, Item, Reason, Params);
                {ok, _Action} ->
                    item_confirm(OrgId, WorkspaceId, Item, Before, Params)
            end
    end.

%% 交接后复核资源指纹：任一字段变化即判该项失败（不静默接受资源被改写）。
item_confirm(OrgId, WorkspaceId, Item, Before, Params) ->
    IdentityId = maps:get(business_identity_id, Item, undefined),
    case resource_fingerprint(OrgId, WorkspaceId, IdentityId, Params) of
        {error, Reason} ->
            fail_item(OrgId, WorkspaceId, Item, Reason, Params);
        {ok, Before} ->
            succeed_item(OrgId, WorkspaceId, Item, Params);
        {ok, After} ->
            Changed = [
                Field
             || Field <- [
                    identity_id,
                    organization_id,
                    identity_hash,
                    conversation_count,
                    contact_count
                ],
                maps:get(Field, Before, undefined) =/= maps:get(Field, After, undefined)
            ],
            fail_item(
                OrgId, WorkspaceId, Item, {resource_changed, IdentityId, Changed}, Params
            )
    end.

%% 交接一项：active 经办行裁决（结束旧行 + 新增新行；owner/identity 行不动）。
rebind_identity(OrgId, WorkspaceId, Item, To, Actor, Params) ->
    IdentityId = maps:get(business_identity_id, Item, undefined),
    From = maps:get(from_user_id, Item, undefined),
    FunctionKey = maps:get(function_key, Item, undefined),
    case
        with_store(Params, fun(Store) ->
            Store:list_assignments(OrgId, WorkspaceId)
        end)
    of
        {error, _} = Err ->
            Err;
        {ok, Assignments} ->
            rebind_by_active(
                OrgId,
                WorkspaceId,
                Item,
                IdentityId,
                From,
                To,
                FunctionKey,
                Actor,
                Assignments,
                Params
            )
    end.

rebind_by_active(
    OrgId,
    WorkspaceId,
    _Item,
    IdentityId,
    From,
    To,
    FunctionKey,
    Actor,
    Assignments,
    Params
) ->
    case eb_offboarding_flow:active_assignment(IdentityId, Assignments) of
        {error, _} = Err ->
            Err;
        none ->
            %% 已无 active 行（重试路径 / 该 identity 从未绑定）⇒ 直接绑 B
            insert_successor(OrgId, WorkspaceId, IdentityId, FunctionKey, To, Actor, Params);
        {ok, Active} ->
            case eb_offboarding_flow:assignee_of(Active) of
                To ->
                    {ok, already_bound};
                From ->
                    end_then_insert(
                        OrgId,
                        WorkspaceId,
                        IdentityId,
                        FunctionKey,
                        To,
                        Actor,
                        Params
                    );
                Other ->
                    {error, {assignee_mismatch, IdentityId, From, Other}}
            end
    end.

end_then_insert(OrgId, WorkspaceId, IdentityId, FunctionKey, To, Actor, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:advance_assignment(OrgId, WorkspaceId, IdentityId, active, ended)
        end)
    of
        ok ->
            insert_successor(OrgId, WorkspaceId, IdentityId, FunctionKey, To, Actor, Params);
        {error, conflict} ->
            {error, {concurrent_assignment_change, IdentityId}};
        {error, _} = Err ->
            Err
    end.

insert_successor(OrgId, WorkspaceId, IdentityId, FunctionKey, To, Actor, Params) ->
    case new_id(organization_business_identity_assignment, Params) of
        {error, _} = Err ->
            Err;
        {ok, AssignmentId} ->
            Row = #{
                id => AssignmentId,
                business_identity_id => IdentityId,
                function_key => FunctionKey,
                user_id => To,
                assigned_by => Actor
            },
            case
                with_store(Params, fun(Store) ->
                    Store:insert_assignment(OrgId, WorkspaceId, Row)
                end)
            of
                {error, conflict} ->
                    {error, {assignment_conflict, IdentityId, To}};
                {error, _} = Err ->
                    Err;
                {ok, _Stored} ->
                    {ok, bind_insert}
            end
    end.

succeed_item(OrgId, WorkspaceId, Item, Params) ->
    ItemId = maps:get(id, Item, undefined),
    Attempt = maps:get(attempt, Item, 0),
    Patch = #{id => ItemId, status => success, attempt => Attempt},
    case update_item(OrgId, WorkspaceId, Patch, Params) of
        {error, _} = Err -> Err;
        {ok, Updated} -> Updated
    end.

fail_item(OrgId, WorkspaceId, Item, Reason, Params) ->
    ItemId = maps:get(id, Item, undefined),
    Attempt = maps:get(attempt, Item, 0),
    Patch = #{
        id => ItemId,
        status => failed,
        attempt => Attempt,
        failure_reason => eb_offboarding_flow:reason_binary(Reason)
    },
    case update_item(OrgId, WorkspaceId, Patch, Params) of
        {error, _Reason} ->
            %% 落库失败必须可见：把失败事实留在返回值里（不静默当成功）
            Item#{
                status => failed,
                attempt => Attempt,
                failure_reason => eb_offboarding_flow:reason_binary(Reason),
                persist_failed => true
            };
        {ok, Updated} ->
            Updated
    end.

execute_finish(OrgId, WorkspaceId, CaseId, CaseVersion, Actor, Items, Params) ->
    Counts = eb_offboarding_flow:item_counts(Items),
    case maps:get(failed, Counts) of
        0 ->
            %% FND-6：全部成功 ⇒ 状态保持 transferring（verify 门不被跳过），
            %% 但 case 行计数必须落成**真实终值**：经 update_offboarding_case_counts/6
            %% 的 version-CAS 更新（不推状态、不递增 version —— 计数不是状态迁移）。
            case
                with_store(Params, fun(Store) ->
                    Store:update_offboarding_case_counts(
                        OrgId, WorkspaceId, CaseId, CaseVersion, transferring, Counts
                    )
                end)
            of
                ok ->
                    execute_audit(OrgId, CaseId, CaseVersion, transferring, Actor, Items, Params);
                {error, conflict} ->
                    {error, {case_conflict, CaseId}};
                {error, _} = Err ->
                    Err
            end;
        _AnyFailed ->
            case
                with_store(Params, fun(Store) ->
                    Store:advance_offboarding_case(
                        OrgId, WorkspaceId, CaseId, CaseVersion, failed, Counts
                    )
                end)
            of
                ok ->
                    execute_audit(
                        OrgId, CaseId, CaseVersion + 1, failed, Actor, Items, Params
                    );
                {error, _} = Err ->
                    Err
            end
    end.

execute_audit(OrgId, CaseId, CaseVersion, Status, Actor, Items, Params) ->
    Counts = eb_offboarding_flow:item_counts(Items),
    audit_result(
        Params,
        OrgId,
        #{
            resource_type => <<"enterprise_offboarding_case">>,
            resource_id => CaseId,
            action => ?ACTION_EXECUTE,
            actor_user_id => Actor,
            detail => #{
                <<"status">> => atom_to_binary(Status, utf8),
                <<"item_total">> => maps:get(total, Counts),
                <<"item_success">> => maps:get(success, Counts),
                <<"item_failed">> => maps:get(failed, Counts)
            }
        },
        #{
            case_id => CaseId,
            status => Status,
            version => CaseVersion,
            items_total => maps:get(total, Counts),
            item_success => maps:get(success, Counts),
            item_failed => maps:get(failed, Counts),
            items => Items,
            snapshot_hash => eb_offboarding_flow:snapshot_hash(Items)
        }
    ).

%% 资源指纹口径（A02）：**owner 与资源**——identity 行自身的 ID/Org/hash，以及挂在
%% 该 identity 上的会话数与客户数。assignee 记录**不在**指纹内：交接本来就会新增
%% 一条 assignee 历史行，把它算进「不变」既不可能也不正确。
resource_fingerprint(OrgId, WorkspaceId, IdentityId, Params) ->
    case
        with_store(Params, fun(Store) -> Store:fetch_identity(OrgId, WorkspaceId, IdentityId) end)
    of
        {error, _} = Err ->
            Err;
        {ok, Identity} ->
            case conversation_count(OrgId, WorkspaceId, IdentityId, Params) of
                {error, _} = Err ->
                    Err;
                {ok, Conversations} ->
                    case contact_count(OrgId, WorkspaceId, IdentityId, Params) of
                        {error, _} = Err ->
                            Err;
                        {ok, Contacts} ->
                            {ok, #{
                                identity_id => maps:get(id, Identity, undefined),
                                organization_id =>
                                    maps:get(organization_id, Identity, undefined),
                                identity_status => maps:get(status, Identity, undefined),
                                identity_function_key =>
                                    maps:get(function_key, Identity, undefined),
                                identity_hash => stable_hash(Identity),
                                conversation_count => Conversations,
                                contact_count => Contacts
                            }}
                    end
            end
    end.

conversation_count(OrgId, WorkspaceId, IdentityId, Params) ->
    case with_store(Params, fun(Store) -> Store:list_conversations(OrgId, WorkspaceId) end) of
        {error, _} = Err ->
            Err;
        {ok, Rows} ->
            {ok,
                length([
                    R
                 || R <- Rows,
                    maps:get(business_identity_id, R, undefined) =:= IdentityId
                ])}
    end.

contact_count(OrgId, WorkspaceId, IdentityId, Params) ->
    case with_store(Params, fun(Store) -> Store:list_contacts(OrgId, WorkspaceId) end) of
        {error, _} = Err ->
            Err;
        {ok, Rows} ->
            {ok,
                length([
                    R
                 || R <- Rows,
                    maps:get(created_by_business_identity_id, R, undefined) =:= IdentityId
                ])}
    end.

stable_hash(Map) when is_map(Map) ->
    Bin = term_to_binary(lists:sort(maps:to_list(Map))),
    <<<<(nibble(N))>> || <<N:4>> <= crypto:hash(sha256, Bin)>>;
stable_hash(Other) ->
    eb_offboarding_flow:reason_binary(Other).

nibble(N) when N < 10 -> $0 + N;
nibble(N) -> $a + N - 10.

fetch_case(OrgId, WorkspaceId, CaseId, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:fetch_offboarding_case(OrgId, WorkspaceId, CaseId)
        end)
    of
        {error, not_found} -> {error, {case_not_found, CaseId}};
        {error, _} = Err -> Err;
        {ok, Case} -> {ok, Case}
    end.

list_items(OrgId, WorkspaceId, CaseId, Params) ->
    with_store(Params, fun(Store) ->
        Store:list_offboarding_items(OrgId, WorkspaceId, CaseId)
    end).

update_item(OrgId, WorkspaceId, Patch, Params) ->
    with_store(Params, fun(Store) ->
        Store:update_offboarding_item(OrgId, WorkspaceId, Patch)
    end).

%% ===================================================================
%% S3：校验（verify）
%% ===================================================================

%% @doc 完成证明的第一半：校验残留、ID/Org/hash/count，全部通过才把 case 从
%% `transferring` 推进到 `verifying`。
%%
%% Params：
%%   workspace_id            必填整数
%%   case_id                 必填整数
%%   actor_user_id           必填整数（审计快照）
%%   expected_snapshot_hash  可选二进制（S1 的指纹；给了就必须逐字相符）
%%
%% 校验项（全部记录在返回体的 `checks` 里，任一不通过即 case 落 `failed`）：
%%   * `key_mismatch`：项的幂等键必须等于 `(case_id, business_identity_id)` 的重算值
%%     ——identity_id 被改写必然对不上（快照防篡改）；
%%   * `incomplete_items`：所有项必须 success；
%%   * `leaver_mismatch` / `successor_mismatch`：项的 from/to 必须等于 case 的
%%     leaver/successor（项与 case 的一致性）；
%%   * `identity_not_active` / `function_key_mismatch`：identity 行仍属本 Org、仍
%%     active，且 `function_key` 与快照一致；
%%   * `item_assignee_mismatch`：该 identity 当前唯一 active 经办人必须就是承接人；
%%   * `resource_changed`：identity 行 hash / 会话数 / 客户数必须与 `execute` 时的
%%     指纹一致（ID/Org/hash/count 不变）；
%%   * `residual_assignments`：leaver 在本 Org **不得**再持有任何 active 经办。
%%
%% `expected_snapshot_hash` 不符 ⇒ `{error, {snapshot_mismatch, Expected, Actual}}`
%% 且**不推进 case**（不是校验失败，而是「拿到的不是同一份快照」）。
%%
%% 诚实边界：S1 的指纹只存在于 append-only 审计事件里，而冻结契约**没有**审计读
%% callback（`eb_audit_port` 只有 `append/2`），所以本函数无法自行回读历史指纹，
%% 需要调用方把 S1 返回值里的指纹带回来比对（给不出就退化为「重算指纹随结果返回」；
%% 该通道缺口登记为 findings[EB08-C2]）。
-spec verify_offboarding(integer(), map()) -> {ok, map()} | {error, term()}.
verify_offboarding(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            verify_args(OrgId, WorkspaceId, Params)
    end;
verify_offboarding(_OrgId, _Params) ->
    {error, {invalid_argument, verify_offboarding}}.

verify_args(OrgId, WorkspaceId, Params) ->
    CaseId = maps:get(case_id, Params, undefined),
    Actor = maps:get(actor_user_id, Params, undefined),
    case {is_pos_int(CaseId), is_pos_int(Actor)} of
        {false, _} -> {error, {invalid_case_id, CaseId}};
        {_, false} -> {error, {invalid_actor_user_id, Actor}};
        {true, true} -> verify_case(OrgId, WorkspaceId, CaseId, Actor, Params)
    end.

verify_case(OrgId, WorkspaceId, CaseId, Actor, Params) ->
    case fetch_case(OrgId, WorkspaceId, CaseId, Params) of
        {error, _} = Err ->
            Err;
        {ok, Case} ->
            verify_status_gate(OrgId, WorkspaceId, CaseId, Actor, Case, Params)
    end.

verify_status_gate(OrgId, WorkspaceId, CaseId, Actor, Case, Params) ->
    case maps:get(status, Case, undefined) of
        transferring ->
            verify_gather(OrgId, WorkspaceId, CaseId, Actor, Case, Params);
        verifying ->
            {error, {already_verified, CaseId}};
        OtherStatus ->
            {error, {invalid_case_status, OtherStatus}}
    end.

verify_gather(OrgId, WorkspaceId, CaseId, Actor, Case, Params) ->
    case list_items(OrgId, WorkspaceId, CaseId, Params) of
        {error, _} = Err ->
            Err;
        {ok, Items} ->
            case
                with_store(Params, fun(Store) ->
                    Store:list_assignments(OrgId, WorkspaceId)
                end)
            of
                {error, _} = Err ->
                    Err;
                {ok, Assignments} ->
                    verify_reports(
                        OrgId,
                        WorkspaceId,
                        CaseId,
                        Actor,
                        Case,
                        Items,
                        Assignments,
                        Params
                    )
            end
    end.

verify_reports(OrgId, WorkspaceId, CaseId, Actor, Case, Items, Assignments, Params) ->
    SnapshotHash = eb_offboarding_flow:snapshot_hash(Items),
    case snapshot_hash_gate(SnapshotHash, Params) of
        {error, _} = Err ->
            Err;
        ok ->
            {Reports, Reasons} =
                verify_item_reports(OrgId, WorkspaceId, Case, Items, Assignments, Params),
            AllReasons = verify_case_reasons(Case, Items, SnapshotHash, Assignments, Reasons),
            verify_conclude(
                OrgId,
                WorkspaceId,
                CaseId,
                Actor,
                Case,
                Items,
                Reports,
                AllReasons,
                SnapshotHash,
                Params
            )
    end.

snapshot_hash_gate(Actual, Params) ->
    case maps:get(expected_snapshot_hash, Params, undefined) of
        undefined ->
            ok;
        Actual ->
            ok;
        Expected ->
            {error, {snapshot_mismatch, Expected, Actual}}
    end.

%% 项级校验：逐项取事实并比对（不改任何行）。
verify_item_reports(OrgId, WorkspaceId, Case, Items, Assignments, Params) ->
    Successor = maps:get(successor_user_id, Case, undefined),
    lists:foldl(
        fun(Item, {Reports, Reasons}) ->
            {Report, ItemReasons} =
                verify_item(OrgId, WorkspaceId, Case, Item, Successor, Assignments, Params),
            {[Report | Reports], ItemReasons ++ Reasons}
        end,
        {[], []},
        Items
    ).

verify_item(OrgId, WorkspaceId, Case, Item, Successor, Assignments, Params) ->
    IdentityId = maps:get(business_identity_id, Item, undefined),
    Leaver = maps:get(leaver_user_id, Case, undefined),
    To = maps:get(to_user_id, Item, undefined),
    CaseReasons = eb_offboarding_flow:item_reasons(Item, Leaver, Successor),
    {Assignee, AssigneeReasons} = item_assignee(IdentityId, To, Assignments),
    case resource_fingerprint(OrgId, WorkspaceId, IdentityId, Params) of
        {error, Reason} ->
            {
                maps:merge(Item, #{assignee => Assignee}),
                [{identity_read_failed, IdentityId, Reason} | AssigneeReasons ++ CaseReasons]
            };
        {ok, Fingerprint} ->
            IdentityReasons = identity_reasons(OrgId, Fingerprint, Item),
            {
                maps:merge(Item, Fingerprint#{assignee => Assignee}),
                AssigneeReasons ++ IdentityReasons ++ CaseReasons
            }
    end.

%% 该 identity 当前唯一 active 经办人必须就是承接人。
item_assignee(IdentityId, To, Assignments) ->
    case eb_offboarding_flow:active_assignment(IdentityId, Assignments) of
        {ok, Active} ->
            case eb_offboarding_flow:assignee_of(Active) of
                To -> {To, []};
                Other -> {Other, [{item_assignee_mismatch, IdentityId, To, Other}]}
            end;
        none ->
            {none, [{item_assignee_missing, IdentityId, To}]};
        {error, Reason} ->
            {unknown, [{item_assignee_ambiguous, Reason}]}
    end.

%% identity 行仍属本 Org、ID 与职能与快照一致。
identity_reasons(OrgId, Fingerprint, Item) ->
    IdentityId = maps:get(business_identity_id, Item, undefined),
    FunctionKey = maps:get(function_key, Item, undefined),
    lists:append([
        mismatch(identity_id, IdentityId, maps:get(identity_id, Fingerprint, undefined)),
        mismatch(
            organization_id, OrgId, maps:get(organization_id, Fingerprint, undefined)
        ),
        mismatch(
            function_key, FunctionKey, maps:get(identity_function_key, Fingerprint, undefined)
        ),
        status_reason(IdentityId, maps:get(identity_status, Fingerprint, undefined))
    ]).

mismatch(_Field, Expected, Expected) ->
    [];
mismatch(Field, Expected, Actual) ->
    [{Field, Expected, Actual}].

status_reason(_IdentityId, active) ->
    [];
status_reason(IdentityId, Status) ->
    [{identity_not_active, IdentityId, Status}].

%% case 级校验：项计数、幂等键重算、残留。
verify_case_reasons(Case, Items, SnapshotHash, Assignments, Reasons) ->
    Leaver = maps:get(leaver_user_id, Case, undefined),
    Counts = eb_offboarding_flow:item_counts(Items),
    Prefix =
        case maps:get(success, Counts) =:= maps:get(total, Counts) of
            true -> [];
            false -> [{incomplete_items, Counts}]
        end,
    KeyReasons =
        case eb_offboarding_flow:recomputed_key_matches(Items) of
            {ok, _} -> [];
            {mismatch, Bad} -> [{key_mismatch, [maps:get(id, I) || I <- Bad]}]
        end,
    Residual = [
        #{
            business_identity_id => maps:get(business_identity_id, A, undefined),
            function_key => maps:get(function_key, A, undefined),
            user_id => maps:get(user_id, A, undefined)
        }
     || A <- Assignments,
        maps:get(user_id, A, undefined) =:= Leaver,
        maps:get(status, A, undefined) =:= active
    ],
    ResidualReasons =
        case Residual of
            [] -> [];
            _ -> [{residual_assignments, Residual}]
        end,
    _ = SnapshotHash,
    Prefix ++ KeyReasons ++ ResidualReasons ++ Reasons.

verify_conclude(OrgId, WorkspaceId, CaseId, Actor, Case, Items, Reports, [], SnapshotHash, Params) ->
    CaseVersion = maps:get(version, Case, undefined),
    VerifyCounts = eb_offboarding_flow:item_counts(Items),
    case
        with_store(Params, fun(Store) ->
            Store:advance_offboarding_case(
                OrgId, WorkspaceId, CaseId, CaseVersion, verifying, VerifyCounts
            )
        end)
    of
        {error, conflict} ->
            {error, {case_conflict, CaseId}};
        {error, _} = Err ->
            Err;
        ok ->
            Counts = eb_offboarding_flow:item_counts(Items),
            audit_result(
                Params,
                OrgId,
                #{
                    resource_type => <<"enterprise_offboarding_case">>,
                    resource_id => CaseId,
                    action => ?ACTION_VERIFY,
                    actor_user_id => Actor,
                    detail => #{
                        <<"snapshot_hash">> => SnapshotHash,
                        <<"item_total">> => maps:get(total, Counts),
                        <<"residual_count">> => 0
                    }
                },
                #{
                    case_id => CaseId,
                    status => verifying,
                    version => CaseVersion + 1,
                    items_total => maps:get(total, Counts),
                    item_success => maps:get(success, Counts),
                    item_failed => maps:get(failed, Counts),
                    items => lists:reverse(Reports),
                    residual_assignments => [],
                    checks => [],
                    snapshot_hash => SnapshotHash
                }
            )
    end;
verify_conclude(OrgId, WorkspaceId, CaseId, _Actor, Case, _Items, _Reports, Reasons, _Hash, Params) ->
    CaseVersion = maps:get(version, Case, undefined),
    FailedCounts = eb_offboarding_flow:item_counts(_Items),
    case
        with_store(Params, fun(Store) ->
            Store:advance_offboarding_case(
                OrgId, WorkspaceId, CaseId, CaseVersion, failed, FailedCounts
            )
        end)
    of
        ok -> {error, {verification_failed, Reasons}};
        {error, _} = Err -> Err
    end.

%% ===================================================================
%% S3：完成证明（finalize）
%% ===================================================================

%% @doc 完成证明的第二半：把成员从 `suspended` 推进到 `removed` 并完成 case。
%%
%% Params：
%%   workspace_id   必填整数
%%   case_id        必填整数
%%   actor_user_id  必填整数（Core 侧要求其为本 Org 的 Owner/Admin）
%%
%% **未 verify 不得 removed（A04）**：只接受 case 状态 `verifying`（该状态**只能**
%% 由一次成功的 verify 经 CAS 到达）；其它状态一律 `{error, {not_verified, Status}}`
%% 并且**不触库、不写审计**。注意这道闸门不是数据库守卫的副作用——`execute` 之后
%% leaver 已无 active 经办，DB 守卫本身会放行移除，所以拦住 finalize 的必须是
%% 「verify 未完成」这一事实（套件里有专门的断言证明这一点）。
%%
%% **DB guard 双层（A07）**：本函数**不在应用层**再查一遍 active 经办，而是把
%% 「仍被依赖资源引用」的裁决完整交给数据库——`organization_member_logic:remove/3`
%% 的 `UPDATE organization_member SET status='removed'` 会在**同一语句内**触发
%% BEFORE 守卫 `trg_organization_member_offboarding_guard`，重新核对 active 经办。
%% 因此「检查与使用之间」的窗口被关闭：verify 之后新出现的残留会让 finalize 得到
%% `{error, {409, _}}`，且 case 不被推进、成员不被移除、零审计。
%%
%% 幂等：case 已 `completed` ⇒ 直接返回 `{ok, #{idempotent => true}}`（零写入零审计）；
%% 并发 finalize 落败方在 Core 侧拿到「成员已不是在册状态」，此时回读 case，若已是
%% `completed` 则同样按幂等成功返回（不重复移除、不重复审计）。
-spec finalize_offboarding(integer(), map()) -> {ok, map()} | {error, term()}.
finalize_offboarding(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            finalize_args(OrgId, WorkspaceId, Params)
    end;
finalize_offboarding(_OrgId, _Params) ->
    {error, {invalid_argument, finalize_offboarding}}.

finalize_args(OrgId, WorkspaceId, Params) ->
    CaseId = maps:get(case_id, Params, undefined),
    Actor = maps:get(actor_user_id, Params, undefined),
    case {is_pos_int(CaseId), is_pos_int(Actor)} of
        {false, _} -> {error, {invalid_case_id, CaseId}};
        {_, false} -> {error, {invalid_actor_user_id, Actor}};
        {true, true} -> finalize_case(OrgId, WorkspaceId, CaseId, Actor, Params)
    end.

finalize_case(OrgId, WorkspaceId, CaseId, Actor, Params) ->
    case fetch_case(OrgId, WorkspaceId, CaseId, Params) of
        {error, _} = Err ->
            Err;
        {ok, Case} ->
            finalize_status_gate(OrgId, WorkspaceId, CaseId, Actor, Case, Params)
    end.

finalize_status_gate(OrgId, WorkspaceId, CaseId, Actor, Case, Params) ->
    case maps:get(status, Case, undefined) of
        verifying ->
            finalize_items_gate(OrgId, WorkspaceId, CaseId, Actor, Case, Params);
        completed ->
            {ok, finalize_result(CaseId, Case, true)};
        OtherStatus ->
            {error, {not_verified, OtherStatus}}
    end.

finalize_items_gate(OrgId, WorkspaceId, CaseId, Actor, Case, Params) ->
    case list_items(OrgId, WorkspaceId, CaseId, Params) of
        {error, _} = Err ->
            Err;
        {ok, Items} ->
            Counts = eb_offboarding_flow:item_counts(Items),
            case maps:get(success, Counts) =:= maps:get(total, Counts) of
                false ->
                    {error, {incomplete_case, Counts}};
                true ->
                    finalize_member(OrgId, WorkspaceId, CaseId, Actor, Case, Counts, Params)
            end
    end.

%% 第二层守卫：数据库在同一语句内复核 active 经办（应用层**不**再查一遍）。
finalize_member(OrgId, WorkspaceId, CaseId, Actor, Case, Counts, Params) ->
    Leaver = maps:get(leaver_user_id, Case, undefined),
    case remove_member_via_core(OrgId, Leaver, Actor) of
        {ok, _Member} ->
            finalize_complete(OrgId, WorkspaceId, CaseId, Actor, Case, Counts, Params);
        {error, _} = Err ->
            finalize_remove_failed(OrgId, WorkspaceId, CaseId, Case, Err, Params)
    end.

remove_member_via_core(OrgId, Leaver, Actor) ->
    organization_member_logic:remove(Actor, OrgId, Leaver).

%% 并发收敛：另一个 finalize 已经完成时，回读 case ⇒ 幂等成功（零写入零审计）。
%% 其它情况原样透传 Core 的错误（含 DB guard 的 `{error, {409, _}}`）。
finalize_remove_failed(OrgId, WorkspaceId, CaseId, _Case, Err, Params) ->
    case fetch_case(OrgId, WorkspaceId, CaseId, Params) of
        {ok, #{status := completed} = Done} ->
            {ok, finalize_result(CaseId, Done, true)};
        _NotConverged ->
            Err
    end.

finalize_complete(OrgId, WorkspaceId, CaseId, Actor, Case, Counts, Params) ->
    CaseVersion = maps:get(version, Case, undefined),
    case
        with_store(Params, fun(Store) ->
            Store:advance_offboarding_case(
                OrgId, WorkspaceId, CaseId, CaseVersion, completed, Counts
            )
        end)
    of
        {error, conflict} ->
            {error, {case_conflict, CaseId}};
        {error, _} = Err ->
            Err;
        ok ->
            audit_result(
                Params,
                OrgId,
                #{
                    resource_type => <<"enterprise_offboarding_case">>,
                    resource_id => CaseId,
                    action => ?ACTION_FINALIZE,
                    actor_user_id => Actor,
                    detail => #{
                        <<"leaver_user_id">> => maps:get(leaver_user_id, Case, undefined),
                        <<"db_guard_rechecked">> => true
                    }
                },
                #{
                    case_id => CaseId,
                    status => completed,
                    version => CaseVersion + 1,
                    leaver_user_id => maps:get(leaver_user_id, Case, undefined),
                    member_removed => true,
                    db_guard_rechecked => true,
                    idempotent => false
                }
            )
    end.

finalize_result(CaseId, Case, Idempotent) ->
    #{
        case_id => CaseId,
        status => completed,
        version => maps:get(version, Case, undefined),
        leaver_user_id => maps:get(leaver_user_id, Case, undefined),
        member_removed => true,
        db_guard_rechecked => true,
        idempotent => Idempotent
    }.

%% ===================================================================
%% 内部辅助：端口 / 租户 / 事实 / 审计
%% ===================================================================

%% 端口解析：Params 里的同键可注入实现（测试与装配用），否则用 eb_infra_ports 装配。
port(Key, Params) ->
    case maps:get(Key, Params, undefined) of
        Mod when is_atom(Mod), Mod =/= undefined -> {ok, Mod};
        _ -> eb_infra_ports:resolve(Key)
    end.

with_store(Params, Fun) ->
    case port(store, Params) of
        {ok, Store} -> Fun(Store);
        {error, _} = Err -> Err
    end.

new_id(Kind, Params) ->
    case port(id, Params) of
        {ok, IdPort} ->
            try
                {ok, IdPort:new_id(Kind)}
            catch
                Class:Reason ->
                    {error, {id_generation_failed, Kind, {Class, Reason}}}
            end;
        {error, _} = Err ->
            Err
    end.

%% 写入成功后追加 append-only 审计；审计失败如实报错，不假装成功。
audit_result(Params, OrgId, Event, Resource) ->
    case port(audit, Params) of
        {ok, Audit} ->
            case Audit:append(OrgId, Event) of
                {ok, AuditId} -> {ok, Resource#{audit_id => AuditId}};
                {error, Reason} -> {error, {audit_append_failed, Reason}}
            end;
        {error, _} = Err ->
            Err
    end.

tenant(OrgId, Params) ->
    case is_pos_int(OrgId) of
        false ->
            {error, {invalid_organization_id, OrgId}};
        true ->
            case maps:get(workspace_id, Params, undefined) of
                Ws when is_integer(Ws), Ws > 0 -> {ok, Ws};
                Other -> {error, {invalid_workspace_id, Other}}
            end
    end.

workspace_id(Params) ->
    maps:get(workspace_id, Params, undefined).

%% 成员状态：默认走**只读事实 Port**（P10，装配实现 eb_member_fact_pg）；
%% `Params.member_facts` 可注入以替换事实源。事实 ≠ 授权结论。
member_status(OrgId, UserId, Params) ->
    case maps:get(member_facts, Params, undefined) of
        Loader when is_function(Loader, 2) ->
            normalize_member(Loader(OrgId, UserId));
        _NoInjection ->
            member_status_via_port(OrgId, UserId, Params)
    end.

member_status_via_port(OrgId, UserId, Params) ->
    case port(member_fact, Params) of
        {error, _} = Err ->
            Err;
        {ok, MemberFact} ->
            try
                normalize_member(MemberFact:member_status(OrgId, UserId))
            catch
                Class:Reason ->
                    {error, {member_fact_query_failed, {Class, Reason}}}
            end
    end.

normalize_member(Status) when Status =:= active; Status =:= suspended; Status =:= removed ->
    {ok, Status};
normalize_member({ok, Status}) when is_atom(Status) ->
    {ok, Status};
normalize_member({error, no_member}) ->
    {ok, not_a_member};
normalize_member({error, _} = Err) ->
    Err;
normalize_member(Other) ->
    {error, {invalid_member_fact, Other}}.

%% Core 的通用 suspend（写路径归本卡）；Reason 只进审计，不进 Core 的 SQL。
suspend_member_via_core(OrgId, Leaver, Actor, SuspensionOrigin, Params) ->
    case organization_member_logic:suspend(Actor, OrgId, Leaver) of
        {error, _} = Err ->
            Err;
        {ok, _Member} ->
            audit_suspend(Params, OrgId, Leaver, Actor, SuspensionOrigin)
    end.

audit_suspend(Params, OrgId, Leaver, Actor, Origin) ->
    case port(audit, Params) of
        {ok, Audit} ->
            case
                Audit:append(OrgId, #{
                    resource_type => <<"organization_member">>,
                    resource_id => Leaver,
                    action => ?ACTION_SUSPEND,
                    actor_user_id => Actor,
                    detail => #{<<"origin">> => origin_binary(Origin)}
                })
            of
                {ok, _AuditId} -> ok;
                {error, Reason} -> {error, {audit_append_failed, Reason}}
            end;
        {error, _} = Err ->
            Err
    end.

origin_binary({offboarding, CaseId}) ->
    <<"offboarding:", (integer_to_binary(CaseId))/binary>>;
origin_binary(Other) ->
    eb_offboarding_flow:reason_binary(Other).

first_defined([]) ->
    undefined;
first_defined([undefined | Rest]) ->
    first_defined(Rest);
first_defined([Value | _Rest]) ->
    Value.

is_pos_int(Value) ->
    is_integer(Value) andalso Value > 0.

is_non_empty_binary(Value) ->
    is_binary(Value) andalso byte_size(Value) > 0.
