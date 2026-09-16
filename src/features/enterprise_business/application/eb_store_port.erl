%%% @doc 扩展点：企业业务持久化读写（EB-02 冻结契约；实现随 EB-03 落地）。
%%%
%%% 这是**真扩展点**：只有 `-callback` 声明，零实现、零 meck。
%%%
%%% 铁律 6（租户作用域必须显式贯穿）：每个资源级 callback 的前两个业务参数
%%% 必须是 `organization_id` 与 `workspace_id`；实现侧的 SQL 必须同语句带这两
%%% 个约束。`list_assignments/2` 是 workspace 级列举，故只带 Org/Workspace。
%%%
%%% 铁律 7（并发状态迁移）：`advance_assignment/5` 是 CAS 形状——
%%% `(OrgId, WorkspaceId, Id, ExpectedStatus, NextStatus)`，实现必须仅当影响
%%% 行数为 1 时返回 `ok`，否则返回 `{error, conflict}`。
-module(eb_store_port).

-export_type([
    identity/0,
    conversation/0,
    message/0,
    assignment/0,
    status/0
]).

-type identity() :: map().
-type conversation() :: map().
-type message() :: map().
-type assignment() :: map().
-type status() :: atom().

%% identity
-callback fetch_identity(OrgId :: integer(), WorkspaceId :: integer(), IdentityId :: integer()) ->
    {ok, identity()} | {error, not_found | term()}.
-callback insert_identity(OrgId :: integer(), WorkspaceId :: integer(), Identity :: identity()) ->
    {ok, identity()} | {error, conflict | term()}.

%% conversation
-callback fetch_conversation(
    OrgId :: integer(), WorkspaceId :: integer(), ConversationId :: integer()
) ->
    {ok, conversation()} | {error, not_found | term()}.
-callback insert_conversation(
    OrgId :: integer(), WorkspaceId :: integer(), Conversation :: conversation()
) ->
    {ok, conversation()} | {error, conflict | term()}.

%% message（canonical 真源：只追加，不由 ACK 改写/删除）
-callback append_message(OrgId :: integer(), WorkspaceId :: integer(), Message :: message()) ->
    {ok, message()} | {error, conflict | term()}.

%% assignment 的 CAS 推进（铁律 7）
-callback advance_assignment(
    OrgId :: integer(),
    WorkspaceId :: integer(),
    IdentityId :: integer(),
    ExpectedStatus :: status(),
    NextStatus :: status()
) -> ok | {error, conflict | term()}.

%% workspace 级列举
-callback list_assignments(OrgId :: integer(), WorkspaceId :: integer()) ->
    {ok, [assignment()]} | {error, term()}.

%% ===================================================================
%% EB-03R 契约面补齐（**只追加**：以上 7 个 callback 一字未动）
%% ===================================================================
%%
%% 为什么必须补（R0-1 的机械证据）：Erlang 的 `-behaviour` 只在**编译期**检查
%% 实现方是否齐全，**不检查调用方是否越界**。application 层通过
%% `eb_infra_ports:resolve(store)` 拿到持久化实现模块名后，就能调用
%% 它的任意导出函数——于是 12 个真实依赖的 callback 从未出现在冻结契约里，
%% 契约门对所有能力缺口**结构性失明**（B1/B3/C1/C2 必在冻结后才暴露）。
%%
%% 本段把「真实依赖面」并入契约面，判定方式交由
%% `scripts/check_eb_port_closure.sh`（调用点 ⊆ 声明，逐条点名缺失项）。
%%
%% 铁律 6 在本段不变：每个 callback 的前两个业务参数仍是
%% `organization_id` / `workspace_id`；返回形态与既有 callback 同规
%%（原子键 map、timestamptz → Unix 秒、NULL → `undefined`）。

-type contact() :: map().
-type contact_identity() :: map().
-type note() :: map().
-type contact_assignment() :: map().
-type policy() :: map().
-type hold() :: map().
-type delivery() :: map().
-type offboarding_case() :: map().
-type offboarding_item() :: map().
-type page_query() :: map().

-export_type([
    contact/0,
    contact_identity/0,
    note/0,
    contact_assignment/0,
    policy/0,
    hold/0,
    delivery/0,
    offboarding_case/0,
    offboarding_item/0,
    page_query/0
]).

%% -- contact / contact_identity（R0-1：调用点已在用，契约未声明）-------------

%% @doc 客户建档（Org 域；workspace 只做归属校验，见实现的同语句 scope 子查询）。
-callback insert_contact(OrgId :: integer(), WorkspaceId :: integer(), Contact :: contact()) ->
    {ok, contact()} | {error, conflict | term()}.

%% @doc 客户详情：跨 Org / 不存在一律 `{error, not_found}`（不区分，避免枚举）。
-callback fetch_contact(OrgId :: integer(), WorkspaceId :: integer(), ContactId :: integer()) ->
    {ok, contact()} | {error, not_found | term()}.

%% @doc 渠道标识落库（组织域 HMAC + 掩码；绝不落明文 subject）。
-callback insert_contact_identity(
    OrgId :: integer(), WorkspaceId :: integer(), Subject :: contact_identity()
) ->
    {ok, contact_identity()} | {error, conflict | term()}.

%% @doc 客户跟进备注（正文为密文；不含明文）。
-callback insert_note(OrgId :: integer(), WorkspaceId :: integer(), Note :: note()) ->
    {ok, note()} | {error, conflict | term()}.

%% @doc 客户 ↔ 业务身份经办关系（§4.1 contact_assignment）。
-callback insert_contact_assignment(
    OrgId :: integer(), WorkspaceId :: integer(), Assignment :: contact_assignment()
) ->
    {ok, contact_assignment()} | {error, conflict | term()}.

%% @doc 客户列表（§5.1 GET contacts）：按 Org + Workspace 归属过滤，键集有序。
-callback list_contacts(OrgId :: integer(), WorkspaceId :: integer()) ->
    {ok, [contact()]} | {error, term()}.

%% @doc 客户资料更新（§5.1 PATCH contacts/:id）。`Patch` 只允许白名单字段；
%% 不存在的行返回 `{error, not_found}`，不可变字段（id/Org）不得出现在结果里。
-callback update_contact(OrgId :: integer(), WorkspaceId :: integer(), Patch :: map()) ->
    {ok, contact()} | {error, not_found | conflict | term()}.

%% -- message / delivery ----------------------------------------------------

%% @doc 消息详情（canonical 只读；含 policy/retain_until 快照）。
-callback fetch_message(OrgId :: integer(), WorkspaceId :: integer(), MessageId :: integer()) ->
    {ok, message()} | {error, not_found | term()}.

%% @doc 只读历史分页（EB-06「只读历史」）：**键集**分页，不是 offset。
%% `Query` 必含 `conversation_id`，可选 `after_id`（严格 `id > AfterId`）与 `limit`。
%% 键集语义下删除/新增不产生漂移，故不得退化为 `OFFSET`。
-callback list_messages_after(OrgId :: integer(), WorkspaceId :: integer(), Query :: page_query()) ->
    {ok, [message()]} | {error, term()}.

%% @doc 会话列表（§5.1 GET conversations）。
-callback list_conversations(OrgId :: integer(), WorkspaceId :: integer()) ->
    {ok, [conversation()]} | {error, term()}.

%% @doc 会话经办交接（EB-06-A04 / EB-D05）：只改当前 identity，不动历史 message。
%% CAS 形状：`IdentityId` 为目标业务身份；冲突返回 `{error, conflict}`。
-callback update_conversation_assignee(
    OrgId :: integer(),
    WorkspaceId :: integer(),
    ConversationId :: integer(),
    IdentityId :: integer()
) ->
    {ok, conversation()} | {error, conflict | not_found | term()}.

%% @doc 投递回执（ACK 只写 delivery 表，绝不改写 canonical message）。
-callback ack_delivery(OrgId :: integer(), WorkspaceId :: integer(), Ack :: delivery()) ->
    {ok, delivery()} | {error, not_found | term()}.

%% -- assignment（首次绑定）--------------------------------------------------

%% @doc 首次绑定 active 经办（CAS `advance_assignment/5` 只能改既有行；
%% 首次绑定必须走本 callback 新建行，且同一 identity 不得有两个 active）。
-callback insert_assignment(
    OrgId :: integer(), WorkspaceId :: integer(), Assignment :: assignment()
) ->
    {ok, assignment()} | {error, conflict | term()}.

%% @doc 业务身份列举（Org 域；§二「identity list」）。
-callback list_identities(OrgId :: integer(), WorkspaceId :: integer()) ->
    {ok, [identity()]} | {error, term()}.

%% @doc 业务身份列举的**键集分页 + active assignment 投影**（C5）。
%%
%% `Query`：
%%   after_id  可选；倒序键集游标（严格 `id < AfterId`；缺省 = 首页）
%%   limit     可选（缺省 50，1..200；越界 `{error, {invalid_limit, _}}`）
%%
%% 每行附 `active_assignment`：C5 白名单七键对象（无 active 经办时 `undefined`，
%% HTTP 面由 application 层投影为 JSON null）。键集语义下删除/新增不产生漂移，
%% 故不得退化为 `OFFSET`。`list_identities/2` 是本 callback 的默认页兼容形状。
-callback list_identities_page(OrgId :: integer(), WorkspaceId :: integer(), Query :: page_query()) ->
    {ok, [identity()]} | {error, term()}.

%% -- retention policy / hold ----------------------------------------------

%% @doc 保留策略新版本（不可变快照；只增不减由 DB 守卫裁决）。
-callback insert_policy(OrgId :: integer(), WorkspaceId :: integer(), Policy :: policy()) ->
    {ok, policy()} | {error, conflict | term()}.

%% @doc 当前生效的保留策略版本（按 data_class）。
-callback latest_policy(OrgId :: integer(), WorkspaceId :: integer(), DataClass :: binary()) ->
    {ok, policy()} | {error, not_found | term()}.

%% @doc 新建保留 hold（append-only hold 事实）。
-callback insert_hold(OrgId :: integer(), WorkspaceId :: integer(), Hold :: hold()) ->
    {ok, hold()} | {error, conflict | term()}.

%% @doc hold 详情（含 released_at；released 行保留原 scope id 以便审计追踪）。
-callback fetch_hold(OrgId :: integer(), WorkspaceId :: integer(), HoldId :: integer()) ->
    {ok, hold()} | {error, not_found | term()}.

%% @doc 释放 hold：一次性写入 released_at + released_by_user_id（append-only）。
-callback release_hold(
    OrgId :: integer(),
    WorkspaceId :: integer(),
    HoldId :: integer(),
    ReleasedBy :: integer() | undefined
) ->
    ok | {error, not_found | term()}.

%% @doc 当前 active（released_at IS NULL）hold 列表。
-callback list_active_holds(OrgId :: integer(), WorkspaceId :: integer()) ->
    {ok, [hold()]} | {error, term()}.

%% -- offboarding（EB-08 的持久化能力预先补齐，见 R0 分类三）------------------

%% @doc 离职交接 case 建档（同 Org + leaver 同时最多一个未完成 case，由 DB 唯一索引裁决）。
-callback insert_offboarding_case(
    OrgId :: integer(), WorkspaceId :: integer(), Case :: offboarding_case()
) ->
    {ok, offboarding_case()} | {error, conflict | term()}.

%% @doc case 详情。
-callback fetch_offboarding_case(OrgId :: integer(), WorkspaceId :: integer(), CaseId :: integer()) ->
    {ok, offboarding_case()} | {error, not_found | term()}.

%% @doc case 列表（含未完成优先的顺序契约见实现）。
-callback list_offboarding_cases(OrgId :: integer(), WorkspaceId :: integer()) ->
    {ok, [offboarding_case()]} | {error, term()}.

%% @doc 交接项建档（幂等键唯一；重放不增行）。
-callback insert_offboarding_item(
    OrgId :: integer(), WorkspaceId :: integer(), Item :: offboarding_item()
) ->
    {ok, offboarding_item()} | {error, conflict | term()}.

%% @doc case 下的交接项列表。
-callback list_offboarding_items(OrgId :: integer(), WorkspaceId :: integer(), CaseId :: integer()) ->
    {ok, [offboarding_item()]} | {error, term()}.

%% @doc 交接项状态推进（EB-08-A04 的 failed → pending 重试：`attempt` 递增、
%% 幂等键逐字不变、失败原因保留）。
-callback update_offboarding_item(
    OrgId :: integer(), WorkspaceId :: integer(), Item :: offboarding_item()
) ->
    {ok, offboarding_item()} | {error, not_found | conflict | term()}.

%% @doc case 状态推进（CAS：`ExpectedVersion` 必须等于当前 `version`，
%% 否则 `{error, conflict}`；合法迁移由 domain `eb_offboarding:transition/2` 裁决）。
-callback advance_offboarding_case(
    OrgId :: integer(),
    WorkspaceId :: integer(),
    CaseId :: integer(),
    ExpectedVersion :: integer(),
    NextStatus :: atom(),
    Counts :: map()
) ->
    ok | {error, conflict | term()}.

%% FND-6：计数专用 CAS —— 不推状态、不递增 version，只把 case 行计数落成
%% 真实 item 终值（用于「状态不变但 items 已定」的时点）。
-callback update_offboarding_case_counts(
    OrgId :: integer(),
    WorkspaceId :: integer(),
    CaseId :: integer(),
    ExpectedVersion :: integer(),
    ExpectedStatus :: atom(),
    Counts :: map()
) ->
    ok | {error, conflict | term()}.
