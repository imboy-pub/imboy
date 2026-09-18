# IMBoy Enterprise Organization V1 Core Contract

> STATUS: `FROZEN_CORE_CONTRACT`
>
> IMPLEMENTATION_STATE: `NOT_STARTED`
>
> DB_RUNTIME_EVIDENCE: `ENVIRONMENT_BLOCKED`
>
> AUTHORITY: 本文是 Organization V1 的规范性合同。Current Model 证据与完整决策见
> `2026-09-16-enterprise-organization-v1.md`。冻结不等于已实现或已通过运行时验收。

## 1. Contract Vocabulary

`MUST` 是实施和集成门禁；`MUST NOT` 是禁止行为；`SOURCE OF TRUTH` 只能有一个；
`TRANSITION` 描述兼容迁移，而非当前已完成事实。

## C01 Identity Contract

- **MUST**：一等 Account identity 复用 `user`；长期只区分 `Human(0)` 与 `Agent(1)`。
- **MUST NOT**：为 Employee、Seat、Customer、Learner、System Bot 或 Developer Bot 创建平行 User 体系。
- **SOURCE OF TRUTH**：`user`；Agent 产品属性由未来通用 Agent registry/binding 承载。
- **INVARIANTS**：Human 与 Agent ID 全局唯一；origin/runtime/transport/binding/trigger 不编码进 account_type。
- **TRANSITION**：当前 2/3 保留，按 C18/M06 expand-first 迁移。
- **ACCEPTANCE**：任一 actor 可解析为一个 User identity；非身份实体不能登录或独立持 JWT。

## C02 Organization Contract

- **MUST**：Organization 是企业租户与企业资源 owner，支持 `active|archived`。
- **MUST NOT**：把 Workspace、Department 或 Business Identity 当成 Organization 替代物。
- **SOURCE OF TRUTH**：`organization`。
- **INVARIANTS**：一个 Organization 可拥有多个 Workspace、Department、Business Identity；跨 Org FK/访问 fail closed。
- **TRANSITION**：保留现有 id/name/status/settings/branding 与 API 兼容。
- **ACCEPTANCE**：所有 Organization-owned 资源均可通过不可伪造 FK/查询路径回溯唯一 Organization。

## C03 Membership Contract

- **MUST**：Human/Agent 的组织归属共用 `organization_member`；状态为 `active|suspended|removed`。
- **MUST NOT**：新增 Employee membership、CS membership 或 Agent organization membership 平行表。
- **SOURCE OF TRUTH**：`organization_member`。
- **INVARIANTS**：`(organization_id,user_id)` 唯一；仅 active 产生当前治理资格；Agent 最多 role=member。
- **TRANSITION**：现有 immediate-add API 逐步由 Invitation/Restore command 替代。
- **ACCEPTANCE**：同一 Account 可属于多个 Org；每个 Org 的状态彼此独立；suspend 即时撤销 Org 权限。

## C04 Owner Contract

- **MUST**：每个 Organization 恰好一个 active Human owner；转移在单事务和 Org 行锁内完成。
- **MUST NOT**：通过普通 role update、Agent membership 或直接写 `owner_id` 绕过 transfer command。
- **SOURCE OF TRUTH**：active `organization_member(role=owner)`。
- **INVARIANTS**：`organization.owner_id` 必须与 owner membership 一致，但只是兼容投影。
- **TRANSITION**：现有 Org->member trigger 仅过渡；改为原子 command + deferred invariant；Owner FK 改 RESTRICT。
- **ACCEPTANCE**：并发 transfer 后恰好一个 owner；owner 删除在未 transfer 时稳定拒绝。

## C05 Workspace Contract

- **MUST**：支持个人 Workspace 与 Organization-owned Workspace；Org 1:N Workspace；membership 独立。
- **MUST NOT**：由 Organization membership 自动创建 Workspace membership，反向亦然。
- **SOURCE OF TRUTH**：ownership 为 `workspace.organization_id`；访问为 `workspace_member`。
- **INVARIANTS**：Organization Role 不等于 Workspace Role；Org owner 不自动成为每个 Workspace owner。
- **TRANSITION**：不把历史 1:N 数据压回 1:1；新增显式 default relation。
- **ACCEPTANCE**：组织外协作者可仅有 Workspace membership；active Org member无 Workspace membership 时访问被拒。

## C06 Business Identity Contract

- **MUST**：Business Identity 是 Organization-owned stable business principal，function_key 不可变。
- **MUST NOT**：解释为 Employee、Account、Membership、Role 或 Workspace member。
- **SOURCE OF TRUTH**：`organization_business_identity`。
- **INVARIANTS**：Identity 不登录；handover 不改变 identity id 或资源 owner；生命周期 active/retired。
- **TRANSITION**：现有表/API 原样保留。
- **ACCEPTANCE**：更换经办 Account 后 Seat、conversation、contact 等仍引用同一 Identity。

## C07 Assignment Contract

- **MUST**：Assignment 表达 Account 对 Business Identity 的时态经办绑定并保留历史。
- **MUST NOT**：当作 Org membership、Department membership、Seat owner 或治理 Role。
- **SOURCE OF TRUTH**：`organization_business_identity_assignment`。
- **INVARIANTS**：同 identity 最多一条 active；assignment 与 identity 同 Org/function；active 必须有 assignee。
- **TRANSITION**：Human 当前可用；Agent 仅在 C13/C14 集成 Gate 后开放。
- **ACCEPTANCE**：handover 结束旧行、创建新行，不重写企业资源历史。

## C08 Customer Service Compatibility Contract

- **MUST**：Seat 保持 Business Identity 的运营 Profile；Session 保持显式 Org+Workspace scope。
- **MUST NOT**：把 Seat 改为 Employee、Account、Membership、Role，或从默认 Workspace 动态推导 Session scope。
- **SOURCE OF TRUTH**：Seat=`customer_service_seat`；Session=`customer_service_session`；operator=active Assignment。
- **INVARIANTS**：suspend 撤销组织授权但不删除 Seat/Session；remove 前 offboarding guard 生效。
- **TRANSITION**：Organization V1 通过 adapter 消费现有 CS/Enterprise Business 合同，不要求大规模重构。
- **ACCEPTANCE**：CS compatibility matrix 全满足；runtime DB 项在隔离 PG Gate 后才能记 PASS。

## C09 Employee Contract

- **MUST**：V1 中 Employee 仅指 active Human Organization Member 的产品视图。
- **MUST NOT**：新增 employee 表、employee_id 或复制 User/Membership 生命周期。
- **SOURCE OF TRUTH**：`organization_member` + `user.account_type=0`。
- **INVARIANTS**：Employee 称呼不增加权限；Agent member 不称 Employee。
- **TRANSITION**：若出现已验证 HR 属性，只能另立 ADR 后新增 1:1 membership profile。
- **ACCEPTANCE**：所有 Employee API/view 均返回原 membership key，无第二身份主键。

## C10 Department Contract

- **MUST**：Department 属于一个 Org，支持树形 parent 和 member 多部门归属；Department admin 为局部目录角色。
- **MUST NOT**：复用 `class_staff`、learner、Seat、Workspace 或 Business Identity 构造 Department。
- **SOURCE OF TRUTH**：未来 `organization_department` 与 `organization_department_member`。
- **INVARIANTS**：member 必须是同 Org membership；禁止环；archive 不级联撤销 Org/Workspace 权限。
- **TRANSITION**：纯 expand，禁止猜测性 backfill。
- **ACCEPTANCE**：跨 Org/成环写入拒绝；兼职和 move 可审计；Department admin 不获得资源访问。

## C11 Invitation Contract

- **MUST**：Invite、Membership、Restore、Accept、Reject、Expire、Revoke 是不同 command/state。
- **MUST NOT**：把 pending 放进 Membership 或继续把 immediate upsert 对新客户端称为 invitation。
- **SOURCE OF TRUTH**：未来 `organization_invitation`。
- **INVARIANTS**：V1 target_user 必填；token/code 只存 digest；同 target/Org 最多一个未终结邀请；accept 幂等。
- **TRANSITION**：现有 `POST /members` 为 legacy direct-add adapter，观测归零后移除。
- **ACCEPTANCE**：过期/撤销/非目标用户不能 accept；重复 accept 不产生第二 Membership 或重复审计副作用。

## C12 Agent Membership Contract

- **MUST**：Agent 可成为多个 Org 的 member，并共用 `organization_member`。
- **MUST NOT**：Agent 成为 Organization owner/admin，或仅凭 membership 获得 Tool capability。
- **SOURCE OF TRUTH**：身份=`user(account_type=1)`；归属=`organization_member`。
- **INVARIANTS**：Agent role 固定 member；Human 与 Agent membership 使用相同状态语义。
- **TRANSITION**：在通用 Agent registry/Grant ready 前，Agent membership 产品入口默认关闭。
- **ACCEPTANCE**：Agent suspend/remove 立即阻止新 AgentRun 和后续 Tool 调用。

## C13 Agent Scope and Grant Boundary

- **MUST**：Agent 执行权限为 active Org membership、active Workspace membership、active Grant、Resource Policy 与 AgentRun context 的交集。
- **MUST NOT**：Profile、Prompt、LLM、Tool 参数、Runtime、Org Role 或 Department Role 扩大 Grant。
- **SOURCE OF TRUTH**：Grant/Delegation 归 Agent Domain；Organization 只提供 membership/governance facts。
- **INVARIANTS**：每次 Tool 调用重检；Grant 绑定 Agent、Org、可选 Workspace、capability、delegator、生命周期。
- **TRANSITION**：本阶段只冻结输入合同，Agent V3.1 设计具体 schema/API/runtime adapter。
- **ACCEPTANCE**：缺任一事实 fail closed；跨 Org/Workspace 参数替换失败并审计。

## C14 Delegation and AgentRun Context Contract

- **MUST**：Delegation 记录谁授权哪个 Agent 做什么；AgentRun 创建时解析并持久化 immutable Org/Workspace context。
- **MUST NOT**：把 Agent 当 User 的同义替身，或在运行中随默认 Workspace 漂移。
- **SOURCE OF TRUTH**：delegation/grant 与 AgentRun 归 Agent Domain；membership/workspace facts 由 Core 提供。
- **INVARIANTS**：principal 与 actor 分开审计；delegator 只能授权其当前可授权范围；撤权即时生效。
- **TRANSITION**：Agent 经办 Business Identity/Seat 默认关闭，直至专用 auth adapter 与集成测试通过。
- **ACCEPTANCE**：Run 和每个 Effect 均带相同 Org/Workspace/Agent/Grant identifiers；越权 fail closed。

## C15 Permission Layering Contract

- **MUST**：分离 Organization Governance、Workspace Authorization、Business Function、Operational Assignment、Enterprise Permission、CS Gate、Agent Profile、Agent Grant、Resource Policy。
- **MUST NOT**：创建万能 Role，或从任一上层自动推导全部下层权限。
- **SOURCE OF TRUTH**：各层自己的表/策略模块；组合器取交集。
- **INVARIANTS**：认证不等于授权；分类不等于权限；membership active 是必要但通常不充分条件。
- **TRANSITION**：复用现有 `eb_auth_permission`、`cs_auth` 等边界，通过 adapter 消费 Core facts。
- **ACCEPTANCE**：每层正反例独立测试；移除任一必要事实即拒绝。

## C16 Lifecycle Contract

- **MUST**：Organization 支持 create、active、archive、restore；普通 V1 API 不提供物理 delete。
- **MUST NOT**：archive 自动恢复/撤销 member、Workspace、Assignment、Seat 或 Grant 的独立状态。
- **SOURCE OF TRUTH**：`organization.status`。
- **INVARIANTS**：archived 禁新写、新 Session、新 AgentRun；允许授权只读和 restore。
- **TRANSITION**：新增显式 archive/restore command 与审计，不改变现有 status 枚举。
- **ACCEPTANCE**：archive/restore 幂等；归档后所有指定写入口一致拒绝。

## C17 Deletion and Offboarding Contract

- **MUST**：User deletion 先执行显式 preflight；owner/Workspace owner/active Assignment/active membership/Agent ownership 全部闭合后才删除。
- **MUST NOT**：依赖 `ON DELETE CASCADE` 决定 Organization 业务结果，或绕过 offboarding guard。
- **SOURCE OF TRUTH**：deletion orchestrator 的 blocker 集合 + 各域实时事实；DB RESTRICT/guard 最终裁决。
- **INVARIANTS**：Owner 未 transfer 必拒；active Assignment 未 handover/end 必拒；Seat/Session 不随 User 删除。
- **TRANSITION**：owner FK 改 RESTRICT；user deletion executor 接入 Org/Enterprise/Agent preflight。
- **ACCEPTANCE**：所有 blocker 返回稳定 code；失败事务无部分删除；并发变化由 DB guard 拒绝。

## C18 API and Migration Compatibility Contract

- **MUST**：新 API 使用 member、invitation、restore、department、archive 等准确术语；迁移遵循 expand/backfill/verify/switch/cleanup。
- **MUST NOT**：修改历史 migration、伪造历史 invitation/department、直接收缩 account_type 或删除 legacy route。
- **SOURCE OF TRUTH**：本合同 + versioned API contract + migration ledger。
- **INVARIANTS**：旧 direct-add 在兼容期行为不变且可观测；2/3 trusted binding 在切换前完成；default Workspace 显式存储。
- **TRANSITION**：M01-M09 逐 Gate 执行；任一 Gate FAIL/BLOCKED 不得越级 cleanup。
- **ACCEPTANCE**：新旧客户端兼容测试、up/down/up、数据对账、零 legacy caller 和 rollback rehearsal 有证据。

## 2. Frozen Relationship Rules

```text
User(account identity)
  -> Organization Membership (governance belonging)
      -> Department Membership (directory only)

Organization 1 -> N Workspace
Organization Membership != Workspace Membership

Organization -> Business Identity -> Assignment -> Human/Agent operator
Business Identity -> Customer Service Seat

Agent Membership != Agent Profile != Agent Grant != Delegation
                 != Workspace Role != Business Identity Assignment
                 != Customer Service Seat
```

## 3. Frozen Final State and Evidence Boundary

```text
MEMBERSHIP_SOURCE_OF_TRUTH = organization_member
OWNER_SOURCE_OF_TRUTH = active Human owner membership
OWNER_COMPATIBILITY_ANCHOR = organization.owner_id
EMPLOYEE_ENTITY = NONE
DEPARTMENT_MODEL = TREE_PLUS_MULTI_MEMBERSHIP_NO_RESOURCE_AUTH
INVITATION_MODEL = SEPARATE_LIFECYCLE
ORGANIZATION_LIFECYCLE = ACTIVE_ARCHIVED_NO_PUBLIC_PHYSICAL_DELETE
WORKSPACE_RELATION = ORG_1_TO_N_MEMBERSHIP_INDEPENDENT
AGENT_MEMBERSHIP = SHARED_TABLE_MEMBER_ONLY
AGENT_GRANT = SEPARATE_AGENT_DOMAIN_CONTRACT
CUSTOMER_SERVICE_MODEL = UNCHANGED
CORE_CONTRACT = FROZEN_CORE_CONTRACT
DB_RUNTIME_EVIDENCE = ENVIRONMENT_BLOCKED
```

冻结表示后续 Plan 可以消费这些边界，不表示 migration、API 或测试已经实现。任何实施发现与当前源码证据
冲突时必须 STOP、记录 evidence 并回到 ADR，而不是静默修改 Acceptance Criteria。
