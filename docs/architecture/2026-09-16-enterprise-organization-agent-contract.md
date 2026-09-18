# Enterprise Organization V1 / Agent Organization Contract

> STATUS: `FROZEN_ORGANIZATION_SIDE_CONTRACT`
>
> AGENT_RUNTIME_IMPLEMENTATION: `OUT_OF_SCOPE`
>
> HIRD_STATUS: `CANDIDATE_RUNTIME_KERNEL`

本文只冻结 Organization 提供给 Agent V3.1 的边界，不设计或实现 Agent Runtime、Hirð、AgentRun
状态机或具体 Grant schema。

## 1. Boundary Model

```text
Agent Identity (user.account_type=1)
  -> active Organization Membership(role=member)
  -> active Workspace Membership
  -> active Agent Grant / Delegation
  -> Resource Policy
  -> immutable AgentRun Context(organization_id, workspace_id, agent_id, grant_id)
  -> per-Tool authorization
```

```text
Organization Membership
!= Agent Profile
!= Agent Grant
!= Delegation
!= Workspace Membership
!= Organization Governance Role
!= Business Identity Assignment
!= Customer Service Seat
```

## 2. Explicit Decisions

| Question | Frozen decision |
|---|---|
| Agent 能否成为 Organization Member | YES，使用 `organization_member` |
| Agent 能否属于多个 Organization | YES，每个 membership 状态独立 |
| Human/Agent 是否共用 Membership 表 | YES，不建第二套 Agent membership |
| Agent 可用 Organization role | 仅 `member`；owner/admin 必须 Human |
| Organization role 是否表达 capability | NO，只表达治理归属；Agent capability 必须来自 Grant |
| Agent Grant 是否独立 | YES，归 Agent Domain |
| Delegation 是谁授予谁 | 有授权资格的 Human principal 向指定 Agent 授予明确 scope/capability |
| Grant 最大边界 | issuer 当前可委托范围、Org/Workspace membership、Resource Policy 的交集 |
| Workspace scope | active `workspace_member` + Grant workspace scope；Org membership 不自动产生 |
| Agent 可访问 Business Identity | 仅在同 Org + explicit Grant + function policy 允许时 |
| Agent 可成为 CS operator | Target 允许经 Assignment；实现默认关闭，直到 Agent/CS Gate PASS |
| AgentRun context 来源 | Trigger input 经 membership/grant/policy 校验后解析并持久化，运行中不可变 |
| Agent 能否代表 User | 仅显式 Delegation；审计必须区分 principal 与 actor |
| 每次 Tool 是否查 Grant | YES，创建 Run 时校验不能替代执行时校验 |

## 3. Identity and Membership

- Agent identity 是 `user(account_type=1)`；Bot 是产品称呼，不是另一个 Account 类型。
- Membership 只证明 Agent 属于某 Org，不证明模型、Runtime、Tool、数据或 Workspace 权限。
- Agent membership 的 create/suspend/restore/remove 必须受 Human owner/admin 管理并审计。
- suspend/remove 必须阻止新 Run，并使进行中 Run 的下一次 Effect 授权 fail closed。
- Agent 不得成为 Organization owner/admin 或 Department admin。

## 4. Profile, Grant and Delegation

| Concept | Owner | Meaning | Can grant permission? |
|---|---|---|---|
| Agent Profile | Agent Domain | Prompt、模型偏好、行为/能力声明 | NO |
| Organization Membership | Organization Core | 组织归属与 member 状态 | NO by itself |
| Workspace Membership | Workspace Core | Workspace 资源授权 | Necessary, not sufficient |
| Agent Grant | Agent Domain | 可执行 capability 与 Org/Workspace/resource 最大边界 | YES, bounded |
| Delegation | Agent Domain | Human principal 对 Agent 的授权关系与责任链 | YES, bounded |
| Resource Policy | Resource owner/domain | 当前资源与动作是否允许 | Final intersect |

Profile、Prompt、LLM response、Tool arguments、Runtime config 和 Hirð effect declaration 都是非权威输入，
不能扩大 Grant。

## 5. Grant Minimum Contract for Agent V3.1

Agent V3.1 必须至少表达：

```text
grant_id
agent_id
organization_id
workspace_scope (none | explicit workspace ids; never implicit all)
capability/action set
resource constraints
delegator_user_id
status
valid_from / expires_at
revoked_at / revoked_by
version
created_at / updated_at
```

约束：

- Grant 的 agent 必须有同 Org active membership。
- workspace scope 中每个 Workspace 必须属于该 Org，Agent 必须有 active Workspace membership。
- delegator 必须是同 Org active Human，且当前有委托该 capability/resource 的资格。
- Grant 默认 deny、显式 allow、可撤销、可过期；禁止 wildcard 默认扩大。
- Grant revoke、membership suspend、Workspace removal、Org archive 都必须在下一 Tool Effect 生效。

## 6. AgentRun Context

AgentRun 创建时必须从 Message/Schedule/Webhook Trigger 中解析候选 scope，然后由 IMBoy 校验并持久化：

```text
agent_id
organization_id
workspace_id (nullable only for explicitly organization-scoped capability)
grant_id/version
delegating_principal_id
trigger_type/trigger_id
```

- 默认 Workspace 只可作为创建 Run 的候选解析输入；解析后必须写入 Run。
- Runtime/Hirð 只消费已验证 Context，不自行查找“最近”“最小 ID”或 Prompt 指定的 Org/Workspace。
- 每次 Tool 调用同时校验 persisted Context、当前 Grant、当前 memberships 和 Resource Policy。
- 审计同时记录 Agent actor、delegating principal、Org、Workspace、Grant、Run 与 Effect。

## 7. Business Identity and Customer Service

Target 合同允许 Agent 作为 Business Identity Assignment 的 assignee，因为 Agent 也是 Account identity；
但该能力在实现上必须默认关闭，直到以下全部 PASS：

1. Agent 有 active Org membership 与适用 Workspace membership。
2. explicit Grant 包含对应 function/action/resource scope。
3. Business Identity 与 AgentRun 属于同 Org。
4. CS auth adapter 对每次动作检查 Assignment + Seat gate + Grant + Resource Policy。
5. HITL-required 写动作 fail closed。
6. suspend/revoke/offboarding/cancel/recovery 的并发与审计测试通过。

Seat 仍绑定 Business Identity；Agent 只是可替换 operator，不成为 Seat owner，也不获得独立 CS Role。

## 8. Organization APIs Exposed to Agent V3.1

Agent V3.1 只可依赖稳定读合同或受控 command：

| Contract | Input | Output |
|---|---|---|
| resolve membership | org_id, agent_id | status, role(member), version |
| resolve workspace membership | workspace_id, agent_id | status, role, version |
| resolve organization state | org_id | active/archived, version |
| validate workspace ownership | org_id, workspace_id | same-org boolean/fact version |
| consume default workspace | org_id | nullable workspace_id; only at Run creation |
| membership lifecycle event | agent_id, org_id | suspend/remove/version event for cancellation/recheck |

Agent Domain 不得直接写 Organization Core 表；写操作通过 Organization command/API。

## 9. Acceptance Matrix

| ID | Precondition | Action | Expected | Evidence |
|---|---|---|---|---|
| AG-ORG-A01 | Agent active Org member only | create Tool-capable Run | denied without Grant | API/runtime test |
| AG-ORG-A02 | Grant in Org A | replace context with Org B | denied and audited | negative E2E |
| AG-ORG-A03 | Org + Grant, no Workspace member | access Workspace resource | denied | permission test |
| AG-ORG-A04 | valid Run | revoke Grant before next Tool | next Tool denied | concurrency E2E |
| AG-ORG-A05 | valid Run | suspend Agent membership | new Run and next Tool denied | lifecycle E2E |
| AG-ORG-A06 | valid Run with default WS | change default during Run | persisted workspace unchanged | DB/runtime test |
| AG-ORG-A07 | Agent Profile claims broader scope | call outside Grant | denied | prompt/tool injection test |
| AG-ORG-A08 | Agent CS assignment no CS Grant | operate Seat | denied | CS integration |
| AG-ORG-A09 | all CS facts valid | allowed operation | actor/principal/scope audit complete | CS integration |
| AG-ORG-A10 | archived Org | create Run | denied | trigger E2E |

## 10. Gate for Phase 3

```text
AGENT_IDENTITY = FROZEN
AGENT_MEMBERSHIP = FROZEN_SHARED_MEMBER_ONLY
AGENT_ORGANIZATION_SCOPE = FROZEN
AGENT_WORKSPACE_SCOPE = FROZEN
ORGANIZATION_ROLE_NE_AGENT_GRANT = FROZEN
AGENT_GRANT_OWNER = AGENT_DOMAIN
AGENTRUN_CONTEXT_INPUT = FROZEN
AGENT_CS_OPERATOR = TARGET_ALLOWED_IMPLEMENTATION_DISABLED
AGENT_CONTRACT = PASS
READY_FOR_AGENT_V3.1_ARCHITECTURE_AND_PLAN = YES
```

这不是 Hirð Production Gate，也不授权 Agent Runtime 实现。Phase 3 必须继续把 Hirð 视为 candidate runtime kernel。
