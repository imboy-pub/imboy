# Enterprise Organization V1 / Customer Service Compatibility

本合同保留组织与客服的兼容边界及行为验收要求，不记录候选通过成绩。Agent 经办能力属于二期，关闭一期入口不取消已有的回归要求。

## 1. Compatibility Decision

Organization V1 接受当前 Customer Service 与 Enterprise Business 的既有边界，不重新设计客服：

```text
Organization Member
  -> Business Identity Assignment -> User/Agent operator
Organization
  -> Business Identity(function=customer_service)
      -> Customer Service Seat
          -> Customer Service Session(organization_id + workspace_id explicit)
```

Seat 不是 Employee、Account、Membership 或 Role；Business Identity 不是 Employee；Session 不从默认
Workspace 动态推导 scope。

## 2. CS Compatibility Matrix

| Existing CS Contract | Organization V1 decision | Migration required | Breaking? |
|---|---|---|---|
| `organization_member` 是组织成员真源 | 原样保留并强化 lifecycle | owner/deletion invariants | NO |
| member `suspended` 即时撤销 Org auth | 原样保留 | 补 lifecycle/API evidence | NO |
| remove 前 offboarding guard | 原样保留 | User deletion 接入同一 preflight | NO |
| Business Identity 是稳定业务主体 | 原样保留 | NONE | NO |
| Assignment 是 User 时态经办绑定 | 原样保留；未来可受控支持 Agent | Agent adapter later | NO for Human |
| Seat 是 Identity 的运营 Profile | 原样保留 | NONE | NO |
| Seat enabled/capacity 是 operational gate | 原样保留 | NONE | NO |
| Session 显式 Org+Workspace | 原样保留 | NONE | NO |
| Organization 与 Workspace membership 独立 | 原样保留 | NONE | NO |
| CS management 使用 Org governance facts | owner/admin 仍可治理；不自动获 Workspace access | adapter checks only | NO |
| active operator 来自 Assignment | Human 保持；Agent 默认关闭 | future Agent/CS gate | NO |
| Organization archive | 禁止新 Session/claim；历史不删除 | small adapter | additive |
| default Workspace | 不用于 Session 授权或重解析 | NONE | NO |


## 3. What Stays Unchanged

- `organization_business_identity` 的 identity、function、retire 语义。
- `organization_business_identity_assignment` 的时态历史、handover 和 offboarding 语义。
- `customer_service_seat` 以 `business_identity_id` 为主键的 Profile 语义。
- `customer_service_session` 的显式 `organization_id + workspace_id` scope。
- Organization suspend/remove 与 DB offboarding guard 的 fail-closed 方向。
- Customer/Contact 是 Organization-owned relationship，不是 member/employee。

## 4. Adapter-Only Changes

实施期允许的 CS 适配仅包括：

1. Organization archived 时拒绝新 Seat claim 和新 Session。
2. Organization Invitation/Restore 完成后再进入 CS onboarding。
3. User deletion preflight 调用现有 offboarding/Assignment facts。
4. Agent operator 启用前增加 Agent Grant + Assignment + CS auth 的交集校验。

这些修改不得改变 Seat/Session/Identity 的表主键、owner 或生命周期。

## 5. Forbidden Changes

- 不新增 `customer_service_member` 或 `customer_service_employee`。
- 不给 Seat 增加登录 identity 或 Organization role。
- 不把 `assignment.user_id` 改称 Seat owner。
- 不把 Department membership 当作坐席资格。
- 不允许 Organization owner/admin 绕过 Workspace membership 或 Session scope。
- 不允许 Agent 只凭 `account_type=1`、Profile 或 Org membership 经办 Seat。

## 6. Suspend and Offboarding Semantics

| Event | Membership | Assignment | Seat | Session | Workspace Membership |
|---|---|---|---|---|---|
| suspend | active -> suspended | 不自动结束 | 保留 | 历史/现存保留 | 不自动改变 |
| restore | suspended -> active | 不自动恢复 | 保留 | 不自动新建 | 不自动改变 |
| offboarding | suspended remains | handover/end | 保留 | 不删历史 | 独立处理 |
| remove | -> removed after no residual | 无 active | 保留 | 不删历史 | 独立处理 |
| User delete | 必须先闭合 | 必须无 active | 不随 User 删除 | 不随 User 删除 | owner/active dependency 先闭合 |

## 7. Acceptance

| ID | Precondition | Action | Expected | Evidence |
|---|---|---|---|---|
| CS-ORG-A01 | active operator assignment | suspend member | Enterprise/CS auth 立即拒绝 | auth + DB integration |
| CS-ORG-A02 | active assignment | direct remove | offboarding guard 拒绝 | PG error assertion |
| CS-ORG-A03 | handover complete | remove member | success，Seat/Session 保持 | PG row assertions |
| CS-ORG-A04 | archived Org | create/claim Session | fail closed | API E2E |
| CS-ORG-A05 | Org member but no Workspace member | access Session | denied | auth E2E |
| CS-ORG-A06 | default Workspace changes | read existing Session | persisted workspace unchanged | DB/API assertion |
| CS-ORG-A07 | Agent assignment without Grant | operate Seat | denied and audited | Agent/CS integration |
| CS-ORG-A08 | Agent assignment + Grant + policies | operate allowed action | principal/actor/scope audited | Agent/CS integration |

## 8. Verification Boundary

第 7 节列出的每项行为必须按对应数据库、接口及审计结果验证。架构合同、单元测试或历史候选成绩不能替代当前运行验证。执行日志、审查和验收结论保存于仓外，并绑定实际提交与产物；本文件不授予发布资格。
