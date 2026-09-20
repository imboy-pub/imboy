# 智能体域（Agent）：Grant · Run · Hirð · HITL

> Purpose：定义平台 AI 智能体从「账号」到「受约束执行」的完整链路。
> 账号层面的定义（account_type=1）见[账号与主体](./accounts-and-actors.md)；本文覆盖执行治理层。
> 架构权威：[Agent Runtime V3.1 架构](../architecture/2026-09-16-imboy-agent-runtime-v3.1.md)（2026-09-16 冻结契约）。

## Concept：一条执行链

```
真人委托人（Human delegator）
   │  委派（Delegation = Grant + 事件谱系，概念名，无独立实体）
   ▼
Grant（授权，迁移 133）──── 最大可执行边界，DEFAULT=DENY，能力只能收窄
   │  trigger: message / schedule / webhook
   ▼
Agent Run（代理运行，迁移 134）── 八态状态机 + 租约 + 幂等五元组
   │  每次工具调用
   ▼
Effect（效果账本）──── dispatch 先持久化再调 adapter；HITL 需审批
   ▼
Tool Adapter（native / MCP）── 工具权限（mcp_client_grant 按 tool 粒度）
```

三个账本**绝不共表**：`agent_run`（Runtime 执行）、`agent_task`（群内 @智能体任务流，九态 + first-writer-wins 审批）、MCP task（外部协议任务）。

## Current

**已实现（并入 main，CHANGELOG alpha.77）：**

- **Grant 基础**：`agent_grant(+workspace/capability/event)`；授权边界=能力×动作×资源类型×约束 JSON（只收窄）；event append-only（触发器禁改删，唯一例外 user 删除 SET NULL）；过期不入库、实时判定；CAS version；按 (org, delegator) 幂等。API：`features/agent/interfaces/agent_grant_handler`。
- **Run 基础**：`agent_run(+event)`、`agent_effect`；状态机 `created→queued→running→(waiting_approval)→succeeded/failed/cancelled`（timeout 不是状态，是 reason_code）；DB 条件更新抢租约；`context_digest`/`args_digest` 全摘要化（不落 Prompt）。
- **Hirð 桥**：`imboy_hird`（ACL：DTO/生命周期/Tool handler/audit bridge，fail-closed）+ `imboy_hird_replay`（确定性重放，audit hash 比对）；`runtime_type ∈ [mock, hird]`。Hirð 完整运行时（actor/dispatch 模块）是外部组件，不在本仓（UNKNOWN：外部运行时的交付与版本来源，见架构文档）。
- **恢复与治理**：`agent_recovery`、`agent_hitl_policy`、`agent_capability_catalog`、`agent_trigger_adapter`。
- **旧有能力**：`ai_agent`（助手发现 `/agent/list`）、`ai_agent_role(+version)`（角色模板版本化，一角色至多一条 published）、`agent_payment_mandate`（代付授权）+ 补偿 worker、`agent_task` 审批（App 端 `/agent_task/approve|reject`）、`agent_hub_audit`（链路元数据，禁 payload/PII）、MCP 治理（`mcp_client` 按 tool 授权 + 审计）。
- **在线形态**：智能体经 `ai_agent_runtime` 注册 presence（「在线」即可被 @）。

**三端实现差（CURRENT 边界）：**

- App 端只消费 `/api/v1/agent/list`（助手发现）与 `agent_task` 审批两块；Grant/Run 无客户端 UI。
- Admin 端 `/ai-agents` 管的是 `ai_agent` 助手档案与角色模板（AI 助手管理），**不是** Grant/Run 治理面。

## TARGET

- Hirð 运行时生产化接入（当前 run 以 mock/桥验证为主）。
- Grant/Run 的组织侧自助管理界面（无计划文档 → PLAN 未立项，UNKNOWN）。

## Contract

1. Grant 语义边界：「Agent Grant ≠ MCP Client Grant ≠ Bot OAuth Grant」三个同名后缀实体互不相干（MCP Client 授权给工具调用方、Bot 授权给 OAuth 集成、Agent Grant 是本域实体）。
2. Run/Effect/Grant event 三类 append-only 表不允许任何 UPDATE/DELETE 路径（DB 触发器守卫）。
3. `delegator` 必须是同组织 active 真人（组织×智能体契约，`docs/architecture/2026-09-16-enterprise-organization-agent-contract.md`）。

## Constraints

- Effect 的 `dispatching` 状态必须先于 adapter 调用持久化（崩溃可对账）。
- 支付类工具受 Payment Mandate 边界约束（付款人恒为 owner，窗口滚动限额）。

## References

- 架构（权威）：`docs/architecture/2026-09-16-imboy-agent-runtime-v3.1.md`
- 迁移：27/29/58/61/90/107/**133/134**
