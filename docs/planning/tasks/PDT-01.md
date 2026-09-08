# PDT-01 — 产品契约、Golden Flow 与安全 ADR

> 所属主计划：[IMBoy Agent Hub 产品收敛执行计划](../ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 本文件是 PDT-01 的权威执行卡。跨任务冻结决策、全局协议、依赖拓扑和独占边界以主计划 §1-§7 为准。

执行前必须先完成：

- 执行主计划 §4.1 的路径探测，使用 `$IMBOY_WORKSPACE_ROOT` 与 `$IMBOY_EVIDENCE_ROOT`，不得写死个人路径。
- 核对依赖状态和 §7 独占边界；只修改本卡声明的范围。
- 后端测试模块名以 G0-01 的 `test-module-map.tsv` 为准；卡片计划名变化时由 Coordinator 同步。
- 证据写入 `$IMBOY_EVIDENCE_ROOT/PDT-01/evidence.json` 并经 EVID-00 verifier 判定。
- 不 reset/clean/stash，不覆盖共享改动；未经人工确认不 commit，不 push，不执行外部或生产动作。

- Status：`blocked`
- Owner：Product-Architecture
- Dependencies：FSM-00=PASS、EVID-00=PASS、TRACE-00=PASS、BUILD-00=Go
- Repo：`imboy`
- Files：
  - Create `docs/api-contracts/agent_hub_task_api_contract.md`
  - Create `docs/api-contracts/agent_hub_webhook_contract.md`
  - Create `docs/api-contracts/agent_hub_mcp_client_contract.md`
  - Create `docs/adr/2026-09-07-agent-hub-trust-boundaries.md`
  - Modify 本计划，仅补 Base SHA/裁决，不更新任务完成状态；不得改写四项冻结契约
- Actions：
  1. 固定 actor：human、ai_agent、mcp_client、developer_bot、system_bot。
  2. 引用 TRACE-00，固定 `correlation_id` 与 `task_id`、`event_id`、`approval_id`、`execution_id`、`delivery_id`、协议 `request_id` 的边界和 JSON string 规则。
  3. 引用 FSM-00，不在 prose 中复制或另定义状态迁移；固定 task API 对矩阵 action/error 的映射。
  4. 固定 tool 风险等级：read、write、financial；V1 默认 read 可直行，write 必须 HITL，financial 不在本计划启用。
  5. 固定 Bot webhook headers、签名原文、重放时间窗、事件类型、reply context、解析 IP pinning 和 redirect 逐跳策略。
  6. 固定 MCP credential 创建一次显示、摘要存储、撤销/到期、兼容 JWT 的边界。
  7. 固定 MCP Client 最小滥用边界：复用现有限流能力，按 client 限制 tools/call 速率与并发；超限可观测且不影响其他 client。
- Verify：

```bash
cd "$IMBOY_WORKSPACE_ROOT/imboy"
rg -n 'task_id|delivery_id|signature|expires_at|E2EE|idempot' docs/api-contracts/agent_hub_* docs/adr/2026-09-07-agent-hub-trust-boundaries.md
git diff --check -- docs/api-contracts docs/adr docs/planning/ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md
```

- Acceptance IDs：`PDT-01-A01` 三份契约均有正例、负例、错误码和兼容策略；`PDT-01-A02` ADR 明确 E2EE、SSRF/IP pin、身份代理、MCP 滥用边界和审计红线；`PDT-01-A03` 全部引用已冻结 FSM/Evidence/Trace，且没有“待实现时再决定”的 P0 字段。
- Stop：支付 tool、E2EE 正文处理或跨租户语义需要产品裁决时标记 `blocked_decision`。
