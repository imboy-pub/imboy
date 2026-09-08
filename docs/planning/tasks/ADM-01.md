# ADM-01 — Admin “AI 协作”统一控制台

> 所属主计划：[IMBoy Agent Hub 产品收敛执行计划](../ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 本文件是 ADM-01 的权威执行卡。跨任务冻结决策、全局协议、依赖拓扑和独占边界以主计划 §1-§7 为准。

执行前必须先完成：

- 执行主计划 §4.1 的路径探测，使用 `$IMBOY_WORKSPACE_ROOT` 与 `$IMBOY_EVIDENCE_ROOT`，不得写死个人路径。
- 核对依赖状态和 §7 独占边界；只修改本卡声明的范围。
- 后端测试模块名以 G0-01 的 `test-module-map.tsv` 为准；卡片计划名变化时由 Coordinator 同步。
- 证据写入 `$IMBOY_EVIDENCE_ROOT/ADM-01/evidence.json` 并经 EVID-00 verifier 判定。
- 不 reset/clean/stash，不覆盖共享改动；未经人工确认不 commit，不 push，不执行外部或生产动作。

- Status：`blocked`
- Owner：Admin-Integrations
- Dependencies：DATA-01、MCP-01、WH-01、WH-02、TRACE-00
- Repo：`imboyadmin`
- Files：新 AI 协作 overview/router；现有 Agent/MCP/Bot 页的小范围接线；API/types/tests。
- Actions：
  1. 增加“AI 协作”父入口，复用现有 AI 助手、Bot、MCP 治理页，不复制 CRUD。
  2. Overview 只显示可操作信号：启用 Agent 数、待批 MCP Client、pending/dead webhook deliveries、awaiting task approvals。
  3. MCP 凭证创建后只显示一次，关闭后不可再取；支持 revoke/expiry/grants。
  4. Webhook delivery 可按状态/时间/Bot 筛选，查看截断错误并单次重放 dead。
  5. Task audit 可按 task/client/agent/status 查；不展示消息正文、secret 或完整 webhook URL。
  6. 所有敏感按钮受既有 RBAC 和确认框控制；读取权限不能获得写操作。
  7. 现有频道 webhook 页接入 WH-02 的一次显示、prefix/status、rotate/grace/disable；不得留下只能靠 API 操作的 V1 孤儿能力。
  8. Task/delivery 详情可显示和复制脱敏 correlation_id，供独立审计重建，不展示内部 secret。
- Verify：

```bash
cd "$IMBOY_WORKSPACE_ROOT/imboyadmin"
bun test src/pages/mcp-governance src/modules/bots src/modules/ai_agent src/modules/agent-hub
bun run typecheck
bun run lint
bun run build
```

- Acceptance IDs：`ADM-01-A01` Agent/MCP/Bot/Task 与频道 webhook 治理从统一入口可达；`ADM-01-A02` secret 一次显示且刷新消失；`ADM-01-A03` 无写权限按钮不可用且 API 不发出；`ADM-01-A04` 分页/筛选变化重置 page=1；`ADM-01-A05` loading/error/empty/success/forbidden 与 rotate UI 均有测试；`ADM-01-A06` correlation 可见但不泄露 secret/PII。
