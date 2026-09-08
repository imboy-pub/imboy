# MCP-02 — MCP Task Tools 与持久审批闭环

> 所属主计划：[IMBoy Agent Hub 产品收敛执行计划](../ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 本文件是 MCP-02 的权威执行卡。跨任务冻结决策、全局协议、依赖拓扑和独占边界以主计划 §1-§7 为准。

执行前必须先完成：

- 执行主计划 §4.1 的路径探测，使用 `$IMBOY_WORKSPACE_ROOT` 与 `$IMBOY_EVIDENCE_ROOT`，不得写死个人路径。
- 核对依赖状态和 §7 独占边界；只修改本卡声明的范围。
- 后端测试模块名以 G0-01 的 `test-module-map.tsv` 为准；卡片计划名变化时由 Coordinator 同步。
- 证据写入 `$IMBOY_EVIDENCE_ROOT/MCP-02/evidence.json` 并经 EVID-00 verifier 判定。
- 不 reset/clean/stash，不覆盖共享改动；未经人工确认不 commit，不 push，不执行外部或生产动作。

- Status：`blocked`
- Owner：Backend-MCP-Tools
- Dependencies：DATA-01、MCP-01、FSM-00、TRACE-00
- Repo：`imboy`
- Files：`imboy_mcp_tools.erl`、task tool tests、MCP task contract；共享 route 由 Coordinator 合并。
- Actions：
  1. 注册最小 tools：`create_agent_task`、`update_agent_task`、`request_task_approval`、`get_agent_task`。
  2. 所有 owner/actor/client_id 从 Ctx 获取；Args 仅含业务目标，不允许覆盖调用身份。
  3. `idempotency_key` 在同 client 下唯一；重复 create/update 返回原结果并保持同一 correlation_id。
  4. write tool 需要独立 grant；未批准 client、新增 tool、跨群、非成员、E2EE/未知场景全部拒绝。
  5. 审批结果通过 MCP read/poll 可读；有 SSE session 时可通知，但 poll 必须是权威兜底。
- Verify：

```bash
cd "$IMBOY_WORKSPACE_ROOT/imboy"
make app
make eunit-local t=imboy_mcp_task_tools_tests
make eunit-local t=imboy_mcp_tools_tests
make eunit-local t=mcp_authz_gate_tests
make eunit-local t=mcp_handler_sse_tests
git diff --check
```

- Acceptance IDs：`MCP-02-A01` 真实协议可创建任务并等到唯一审批结果；`MCP-02-A02` 未授权 tool 在执行前拒绝；`MCP-02-A03` 重放相同 idempotency key 不产生第二任务/消息且 correlation 不变；`MCP-02-A04` 超速/超并发 client 被隔离限流。
