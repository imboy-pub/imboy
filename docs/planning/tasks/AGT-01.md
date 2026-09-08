# AGT-01 — 内建 Agent Tool Loop 可行性 Spike

> 所属主计划：[IMBoy Agent Hub 产品收敛执行计划](../ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 本文件是 AGT-01 的权威执行卡。跨任务冻结决策、全局协议、依赖拓扑和独占边界以主计划 §1-§7 为准。

执行前必须先完成：

- 执行主计划 §4.1 的路径探测，使用 `$IMBOY_WORKSPACE_ROOT` 与 `$IMBOY_EVIDENCE_ROOT`，不得写死个人路径。
- 核对依赖状态和 §7 独占边界；只修改本卡声明的范围。
- 后端测试模块名以 G0-01 的 `test-module-map.tsv` 为准；卡片计划名变化时由 Coordinator 同步。
- 证据写入 `$IMBOY_EVIDENCE_ROOT/AGT-01/evidence.json` 并经 EVID-00 verifier 判定。
- 不 reset/clean/stash，不覆盖共享改动；未经人工确认不 commit，不 push，不执行外部或生产动作。

- Status：`blocked`
- Owner：Backend-Agent-Spike
- Dependencies：PDT-01、FSM-00、TRACE-00
- Repo：`imboy`
- Scope：测试和决策文档；不改生产默认路径，不调用真实 LLM。
- Actions：
  1. 用 fake OpenAI-compatible provider 返回 tool_calls，验证 request/response JSON 能被薄适配。
  2. 直接调用现有 registry 的 fake read tool，验证 schema、tool result 回填和最多 3 轮终止。
  3. 验证 write tool 不执行而生成 `awaiting_approval`；拒绝后不重试执行。
  4. 验证递归 tool call、未知 tool、坏 JSON、超时、超大结果、provider 不支持 tools 的降级。
  5. 输出 Go/No-Go 决策和预计生产改动文件。
- Verify：

```bash
cd "$IMBOY_WORKSPACE_ROOT/imboy"
make app
make eunit-local t=ai_agent_tool_loop_spike_tests
git diff --check -- test docs/planning
```

- Acceptance IDs：`AGT-01-A01` 核心循环不超过 200 行 spike 代码且没有新依赖；`AGT-01-A02` 全部负例有界终止；`AGT-01-A03` 决策文档明确复杂度、correlation 传播和残余风险。
- Go 条件：fake provider + registry + HITL 三者可用同一自包含上下文闭环，且无需 HTTP 自调用或复制 MCP registry。
- No-Go：任一条件不满足，记录 `wont_do_v1`，AGT-02 不执行且不阻断基础 Gate。
