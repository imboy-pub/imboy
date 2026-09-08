# AGT-02 — 受控内建 Agent Tool Loop（仅 AGT-01 Go）

> 所属主计划：[IMBoy Agent Hub 产品收敛执行计划](../ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 本文件是 AGT-02 的权威执行卡。跨任务冻结决策、全局协议、依赖拓扑和独占边界以主计划 §1-§7 为准。

执行前必须先完成：

- 执行主计划 §4.1 的路径探测，使用 `$IMBOY_WORKSPACE_ROOT` 与 `$IMBOY_EVIDENCE_ROOT`，不得写死个人路径。
- 核对依赖状态和 §7 独占边界；只修改本卡声明的范围。
- 后端测试模块名以 G0-01 的 `test-module-map.tsv` 为准；卡片计划名变化时由 Coordinator 同步。
- 证据写入 `$IMBOY_EVIDENCE_ROOT/AGT-02/evidence.json` 并经 EVID-00 verifier 判定。
- 不 reset/clean/stash，不覆盖共享改动；未经人工确认不 commit，不 push，不执行外部或生产动作。

- Status：`blocked_conditional`
- Owner：Backend-Agent-Tools
- Dependencies：AGT-01=Go、DATA-01、MCP-01、FSM-00、TRACE-00
- Repo：`imboy`
- Files：LLM tool callback、`agent_tool_loop/gate`、Agent reply/group reply、tests。
- Actions：
  1. 给 behaviour 增加可选 tool-capable callback；不破坏 `tools=false` provider。
  2. 复用 MCP registry/schema，不复制工具清单；使用独立 Agent gate。
  3. C2C/C2G tool 执行身份为发起 human；agent_uid 仅作执行者审计字段。
  4. read tool 可按 role allowlist 自动执行；write tool 进入 DATA-01 的 HITL；financial 永不执行。
  5. 最多 3 轮、每轮 tool 数上限、总结果字节上限、总 deadline；超限给用户可见错误并审计。
  6. 批准后使用持久 execution idempotency key；可证明幂等的 tool 才能恢复重试，不可证明幂等的 tool 在不确定结果时转人工复核，不重复自动调用。
- Verify：

```bash
cd "$IMBOY_WORKSPACE_ROOT/imboy"
make app
make eunit-local t=imboy_llm_openai_tools_tests
make eunit-local t=agent_tool_gate_tests
make eunit-local t=agent_tool_loop_tests
make eunit-local t=ai_agent_reply_tests
make eunit-local t=ai_agent_group_reply_tests
make eunit-local t=ai_agent_policy_tests
git diff --check
```

- Acceptance IDs：`AGT-02-A01` read tool 正常闭环；`AGT-02-A02` write tool 未审批零副作用、批准后按 FSM/幂等契约调度、拒绝后零次；`AGT-02-A03` proactive Agent 无 human actor 时写工具拒绝；`AGT-02-A04` 未知 tool/循环/超时均有界失败且 correlation 可重建。
- Stop：不得把 financial tool 临时归类 read/write；不得为兼容某单一云 Provider 在通用 loop 内堆特判。
