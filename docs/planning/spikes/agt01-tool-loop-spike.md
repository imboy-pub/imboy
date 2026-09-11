# AGT-01 Spike 决策 — LLM tool-loop 薄适配（No-Go for V1）

> Spike 代码：test/spike/ai_agent_tool_loop_spike.erl（核心循环 93 行，≤200 达标）
> 测试：test/spike/ai_agent_tool_loop_spike_tests.erl（6/6 绿）
> 2026-09-11 复审结论：**No-Go for V1**。保留 spike 作为研究证据，生产只支持
> “内建 Agent 纯对话 + 外部 MCP Client 执行 tools”。

复审发现 spike 的 Go 前提没有在生产链成立：当前生产 Provider 均声明
`tools=false`；Agent/角色模型没有持久 tool allowlist 与风险等级；群回复丢失发起
human 身份并把 `agent_uid` 误作调用者；registry 的 write tool 尚未接入持久 HITL
执行恢复。为收口安全，不伪造 MCP Client 凭证，也不把上述缺口扩建为新功能。
AGT-02 在 V1 中不执行；重新启用必须先完成独立 `agent_tool_gate`、human actor
传播、read/write/financial 分类、write HITL/幂等恢复及生产 Provider 契约测试。

## 1. 闭环验证（fake provider，零网络/零新依赖）

- fake OpenAI 兼容 provider（fun 注入）：tool_calls 轮 → registry fake read tool
  执行 → 结果回填 → 第二轮定稿，同一自包含上下文闭环（positive_loop）。
- registry 复用：barrel_mcp_registry 现有 run_tool/3 与 reg 注册面，未复制 registry。
- HITL：write tool 不执行而返回 awaiting_approval 语义，loop 不重试执行
  （write_tool_no_execute 用例，计数=1）。
- 无 HTTP 自调用：ToolFun 以 fun 注入（生产接线点=MCP-01 的 gate+Principal）。

## 2. 负例矩阵（全部有界终止，A02）

| 负例 | 行为 |
|---|---|
| 递归 tool call（provider 恒 tool_calls） | 轮数上限 3 → {max_rounds_exceeded,3} |
| 未知 tool | 错误消息回填为 tool result，循环继续至定稿 |
| 坏 JSON arguments | 错误消息回填，不崩溃 |
| 超大结果（20KB） | 截断至 8KB 回填 |
| provider 坏响应形态 | {bad_provider_response,_} |
| provider error | {provider_error,_} |

## 3. 重新立项前置门（当前不执行）

- Provider 必须有真实 `tools` 请求/响应解析测试，不能只把 capability 改成 true。
- Agent/角色配置必须持久化 allowlist 与 read/write/financial 风险等级。
- 新建独立 `agent_tool_gate`；调用身份继承发起 human，`agent_uid` 只作审计字段。
- write tool 必须接入持久审批、execution idempotency key 与崩溃恢复；financial 拒绝。
- C2C/C2G、proactive 无 human actor、全部 registry 回包及总 deadline 均须有回归。

## 4. correlation 传播

spike 证明 correlation_id 可以经 Opts/ctx 透传，但这不是生产证据。重新立项时必须
由真实入口生成并贯穿 task/approval/execution；tool 审计只记 digest 与 tool 名。

## 5. 残余风险

- spike 文件仍保留，不能被产品文档或 UI 当作已交付能力。
- 重新立项前，内建 Agent 不得调用任何 registry tool；外部 MCP 路径不受影响。
