# AGT-01 Spike 决策 — LLM tool-loop 薄适配（Go）

> Spike 代码：test/spike/ai_agent_tool_loop_spike.erl（核心循环 93 行，≤200 达标）
> 测试：test/spike/ai_agent_tool_loop_spike_tests.erl（6/6 绿）
> 结论：**Go** —— AGT-02 可执行（内建 Agent tool 执行链）

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

## 3. 生产化预计改动文件（AGT-02 范围）

- src/lib/imboy_llm_openai.erl：tool_calls 解析/消息序列化扩展（现仅文本）。
- 新 src/logic/ai_agent_tool_loop.erl：spike 循环生产化（Ctx 注入
  Principal+correlation_id（TRACE-00），工具结果审计）。
- src/logic/ai_agent_group_reply.erl：接 tool-loop（替换/扩展现关键词直答）。
- src/mcp/mcp_authz_gate.erl：write tool → awaiting_approval 的 HITL 桥（复用
  agent_task）。

## 4. correlation 传播

loop 每轮共享同一 correlation_id（经 Opts/ctx 透传；TRACE-00 §6 通道规则），
tool 结果审计只记 digest 与 tool 名（隐私红线）。

## 5. 残余风险

- spike 未覆盖流式 tool_calls（Phase 2）；并发多 tool call 按顺序执行。
- token 预算控制（messages 累积上限）留生产化参数化。
