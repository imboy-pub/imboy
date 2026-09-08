# Agent Hub MCP Client 契约（PDT-01）

> 版本：1.0（冻结）| 任务卡：[PDT-01](../planning/tasks/PDT-01.md) | 主计划：[§7](../planning/ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
>
> 引用真源：链字段 TRACE-00；任务状态语义 FSM-00；task API 面
> [agent_hub_task_api_contract.md](./agent_hub_task_api_contract.md)。MCP 协议本
> 身（JSON-RPC 2.0 / streamable HTTP）以外部规范为准，本文件固定 IMBoy 侧的身
> 份、治理与滥用边界。

## 1. 目的与范围

固定外部 MCP Agent 接入 IMBoy 的：credential 生命周期（创建即显、摘要存储、撤
销/到期）、与现有 JWT 认证体系的边界、每 client 限流与并发治理、task tool 的
授权边界。实现任务为 MCP-01（credential/治理）与 MCP-02（task tools）。

不在范围：MCP 线协议编码、内建 Agent（AGT-01/AGT-02）、Bot webhook
（agent_hub_webhook_contract.md）。

## 2. MCP client credential 生命周期（冻结）

| 阶段 | 冻结语义 |
|---|---|
| 创建 | 管理面（admin JWT）创建 `mcp_client` 记录：`client_id` + 随机 secret（≥32 字节）；**明文 secret 仅在创建响应返回一次**，服务端不保存明文 |
| 存储 | 只存 `SHA-256(secret)` 摘要（hex）+ 摘要算法标识；日志/审计/错误信息禁止出现明文或摘要全文（审计只允许 client_id，TRACE-00 §7） |
| 认证 | `Authorization: Bearer <secret>`；服务端摘要比对（常数时间比较）；认证通过后按 TRACE-00 §3.1 生成 correlation_id |
| 撤销 | revoke 后该 credential 的现有会话在下一个请求即 401（无宽限期）； revoke 事件留管理审计 |
| 到期 | `expires_at`（UTC ISO-8601 string，JSON 一律 string）到期后同 revoke；到期前 0 通知（由创建方自行管理轮换） |
| 轮换 | create-new → 切换 → revoke-old 由使用方编排；平台不做双活宽限 |

## 3. 与现有 JWT 认证体系的边界（冻结）

| 维度 | JWT 用户身份 | MCP client credential |
|---|---|---|
| 令牌形态 | 签名 JWT（有 expiry 的自包含断言） | 不透明随机 secret（无自包含语义） |
| 校验方式 | 验签 + claims | 服务端摘要查表 |
| 身份语义 | 用户（human） | 平台 actor `mcp_client`（agent 身份，不是用户） |
| 授权模型 | 用户角色/权限 | grant 白名单：credential 创建时登记可用的 tool/toolset |
| 复用 | MCP 认证**不复用** JWT 验签中间件，二者并列；`mcp_client` 不得持有 JWT，亦不得把 JWT 当 secret 存摘要 |

硬规则：MCP credential 永远不映射到某个用户身份；tool 执行的审计 actor 记
`mcp_client` + `client_id`，禁止以用户名义落链。JWT 兼容仅限一点：两者的
HTTP header 形态一致（`Authorization: Bearer …`），中间件按 token 前缀/长度与
查表结果分流，分流失败一律 401（fail-closed）。

## 4. 滥用边界：限流与并发（冻结）

复用现有限流能力（`imboy_limiter`/config 现有 per-key 限流基建），按
credential 维度隔离：

| 限制 | V1 默认（config 可调） | 超限行为 |
|---|---|---|
| `tools/call` 速率 | 每 client 每 60 秒 60 次 | `429 rate_limited`（JSON-RPC error），带 `retry_after_sec` |
| 并发执行 | 每 client 4 个并发 task/execution | 超出的创建请求 `429 concurrency_limited` |
| 会话数 | 每 client 8 个活跃 MCP 会话 | 最旧会话被拒 `429` |
| payload | tool 参数 ≤ 256 KiB | `413` |

隔离保证：限额按 credential 独立计数，任一 client 触发限流**不得影响**其他
client 或用户流量（按 key 隔离，禁止全局熔断）。可观测：限流命中必须产生服务
端度量/日志（计 client_id、限流类型、时间），但不进入 trace 链（TRACE-00 §7）。

## 5. task tool 授权边界（MCP-02 实现目标）

- tool 清单按 credential 的 **grant 白名单**判定：未授权 tool 的 `tools/call`
  → `-32602 tool_not_granted`（JSON-RPC error code 使用实现选定的应用错误码，
  语义固定）。
- task tool 创建的任务：actor=`mcp_client`、execution_actor=`mcp_client`，走
  task API 契约（FSM-00 九状态；read 直行/write HITL/financial 禁用同
  §7）。
- `tools/list` 只返回 grant 白名单内工具；未授权工具不出现在枚举中（不泄露存在性）。
- sampling/elicit 反向请求：V1 不启用（`method_not_found`）。

## 6. 错误码（冻结）

| 错误 | 场景 |
|---|---|
| `401 credential_invalid` | secret 错误/撤销/到期/分流失败 |
| `403 tool_not_granted` | grant 白名单外（HTTP 面表述） |
| `429 rate_limited` / `429 concurrency_limited` | §4 超限 |
| `422 risk_level_disabled` | financial 工具（与 task API 同码同义） |
| `409 invalid_transition` / `already_decided` | 与 task API 同码同义 |

兼容策略：错误码语义跨 task API / webhook / MCP 三契约保持一致字面；只增不改。

## 7. 正例 / 负例

正例：client 以 `Authorization: Bearer <secret>` 连接 → 校验摘要、未到期、未
撤销 → 会话建立；`tools/call` 创建 read 任务 → `submitted` → start/complete
链路按 task API；审计链 actor=`mcp_client`。

负例：
- 撤销后的 secret 下一请求 → `401 credential_invalid`。
- grant 外工具 `tools/call` → `tool_not_granted`，且 `tools/list` 不出现该工具。
- 第 5 个并发任务创建 → `429 concurrency_limited`；同刻其他 client 不受影响。
- credential 伪装用户：payload 携带 `"actor": "human"` → 忽略，审计仍记
  `mcp_client`。
- 客户端 `correlation_id` 注入 → TRACE-00 §3.3 丢弃。

## 8. 现状差异（现有代码 vs 本契约）

| # | 现状 | 目标 | 兑现任务 |
|---|---|---|---|
| 1 | MCP 接入无独立 credential（复用内部会话） | §2 生命周期 | MCP-01（migration 91） |
| 2 | 无 per-client 限流/并发治理 | §4 | MCP-01 |
| 3 | `barrel_mcp_tasks` 直接 working、无审批分支 | 九状态矩阵 + grant 边界 | MCP-02 |

## 9. 变更规约

credential 语义、JWT 边界、限额隔离保证、授权模型为冻结项；限额默认值属
config 可调（调大不算契约变更）。破坏性变更升版并经 Coordinator 合并。
