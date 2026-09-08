# Agent Hub Task API 契约（PDT-01）

> 版本：1.0（冻结）| 任务卡：[PDT-01](../planning/tasks/PDT-01.md) | 主计划：[§7](../planning/ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
>
> 引用真源：状态语义以 [agent_hub_task_state_machine.md](./agent_hub_task_state_machine.md)（FSM-00 冻结）为准，
> 链/ID 语义以 [agent_hub_correlation_contract.md](./agent_hub_correlation_contract.md)（TRACE-00 冻结）为准。
> 本文件不复制、不另定义状态迁移与链格式；两者冲突时以冻结契约为准并修本文件。

## 1. 目的与范围

固定 Agent Task 的 HTTP API 面：资源与端点、平台 actor 全集、鉴权与授权映射、
工具风险等级、请求/响应与错误码、幂等与审计要求。实现任务为 DATA-01（持久化
与状态机接线）与 MCP-02（task tools）。

不在范围：状态迁移语义（FSM-00）、correlation 链格式（TRACE-00）、MCP 协议
封装（agent_hub_mcp_client_contract.md）、Webhook 交付（agent_hub_webhook_contract.md）。

## 2. 平台 actor 全集（冻结）

| actor | 身份来源 | 说明 |
|---|---|---|
| `human` | JWT 已认证用户（current_uid） | 含任务 owner、管理员、审批人 |
| `ai_agent` | 内建 Agent 运行时（服务端派生身份） | 平台内建 agent，type=1 |
| `mcp_client` | MCP client credential（见 mcp_client 契约） | 外部 MCP Agent |
| `developer_bot` | Bot API token（bot_repo 凭证） | 开发者 Bot |
| `system_bot` | 平台系统 bot 身份 | 仅用于终态可靠群消息等平台通知投递 |

与 FSM-00 `allowed_actors` 的映射（实现层据此判定授权，不得放宽）：

| FSM-00 actor | 允许的平台 actor |
|---|---|
| `task_owner` | `human`（owner 本人）、`ai_agent`、`mcp_client`、`developer_bot`（创建者身份） |
| `admin` | `human`（持 admin 角色） |
| `human_approver` | `human`（群内有权成员且非任务所属 agent，服务端会话派生） |
| `agent_worker` | `ai_agent`（内建执行体）、`mcp_client`（外部执行体） |
| `system` | 平台定时器/调度器（无外部身份；`system_bot` 不属于此类，它只投递通知） |

硬规则：审批人身份必须由服务端已认证会话派生（JWT current_uid），禁止从请求体
透传 actor 字段冒充；`agent_worker` 的执行身份绑定到任务创建时登记的执行体，
切换执行体视为新任务。

## 3. ID 与链字段边界（引用 TRACE-00）

| 字段 | 定义域 | 规则 |
|---|---|---|
| `correlation_id` | 审计链 | TRACE-00 §2；服务端首个受信入口生成，客户端提供值一律丢弃 |
| `task_id` | Agent Task 实体 | TRACE-00 §4.1 `task` 实体 `entity_id`；JSON 一律 string，`^[A-Za-z0-9_-]{16,64}$` |
| `event_id` / `approval_id` / `execution_id` / `delivery_id` | 对应链实体 | 同上格式；各自独立生成，禁止复用彼此或复用 task_id |
| `request_id`（协议） | 一次 HTTP 请求 / JSON-RPC 配对 | 仅做协议级配对与幂等去重（沿用现有语义，TRACE-00 §8），不落链、不与 entity_id 混用 |

规则：Task API 的 JSON 中所有 ID 字段一律 string（禁止 number）；日志与审计中
链字段只允许 TRACE-00 §7 白名单。

## 4. 端点（V1 冻结）

认证：全部端点要求已认证（JWT / MCP credential / Bot token）。未认证请求不产
生链实体（TRACE-00 §3.1）。

| 端点 | 方法 | actor | 语义 |
|---|---|---|---|
| `/api/v1/agent/tasks` | POST | `human`/`ai_agent`/`mcp_client`/`developer_bot` | 创建任务，初始态 `submitted`；body 含 `tool`、`risk_level`、`params_digest`、`group_id`（HITL 卡片投递群）、`execution_actor` |
| `/api/v1/agent/tasks/:task_id` | GET | owner/admin/同群成员 | 读任务详情（状态、决定、结果摘要；不含敏感参数明文） |
| `/api/v1/agent/tasks/:task_id/actions` | POST | 见 §5 映射 | 提交状态动作 `{action, reason?}`；状态迁移按 FSM-00 矩阵校验 |
| `/api/v1/agent/tasks/:task_id/events` | GET | owner/admin/同群成员 | 进度事件流（`progress` 自环事件只读；实时通道走既有 SSE，见 TRACE-00 §6） |
| `/api/v1/agent/tasks/:task_id/approval` | POST | `human_approver` | `{decision: approve|reject, note?}`；即 FSM-00 边 #10/#11 |

创建时参数隐私：`params` 不落审计链（TRACE-00 §7）；API 只接收并为工具执行保
留参数，审计仅记 `params_digest`（SHA-256，hex）。

## 5. 动作到 FSM-00 矩阵的映射（冻结）

`POST /actions` 的 `action` 字段 ∈ FSM-00 动作全集，逐边映射：

| API action | FSM-00 边 | 授权判定 |
|---|---|---|
| `start` | #1 submitted→working | actor ∈ `agent_worker` 且登记为该任务执行体 |
| `progress` | #5 working 自环 | 同 `start`；body 可带 `progress_note`（不落链，只进实时事件） |
| `complete` | #7 working→completed | 同 `start`；body `result_digest` |
| `fail` | #3/#8/#15（服务端按当前状态选择唯一合法边） | `agent_worker` 或 `system` |
| `cancel` | #2/#9/#13 | actor ∈ `task_owner`/`admin` 映射（§2） |
| `need_approval` | #6 working→awaiting_approval | 同 `start`；body 必含 `approval_group_id` 与风险摘要 |
| `approve` / `reject` | #10/#11 | 仅经 `/approval` 端点；first-writer-wins |
| `resume` | #14 approved→working | 仅 `system`（平台调度器）；API 不暴露给外部调用，外部请求该动作一律 `forbidden_actor` |
| `expire` | #4/#12 | 仅 `system` 定时器；API 不暴露 |

**确定性与错误码**：同一 `(状态, 动作)` 目标状态唯一（FSM-00 §3）；未声明组
合一律拒绝，状态不变、无新副作用。

## 6. 错误码（冻结）

| 错误码 | HTTP | 场景 |
|---|---|---|
| `invalid_transition` | 409 | FSM-00 非法补集（含终态上的一切动作） |
| `already_decided` | 409 | 审批重复/反向（first-writer-wins 落败；state_change=false、无新副作用） |
| `forbidden_actor` | 403 | 平台 actor 不在该边的 FSM `allowed_actors` 映射内（含冒充 `system`/`resume`/`expire`） |
| `not_group_member` | 403 | 审批人非群内有权成员 |
| `self_approval_forbidden` | 403 | 审批人是任务所属 agent |
| `risk_level_disabled` | 422 | 创建/注册了 V1 未启用的 `financial` 工具 |
| `tool_unknown` | 422 | tool 未注册或未对该执行体授权 |
| `task_not_found` | 404 | task_id 不存在或无可见性 |
| `expired_task` | 409 | 对 expired 任务补审批/补动作（invalid_transition 的具象化，便于客户端提示） |

兼容策略：错误码只增不改义；客户端必须按字面匹配并保留未知码透传展示。

## 7. 工具风险等级（冻结）

| 等级 | 定义 | V1 策略 |
|---|---|---|
| `read` | 只读查询/检索，无外部副作用 | 可免审批直行：working→complete（边 #7，免审批执行挂 task，TRACE-00 §4.1） |
| `write` | 产生外部可见副作用（发消息、写记录、调外部系统） | **必须 HITL**：working→need_approval→(approve→resume) 才可执行；批准只执行一次 |
| `financial` | 资金/支付/转账语义 | **V1 不启用**：创建即 `risk_level_disabled`，无绕过路径；启用需本契约升版 |

规则：风险等级在工具注册时声明并由服务端权威判定，不信任任务创建方自报；
`write` 工具免审批直行视为 P0 违规。

## 8. 正例

```json
POST /api/v1/agent/tasks
{ "tool": "workspace.report.weekly", "risk_level": "read",
  "params_digest": "9f2a…c3（64 hex）", "execution_actor": "mcp_client" }
→ 201 { "task_id": "task-01h9x2v7c4b5a6988796a5b4", "status": "submitted",
        "correlation_id": "corr-0f1e2d3c4b5a69788796a5b4c3d2e1f0" }

POST /api/v1/agent/tasks/task-01h9x2v7c4b5a6988796a5b4/actions
{ "action": "complete", "result_digest": "77a0…1b" }
→ 200 { "status": "completed" }
```

审批流（write 工具）：`need_approval → (human)approve → 系统调度 resume → complete`；
重复 approve → `409 already_decided`。

## 9. 负例（默认拒绝）

- 对 `completed` 任务再 `complete` → `409 invalid_transition`（终态无出边）。
- `mcp_client` 直接调 `resume` → `403 forbidden_actor`。
- 请求体携带 `"actor": "human_approver"` 冒充审批人 → 服务端忽略该字段，按会话
  身份判定；非 human 会话 → `403 forbidden_actor`。
- `risk_level: "financial"` 创建 → `422 risk_level_disabled`。
- 审批人是任务所属 agent → `403 self_approval_forbidden`。
- 客户端在 header/body 提供 `correlation_id` → 丢弃并记安全日志（TRACE-00 §3.3），
  响应返回服务端值。

## 10. 现状差异（现有代码 vs 本契约）

| # | 现状 | 目标 | 兑现任务 |
|---|---|---|---|
| 1 | `agent_task_handler/demo` 驱动，无持久任务 API | 本契约端点 | DATA-01 |
| 2 | 审批决定存 ETS | 审批沿 FSM-00 边走持久化 + 本契约 `/approval` | DATA-01 |
| 3 | MCP task 工具词汇（submitted/working/…） | 经 MCP-02 映射到九状态矩阵 | MCP-02 |

## 11. 变更规约

本契约由 PDT-01 冻结。端点、actor 映射、错误码、风险等级策略变更必须升版本
并经 Coordinator 合并；实现任务发现缺口时回 PDT-01 走契约变更，不得在实现层
私扩端点或放宽授权。
