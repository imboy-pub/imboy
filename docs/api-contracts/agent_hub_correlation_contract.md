# Agent Hub correlation_id 全链路追踪契约（TRACE-00 冻结）

> 版本：1.0（冻结）| 任务卡：[TRACE-00](../planning/tasks/TRACE-00.md) | 主计划：[§1.1/§4/§7](../planning/ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
>
> 本文件是 Agent Hub 审计链的权威契约。机器可判定实现见
> `scripts/verify_agent_hub_correlation_trace.py`，测试见
> `test/scripts/test_verify_agent_hub_correlation_trace.py`，正反例见
> `test/fixtures/agent_hub/correlation/`。契约与 verifier 不一致时以本文件为准并修
> verifier。

## 1. 目的与范围

Agent Hub 的所有产品能力（内建 AI Agent、外部 MCP Agent、开发者 Bot、频道
Webhook、Agent Task/HITL）必须能按一条审计链重建"谁发起了什么、执行到哪一步、
结果如何"。本契约冻结：

1. 可信 `correlation_id` 的生成、透传与防覆盖规则。
2. 六实体（request/task/event/approval/execution/delivery）与链终点 outcome 的
   模型、ID 格式、父引用（必需边）与时间戳约束。
3. 各入口与执行通道（HTTP/MCP/SSE、异步 worker、重试、Bot reply、Webhook
   outbox、服务重启）的传播规则。
4. 日志/审计的隐私红线。
5. 审计导出 JSON 的机器格式（verifier 唯一输入）。

不在范围：E2EE 消息正文、业务状态机（FSM-00 冻结）、任务持久化 schema
（DATA-01）、Webhook 交付语义细节（WH-01/WH-02）。本契约只约束"链"的形状，
不约束业务字段的含义。

## 2. correlation_id 定义

| 属性 | 冻结值 |
|---|---|
| 字段名（日志/审计/JSON） | `correlation_id`（snake_case，全局唯一命名，禁止 `cid`/`trace_id`/`x-correlation` 等别名出现在链数据中） |
| JSON 类型 | 一律 `string`（禁止 number/integer；64-bit 安全） |
| 格式 | `^[A-Za-z0-9_-]{16,64}$` |
| 生成方 | 服务端，在**首个受信入口**（见 §3） |
| 生成算法（推荐） | `corr-` 前缀 + 16 字节 `crypto:strong_rand_bytes(16)` 的 hex（共 37 字符，落在 16–64 区间）；任何满足格式的服务端随机值均可 |
| 生命周期 | 从 request 创建起到 outcome 判定为止；跨进程、跨重启、跨重试不变 |
| 每链数量 | 恰好 1 个（重复根/第二条链=违规） |

内部服务间透传使用 HTTP header `x-imboy-correlation-id`（仅限已认证的内部跳
数）。该 header 来自外部客户端时**不读取、不透传、不落链**（见 §3.3）。

## 3. 可信入口与防覆盖

### 3.1 生成时机

服务端在**首个受信入口**生成 `correlation_id`，即请求通过认证（JWT 用户、
MCP client 凭证、incoming webhook token 验签、Bot API token）之后的第一个处
理点。未认证阶段不生成、不记录链实体。

### 3.2 透传边界

只允许**已认证内部上下文**透传 `correlation_id`：

- 同一服务内：进程字典/ctx map/GenServer 消息携带。
- 内部服务间：内部认证之上的 `x-imboy-correlation-id` header。
- 出站到外部系统（Bot webhook）：签名 payload 中包含 `correlation_id` 明文
 （外部系统在回包中回传时按外部输入处理，回传值不作为可信来源）。

### 3.3 客户端覆盖禁止（硬规则）

外部客户端以任何形式（header、query、JSON body、MCP 参数 `_meta`、Bot 回包）
提供的 `correlation_id` 同名字段值**不得覆盖**服务端可信值：

- 已有可信值时：外部值直接丢弃。拒绝事实记录到**安全日志**（非链导出），
  不产生链实体、不进入审计导出。
- 若防御失效导致外部值进入链：审计导出中出现 `client_override: true` 标记
  即 verifier 违规（退出码 2）。

### 3.4 断链处理

任何执行点找不到可继承的 `correlation_id`（如重启后恢复、消息丢失）时，必须
**显式失败**或产生**可告警缺口**（gap 告警 + 拒绝处理）；禁止静默生成新的
`correlation_id` 形成第二条链。持久化实体必须能通过自身 `correlation_id` 列
重新挂回原链。

## 4. 实体模型与必需边

### 4.1 六实体 + outcome

| entity_type | 职责 | 允许的 parent_entity_id 类型 | 说明 |
|---|---|---|---|
| `request` | 链根：一次受信入口调用（HTTP/MCP/SSE/webhook/Bot API） | 无（必须为 `null`） | 每 correlation 恰好 1 个 |
| `task` | Agent Task（含 MCP task tool 创建的任务） | `request` | |
| `event` | 任务/链事件（进度、状态变化、入站业务事件） | `request`, `task`, `event` | 入站 webhook 无 task 时可直接挂 request；事件流可级联 |
| `approval` | HITL 审批卡片/审批结果 | `task`, `event` | |
| `execution` | 工具/动作执行 | `approval`, `task` | 免审批执行挂 task；批准后执行挂 approval，批准只执行一次 |
| `delivery` | 出站 Webhook 交付（outbox） | `execution`, `event` | Bot mention 出站挂 event；执行结果交付挂 execution |
| `outcome` | 链的可判定终点（succeeded/failed/cancelled/expired…） | `execution`, `delivery` | 每 correlation **至多** 1 个；outcome 存在即链已判定 |

六实体各自保留独立 `entity_id`（TSID 或业务 ID，JSON 一律 string，格式
`^[A-Za-z0-9_-]{16,64}$`，与 correlation_id 同一格式约束）；继承同一
`correlation_id`。`entity_id` 在一份审计导出内全局唯一（跨 correlation 重复
= 违规，即"同一实体挂两条链"的机器判定）。

### 4.2 主链形状

```
request -> task -> event -> approval -> execution -> delivery -> outcome
                 \-> event(级联)                      \-> outcome(执行失败终点)
request -> event -> delivery   (Bot mention / 入站事件直连交付)
request -> task -> execution   (免审批执行)
```

verifier 不强制"链必须完整到 outcome"（进行中的链合法），但强制：凡出现的实
体，父引用必须按 §4.1 表闭合；完整链（六实体+outcome 全存在且合法）由
`test/fixtures/agent_hub/correlation/valid.json` 正例证明可重建。

## 5. 审计导出 JSON（verifier 输入，冻结）

顶层为 JSON 数组，每元素一条实体记录：

```json
[
  {
    "correlation_id": "corr-0f1e2d3c4b5a69788796a5b4c3d2e1f0",
    "entity_type": "request",
    "entity_id": "req-0123456789abcdef",
    "parent_entity_id": null,
    "timestamp": "2026-09-07T01:02:03.000000Z",
    "status": "accepted"
  }
]
```

字段白名单（只允许以下 7 个键，缺任一必填键或多任何键均违规）：

| 字段 | 类型 | 必填 | 约束 |
|---|---|---|---|
| `correlation_id` | string | 是 | `^[A-Za-z0-9_-]{16,64}$` |
| `entity_type` | string | 是 | §4.1 枚举之一 |
| `entity_id` | string | 是 | `^[A-Za-z0-9_-]{16,64}$` |
| `parent_entity_id` | string 或 null | 是 | request 必须为 null；其余必须为合法 entity_id 引用（同 correlation 内存在且类型符合 §4.1） |
| `timestamp` | string | 是 | UTC ISO-8601：`^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d{1,6})?Z$` |
| `status` | string | 是 | `^[a-z0-9_]{1,32}$`（业务值由 FSM-00/各契约定义，本契约只管形状） |
| `client_override` | boolean | 否 | `true` = 该记录的 correlation_id 来源被判定为客户端提供；**出现 true 即违规**（§3.3） |

时间戳约束：沿每条 parent→child 边**单调不减**（child ≥ parent，相等允许，
容差为 0）；时钟回拨即违规并告警。

## 6. 传播规则（按通道）

| 通道 | 规则 |
|---|---|
| HTTP API（JWT 用户） | 认证中间件通过后生成/继承 `correlation_id`，放入请求 ctx；handler→logic→ds→repo 全程透传；每条链实体落审计时带 `correlation_id` |
| MCP（JSON-RPC/SSE） | MCP 会话认证通过后的首个请求生成；同会话后续 tool call、SSE 事件、sampling/elicit 反向请求沿用；会话内每个 JSON-RPC `request_id` 仅做协议配对，**不**承担链职责（见 §8 兼容映射） |
| SSE 推送 | 事件帧继承所属链的 `correlation_id`；不因推送产生新链 |
| 异步 worker（task observer、outbox worker 等） | 派发消息必须携带完整 ctx（含 `correlation_id`）；worker 内禁止重新生成；找不到继承值=显式失败+告警缺口（§3.4） |
| 重试 | 同一 execution/delivery 的所有重试沿用同一 `correlation_id` 与同一实体 `entity_id`+幂等键（status 变化产生新 event/状态，不产生新链） |
| Bot reply（外部系统回调 IMBoy API） | 回包是**新入口**（新 request、新链），签名 payload 中回传的原 `correlation_id` 仅作关联线索存元数据，不并入原链、不覆盖新链 ID |
| Webhook outbox（出站） | outbox 行落库时持久化 `correlation_id` 列；投递、重试、死信、手工重放全程保留原值；delivery 实体引用原链 |
| 服务重启后 | 持久化实体按自身 `correlation_id` 重新挂回原链；恢复过程产生的新 event/approval/execution 必须挂在既有 `correlation_id` 上；找不到原值时显式失败+告警，禁止新建 request 根 |

## 7. 隐私红线（日志/审计）

链数据（本契约 §5 导出格式 + 运行时审计日志中的链字段）**只**允许记录：
`correlation_id`、各实体 `entity_id`/`entity_type`/`parent_entity_id`、
`timestamp`、`status`。

禁止进入链数据：secret/token/API key、消息正文（含 E2EE 密文与明文）、完整
URL（只允许记录 host 或路由名）、PII（用户账号、手机号、邮箱、IP 明文）。
verifier 以字段白名单（§5）+ 敏感字段黑名单（`body`/`content`/`payload`/
`secret`/`token`/`url` 等）双向强制：导出中出现即违规。

## 8. 与现有协议字段的兼容映射（不重命名）

现状（2026-09 基线 `rg "request_id|correlation" src/`）：`src/` 中无
`correlation` 字段；`request_id` 存在于 15 个文件，语义互不相同：

| 现有字段 | 位置 | 现语义 | 与 correlation_id 的关系 |
|---|---|---|---|
| MCP JSON-RPC `request_id` | `src/mcp/barrel_mcp_protocol.erl`、`barrel_mcp_session.erl`、`barrel_mcp_registry.erl`、`barrel_mcp_tasks.erl` | 单连接内 JSON-RPC 请求-响应配对 ID（integer/binary，`sampling-`/`elicit-`/`roots-` 前缀生成） | **并存，不重命名**：request_id 只做协议配对，作用域为一次 RPC；correlation_id 跨实体跨重启。MCP 链实体的 entity_id 不得复用 request_id |
| 业务 `request_id` | `channel_message_repo`、`workspace_repo`/`ds`/`handler`、`olm_identity_repo`、`channel_ds`、`channel_logic_message`、`olm_identity_logic`、`olm_handler`、`channel_handler`、`workspace_logic` | 各业务域的幂等/请求标识（入站 webhook 事件去重等） | **并存，不重命名**：业务 request_id 继续承担幂等去重；correlation_id 是新增独立字段（新列/新键），实现任务（DATA-01/WH-01/WH-02/MCP-01）落库时新增，不迁移现有列 |

规则：本契约不批量重命名任何现有 `request_id`；实现任务引入链字段时一律新键
`correlation_id`。日志中同一条记录同时出现两者时字段名不得混用。

## 9. verifier 判定语义（冻结）

`scripts/verify_agent_hub_correlation_trace.py <audit-export.json>`：

- **0（OK）**：全部检查通过；stdout 输出 JSON 摘要（含每类实体计数与
  correlation 数）。
- **2（VIOLATION）**：任一违规——文件不可读/非 JSON 数组/记录结构非法/字段
  越界（含敏感字段）、客户端覆盖、无 request 根、重复根、孤儿记录（parent
  不存在于同 correlation）、父类型非法、自引用、entity_id 全局重复、
  重复 outcome、时间回拨。stdout 输出 `{"decision":"VIOLATION","errors":[...]}`
  （错误码只含字段路径与违规类型，不含字段值）。
- **1**：用法错误（参数个数不为 1）。

测试固定正负例：合法链、孤儿 delivery、双根、客户端覆盖、时间倒序、断链
（task 无 request 父）等，见 `test/scripts/test_verify_agent_hub_correlation_trace.py`。

## 10. 变更控制

本契约为 TRACE-00 冻结产物。后续任务（DATA-01/MCP-01/WH-01/WH-02/BOT-01/
E2E-01 等）必须引用本文件的字段与规则，不得私改格式；需要扩展字段时由
Coordinator 在本文件升版并同步 verifier 与测试，旧导出必须持续可判定。
