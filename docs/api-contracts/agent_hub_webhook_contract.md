# Agent Hub Webhook 契约（PDT-01）

> 版本：1.0（冻结）| 任务卡：[PDT-01](../planning/tasks/PDT-01.md) | 主计划：[§7](../planning/ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
>
> 引用真源：链字段以 [agent_hub_correlation_contract.md](./agent_hub_correlation_contract.md)（TRACE-00）为准；
> 交付重试/死信/重放语义由 WH-01 实现。本文件固定协议形状与安全边界；冲突时以
> 本文件为准修实现。

## 1. 目的与范围

固定两类 webhook 的协议契约：

- **出站 Bot Webhook**（IMBoy → 开发者系统）：Bot mention/事件与 Agent 执行结
  果交付（BOT-01 路由、WH-01 outbox 可靠交付）。
- **入站 Channel Webhook**（外部系统 → IMBoy 频道）：token 摘要与频道锁定
  （WH-02）。

不在范围：状态机（FSM-00）、task API（agent_hub_task_api_contract.md）、MCP
客户端（agent_hub_mcp_client_contract.md）。

## 2. 出站 Bot Webhook

### 2.1 请求形状（冻结）

IMBoy 以 `POST`（HTTPS only）投递 JSON；headers：

| Header | 值 |
|---|---|
| `Content-Type` | `application/json` |
| `User-Agent` | `IMBoy-Webhook/1.0` |
| `X-IMBoy-Delivery` | delivery 实体 `delivery_id`（TRACE-00 §4.1；string，同 ID 格式） |
| `X-IMBoy-Event` | 事件类型（§2.3 枚举） |
| `X-IMBoy-Timestamp` | Unix 秒（签名时刻） |
| `X-IMBoy-Signature` | `sha256=<hex>`（§2.2） |

payload 顶层固定键：

```json
{ "event": "bot.mention", "delivery_id": "dlv-…", "correlation_id": "corr-…",
  "bot_id": "bot-…", "occurred_at": "2026-09-07T12:00:00.000000Z", "data": { } }
```

`data` 内为业务字段；`correlation_id` 为明文回传线索（TRACE-00 §3.2/§6：回包
按外部输入处理，不作可信来源）。`data` 禁止包含消息 E2EE 密文或明文正文、
secret、完整 URL（只允许 host/路由名）；违规即实现缺陷。

### 2.2 签名（冻结）

签名原文（signature base string）为两行 ASCII，`\n` 分隔：

```
<X-IMBoy-Timestamp 的十进制字符串>
<原始请求体字节（不解析、不重序列化）>
```

`X-IMBoy-Signature = "sha256=" + HMAC-SHA256(key=bot webhook secret, msg=原文)`
 的小写 hex。验证方（开发者系统）必须：先验签再解析 body；拒绝缺失 timestamp
或签名的请求。

### 2.3 重放时间窗与事件类型（冻结）

- **重放窗口**：接收方应拒绝 `|now − X-IMBoy-Timestamp| > 300` 秒的请求；IMBoy
  侧重试投递一律重新签名、更新 timestamp（body 中 `delivery_id` 不变）。
- **事件类型枚举（V1）**：`bot.mention`、`bot.message`、`bot.added`、
  `bot.removed`、`agent.task.completed`、`agent.task.failed`、
  `agent.task.awaiting_approval`。未知类型接收方可忽略（向前兼容）；IMBoy 不
  发送枚举外类型。

### 2.4 reply context（冻结）

Bot 需要回复时，IMBoy 在 `data.reply_context` 内携带**签名不透明令牌**：包含
来源会话（chat/channel 标识）、消息游标与过期时间（≤ 15 分钟），由服务端签名。
Bot 回调 IMBoy API 时原样回传该令牌；服务端验签后映射到来源会话。令牌解析失
败/过期/签名不符一律 `401 reply_context_invalid`，不泄露内部结构。Bot 回包是
新入口、新链（TRACE-00 §6 Bot reply 行）。

### 2.5 出站安全边界：IP pinning 与 redirect 逐跳策略（冻结）

对每一条 delivery（WH-01 outbox 落库时）：

1. **解析一次并固定**：投递创建时解析目标 host 得到 IP 清单，选定 IP **pin**
   在 outbox 行；后续该 delivery 的所有重试只连 pinned IP（TLS SNI/Host 仍用
   原域名），DNS TTL 变化不触发重新解析（防 DNS rebinding）。
2. **私有网段拒绝**：pinned IP 落在 RFC1918/loopback/link-local/组播/保留段
   一律拒绝（`destination_forbidden`），创建时即拒绝而非投递时失败。
3. **redirect 逐跳策略**：默认**不跟随** redirect（3xx 视为失败
   `redirect_not_followed`，进入重试/死信）。若后续启用跟随，必须逐跳满足：
   仅 HTTPS、同注册域名后缀、每跳重新做私网判定与重新 pin、重签新跳的请求、
   最多 1 跳；任一不满足即停。V1 按"不跟随"实现。
4. TLS 证书校验开启（不禁用校验、不接受自签名，除非该 bot 凭证显式登记指纹）。

### 2.6 错误码（接收方→IMBoy 的投递判定）

| 结果 | 判定 |
|---|---|
| 2xx | 成功（delivery 成功一次；重复 2xx 按幂等成功处理） |
| 410 | 永久失效：dead，不再重试 |
| 其他 4xx | 失败；不重试（配置错误），dead + 原因留档 |
| 5xx / 超时 / TLS / 连接失败 | 失败；按 WH-01 退避重试，重试耗尽 dead |

重放（手工/管理触发）复用同一 `delivery_id` 与幂等键（TRACE-00 §6 重试行）。

## 3. 入站 Channel Webhook（WH-02 契约面）

| 项 | 冻结值 |
|---|---|
| 凭证 | channel webhook token；**存储只允许 SHA-256 摘要**，明文仅在创建响应返回一次 |
| 认证 | `Authorization: Bearer <token>` 或 `X-IMBoy-Webhook-Token`；服务端摘要比对 |
| 轮换 | rotate 后旧 token 立即失效（摘要删除）；rotate 不改变频道锁定 |
| 频道锁定 | token 绑定单一 channel_id；跨频道重放有效（403 `channel_mismatch`） |
| 幂等 | body `request_id`（业务幂等键，TRACE-00 §8 并存语义）去重，重复请求返回首次结果 |
| 频率 | 复用现有 per-token 限流能力；超限 `429` 可观测，不影响其他 token |

入站请求不产生 `delivery`；产生的链实体为 `request`（根）+ `event`（入站业务
事件，可直挂 request，TRACE-00 §4.1）。

## 4. 正例 / 负例

正例：开发者系统收到 `X-IMBoy-Timestamp: 1788801600`、body 原始字节，本地以
同一 secret 计算 HMAC 与 `X-IMBoy-Signature` 比对通过、时间差 5 秒 → 处理
`bot.mention`。

负例（IMBoy 出站侧全部在创建/投递时拒绝）：
- 目标 `http://` → 拒绝（HTTPS only）。
- 目标解析到 `127.0.0.1`/`10.0.0.0/8`/`169.254.169.254` → `destination_forbidden`。
- 目标 302 到另一域名 → `redirect_not_followed`（V1 不跟随）。
- 重放窗口外旧 timestamp 的入站回包 → 接收方拒绝；IMBoy 回调验签失败 → `401`。

## 5. 现状差异（现有代码 vs 本契约）

| # | 现状 | 目标 | 兑现任务 |
|---|---|---|---|
| 1 | bot webhook 事件即发即忘（无 outbox 重试/死信） | §2.6 可靠交付 | WH-01（migration 92） |
| 2 | 出站无 IP pin/redirect 策略 | §2.5 | WH-01 |
| 3 | channel webhook token 校验路径存在，摘要化与 rotate 面不全 | §3 | WH-02（migration 93） |
| 4 | reply context 无签名令牌 | §2.4 | BOT-01 |

## 6. 变更规约

headers、签名原文、窗口时长、事件枚举、IP pin/redirect 策略为冻结项；扩展事
件类型只允许追加（接收方忽略未知）；破坏性变更必须升版并经 Coordinator 合并。
