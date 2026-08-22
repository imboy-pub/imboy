# Bot 开发者指南 / Bot Developer Guide

> **最后更新 / Last updated**: 2026-08-21 | **版本**: 1.0

---

## 什么是 Bot / What is a Bot

Bot 是 IMBoy 平台上的开发者服务账号（`account_type=3`），通过 Webhook 接收消息并响应。Bot 与 Agent 的区别：

- **Agent**（`account_type=1`）：平台内置 AI 助手，走 LLM 调用链路，无需开发者维护
- **Bot**（`account_type=3`）：开发者注册的第三方服务，走 Webhook 推送，完全自定义逻辑

> ⚠️ 注意：`account_type=2` 是频道 incoming webhook bot（`channel_webhook_ds` 创建），**不是**开发者 Bot。判定 Bot 须同时检查 `account_type=3` 且 `bot` 表存在该 user_id 行。

---

## 注册 Bot / Registering a Bot

### API

```bash
POST /api/v1/bot/register
Content-Type: application/json
Authorization: Bearer <JWT>

{
  "name": "我的 Bot",
  "username": "my_bot",
  "description": "这是一个示例 Bot",
  "webhook_url": "https://example.com/webhook",
  "commands": ["/help", "/ping"],
  "events": ["message"],
  "is_public": true
}
```

注册成功后返回：
```json
{
  "errcode": 0,
  "data": {
    "bot_id": "1234567890123456",
    "api_token": "bot_xxxxx",
    "verify_token": "vfy_xxxxx"
  }
}
```

- `api_token`：Bot 调用 IMBoy API 的凭证（**请妥善保管**）
- `verify_token`：Webhook 推送验签 token（与 `api_token` 职责分离）

### 约束

- `username` 全局唯一，用于 @botname 提及
- 注册后自动创建 `account_type=3` 的 user 账号
- 一个开发者可注册多个 Bot
- Bot 注册后默认为启用状态（`status=1`）

---

## Webhook 消息推送 / Webhook Message Push

当用户给 Bot 发送 C2C 私聊消息时，IMBoy 会异步推送消息到 Bot 的 `webhook_url`。

### 推送格式

```
POST {webhook_url}
Content-Type: application/json
X-IMBoy-Signature: sha256=xxx
```

```json
{
  "event": "message",
  "from": {
    "user_id": "1234567890",
    "nickname": "张三"
  },
  "chat": {
    "type": "c2c",
    "chat_id": "c2c:100:200"
  },
  "message": {
    "msg_id": "msg_xxxxx",
    "msg_type": "text",
    "text": "你好，Bot！"
  }
}
```

### 签名验证

Webhook 请求携带 `X-IMBoy-Signature: sha256=<hex>` 头，使用 `verify_token` 作为密钥进行 HMAC-SHA256 签名：

```python
import hmac, hashlib

def verify_signature(payload: bytes, signature: str, verify_token: str) -> bool:
    expected = hmac.new(
        verify_token.encode(),
        payload,
        hashlib.sha256
    ).hexdigest()
    return hmac.compare_digest(f"sha256={expected}", signature)
```

### 推送策略

- 异步执行（`elib_async`），不阻塞消息主路径
- HTTP 超时 5 秒
- 失败仅记日志，不重投（离线 Bot 消息不暂存）
- 本期仅覆盖 C2C 私聊（用户 ↔ Bot 一对一），群内 @Bot 不在本期

---

## Bot API 参考 / Bot API Reference

管理类端点（注册/更新/启停/搜索）使用**用户 JWT** 认证（Bot 开发者本人）。
`api_token` 用于 **Bot 服务器** 调用的端点（无用户 JWT），见下方「发送消息」。

### 发送消息（Bot 服务器调用，api_token 认证）

```bash
POST /api/v1/bot/send_message
Content-Type: application/json
Authorization: Bearer <api_token>

{
  "to_uid": "200",
  "msg_type": "text",
  "payload": { "text": "你好，这是 Bot 的回复" }
}
```

返回：

```json
{
  "errcode": 0,
  "data": { "msg_id": "m1234567890" }
}
```

**防护规则**：

- **限流**：`agent_rate_limiter` 以 bot_id 为 scope 限流，超限返回错误
- **会话前置校验**：仅允许回复与 Bot 有过消息往来的用户（`msg_c2c` 双向任一即算，Telegram started-chat 范式）。用户未先发起对话时返回「用户未与 Bot 建立会话，不可主动发送」——防止 Bot 骚扰任意用户

### 管理端点（用户 JWT 认证）

#### 获取 Bot 信息

```bash
GET /api/v1/bot/get?bot_id=xxx
```

#### 更新 Bot（仅属主）

```bash
POST /api/v1/bot/update
Content-Type: application/json

{
  "bot_id": "xxx",
  "name": "新名称",
  "description": "新描述"
}
```

#### 启用/停用（仅属主）

```bash
POST /api/v1/bot/enable
POST /api/v1/bot/disable
```

#### 列出我的 Bot

```bash
GET /api/v1/bot/list_mine?page=1&size=20
```

#### 搜索 Bot（注册表）

```bash
GET /api/v1/bot/search?q=关键字&page=1&size=20
```

---

## Bot 注册表 vs GitOps 插件市场

| 维度 | Bot 注册表 | GitOps 插件市场 |
|------|-----------|----------------|
| 范围 | 本实例内 Bot 元数据 | 跨实例分发渠道 |
| 内容 | 身份、凭证、OAuth、事件订阅 | 安装包元数据、版本 |
| 检索 | `bot.is_public` + search API | `index.json` 目录 |
| 关系 | 运行时注册 | 分发渠道 |

两者互补不替代：`bot` 表 + search 仅做本实例注册表检索，不承担跨实例分发职责。

---

## Bot 权限与 OAuth / Bot Permissions & OAuth

### Bot OAuth Grant

用户授权 Bot 代表自己操作：

```json
{
  "bot_id": "xxx",
  "user_id": "xxx",
  "scopes": ["send_message", "read_profile"],
  "access_token": "oauth_xxx",
  "expires_at": "2026-12-31T00:00:00Z"
}
```

- `UNIQUE(bot_id, user_id)`：同一用户重新授权须复用同一行（UPDATE 而非 INSERT）
- 撤销时设置 `revoked_at` 和 `status=0`

### 安全原则

- `api_token` 和 `verify_token` 职责分离：`api_token` 用于 Bot 调 IMBoy API，`verify_token` 用于 Webhook 推送验签
- Webhook 推送只发签名（`X-IMBoy-Signature`），不发 token——接收方即 verify_token 持有者（Slack 同款范式）

---

## 历史 / History

- **2026-08-21**：Bot 基础设施上线（迁移 00000070），含 `bot` 表 + `bot_oauth_grant` 表 + 7 个 API 端点
- **2026-08-21**：Bot Webhook 推送模块上线（C2C 场景，异步，HMAC-SHA256 签名）
- **2026-08-22**：`POST /api/v1/bot/send_message` 上线（api_token 认证 + 限流 + 会话前置校验）；管理端点补属主校验；Webhook 推送接入 5s 超时