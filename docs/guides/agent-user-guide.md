# AI 助手（Agent）用户指南 / AI Agent User Guide

> **最后更新 / Last updated**: 2026-08-21 | **版本**: 1.0

---

## 什么是 Agent / What is an Agent

Agent 是 IMBoy 平台内置的 AI 助手账号（`account_type=1`），由 LLM（大语言模型）驱动，可以像真人用户一样参与对话。Agent 与普通用户的区别在于：

- Agent 有独立的账号（`user` 表，`account_type=1`）和 AI 配置（`ai_agent` 表）
- Agent 收到 C2C 消息后自动调用 LLM 生成回复
- Agent 可以配置不同的 provider（如 ark、bailian）、模型、system_prompt 和行为策略

---

## 发现 Agent / Discovering Agents

Agent 发现 API 均需 JWT 认证。

### 公开发现（分页）

```bash
GET /api/v1/agent/discover?page=1&size=20
```

返回启用且公开可发现的 Agent 列表（`ai_agent.status=1` 且 `visibility=1` 且 `user.status=1`）。

### 关键词搜索

```bash
GET /api/v1/agent/search?q=助手&page=1&size=20
```

按昵称模糊匹配搜索 Agent。

### 分类列表

```bash
GET /api/v1/agent/categories
```

返回所有启用且公开的 Agent 分类（去重，非空，按字母序）。

### 响应格式

```json
{
  "errcode": 0,
  "data": {
    "total": 5,
    "page": 1,
    "size": 20,
    "list": [
      {
        "id": 1000000000000000001,
        "name": "Ark 助手",
        "avatar": "...",
        "description": "Ark 平台 AI 助手"
      }
    ]
  }
}
```

---

## 与 Agent 对话 / Chatting with an Agent

Agent 使用 C2C（一对一私聊）消息通道。用户向 Agent 账号发送普通 C2C 消息即可触发 AI 回复。

### 流程

1. 通过发现 API 获取 Agent 的 user_id
2. 建立会话（客户端自动创建）
3. 发送 C2C 消息（`msg_type: text`）
4. 服务端落库后自动调用 LLM → Agent 回复以 C2C 消息返回

### 注意事项

- 仅支持 `text` 类型消息触发（其他类型静默忽略）
- 群内 @Agent（C2G 触发）不在当前支持范围
- 每个用户有速率限制（共享限流器），超限后消息静默丢弃

---

## Agent 配置 / Agent Configuration

Agent 通过管理后台（`/api/adm/ai_agent/`）配置，关键字段：

| 字段 | 类型 | 说明 |
|------|------|------|
| `provider` | string | LLM 提供商（如 `ark`、`bailian`） |
| `model` | string | 模型名称（如 `doubao-lite-4k`、`qwen3.7-flash`） |
| `system_prompt` | text | 系统提示词，定义 Agent 行为 |
| `visibility` | int | `0`=私有（仅 owner 可发现）、`1`=公开可发现 |
| `category` | string | 分类标签（用于发现 API 筛选） |
| `greeting` | text | 首次对话问候语 |
| `temperature` | float | LLM 生成温度（0-1） |
| `owner_uid` | int | 拥有者用户 ID（`0`=平台官方） |

---

## 历史 / History

- **2026-08-21**：废弃 `bot_*` 前缀，Agent C2C 成为唯一入口（`msg_c2s_logic.erl` 返回 `bot_prefix_deprecated`）
- **2026-08-21**：为 ark、bailian 创建默认 Agent（迁移 00000071）
- **2026-08-21**：Agent 公开发现 API 上线（`/api/v1/agent/discover`、`/api/v1/agent/search`、`/api/v1/agent/categories`）