# 群 AI 助手运营手册 / Group AI Agent Ops Manual

> **版本 / Version**: 1.0.0 | **最后更新 / Last Updated**: 2026-07-24
> **适用范围 / Scope**: P0-3 AI 社群管家（群内 @管家 答疑）
> **前置 / Prerequisites**: 已注册至少一个 `account_type=1` 的 AI 助手账号（见「AI 助手管理」）
> **关联代码 / Related code**: `src/logic/ai_agent_group_reply.erl`、`src/lib/agent_trigger_policy.erl`、`ai_agent.trigger_policy` jsonb（迁移 27）

---

## 0. 工作原理（先读这节）

人在群里 **@某个 AI 助手** → 该助手据 `trigger_policy` 判定是否回复 → 过限流闸门 → 异步走 LLM → 回复作为一条正常群消息回群（助手是一等群成员）。

**三条不可逾越的红线 / Hard rules**：

1. **E2EE 群消息一律不触发**。端到端加密群里的 @ 不会把消息送给模型——服务端不解密做 AI。要让 @管家 生效，群必须是**非 E2EE** 普通群。
2. **助手不会自触发**。助手发的消息不再 @ 助手，避免 agent↔agent 死循环。
3. **限流闸门**。同一助手对同一用户有频率上限（`agent_rate_limiter`，防金钱 DoS），超限静默丢弃并在日志打 `AGENT_GROUP_RATE_LIMITED`。

---

## 1. 建群与拉助手入群 / Create group & add agent

1. **建群**：通过客户端或管理后台创建一个普通群（**非 E2EE**），记下 `group_id`。
2. **拉助手入群**：把目标 AI 助手账号（`account_type=1`）作为**普通成员**加入该群。助手必须是群成员，回复才会以群成员身份发出。

> 验证入群：在群里发一条 `@助手名 你好`，看助手是否进入触发判定（见第 4 节）。若助手根本不在群成员列表，则永远不会被触发。

---

## 2. 配置触发策略 trigger_policy / Configure trigger policy

`trigger_policy` 是 `ai_agent` 表的 jsonb 列（迁移 `00000027_ai_agent.up.sql`）。字段：

| 键 / Key | 类型 | 默认 / Default | 含义 |
|----------|------|----------------|------|
| `mention` | boolean | `true` | 被 @ 时触发（**安全默认开启**） |
| `suffix_q` | boolean | `false` | 文本以 `?` / `？` 结尾时触发 |
| `keywords` | string[] | `[]` | 文本包含任一关键词时触发 |
| `group_allowlist` | integer[] | `[]` | 非空时**仅在这些群**触发；空 = 不限群 |

判定逻辑（`agent_trigger_policy:should_trigger/2`）：先过 `group_allowlist` 群白名单门，再 `mention OR suffix_q OR keywords` 任一命中即放行。

> ⚠️ **管理后台 UI 暂未暴露 trigger_policy 编辑**（「AI 助手管理」编辑对话框目前不含此字段）。当前两种配置方式：

### 方式 A：管理后台 API（推荐 / Recommended）

```bash
# 业务库为 imboy_v1（本地 docker imboy_pg18:4323；生产以实际为准）
curl -X POST 'https://<your-host>/api/adm/ai_agent/update' \
  -H 'Cookie: <admin session>' \
  -H 'Content-Type: application/json' \
  -d '{
    "user_id": <助手 user_id>,
    "trigger_policy": {
      "mention": true,
      "keywords": ["退款", "发货", "密码"],
      "group_allowlist": [<group_id>]
    }
  }'
```

### 方式 B：直接改库 / Direct SQL（应急或批量）

```sql
-- 仅在指定群触发，且 @ 或命中关键词都回
UPDATE ai_agent
SET trigger_policy = '{"mention": true, "keywords": ["退款","发货"], "group_allowlist": [<group_id>]}'
WHERE user_id = <助手 user_id>;

-- 全群通用、仅 @ 才回（最保守）
UPDATE ai_agent
SET trigger_policy = '{"mention": true}'
WHERE user_id = <助手 user_id>;
```

---

## 3. 推荐配置模式 / Recommended presets

| 场景 | trigger_policy |
|------|----------------|
| 客服群：只在指定群、@ 或关键词都回 | `{"mention": true, "keywords": ["退款","发货","密码"], "group_allowlist": [<gid>]}` |
| 闲聊群：仅 @ 才回（防刷屏，最保守） | `{"mention": true}` |
| 全站通用助手：所有群、@ 或疑问句都回 | `{"mention": true, "suffix_q": true}` |

---

## 4. 触发验证 / Verify triggering

在目标群里：

1. **@ 触发**：发 `@助手 你好` → 助手应在数秒内回群。
2. **关键词触发**（若配了 keywords）：发包含关键词的普通消息（不 @）→ 助手应回复。
3. **白名单负向**：若设了 `group_allowlist`，在**不在名单**的群里 @ → 助手**不应**回复。

> 群规/FAQ 注入：在「AI 知识库」页（`/ai-agents/knowledge`）粘贴群规与 FAQ 并开启总开关，助手回复会自动带上这些上下文（仅对非 E2EE 群生效）。

---

## 5. 停用与启用 / Disable & re-enable

```bash
# 停用（status=0）：助手不再回复，也不进助手广场
curl -X POST 'https://<your-host>/api/adm/ai_agent/set_status' \
  -H 'Cookie: <admin session>' -H 'Content-Type: application/json' \
  -d '{"user_id": <助手 user_id>, "status": 0}'

# 启用（status=1）
# ... "status": 1
```

或管理后台「AI 助手管理」列表行的停用/启用开关。

> 停用是**软停用**：账号与历史消息保留，仅不再触发回复。`trigger_policy` 配置不动，重新启用即恢复。

---

## 6. 排障 / Troubleshooting

| 现象 / Symptom | 排查 / Check |
|----------------|--------------|
| @ 了助手但完全不回 | ① 助手是否在该群成员里？② 群是否 E2EE？（E2EE 群永不触发）③ `status` 是否=1？④ `trigger_policy.group_allowlist` 是否漏了本群？ |
| 偶尔回不了 | 查日志 `AGENT_GROUP_RATE_LIMITED`——被限流闸门挡（同一助手对同一用户频率过高），属正常防刷屏。 |
| 关键词不生效 | `keywords` 是**子串匹配**（`binary:match`），注意大小写与全/半角；中文关键词直接写原文。 |
| 助手回复不带群规 | 「AI 知识库」页 `enabled` 是否开？群规/FAQ 是否已保存？ |
| 改了 trigger_policy 不生效 | API/SQL 改完后，`ai_agent` 行有缓存（depcache `{Table, Id}`）；必要时重启节点或清缓存让路由侧读到新策略。 |
