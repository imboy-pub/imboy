# 账号与主体（Accounts and Actors）

> Purpose：定义 IMBoy 里「谁在行动」。三端所有权限、消息、加密、计费模型都建立在这套主体分类上。
> 术语定义见[术语表 · 账号与身份](../glossary.md#1-账号与身份)。

## Concept：四类账号 + 两个体系外主体

IMBoy 的 `user` 表承载四类账号主体，由 `user.account_type` 区分（迁移 27 定义、70 定稿）：

| account_type | 主体 | 中文 | 能力要点 |
|---|---|---|---|
| 0 | Human | 真人 | 登录、好友、入群、拥有组织/工作区/项目、充当 Grant 委托人 |
| 1 | Agent | 智能体 | 平台 AI 账号（`ai_agent` 表：provider/model/角色/触发策略），可被 @ 与入群 |
| 2 | System Bot | 系统机器人 | 平台内置服务账号；现行唯一形态是频道 Webhook Bot（`bot_uid=account_type 2`） |
| 3 | Bot | Bot | 开发者第三方服务（`bot` 表：`@调用名`、API Token 摘要、Webhook outbox 重试交付） |

体系外主体（不在 `user` 表）：

- **平台管理员（Platform Admin）**：`adm_user`/`adm_role` 独立账号域，cookie 会话 + RBAC，只经 `/api/adm/*` 行动。
- **访客（Visitor）**：客服外部咨询者，凭门店密钥签发的访问令牌接入，不建 user 行、不占 License 配额（客服系统裁决，见 `docs/plans/2026-09-13-customer-service-system-design.md`）。
- **业务身份（Business Identity）**：组织的稳定经办主体（不可登录、不发 JWT），见[客服域](./customer-service.md)与[企业业务域](./enterprise-business.md)。

## Current

- 账号类型枚举与上表一致（`include/` 与迁移 70）；客户端本地 `contact.account_type` 同步为 `0 真人 / 1 AI / 2 官方`（imboyapp SQLite v23 起三值；类型 3 Bot 在客户端仅体现为徽章语义，无独立 Bot 域模型）。
- 智能体是**一等 user**：迁移 71 为存量 LLM provider 生成默认 Agent 账号（ID 高位偏移段），admin 端 `/ai-agents` 管理面走 `ai_agent_*` 端点。
- 在线状态：真人与智能体共用 `syn` presence（`ai_agent_runtime` 把智能体注册为在线）。
- 删除约束：组织 active owner 的 user 行被数据库拒绝删除（迁移 126/127 不变量）；企业业务 active 经办人的 user 行同理（迁移 114 CHECK）。

## Contract（跨端稳定约定）

1. `account_type` 枚举值是三端契约，新增类型必须三端同步。
2. 「Bot 与智能体是两套体系」：`bot_*` 表服务 account_type=3，`ai_agent`/`agent_*` 表服务 account_type=1，文档与代码不得互用。
3. 客户端 UI 显示名「AI 助手」对应本文「智能体（Agent）」。

## Constraints

- 关键治理角色（组织所有者、Grant 委托人）仅允许真人（`account_type=0`），由数据库触发器与领域层双重守卫。

## References

- 迁移：`00000027`、`00000070`（账号类型定稿）、`00000071`（Agent 账号回填）
- 客户端：imboyapp `lib/store/model/`（contact 模型）、`lib/component/ui/bot_badge.dart`
- 管理端：imboyadmin `src/modules/ai_agent`、`src/modules/bots`
