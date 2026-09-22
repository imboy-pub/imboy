# Internal API v1 — 端点参考（23 端点）

> 与代码冻结表（`src/api/enterprise_internal_routes.erl`）逐条一致；
> 字段级 schema 见 `.contract/api/openapi.yaml`（`api_internal` 组）。
> 认证、限流桶、幂等、`sender_mode` 语义见 [README.md](./README.md)。

图例：幂等 `—`=不需要 / `K`=必须带 `Idempotency-Key` / `code`=一次性 code。

## 应用与凭证

| ID | 方法与路径 | Scope | 限流桶 | 幂等 |
|---|---|---|---|---|
| INT-01 | `GET /api/internal/v1/application` | `application:read` | read | — |

读本应用信息（名称、组织、授予的 scope、状态）。健康检查与联调首选。

## 外部身份映射（OA 用户 ↔ 平台用户）

| ID | 方法与路径 | Scope | 限流桶 | 幂等 |
|---|---|---|---|---|
| INT-02 | `PUT /api/internal/v1/identity-mappings` | `identities:write` | write | K |
| INT-03 | `POST /api/internal/v1/identity-mappings/resolve` | `identities:read` | read | — |
| INT-15 | `DELETE /api/internal/v1/identity-mappings` | `identities:write` | write | K |
| INT-16 | `POST /api/internal/v1/identity-mappings/directory` | `identities:read` | read | — |
| INT-17 | `POST /api/internal/v1/directory/users` | `identities:read` | read | — |

- **绑定（INT-02）/ 解绑（INT-15）**：把 OA 侧用户标识绑定到平台用户；解绑后
  该用户对你的消息/群操作立即不可达（`identity_not_mapped`）。
- **解析（INT-03）**：OA 标识 → 平台用户；未映射返回 `identity_not_mapped`（422）。
- **目录（INT-16/17）**：查询本组织已映射/可映射用户（分页，只回元数据）。

## 企业群组

| ID | 方法与路径 | Scope | 限流桶 | 幂等 |
|---|---|---|---|---|
| INT-04 | `POST /api/internal/v1/groups` | `groups:write` | write | K |
| INT-05 | `PUT /api/internal/v1/groups/{group_id}/members` | `groups:write` | write | K |
| INT-06 | `DELETE /api/internal/v1/groups/{group_id}/members` | `groups:write` | write | K |
| INT-18 | `GET /api/internal/v1/groups/{group_id}` | `groups:write` | read | — |
| INT-19 | `PATCH /api/internal/v1/groups/{group_id}` | `groups:write` | write | K |
| INT-20 | `PUT /api/internal/v1/groups/{group_id}/members/roles` | `groups:write` | write | K |
| INT-21 | `DELETE /api/internal/v1/groups/{group_id}` | `groups:write` | write | K |

成员操作按 `user_id` 精确定位（必须是本组织已映射用户）；跨组织 `group_id`
一律 `resource_not_found`（平台侧不区分「不存在」与「不归你」）。

## 企业文件（直传）

| ID | 方法与路径 | Scope | 限流桶 | 幂等 |
|---|---|---|---|---|
| INT-07 | `POST /api/internal/v1/files/presign` | `files:write` | write | K |
| INT-08 | `POST /api/internal/v1/files/confirm` | `files:write` | write | K |
| INT-22 | `POST /api/internal/v1/files/governance` | `files:write` | write | K |

两段式直传：`presign` 取对象存储预签名 → 你方直传文件 → `confirm` 确认入库；
`governance` 为治理动作（如生命周期/元数据管理）。

## 消息（`sender_mode` 见 README §6）

| ID | 方法与路径 | Scope（动态） | 限流桶 | 幂等 |
|---|---|---|---|---|
| INT-09 | `POST /api/internal/v1/messages/direct` | `messages:send` 或 `messages:send_as_human` | write | K |
| INT-10 | `POST /api/internal/v1/groups/{group_id}/messages` | 同上 | write | K |

- INT-09 单聊直发；INT-10 群发到企业群。
- 请求显式声明 `sender_mode=application | human`：
  `application` 消耗 `messages:send`；`human` 消耗 `messages:send_as_human`
  且必须指定本组织已映射、已启用的目标发送者（不接受别名参数，平台侧强制校验）。
- 消息为**企业托管、固定非端到端加密**；重复的 `Idempotency-Key` 返回首次结果。

## 好友申请（只发起，平台不自动通过）

| ID | 方法与路径 | Scope | 限流桶 | 幂等 |
|---|---|---|---|---|
| INT-11 | `POST /api/internal/v1/friend-requests` | `friend_requests:create` | write | K |

仅可对**同组织已映射用户**发起，发送者形态固定为 `human`（代 OA 用户）。
平台**没有**自动通过/确认、删除好友、批量加好友的 API。

## Webhook（出站回调）

| ID | 方法与路径 | Scope | 限流桶 | 幂等 |
|---|---|---|---|---|
| INT-12 | `PUT /api/internal/v1/webhook` | `webhooks:manage` | write | K |
| INT-13 | `POST /api/internal/v1/webhook/deliveries/{delivery_id}/replay` | `webhooks:manage` | write | K |
| INT-23 | `GET /api/internal/v1/webhook/deliveries` | `webhooks:manage` | read | — |

登记/更新回调地址；投递状态分页查询；失败投递可按 `delivery_id` 重放。
你方回调端将收到 `x-imboy-delivery / x-imboy-event / x-imboy-timestamp /
x-imboy-signature` 四个签名头（验证方式见 README §8）。

## OA SSO

| ID | 方法与路径 | Scope | 限流桶 | 幂等 |
|---|---|---|---|---|
| INT-14 | `POST /api/internal/v1/oa/sso/exchange` | `sso:exchange` | sso | `code` 一次性 |

一次性 code 原子交换登录态；code 重放返回失败（不存在「二次成功」）。
独立限流桶，不与业务读写互抢配额。
