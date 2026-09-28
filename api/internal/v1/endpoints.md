# Internal API v1 — 端点参考（32 端点）

> 与代码冻结表（`src/api/enterprise_internal_routes.erl`）逐条一致；
> 字段级机器契约见 `../openapi-internal.yaml`（编辑真源，
> `../paths/internal/v1/`）与 `../openapi-internal.bundle.yaml`（bundle 单文件）。
> 认证、限流桶、幂等、`sender_mode` 语义见 [README.md](./README.md)。

图例：幂等 `—`=不需要 / `K`=必须带 `Idempotency-Key` / `code`=一次性 code。

## 企业数据 CRUD 覆盖审计（2026-09-23）

运营后台与企业集成是两个独立安全面：

- **运营后台**：Admin Cookie + `/api/adm/*`，供平台运营人员跨企业治理数据；
- **企业集成**：Application Credential + `/api/internal/v1/*`，只能访问凭证所属
  Organization 与已授予 Workspace，不得复用 Admin 权限。

因此，管理后台出现一个菜单，并不表示同名资源应自动暴露完整 Internal CRUD。
当前冻结合同（INT-01..INT-31）的实际覆盖如下：

| 企业数据 | 分页查看 | 新增 | 详情 | 修改 | 删除/归档 | 当前结论 |
|---|---|---|---|---|---|---|
| 组织治理 | — | — | INT-01 仅返回凭证所属组织上下文 | — | — | **不开放组织增删**；Application 不能创建或删除自己的授权父域 |
| Workspace | INT-24 | — | INT-25 | — | — | **只读已实现（写操作为 P1 待补）**；写入仍只能经 Admin 面操作既有 Grant 边界 |
| 客服坐席 | — | — | — | — | — | **待实现**；不得复用独立 Seat JWT 的坐席工作台接口 |
| 企业频道 | INT-30 | — | INT-31 | — | — | **只读已实现（写操作为 P1 待补）**；仅 `scope=workspace`，个人频道永久排除 |
| 企业群 | INT-26 | INT-04 | INT-18 | INT-19 / INT-20 | INT-21 / INT-06 | **只读+写核心已实现**（成员分页 INT-27） |
| 企业项目 | INT-28 | — | INT-29 | — | — | **只读已实现（写操作为 P1 待补）**；必须受 Workspace Grant 约束 |

### 需要追加的 Internal v1 合同

以下是补齐上述企业数据所需的**最小追加面**。其中 P0 只读四组（INT-24..31）
已于 V2.1 落地——已进入 `enterprise_internal_routes:routes/0`、可调用，
并出现在下方 Postman 集合中；下列仍标注**待补**的行（客服坐席、各资源 P1
写操作）未进入 `routes/0`，当前调用会返回 404。必须先完成 handler、
Grant/Scope、OpenAPI、审计和自动化测试，再作为 v1 只追加端点发布。

| 优先级 | 资源 | 建议追加路径 | Scope | 语义 |
|---|---|---|---|---|
| P0 | Workspace | `GET /workspaces`、`GET /workspaces/{workspace_id}` | `workspaces:read` | ✅ 已实现（INT-24/25）；Grant 过滤的 cursor 分页与详情 |
| P0 | 企业群 | `GET /groups`、`GET /groups/{group_id}/members` | `groups:read` | ✅ 已实现（INT-26/27）；只返回 Application 可访问的 Workspace 企业群 |
| P0 | 企业项目 | `GET /projects`、`GET /projects/{project_id}` | `projects:read` | ✅ 已实现（INT-28/29）；Grant 过滤的 cursor 分页与详情 |
| P0 | 企业频道 | `GET /channels`、`GET /channels/{channel_id}` | `channels:read` | ✅ 已实现（INT-30/31）；仅 `scope=workspace`，排除个人频道 |
| P0 | 客服坐席 | `GET /customer-service/seats`、`GET /customer-service/seats/{seat_id}` | `customer_service:read` | Organization + Workspace 过滤的 cursor 分页与详情 |
| P1 | Workspace | `POST /workspaces`、`PATCH /workspaces/{workspace_id}`、`DELETE /workspaces/{workspace_id}` | `workspaces:write` | 新建、修改、软归档；写请求必须幂等 |
| P1 | 企业项目 | `POST /projects`、`PATCH /projects/{project_id}`、`DELETE /projects/{project_id}` | `projects:write` | 新建、修改、软归档；项目必须属于已授权 Workspace |
| P1 | 企业频道 | `POST /channels`、`PATCH /channels/{channel_id}`、`DELETE /channels/{channel_id}` | `channels:write` | 新建、修改、软归档；`scope/workspace_id` 创建后不可变 |
| P1 | 客服坐席 | `POST /customer-service/seats`、`PATCH /customer-service/seats/{seat_id}`、`DELETE /customer-service/seats/{seat_id}` | `customer_service:write` | 开通、调整并发/状态、停用；不签发 Admin 或 Seat 身份 |

约束：

- 所有列表使用 `cursor + limit`，不提供无界导出；默认 `limit=20`，最大 `100`；
- `DELETE` 是可审计的软归档/停用，不做物理删除；
- 所有 P1 写请求要求 `Idempotency-Key`，并记录真实 `origin_application_id`；
- 组织创建、组织删除、Application/Credential 生命周期、Grant 签发/撤销仍只属于
  `/api/adm/*`，不会下放给 Application Credential；
- 上述待补合同正式实现前，当前可导入 Postman 的权威集合仍是本目录的
  `IMBoy-Internal-API-v1.postman_collection.json`（32 个已实现端点）。

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
  游标为 CURSOR-V2 签名形态（HMAC-SHA256、24h 有效，绑定页族
  identity_mappings / directory_users 与 organization / application /
  workspace 过滤）：篡改、垃圾串、跨页族、换过滤、过期（>24h）及旧版
  未签名形态一律 `invalid_request`（400）；签名密钥不可用
  `security_gate_closed`（503）。page_size 1..100（缺省 50），越界拒绝。

## 企业群组

| ID | 方法与路径 | Scope | 限流桶 | 幂等 |
|---|---|---|---|---|
| INT-04 | `POST /api/internal/v1/groups` | `groups:write` | write | K |
| INT-26 | `GET /api/internal/v1/groups` | `groups:read` | read | — |
| INT-05 | `PUT /api/internal/v1/groups/{group_id}/members` | `groups:write` | write | K |
| INT-27 | `GET /api/internal/v1/groups/{group_id}/members` | `groups:read` | read | — |
| INT-06 | `DELETE /api/internal/v1/groups/{group_id}/members` | `groups:write` | write | K |
| INT-18 | `GET /api/internal/v1/groups/{group_id}` | `groups:read` | read | — |
| INT-19 | `PATCH /api/internal/v1/groups/{group_id}` | `groups:write` | write | K |
| INT-20 | `PUT /api/internal/v1/groups/{group_id}/members/roles` | `groups:write` | write | K |
| INT-21 | `DELETE /api/internal/v1/groups/{group_id}` | `groups:write` | write | K |

成员操作按 `user_id` 精确定位（必须是本组织已映射用户）；跨组织 `group_id`
一律 `resource_not_found`（平台侧不区分「不存在」与「不归你」）。
V2.1 FIX：群详情（INT-18）与群/成员只读分页（INT-26/27）scope 均为
`groups:read`（此前文档误标 `groups:write`）。

## Workspace（只读）

| ID | 方法与路径 | Scope | 限流桶 | 幂等 |
|---|---|---|---|---|
| INT-24 | `GET /api/internal/v1/workspaces` | `workspaces:read` | read | — |
| INT-25 | `GET /api/internal/v1/workspaces/{workspace_id}` | `workspaces:read` | read | — |

Workspace keyset 列表与详情（V2.1 新增）；行集收窄为「当前生效 Grant 覆盖
的 W 集合」，已归档 Workspace 不可见。写操作为 P1 待补。

## 企业项目（只读）

| ID | 方法与路径 | Scope | 限流桶 | 幂等 |
|---|---|---|---|---|
| INT-28 | `GET /api/internal/v1/projects` | `projects:read` | read | — |
| INT-29 | `GET /api/internal/v1/projects/{project_id}` | `projects:read` | read | — |

企业项目 keyset 列表与详情（V2.1 新增）；必须受 Workspace Grant 约束。
写操作为 P1 待补。

## 企业频道（只读）

| ID | 方法与路径 | Scope | 限流桶 | 幂等 |
|---|---|---|---|---|
| INT-30 | `GET /api/internal/v1/channels` | `channels:read` | read | — |
| INT-31 | `GET /api/internal/v1/channels/{channel_id}` | `channels:read` | read | — |

工作区企业频道 keyset 列表与详情（V2.1 新增）；**仅 `scope=workspace` 的
频道**（`status=1` 启用中），个人频道永久排除。写操作为 P1 待补。

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
| INT-32 | `POST /api/internal/v1/webhook/test-delivery` | `webhooks:manage` | write | K |

登记/更新回调地址；投递状态分页查询；失败投递可按 `delivery_id` 重放；
测试投递（INT-32，v1.1.1）发出合成 `webhook.ping` 事件验证回调链路（该事件
类型**不可订阅**，仅本端点产生）。
你方回调端将收到 `x-imboy-delivery / x-imboy-event / x-imboy-timestamp /
x-imboy-signature` 四个签名头；可订阅事件白名单（4 值）、回调正文信封
结构与 5/30/300s 重试节奏见 [README §8](./README.md#8-webhook-出站你方系统将收到的回调)。

INT-23 分页为 CURSOR-V2 签名游标（`cursor` / `page_size`，缺省 20 上限 50；
排序 `created_at DESC, delivery_id DESC`）。**旧 offset 参数 `page`/`size`
任一出现即 400 `cursor_required_v1`**（versioned 迁移错误，DEC-INT23-COMPAT）；
游标篡改/跨页族/换过滤/过期（>24h）一律 400 `invalid_request`。

## OA SSO

| ID | 方法与路径 | Scope | 限流桶 | 幂等 |
|---|---|---|---|---|
| INT-14 | `POST /api/internal/v1/oa/sso/exchange` | `sso:exchange` | sso | `code` 一次性 |

一次性 code 原子交换登录态；code 重放返回失败（不存在「二次成功」）。
独立限流桶，不与业务读写互抢配额。
