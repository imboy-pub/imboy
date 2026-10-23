# Internal API v1 — 端点参考（49 个 Internal 操作）

> 与代码冻结表（`src/api/enterprise_internal_routes.erl`）逐条一致；
> 字段级机器契约见 `../../openapi-internal.yaml`（编辑真源，
> `../../paths/internal/v1/`）与 `../../openapi-internal.bundle.yaml`（bundle 单文件）。
> 认证、限流桶、幂等、`sender_mode` 语义见 [README.md](./README.md)。

图例：幂等 `—`=不需要 / `K`=必须带 `Idempotency-Key` / `code`=一次性 code。

## 资源覆盖范围

运营后台使用 Admin Cookie 与 `/api/adm/*`；Internal 使用 HMAC 签名和
`/api/internal/v1/*`。Admin 的菜单与 CRUD 不自动构成集成能力。

| 资源 | 当前操作 | 范围 |
|---|---|---|
| 来源目录 | INT-43、45..49 | 可信来源显式增量、核对、成员离职检查及停用/恢复；无任意组织治理 |
| Workspace | INT-24/25、37..39 | 分页、详情、创建、修改、软归档 |
| 客服坐席 | INT-33..36 | 分页、详情、开通和配置调整；停用使用 PATCH enabled=false |
| 企业频道 | INT-30/31、40..42 | 仅 Workspace 频道；分页、详情、创建、修改、软归档 |
| 企业群 | INT-04..06、18..21、26/27 | 创建、成员与角色管理、资料修改、解散、分页与详情 |
| 企业项目 | INT-28/29 | 只读分页与详情；当前不提供创建、修改或归档接口 |

列表使用不透明 `cursor`，不提供无界导出。页大小参数分三组：

| 列表 | 参数 | 默认 | 范围 |
|---|---|---|---|
| Workspace、群、群成员、项目、频道、客服坐席 | `limit` | 50 | 1..100 |
| 身份目录、可映射用户、来源组织/部门/人员 | `page_size` | 50 | 1..100 |
| Webhook 投递 | `page_size` | 20 | 1..50 |

超界拒绝，不静默截断；`has_more=false` 时 `next_cursor=null`。
写请求按端点的幂等要求调用，资源版本、来源与业务归属由平台复核。
应用生命周期和凭证治理通过 Admin 管理面处理。

`scope=workspace` 是资源业务归属字段，不是授权枚举。

## 应用与凭证

| ID | 方法与路径 | 限流桶 | 幂等 |
|---|---|---|---|
| INT-01 | `GET /api/internal/v1/application` | read | — |

读本应用信息（名称、组织、认证方式、状态）。健康检查与联调首选。

## 外部身份映射（OA 用户 ↔ 平台用户）

| ID | 方法与路径 | 限流桶 | 幂等 |
|---|---|---|---|
| INT-02 | `PUT /api/internal/v1/identity-mappings` | write | K |
| INT-03 | `POST /api/internal/v1/identity-mappings/resolve` | read | — |
| INT-15 | `DELETE /api/internal/v1/identity-mappings` | write | K |
| INT-16 | `POST /api/internal/v1/identity-mappings/directory` | read | — |
| INT-17 | `POST /api/internal/v1/directory/users` | read | — |

- **绑定（INT-02）/ 解绑（INT-15）**：把 OA 侧用户标识绑定到平台用户；解绑后
  该用户对你的消息/群操作立即不可达（`identity_not_mapped`）。
- **解析（INT-03）**：OA 标识 → 平台用户；只返回命中的 active 映射，未映射项省略，不使整个请求失败。最多 100 个外部标识。
- **目录（INT-16/17）**：查询本组织已映射/可映射用户（分页，只回元数据）。
  游标为 CURSOR-V2 签名形态（HMAC-SHA256、24h 有效，绑定页族
  identity_mappings / directory_users 与 organization / application /
  workspace 过滤）：篡改、垃圾串、跨页族、换过滤、过期（>24h）及旧版
  未签名形态一律 `invalid_request`（400）；签名密钥不可用
  `security_gate_closed`（503）。page_size 1..100（缺省 50），越界拒绝。

## 企业群组

| ID | 方法与路径 | 限流桶 | 幂等 |
|---|---|---|---|
| INT-04 | `POST /api/internal/v1/groups` | write | K |
| INT-26 | `GET /api/internal/v1/groups` | read | — |
| INT-05 | `PUT /api/internal/v1/groups/{group_id}/members` | write | K |
| INT-27 | `GET /api/internal/v1/groups/{group_id}/members` | read | — |
| INT-06 | `DELETE /api/internal/v1/groups/{group_id}/members` | write | K |
| INT-18 | `GET /api/internal/v1/groups/{group_id}` | read | — |
| INT-19 | `PATCH /api/internal/v1/groups/{group_id}` | write | K |
| INT-20 | `PUT /api/internal/v1/groups/{group_id}/members/roles` | write | K |
| INT-21 | `DELETE /api/internal/v1/groups/{group_id}` | write | K |

添加/移除成员使用 `external_user_ids` 数组；角色修改使用
`roles: [{external_user_id, role}]`，role 为整数 1/2/3/5（普通成员/嘉宾/管理员/副群主）。
标识须映射到本组织有效用户；群主角色不由该接口调整。跨组织 `group_id`
一律 `resource_not_found`（平台侧不区分「不存在」与「不归你」）。
签名请求无需为群详情或成员分页另配读取权限。

## Workspace

| ID | 方法与路径 | 限流桶 | 幂等 |
|---|---|---|---|
| INT-24 | `GET /api/internal/v1/workspaces` | read | — |
| INT-25 | `GET /api/internal/v1/workspaces/{workspace_id}` | read | — |

Workspace keyset 列表与详情；签名请求按目标业务组织收窄行集，已归档 Workspace 不可见。写操作 INT-37..39 已实现。

## 企业项目（只读）

| ID | 方法与路径 | 限流桶 | 幂等 |
|---|---|---|---|
| INT-28 | `GET /api/internal/v1/projects` | read | — |
| INT-29 | `GET /api/internal/v1/projects/{project_id}` | read | — |

企业项目 keyset 列表与详情（V2.1 新增）；按目标 Workspace 业务归属校验。
当前合同只提供读取，写操作不属于已实现的 v1 接口面。

## 企业频道（读取与写管理）

| ID | 方法与路径 | 限流桶 | 幂等 |
|---|---|---|---|
| INT-30 | `GET /api/internal/v1/channels` | read | — |
| INT-31 | `GET /api/internal/v1/channels/{channel_id}` | read | — |

工作区企业频道 keyset 列表与详情（V2.1 新增）；**仅 `scope=workspace` 的
频道**（`status=1` 启用中），个人频道永久排除。写操作 INT-40..42 已实现，详见下文。

## 企业文件（直传）

| ID | 方法与路径 | 限流桶 | 幂等 |
|---|---|---|---|
| INT-07 | `POST /api/internal/v1/files/presign` | write | K |
| INT-08 | `POST /api/internal/v1/files/confirm` | write | K |
| INT-22 | `POST /api/internal/v1/files/governance` | write | K |

两段式直传：`presign` 取对象存储预签名 → 你方直传文件 → `confirm` 确认入库；
`governance` 为治理动作（如生命周期/元数据管理）。

## 消息（`sender_mode` 见 README §6）

| ID | 方法与路径 | 限流桶 | 幂等 |
|---|---|---|---|
| INT-09 | `POST /api/internal/v1/messages/direct` | write | K |
| INT-10 | `POST /api/internal/v1/groups/{group_id}/messages` | write | K |

- INT-09 单聊直发；INT-10 群发到企业群。
- 请求显式声明 `sender_mode=application | human`：
  两种模式均使用 HMAC；`human` 还必须指定本组织已映射、已启用的发送者
  （不接受别名参数，平台侧强制校验），不另设逐接口 scope 权限。
- 消息为**企业托管、固定非端到端加密**；幂等窗口内相同请求重放首次结果。
- 响应 `msg_id` 为 UUID v7 字符串，`webhook_resource_id` 为 JSON int64 行 ID；
  `accepted_at` 为 Unix 毫秒。三者不能互换。

## 好友申请（只发起，平台不自动通过）

| ID | 方法与路径 | 限流桶 | 幂等 |
|---|---|---|---|
| INT-11 | `POST /api/internal/v1/friend-requests` | write | K |

仅可对**同组织已映射用户**发起，发送者形态固定为 `human`（代 OA 用户）。
平台**没有**自动通过/确认、删除好友、批量加好友的 API。

## Webhook（出站回调）

| ID | 方法与路径 | 限流桶 | 幂等 |
|---|---|---|---|
| INT-12 | `PUT /api/internal/v1/webhook` | write | K |
| INT-13 | `POST /api/internal/v1/webhook/deliveries/{delivery_id}/replay` | write | K |
| INT-23 | `GET /api/internal/v1/webhook/deliveries` | read | — |
| INT-32 | `POST /api/internal/v1/webhook/test-delivery` | write | K |

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

| ID | 方法与路径 | 限流桶 | 幂等 |
|---|---|---|---|
| INT-14 | `POST /api/internal/v1/oa/sso/exchange` | sso | `code` 一次性 |

一次性 code 原子交换登录态；code 重放返回失败（不存在「二次成功」）。
独立限流桶，不与业务读写互抢配额。

## 企业客服坐席 / Customer service seats

| ID | 方法与路径 | 限流桶 | 幂等 |
|---|---|---|---|
| INT-33 | `GET /api/internal/v1/customer-service/seats` | read | — |
| INT-34 | `GET /api/internal/v1/customer-service/seats/{business_identity_id}` | read | — |
| INT-35 | `POST /api/internal/v1/customer-service/seats` | write | K |
| INT-36 | `PATCH /api/internal/v1/customer-service/seats/{business_identity_id}` | write | K |
| INT-37 | `POST /api/internal/v1/workspaces` | write | K |
| INT-38 | `PATCH /api/internal/v1/workspaces/{workspace_id}` | write | K |
| INT-39 | `DELETE /api/internal/v1/workspaces/{workspace_id}` | write | K |


## 企业频道写管理（INT-40..42）

| ID | 方法与路径 | 归属边界 | 幂等 |
|---|---|---|---|
| INT-40 | POST /api/internal/v1/channels | workspace_id 指定的工作空间 | 必需 |
| INT-41 | PATCH /api/internal/v1/channels/{channel_id} | 频道所属工作空间 | 必需 |
| INT-42 | DELETE /api/internal/v1/channels/{channel_id} | 频道所属工作空间 | 必需 |

POST 必需 workspace_id、creator_user_id、name，可选 description、avatar。企业 Owner 或有效在册企业成员且是工作空间 Owner/Member 才可作为创建者；Guest、离职、停用账号被拒绝。不隐式增加企业或空间成员，不签发 Human 身份。频道始终是企业非公开频道（visibility=1、access_type=0、join_policy=1），复用现有管理频道配额。

PATCH 必需 expected_version，并至少修改 name/description/avatar 一项；DELETE 必需 expected_version，为 status=0 软归档。scope、workspace_id、creator_user_id、策略与未知字段拒绝；旧版本 409 version_conflict。GET 返回治理 version，旧用户/Admin 入口的资料与状态变更也受版本触发器保护，订阅计数变化不增加治理版本。归档保留历史消息、订阅及管理员关系。

资源变更、带 application_id/correlation_id 且 actor_user_id=NULL 的应用审计，以及幂等结果共用事务。资源实际归属仍为工作空间所在企业；审计记录应用来源。重试必须复用同业务 key、方法、路径及相同规范化 JSON 内容；重放前验证当前凭证与业务归属，停用后拒绝。幂等键为 1..128 个可打印 ASCII 字符，有效期默认 24 小时且可配置，统一规则见 README §5。

English summary: Three channel write operations are implemented with HMAC authentication and Workspace business attribution, eligible Human creator membership, non-public visibility, quota, optimistic governance versions, atomic application audit and authorized idempotent replay. Soft archive preserves channel history and identity relations.

## 来源目录与 OA 入站 / Source directory and inbound login

| ID | 方法与路径（前缀 /api/internal/v1） | 限流 | 幂等 |
|---|---|---|---|
| INT-43 | POST /directory/sync | internal_write | K |
| INT-44 | POST /oa/login/tickets | internal_sso | 不要求；票据单次消费 |
| INT-45 | GET /directory/organizations | internal_read | — |
| INT-46 | GET /directory/departments | internal_read | — |
| INT-47 | GET /directory/persons | internal_read | — |

INT-46/47 必传 external_organization_id；分页 page_size=1..100（默认 50）、cursor。
INT-46 可按 external_department_id/status 过滤；INT-47 可按 external_user_id、source_status、
membership_status、projection_status 精确过滤。未知或重复参数拒绝，游标绑定完整过滤条件。
读取只返回有效授权覆盖的已建立组织，不显示未引导配置，亦不要求目录写权限。
字段、状态与双方向 OA 流程以 README 和 ../../paths/internal/v1/directory/ 的 schema 为准。

English: INT-43/44 are implemented source-sync and login-ticket operations. INT-45..47 add read-only
source reconciliation. Human login exchange/link/register remain outside the Internal credential surface.


| ID | 方法与路径 | 限流桶 | 幂等 |
|---|---|---|---|
| INT-48 | GET /directory/memberships | internal_read | — |
| INT-49 | PATCH /directory/memberships | internal_write | K |

INT-48 必传 external_organization_id、external_user_id，返回成员版本及离职阻塞计数。
INT-49 使用相同外部标识、status=active/suspended、expected_member_version；仅来源托管普通成员，
恢复必须匹配来源暂停标记。离职先暂停、人工交接，再由 INT-43 显式移除任职。详见 README 的 OA 企业与成员生命周期。
