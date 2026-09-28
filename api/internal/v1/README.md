# IMBoy 企业内部集成 API（Internal API v1）— 集成方文档

> **版本**：v1.0（冻结）｜ **受众**：企业 OA / 第三方集成系统开发者
> **Base Path**：`/api/internal/v1`
> **机器契约**：`api/openapi-internal.yaml`（编辑真源，25 path / 31 端点，
> 字段级 schema 逐端点实证自 handler）；`api/openapi-internal.bundle.yaml`
> （bundle 单文件，可直接导入 Postman / Apifox / openapi-generator）。
> 本目录是面向集成方的交付文档。路由与合同由 12 项机械断言
> （`scripts/check_enterprise_release_manifest.py`）持续守护，文档漂移会被门禁发现。

本目录就是给集成方的交付物：整个 `v1/` 目录可直接打包发给对接方。
Base URL（协议/域名/端口）由部署方提供，本文档只约定路径与协议语义。

> **不要把 Admin 菜单接口等同于 Internal API**：运营后台使用 Admin Cookie +
> `/api/adm/*`；本目录使用 Application Credential + `/api/internal/v1/*`。
> 两者不能互换。组织、Workspace、项目、企业频道、客服坐席等运营治理资源的
> 当前覆盖与待补合同，见 [endpoints.md 的 CRUD 覆盖审计](./endpoints.md#企业数据-crud-覆盖审计2026-09-23)。

---

## 1. 你能集成什么

企业应用通过 IMBoy 签发的**集成凭证（Credential）**，以本 API 完成以下集成域：

| 域 | 能力 |
|---|---|
| 应用与凭证 | 读取本应用信息（INT-01） |
| 外部身份映射 | OA 用户 ↔ IMBoss 用户的绑定 / 解析 / 解绑 / 目录查询（INT-02/03/15/16/17） |
| 企业群组 | 建群、增删成员、改群、成员角色；详情/列表/成员只读（INT-04/05/06/18/19/20/21/26/27） |
| Workspace 只读 | 列表与详情（INT-24/25） |
| 企业文件 | 直传预签名 / 确认入库 / 治理（INT-07/08/22） |
| 消息 | 应用身份直发、以人类身份代发；单聊与群聊（INT-09/10） |
| 好友申请 | 代 OA 用户发起（只发起、不自动通过，见 §6）（INT-11） |
| Webhook | 登记出站回调、查询/重放投递、连通性测试（INT-12/13/23/32） |
| OA SSO | 一次性 code 原子交换登录态（INT-14） |
| 企业项目只读 | 列表与详情（INT-28/29） |
| 企业频道只读 | scope=workspace 列表与详情（INT-30/31） |

**边界（务必了解）**：

- 本 API 与 Admin 管理面（`/api/adm/*`）、人类用户面（`/api/v1/*`）**三前缀互不相交**：
  集成凭证调不了另外两个面，另外两面的凭据也调不了本 API。
- IMBoy **不提供 Open Platform 公网面**（不存在 `/api/open/v1/*` 生产路由）。
- 通过本 API 发送的企业托管消息为**固定非端到端加密**，不进入人类用户间
  C2C/C2G 的 E2EE 存储面；平台侧保留审计事实（发送者为应用、或应用代人类）。

---

## 2. 快速开始

1. 由平台管理员在 **Admin 管理台 → 企业应用治理** 为你的应用签发凭证；
   **secret 明文只在签发/轮换响应中出现一次**，请立即妥善保存（平台侧只存不可逆摘要）。
2. 用凭证调用第一个接口（读取应用自身）：

```bash
curl -sS "$BASE_URL/api/internal/v1/application" \
  -H "Authorization: Bearer ib_int_9100000000000000003.YOUR-SECRET" | jq
```

3. 之后按 [endpoints.md](./endpoints.md) 的端点表与 `../openapi-internal.yaml`
   （编辑真源；工具导入用 `../openapi-internal.bundle.yaml`）的字段 schema 开发。

---

## 3. 认证

所有请求必须携带：

```
Authorization: Bearer ib_int_<application_id>.<secret>
```

- 凭证与**单个企业应用**一一对应，其授权范围 = 签发时授予的 scope 集合（§4）。
- 凭证可由平台管理员**轮换**（旧凭证立即失效）与**撤销**；应用被停用/归档后
  所有请求返回 `application_disabled`（403）。
- secret 在平台侧只保存不可逆摘要；**泄露即轮换**，无需担心「改不回来」。

## 4. 授权（Scope，固定 14 枚举，无通配）

| Scope | 解锁能力 |
|---|---|
| `application:read` | 读本应用信息 |
| `identities:read` | 身份解析、目录查询 |
| `identities:write` | 身份绑定 / 解绑 |
| `groups:write` | 群组与成员管理 |
| `groups:read` | 群组/成员只读（INT-18/26/27） |
| `workspaces:read` | Workspace 列表与详情（INT-24/25） |
| `projects:read` | 项目列表与详情（INT-28/29） |
| `channels:read` | 频道列表与详情（INT-30/31） |
| `files:write` | 文件直传与治理 |
| `messages:send` | 以**应用身份**发消息 |
| `messages:send_as_human` | 以**人类身份**代发消息（见 §6） |
| `friend_requests:create` | 发起好友申请 |
| `webhooks:manage` | Webhook 登记与投递管理 |
| `sso:exchange` | OA SSO 一次性 code 交换 |

Scope 由管理员签发时授予，集成方**不可自选**、不存在 `*` 通配；
两个消息域端点（INT-09/10）按请求里的 `sender_mode` 动态要求
`messages:send` 或 `messages:send_as_human`。

## 5. 限流与幂等

**限流**：三只独立桶 —— `internal_read`（读）/ `internal_write`（写）/
`internal_sso`（SSO 交换）。超限返回 `rate_limited`（429），并携带
`Retry-After` 响应头（delta-seconds＝窗口剩余时间向上取整，最小 1s；
v1.1.1 起提供）；具体阈值由部署配置，
**认证失败也计读桶**（fail-closed：挡不住时宁可错杀）。

**幂等**：所有写动作（下表标 `required`）必须携带 `Idempotency-Key` 头，
同一 key 重复提交返回首次结果而非二次生效；key 冲突返回
`idempotency_conflict`（409）。SSO 交换的 code 本身一次性（`single_use_code`）。

## 6. 消息发送者语义（`sender_mode`）— 重要

企业托管消息支持两种发送者形态，**请求里显式声明，权限随形态收紧**：

| sender_mode | 语义 | 要求 scope |
|---|---|---|
| `application` | 以应用身份发出（展示为应用/机器人） | `messages:send` |
| `human` | 应用**代**某企业用户发出（该用户须为本组织已映射、已启用用户） | `messages:send_as_human` |

硬性约束：

- 发送者归属校验在平台侧强制：**不接受** `actor_user_id` / `as_user_id` 之类的
  别名参数；跨组织、未映射、已停用用户一律拒绝。
- 企业托管消息**固定非端到端加密**，不会写入人类用户间 E2EE 消息存储；
  审计与合规视图永久保留「由应用发出 / 应用代发」的事实。
- **好友申请（INT-11）只能以 `human` 形态发起**：仅可对同组织已映射用户发起，
  平台**不会**自动通过/确认，也**不提供**删除好友、批量加好友的能力。

## 7. 错误模型

错误响应 = HTTP 状态码 + 稳定错误码信封（字段级 schema 见 OpenAPI）：

```json
{ "error": { "code": "insufficient_scope", "message": "..." } }
```

13 个稳定错误码（`code` 为机器可读、snake_case、**长期稳定**，勿按 message 文案做逻辑）：

| code | HTTP | 含义 |
|---|---|---|
| `invalid_request` | 400 | 请求参数/形状非法 |
| `invalid_credential` | 401 | 凭证缺失/格式错/不存在的 locator |
| `credential_expired` | 401 | 凭证已过期 |
| `application_disabled` | 403 | 应用被停用/归档 |
| `organization_disabled` | 403 | 所属组织被停用 |
| `insufficient_scope` | 403 | 凭证未被授予所需 scope |
| `organization_boundary_violation` | 403 | 跨组织访问（IDOR 防线） |
| `resource_not_found` | 404 | 资源不存在或不属于本组织 |
| `identity_not_mapped` | 422 | 目标用户未做身份映射 |
| `idempotency_conflict` | 409 | Idempotency-Key 与既有请求冲突 |
| `rate_limited` | 429 | 触发限流（fail-closed） |
| `security_gate_closed` | 503 | 平台安全闸关闭（暂时拒绝服务） |
| `internal_error` | 500 | 内部错误（安全兜底：未列入稳定表的码一律收敛为此值） |

## 8. Webhook 出站（你方系统将收到的回调）

出站投递带以下签名头，供你方验证请求确实来自 IMBoy（**旋转凭证后旧签名一律不通过**）：

| 头 | 含义 |
|---|---|
| `x-imboy-delivery` | 投递 ID（幂等去重用） |
| `x-imboy-event` | 事件类型 |
| `x-imboy-timestamp` | 时间戳 |
| `x-imboy-signature` | 签名（HMAC-SHA256 hex，签名原文 = `<timestamp> "." <raw body>`） |

**可订阅事件（固定白名单，INT-12 `events` 只接受以下值）**：

| event_type | 触发时机 | resource |
|---|---|---|
| `message.enterprise.accepted` | 企业托管消息受理成功（INT-09/10，与消息同事务） | `{type: "msg_c2c" \| "msg_c2g", id: 消息表行 ID}` |
| `message.enterprise.failed` | 企业托管消息发送失败（业务回滚后独立事务；当前信封**不含**失败原因，仅事件本身表明该消息未受理） | `{type: "msg_c2c" \| "msg_c2g", id: 消息表行 ID}` |
| `file.confirmed` | 附件确认入库（INT-08，与转正同事务） | `{type: "attachment", id: 附件行 ID}` |
| `group.member.changed` | **白名单预留，当前版本无触发点**：订阅合法，但暂不会产生投递 | — |
| `webhook.ping` | **不可订阅**（INT-12 `events` 填它会被拒绝）；仅由 INT-32 测试投递产生 | `{type: "webhook", id: 端点配置代际}` |

> **注意**：`resource.id` 是平台侧消息表行 ID（int64），与 INT-09/10 响应返回的
> `msg_id`（客户端消息 ID）是**两个不同标识符**。v1.1.1 起 INT-09/10 响应追加
> `webhook_resource_id` 字段（与事件 `resource.id` 同源），集成方以此把回调
> 关联回本次发送响应；此前发起的消息只能以你方业务侧记录关联。

**回调正文信封（键集封闭，多一个键都是合同变更）**：

```json
{
  "event_id": "…",
  "delivery_id": "…",
  "event_type": "message.enterprise.accepted",
  "version": 1,
  "occurred_at": "2026-09-28T08:30:00.123Z",
  "organization_id": 9100000000000000001,
  "application_id": 9100000000000000003,
  "resource": { "type": "msg_c2c", "id": 7200000000000000042 }
}
```

- `occurred_at` 为 ISO-8601 UTC（毫秒、`Z` 结尾）；信封**不含**消息正文、
  secret 或签名 URL；需要正文细节时由你方按 `resource.id` 自行关联。
- `version` 当前恒为 `1`；变更会作为新 `version` 值发布，不会静默改字段。

投递失败按 **5s / 30s / 300s 三次退避**自动重试，全部失败后进入死信；
可用 INT-23 查询、INT-13 按 `delivery_id` 重放（幂等键由平台侧派生）。

**连通性测试（INT-32）**：`POST /webhook/test-delivery` 向当前端点投递一条
合成 `webhook.ping` 事件（走与真实事件完全相同的入箱/签名/重试管线），用于
联调期验证 URL、签名验证逻辑与防火墙放行；成功仅代表已入箱，投递结果以
INT-23 的 `status` 为准。端点未配置/disabled 时返回 `invalid_request`（400）。

## 9. OA SSO（一次性 code 交换）

流程：OA 侧登录成功 → 向 IMBoy 换取一次性 code → 用户浏览器携带 code 到
IMBoy → 前端/网关调 INT-14 原子交换（`single_use_code`：一个 code 只能成功
交换一次，重放即失败）。完成即建立 IMBoy 会话，无需再传 OA 凭据。

## 10. 端点参考与 Postman 集合

- 人类阅读：[endpoints.md](./endpoints.md)（32 个端点，按域分组）。
- **机器契约**：`../openapi-internal.yaml`（编辑真源）与
  `../openapi-internal.bundle.yaml`（bundle 单文件）——字段级请求/响应
  schema 逐端点从 handler 实证（`src/api/enterprise_*_handler.erl`），
  13 个稳定错误码为可复用 response 组件；scope / 限流桶 / 幂等要求以
  `x-imboy-scope` / `x-imboy-rate-bucket` / `x-imboy-idempotency` 扩展字段
  标注。工具导入用 bundle 单文件。
- **动手联调**：[IMBoy-Internal-API-v1.postman_collection.json](./IMBoy-Internal-API-v1.postman_collection.json)
  —— Postman / Apifox 直接导入（Collection v2.1），已含全部 32 个端点、按域分文件夹、
  示例请求体与 `{{base_url}}` / `{{credential}}` 变量；导入后填好两个变量即可发请求。
  集合只收录当前冻结路由表中**已实现、可调用**的端点；CRUD 覆盖审计中标为
  `待实现` 的路径不会作为假请求提前塞入集合。
  注意：示例体为合成数据（ID / 域名 / object_key 均为占位值，替换后使用）；
  v1.1.1 起示例字段名与必填集已与机器契约逐端点对齐（此前 INT-02/03/04/
  07/08/09/10/11/12/14/20/22 共 12 处示例与契约不一致，已全部修正）。

## 11. 版本与变更

- 本 API **已冻结为 v1**：只会追加新端点/新可选字段，不会破坏性变更既有形状。
- 变更记录见 [CHANGELOG.md](./CHANGELOG.md)。
