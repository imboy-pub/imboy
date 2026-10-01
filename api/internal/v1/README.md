# IMBoy 企业内部集成 API（Internal API v1）— 集成方文档

> **版本**：v1.0（冻结）｜ **受众**：企业 OA / 第三方集成系统开发者
> **Base Path**：`/api/internal/v1`
> **机器契约**：`api/openapi-internal.yaml`（编辑真源，28 path / 42 端点，
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
| 外部身份映射 | OA 用户 ↔ IMBoy 用户的绑定 / 解析 / 解绑 / 目录查询（INT-02/03/15/16/17） |
| 企业群组 | 建群、增删成员、改群、成员角色；详情/列表/成员只读（INT-04/05/06/18/19/20/21/26/27） |
| Workspace 只读 | 列表与详情（INT-24/25） |
| 企业文件 | 直传预签名 / 确认入库 / 治理（INT-07/08/22） |
| 消息 | 应用身份直发、以人类身份代发；单聊与群聊（INT-09/10） |
| 好友申请 | 代 OA 用户发起（只发起、不自动通过，见 §6）（INT-11） |
| Webhook | 登记出站回调、查询/重放投递、连通性测试（INT-12/13/23/32） |
| OA SSO | 一次性 code 原子交换登录态（INT-14） |
| 企业项目只读 | 列表与详情（INT-28/29） |
| 企业频道 | 列表与详情（INT-30/31）；创建、修改、软归档（INT-40..42） |

**边界（务必了解）**：

- 本 API 与 Admin 管理面（`/api/adm/*`）、人类用户面（`/api/v1/*`）**三前缀互不相交**：
  集成凭证调不了另外两个面，另外两面的凭据也调不了本 API。
- 路径中的群、工作区、项目和频道 ID，以及项目／频道列表的
  `workspace_id` 查询参数，必须是十进制正整数，范围 `1..9223372036854775807`
  （PostgreSQL BIGINT / int64）。非法格式、零、负数或越界值返回 HTTP 400
  `invalid_request`；凭证无效时仍优先返回 HTTP 401。
  Resource path IDs and workspace query IDs must be positive decimal int64 values;
  invalid values return 400 after credential authentication.
  Webhook `delivery_id` 是不透明字符串，必须原样传递，不应用此整数规则。
  Webhook delivery IDs are opaque strings and must be passed unchanged.
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
Authorization: Bearer <credential_prefix>.<secret>
```

- 每份凭证只属于一个企业应用，同一应用可以有多份凭证。有效权限为应用 allowed_scopes 与当前生效 Grant scopes 的交集；没有 Grant、全部撤销或全部过期时不回退为应用默认权限。
- 凭证可由平台管理员**轮换**（旧凭证立即失效）与**撤销**；应用被停用/归档后
  所有请求返回 `application_disabled`（403）。
- secret 在平台侧只保存不可逆摘要；**泄露即轮换**，无需担心「改不回来」。

`credential_prefix` 是服务端签发的完整 `ib_int_...` 前缀，不是 application_id；也不要用返回的 credential_id 自行重建。签发／轮换响应的 `secret` 字段实际给出可直接使用的完整凭证，原样作为 Bearer 值保存。

## 4. 授权（Scope，固定 18 枚举，无通配）

| Scope | 解锁能力 |
|---|---|
| `application:read` | 读本应用信息 |
| `identities:read` | 身份解析、目录查询 |
| `identities:write` | 身份绑定 / 解绑 |
| `groups:write` | 群组与成员管理 |
| `groups:read` | 群组/成员只读（INT-18/26/27） |
| `workspaces:read` | Workspace 列表与详情（INT-24/25） |
| `workspaces:write` | Workspace 创建、修改与软归档（INT-37..39；不自动授予） |
| `projects:read` | 项目列表与详情（INT-28/29） |
| `channels:read` | 频道列表与详情（INT-30/31） |
| `channels:write` | 企业频道创建、修改和软归档（INT-40..42；工作空间 Grant） |
| `files:write` | 文件直传与治理 |
| `messages:send` | 以**应用身份**发消息 |
| `messages:send_as_human` | 以**人类身份**代发消息（见 §6） |
| `friend_requests:create` | 发起好友申请 |
| `webhooks:manage` | Webhook 登记与投递管理 |
| `sso:exchange` | OA SSO 一次性 code 交换 |
| `customer_service:read` | 企业坐席列表与详情（企业全域 Grant） |
| `customer_service:write` | 创建、修改、停用坐席（企业全域 Grant） |

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

16 个稳定错误码（`code` 为机器可读、snake_case、**长期稳定**，勿按 message 文案做逻辑）：

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
| `version_conflict` | 409 | 资源版本已变化，刷新后重新提交 |
| `resource_conflict` | 409 | 资源已存在，例如坐席重复开通 |
| `seat_limit_exceeded` | 409 | 企业坐席额度不足 |

## 8. Webhook 出站（你方系统将收到的回调）

出站投递使用独立的 Webhook HMAC secret，与 Internal API 的 Bearer 凭证不同。通过 INT-12（`PUT /webhook`）设置 `rotate: true` 轮换签名密钥，响应只返回新 secret 一次；轮换 API 凭证不会轮换此密钥。接收方更新为新签名密钥后，旧密钥生成的签名不能通过验证。

出站投递包含以下签名头：

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

客户端先以 Human JWT 调用 `GET /api/v1/workbench/entries?organization_id=<当前企业ID>`。
`payload.entries` 返回该企业授权 OA 条目（最多 20，按应用 ID 升序）：`kind`、`organization_id`、`application_id`、`application_key`、`label`、`redirect_uri`。没有授权配置返回空数组；省略 / 非法企业 ID 返回 400。该配置发现接口属于 Human 认证面，不能使用 Application Credential。客户端必须匹配当前企业，不从其他企业取第一项；有多个应用时需要明确选择。

流程：用户已登录 IMBoy → IMBoy 客户端以 Human JWT 调用
`POST /api/v1/oa/sso/code`，提交 `application_key`、预注册的 `redirect_uri`
与随机 `nonce` → 浏览器 / WebView 携带一次性 `code` 和 `state=nonce` 到
OA 回调地址 → **OA 服务端**使用本应用 Credential 调 INT-14，提交
`code`、同一 `redirect_uri` 与 `nonce` → 取得企业、应用、平台用户及
OA 外部用户标识 → **OA 建立自己的登录会话**。

多企业客户端签发时还必须提交当前选定的 `organization_id`（JSON 正整数 int64）。服务端只从该企业匹配应用并校验有效成员关系；不会退回另一企业。为兼容旧客户端，省略该字段仍按成员关系收敛，多义拒绝。该字段属于 Human 签发请求，INT-14 的应用凭证及交换字段不变。

- code 固定 60 秒有效，绑定企业、应用、redirect 与 nonce；只能成功交换
  一次，过期 / 重放 / 绑定不符均拒绝。身份映射及用户有效状态由 IMBoy 验证。
- INT-14 返回身份事实，不返回 IMBoy JWT、应用 secret 或 OA 会话 Cookie；
  OA 负责验证回调 state、设置自己的会话并处理登录失败。
- Application Credential 只放在 OA 服务端；不能交给浏览器 / WebView。
  本协议是 **IMBoy → OA** 单点登录，不提供 OA 反向登录 IMBoy 的会话接口。

## 10. 端点参考与 Postman 集合

- 人类阅读：[endpoints.md](./endpoints.md)（32 个端点，按域分组）。
- **机器契约**：`../openapi-internal.yaml`（编辑真源）与
  `../openapi-internal.bundle.yaml`（bundle 单文件）——字段级请求/响应
  schema 逐端点从 handler 实证（`src/api/enterprise_*_handler.erl`），
  16 个稳定错误码为可复用 response 组件；scope / 限流桶 / 幂等要求以
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

## 客服坐席集成 / Customer service seats

INT-33..36 管理企业坐席配置，不签发 Seat JWT。坐席主键为 business_identity_id，必须对应本企业 customer_service 业务身份；workspace_id 只是审计位置。读写权限分别授予，必须有覆盖对应 scope 的企业全域 Grant，仅 Workspace Grant 不足。PATCH 必须带 expected_version 以及 enabled/max_concurrent 至少一个；停用用 enabled=false，不删除历史。列表默认 limit=50，最大100，按 ID 升序，包含停用项。应用、企业或 Grant 撤销后，包括幂等重放在内的下一请求仍须通过授权检查。

English: Seats belong to the organization. Workspace IDs select audit locations only. Reads and writes need separate scopes and organization-wide grants. Updates require optimistic versions; disabling retains history. Signed cursors bind organization, application and seat-list family.

工作空间写管理：INT-37..39，独立 workspaces:write（不自动授予）。创建指定本企业 Owner/Admin；修改与软归档使用 expected_version；默认归档须显式同企业替代项及其 Grant。GET 工作空间追加 version。应用审计、资源及幂等响应同事务；归档后的同 key 重放仍需当前有效 Grant。


企业频道写管理：INT-40 POST /channels（workspace_id、creator_user_id、name；可选 description/avatar）、INT-41 PATCH /channels/{channel_id}（expected_version 及至少一个资料字段）、INT-42 DELETE /channels/{channel_id}（expected_version）。创建者须为有效企业成员（企业 Owner 包含）及工作空间 Owner/Member，Guest 不可创建；复用现有 20 个有效管理频道配额。创建固定非公开、免费且邀请制（visibility=1/access_type=0/join_policy=1）；归属、创建者及策略不允许在此接口修改。GET 增加 version；资料或治理状态更新增加版本，订阅计数等派生更新不增加版本。归档 status=0，消息、订阅及管理员关系保留。当前 Grant 在重放前验证，撤权后的同 key 重试拒绝；应用 actor 与幂等记录同事务。

English summary: Internal channel writes require an explicit channels:write grant scoped to the target Workspace. Creation checks current Organization and Workspace membership, preserves the existing channel quota, and creates a non-public channel. Profile changes and soft archive require an optimistic governance version. Audit and byte-exact authorized replay are atomic; archive retains history and identities.
