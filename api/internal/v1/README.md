# IMBoy 企业内部集成 API（Internal API v1）— 集成方文档

> **版本**：v1.0（冻结）｜ **受众**：企业 OA / 第三方集成系统开发者
> **Base Path**：`/api/internal/v1`
> **机器契约**：`api/openapi-internal.yaml`（编辑真源，19 path / 23 端点，
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
| 企业群组 | 建群、增删成员、改群、成员角色（INT-04/05/06/18/19/20/21） |
| 企业文件 | 直传预签名 / 确认入库 / 治理（INT-07/08/22） |
| 消息 | 应用身份直发、以人类身份代发；单聊与群聊（INT-09/10） |
| 好友申请 | 代 OA 用户发起（只发起、不自动通过，见 §6）（INT-11） |
| Webhook | 登记出站回调、查询/重放投递（INT-12/13/23） |
| OA SSO | 一次性 code 原子交换登录态（INT-14） |

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

3. 之后按 §5 的端点表与 `.contract/api/openapi.yaml` 的字段 schema 开发。

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

## 4. 授权（Scope，固定 10 枚举，无通配）

| Scope | 解锁能力 |
|---|---|
| `application:read` | 读本应用信息 |
| `identities:read` | 身份解析、目录查询 |
| `identities:write` | 身份绑定 / 解绑 |
| `groups:write` | 群组与成员管理 |
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
`internal_sso`（SSO 交换）。超限返回 `rate_limited`（429）；具体阈值由部署配置，
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
| `x-imboy-signature` | 签名 |

投递失败自动重试，最终进入死信；可用 INT-23 查询、INT-13 按 `delivery_id`
重放（幂等键由平台侧派生）。

## 9. OA SSO（一次性 code 交换）

流程：OA 侧登录成功 → 向 IMBoy 换取一次性 code → 用户浏览器携带 code 到
IMBoy → 前端/网关调 INT-14 原子交换（`single_use_code`：一个 code 只能成功
交换一次，重放即失败）。完成即建立 IMBoy 会话，无需再传 OA 凭据。

## 10. 端点参考与 Postman 集合

- 人类阅读：[endpoints.md](./endpoints.md)（23 个端点，按域分组）。
- **机器契约**：`../openapi-internal.yaml`（编辑真源）与
  `../openapi-internal.bundle.yaml`（bundle 单文件）——字段级请求/响应
  schema 逐端点从 handler 实证（`src/api/enterprise_*_handler.erl`），
  13 个稳定错误码为可复用 response 组件；scope / 限流桶 / 幂等要求以
  `x-imboy-scope` / `x-imboy-rate-bucket` / `x-imboy-idempotency` 扩展字段
  标注。工具导入用 bundle 单文件。
- **动手联调**：[IMBoy-Internal-API-v1.postman_collection.json](./IMBoy-Internal-API-v1.postman_collection.json)
  —— Postman / Apifox 直接导入（Collection v2.1），已含全部 23 个端点、按域分文件夹、
  示例请求体与 `{{base_url}}` / `{{credential}}` 变量；导入后填好两个变量即可发请求。
  集合只收录当前冻结路由表中**已实现、可调用**的端点；CRUD 覆盖审计中标为
  `待实现` 的路径不会作为假请求提前塞入集合。
  注意：示例体为合成数据、个别字段形态以机器契约为准
  （如 INT-03 请求为 `external_user_ids` 数组、INT-20 为
  `roles:[{external_user_id, role}]` 对象数组）。

## 11. 版本与变更

- 本 API **已冻结为 v1**：只会追加新端点/新可选字段，不会破坏性变更既有形状。
- 变更记录见 [CHANGELOG.md](./CHANGELOG.md)。
