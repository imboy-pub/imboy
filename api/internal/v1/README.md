# IMBoy 企业内部集成 API（Internal API v1）— 集成方文档

> **版本**：v1.0（冻结）｜ **受众**：企业 OA / 第三方集成系统开发者
> **Base Path**：`/api/internal/v1`
> **机器契约**：`api/openapi-internal.yaml`（编辑真源，34 path / 49 端点，
> 字段级 schema 逐端点实证自 handler）；`api/openapi-internal.bundle.yaml`
> （bundle 单文件，可直接导入 Postman / Apifox / openapi-generator）。
> 本目录是面向集成方的交付文档。路由与合同由静态机械断言
> （`scripts/check_enterprise_release_manifest.py`）持续检查；它们不替代字段级核对与真实 HTTP 验证。

本目录就是给集成方的交付物：整个 `v1/` 目录可直接打包发给对接方。
Base URL（协议/域名/端口）由部署方提供，本文档只约定路径与协议语义。

> **不要把 Admin 菜单接口等同于 Internal API**：运营后台使用 Admin Cookie +
> `/api/adm/*`；本目录使用 Application Credential + `/api/internal/v1/*`。
> 两者不能互换。组织、Workspace、项目、企业频道、客服坐席等运营治理资源的
> 当前覆盖范围，见 [endpoints.md 的资源覆盖范围](./endpoints.md#资源覆盖范围)。

---

## 1. 你能集成什么

企业应用通过 IMBoy 签发的**集成凭证（Credential）**，以本 API 完成以下集成域：

| 域 | 能力 |
|---|---|
| 应用与凭证 | 读取本应用信息（INT-01） |
| 外部身份映射 | OA 用户 ↔ IMBoy 用户的绑定 / 解析 / 解绑 / 目录查询（INT-02/03/15/16/17） |
| 企业群组 | 建群、增删成员、改群、成员角色；详情/列表/成员只读（INT-04/05/06/18/19/20/21/26/27） |
| Workspace | 列表/详情（INT-24/25），创建/修改/软归档（INT-37..39） |
| 企业文件 | 直传预签名 / 确认入库 / 治理（INT-07/08/22） |
| 消息 | 应用身份直发、以人类身份代发；单聊与群聊（INT-09/10） |
| 好友申请 | 代 OA 用户发起（只发起、不自动通过，见 §6）（INT-11） |
| Webhook | 登记出站回调、查询/重放投递、连通性测试（INT-12/13/23/32） |
| OA SSO | 一次性 code 原子交换登录态（INT-14）；入站登录签票（INT-44） |
| 来源目录 | 显式增量同步（INT-43），组织/部门/人员核对（INT-45..47），成员离职检查及停用/恢复（INT-48/49） |
| 客服坐席 | 列表、详情、开通和调整配置（INT-33..36）；不签发坐席身份 |
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

1. 平台管理员创建 OA 连接并签发集成凭证。
   完整凭证形如 `ib_int_...<定位符>.<secret>`，secret 仅在签发/轮换时返回一次。
2. 设置部署方提供的 `BASE_URL` 和环境变量 `IMBOY_CREDENTIAL`，按下列标准库示例读取应用自身。
   正式环境使用 HTTPS；secret 留在 OA 服务端，不放进浏览器或移动端。
3. 之后按 [endpoints.md](./endpoints.md) 与 `../../openapi-internal.yaml` 开发。
   随附 Postman 集合自动生成签名，每次发送均使用新 nonce。

```python
import hashlib, hmac, os, secrets, time, urllib.request

credential, secret = os.environ["IMBOY_CREDENTIAL"].split(".", 1)
algorithm = "sha256"
method, path, query = "GET", "/api/internal/v1/application", ""
body, content_type, idempotency_key = b"", "application/json", ""
timestamp, nonce = str(int(time.time())), secrets.token_urlsafe(24)
canonical = "\n".join([
    "IMBOY-INTERNAL-V1", algorithm, credential, "", method, path, query,
    content_type, idempotency_key, hashlib.sha256(body).hexdigest(), timestamp, nonce,
]).encode("utf-8")
signature = hmac.new(secret.encode("utf-8"), canonical, algorithm).hexdigest()
headers = {
    "Content-Type": content_type, "X-Imboy-Credential": credential,
    "X-Imboy-Timestamp": timestamp, "X-Imboy-Nonce": nonce,
    "X-Imboy-Signature": algorithm + "=" + signature,
}
if idempotency_key:
    headers["Idempotency-Key"] = idempotency_key
url = os.environ["BASE_URL"].rstrip("/") + path + ("?" + query if query else "")
request = urllib.request.Request(url, data=body if body else None, headers=headers, method=method)
class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, *args): return None
with urllib.request.build_opener(NoRedirect()).open(request, timeout=30) as response:
    print(response.read().decode("utf-8"))
```

写请求把 `method/path/query/body` 换成实际值，正文编码一次后原样发送；
按端点要求填写稳定的 `idempotency_key`。重试时复用幂等键、重新生成时间戳、nonce 和签名。

## 3. 请求签名认证

每次请求发送以下四个签名头：

| 头 | 内容 |
|---|---|
| `X-Imboy-Credential` | 签发返回的完整 `ib_int_...` 定位符，即完整凭证第一个 `.` 之前的部分 |
| `X-Imboy-Timestamp` | Unix 秒，十进制、无前导零；与服务端时间相差不超过 300 秒 |
| `X-Imboy-Nonce` | 每请求随机生成，16..128 字符，仅 `A-Z a-z 0-9 _ -` |
| `X-Imboy-Signature` | `sha256=<64位小写hex>` |

签名采用 **HMAC-SHA256**，不是不带密钥的普通 hash。
HMAC key 是完整凭证第一个 `.` 后的 secret 原文；定位符不是 application_id，不能自行重建。

签名原文为以下 12 行 UTF-8 字节，以单个 LF（`\n`）分隔，末尾无换行：

```text
IMBOY-INTERNAL-V1
sha256
credential 定位符
X-Imboy-Organization 原值（未指定时空行）
大写 HTTP 方法
原始路径（不含 ?）
原始 query（不含 ?，无 query 时空行）
Content-Type 头的原值（无此头时空行）
Idempotency-Key 头的原值（无此头时空行）
正文原始字节的 SHA256 小写 hex
X-Imboy-Timestamp
X-Imboy-Nonce
```

- query 不排序、不解码，正文不重新格式化；空正文使用 `SHA256(b"")`。
- 同一凭证与 nonce 只接受一次，去重由 PostgreSQL 原子执行，跨进程、跨节点有效。
  过期、未来超过窗口、重放、签名错误均返回 `invalid_credential`（401）。
- 平台检查凭证有效期、撤销状态与应用状态；应用停用/归档仍返回 `application_disabled`（403）。
- secret 使用已有数据库主密钥 AES-GCM 加密保存，不返回、不写日志；不可逆摘要不作为 HMAC key。
  主密钥未配置时不能签发新的签名凭证；配置沿用 `postgre_aes_key`，轮换兼容 `postgre_aes_key_old`。

English: HMAC requests authenticate the integration using a shared secret, timestamp and single-use
nonce. Raw method, path, query, content type,
idempotency key and body are signed.

## 4. 业务数据校验

`X-Imboy-Organization` 可选，填写目标业务组织的十进制 ID，并参与签名。
未指定时使用应用的业务归属；无归属的来源连接在仅有一个有效已建立组织时自动定位该组织。
多个来源组织时须显式选择。平台只接受应用原归属或管理员已配置且当前有效的来源组织，
不会因请求头指定其他 ID 就放开访问。来源目录接口仍以正文/query 的外部组织标识寻址。

签名验证通过后即可调用本集成的 Internal API，无逐接口权限表、无通配权限配置。
平台继续校验资源所属组织/工作区、应用创建来源、成员有效状态、版本和幂等关系。
个人域资源不会因此变成企业资源，应用不能冒充未映射或已停用的成员。

首次同步组织仍使用管理员配置的可信来源、外部组织标识与确认的 Human Owner；
这是创建业务对象的来源事实。来源停用或到期后，
其目录同步、核对和成员状态变更立即拒绝；组织/群组/频道的既有归属检查继续执行。

## 5. 限流与幂等

**限流**：三只独立桶 —— `internal_read`（读）/ `internal_write`（写）/
`internal_sso`（SSO 交换与入站签票）。超限返回 `rate_limited`（429），并携带
`Retry-After` 响应头（delta-seconds＝窗口剩余时间向上取整，最小 1s；
v1.1.1 起提供）；具体阈值由部署配置，
配置或存储不可用时返回 `security_gate_closed`（503）。

**幂等**：所有标记 `required` 的操作必须携带 `Idempotency-Key` 头，
key 为 **1..128 个可打印 ASCII 字符**（`0x20..0x7E`，不含控制字符）。
幂等作用域为 `(organization_id, application_id, key)`，默认有效期 **24 小时**；
部署可通过 `enterprise_internal_idempotency_ttl_seconds` 设置正整数秒数。
窗口到期后，相同 key 的下一次请求会被视为新请求，不能依赖它永久去重。

窗口内重试必须保持相同方法、具体路径与规范化 JSON 内容；对象键顺序不影响
幂等摘要，浮点数或不可规范化正文被拒绝。不要混淆两种摘要：HMAC 签名始终
使用原始正文字节；每次重试须重新生成时间戳、nonce 和签名。
普通已完成写请求重放首次提交的 HTTP 状态和 JSON 正文字节，并附
`Idempotent-Replayed: true`；当前凭证及业务归属检查仍执行，重放不重复业务写入和审计。
同 key 换方法、路径或内容返回 `idempotency_conflict`（409）。
INT-07 附件能力是例外：幂等记录保存登记事实，重放仍重新授权、读取当前会话并签发能力；
会话确认、过期或配置不可用时，响应可变化或返回 409/410/503，不能缓存首次上传能力长期使用。
以端点 `x-imboy-idempotency` 为准：读取用 POST 也可以是 `not_required`；
INT-14 的 code 本身一次性（`single_use_code`）；INT-44 不要求该头，票据单次消费。

## 6. 消息发送者语义（`sender_mode`）— 重要

企业托管消息支持两种发送者形态，**请求里显式声明，平台分别校验发送者身份**：

| sender_mode | 语义 | 身份校验 |
|---|---|---|
| `application` | 以应用身份发出（展示为应用/机器人） | 本应用的有效 principal |
| `human` | 应用**代**某企业用户发出（该用户须为本组织已映射、已启用用户） | 本来源映射且当前有效的 Human |

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
{ "error": { "code": "invalid_credential", "message": "..." } }
```

通用模型含 15 个稳定错误码（`code` 为机器可读、snake_case、**长期稳定**，勿按 message 文案做逻辑）：

| code | HTTP | 含义 |
|---|---|---|
| `invalid_request` | 400 | 请求参数/形状非法 |
| `invalid_credential` | 401 | 凭证缺失/格式错/不存在的 locator |
| `credential_expired` | 401 | 凭证已过期 |
| `application_disabled` | 403 | 应用被停用/归档 |
| `organization_disabled` | 403 | 所属组织被停用 |
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

INT-23 还保留一个端点特定的兼容错误：旧 `page`/`size` 参数返回 HTTP 400
`cursor_required_v1`，详见该端点 schema；通用错误枚举不代表每个端点的全部例外。

## 8. Webhook 出站（你方系统将收到的回调）

出站投递使用独立的 Webhook HMAC secret，与 Internal API 的入站签名凭证不同。通过 INT-12（`PUT /webhook`）设置 `rotate: true` 轮换签名密钥，首次成功响应返回新 secret；同幂等键的授权重放仍返回该响应快照。普通读取不返回 secret；轮换 API 凭证不会轮换此密钥。接收方更新为新签名密钥后，旧密钥生成的签名不能通过验证。

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

## 9. OA 双方向登录与账号关联

### IMBoy → OA：已登录 IMBoy 用户免登 OA

Human 客户端使用 JWT 调用 `GET /api/v1/workbench/entries` 发现实例内 OA 应用；
旧客户端携带的 organization_id 查询参数会被忽略，发现不依赖组织成员关系。
多应用时明确选择 application_key；没有可用应用时返回空列表。

Human JWT 调用 `POST /api/v1/oa/sso/code`，提交 application_key、预注册 redirect_uri
与随机 nonce，取得 60 秒一次性 code。支持按需取码的 App 容器也可经 getAuthCode 获取。
OA 服务端以该应用 Credential 调 INT-14，提交 code、相同 redirect_uri、nonce。
code 绑定应用、用户、redirect 和 nonce，单次消费；不强制组织或外部映射。

INT-14 成功返回裸身份事实，保留 organization_id 字段：有业务归属时为精确 JSON int64，
无归属为 null；此字段不表示授权范围。外部映射可选，缺省回退平台 user_id 的十进制字符串。
响应不含 IMBoy JWT 或 OA Cookie。OA 校验 state 并建立自己的会话，应用凭据只留 OA 服务端。

### OA → IMBoy：登录原账号、确认关联或显式新建

OA 后端先认证员工，然后通过 HMAC-SHA256 签名调用
`POST /api/internal/v1/oa/login/tickets`（INT-44），提交 external_user_id、intent=login|link、
S256 code_challenge。verifier 由 IMBoy 发起客户端生成并保留，OA 只接收 challenge。
票据绑定应用、发行凭据、外部人员、意图和 challenge，60 秒单次消费。

发起客户端携带本地预期 application_key、ticket、code_verifier、非空 did 调用公开
`POST /api/v1/oa/login/exchange`。已关联身份登录原账号；未关联或 link 意图返回
pending_token，完成窗口 5 分钟且重试不续期。后续二选一：

- 已登录 Human 用自身 JWT 调 `POST /api/v1/oa/login/link`，证明当前账号和 OA 身份；
- 用户明确选择新建后调公开 `POST /api/v1/oa/login/register`，要求 confirm_create=true。

两个完成入口均须携带 application_key、pending_token、code_verifier、did。不按姓名、
手机或邮箱自动合并；旧关联不能被普通同步或登录换绑。目录同步本身不签发会话。
目录投影依据可信来源与当前人员状态；来源尚未就绪时，返回 projection_pending=true。

Internal 签票成功使用 `{code:0,msg,sv_ts,payload}` 信封；错误使用 Internal error 信封。
三个 Human 入口使用客户端业务信封及各自认证要求，不能向它们传 Application Credential。

English: Outbound SSO and inbound OA login are separate protocols. Organization ownership is optional;
source-directory access still requires a current, trusted source configuration. Tickets use client-generated PKCE S256,
application and device binding, short expiration and one-time consumption. Never expose credentials in H5.

## 10. 端点参考与 Postman 集合

- 人类阅读：[endpoints.md](./endpoints.md)（49 个 Internal 操作，按域分组）。
- **机器契约**：`../../openapi-internal.yaml`（编辑真源）与
  `../../openapi-internal.bundle.yaml`（bundle 单文件）——字段级请求/响应
  schema 逐端点从 handler 实证（`src/api/enterprise_*_handler.erl`），
  15 个稳定错误码为可复用 response 组件；限流桶 / 幂等要求以
  `x-imboy-rate-bucket` / `x-imboy-idempotency` 扩展字段
  标注。工具导入用 bundle 单文件。
- **动手联调**：[IMBoy-Internal-API-v1.postman_collection.json](./IMBoy-Internal-API-v1.postman_collection.json)
  —— Postman / Apifox 直接导入（Collection v2.1），已含全部 49 个 Internal 操作、按域分文件夹、
  示例请求体与 base_url / credential 等变量；另含 3 个 Human 入站伴随请求，其认证分别配置。目录外部组织标识须对应可信来源配置，不能只替换域名即全量运行。
  集合收录全部 Internal 冻结路由及上述 Human 伴随请求，不包含尚未实现的接口。
  注意：示例体为合成数据（ID / 域名 / object_key 均为占位值，替换后使用）；
  首页请求省略 cursor，翻页时填上一次真实响应的 next_cursor。

## 11. 版本与变更

- 本 API **已冻结为 v1**：只会追加新端点/新可选字段，不会破坏性变更既有形状。
- 变更记录见 [CHANGELOG.md](./CHANGELOG.md)。

## 客服坐席集成 / Customer service seats

INT-33..36 管理企业坐席配置，不签发 Seat JWT。坐席主键为 business_identity_id，必须对应本企业 customer_service 业务身份；workspace_id 只是审计位置。签名请求按目标企业与有效业务身份校验。PATCH 必须带 expected_version 以及 enabled/max_concurrent 至少一个；停用用 enabled=false，不删除历史。列表默认 limit=50，最大100，按 ID 升序，包含停用项。应用、凭证或企业停用后，包括幂等重放在内的下一请求仍须通过授权检查。

English: Seats belong to the organization. Workspace IDs select audit locations only. Updates require optimistic versions; disabling retains history. Signed cursors bind organization, application and seat-list family.

工作空间写管理：INT-37..39，签名验签后按业务归属校验。创建指定本企业 Owner/Admin；修改与软归档使用 expected_version；默认归档须显式同企业替代项及其业务归属。GET 工作空间追加 version。应用审计、资源及幂等响应同事务；归档后的同 key 重放仍需当前有效凭证与业务归属。


企业频道写管理：INT-40 POST /channels（workspace_id、creator_user_id、name；可选 description/avatar）、INT-41 PATCH /channels/{channel_id}（expected_version 及至少一个资料字段）、INT-42 DELETE /channels/{channel_id}（expected_version）。创建者须为有效企业成员（企业 Owner 包含）及工作空间 Owner/Member，Guest 不可创建；复用现有 20 个有效管理频道配额。创建固定非公开、免费且邀请制（visibility=1/access_type=0/join_policy=1）；归属、创建者及策略不允许在此接口修改。GET 增加 version；资料或治理状态更新增加版本，订阅计数等派生更新不增加版本。归档 status=0，消息、订阅及管理员关系保留。当前凭证与业务归属在重放前验证，停用后的同 key 重试拒绝；应用 actor 与幂等记录同事务。

English summary: HMAC channel writes validate attribution; the target Workspace must match the application business attribution. Creation checks current Organization and Workspace membership, preserves the existing channel quota, and creates a non-public channel. Profile changes and soft archive require an optimistic governance version. Audit and byte-exact authorized replay are atomic; archive retains history and identities.

## 来源目录同步与核对 / Source directory reconciliation

INT-43 同步一次最多 100 个部门和 100 个人员，整批同事务；未包含的记录不删除。
按外部稳定 ID 和各资源 source_version 处理，不按名称匹配。幂等重试须保留同 key 和同正文；
业务版本/状态冲突先核对，不盲目重试或增加版本覆盖。部门循环为 400，归档父部门、非空部门、
Owner/任职状态冲突为对应 403/409；数据库未知故障仍为 500。跨批不具有整体原子性。

INT-45..47 为 GET，经 HMAC-SHA256 签名认证，只要求当前有效的来源配置与应用/凭据实时状态，不设独立读写权限：
organizations 返回已建立组织且来源配置当前有效 的本来源组织；
departments/persons 必传 external_organization_id。尚未 bootstrap 的来源配置仅在可信 Admin 查看。
逐请求及 SQL 读取时复核应用、凭据、来源、授权有效期与目标组织状态，撤权后的旧 cursor 也重验。

分页使用 page_size（默认 50、最大 100）和不透明 cursor，按外部 ID 的 UTF-8 字节序升序。
返回 items/page_size/has_more/next_cursor；游标绑定来源应用、组织、页族与完整过滤，24 小时有效。
禁止重复/未知参数；实时分页不是一致性快照。响应 Cache-Control: no-store，不提供无界导出。

persons 区分 binding_status=unlinked|linked|retired 和
projection_status=unlinked|pending|projected|removed|retired；后者同时核对当前账号、组织成员与部门关系。
只返回该来源在目标组织有任职记录的人员，包含待关联/待投影/移除记录，不枚举全实例用户。
来源人员 status 与组织任职 status 各自版本独立；只离职一个组织不得退休整个来源账号关联。

English: Directory sync is bounded, incremental and atomic per batch. Reconciliation GETs require only
a current source configuration and live application/credential status. Pending account associations and
membership projections are distinct. Cursors are signed and filter-bound; pages are live, not snapshots.


## OA 企业与成员生命周期

1. 平台管理员先创建应用、签发凭据并配置可信目录来源：绑定外部组织 ID、Owner、到期时间及是否准许首次建组织。OA 不能自行指定任意 Owner 或越过可信配置。
2. `POST /api/internal/v1/directory/sync`（INT-43）首次初始化组织，再按各自递增版本同步部门、人员和任职。全量中遗漏的记录不会自动删除；离职须显式发送 `membership.status=removed`。来源人员与组织任职版本独立。
3. 未关联人员进入待关联状态；通过既有可信身份关联/注册流程关联 Human 账号后投影成员及部门关系。OA 不能凭任意用户 ID 接管账号。使用 INT-45..47 核对来源与实际投影状态。
4. `GET /api/internal/v1/directory/memberships?external_organization_id=...&external_user_id=...`（INT-48）返回当前成员状态、`member_version`、离职阻塞数量及业务资格。目标组织必须属于当前有效的可信来源。检查是建议，实际变更会重新验证。
5. 离职先用 `PATCH /api/internal/v1/directory/memberships`（INT-49）发送外部组织/人员 ID、`status=suspended`、检查返回的 `expected_member_version`，并携带 `Idempotency-Key`。只支持本来源托管、已关联投影的普通成员；Owner/Admin 交由人工治理。
6. 停用保留账号、历史及其他组织关系。旧 JWT 和现有连接访问所属 Workspace、项目、群组及频道时仍即时复核资格；恢复后复用原成员与订阅关系，个人域及其他组织不受影响。先交接工作区/项目/群组/频道所有权、未完成任务，解除业务身份分配，再以 INT-43 显式移除任职。实际移除仍受交接检查保护。仅离开一个组织时保持 `source_status=active`，避免退役跨组织关联。
7. 取消离职时重新检查，以新版本和新幂等键 PATCH `status=active`。只能恢复本来源暂停且未被人工再次修改的成员；人工暂停、角色提升、旧版本或来源不匹配均拒绝。409 后重新读取并处理冲突，不能盲重试覆盖。

成员状态修改、暂停来源标记、审计及幂等结果同事务；撤权后旧请求重放也重新鉴权。读取和修改响应禁止缓存。64 位 `member_version` 必须按 int64 无损处理。

当前部门契约不包含负责人、主部门、岗位和排序；这些字段需按广州 OA 的真实字段及治理规则确认后另行扩展。
