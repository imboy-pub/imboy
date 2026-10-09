# 微社群 API 契约 v1 / Micro-community API contract v1

本文件描述当前隔离候选源码的接口，不代表 PostgreSQL、管理后台真实浏览器或 Android 验收通过。路由真源为 `src/imboy_router.erl`，Human 与平台管理员身份必须分离。 / This documents the current source candidate, not a live database, browser or Android qualification. The router is authoritative; Human and platform administrator identities remain distinct.

OpenAPI 编辑真源：[Human](../../../api/openapi-api.yaml)、[Admin](../../../api/openapi-adm.yaml)。本次不修改已有 `/channel/:channel_id/messages` 游标接口，也不更新聚合文件或计划哈希。 / These are the editable OpenAPI sources. Existing message cursors, the aggregate document and plan hash are unchanged.

## 路由 / Routes

| 受众 / Audience | 方法 / Method | 路径 / Path | 输入、结果 / Input and result |
|---|---|---|---|
| Human | GET | `/api/v1/channel/:channel_id/discussion` | 返回 `{discussion_group_id}`，未绑定为 `null` / Binding or null |
| Human | PUT | 同上 / Same | `{discussion_group_id: bigint \| null}`；返回绑定 / Set or restricted unbind |
| Human | DELETE | 同上 / Same | 等价 set null / Equivalent to set null |
| Human | GET | `/api/v1/channel/:channel_id/discussion/candidates` | `page=1,size=10`；`{list,total,page,size}` / Candidate page |
| Human | POST | `/api/v1/channel/:channel_id/discussion/join` | 显式加入；返回 `{discussion_group_id}` / Explicit join |
| Human | GET | `/api/v1/channel/:channel_id/posts` | `page,size,q,tag,essence`；`{list,total,page,size}` / Post search |
| Human | PUT | `/api/v1/channel/:channel_id/posts/:message_id/essence` | `{is_essence: boolean}`；返回 `id,is_essence,is_pinned` / Essence update |
| Human | PUT | `/api/v1/channel/:channel_id/posts/:message_id/tags` | `{tags: string[]}`；返回 `id,payload` / Tags update |
| Admin | GET, PUT, DELETE | `/api/adm/channel/:channel_id/discussion` | 平台读取、绑定、受限解绑 / Platform binding lifecycle |
| Admin | GET | `/api/adm/channel/:channel_id/discussion/candidates` | `page=1,size=10`；`{list,total,page,size}` / Candidate page |
| Admin | PUT | `/api/adm/channel/:channel_id/message/:message_id/essence` | `{is_essence: boolean}`；返回 `id,is_essence,is_pinned` / Platform essence update |

Human 共六种路径，Admin 共三种路径；路径内的冒号参数在 OpenAPI 中使用花括号。候选和新 posts 都使用页码分页：`page >= 1`，`1 <= size <= 50`，默认 `page=1,size=10`，越界拒绝而非自动截断。 / Six Human paths and three Admin paths; OpenAPI uses braces. Candidates and new posts use validated page/size pagination, not cursors.

搜索 `q` 为 UTF-8 字符串，最多 200 Unicode code points；`tag` 为 trim 后的精确标签；`essence` 省略时不筛选，仅接受 `true` 或 `false`。旧 messages 的分页/游标契约保持原样，不把新 posts 的 page/size 当作旧接口改造。 / Search accepts up to 200 Unicode code points, an exact normalized tag, and an optional boolean essence filter. The old messages pagination/cursor contract is preserved.

## 标识符及信封 / Identifiers and envelope

`channel_id`、`message_id`、群 ID 都是 `1..9223372036854775807` 的 bigint。路径用十进制文本，JSON 中规范传输为 integer，`discussion_group_id` 额外允许 JSON `null`。客户端必须在解析响应时保留整数精度，再转换为 `EntityId`；Admin 使用现有 `safeParseBigIntJson`，不得 `Number(id)` 回转。部分现有调用方会提交十进制字符串，当前逻辑兼容该形式；不要由此把服务端响应定义成 string。 / IDs are positive signed 64-bit integers, decimal in paths and integer on the JSON wire. Discussion binding also permits null. Preserve exact precision before converting to client EntityId; do not round through JavaScript Number. Decimal string requests remain a compatibility input, not the canonical response type.

成功信封为 `{code:0,msg:"success",sv_ts:<毫秒>,payload:<结果>}`。内部 atom 键如 `discussion_group_id`、`is_essence` 编码后仍是同名 JSON snake_case 字符串键。 / Successful envelopes contain code 0, server milliseconds and payload; atom keys retain their snake_case JSON names.

这些 handler 调用 `elib_response:error/3`，源码明确最终通过 `reply_json_with_status(200,...)` 返回：**业务 `code=400/403/404/405/409/500/503/980` 不等于 HTTP 状态码**。业务失败 payload 为 `{}`。客户端应检查信封 code；不能仅凭 HTTP 200 宣称成功。认证中间件、协议边界可能返回真实 HTTP 错误，应另行处理。 / Business failures use HTTP 200 with a nonzero envelope code and empty object payload. Authentication and protocol boundaries may use actual HTTP errors. HTTP 200 alone is not success evidence.

| 业务 code / Business code | 含义 / Meaning |
|---|---|
| 400 | 参数、类型、标签或分页无效 / Invalid input |
| 401 | 内容 handler 缺少认证用户 / Missing authenticated Human |
| 403 | 权限、当前权益、一期门或关闭功能拒绝 / Permission, entitlement or feature gate denial |
| 404 | scoped 频道/消息/群不存在 / Scoped resource missing |
| 405 | 不支持该方法 / Unsupported method |
| 409 | 绑定冲突，或非空讨论群试图解绑/换群 / Binding conflict or populated group lifecycle change |
| 500 | 绑定、Admin 事务或审计失败 / Binding or platform transaction/audit failure |
| 503 | 内容/讨论服务或事务异常 / Content/discussion service failure |
| 980 | Human 内容写守卫的归档/禁用拒绝 / Human content write guard denial |

以上是当前模块的业务错误类别，不承诺所有端点都返回表中每一种 code。Admin bridge 对未专门映射的事务错误统一返回 500。 / Error categories vary by endpoint; unmapped Admin transaction errors become 500.

## 权限、绑定和持续授权 / Permissions, lifecycle and continuous access

所有新增微社群入口均受现有 `channel` 功能与 `phase1_delivery` 门控制；一期即使远端旧配置开启 channel，仍拒绝这些新入口。Admin UI 从实时 policy 的明确 profile 和 channel=true 判定，缺失、未知、失败或刷新中均关闭入口。 / All new endpoints require channel capability and reject phase1_delivery. Admin UI uses explicit live policy and fails closed on unknown, missing or failed policy state.

Human 绑定/替换/解绑要求频道管理 role>=2 和群 owner/admin role>=3，频道 creator 与群 owner 必须相同；正数频道管理员行优先，creator 只作无有效管理员角色时的 fallback。群必须 personal、status=1、type=2 私有、join_limit=3 仅邀请、e2ee_mode=0；频道必须 personal、active。**不降级 E2EE，也不将 enterprise/workspace 群伪装为 personal**。 / Human lifecycle operations require dual management and the same creator/owner, respecting existing positive admin-role precedence. Only active personal, private invitation-only non-E2EE groups qualify; there is no E2EE downgrade or workspace scope conversion.

初绑只允许 owner-only 且没有聊天正式表、staging、永久存储、request ledger、附件、公告、文件、相册/照片、任务、日程、投票或 agent task 的群。当前 pristine 谓词也拒绝历史非 owner 成员记录及已软删除内容。解绑/换群要求旧群仍 pristine，新群同样 pristine；非空失败保留原关联。同值重试仍重验权限、归属和结构，但不要求群继续空、不重复审计。 / Initial and replacement groups must be owner-only and unused across all checked storage families, including historical non-owner rows and deleted content. Populated groups cannot unbind or swap; valid same-value retries recheck authorization and structure without duplicate audit.

绑定操作在同一事务内先以固定表顺序获取保守表锁，再获取频道/群/成员及序列锁，关系与 group_log 审计一起提交。该首版策略会短时阻塞被锁表的全局写入；数据库锁失败必须回滚，不能包装成幂等成功。 / Binding uses fixed-order conservative table locks followed by entity/sequence locks, with atomic relationship and audit writes. This briefly blocks global writes to those tables; lock failure must roll back.

Human GET discussion 要求当前 active 订阅；付费频道另须 paid(status=1) 且 `subscription_end_at IS NULL OR > clock_timestamp()`。双端 manager 有明确例外；未授权者不能获取群 ID。浏览、订阅、支付均不会隐式入群，必须 POST join；以后每次群访问和投递仍检查当前权益。退款、退订、到期不能靠旧成员状态、旧缓存或旧 recipient snapshot 继续授权。 / Reading the discussion binding requires an active subscription and, when paid, a valid paid order including perpetual NULL expiry, or a dual-manager exemption. Join is explicit, and subsequent access/delivery rechecks current entitlement rather than stale membership or snapshots.

Admin 使用认证中间件的真实 `adm_user_id`。讨论读取、候选与生命周期要求平台 `channels:update` 和 `groups:update`；精华要求 `channels:update`。事务锁定 active 管理员行并重读角色权限，不把平台管理员 ID 当作 Human UID，也不冒充 creator。绑定 group_log 与精华 user_log 的 actor_kind/admin_id 明确记录真实管理员，审计失败回滚。 / Platform APIs use real authenticated adm_user_id and transactional role checks: both permissions for discussion, channel update for essence. Platform actors never impersonate personal owners, and audits remain atomic.

## 标签和精华 / Tags and essence

发布 payload 缺 `tags` 时保留所有已有字段；专用 tags PUT 必须显式提供 tags。原始数组最多五项，所有项必须是有效 UTF-8 字符串，trim 后每项 1–20 Unicode code points；稳定去重，不改顺序。五个重复项可归一为一项，六个重复项仍拒绝。空数组清空；非数组、非字符串、空白串、无效 UTF-8、过长标签均拒绝。只修改 `payload.tags`，附件与其他 payload 字段保持不变。 / Missing publication tags preserve payload; the tags patch requires tags. Validate at most five raw UTF-8 string items, trim each to 1–20 code points and stable-deduplicate. Six duplicates still fail; [] clears. Other payload fields and attachments survive.

精华 `is_essence` 与置顶 `is_pinned` 是独立布尔字段，切换精华不调用 pin API、不改变置顶。Human 精华要求频道 manager；tags 允许 manager 或在既有编辑窗口内的消息作者。所有消息更新都检查路径 channel_id 与 message_id 的归属，不能跨频道更新。 / Essence and pin are independent. Human essence requires a channel manager; tags permit managers or authors within the existing edit window. All updates are scoped to the path channel and message.

## 验证边界 / Verification boundary

本契约从 handler、logic、DS、repo 和 `elib_response` 源码交叉核对；YAML 可解析不等于运行时验收。真实 PG 权益/锁竞争、管理员审计、浏览器操作、Android 深链和缓存拒绝仍须由主控账本提供证据。禁止将 mock 单测或本文件当作上述验收 PASS。 / Source checks and parseable YAML do not establish runtime acceptance. Live entitlement, concurrency, audit, browser and Android evidence belongs in the controller's acceptance ledger.
