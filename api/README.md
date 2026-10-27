# IMBoy API 与协议契约 / API and protocol contracts

本目录描述当前实现的接口。契约中存在一个端点，不代表当前部署 profile 已启用它，
也不代表已通过真实请求、权限负例或端到端验收。
A documented endpoint is not necessarily enabled or qualified by runtime acceptance tests.

## 查阅入口 / Entry points

| 受众 | 契约 | 认证与响应 |
|---|---|---|
| APP 与业务客户端 | [openapi-api.yaml](openapi-api.yaml) | 通常 Access JWT；公开、Bot、访客和 Seat 凭证见具体端点 |
| 管理后台 | [openapi-adm.yaml](openapi-adm.yaml) | Admin Cookie 会话；登录和 setup 等入口显式公开 |
| 企业集成 | [openapi-internal.yaml](openapi-internal.yaml) · [集成指南](internal/v1/README.md) | HMAC 请求签名；裸成功对象／稳定 error，OA票据成功信封例外 |
| WebSocket | [asyncapi.yaml](asyncapi.yaml) · [协议契约](../docs/reference/ws-protocol-contract.md) | `/api/v1/ws`；有效 Access JWT 与编码子协议 |
| 公共响应 | [Envelope.yaml](components/schemas/Envelope.yaml) · [格式说明](../docs/reference/api-format.md) | `code/msg/sv_ts/payload`，具体载荷由端点定义 |
| Protobuf 生成 | [codegen/README.md](codegen/README.md) | WebSocket Protobuf 载荷生成，与 REST 客户端生成分开 |

`paths/<api|adm|internal>/v1/` 保存端点，`components/` 保存可复用定义。
目录中的 `v1` 是文档定位，不保证 URL 包含 `/v1`；请求路径以受众 root 的 `paths` 为准。
Use the root's path keys and operation security requirements when calling an endpoint.

## 编辑与校验 / Editing and validation

编辑对应受众 root 及引用文件。`openapi.yaml` 是生成的三受众聚合桥接；
`openapi-internal.bundle.yaml` 是 Internal 单文件交付物，二者均不手改。
`gen_aggregate.py` 保留受众认证差异：默认认证不同的面必须在 leaf 操作中显式声明
`security`，防止聚合后继承客户端 JWT。兼容别名也必须使用唯一 `operationId`。

需要 Python 3 + PyYAML 6.0.2 + jsonschema 4.25.1、Node 24 及 Redocly CLI 2.70.0（与 CI 一致）。
在后端仓库根目录运行：

```bash
# 修改后生成
python3 api/gen_aggregate.py
python3 api/flatten_internal.py

# 检查当前产物
python3 api/gen_aggregate.py --check
python3 api/flatten_internal.py --check
python3 -m unittest discover -s test/scripts -p 'test_api_*.py'
redocly lint api/openapi-api.yaml api/openapi-adm.yaml api/openapi-internal.yaml --config api/redocly.yaml
python3 scripts/check_enterprise_release_manifest.py
```

生成一致性、schema lint 和路由断言是静态检查，不能替代实际 HTTP／权限／业务验收。
Schema and generation checks do not prove runtime authorization or end-to-end behavior.

## 历史消息时间与恢复 / History time and recovery

`GET /api/v1/msg/history` 实际固定 `msg_handler` 的 `action=history`。
Flutter `API.msgBackfill` 的真实路径 `GET /api/v1/msg` 固定分派 `action=history_backfill`，
兼容现有客户端传入的 `action=history_backfill` query；该参数不切换路由，也不是新增协议。
补扫参数、错误与授权见 [`backfill.yaml`](paths/api/v1/msg/backfill.yaml)，wire 例子见
[`HistoryBackfill.yaml`](components/responses/HistoryBackfill.yaml)。
Both actions require the JWT user's conversation scope and current open group membership generation;
`did` only filters device envelopes and never grants access.

| 字段 / Field | wire 与权威语义 / Wire and authority |
|---|---|
| `sv_ts` | 信封生成时间，UTC Unix 毫秒；不是消息接受时间 / Response time in epoch ms |
| `ArchivedMessage.created_at` / `server_ts` | 既有 Unix 毫秒合同；`server_ts` 可空 / Existing epoch-ms projection |
| `ArchivedMessage.accepted_at` | 服务端首次接受事实，字段必填、nullable int64 Unix 毫秒；拒绝 RFC3339 字符串，旧行未知保持 null / Required nullable epoch-ms first acceptance instant (D04) |
| `safe_head` / `blocked` / `gaps` | 同快照安全头（0 合法）、扫描越界阻塞标志、明确授权或终态空洞 / Safe progress metadata |
| backfill `range_limit` | 每次扫描跨度 1000，不拒绝总历史跨度大于 1000；不是 query 参数 / Per-call scan span |

REST 时间类型遵循冻结 D04，见[时间字段合同](../docs/reference/time-field-contract.md#22-消息核心时间四元组)和[项目约定](../docs/CONVENTIONS.md)。数据库 `timestamptz` 不改变 REST 毫秒整数合同。

补扫窗口为授权下界钳制后的 `(after_seq, min(safe_head, after_seq+1000)]`，查询带 SQL 上界和
`limit+1`；未满页或窗口剩余行已耗尽时，`next_seq` 推进到窗口终点。`has_more` 表示尚未扫描到
安全头，设备过滤后的空消息页不能用作终止条件。已有过冲游标不自动回退；从本地确认的恢复起点
重扫，按账号/会话隔离 checkpoint，按 `msg_id` 去重且持久化成功后再推进。不要从未知旧时间猜造
now/1970 或统一加八小时。`backfill/6` 的 bounded-window 实现与本合同须由最终集成候选共同验收。
Persist idempotently before advancing a conversation checkpoint; unknown legacy timestamps remain unknown.

## 设计与兼容规则 / Design and compatibility

- 文档必须跟随真实路径、方法、认证、HTTP 状态、业务码和最终 JSON 类型。
  修正文档不意味着迁移运行时协议；已发布字段和路径按兼容策略演进。
- 新接口优先资源名、明确的 HTTP 方法与状态语义；保留必要动作端点。
  不为统一命名自行更改存量 URL、ID 类型、成功信封或错误处理。
- 参数写明必填、类型、范围、默认值、空值与业务资格；有数据载荷时提供具体 schema。
  无载荷或可扩展对象按真实语义声明，不用开放 object 掩盖未知字段。
- 列表按实际实现声明 offset／cursor 参数、返回容器、排序与边界行为。
  前端每页 10 条的默认体验不等于后端默认值；需要时显式传参。
- 实体 TSID 保留完整精度；普通业务消息 ID 是不透明字符串。
  以 [TSID 约定](../docs/reference/tsid-field-convention.md) 及端点最终序列化为准。
- `organization` 是业务对象；归属校验、职能权限和部署实例的授权边界分别按实现描述。
- 条件编译、功能开关和专用凭证必须明确标注。完整路由来源是
  [`imboy_router.erl`](../src/imboy_router.erl) 及其委托路由模块；OpenAPI 不自动证明所有动态注册接口均已收录。

## 范围外的 HTTP 内容 / Non-API HTTP content

法律页面 `/account-deletion`、`/privacy-policy` 和 `/static/[...]` 由 `cowboy_static`
提供；开发调试路由不作为客户端契约。其他页面、流和握手只在存在明确接入需求时
按其实际 `text/html`、`text/event-stream`、字节流或 HTTP 状态描述。
不要仅因响应不是 JSON 就把已有集成入口排除，也不要为路由清单补不存在的 CRUD。

执行计划、临时输出和测试结果保留在仓库外。必要的契约、脱敏 fixtures 和回归测试
按项目要求维护；本入口不记录任务进度或未经当前候选验证的覆盖率声明。
