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
