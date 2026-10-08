# IMBoy API 与协议契约 / API and protocol contracts

本目录保存机器可读的接口与协议定义。接口存在于契约中，不代表它在当前交付 profile 中启用，也不代表已通过端到端验收。
This directory contains machine-readable contracts. A documented endpoint is not necessarily enabled by the delivery profile or qualified by end-to-end tests.

## 查阅入口 / Entry points

| 受众 / Audience | 定义 / Contract |
|---|---|
| APP 与业务客户端 / Apps and business clients | [openapi-api.yaml](./openapi-api.yaml) |
| 管理后台 / Admin | [openapi-adm.yaml](./openapi-adm.yaml) |
| 企业集成 / Enterprise integration | [openapi-internal.yaml](./openapi-internal.yaml) · [集成说明 / Integration guide](./internal/v1/README.md) |
| WebSocket | [asyncapi.yaml](./asyncapi.yaml) · [协议说明 / Protocol guide](../docs/reference/ws-protocol-contract.md) |
| 公共响应信封 / Response envelope | [Envelope.yaml](./components/schemas/Envelope.yaml) |
| 代码生成 / Code generation | [codegen/README.md](./codegen/README.md) |

端点文件位于 `paths/<api|adm|internal>/v1/`，公共定义位于 `components/`。
目录中的 `v1` 用于契约定位，不保证请求 URL 带有 `/v1`；请求路径以对应 root 的 `paths` 为准。
Endpoint files live under `paths/<api|adm|internal>/v1/`; shared definitions live under `components/`.
The directory version does not imply a URL prefix. Use the root's `paths` keys.

## 修改与校验 / Editing and validation

编辑对应受众 root 或其引用的端点文件。`openapi.yaml` 是三份 root 的聚合生成物，不直接编辑。
Edit the relevant audience root or referenced path file. Do not edit the generated aggregate `openapi.yaml`.

在后端仓库根目录执行 / Run from the backend repository root:

```bash
redocly lint api/openapi-api.yaml api/openapi-adm.yaml api/openapi-internal.yaml --config api/redocly.yaml
python3 api/gen_aggregate.py
python3 api/gen_aggregate.py --check
```

Redocly CLI 需要单独提供；配置见 [redocly.yaml](./redocly.yaml)。聚合检查仅证明生成物一致，不能代替 schema 校验、权限测试或真实请求验收。
Provide Redocly CLI separately. Aggregate checks prove generated-file consistency only; they do not replace schema validation, authorization tests, or real-request acceptance.

## 数据约定 / Data conventions

- JSON 信封以公共定义为准，字段为 `code`、`msg`、`sv_ts`、`payload`；业务失败须检查 `code`，不能仅看 HTTP 状态。
- ID 必须保留完整精度；各端点的 wire 类型以 schema 和实际处理代码为准，前端解析遵循 [TSID 约定](../docs/reference/tsid-field-convention.md)。
- 分页参数与返回字段按端点定义，不统一假设 cursor 或 `items`。
- 协议背景见 [参考文档索引](../docs/reference/README.md)，业务说明见 [三端对齐约定](../docs/api-contracts/three-platform-alignment.md)。

The common envelope uses `code`, `msg`, `sv_ts`, and `payload`. Inspect business codes as well as HTTP status. Preserve ID precision and follow each endpoint's wire and pagination schema.

执行计划、测试结果、审查报告与运行证据保存在仓库外；本入口不维护历史任务进度或未经当前候选验证的覆盖率声明。
Keep execution plans, test results, review reports, and runtime evidence outside Git. This entry point does not track historical task status or unverified coverage claims.

## 已排除路由 / Explicitly excluded routes

下列路由在 `src/imboy_router.erl` 中存在，但**有意不挂载**到 `openapi.yaml`：

| 路由 | Handler | 排除原因 / Exclusion rationale |
|------|---------|--------------------------------|
| `/account-deletion` | `index_handler` (HTML) | 合规需要的账号注销页，HTML 响应而非 JSON API |
| `/privacy-policy` | `index_handler` (HTML) | 隐私政策页，HTML 响应 |
| `/static/[...]` | `cowboy_static` | 静态资源中间件，不属于业务契约 |
| `/static/admin/[...]` | `cowboy_static` | 管理后台前端 SPA 资源 |
| `/test/req_get` | `test_handler` | 开发调试端点，不暴露给客户端 codegen |
| `/test/req_post` | `test_handler` | 同上 |

如未来需要把这些挂入 OpenAPI（例如静态资源需 SDK 化、或测试端点需契约化），可参考 `paths/api/v1/system/help.yaml` 的 `text/html` content type 模式。

---
