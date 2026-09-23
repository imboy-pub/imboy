# Internal API v1 — 变更记录

本 API 遵循「冻结 + 只追加」纪律：v1 内不做破坏性变更（不改既有路径语义、
不删字段、不收紧既有错误码）；破坏性演进将另开 v2 目录并行。

## v1.0.1 — 2026-09-23（文档澄清）

- 增加企业数据 CRUD 覆盖审计，明确组织、Workspace、客服坐席、企业频道、
  企业群、企业项目的已实现范围与待补合同。
- 明确 `/api/adm/*` 运营治理面与 `/api/internal/v1/*` 企业集成面不能互换。
- Postman 集合继续只收录冻结路由表中 23 个已实现端点，不加入调用必然 404 的合同草案。

## v1.0 — 2026-09-22（冻结）

- 首个冻结版本：23 个端点（INT-01..INT-23），10 个 scope，13 个稳定错误码。
- 域：应用与凭证 / 外部身份映射 / 企业群组 / 企业文件直传 / 消息（应用直发
  与人类代发，固定非 E2EE）/ 好友申请（只发起不自动通过）/ Webhook（含投递
  查询与重放）/ OA SSO 一次性 code 交换。
- 与 Admin 面（`/api/adm/*`）、人类面（`/api/v1/*`）三前缀互不相交；
  不存在 Open Platform 公网面（`/api/open/v1/*` 生产路由 = 0）。

### 真源与守护

- 路由/授权冻结表：`src/api/enterprise_internal_routes.erl`（+ `enterprise_internal_boundary.erl`）
- 字段级 schema：`.contract/api/openapi.yaml`（`api_internal` 组）与 `.contract/api_contract.json`
- 机械守护：`scripts/check_enterprise_release_manifest.py`（12 项断言：
  冻结表 ↔ cowboy 路由 ↔ OpenAPI ↔ 接线测试逐条相等；`/api/open/v1` 生产面 = 0）
- 本目录为面向集成方的交付文档；与真源冲突时以真源为准，并视为文档缺陷修复。
