# Internal API v1 — 变更记录

本 API 遵循「冻结 + 只追加」纪律：v1 内不做破坏性变更（不改既有路径语义、
不删字段、不收紧既有错误码）；破坏性演进将另开 v2 目录并行。

## Unreleased — Workspace writes

- 新增 INT-37..39 创建、修改及软归档；独立 workspaces:write，GET 投影追加 version。
- 迁移 160 为所有工作空间修改自动增加版本；回退遇新授权或版本证据时拒绝静默丢弃。
- 默认归档显式交接，应用审计与幂等同事务。


## v1.2 候选 — 2026-10-01（未发布 / unreleased）

- 新增 INT-33..36 企业坐席配置，28 path / 36 端点。
- 新增 customer_service:read/write 显式权限和 3 个稳定 409 错误码。
- 企业全域 Grant、事务内幂等与应用审计；PATCH 版本校验和停用保留历史。
- Migration 159 不自动授予权限；存在新权限或用量时拒绝 down，不删数据。
- English: Unreleased additive seat governance contract. Local HTTP conformance passes; production release remains pending.

## 2026-10-01 — 文档修正（协议与路由不变）

- README §9 修正 OA SSO 方向：已登录 IMBoy 的 Human 客户端签发 code，
  OA 服务端凭 Application Credential 交换身份并建立 OA 自己的会话。
  INT-14 不签发 IMBoy JWT 或 OA Cookie；补充 60 秒、单次消费、绑定字段
  与 Credential 仅限服务端的说明。
- README 机器契约计数同步当前 INT-01..INT-32：26 path / 32 端点。

## v1.1.1 — 2026-09-28（文档澄清：Webhook 交付细节补齐）

- **修复 README §2 失效指针**：快速开始第 3 步原指向「§5 的端点表与
  `.contract/api/openapi.yaml`」——README 无端点表（§5 为限流与幂等），
  端点表在 endpoints.md；机器契约为 `api/openapi-internal.yaml`（bundle
  单文件可导入）。属文档面 drift 修复，冻结合同无变化。
- **README §8 补齐 Webhook 交付细节**（此前集成方无法从交付文档获知）：
  - 可订阅事件白名单 4 值：`message.enterprise.accepted` / `message.enterprise.failed`
    / `file.confirmed` / `group.member.changed`（末者为白名单预留、当前版本
    无触发点，订阅合法但暂不投递）；
  - 回调正文信封结构（8 键封闭、version=1、occurred_at 为 ISO-8601 UTC 毫秒）；
  - 签名原文 = `<timestamp> "." <raw body>`（HMAC-SHA256 hex）；
  - 重试节奏 5/30/300s 三次退避后入死信。
- 如实标注两个既有语义边界：`resource.id` 为平台消息表行 ID，与 INT-09/10
  响应的 `msg_id` 是不同标识符（信封无 correlation_id）；`message.enterprise.failed`
  当前信封不含失败原因（reason_code 不外显）。
- endpoints.md Webhook 节同步事件枚举指引（单一真源指向 README §8）。
- **Postman 集合示例体修正（12 处与契约不一致）**：INT-02（去契约外
  `workspace_id`）、INT-03（`external_user_id` → `external_user_ids` 数组）、
  INT-04（`external_user_ids` → `members`）、INT-07/08（补缺失的整个请求体：
  `file_name`/`mime_type`/`object_key`）、INT-09（`to_id` → `recipient_user_id`、
  补 `msg_type`）、INT-10（补 `msg_type`）、INT-11（`external_user_id` →
  `sender_user_id`+`target_user_id`）、INT-12（`webhook_url`/`name` →
  `url`+必填 `events`）、INT-14（补 `redirect_uri`/`nonce`）、INT-20
  （`external_user_ids`+`roles` 平行数组 → `roles:[{external_user_id,role}]`）、
  INT-22（`file_id` → `op`+`object_key`）。端点数与 URL 集不变（31）。
- **OpenAPI 机器契约补充枚举**（不改变路由/字段，纯收紧文档表达）：
  INT-12 请求与响应的 `events`、INT-23 的 `status` 过滤参数与响应
  `event_type`/`status` 均补白名单/状态机 enum；bundle 与聚合入口已重新
  生成（`flatten_internal.py` / `gen_aggregate.py`，--check 通过）。
- **429 追加 `Retry-After` 响应头**（纯追加，信封体与状态码不变）：
  服务端在超限拒绝时返回 delta-seconds（＝限流窗口剩余毫秒向上取整，
  下限 1s，per_minute 桶 ⇒ 1..60）。实现：`enterprise_internal_rate` 已有
  的窗口剩余值经 `enterprise_internal_auth`（`take_retry_after_seconds/0`，
  一次性取出即清）传递给 `enterprise_internal_middleware`，经
  `enterprise_internal_error:reply/3` 附加；`decide/4` 返回契约
  （A2 冻结测试按 atom 断言）不变。新增 eunit
  `middleware_rate_limited_retry_after`（秒数边界 + 一次性语义）。
- **新增端点 INT-32 `POST /webhook/test-delivery`**（连通性测试，31→32 端点）：
  向当前配置的出站端点投递合成 `webhook.ping` 事件，走与真实事件完全相同的
  入箱/SSRF pin/签名/重试管线（INT-23 可查、INT-13 可重放）。scope 复用
  `webhooks:manage`、幂等 required；`webhook.ping` 事件类型**不可订阅**
  （订阅白名单保持 4 值），仅本端点产生。实现：`enterprise_webhook_logic:
  emit_ping_tx/2`（独立路径：不检查订阅、错误上抛、返回 delivery_id）+
  `enterprise_webhook_handler:test_delivery/3`；路由/boundary/manifest/
  OpenAPI/Postman/接线测试（正例 + 幂等矩阵第 17 条）全链同步。
- **INT-09/10 响应追加可选字段 `webhook_resource_id`**（纯追加）：
  与 webhook 事件 `resource.id` 同源的消息表行 ID，补齐「回调 ↔ 发起
  响应」关联缺口（此前集成方只能靠自身业务记录关联）；实现位于
  `enterprise_message_logic:finish_audit/10` 响应 map（单聊/群聊共用，
  恒返回）。README §8 关联指引同步更新。

## v1.1.0 — 2026-09-24（V2.1 只读扩面）

- 新增 8 个只读 GET 端点 **INT-24..31**：Workspace 列表/详情（24/25）、
  企业群列表/成员列表（26/27）、企业项目列表/详情（28/29）、企业频道
  列表/详情（30/31）。全部 `Idempotency-Key` 不要求，限流桶 `internal_read`。
- **INT-18 scope 修正**：群详情（GET）`groups:write` → `groups:read`
  （与 routes registry / boundary spec 一致，属文档面 drift 修复）。
- 面收敛：cowboy path 19 → 25、端点 23 → 31、scope 10 → 14
  （migration 00000144 CHECK 扩到 14 值）。
- Postman 集合同步补齐 8 条只读请求（31 端点），见 README §10；
  本文件「真源与守护」指针同步修正为 `api/openapi.yaml` 聚合入口。

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
- 字段级 schema：`api/openapi.yaml`（聚合入口，`api_internal` 组）与
  `.contract/api_contract.json`
- 机械守护：`scripts/check_enterprise_release_manifest.py`（12 项断言：
  冻结表 ↔ cowboy 路由 ↔ OpenAPI ↔ 接线测试逐条相等；`/api/open/v1` 生产面 = 0）
- 本目录为面向集成方的交付文档；与真源冲突时以真源为准，并视为文档缺陷修复。
