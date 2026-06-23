# API 契约完整性与三端对接一致性审计 / API Contract Completeness Audit

> **日期 / Date**: 2026-06-23
> **范围 / Scope**: 后端 `imboy`(Erlang) ↔ 移动端 `imboyapp`(Flutter) ↔ 管理后台 `imboyadmin`(React)
> **方法 / Method**: 以 `src/imboy_router.erl` 为后端真值源，做四向集合差 + 响应字段抽查
> **基线 / Builds on**: [contract-audit-response-fields-2026-06-02.md](./contract-audit-response-fields-2026-06-02.md)（旧 admin 仓）、[tsid-field-matrix.md](./tsid-field-matrix.md)、[openapi.yaml](./openapi.yaml)
> **约束 / Constraint**: 本轮**仅审计出报告，未改动任何业务代码**。

---

## 0. 执行摘要 / Executive Summary

| 维度 | 结论 |
|------|------|
| **路径层（四向差集）** | ✅ 基本健康。admin 前端 ~179 个调用 **100% 命中**后端 `/adm` 路由；app `/api/v1` 前缀 **完整命中** v1 镜像，**无 404 风险**。 |
| **响应字段层** | ⚠️ 发现 **3 HIGH + 4 MED + 若干 LOW**。集中在 06-22 新增的 **finance / license / billing / channel-order** 域——这些端点从未进过 06-02 响应字段审计。 |
| **文档层** | ⚠️ OpenAPI 仅冻结 **30 个**"对外稳定"端点，后端实际 v1 端点约 **349 个**。这是设计取舍（OpenAPI=稳定契约子集），但需在文档中明确声明范围，否则易误判为"漏文档"。 |
| **金额/TSID 横向一致性** | ⚠️ 两套金额单位并存（钱包域=**分**，频道订单=**元**），跨域复用/对账时高风险混淆。 |

**关键前提澄清**：旧记忆 `imboy-contract-audit` 称"响应字段层未做"，但 `contract-audit-response-fields-2026-06-02.md` 已对 **app + 旧 admin 仓(imboy-admin-frontend) + sdk** 做过响应字段审计并修复 29 项。**当前 `imboyadmin`（React 重写版）从未被该审计覆盖**，本轮重点补齐了它的财务/许可域。

---

## 1. 四向差集表 / Four-Way Set Difference

### 1.1 别名机制（前提）

`imboy_router.erl:825-842` 用列表推导为所有核心路由生成 `/api` 前缀副本：
```erlang
api_alias_routes(Routes) -> [{"/api" ++ Path, H, O} || {Path,H,O} <- Routes, should_alias_path(Path)].
```
白名单（不生成 `/api` 副本）：`/`、`/help`、`/brand`、`/metrics`、`/privacy-policy`、`/account-deletion`、`/static/*`、`/static/admin/*`。

**ApiV1Routes 是权威全量超集**（349 条）；MainRoutes(v0，37 条)已标注"1.1.0 移除的兼容层"；AdmRoutes(129 条)。三组各自生成 `/api` 副本，双路并存。

### 1.2 差集结论

| 方向 | 结论 | 严重度 | 备注 |
|------|------|--------|------|
| **后端 ∖ admin**（后端有/admin未用） | `/adm/welcome`、`/adm/admin/config/policy/saved`、`/adm/app_version/version_stats`、`/adm/report/create`(create由用户端发起) | LOW | 服务端能力多于后台消费，非 bug。 |
| **admin ∖ 后端**（admin调用/后端无） | **空集** — 逐条核对 ~179 个调用全部命中（含 `/admin/list↔/admins/list` 等 fallback 候选、`/channel/:id/...` 参数化路径） | ✅ 无 | admin 与后端 adm 路由明显同步开发。 |
| **后端 ∖ app**（后端有/app未用） | 大量：billing 全域、live_room 部分、channel admin 子集、e2ee 合规端点等 | LOW | 多为 admin-only / web-only / 未来功能，预期内。 |
| **app ∖ 后端**（app调用/后端无） | **空集（核心端点）** — `/api/v1/user/show`、`/friend/list`、`/group/detail`、`conversation/*`、`group_member/*`、`group_album/*`、wallet/channel-order 全套均有 v1 镜像 | ✅ 无 | app 的 22 个"未调用常量"是死代码(LOW)，非缺端点。 |
| **后端 ∖ OpenAPI**（后端有/yaml未声明） | ~319 个 v1 端点 + 127 个 adm 端点未进 OpenAPI（仅冻结 30 个：auth/user/msg/wallet/billing/channel-order/setup/admin/brand） | **MED** | 见 §3。 |

> **路径层无 CRITICAL。** app 与 admin 均与后端路径对齐，无 404 风险。

---

## 2. 响应字段不一致清单 / Response Field Mismatches

> 序列化机制：后端 `elib_response:success` → `jsone:encode` 直编 Erlang map。**TSID(integer)→JSON integer；DB `timestamptz` 经 `epgsql_codec_rfc3339_bin` 解码为 RFC3339 字符串**（非毫秒戳）。admin `client.ts` 有全局 `safeParseBigIntJson`（16+位裸整数转 string）；app `model_parse_utils.dart` 对 int/string 双形态健壮。

### 2.1 HIGH（应在多租户/对账上线前修复）

| # | 端点 | 字段 | 后端 | 客户端期望 | 风险 |
|---|------|------|------|-----------|------|
| H1 | `POST /adm/stats/license`(applyLicense) | `current_users` / `current_nodes` | **POST 成功分支不返回**（`adm_stats_handler.erl:71-79` 比 GET 分支少这两字段） | 前端 `applyLicense` 返回类型声明含此二字段 | 提交 license 后 UI 拿到 `undefined`，配额进度条显示空/NaN。GET 与 POST 返回结构不对称。 |
| H2 | `GET /adm/finance/billing/subscriptions` | `tenant_id` | `adm_finance_handler.erl:239` 的 TSID 转换名单只含 `[id, plan_id]`，**漏 `tenant_id`** | `BillingSubscription.tenant_id: EntityId(string)` | 单租户=0 无碍；多租户若为 TSID 仅靠前端 `safeParseBigIntJson` 兜底，后端契约不稳健。 |
| H3 | 频道订单 vs 钱包域 | `amount` 单位 | **频道订单=元**(`channel_logic_order` 注释明示)；**钱包/充值=分**(integer) | app `ChannelOrderModel.amount: double`(元)；`RechargeOrder.amount: int`(分) | 两套单位在同一 app 并存，各自正确，但跨域复用/对账极易混淆 100 倍。属任务点名高风险域。 |

### 2.2 MED

| # | 端点 | 字段 | 后端 | 客户端期望 | 风险 |
|---|------|------|------|-----------|------|
| M1 | `GET /adm/finance/billing/subscriptions` | `plan_code` | `billing_subscription_repo` 的 `?COLUMNS` 无此列且无 JOIN plan 表，**从不返回** | 前端标 `plan_code?` 并注释"后端可能一并返回"（**臆测**） | UI 想显示套餐 code 会永远 `undefined`。需后端补 JOIN 或前端用 `plan_id` 二次查。 |
| M2 | `GET /adm/stats/finance/report` | `recharge_amount`/`subscription_amount` | SQL `SUM(amount)` 单位**分**(bigint) | `FinanceMonthData.*: number`（**无单位注释**，stats.ts 未引 money.ts） | DashboardPage 若直接渲染会把"分"当"元"放大 100 倍。需核对渲染处是否 `fenToYuan`。 |
| M3 | `GET /adm/stats/license` | `expires_at` | integer **毫秒**时间戳（license payload 单位 ms） | `expires_at: number` | 类型对但单位易错；前端按秒处理会错算到期（影响 LicenseExpiryBanner）。 |
| M4 | `GET /adm/stats/ranking` | `id` | SQL `SELECT u.id/g.id/c.id`(TSID) **未显式转 string**（raw row 直接透传） | `RankingItem.id: EntityId(string)` | 靠前端全局兜底；若某 id <16 位则前端拿到 number 与 string 类型不符。 |

### 2.3 LOW（容错已生效，记录备查）

| # | 端点 | 字段 | 说明 |
|---|------|------|------|
| L1 | `wallet/red_packet/detail` | 整体结构 | 后端返回 `{packet:{...}, receivers:[...]}` 嵌套；`RedPacketModel.fromJson` 期望扁平字段。app `getRedPacketDetail` 当前返回原始 Map 规避崩溃，但任何 `RedPacketModel.fromJson(payload)` 直调会拿到全默认值。建议 app 注释取 `payload['packet']`。 |
| L2 | 频道订单 | `channel_name` | `channel_order_repo` SELECT 不返回此列，app `channelName` 恒 null（`parseModelNullableString` 容错，不崩，功能缺失）。 |
| L3 | `/adm/finance/payment-transactions` | `currency`/`paid_at`/`biz_order_no` | 后端多发，前端 `PaymentTransaction` 未声明；对账页若要展示币种/时间需补类型。 |
| L4 | `/adm/finance/wallets` | `balance_yuan`/`version` | `balance_yuan` 仅用户端 `/v1` 钱包返回，admin list 不返回（前端已 `?` 容错，但注释误导）；`version`(乐观锁)后端多发前端未声明。 |

> §2.1–2.3 中标注的 06-02 旧审计 29 项已修复结论**对当前 imboyadmin 不自动成立**（旧审计针对 imboy-admin-frontend）。imboyadmin 的 TSID 安全靠 `client.ts` 全局 `safeParseBigIntJson` 兜底，多数场景有效。

---

## 3. 文档完整性 / Documentation Coverage

| 文档 | 覆盖 | 缺口 |
|------|------|------|
| `openapi.yaml` | 30 端点（auth/user/msg/wallet/billing/channel-order/setup/admin/brand） | 未含 conversation/friend/group/moment/channel(非订单)/e2ee/live/feedback/mention/location/group_album 等 ~319 个 v1 端点 + 127 adm 端点 |
| `rest-api-v1-catalog.md` | /v1 全量目录(58K) | 权威全量目录，OK |
| `tsid-field-matrix.md` | 94 表主键 + 20+ 外键 TSID | `msg_id`(VARCHAR40)、`conversation.user_id/peer_id` 物理类型待迁移；admin 端 TSID 表示法文档滞后 |

**判断**：OpenAPI 的 30 端点缺口**是设计取舍**（冻结对外稳定契约子集），非 bug。但应在 `openapi.yaml` 的 `info.description` 显式声明"本文件仅冻结对外稳定端点，全量见 rest-api-v1-catalog.md"，避免后续误判。

---

## 4. 按严重度排序的修复建议（待确认后再改码）

### CRITICAL
- 无。

### HIGH
1. **H1** — `adm_stats_handler.erl` POST 分支补 `current_users`/`current_nodes`，与 GET 对称（或前端 applyLicense 后改为重新 GET 一次状态）。
2. **H2** — `adm_finance_handler.erl:239` 的 TSID 转换名单补入 `tenant_id`（多租户上线前必修）。
3. **H3** — 统一金额单位文档：在 `payment-wallet-integration.md` 与 app model 注释明确标注「频道订单=元、钱包域=分」，并评估是否统一为分。

### MED
4. **M1** — 决策 `plan_code`：后端 `billing_subscription_repo` 补 JOIN plan，或前端删除臆测注释改用 plan_id 二次查。
5. **M2** — `FinanceMonthData` 加单位注释 + DashboardPage 渲染处接 `money.ts` 的 fenToYuan（核对是否已换算）。
6. **M3** — license `expires_at` 在 TS 类型注释标「毫秒」，核对 LicenseExpiryBanner 换算。
7. **M4** — `adm_stats_handler` ranking 的 `id` 显式转 string（不依赖前端兜底）。

### LOW
8. `openapi.yaml` 加范围声明（§3）。
9. red_packet/detail 嵌套结构在 app 加注释（L1）；channel_name 决策补发或删字段（L2）；finance 多发字段按需补 TS 类型（L3/L4）。
10. 清理 app `const.dart` 22 个未调用常量（死代码）。

---

## 5. 抽查覆盖与局限 / Coverage & Limitations

**已核对**：后端全部 519 老路由 + adm 129 路由路径；admin 全量调用；app 核心 + 支付/钱包/频道订单域；finance/stats/license/billing 响应字段逐字段。

**未穷尽**（建议下轮）：
- imboyadmin 群组/消息/插件域响应字段（本轮聚焦财务/许可高风险区）。
- app 群组协作域(album/task/vote/schedule) 响应字段逐字段（06-02 已覆盖部分）。
- WebSocket 消息 envelope 字段（见 `websocket-api-2.md`，本轮未触及）。
- 方法级歧义：多个 adm 端点同路径承载 GET+POST/PUT（如 `/stats/license` GET 查询+POST 应用、`/channel/detail/:id` GET+PUT），Cowboy 路由方法无关、由 handler 内部分发——工作正常但未按方法文档化(INFO)。
