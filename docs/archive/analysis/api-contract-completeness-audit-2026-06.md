# API 契约完整性与三端对接一致性审计 / API Contract Completeness Audit

> **日期 / Date**: 2026-06-23
> **范围 / Scope**: 后端 `imboy`(Erlang) ↔ 移动端 `imboyapp`(Flutter) ↔ 管理后台 `imboyadmin`(React)
> **方法 / Method**: 以 `src/imboy_router.erl` 为后端真值源，做四向集合差 + 响应字段抽查
> **基线 / Builds on**: [contract-audit-response-fields-2026-06-02.md](./contract-audit-response-fields-2026-06-02.md)（旧 admin 仓）、[tsid-field-matrix.md](../../reference/tsid-field-matrix.md)、[openapi.yaml](../../../api/openapi.yaml)
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

---

# 第二轮补充审计（2026-06-23，§5 遗留域）

> 覆盖 §5 列出的未穷尽域：imboyadmin 群组/消息/插件、app 群组协作、WebSocket envelope。仅审计不改码。
> **⚠️ 复核提醒**：本轮部分行号/字段来自子代理取证，修复前须对涉及文件单点 `Read` 复核精确位置。尤其"时间字段类型"存在子代理分歧（见 §6.0），结论需后端验证。

## 6. imboyadmin 群组/消息/插件域响应字段

### 6.0 关键机制（已核实，纠正一处普遍误判）

**adm 域所有 handler 走 `elib_response:success/2,3,4`，该函数无条件调用 `elib_cnv:convert_at_timestamps`**（`src/lib/elib_response.erl:30,40,52`），把**以 `_at`/`_ts` 结尾的字段**经 `elib_dt:rfc3339_to` 转成**毫秒整数 number**（`src/lib/elib_cnv.erl:136-137`）。DB codec 先解出 RFC3339 binary，响应层又转回毫秒整数。

> ⚠️ 边界：`convert_at_timestamps` **只转 `_at`/`_ts` 结尾字段**。不以此结尾的时间字段（如 `deadline`）不被转、仍是 RFC3339 字符串——这正是 §7 app 域 `deadline` 与本节 `*_at` 表现不同的原因。两轮结论不矛盾，但**修复前须按字段名逐个确认走哪条路径**。

### 6.1 发现清单

| # | 端点 | 字段 | 后端实际 | 前端期望 | 风险 | 说明 |
|---|------|------|----------|----------|------|------|
| A1 | **全 adm 域** `*_at`/`*_ts` | created_at/updated_at/start_at/end_at/subscribed_at/expires_at… | **毫秒整数 number** | 多数声明 `string` | **HIGH** | 系统性。前端把毫秒整数当 ISO 字符串渲染→错误/空日期。需统一：前端改 `number`(毫秒) 或后端 adm 域停用该转换。**需后端确认覆盖范围**。 |
| A2 | plugin/logs | 包裹键 | 后端 `{list, limit, offset}`(`adm_plugin_handler.erl:367`) | 前端读 `{items, page, size, total, total_pages}`(`plugins.ts:215`) | **HIGH** | 键错位→`items` 恒 undefined→**日志列表永远空、total 恒 0**。 |
| A3 | plugin/logs | `action` | 表字段是 `event`，无 `action` | `action`(兜底 `'enable'`) | **HIGH** | 字段名不符→所有日志动作恒显示 `'enable'`。 |
| A4 | 群组子域(vote/notice/schedule/tag/category/file/album/task) | `id`/`group_id`/`creator_id` 等 TSID | **裸 JSON integer**（未调 tsid_keys_to_bin，与 group 主体不同） | `EntityId`=string | **HIGH** | 类型谎报，靠 safeParseBigIntJson(≥16位) 兜底；TSID<16位或 id=0 不触发→前端拿 number。 |
| A5 | group/category/list | `id`(默认"未分类"项=0) | integer `0` | `EntityId`=string | **HIGH** | id=0 必然不被兜底转 string，确证 A4 类型谎报。 |
| A6 | group/members | `joined_at` | 疑 SELECT 取不存在列（应为 created_at），`adm_group_handler.erl:215` | `joined_at: string`(必填) | **HIGH** | **须后端 Read 复核 SQL 列名**：若取不存在列→端点失败；若实为 created_at→入群时间恒空。 |
| A7 | channel/stats | `channel_id` | integer（未 normalize，`adm_channel_handler.erl:392`） | `EntityId`=string | HIGH | 该端点唯一 ID 靠兜底；边界短 ID 类型错。 |
| A8 | plugin/logs | `result` | `ok/failed/cancelled/timeout` | `'success'/'failure'`(兜底 success) | MED | 枚举不对齐→结果状态全显示 success。 |
| A9 | plugin/list·detail | `description`/`installed_at`/`config`/`state` 枚举 | 多数不返回；state=`installed/enabled/disabled/unknown/failed` | 前端声明全字段；枚举含 `error/installing/upgrading` | MED | UI 列恒空；后端 `failed/unknown` 被前端 normalize 误兜底成 `disabled`(隐藏失败态)。 |

一致项（无问题）：消息域 from_id/to_id（显式转 string）、msg_id(varchar 非 TSID)、group 主体 ID（显式 tsid_keys_to_bin）、channel amount/status。

## 7. imboyapp 群组协作域响应字段（相册/任务/投票/日程/公告/分类标签）

> app 普遍用双键兜底（`A ?? B`）吸收后端字段名不一致，掩盖后端命名混乱根因。TSID 处理整体健壮（`_toInt`/`_toText`/toString），仅一处脆弱。

| # | 端点 | 字段 | 后端 | app期望 | 风险 | 说明 |
|---|------|------|------|---------|------|------|
| B1 | group_task list/detail | `deadline` | **RFC3339 字符串**（不以 _at 结尾，未被转毫秒） | 当 epoch int 处理(`_toInt`×1000) | **HIGH** | `group_task_page.dart:207`/`detail_page:76` 对 RFC3339 解析失败返 0→截止时间显示错误。 |
| B2 | group_vote my_vote | 外层结构 | 返单对象 `{ok, Map}` | api:204 硬包成 `[Map]` 数组 | **HIGH** | 协议层结构错配，调用方被迫 `.first`。 |
| B3 | group_vote list | `participant_count` | **不返回** | `group_vote_page.dart:216` 读取(默认0) | **HIGH** | 列表参与人数永远显示 0。 |
| B4 | group_category/list、group_tag/list | `category_name`/`tag_name` | 返 `category_name`/`tag_name` | 读 `name`(双键兜底) | **HIGH** | 后端字段名与 app 不符，靠 `name ?? category_name` 掩盖根因(后端 DS)。 |
| B5 | group_notice | edit/publish/pin/unpin/mark_read | 后端实现 11 个 action | app 仅消费 list 展示 + add/delete | **严重(功能缺口)** | 大量后端公告能力 app 未接入。 |
| B6 | group_category | `id` | TSID integer | `sortCategories(List<int>)`/`deleteCategory(int)` 直接用 int | MED | **唯一脆弱 int 读法**；若 id 以 string 返回会类型崩溃。 |
| B7 | group_schedule my_list | 字段集 | Repo:184 仅返 id/schedule_id/group_id/title/start_at/end_at/status | 期望同 list(含 location/description/remind_before) | MED | my_list 字段比 list 少，UI 这些字段恒 null。 |
| B8 | group_schedule detail、group_notice/list | 参与者 `nickname`/`publisher_name` | SELECT 不含昵称 | 读 nickname/publisher_name 降级 uid | MED | 显示 uid 而非昵称，app 被迫 N+1 查本地群成员补昵称。 |
| B9 | group_task my_tasks | `assignment_status` | 未返回 | app 读取 | MED | app 读不到。 |
| B10 | group_album list_comments | 整端点 | 实现了但返 `{comments:[...]}` 无分页 | **无 app 调用方** | LOW | dead endpoint，结构也与其它端点不一致。 |

死代码：相册 `url` fallback、日程 `start_time` 双键、投票 `end_at` 均为无效/未用分支。

## 8. WebSocket Envelope 三方对账（文档↔后端↔app）

### 8.0 关键背景

> **⚠️ 2026-06-23 复核修正（重要）**：子代理审计的 `asyncapi.yaml` 是 **`docs/api/asyncapi.yaml`——一份 251 行的人类可读历史快照**。据 `imboy/api/README.md` 与 `imboy/api/asyncapi.yaml` 文件头：codegen 真源（Source of Truth）是 **`imboy/api/asyncapi.yaml`（AsyncAPI 3.0，v2 二进制帧协议）+ `imboy/api/proto/*.proto`**，`docs/api/asyncapi.yaml` 已被提升到 `api/` 后保留为"设计参考"。
> 因此下表 W1–W11 中标"asyncapi 偏差"的项，**多数是历史快照过时**，而非 codegen 真源缺陷——**`api/asyncapi.yaml` + proto 本轮未对账**，其与后端/app 的一致性是独立的待办（见 §9 W-doc 修正）。
> WS 真源对账应以 `api/proto/imboy_v2_frame.proto` + `api/proto/imboy_s2c.proto` 为字节级基准，下表仅反映"历史快照 vs 后端 vs app"，**勿据此直接改 codegen 真源**。

参与方：
- **`websocket-api-2.md`（1600+行）** — 人类可读设计文档，字段 `id/type/from/to/msg_type/action/e2ee/payload/created_at/server_ts`，与后端代码一致。
- **`docs/api/asyncapi.yaml`（历史快照，非真源）** — 字段 `msg_id/content_type/content/ciphertext/conv_seq/at_uids`，与后端代码逐项不符（已过时）。
- **`api/asyncapi.yaml` + `api/proto/*.proto`（codegen 真源，本轮未审）** — 待独立对账。

权威实现：`message_ds:assemble_msg/8`(`src/ds/message_ds.erl:199`) + `encode_websocket_message/1`(:262)。

### 8.1 字段对照（只列不一致/可疑）

| # | 字段 | 权威文档 | asyncapi.yaml | 后端实发 | app读取 | 风险 | 说明 |
|---|------|----------|---------------|----------|---------|------|------|
| W1 | 消息ID | `id`(string) | **`msg_id`**(int64) | `id`(binary) | `id`(String) | HIGH | asyncapi 字段名+类型双错。 |
| W2 | `from` | `from`(string示例) | `from`(int64) | `from`(**integer**=CurrentUid) | parseModelInt | **HIGH(TSID)** | 后端发 integer；JS SDK(Number 53位) 大 TSID 丢精度。app Dart int 安全。 |
| W3 | `to` | `to`(string示例) | `to`(int64) | `to`(**透传客户端原值**, binary 或 integer) | parseModelInt | **HIGH(TSID)** | 类型不固定，与 from 不对称。 |
| W4 | 消息类型字段 | `msg_type` | **`content_type`** | `msg_type` | `msg_type` | HIGH | asyncapi 字段名错。 |
| W5 | E2EE 载体 | 顶层 `e2ee`(map) + 密文入 `payload` | **`ciphertext`**(string) | `e2ee`(map\|null)+`payload` | `e2ee`(Map) | HIGH | asyncapi 命名+结构双错。 |
| W6 | 业务体 | `payload`(object) | **`content`** | `payload` | `payload` | HIGH | asyncapi 字段名错。 |
| W7 | `action` | `action`(S2C分发) | **未声明** | `action`(顶层) | `action`(强依赖) | MED | asyncapi 缺 S2C/撤回/编辑/已读核心分发字段。 |
| W8 | `server_ts` | int 毫秒 | **未声明** | int 毫秒(`elib_dt:millisecond()`) | int(S2C去重) | MED | asyncapi 完全没有。 |
| W9 | `created_at` | RFC3339 串(示例) | **int 毫秒** | **RFC3339 binary**(`to_rfc3339`) | int\|RFC3339 容错 | **HIGH** | 同信封内 created_at(字符串) 与 server_ts(毫秒int) 时间格式混用；asyncapi 又标 int。 |
| W10 | `conv_seq` | 未在示例 | **声明**(服务端回填) | **WS下行不发**(仅 msg_archive 游标用) | 不读 | MED | 文档承诺了实际不存在的字段。 |
| W11 | `at_uids` | (C2G mentions) | **声明**顶层 int64数组 | **未发**(改用 `payload.mentions`) | 经 payload 读 | MED | asyncapi 顶层 at_uids 不存在。 |

### 8.2 消息 type 枚举：三方不一致

| 来源 | type 值域 |
|------|-----------|
| asyncapi.yaml | **小写** c2c/c2g/ack/s_ack/ping/pong/token_refresh/revoke/edit |
| 后端实发 | **大写** C2C/C2G/S2C/C2S + C2G_ERROR/C2C_SERVER_ACK/C2G_SERVER_ACK |
| app 识别 | 大写 C2C/C2G/C2S/S2C/CHANNEL（`type.toUpperCase()` 强制） |

- 大小写整体冲突（app `toUpperCase()` 容错，但 asyncapi 值域错）。
- ACK：asyncapi 建模独立 type `ack/s_ack`；后端实际服务端 ACK 用 `*_SERVER_ACK` type、客户端 ACK 走文本协议 `CLIENT_ACK,...`。
- 撤回/编辑：asyncapi 用顶层 `type:revoke/edit`；后端用 `type:C2C/S2C` + `action:message_revoke_ack/message_edit_ack`。
- 后端/app 共享 20+ 个 S2C `action` 值，asyncapi 完全未覆盖。

## 9. 更新后的修复优先级（合并两轮，待确认）

### CRITICAL
- 无（均不阻断，但有功能性错误）。

### HIGH（新增，按影响面排序）
- **W-doc（已修正）**：噪音源是 **`docs/api/asyncapi.yaml`（历史快照）**——它与实现逐项脱节但**非 codegen 真源**。建议：① 给 `docs/api/asyncapi.yaml` 加显著"历史快照，已被 `api/asyncapi.yaml` 取代，勿据此实现"头注，或直接删除（纯文档，零代码风险，可最先做，但需确认无 docs 渲染引用）；② **真正的 codegen 真源 `api/asyncapi.yaml` + `api/proto/*.proto` 与后端/app 的一致性本轮未对账，是独立的下一轮工作**（涉及 proto 字节级基准，工作量较大）。
- **A1/A2/A3**：adm 域时间字段毫秒-vs-string（系统性，需后端确认范围）；plugin/logs 契约全断（日志页恒空，改动小）。
- **A4/A5**：群组子域 TSID 未转 string + category id=0 → 后端补 `tsid_keys_to_bin` 与 group 主体看齐。
- **A6**：group/members `joined_at` 列名需后端先 Read 复核真伪。
- **B1/B2/B3/B4**：app 任务 deadline 类型错配、投票 my_vote 结构错配、participant_count 恒0、分类/标签字段名错配。
- **W2/W3/W9**：WS envelope from/to TSID 类型不统一（JS SDK 精度风险）+ created_at/server_ts 时间格式混用。

### MED / 功能缺口
- A8/A9（plugin 枚举与字段缺失）、B5（公告 11 能力 app 未接入）、B6-B9、W7/W8/W10/W11。

### 复核要求
本轮 HIGH 项落地前，**逐一对涉及文件 Read 复核**（特别是 A1 转换覆盖范围、A6 SQL 列名、B1 deadline 实际格式、W2/W3 from/to 序列化）。子代理曾在"时间字段类型"上互相纠正，证明此域需代码级确认而非二手结论。
