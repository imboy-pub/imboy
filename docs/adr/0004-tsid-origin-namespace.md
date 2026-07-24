# 0004 — TSID 之上预留 origin 命名空间位（不改现有数据）

- 状态：Proposed
- 日期：2026-07-24
- 关联文件：`src/lib/elib_tsid.erl`、`src/imboy_app.erl`、`docs/analysis/tsid-field-convention.md`、`docs/CONVENTIONS.md`
- 关联 ADR：无前序（本篇为 ID 命名空间首篇）；参阅 `docs/analysis/p0-billing-multitenant-authz-2026-07.md`（租户现状）

## 背景

imboy 的所有业务实体主键由应用层 TSID 生成（取代 BIGSERIAL），位布局为：

```
[sign:1=0][timestamp_ms:42][node_id:10][sequence:11]
```

其中 10-bit `node_id` 已支持按 `dc_bits` 动态划分为 `dc_id` + `node_id`（默认 3+7 = 8 DC × 128 node）。生成入口 `elib_tsid:generate/0,1` 返回 `pos_integer()`，落库为 PostgreSQL `BIGINT`。

**跨部署实例的唯一性完全依赖 node 段**。而 `tsid_dc_id`/`tsid_node_id` 来自 application env（默认均为 1），`.env.example` 当前**未列出这两个键**。这意味着：

- 多个私有化部署实例若不显式配置，会都用 `dc_id=1, node_id=1`，在同一毫秒生成**数值完全相同**的 ID。
- 白标（换肤+独立部署）是已确认的商业方向（见 `docs/analysis/*monetization*`、`im-competitor-capability-roadmap-2026-06.md`），但现有方案只覆盖前端品牌与 SSO，**未回答「当两个白标客户的数据需要合并 / 迁移 / 联邦时 TSID 撞键怎么办」**。
- 库内现有的归属字段都不能解决此问题：`billing_subscription.tenant_id`（bigint DEFAULT 0，逻辑字段、无 tenant 实体表、无 uid 映射）、`user.account_type`（0=human/1=ai/2=bot，账号类型投影，非部署标识）、CORS `origins`（域名白名单）、`user.source`（注册渠道字符串）——四者均非「ID 生成层的部署/实例命名空间」。

> 历史先例：`system_id_segment` 表（`00000001_foundation` L1348）保留了早期「按 datacenter × table 分配 ID 段」的思路，证明项目早有按维度隔离 ID 的意图，后被应用层 TSID 取代。本 ADR 是其精神延续：把「部署/实例」维度显式编码进 ID 本身。

本篇为**架构保险**（与 S0-1 消息信封 `ver` 字段同批）：当前只有单一版本/单实例在跑，尚无实际撞键事故，趁成本低廉预留演进位。

## 决定

**从 10-bit node 段切出高位作为 `origin`（部署/实例命名空间）位，默认 0 = 现状。** 不动 42-bit 时间戳（纪元寿命每丢 1 位减半，代价不可接受），不触动已生成的存量 ID。

### 1. 位布局（参数化，向后兼容）

复用现有 `dc_bits` 动态划分机制，新增 `origin_bits` 参数。node 段 10 位重新三段切分：

```
node_id 段(10) = [ origin_id : origin_bits ][ dc_id : dc_bits ][ node_id : (10 - origin_bits - dc_bits) ]
```

| `origin_bits` | `dc_bits`(默认3) | 剩余 node 位 | origin 取值数 | DC 数 | node/DC | 适用场景 |
|---|---|---|---|---|---|---|
| **0（默认）** | 3 | 7 | 1（即无 origin） | 8 | 128 | 现状，单实例 |
| 2 | 3 | 5 | 4 | 8 | 32 | 少量白标联邦 |
| 4 | 2 | 4 | 16 | 4 | 16 | 多租户托管 |

- **`origin_bits = 0`（默认）时位布局与当前完全一致**，所有存量 ID 高位语义不变，`parse/1` 仍正确 → 满足「不改现有数据」。
- `origin_id = 0` 即「未设置 origin」，语义等同今天。非零 origin 仅在多实例数据需要互通时才赋值。

### 2. 配置入口

`elib_tsid:init/1` 的选项列表新增 `origin_bits`（默认 0）与运行时 `origin_id`（默认 0）。`imboy_app.erl` 启动读取 `tsid_origin_id` / `tsid_origin_bits` env（默认 0）。**`.env.example` 补齐 `tsid_dc_id` / `tsid_node_id` / `tsid_origin_id` / `tsid_dc_bits` / `tsid_origin_bits` 五个键并给注释**——这是堵住「多实例默认撞键」的直接措施。

### 3. 契约与解析同步

- `elib_tsid:parse/1` 返回 map 新增 `origin_id` 字段。
- `tsid-field-convention.md` 位布局图与 `CONVENTIONS.md` §1 同步加入 origin 段说明（标注默认 0 = 当前行为）。

### 4. 新建表 / 契约模板（origin 可空字段）

> 仅为「未来需要跨实例归属」的实体预留；绝大多数单实例表**无需加**此列。

**建表 DDL 模板片段**（origin 可空，缺省 NULL = 本实例/未设置）：

```sql
-- 跨实例归属预留列（ADR 0004）；单实例部署保持 NULL，不影响任何现有查询
origin_id  smallint  NULL,
-- 业务主键仍为 TSID bigint，不变
id         bigint    NOT NULL,
PRIMARY KEY (id)
```

**JSON 契约模板**（响应体内，可选字段，旧客户端忽略无碍）：

```json
{ "id": 7234567890123456, "origin_id": null }
```

`origin_id` 与 `account_type` / `tenant_id` 的边界：
- `account_type`：账号类型（人/AI/Bot），与部署实例无关。
- `tenant_id`（billing）：库内计费逻辑字段，无实体表、无 uid 映射；`origin_id` 是 ID 生成层编码，二者正交。
- 仅当某实体需要「标记这条数据来自哪个部署实例」时才加 `origin_id` 列；不加 = 本实例本地数据。

## 后果

- ✅ **零迁移落地当前需求**：`origin_bits` 默认 0，存量 ID 与生成行为完全不变；本 ADR 可在不下一个 release 前以纯代码 + 文档方式合入。
- ✅ **为白标/联邦预留位**：未来两个实例数据合并时，各自配不同 `origin_id` 即可让 TSID 全局唯一，无需 re-key。
- ✅ **堵住多实例撞键根因**：`.env.example` 显式列出 node/dc/origin 配置项，私有化部署不会再「默认都用 1」。
- ⚠️ **挤占 node 容量**：每分配 1 位给 origin，node 段少 1 位（DC×node 组合数减半）。这是显式权衡——多实例隔离价值 > 单实例节点数（单实例极少超 32 节点）。
- ⚠️ `origin_id` 非零一旦启用并写入存量数据，**不可回退**为 0（位语义已变）。故默认关、按需开。
- ⚠️ 本 ADR 只解决「ID 层全局唯一」；跨实例的**数据同步 / 权限隔离 / 路由**仍需独立设计（不在本篇范围）。

## 备注

- 本篇状态 `Proposed`：落地代码（`elib_tsid:init` 扩展 + `.env.example` + parse/契约同步）待随下一个需要多实例的特性一起实现；当前先冻结**设计**与**位分配方案**，避免日后临时切位破坏存量。
- 若将来确认「永远不会做多实例数据合并」，本 ADR 可被一篇 `Superseded` 直接取代而不留代码债（因为默认 origin_bits=0 等于没改）。
