# ADR: 频道访问控制与付费语义契约

> 日期：2026-08-24 | 状态：**PASS（M1+M2 冻结为代码事实，M3 不引入——产品已确认）** | 关联计划：`docs/planning/2026-08-24-private-paid-channel-red-packet-remediation-plan.md` Step 1
>
> 适用：`imboy` 后端、`imboyapp` Flutter、`imboyadmin` 管理后台。本 ADR 冻结现有频道类型的产品语义，供后续 Step 2–8 作为契约基线。

## 1. 背景

付费频道支付宝下单链路、退款对账、红包资金链路修复（计划 8 步）的前置是：先冻结「频道类型 × 邀请 × 价格 × 订单 × 退款权益」的产品语义，避免后续步骤在语义未定的情况下改权限或订单逻辑。

## 2. 决策

### 2.1 选定模型（冻结为当前代码事实，非新决策）

| 模型 | 频道类型 | 语义 | 代码证据 |
|------|---------|------|---------|
| **M1** | `type=1` 私有频道 | 邀请校验通过 → 订阅；**不创建订单、不收费** | `channel_logic_subscription.erl:44-52`（`subscribe_private_channel` 校验 `channel_invitation_ds:is_invited` → accept → subscribe）；`channel_logic_common.erl:100-104`（内容访问校验 `channel_subscription_ds:is_subscribed`） |
| **M2** | `type=2` 付费频道 | 创建订单 → 付款成功 → 订阅；**不校验邀请** | `channel_logic_subscription.erl:55-56`（`subscribe_paid_channel` 校验 `channel_order_ds:has_purchased`）；`channel_logic_order.erl:67`（`Type =/= 2` → "只有付费频道支持购买"）；`channel_logic_common.erl:105-112`（内容访问校验 `has_purchased`） |
| `type=0` 公开频道 | 直接订阅，无邀请无订单 | `channel_logic_common.erl:98-99`（`ok`）；`channel_logic_subscription.erl:58`（兜底 `channel_ds:subscribe`） |

**M1 与 M2 是并存的两类频道，不是二选一。** 当前代码已逐层证实：订阅逻辑、订单逻辑、内容访问逻辑、客户端模型、数据库迁移列注释、创建 handler 类型校验完全一致。

### 2.2 待定模型（需产品确认，当前 BLOCKED）

| 模型 | 语义 | 现状 |
|------|------|------|
| **M3** | 「私有且付费」频道：**同时要求邀请和支付** | **代码中不存在**。`channel_handler.erl:77` 守卫 `_ when Type < 0; Type > 2` 直接拒绝 `Type>2`；迁移 `00000003` `channel.type` 列注释仅 `0/1/2`；创建 handler 的 `Opts`（`channel_handler.erl:80-85`）无价格字段（价格在独立的 `channel_price` 表，仅 type=2 消费） |

**M3 是新增产品决策，非既有事实。** 在产品负责人确认前：
- 不得把 `type=1` 改成收费；
- 不得新增 `type=3` 或组合收费机制；
- 不得修改现有权限或订单逻辑（计划 Step 1 验收标准）。

### 2.3 拒绝模型

- **「公开且收费」**（type=0 收费）：与公开频道的「直接订阅」语义冲突，且 `channel_logic_order.erl:67` 已限定 `Type =:= 2` 才可下单。拒绝。
- **邀请即收费**（type=1 收费）：破坏私有频道「关系驱动、无交易」的语义。拒绝。

## 3. 行为矩阵（现有事实）

| 维度 | type=0 公开 | type=1 私有 (M1) | type=2 付费 (M2) |
|------|------------|------------------|------------------|
| 创建校验 | `Type≥0且≤2` 通过 | 同左 | 同左 |
| 是否需邀请 | 否 | **是**（`channel_invitation` 表，status 0-4） | 否 |
| 是否需价格 | 否 | 否 | **是**（`channel_price` 表，`subscription_type` 1一次性/2月/3年） |
| 是否创建订单 | 否 | 否 | **是**（`channel_order` 表） |
| 订阅前置 | 直接 `subscribe` | 邀请 accept → subscribe | `has_purchased` 为真 → subscribe |
| 内容访问门 | `ok` | `is_subscribed` | `has_purchased`（订单为唯一权益来源） |
| 管理员绕过 | `role>0` → `ok` | 同左 | 同左 |
| 退款后权益 | N/A | N/A | 退款 `status=2` → `unsubscribe`（`channel_logic_order.erl` `do_refund_order`） |

### 3.1 订单状态机（迁移 `00000003` 列注释 + CHECK 约束）

`channel_order.status` CHECK `status = ANY (ARRAY[0,1,2,3,4])`：

| 值 | 含义 |
|----|------|
| 0 | 待支付 |
| 1 | 已支付 |
| 2 | 已退款 |
| 3 | 已取消 |
| 4 | 已过期 |

> ⚠️ **Step 3 前置缺口**：当前状态机**无 `refunding` 中间态**，直接 `paid(1) → refunded(2)`。计划 Step 3 要求「`paid -> refunding -> refunded` 可恢复语义」。Step 3 须先解决该缺口（迁移加状态值或用 CAS 条件抢占），属 Step 3 范围，本 ADR 仅记录现状。
>
> ✅ **此缺口已由迁移 `00000057`（Step 3, commit `b02e674b`）解决**：CHECK 已更新为 `ARRAY[0,1,2,3,4,5]`，`refunding(5)` 占位态已落盘，详见 §8.9。

### 3.2 邀请状态机（迁移 `00000003`）

`channel_invitation.status` CHECK `status = ANY (ARRAY[0,1,2,3,4])`：0待处理/1已接受/2已拒绝/3已过期/4已取消。`uk_channel_invitation_pending` 唯一索引 `(channel_id, invitee_uid) WHERE status=0` 保证同一 pending 邀请不重复。

### 3.3 支付网关结算分层（`channel_logic_order.erl`）

- `?INSTANT_SETTLE_METHODS = [wallet, mock]`：`settle/7` 立即调 `do_pay_order` 发货；
- 第三方 `alipay/wechat/stripe`：仅创建支付意图，返回 `PENDING`，**回调前不开通频道**（`payment_callback_logic` 回调成功后才 `do_pay_order`）；
- 金额单位：`wallet` 网关传「元」，第三方传「分」（`to_gateway_amount` + `yuan_to_fen`）。

## 4. API 兼容与迁移策略

### 4.1 若产品确认**不引入 M3**（仅保留 M1+M2）

- **零迁移、零 API 变更**。现有 type=0/1/2 语义即契约。
- Step 2/3/6 直接针对 `type=2` 执行；Step 4/5/7（红包）独立并行。

### 4.2 若产品确认**引入 M3**（需选实现路径，未确认前不写迁移）

两条候选路径，供产品决策时评估：

**路径 A — 新增 `type=3`：**
- 改动：`channel_handler.erl:77` 放宽守卫到 `Type > 3`；迁移更新 `channel.type` 列注释加 `3 私有付费`；
- 逻辑：`channel_logic_subscription.erl` 加 `3` 分支（先校验邀请再校验 `has_purchased`）；`channel_logic_common.erl:96` 加 `3` 分支（`is_subscribed AND has_purchased`）；`channel_logic_order.erl:67` 放宽 `Type =:= 3`；
- 客户端：`channel_model.dart` `ChannelType` 加 `privatePaid(3)`；`channel_detail_rules.dart` 加规则；
- 影响面：大，触及所有 type 分支 + 客户端枚举 + 管理后台。

**路径 B — 组合标志（`type=2` + `requires_invitation` 布尔）：**
- 改动：`channel` 表加 `requires_invitation boolean DEFAULT false`；创建 handler 加可选参数；
- 逻辑：`subscribe_paid_channel` 在 `has_purchased` 前先校验 `requires_invitation → is_invited`；内容访问同序；
- 影响面：小，type 枚举不变，仅付费频道可选叠加邀请门；
- 旧数据兼容：`requires_invitation` 默认 false，存量 type=2 行零影响。

> 两条路径均为**候选方案**，不是本 ADR 的决策。产品确认 M3 后再选路径并单独设计迁移，不在 Step 1 范围内执行。

## 5. 后续步骤可执行性

| 步骤 | 依赖 | 可否执行 | 说明 |
|------|------|---------|------|
| Step 2 订单支付幂等与回调 | Step 1 | **可执行（针对 type=2）** | 计划明示「只保留现有付费频道模型，可直接针对 type=2 执行」 |
| Step 3 退款与对账 | Step 2 | 可执行（须先补 refunding 态，见 3.1） | — |
| Step 6 Flutter 支付等待 | Step 2 契约 | 可执行 | 后端状态契约本 ADR 已冻结 |
| Step 4 红包金额与资金 | 无 | **可执行（与 Step 2 并行）** | 独立 |
| Step 5 红包范围强制 | Step 4 | 可执行 | 独立于频道类型 |
| Step 7 Flutter 红包状态 | Step 4 | 可执行 | 独立 |
| Step 8 集成验收 | Step 1–7 | 待 | — |
| M3 相关任何实现 | 产品确认 | **BLOCKED** | 未确认前不得动权限/订单逻辑 |

## 6. 验收对照

- ✅ 文档明确选定模型（M1+M2，冻结为代码事实）与拒绝模型（M3，不引入）；
- ✅ 后端三层（订阅 `channel_logic_subscription.erl`、订单 `channel_logic_order.erl`、内容访问 `channel_logic_common.erl`）、客户端两层（`channel_model.dart`、`channel_detail_rules.dart`）、数据库迁移 `00000003` 列注释、创建 handler `channel_handler.erl:77` 类型校验——逐项对照一致；
- ✅ 产品已确认不引入 M3 → Step 1 = **PASS**，本步骤**未修改任何权限或订单逻辑**（仅产出本文档）。Step 2/3/6 可针对 type=2 推进。

## 7. 产品确认结果（2026-08-24 已确认）

1. **是否引入 M3「私有且付费」频道？→ 否，不引入。**
   - 现有 `type=0`（公开）/`type=1`（私有，邀请）/`type=2`（付费，订单）语义即最终契约。
   - Step 1 → PASS；Step 2/3/6 针对 type=2 推进；Step 4/5/7（红包）独立并行。
   - M3 的两条候选路径（A 新增 type=3 / B 组合标志）保留在 4.2 节作为历史参考，**当前不执行、不迁移**。未来若产品改判，须另起 ADR 并单独设计迁移。

---

## 8. Phase 2 — 正交频道模型（Step 9 冻结契约）

> 日期：2026-08-25 | 状态：**PASS（契约冻结，待 Step 10 落盘迁移）** | 关联计划 Step 9
>
> 产品方向（2026-08-25 确认）：**不把 `type` 从 3 种扩成 4 种**。改为三个正交维度，四种情况是组合而非死枚举。
> 三层语义顺序：**Visibility（谁能发现）→ Join Policy（怎么加入）→ Monetization（是否付费）**。

### 8.1 架构决策

将 `channel.type` **降级为 legacy projection**，新增三个正交字段成为后端权威：

| 维度 | 字段 | 语义层 | 决定 |
|------|------|--------|------|
| 可见性 | `visibility` | Visibility | 谁能**发现**这个频道 |
| 加入策略 | `join_policy` | Join Policy | **怎么加入**这个频道 |
| 付费属性 | `access_type` | Monetization | **是否付费**（语义别名 `monetization`） |

- **不扩展 `type` 枚举**（不走 §4.2 路径 A 新增 `type=3`）。
- **不引入组合标志**（不走 §4.2 路径 B `requires_invitation` 布尔）。
- §2.2 待定模型 M3「私有且付费」**从待定升级为四个正交组合之一（C4）**，无需新增 `type` 值。
- `type` 列原地保留，不再承担新领域语义，只作旧客户端兼容投影（§8.5）。

### 8.2 字段矩阵（DDL 契约，Step 10 落盘）

```sql
ALTER TABLE channel ADD COLUMN visibility  smallint NOT NULL DEFAULT 0
  CHECK (visibility = ANY(ARRAY[0,1]));
ALTER TABLE channel ADD COLUMN access_type smallint NOT NULL DEFAULT 0
  CHECK (access_type = ANY(ARRAY[0,1]));
ALTER TABLE channel ADD COLUMN join_policy  smallint NOT NULL DEFAULT 0
  CHECK (join_policy = ANY(ARRAY[0,1,2,3]));
```

| 字段 | 类型 | 值域 | 默认 | 含义 |
|------|------|------|------|------|
| `visibility` | smallint NOT NULL | 0=public, 1=private | 0 | 0 公开可发现 / 1 私有不可发现 |
| `access_type` | smallint NOT NULL | 0=free, 1=paid | 0 | 0 免费 / 1 付费（语义别名 monetization） |
| `join_policy` | smallint NOT NULL | 0=open, 1=invite, 2=approval, 3=purchase | 0 | 0 直接 / 1 邀请 / 2 审批(本阶段 fail-closed) / 3 购买 |

- 默认值全 `0` 对齐回填基线（`type=0 → public/free/open`）。
- 迁移顺序（Step 10）：`ADD COLUMN ... DEFAULT ... NOT NULL` → `UPDATE ... WHERE type=N` 回填 → `DROP COLUMN type`。
- v1 只实现 `open`/`invite`/`purchase`；`approval` 保留枚举但 fail-closed（§8.6）。

#### 8.2.1 迁移安全守卫（Step 10 迁移 00000072 必含）

**abort 守卫（验收 B）**：`channel.type` 在迁移 `00000003` 建表时**无 CHECK 约束**（仅 status/subscriber_count 有 CHECK），type 可为任意 smallint。回填 `UPDATE ... WHERE type=N` 仅命中 type IN(0,1,2)；type NOT IN(0,1,2) 的历史行不命中回填，被**隐式保留 DEFAULT (0,0,0)=C1 公开免费**。当前 `channel_logic_common.erl:113-114` 对未知 type 返回错误（fail-closed），但 Step 11 改用新字段判定后这些行将变 `ok`（fail-open）。迁移回填 UPDATE **之前**必须插入：

```sql
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM channel WHERE type NOT IN (0,1,2)) THEN
    RAISE EXCEPTION '发现非法 type 值，迁移中止，需人工清理'
      USING DETAIL = (SELECT string_agg(type::text||':'||count::text, ', ')
        FROM (SELECT type, count(*) AS count FROM channel
              WHERE type NOT IN (0,1,2) GROUP BY type) t);
  END IF;
END $$;
```

`erlang_migrate_pg` 用 `with_pg_transaction` 单事务包裹，`RAISE EXCEPTION` 会 ROLLBACK 整个迁移（含 ADD COLUMN），实现「阻止自动猜测」。默认 0 本身即隐式猜测，此守卫将其转为显式 abort。

**回填幂等性（Step 10 验收「重复执行验证通过」）**：回填 UPDATE 须加默认态守卫，仅在尚未回填（仍为默认 0）时写入，避免双写期覆盖已漂移数据：

```sql
UPDATE channel SET visibility=0, access_type=0, join_policy=0
  WHERE type=0 AND visibility=0 AND access_type=0 AND join_policy=0;
-- type=1/2 同理加 AND 默认态条件
```

**设计决策：删除 type 列，无触发器**：旧 `type` 列在回填后删除，不再保留兼容桥架。无双向投影触发器——单源事实（三正交字段）零歧义。Step 11 应用代码必须使用 `visibility/access_type/join_policy` 显式写入；API 响应中需要 `type` 字段的，由应用层 `compute_type()` 从三字段计算。此设计消除了 Step 10→11 窗口期脏数据风险（没有触发器就没有投影方向错误），代价是要求 Step 11 应用代码在迁移前部署或同步部署。

**type=2 人工盘点强制门（F2）**：`type=2 → public` 回填假设无法在 SQL 内自动判定可见性（visibility 是新增字段）。迁移文件内建 `channel_access_type2_audit` 表 + `RAISE EXCEPTION` 守卫：存在 type=2 存量行且无审计行 → 迁移中止并回滚；空库/无 type=2 行 → 自动放行。运维流程：`SELECT id, name, type FROM channel WHERE type=2` 人工核对 → 确认后 `INSERT INTO channel_access_type2_audit` 写入审计行 → 重跑迁移。

### 8.3 组合矩阵（四种核心组合 = 四个组合，非死枚举）

| 组合 | visibility | access_type | join_policy | 旧 type 投影 | 准入门 | 语义 |
|------|-----------|-------------|-------------|-------------|--------|------|
| **C1** 公开免费 | 0 public | 0 free | 0 open | `type=0` | 无 | 直接订阅 |
| **C2** 私有免费 | 1 private | 0 free | 1 invite | `type=1` | 邀请 | 邀请 accept → 订阅 |
| **C3** 公开付费 | 0 public | 1 paid | 3 purchase | `type=2` | 购买 | 可发现 + 付费下单 → 订阅 |
| **C4** 私有付费 | 1 private | 1 paid | 3 purchase | `type=1`（安全降级） | 邀请或 link | 不可发现 + 付费下单（须合法上下文） |
| approval | * | * | 2 approval | `type=1`（fail-closed） | — | 本阶段未实现，拒绝 |

- **唯一无门组合是 C1**。C2/C3/C4 均有准入门。验收点：除 C1 外，无组合错误表现为「公开+免费+免邀请」（即不得误把付费/私有组合投影成无门公开）。
- C4 = §2.2 待定 M3 的正交落地，**无需新增 `type` 值**，旧客户端安全降级为 `type=1`（私有）。

#### 8.3.1 非法组合与默认投影（fail-closed 不变量）

三字段值域 `visibility{0,1} × access_type{0,1} × join_policy{0,1,2,3}` = 16 种理论组合。§8.3 定义 5 行（C1-C4 + approval 覆盖 8 种）。剩余 8 种未定义：

| 组合 | vis | acc | jp | 语义判定 | 处理 |
|------|-----|-----|-----|---------|------|
| — | 0 | 0 | 1 | 公开+免费+邀请 | 延期（语义可成立，v1 不实现） |
| — | 0 | 0 | 3 | 免费+购买 | **矛盾**，创建时拒绝 |
| — | 0 | 1 | 0 | 付费+开放 | **矛盾**，创建时拒绝 |
| — | 0 | 1 | 1 | 公开付费+邀请 | 延期（C3 变体，v1 不实现） |
| — | 1 | 0 | 0 | 私有+开放 | 延期（语义可成立，v1 不实现） |
| — | 1 | 0 | 3 | 免费+购买 | **矛盾**，创建时拒绝 |
| — | 1 | 1 | 0 | 付费+开放 | **矛盾**，创建时拒绝 |
| — | 1 | 1 | 1 | 私有付费+邀请 | 延期（C4 变体，v1 不实现） |

- **矛盾组合**（free+purchase、paid+open）由应用层 `channel_access_policy`（Step 11）创建时拒绝。DB 不做复合 CHECK（避免迁移复杂度），可加防御性 `CHECK (NOT (access_type=0 AND join_policy=3))` 禁最危险矛盾（Step 10 可选纵深防御）。
- **延期组合** v1 不实现，按 fail-closed 处理。
- **默认投影不变量**：所有未在 C1-C4+approval 枚举的组合，投影 `type=1`（fail-closed），**不得回落 `type=0`**。若回落 type=0，paid 频道对旧客户端获无门公开访问——正是验收 C 要堵的口子。此不变量使验收 C 在投影层有显式保证。

### 8.4 权限矩阵（对照现有代码 → Step 11/12 收口）

| 维度 | C1 public/free/open | C2 private/free/invite | C3 public/paid/purchase | C4 private/paid/purchase |
|------|---|---|---|---|
| discovery（发现） | ✅ 可发现 | ❌ 不可发现 | ✅ 可发现 | ❌ 不可发现 |
| detail（详情） | ✅ | ✅（须合法邀请/link 上下文） | ✅（展示购买入口） | ❌（无合法上下文拒绝） |
| join（加入） | ✅ 直接订阅 | ✅ 邀请 accept → subscribe | ✅ 付费 → subscribe | ❌ 无上下文拒绝；有上下文 → 付费 → subscribe |
| content（内容） | ✅ `ok` | `is_subscribed` | `has_purchased` | `has_purchased`（订单为唯一权益） |
| 退款后权益 | N/A | N/A | `unsubscribe` | `unsubscribe` |
| 管理员绕过 | `role>0 → ok` | 同左 | 同左 | 同左 |

**现有代码分派（Step 11/12 收口目标）**：

- `channel_logic_common.erl:96-112` `ensure_channel_content_access_by_type/3` 按 `type` 分派（0=ok，1=is_subscribed，2=has_purchased）→ **Step 11** 改 `channel_access_policy` 模块按正交字段判定。
- `channel_logic_subscription.erl:44-58` `subscribe/2` 按 `type` 分派（1=invitation 链，2=has_purchased，0=直接 subscribe）→ **Step 11** 改按 `join_policy` 判定。
- `channel_logic_order.erl:67` `Type =/= 2` 硬编码「只有付费频道支持购买」→ **Step 12** 放宽为 `access_type=paid AND join_policy=purchase`。
- **`channel_logic_invitation.erl:94-125` `do_accept_invitation/4`**（CRITICAL 第四收口点）：第 97 行接受邀请后**直接** `channel_ds:subscribe(ChannelId, Uid)` 无 has_purchased/type/join_policy 检查；第 111-115 行 `not_found_or_expired` 竞态分支也直接 subscribe 且吞错误 `({error, _} -> ok)`。C4 受邀者 accept 后免费订阅付费频道。此路径**不调用 `subscribe/2`** 而直接调 `channel_ds:subscribe`，Step 11 对 `subscribe/2` 的修复无法覆盖 → **Step 11 须将 `do_accept_invitation` 列为第四收口点**：对 `join_policy=purchase` 接受后不直接 subscribe，返回购买上下文，仅 `payment_callback_logic:ensure_subscribed` 确认 `status=1` 已支付后才 subscribe；同时修复竞态分支无条件 subscribe+吞错。
- **Discovery SQL 收口点**（HIGH）：`channel_discovery_logic.erl` 的 `discover`/`featured`/`trending` SQL 仅 `WHERE c.status=1`（第 88/116/241/252 行），无 type/visibility 过滤 → C2/C4 私有频道泄漏到公开发现列表。`channel_repo:list_discover`（第 307 行）`WHERE status=1 AND type=0` → 迁移后按 type 过滤会错误排除 C3（公开付费应可发现）。**Step 11** 须将 discovery SQL 从 `type=0` 改为 `visibility=0` 过滤。

### 8.5 Legacy API 投影矩阵（双向）

**读取侧（DB 迁移时 `type → 新字段` 回填）：**

| 旧 type | visibility | access_type | join_policy | 备注 |
|---------|-----------|-------------|-------------|------|
| 0 | 0 public | 0 free | 0 open | 无歧义 |
| 1 | 1 private | 0 free | 1 invite | 无歧义 |
| 2 | 0 public | 1 paid | 3 purchase | ⚠️ **假设历史付费频道均公开，须 Step 10 逐条盘点核实，不猜测覆盖** |
| NULL/未知 | — | — | — | 报告并阻止自动猜测，不回填 |

**写入侧（新字段 → `type` 投影给旧客户端）：**

| 组合 | 投影 type | 安全语义 |
|------|---------|---------|
| C1 (0,0,0) | 0 | 直接映射 |
| C2 (1,0,1) | 1 | 直接映射 |
| C3 (0,1,3) | 2 | 直接映射 |
| C4 (1,1,3) | **1** | **安全降级为私有**：旧客户端看不到付费属性，但不会 fail-open（旧客户端按私有邀请处理，不会误开放） |
| approval（含 join_policy=2） | **1** | **fail-closed**：投影为私有，旧客户端按私有处理 |

**不变量**：
- 旧客户端**永不会得到 `type>2`**。
- fail-closed 组合（C4、approval）投影为 `type=1`，**不 fail-open**。
- `type=2 → public/paid/purchase` 回填假设须 Step 10 盘点核实；若发现历史付费频道实际私有，按 C4 处理，不猜测覆盖。
- **未识别组合默认投影**：所有未在 C1-C4+approval 枚举的组合（§8.3.1 的 8 种未定义），写入侧投影 `type=1`（fail-closed），**不得回落 `type=0`**——验收 C 在投影层的显式保证。
- **写入侧投影落地层**：选定 DB `BEFORE INSERT OR UPDATE` 触发器据 `NEW.visibility/access_type/join_policy` 计算 `NEW.type`（§8.2.1），Step 10 迁移同步落地。`channel_handler:create`（第 77 行守卫 `Type<0;Type>2`）与 `channel_ds:create_channel` 参数列表须在 Step 11 收口为接受新正交字段。
- **C4 迁移影响**：若 Step 10 盘点发现私有 type=2 频道，重分类为 C4 后旧客户端可见 type 从 2→1，行为变更为 fail-closed（更严格），属安全降级。

### 8.6 approval 契约（保留枚举，本阶段 fail-closed）

- `join_policy=2`（approval）保留为枚举值，**本阶段不实现**。
- 行为：fail-closed，返回明确错误（如「该加入策略暂未开放」），不创建订单、不开通订阅。
- 旧客户端投影 `type=1`（私有），旧客户端按私有邀请处理，不会 fail-open。
- **不得宣称已实现**。未来实现须另起 ADR 并单独设计审批流。

### 8.7 channel_price 作为 v1 单商品价格源

- 现有 `channel_price` 表（`subscription_type` 1一次性/2月/3年）**继续作为 `access_type=paid` 频道唯一金额权威**。
- **不新增并行价格表**。C3/C4 下单金额一律由后端读 `channel_price`，**客户端不传价格**（金额以服务端订单为权威，对照 Step 2 实现要求）。
- 未来多商品演进为 `channel_products` 是**下一阶段事项**，不在 Step 9 范围。

### 8.8 private+purchase 的 invite / shareable purchase link 上下文（C4 专属）

C4（私有付费）**不能因 `join_policy=purchase` 就出现在公开发现列表**。须通过合法上下文进入购买：

| 路径 | 机制 | 状态 |
|------|------|------|
| **invite 路径** | 复用 `channel_invitation` 表；invitee accept 后进入购买页，付款后才 subscribe | ⚠️ 邀请**创建**可先行；C4 **accept 上线**须 Step 11 支付门落地后（见下方停止条件） |
| **shareable purchase link 路径** | 带签名的可分享购买链接 | ⏳ 待 Step 11 定义身份验证/签名/过期/单次多次语义 |

- 无合法上下文时，**discovery / detail / join 三维度均拒绝**。
- **停止条件**：link 身份验证和过期语义未定义前，C4 的 link 路径**不得上线**。invite 路径的**邀请创建**可先行（依赖现有 invitation 链），但 C4 的 **accept 上线**须 Step 11 支付门（`do_accept_invitation` 收口，§8.4）落地后才放行——否则 `do_accept_invitation:97` 直接 subscribe 绕过支付，fail-open 立即生效。

### 8.9 订单与退款矩阵

| 维度 | C1/C2 | C3/C4 |
|------|-------|-------|
| 创建订单 | ❌ N/A | ✅ 创建 `channel_order` |
| 状态机 | N/A | 0待支付 → 1已支付 → 2已退款 / 3已取消 / 4已过期 / 5退款中 |
| settle | N/A | wallet/mock 即时入账；alipay/wechat/stripe 第三方回调发货（回调前不开通） |
| refund | N/A | CAS `1 → 5(refunding) → 2(refunded)` 幂等；同步 `payment_transaction` 状态 |
| 对账 | N/A | `payment_reconcile` 不再误判已退款为「已收款未发货」 |
| C4 下单条件 | — | **Step 12**：`access_type=paid AND join_policy=purchase` + 合法购买上下文 |

- `channel_logic_order.erl:67` `Type =/= 2` 在 **Step 12** 放宽为 `access_type=paid AND join_policy=purchase`。
- 退款状态机 `refunding(5)` 占位态为 Step 3 已交付（`b02e674b`）。

### 8.10 Step 9 验收对照

- ✅ 更新本 ADR（追加 §8，保留 §1-7 历史基线不动）；
- ✅ 输出字段（§8.2）、组合（§8.3）、权限（§8.4）、发现（§8.4 discovery 行）、订单（§8.9）、退款（§8.9）和 legacy API 投影矩阵（§8.5）；
- ✅ 明确 `channel_price` 作为 v1 单商品价格源，不新增并行价格表（§8.7）；
- ✅ 明确 private+purchase 的 invite/shareable link 上下文（§8.8）；
- ✅ **无组合同时表现为公开、免费、免邀请**：除 C1 外无组合投影为 `(0,0,0)`，C4/approval 均降级为 `type=1` 不 fail-open；
- ✅ **旧客户端不会因未知类型 fail-open**：旧客户端永不得 `type>2`，fail-closed 组合投影 `type=1`。

### 8.11 Step 9 停止条件核对

- ❎ ~~无法保证 private+paid 对旧客户端安全降级~~ → 已保证：C4 → `type=1` 安全降级（§8.5）。
- ⏳ ~~迁移需要猜测历史频道可见性~~ → `type=0/1` 映射无歧义；`type=2 → public` 假设须 Step 10 逐条盘点，**不猜测覆盖**（§8.5）。风险已转移至 Step 10 盘点门（§8.2.1 abort 守卫 + 人工 dry-run），非在 Step 9 消除。
- ❎ ~~approval 被误认为本阶段已实现~~ → 明确 fail-closed + 标注未实现（§8.6）。
