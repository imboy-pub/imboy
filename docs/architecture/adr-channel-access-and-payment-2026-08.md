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
