# 支付与钱包前后端联调指南

**版本**：1.0.0 | **最后更新**：2026-06-14 | **面向**：Flutter 客户端 + React 管理后台联调

---

## 概览

### 五组 API 职责

| 组别 | 路径前缀 | 职责 | JWT 认证 |
|------|----------|------|---------|
| 钱包 | `/v1/wallet/` | 余额查询、流水、Mock 充值、充值订单链路 | 必须 |
| 充值订单 | `/v1/wallet/recharge/` | 创建订单 → 拉起支付 → 查询状态 | 必须 |
| 支付回调 | `/v1/payment/callback/{gateway}` | 第三方网关主动回调入账 | **免 JWT** |
| SaaS 计费 | `/v1/billing/` | 套餐管理、订阅、用量、账单 | 必须 |
| 频道订单 | `/v1/channel/` | 付费频道购买、支付、退款 | 必须 |

### Sandbox vs Production 差异

| 维度 | Sandbox（mock 网关） | Production（真实网关） |
|------|---------------------|----------------------|
| 充值方式 | `payment_method: "mock"` | `alipay` / `wechat` / `stripe` |
| 入账时机 | 调用 `recharge/pay` 即时入账 | 等待第三方异步回调 |
| 验签 | 直通，不验签 | 各网关签名验证（**TODO[live]，尚未实现**） |
| 是否轮询订单 | 不需要（mock 同步返回 status=1） | 需要轮询 `recharge/{order_no}` |

> **当前限制**：生产网关 live 签名逻辑均标注 `TODO[live]`，尚未实现。只有 mock 通道和 sandbox 可用于端到端验证。erlang_pay 独立支付库（`erlang_pay/`）尚未集成进主服务，真实网关请求转发暂缺。

---

## 统一响应信封

所有接口均返回：

```json
{
  "code": 0,
  "msg": "success.",
  "sv_ts": 1771481621000,
  "payload": { ... }
}
```

| 字段 | 类型 | 说明 |
|------|------|------|
| `code` | integer | 0 = 成功；非 0 = 错误码，见 `error_code.hrl` |
| `msg` | string | 提示文本 |
| `sv_ts` | integer | 服务端毫秒时间戳 |
| `payload` | object/null | 业务数据；错误时为 null |

### TSID 说明

所有 ID 字段（`user_id`、`channel_id`、`subscription_id` 等）均为 64-bit TSID，JSON 传输使用 **integer 类型**。

Flutter 端使用 `safeParseBigIntJson` 转换，类型声明使用 `EntityId`，禁止直接写 `String` 或 `int`。

---

## 金额单位约定

| 场景 | 单位 | 举例 |
|------|------|------|
| 钱包余额、充值订单、SaaS 账单 | **分（fen，integer）** | 5999 = 59.99 元 |
| 频道订单 amount | **元（yuan，numeric(10,2)）** | 59.99 = 59.99 元 |

> **注意**：`channel_order.amount` 是元而不是分，和其他订单的分单位不同，前端显示时不要除以 100。

---

## 沙箱充值闭环步骤

以下四步是用 mock 网关完成一次完整充值的最短路径，适合联调验证。

### Step 1 — 查询余额（可选，确认初始状态）

```http
GET /v1/wallet/balance
Authorization: Bearer <JWT>
```

**响应**：

```json
{
  "code": 0,
  "msg": "success.",
  "sv_ts": 1771481621000,
  "payload": {
    "balance": 0,
    "balance_yuan": 0.0,
    "frozen": 0
  }
}
```

### Step 2 — 创建充值订单

```http
POST /v1/wallet/recharge/order
Authorization: Bearer <JWT>
Content-Type: application/json

{
  "amount": 9900,
  "payment_method": "mock"
}
```

**响应**（status=0 待支付）：

```json
{
  "code": 0,
  "msg": "success.",
  "sv_ts": 1771481622000,
  "payload": {
    "id": 7340678120000000001,
    "order_no": "RCH20260614_A1B2C3D4",
    "user_id": 7340678120000000099,
    "amount": 9900,
    "currency": "CNY",
    "payment_method": "mock",
    "payment_no": null,
    "status": 0,
    "paid_at": null,
    "expires_at": "2026-06-14T13:00:00Z",
    "extra_data": null,
    "created_at": "2026-06-14T12:00:00Z"
  }
}
```

### Step 3 — 拉起支付（mock 即时入账）

```http
POST /v1/wallet/recharge/pay
Authorization: Bearer <JWT>
Content-Type: application/json

{
  "order_no": "RCH20260614_A1B2C3D4"
}
```

**响应**（mock 同步返回 status=1，balance 为入账后余额）：

```json
{
  "code": 0,
  "msg": "success.",
  "sv_ts": 1771481623000,
  "payload": {
    "order_no": "RCH20260614_A1B2C3D4",
    "payment_no": "MOCK_PAY_XYZ",
    "payment_method": "mock",
    "amount": 9900,
    "status": 1,
    "balance": 9900
  }
}
```

> 真实网关时 `status=0`，`balance` 字段不存在，需要轮询 Step 4。

### Step 4 — 轮询订单状态（真实网关使用）

```http
GET /v1/wallet/recharge/RCH20260614_A1B2C3D4
Authorization: Bearer <JWT>
```

**响应**（已支付时 status=1）：

```json
{
  "code": 0,
  "msg": "success.",
  "sv_ts": 1771481630000,
  "payload": {
    "order_no": "RCH20260614_A1B2C3D4",
    "status": 1,
    "paid_at": "2026-06-14T12:01:00Z",
    "payment_no": "MOCK_PAY_XYZ",
    "amount": 9900
  }
}
```

建议：间隔 2 秒轮询，最多 30 次（约 60 秒），超时后提示用户手动刷新。

### Mock 直接加款（快速调试专用）

跳过订单，直接向钱包加款（仅非生产环境）：

```http
POST /v1/wallet/topup
Authorization: Bearer <JWT>
Content-Type: application/json

{
  "amount": 50000
}
```

**响应**：

```json
{
  "code": 0,
  "msg": "success.",
  "sv_ts": 1771481640000,
  "payload": {
    "balance": 59900,
    "balance_yuan": 599.0,
    "reference_no": "TOP1771481640000_ABCD1234"
  }
}
```

---

## 端点详情

### 一、钱包

#### GET /v1/wallet/balance — 查询余额

| 项目 | 说明 |
|------|------|
| 认证 | Bearer JWT |
| 请求参数 | 无 |

**响应 payload**：

| 字段 | 类型 | 说明 |
|------|------|------|
| `balance` | integer | 余额，单位分 |
| `balance_yuan` | number | 余额元数（balance/100.0），仅供显示 |
| `frozen` | integer | 冻结金额，单位分 |

---

#### GET /v1/wallet/transactions — 钱包流水

| 项目 | 说明 |
|------|------|
| 认证 | Bearer JWT |

**Query 参数**：

| 参数 | 类型 | 默认 | 说明 |
|------|------|------|------|
| `page` | integer | 1 | 页码 |
| `page_size` | integer | 20 | 每页数量，最大 100 |

**响应 payload**：

| 字段 | 类型 | 说明 |
|------|------|------|
| `list` | array | WalletTransaction 列表 |
| `total` | integer | 总记录数 |
| `page` | integer | 当前页 |
| `page_size` | integer | 每页数量 |

**WalletTransaction 字段**：

| 字段 | 类型 | 说明 |
|------|------|------|
| `id` | integer | TSID |
| `user_id` | integer | 用户 TSID |
| `amount` | integer | 变动金额，单位分；正数入账，负数支出 |
| `balance_after` | integer | 变动后余额，单位分 |
| `reference_no` | string | 幂等键，格式 `TOP<ts>_<hex>` 或 `RCH_<order_no>` |
| `remark` | string/null | 备注 |
| `created_at` | string | ISO 8601 |

---

#### POST /v1/wallet/topup — Mock 直接充值（非生产专用）

| 项目 | 说明 |
|------|------|
| 认证 | Bearer JWT |

**请求体**：

| 字段 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `amount` | integer | 是 | 充值金额，单位分；范围 100~1000000 |

**响应 payload**：

| 字段 | 类型 | 说明 |
|------|------|------|
| `balance` | integer | 充值后余额，单位分 |
| `balance_yuan` | number | 元数，仅供显示 |
| `reference_no` | string | 本次流水幂等键 |

---

### 二、充值订单

#### POST /v1/wallet/recharge/order — 创建充值订单

| 项目 | 说明 |
|------|------|
| 认证 | Bearer JWT |

**请求体**：

| 字段 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `amount` | integer | 是 | 充值金额，单位分；范围 100~1000000 |
| `payment_method` | string | 是 | `alipay`/`wechat`/`stripe`；非生产另允许 `mock` |

**响应 payload**（RechargeOrder）：

| 字段 | 类型 | 说明 |
|------|------|------|
| `order_no` | string | 订单号，全局 UNIQUE，用于后续步骤 |
| `status` | integer | 0=待支付 / 1=已支付 / 2=退款中 / 3=已退款 / 4=已过期 |
| `payment_method` | string | 支付方式 |
| `expires_at` | string/null | 订单过期时间 |

---

#### POST /v1/wallet/recharge/pay — 拉起支付

| 项目 | 说明 |
|------|------|
| 认证 | Bearer JWT |

**请求体**：

| 字段 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `order_no` | string | 是 | 充值订单号 |

**响应 payload**：

| 字段 | 类型 | 说明 |
|------|------|------|
| `order_no` | string | 订单号 |
| `payment_no` | string | 网关支付单号 |
| `payment_method` | string | 支付方式 |
| `amount` | integer | 金额，单位分 |
| `status` | integer | mock=1（已支付）；真实网关=0（等待回调） |
| `balance` | integer/null | mock 入账后新余额，单位分；真实网关无此字段 |

---

#### GET /v1/wallet/recharge/{order_no} — 查询充值订单状态

| 项目 | 说明 |
|------|------|
| 认证 | Bearer JWT |
| Path 参数 | `order_no`：充值订单号 |

**响应 payload**：完整 RechargeOrder 对象（字段见创建订单部分）。

---

### 三、支付回调

#### POST /v1/payment/callback/{gateway} — 统一回调 Webhook

| 项目 | 说明 |
|------|------|
| 认证 | **免 JWT**（第三方服务器主动推送，前端不调用此接口） |
| Path 参数 | `gateway`：`alipay` / `wechat` / `stripe` / `mock` |

**沙箱（mock gateway）归一化字段**：

| 字段 | 类型 | 说明 |
|------|------|------|
| `gateway_payment_no` | string | 第三方支付单号（回调幂等键） |
| `biz_type` | integer | 1=充值 / 2=频道订单 / 3=SaaS 账单 |
| `biz_order_no` | string | 业务订单号 |
| `user_id` | integer | 用户 TSID |
| `amount` | integer | 金额，单位分 |
| `trade_no` | string | 平台内部交易单号 |

**各网关应答格式**：

| 网关 | 成功应答 | 失败 / 验签错误 |
|------|---------|----------------|
| alipay | 文本 `success`（必须是此字符串，否则支付宝重推） | 其他文本 |
| wechat | `{"code":"SUCCESS","message":"成功"}` | 其他 JSON |
| stripe | `{"received":true}` | HTTP 400 + `{"received":false}` |
| mock | 标准信封 `{"code":0,...}` | 标准信封带 code≠0 |

> **验签现状**：sandbox 直通不验签；各网关 live 签名均标注 `TODO[live]`，尚未实现。

---

### 四、SaaS 计费

#### POST /v1/billing/plan — 创建套餐（管理端）

| 项目 | 说明 |
|------|------|
| 认证 | Bearer JWT（管理员权限） |

**请求体**：

| 字段 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `code` | string | 是 | 套餐唯一编码，如 `starter`/`pro`/`max` |
| `name` | string | 是 | 套餐名称 |
| `price` | integer | 是 | 价格，单位分 |
| `billing_period` | string | 是 | `month` 或 `year` |
| `quota_config` | object | 否 | 配额上限 jsonb |
| `description` | string | 否 | 套餐描述 |

**响应 payload**：`{"id": <plan_id>}`

---

#### POST /v1/billing/plan/update — 更新套餐（管理端）

**请求体**：`id` 必填，其余字段均为可选更新。

| 字段 | 类型 | 说明 |
|------|------|------|
| `id` | integer | 必填 |
| `name` | string | 可选 |
| `price` | integer | 可选，单位分 |
| `billing_period` | string | 可选 |
| `quota_config` | object | 可选 |
| `description` | string | 可选 |
| `status` | integer | 可选；0=下架 / 1=上架 |

---

#### GET /v1/billing/plan/list — 套餐列表

返回所有 `status=1` 上架套餐，列表在 `payload.list`。

---

#### POST /v1/billing/subscribe — 订阅套餐

**请求体**：

| 字段 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `plan_id` | integer | 是 | 套餐 ID |
| `tenant_id` | integer | 否 | 租户 ID，单租户传 0 或省略 |
| `trial` | boolean | 否 | true=创建试用订阅 |

**DB 约束**：`uniq_billing_sub_active`——同一租户同一时刻只允许一条 `status IN(0,1)` 的订阅，重复订阅返回错误。

**响应 payload**：`{"subscription_id": <id>}`

---

#### POST /v1/billing/renew — 续费

| 字段 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `subscription_id` | integer | 是 | 订阅 ID |

**响应 payload**：

| 字段 | 类型 | 说明 |
|------|------|------|
| `subscription_id` | integer | 订阅 ID |
| `current_period_end_ms` | integer | 续费后新到期时间戳（毫秒） |

---

#### POST /v1/billing/cancel — 取消订阅

**请求体**：`{"subscription_id": <id>}`  
订阅状态变为 3（取消），不立即退款。

---

#### GET /v1/billing/subscription — 查询当前订阅

**Query 参数**：`tenant_id`（默认 0）

**响应 payload**：BillingSubscription 对象，无订阅时返回空 map。

BillingSubscription 字段：

| 字段 | 类型 | 说明 |
|------|------|------|
| `id` | integer | 订阅 ID |
| `plan_id` | integer | 套餐 ID |
| `status` | integer | 0=试用 / 1=生效 / 2=过期 / 3=取消 |
| `current_period_start` | string/null | 当前计费期开始 |
| `current_period_end` | string/null | 当前计费期结束 |
| `auto_renew` | boolean | 是否自动续费 |

---

#### POST /v1/billing/usage — 上报用量（增量）

**请求体**：

| 字段 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `subscription_id` | integer | 是 | 订阅 ID |
| `metric` | string | 是 | 指标键，如 `message`/`storage`/`dau` |
| `used` | integer | 是 | 本次增量（非总量），非负整数 |
| `period` | string | 否 | 周期标识，如 `2026-06`；缺省后端取当前月份 |

**幂等机制**：DB `uniq_billing_usage_sub_metric_period`（subscription_id, metric, period）做 upsert 累加。

**响应 payload**：`{"metric": "message", "used": 12500}`（当前周期累计总量）

---

#### GET /v1/billing/quota — 查询配额

**Query 参数**：

| 参数 | 必填 | 说明 |
|------|------|------|
| `subscription_id` | 是 | 订阅 ID |
| `metric` | 是 | 指标键 |
| `period` | 否 | 周期，缺省当前月份 |

**响应 payload**：源码未明确具体字段，由 `billing_logic:check_quota/3` 返回。

---

#### POST /v1/billing/invoice/generate — 生成账单

**请求体**：`{"subscription_id": <id>}`

**幂等**：DB `uniq_billing_invoice_sub_period`（subscription_id, period_start, period_end）。

**响应 payload**：

| 字段 | 类型 | 说明 |
|------|------|------|
| `invoice_id` | integer | 新账单 ID（已存在时无此字段） |
| `already_generated` | boolean/null | 已存在时为 true |

---

#### POST /v1/billing/invoice/pay — 支付账单

**请求体**：

| 字段 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `invoice_no` | string | 是 | 账单号 |
| `payment_method` | string | 是 | `wallet`/`alipay`/`wechat`/`stripe`/`mock` |

**响应 payload**：源码未明确具体字段，由 `billing_logic:pay_invoice/2` 返回。

---

#### GET /v1/billing/invoice/list — 账单列表

**Query 参数**：`subscription_id`（必填）

**响应 payload.list**（BillingInvoice 字段）：

| 字段 | 类型 | 说明 |
|------|------|------|
| `invoice_no` | string | 账单号，UNIQUE |
| `amount` | integer | 金额，单位分 |
| `status` | integer | 0=待支付 / 1=已付 / 2=逾期 |
| `period_start` | string/null | 计费期开始 |
| `period_end` | string/null | 计费期结束 |
| `paid_at` | string/null | 支付时间 |

---

### 五、频道订单

#### POST /v1/channel/{channel_id}/order — 创建频道订单

| 项目 | 说明 |
|------|------|
| 认证 | Bearer JWT |
| Path 参数 | `channel_id`：频道 TSID（整数字符串） |

**前置条件**：
- 频道类型 `type=2`（付费频道）且 `status=1`（已启用）
- 当前用户未购买过该频道
- DB 约束：`uniq_channel_order_pending`——同一用户同一频道只允许一笔待支付订单

**响应 payload**（ChannelOrder）：

| 字段 | 类型 | 说明 |
|------|------|------|
| `order_no` | string | 订单号 |
| `channel_id` | integer | 频道 TSID |
| `amount` | number | 金额，**元**（非分），如 `59.99` |
| `status` | integer | 0=待支付 |
| `payment_method` | string | 初始默认 `wallet` |

---

#### POST /v1/channel/order/pay — 支付频道订单

**请求体**：

| 字段 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `order_no` | string | 是 | 频道订单号 |

支付成功后自动调用 `channel_ds:subscribe` 开通频道订阅，并推送通知。

**响应 payload**：空 map `{}`。

---

#### POST /v1/channel/order/refund — 申请退款

**请求体**：

| 字段 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `order_no` | string | 是 | 频道订单号 |
| `refund_reason` | string | 否 | 退款原因，缺省 "用户申请退款" |

**前置条件**：订单 `status=1`（已支付）。退款后自动取消频道订阅。

**幂等**：已退款（`status=2`）再次调用返回错误，不会重复退款。

**响应 payload**：空 map `{}`。

> **注意**：路由注册顺序中 `/v1/channel/order/refund` 早于 `/v1/channel/orders/:order_no`，确保 "refund" 不被当作 order_no 解析。

---

#### GET /v1/channel/orders/{order_no} — 查询频道订单详情

仅订单归属用户可查。**响应 payload**：ChannelOrder 对象。

---

#### GET /v1/channel/orders/my — 我的频道订单列表

返回最近 50 条，按创建时间降序。**响应 payload**：`{"list": [...]}`。

---

## 幂等说明

| 场景 | 幂等键 | 实现位置 |
|------|--------|---------|
| Mock 直接加款（topup） | `reference_no`（格式 `TOP<ts>_<hex>`），DB UNIQUE | `wallet_transaction.reference_no` |
| 充值入账（callback 后） | `reference_no = RCH_<order_no>`，DB UNIQUE | `wallet_transaction.reference_no` |
| 支付回调防重放 | `(gateway, gateway_payment_no)` 联合唯一索引 | `payment_transaction` 表 |
| SaaS 用量上报 | `(subscription_id, metric, period)` 联合唯一，upsert 累加 | `billing_usage` 表 |
| SaaS 账单生成 | `(subscription_id, period_start, period_end)` 联合唯一 | `billing_invoice` 表 |
| 频道待支付订单 | `(user_id, channel_id, status=0)` 唯一约束 | `uniq_channel_order_pending` |
| 租户活跃订阅 | `(tenant_id, status IN(0,1))` 唯一约束 | `uniq_billing_sub_active` |

---

## 回调对接详情

### 回调流程

```
第三方支付服务器
    → POST /v1/payment/callback/{gateway}（HTTP，免 JWT）
        → 后端读取 RawBody 原始字节
        → payment_callback_handler:normalize/3 按网关归一化
        → payment_callback_logic:handle/3：
            1. 验签（sandbox 直通；live TODO[live]）
            2. 幂等检查（gateway + gateway_payment_no）
            3. 入账/流水
        → 按网关格式应答
```

### 沙箱本地模拟回调

```bash
# 模拟 mock 网关回调
curl -X POST http://127.0.0.1:9800/v1/payment/callback/mock \
  -H "Content-Type: application/json" \
  -d '{
    "gateway_payment_no": "MOCK_PAY_XYZ001",
    "biz_type": 1,
    "biz_order_no": "RCH20260614_A1B2C3D4",
    "user_id": 7340678120000000099,
    "amount": 9900,
    "trade_no": "TRD20260614_001"
  }'
```

---

## 已知限制与 TODO

| 编号 | 限制 | 影响范围 | 状态 |
|------|------|---------|------|
| L1 | 生产网关签名验证未实现（alipay/wechat/stripe live 签名均 TODO[live]） | 所有真实网关回调 | 待排期 |
| L2 | erlang_pay 独立库未集成进主服务（现为子目录 `erlang_pay/`） | 生产网关请求转发 | 待排期 |
| ~~L3~~ | ~~recharge 入账非单事务丢钱风险~~ **已修复**：入账重构为单事务（`recharge_order_ds:credit_in_tx/4` 订单状态翻转+钱包加余额+流水原子完成，幂等），见 `recharge_logic:credit_order/3`（提交 3836042） | 充值成功入账 | ✅ 已解决 |
| L4 | 频道订单 amount 为元，与其他金额字段的分单位不一致 | 前端金额显示 | 已知，接口约定不变 |
| L5 | `/v1/billing/quota` 和 `/v1/billing/invoice/pay` 的响应 payload 字段源码未明确 | SaaS 联调 | 需读源码 billing_logic 确认 |
| L6 | 订阅、计费相关接口暂无权限细粒度校验（管理端与用户端共享 JWT）  | SaaS 安全 | 待排期 |
| L7 | mock 充值（topup）在生产环境应禁用，需通过 `IMBOYENV` 或 `payment.mode` 配置保障 | 安全边界 | 待确认 |

---

## 错误码参考（支付相关）

通用错误码位于 `include/error_code.hrl`，支付相关常见：

| code | 含义 |
|------|------|
| 0 | 成功 |
| 40001 | 参数错误（amount 范围/缺字段等） |
| 40401 | 订单不存在 |
| 40402 | 无权操作（非订单归属用户） |
| 40403 | 频道不支持付费购买（type≠2 或 status≠1） |
| 40901 | 重复操作（幂等冲突，如同频道已有待支付订单） |
| 42201 | 余额不足（wallet 支付时） |
| 50001 | 服务端内部错误 |

> 具体错误码以 `error_code.hrl` 为准，上表为常见场景参考，不完整。

---

## 快速检查清单（联调前）

- [ ] 后端 `IMBOYENV` 已设为 `local`，mock 网关白名单生效
- [ ] JWT token 有效（通过 `/passport/login` 获取）
- [ ] TSID 字段已用 `safeParseBigIntJson` 处理，未直接 JSON.parse
- [ ] 频道订单金额字段 `amount` 前端不除以 100（已是元）
- [ ] 轮询订单状态时设置最大重试次数（建议 30 次，间隔 2s）
- [ ] 真实网关回调不接受前端直接调用（仅第三方服务器推送）
- [ ] 沙箱联调完毕后确认 mock 通道在生产构建中已禁用
