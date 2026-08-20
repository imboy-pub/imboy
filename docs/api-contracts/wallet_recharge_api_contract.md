# 钱包与充值链路 API 契约（余额 / 流水 / 充值订单）

> 状态：存量契约固化（2026-08-20 以 backend 代码为权威源整理）。前缀 `/api/v1`，需要用户登录态。金额单位**分**（integer），与钱包账务统一。

> ## ⚠️ 权威声明：充值订单 status 枚举（三端必须逐值一致）
>
> **0=待支付 1=已支付 2=已取消 3=已退款 4=已过期**
>
> 权威源：`src/repo/recharge_order_repo.erl:7`（模块文档）与 `:33-37`（`?STATUS_PENDING/PAID/CANCELLED/REFUNDED/EXPIRED` 宏），DB CHECK 约束 `chk_recharge_order_status`（`priv/migrations/00000010_payment.up.sql:35`）。
> 背景：2026-08-20 曾发生三端枚举漂移事故（Flutter 曾把 3 写成「已过期」、4 写成「支付失败」，与后端颠倒；已修复）。任何端**禁止凭语义猜测改序**，修改枚举前必须先改本文件与后端宏，并同步三端。

## 1. 端点表

| 端点 | 方法 | 说明 | 权威源 |
|---|---|---|---|
| `/api/v1/wallet/balance` | GET | 余额查询（钱包不存在自动建 0 额度钱包） | `src/api/wallet_handler.erl:87-108` |
| `/api/v1/wallet/transactions` | GET | 流水分页 | `src/api/wallet_handler.erl:110-117` |
| `/api/v1/wallet/topup` | POST | mock 充值（仅非生产环境；amount 100-1000000 分） | `src/api/wallet_handler.erl:119-161` |
| `/api/v1/wallet/recharge/order` | POST | 创建充值订单 | `src/api/wallet_handler.erl:170-184`、`src/logic/recharge_logic.erl:48-73` |
| `/api/v1/wallet/recharge/pay` | POST | 拉起第三方支付 | `src/api/wallet_handler.erl:186-204`、`src/logic/recharge_logic.erl:79-97、145-183` |
| `/api/v1/wallet/recharge/:order_no` | GET | 查询订单状态（仅本人） | `src/api/wallet_handler.erl:206-222`、`src/logic/recharge_logic.erl:126-138` |

充值三接口依赖 payment_gateway 开启，未开启返回 `ERR_FEATURE_DISABLED`（`wallet_handler.erl:35-50`）。

## 2. `GET /api/v1/wallet/balance`

响应 payload（`wallet_handler.erl:103-107`）：

| 字段 | 类型 | 可空 | 语义 | 权威源 |
|---|---|---|---|---|
| `balance` | int | 否 | 余额，**单位分** | wallet_handler.erl:104 |
| `balance_yuan` | float | 否 | 余额（元）= `balance / 100.0`，展示用便捷字段；金额运算一律用分 | wallet_handler.erl:105 |
| `frozen` | int | 否 | 冻结金额，单位分 | wallet_handler.erl:106 |

## 3. `GET /api/v1/wallet/transactions`

请求：`page`（默认 1）、`size`。响应为分页信封 `{total, page, size, list}`（`src/repo/wallet_repo.erl:84-91` + `src/lib/elib_pg.erl:509-531`；注意无 total_page）。

`list[]` 行字段（SQL 列 `wallet_repo.erl:88`，仅返回 `status=1` 的有效流水，按 `id desc` 排序）：

| 字段 | 类型 | 可空 | 语义 | 权威源 |
|---|---|---|---|---|
| `id` | int | 否 | 流水 TSID（bigint JSON number 直出，见 §7.4） | wallet_repo.erl:88 |
| `wallet_id` | int | 否 | 钱包 TSID | wallet_repo.erl:88 |
| `user_id` | int | 否 | 用户 TSID | wallet_repo.erl:88 |
| `amount` | int | 否 | 变动金额（分），正=入账，负=出账 | wallet_repo.erl:88 |
| `balance_after` | int | 否 | 变动后余额（分），事务内计算 | wallet_repo.erl:88、121-122 |
| `tx_type` | int | 否 | 流水类型，见下方枚举 | wallet_repo.erl:88 |
| `reference_no` | string | 否 | 关联业务单号（UNIQUE，幂等键；充值入账固定 `RCH_<order_no>`，`recharge_logic.erl:101-103`） | wallet_repo.erl:88 |
| `remark` | string | 否 | 备注 | wallet_repo.erl:88 |
| `status` | int | 否 | 流水状态：0 待处理（如提现审核中）1 已完成 2 已拒绝（CHECK {0,1,2}，`00000004_social.up.sql:2028`） | wallet_repo.erl:88 |
| `created_at` | int | 否 | 毫秒 int（REST 信封转换） | wallet_repo.erl:88 + `elib_response.erl:30-32` |

**tx_type 枚举**（DB CHECK 值域 `ARRAY[1..11,20,21]`，迁移 `00000004_social.up.sql:2031` → `00000018` → `00000030`）：

| 值 | 语义 | 权威源 |
|---|---|---|
| 1 | 充值/入账（topup） | `src/ds/wallet_ds.erl:93` |
| 2 | 消费/扣减 | `wallet_repo.erl:175-182`（借记腿） |
| 3/4/5 | 退款/冻结/解冻 | admin 类型注释 `imboyadmin/src/types/billing.ts:38`（后端无独立注释，登记口径） |
| 10 | 提现（提现流水分页固定 `tx_type=10`，`wallet_repo.erl:262-269`） | 迁移 000018、`adm_stats_handler.erl:391-395` |
| 11 | 提现拒绝退款 | 迁移 `00000018_wallet_tx_type_withdraw_refund.up.sql`、`adm_finance_handler.erl:493` |
| 20 / 21 | Agent 受控支付 借记(付款人)/贷记(收款方) | 迁移 `00000030_wallet_tx_type_agent_payment.up.sql` |

## 4. `POST /api/v1/wallet/recharge/order`

请求 body：`amount`（int，分）、`payment_method`。白名单随环境（`recharge_logic.erl:35-37、257-269`）：生产 `alipay`/`wechat`/`stripe`，非生产额外允许 `mock`；白名单外报“不支持的支付方式”（`:54-57`；`sandbox` 已禁）。金额校验与 topup 同范围（`recharge_logic.erl:50`）。

响应 payload = 订单全行（`order_transfer/1` 恒等整形，`recharge_logic.erl:294-297`），列定义 `priv/migrations/00000010_payment.up.sql:22-37`：

| 字段 | 类型 | 可空 | 语义 | 权威源 |
|---|---|---|---|---|
| `id` | int | 否 | 订单 TSID | 00000010_payment.up.sql:23 |
| `order_no` | string | 否 | 订单号，`RCH` 前缀 | :24、recharge_order_repo.erl:6 |
| `user_id` | int | 否 | 下单用户 TSID | :25 |
| `amount` | int | 否 | 金额（分，CHECK >=0） | :26 |
| `currency` | string | 否 | 默认 `CNY` | :27 |
| `payment_method` | string | 否 | 支付方式 | :28 |
| `payment_no` | string | 是 | 第三方支付单号（回调/入账时回填，下单阶段为 null） | :29、recharge_logic.erl:150 |
| `status` | int | 否 | **见顶部权威枚举**；新建恒 0 | :30 |
| `paid_at` | int | 是 | 支付时间（毫秒 int） | :31 |
| `expires_at` | int | 是 | 过期时间（毫秒 int）；默认下单 +30 分钟 | :32、recharge_order_repo.erl:39 |
| `extra_data` | object | 是 | 扩展 jsonb | :33 |
| `updated_at` | int | 是 | 毫秒 int | :34 |
| `created_at` | int | 否 | 毫秒 int | :35 |

## 5. `POST /api/v1/wallet/recharge/pay`

请求 body：`order_no`。校验：订单归属当前用户、status=0（否则“订单状态不允许支付”），`recharge_logic.erl:79-97`。

响应 payload（统一信封契约，`recharge_logic.erl:151-161、164-180`）：

| 字段 | 类型 | 可空 | 语义 | 权威源 |
|---|---|---|---|---|
| `payment_method` | string | 否 | 支付方式（binary key，Flutter dart 收 string key） | recharge_logic.erl:157 |
| `payment_no` | string | 否 | 支付单号 | :158 |
| `pay_params` | object | 否 | 网关支付参数透传：alipay `{order_str}`、wechat `{prepay_id}` 或 `{code_url}`、wallet/mock `{}` | :159、152-154 |
| `order_no` | string | 否 | 订单号（供前端轮询） | :160 |
| `amount` | int | 否 | 金额（分） | :161 |
| `status` | int | 否 | mock 即时入账=1；真实网关=0（等回调） | :169、179 |
| `balance` | int | 是 | 仅 mock 即时入账成功时返回新余额 | :170 |

## 6. `GET /api/v1/wallet/recharge/:order_no`

仅本人可查（`recharge_logic.erl:126-138`）。响应 payload 同 §4 订单全行（含最新 status）。入账幂等：已支付订单重复回调直接 `already_credited`，不重复入账（`:99-124`）。

## 7. 前端消费方

| 端 | 文件 | 说明 |
|---|---|---|
| Flutter | `imboyapp/lib/store/api/wallet_api.dart` | `WalletBalance`（balance/balance_yuan/frozen）；`RechargeOrderStatus`（**注释显式引用后端权威定义**，2026-08-20 修正记录在案）；`RechargeOrder`（order_no/amount/status/payment_method）；`payRecharge` 返回 pay_params 透传 |
| Flutter | `imboyapp/lib/page/wallet/wallet_provider.dart` | 流水 `{list,total}` 分页；`pay_params['pay_params']` 唤起 SDK；轮询终态 cancelled(2)/refunded(3)/expired(4) 判定 |
| admin | `imboyadmin/src/types/billing.ts` | `Wallet`（balance/balance_yuan/frozen，单位分）；`WalletTransaction`；`RechargeOrder.status` 注释与权威枚举逐值一致 |

## 8. 已知漂移与注意事项（登记，不改代码）

1. **【已修复，防复发重点】充值 status 枚举**：2026-08-20 三端漂移（Flutter 3/4 含义颠倒）已修。本文件顶部为唯一权威表述；三端消费方注释均已回链后端 `recharge_order_repo`。
2. **【漂移】admin `WalletTransaction` 类型只列 tx_type 1-5**（`billing.ts:38-39`），实际 DB 值域已扩到 1-11,20,21（迁移 18/30）；6-9 未见语义登记，10/11/20/21 缺失。管理端如遇提现/Agent 支付流水会落在类型注释覆盖之外（结构仍兼容，语义文档缺口）。
3. **【漂移】Flutter `WalletTransaction.txType` 注释只写 “1=topup, 2=deduct”**（`wallet_provider.dart:8`），未覆盖 3/4/5/10/11/20/21；`isIncome => txType == 1` 的判定对退款(3)/解冻(5)等正数入账类流水会误判为支出展示。
4. **【漂移】流水行 TSID 未转 string**：`wallet_repo:page_transactions` 直出 bigint（id/wallet_id/user_id 为 JSON number），与 adm/group 的 `tsid_keys_to_bin` 约定不一致；admin `WalletTransaction` 类型却声明 `EntityId`(string)。Dart int64 消费无碍；JS 侧 53bit 精度风险，`safeParseBigIntJson` 仅兜底 16 位以上数字。
5. **`balance_yuan` 是 float 展示字段**：`Balance / 100.0` 有浮点表示误差（如 1.1 元），仅用于展示，禁止参与金额运算或再 ×100 反算。
6. **分页信封无 total_page**：统一 `{total,page,size,list}`（`elib_pg.erl:509-531`），与管理端各分页接口同构。
7. **流水只返回 status=1**：待审核提现（status=0）与已拒绝（status=2）不在 C 端流水中出现。
