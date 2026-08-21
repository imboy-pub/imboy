# TDD 证据报告：充值主动查单确认（confirm）

> 任务来源：用户报告「支付宝支付线上成功，但支付宝没扣款、IMBoyAPP 钱包余额没增加」（2026-08-21）。
> 旅程为本次 TDD 运行中推导（无 plan 文件）。

## 1. 根因（运行时取证）

- 本地 DB（imboy_v1）：2026-08-21 14:35–14:36 三笔 1 元 alipay 充值订单全部 `status=0` 挂起，`payment_transaction` 无任何 alipay 回调记录。
- 挂单用户 uid=1000000060（余额 0）；生产库无对应订单（用户走本地沙箱链路）。
- 上一会话经 rpc set_env 注入的沙箱凭据随节点死亡丢失（sys.local.config 槽为空占位）。
- 架构缺口：入账**只依赖异步回调（notify）**；notify 丢失/不可达（沙箱无公网地址、生产偶发丢包）→ 已付款订单永远待支付。此为支付子系统早已登记的 P0 缺口（query 查单 + 幂等双层）。

## 2. 用户旅程

- As a 用户, 我支付完成后回到 App, 想让服务端主动向支付宝查单并把已付款订单入账, 使得异步回调丢失时余额也能正确增加。

## 3. 任务报告

| 任务 | 执行摘要 | 验证命令 | 结果 |
|---|---|---|---|
| RED：confirm 测试 | 写 6 用例，`undefined_function payment_gateway:query_order/2` 失败=预期缺口 | `make eunit-local t=recharge_logic_confirm_tests` | 6 failed（有效 RED） |
| GREEN：后端实现 | gateway optional callback + alipay query_order + recharge_logic:confirm/2 + handler + 路由 | 同上 | 6/6 passed |
| 回归 | 充值/支付 8 套件 | 逐模块 `make eunit-local t=…` | 全 PASS |
| 客户端 | 轮询改调 confirm + WalletApi.confirmRechargeOrder + 注入构造 | `flutter test test/unit_test/page/wallet/` | 14/14 passed |
| 真实挂单 HTTP 验证 | 本人 token confirm 真实挂单 RCH1787294174121079640 | curl POST /api/v1/wallet/recharge/confirm | `{status:0}`（无凭据→归一 pending，不误报） |

提交：imboy `587d8a7d`、imboyapp `bc77aebd`（均未 push）。

## 4. 测试规格

| # | 保证 | 测试 | 类型 | 结果 |
|---|---|---|---|---|
| 1 | 非本人订单 confirm 被拒（IDOR）且不触网关 | recharge_logic_confirm_tests: confirm_non_owner_rejected | unit | PASS |
| 2 | 已支付订单幂等返回 status=1，不重复入账 | confirm_already_paid_idempotent | unit | PASS |
| 3 | 网关 TRADE_SUCCESS → 幂等入账并返回新余额 | confirm_gateway_success_credits | unit | PASS |
| 4 | WAIT_BUYER_PAY / 交易不存在 / 网关不支持 → 如实 pending | confirm_gateway_{pending,not_exist,unsupported}_keeps_pending | unit | PASS |
| 5 | 客户端轮询：confirm 已付 → recharge 成功（1 次调用） | wallet_notifier_recharge_confirm_test: RC-1 | unit | PASS |
| 6 | 客户端轮询：终态（已退款）→ 停止轮询失败返回 | RC-2 | unit | PASS |
| 7 | 客户端轮询：持续待支付 → 6 次上限后失败 | RC-3 | unit | PASS |

## 5. 覆盖与已知缺口

- 未覆盖（有意）：支付宝网关 query_order 的 HTTP 细节由 erlang_pay 既有测试与生产探针覆盖；confirm 只依赖归一后的 trade_state。
- **验收前置**：沙箱凭据（appId/应用私钥/支付宝公钥）未注入本地节点（等用户提供）——注入后即可用 confirm 把三笔真实挂单查单入账或如实保持待支付。
- 生产部署：需蓝绿发布 alpha.45 后生产同样受益（notify 丢包兜底）。
