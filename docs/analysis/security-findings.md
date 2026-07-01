# 安全与规范一致性扫描进度

> **正本 OpenAPI**: `imboy/api/openapi.yaml` (v1.0.0-rc.3, 551 paths)
> **路由权威**: `imboy/src/imboy_router.erl` (603 routes)
> **参考审计**: `security-auth-middleware-audit.md` (2026-05-27，认证中间件已审)
> **更新规则**: 每轮处理一个 API 后追加到 ## 已处理；不确定项追加到 ## 待人工确认

---

## 已处理

| 路由/模块 | 处理日期 | 结论 | 提交 |
|-----------|---------|------|------|
| `auth_middleware` / `auth_middleware_api_v1` | 2026-05-27 | 见 security-auth-middleware-audit.md | — |
| `/v1/wallet/recharge/:order_no` | 2026-07-01 | 安全通过（IDOR 防护 `order.user_id==current_uid` + SQL 全 `$N` 参数化 + JWT 鉴权）；补 openapi spec（路由原先未文档化）；加 `recharge_logic_query_tests` 本人/非本人/不存在 三用例。输入校验 LOW：order_no 仅校验非空无格式上限（IDOR+参数化已挡实质风险，暂不改） | 4c9b5a36 |
| `/v1/wallet/recharge/order` | 2026-07-01 | 安全通过（金额 `[Min,Max]` 区间校验 + 支付方式白名单[生产排除 mock] + `user_id` 来自 JWT 无 mass-assignment + SQL 全参数化 + JWT 鉴权）；补 openapi spec（路由原先未文档化）；加 `recharge_logic_create_tests` 正常/金额越界/方式非法 三用例 | 44782764 |
| `/v1/wallet/recharge/pay` | 2026-07-01 | 安全通过（IDOR 防护 `order.user_id==current_uid` + 状态必须待支付 + 金额/方式取自订单快照防篡改 + SQL 全参数化 + JWT）；补 openapi spec；加 `recharge_logic_pay_access_tests` 非本人/非待支付 两拒绝用例（happy path 已由 `envelope_tests` 覆盖） | 760d67e7 |
| `/v1/wallet/topup` | 2026-07-01 | 🚨 **CRITICAL 已修**：mock 充值原先三层（handler/logic/ds）均无生产门禁，生产可凭空生成余额。修复：新增 `wallet_logic:topup_enabled_for_env/1`（pro/prod/production/未配置→拒绝）+ `wallet_handler:topup/2` 入口拦截；加 `wallet_logic_topup_tests` 7 用例；更新 topup.yaml | 3b940688 |
| `/v1/wallet/withdraw` | 2026-07-01 | 安全通过（JWT + IDOR 操作本人钱包 + 金额整数≥100 + 渠道白名单 alipay/wechat + 账号非空 + 原子事务 `balance-$1>=0` 防透支双花 + 全参数化 SQL + 管理员审批流 reject 则退款）；补 openapi spec；加 `withdrawal_logic_tests` 正常/余额不足/金额越界 三用例 | 0ae2d186 |
| `/v1/wallet/transfer/send` | 2026-07-01 | 核心安全通过（`sender_uid` 来自 JWT 防伪造 + 自转 `sender==receiver` 拒绝 + 金额≥100 + 原子 `balance>=amount` 防透支双花 + 全参数化 SQL + 托管 accept/refund 模式）；**修健壮性**：handler `binary_to_integer` 非数字原 badarg 500，改 `try-of-catch` 优雅报错（OTP29 `catch` 已废弃）；加 `transfer_logic_tests` 自转/金额越界/正常 三用例 + 补 openapi spec | e7f17dc3 |

---

## 待人工确认

_（loop 遇到不确定改动时追加到此处）_

- **[加固建议] `/v1/wallet/topup` 路由级 dev-only 注册**：当前修复已在 handler 入口拦截生产请求（CRITICAL 已堵，见 3b940688），但路由仍于 `get_routes()` 在生产注册（仅返回错误）。可进一步加固：仿 `test_routes()` 用 `is_dev_env()` 把 topup 移到 dev-only 注册段，生产连路由都不暴露（彻底消除攻击面）。需先评估是否有运维脚本/监控依赖该端点。— 2026-07-01

---

## 路由清单（待扫描）

> 按优先级排序：P0=高风险（支付/E2EE/认证）→ P1=业务核心 → P2=管理后台 → P3=只读/辅助

### P0 — 支付 & 钱包（高风险）

- [x] `/v1/wallet/recharge/:order_no` (#4c9b5a36)
- [x] `/v1/wallet/recharge/order` (#44782764)
- [x] `/v1/wallet/recharge/pay` (#760d67e7)
- [x] `/v1/wallet/topup` (#3b940688 🚨CRITICAL 已修)
- [x] `/v1/wallet/withdraw` (#0ae2d186)
- [x] `/v1/wallet/transfer/send` (#e7f17dc3)
- [ ] `/v1/wallet/transfer/accept`
- [ ] `/v1/wallet/balance`
- [ ] `/v1/wallet/transactions`
- [ ] `/v1/wallet/red_packet/send`
- [ ] `/v1/wallet/red_packet/open`
- [ ] `/v1/wallet/red_packet/:id/detail`
- [ ] `/v1/payment/callback/:gateway`
- [ ] `/v1/billing/subscribe`
- [ ] `/v1/billing/cancel`
- [ ] `/v1/billing/renew`
- [ ] `/v1/billing/invoice/pay`
- [ ] `/v1/billing/invoice/generate`
- [ ] `/v1/billing/invoice/list`
- [ ] `/v1/billing/plan/update`

### P0 — E2EE（密钥安全）

- [ ] `/v1/e2ee/report_device_key`
- [ ] `/v1/e2ee/user_keys`
- [ ] `/v1/e2ee/key/status`
- [ ] `/v1/e2ee/transfer/create`
- [ ] `/v1/e2ee/transfer/accept`
- [ ] `/v1/e2ee/transfer/confirm`
- [ ] `/v1/e2ee/transfer/cancel`
- [ ] `/v1/e2ee/transfer/info`
- [ ] `/v1/e2ee/transfer/pending`
- [ ] `/v1/e2ee/backup/delete`
- [ ] `/v1/e2ee/backup/list`
- [ ] `/v1/e2ee/compliance_key`
- [ ] `/v1/e2ee/recovery/start`
- [ ] `/v1/e2ee/notifications/pull`
- [ ] `/v1/e2ee/social/contacts`
- [ ] `/v1/e2ee/social/create_shards`
- [ ] `/v1/e2ee/social/decrypt_shard`
- [ ] `/v1/e2ee/social/recover`

### P0 — 认证 & 账号

- [ ] `/v1/passport/login`
- [ ] `/v1/passport/signup`
- [ ] `/v1/passport/quick_login`
- [ ] `/v1/passport/findpassword`
- [ ] `/v1/passport/getcode`
- [ ] `/v1/passport/bind_mail`
- [ ] `/v1/passport/qr_login/create`
- [ ] `/v1/passport/qr_login/scan`
- [ ] `/v1/passport/qr_login/confirm`
- [ ] `/v1/passport/qr_login/cancel`
- [ ] `/v1/passport/qr_login/status`
- [ ] `/v1/passport/qr_login/subscribe`
- [ ] `/v1/refreshtoken`
- [ ] `/v1/user/change_password`
- [ ] `/v1/user/set_password`
- [ ] `/v1/user/apply_logout`
- [ ] `/v1/user/credential`
- [ ] `/v1/user/export_data`
- [ ] `/v1/user_device/kick`
- [ ] `/v1/user_device/kick-others`
- [ ] `/v1/user_device/sessions`
- [ ] `/v1/user_device/check_login`

### P1 — 消息核心

- [ ] `/v1/msg/offline`
- [ ] `/v1/msg/offline_ack`
- [ ] `/v1/msg/history`
- [ ] `/v1/msg/forward`
- [ ] `/v1/msg/pin`
- [ ] `/v1/msg/reaction/add`
- [ ] `/v1/msg/reaction/remove`
- [ ] `/v1/msg/reaction/list`
- [ ] `/v1/msg/read_stats`
- [ ] `/v1/ws`

### P1 — 群组

- [ ] `/v1/group/add`
- [ ] `/v1/group/edit`
- [ ] `/v1/group/dissolve`
- [ ] `/v1/group/transfer`
- [ ] `/v1/group/detail`
- [ ] `/v1/group/page`
- [ ] `/v1/group_member/join`
- [ ] `/v1/group_member/leave`
- [ ] `/v1/group_member/role`
- [ ] `/v1/group_member/mute`
- [ ] `/v1/group_member/unmute`
- [ ] `/v1/group_member/alias`
- [ ] `/v1/group_member/page`
- [ ] `/v1/group_member/same_group`
- [ ] `/v1/group/vote/create`
- [ ] `/v1/group/vote/cast`
- [ ] `/v1/group/vote/close`
- [ ] `/v1/group/vote/cancel`
- [ ] `/v1/group/task/create`
- [ ] `/v1/group/task/assign`
- [ ] `/v1/group/task/submit`
- [ ] `/v1/group/task/review`

### P1 — 频道

- [ ] `/v1/channel/create`
- [ ] `/v1/channel/:channel_id`
- [ ] `/v1/channel/:channel_id/subscribe`
- [ ] `/v1/channel/:channel_id/unsubscribe`
- [ ] `/v1/channel/:channel_id/message`
- [ ] `/v1/channel/:channel_id/message/:message_id/delete`
- [ ] `/v1/channel/:channel_id/message/:message_id/pin`
- [ ] `/v1/channel/:channel_id/message/:message_id/revoke`
- [ ] `/v1/channel/:channel_id/order`
- [ ] `/v1/channel/order/pay`
- [ ] `/v1/channel/order/refund`

### P1 — 附件

- [ ] `/v1/attachment/presign`
- [ ] `/v1/attachment/confirm`
- [ ] `/v1/attachment/view_url`

### P2 — 管理后台（高权限）

- [ ] `/adm/finance/withdrawals/complete`
- [ ] `/adm/finance/withdrawals/reject`
- [ ] `/adm/channel/order/refund`
- [ ] `/adm/user/ban`
- [ ] `/adm/user/unban`
- [ ] `/adm/user/force_logout`
- [ ] `/adm/admin/create`
- [ ] `/adm/admin/disable`
- [ ] `/adm/admin/assign_role`
- [ ] `/adm/admin/compliance_key/create`
- [ ] `/adm/admin/compliance_key/revoke`
- [ ] `/adm/setup/init`
- [ ] `/adm/passport/do_login`
- [ ] `/adm/role/permission/update`
- [ ] `/adm/storage/orphan/cleanup`

### P2 — 用户资料 & 好友

- [ ] `/v1/user/update`
- [ ] `/v1/user/setting`
- [ ] `/v1/user/change_state`
- [ ] `/v1/user/show`
- [ ] `/v1/user/search`
- [ ] `/v1/friend/add`
- [ ] `/v1/friend/confirm`
- [ ] `/v1/friend/reject`
- [ ] `/v1/friend/delete`
- [ ] `/v1/friend/denylist/add`
- [ ] `/v1/friend/denylist/remove`

### P3 — 只读 & 辅助

- [ ] `/v1/app/features`
- [ ] `/v1/app/policy`
- [ ] `/v1/app/manifest`
- [ ] `/v1/app/ice_servers`
- [ ] `/v1/app_version/check`
- [ ] `/v1/billing/plan/list`
- [ ] `/v1/billing/quota`
- [ ] `/v1/billing/usage`
- [ ] `/v1/billing/subscription`
- [ ] `/brand`
- [ ] `/app_version/check`
- [ ] `/v1/location/makeMyselfVisible`
- [ ] `/v1/location/makeMyselfUnvisible`
- [ ] `/v1/location/peopleNearby`

### 遗留路由（非 /v1 前缀，低优先级）

> 这些路由与 /v1/* 有镜像关系，扫完 /v1 后按需补扫

- [ ] `/conversation/*` (legacy)
- [ ] `/friend/*` (legacy)
- [ ] `/group/*` (legacy)
- [ ] `/msg/*` (legacy)
- [ ] `/user/*` (legacy)
- [ ] `/fts/*` (legacy)

---

## 扫描检查清单（每轮执行）

每个 API 须逐项确认：

```
[ ] SQL: 所有 elib_pg:query 调用使用 $N 参数化，无字符串拼接
[ ] JWT: 非 open() 路由经过 auth_middleware / auth_middleware_api_v1
[ ] 输入: Handler 对 body/query/path 参数做了类型/长度/格式校验
[ ] 权限: 资源 owner 校验（如 user_id 来自 JWT 的 current_uid，非请求参数）
[ ] Spec: imboy/api/openapi.yaml 路径/方法/字段/状态码与实现一致
[ ] EUnit: 存在正常路径 + 至少 1 个非法输入的测试用例
```
