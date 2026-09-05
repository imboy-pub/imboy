# IMBoy D-04 Store Evidence Checklist (Account Deletion)

Task: Implementation Plan D-04 — App/Web Deletion Acceptance.
Purpose: evidence list for Apple App Store / Google Play account-deletion review,
plus the internal sign-off state. Console mutation is out of scope unless the
owner explicitly authorizes it.

## Server-side (ready)

| # | Evidence | How to verify | State |
|---|---|---|---|
| S1 | Authenticated status endpoint | `GET /api/v1/user/deletion_status` returns status / requested_at / expected_deletion_at / grace_days / retained_categories | ✅ (D-01) |
| S2 | Cancel during grace period | `POST /api/v1/user/cancel_logout` → request `cancelled`, account usable again | ✅ (D-01) |
| S3 | Idempotent request | duplicate apply keeps first requested_at (grace clock not reset) | ✅ (D-01 tests) |
| S4 | Balance gate | job stays pending while `wallet.balance ≠ 0`; completes after zero | ✅ (D-03 tests) |
| S5 | Ownership transfer-or-close | owned group/workspace/channel transferred to successor, else deleted/archived | ✅ (D-03 tests) |
| S6 | Public web page | `https://<host>/account-deletion` reachable without login, whitelisted in router | ✅ |
| S7 | Automated deletion after grace | sweeper claims expired request → executor → user row deleted; tombstone job `completed` | ✅ (D-03 tests; production run needs `user_deletion_enabled=true`) |
| S8 | Retained categories published | web page "What happens to your data" + `retained_categories` in status endpoint | ✅ |

## Client (app)

| # | Evidence | How to verify | State |
|---|---|---|---|
| C1 | Deletion entry point discoverable in app | Mine → Account security → Delete account | ✅ existing page |
| C2 | Confirmation + read-and-agree checkbox before submit | logout_account_page | ✅ |
| C3 | Grace period + expected completion date displayed from server | page fetches `deletion_status` and renders banner | ✅ macOS e2e (integration_test/d04, banner text asserted against `requested` payload) |
| C4 | Cancel flow in app during grace period | cancel button → request cancelled → account usable | ✅ macOS e2e (`cancel_logout` 200 → status flips out of `requested`; DB: request=`cancelled`, user.status back to 1) |
| C5 | Retained categories note shown before submit | page copy | ✅ macOS e2e (`审计日志` retained-note asserted on page) |
| C6 | Completion/failure status without exposing PII | status banner (pending) / account unusable after completion | ✅ banner path verified; completed path covered by D-03 sweeper tests |
| C7 | Android/iOS real-device flow recorded (offline/retry, reauthentication) | device run on MRD-AL00 + second device | ⬜ adb touch-injection dies after screen-off cycles on EMUI 9 — do the 2-minute manual walk (below), or use a device with working injection |

## Automated e2e (2026-09-05, macOS + real backend — ALL GREEN)

`integration_test/d04_account_deletion_flow_test.dart` on macOS against the
hot-patched 9801 node (main code): **All tests passed**. Proven end-to-end:

- login → deletion page renders retained-categories note (C5) and grace
  banner from `requested` payload (C3)
- apply → 200, `user.status=2`, idempotent re-apply keeps first
  `requested_at` (S3)
- product cascade after apply: local logout (token/E2EE/SQLite purge) +
  `/welcome`
- **grace-period re-login allowed** (status=2 passes the sign-in gate) and
  re-issued token works
- cancel → request `cancelled`, `user.status` restored to 1 (S2/C4)

Backend bring-up notes learned here (for any fresh node):
`config` table needs `pub.imboy.app_<os>_<sk>` rows (sk header is constant
"1"); write them via `config_ds:set` (values are AES-encrypted at rest —
plain-SQL inserts read back empty on new code); register TSID generators
`user_deletion_request` / `user_deletion_job`; hot-swapped router needs
`cowboy:set_env(imboy_listener, dispatch, ...)`.

## Manual device walk (2 minutes, MRD-AL00 — CURRENT STATE 2026-09-05)

最新 APK（APP_ENV=local + localhost:9804 reverse + D-04 状态区块）已装上
MRD-AL00，应用当前停在登录页且表单已预填（smoke_alice）。EMUI 触控注入
在息屏循环后失效，请直接用手指：

1. 点亮屏幕（电源键）→ 登录页表单已预填 → 点「登录」。
2. 主页 → 我的 → 设置 → 注销账号：看「数据留存说明」区块（C5）。
3. 勾选已阅读 → 点注销 → 确认弹窗：应出现状态横幅「注销申请已提交，
   预计 xx 完成」+ 撤销入口（C3/C6）。
4. 点「撤销注销申请」→ 横幅消失、账号可用（C4）。录屏即商店证据。
5. 生产环境前置：user_deletion_enabled=true + 迁移 ≥ 00000086。

设备 reverse 已改为 `tcp:9804 → 宿主 9801`（9801 已热更为 main 代码，
含 D-01 全部接口；9804 节点是 D-01 之前的旧构建，勿再用于验收）。

Never touch the store console. 自动化路径：integration_test/
d04_account_deletion_flow_test.dart（macOS 全绿，运行命令见文件头；
模拟器 d04 路线备用，AVD 需重建）。

## Store console

| # | Evidence | State |
|---|---|---|
| A1 | App Store: account-deletion URL or in-app flow declared in App Review notes | ⬜ release-time |
| A2 | Google Play Data safety form: account deletion URL = `https://<host>/account-deletion` | ⬜ release-time |
| A3 | No console mutation performed without explicit owner authorization | ✅ none performed |

## Ops prerequisites for S7 in production

- `user_deletion_enabled = true` (sweeper disabled by default)
- `user_deletion_retention_days` = product-approved grace period (default 60)
- migrations ≥ 00000086 applied

## Product decisions pending owner confirmation (D-02)

1. trust_audit audit logs: retained
2. messages: server-side ciphertext deleted (E2EE copies on other devices out of scope)
3. owned group/workspace/channel: transfer to successor, else delete/close
4. wallet: balance must be zero before completion; financial records retained, anonymised
