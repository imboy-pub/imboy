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
| C3 | Grace period + expected completion date displayed from server | page fetches `deletion_status` and renders banner | ⬜ pending |
| C4 | Cancel flow in app during grace period | cancel button → request cancelled → account usable | ⬜ pending wiring check |
| C5 | Retained categories note shown before submit | page copy | ⬜ pending |
| C6 | Completion/failure status without exposing PII | status banner (pending) / account unusable after completion | ⬜ pending |
| C7 | Android/iOS real-device flow recorded (offline/retry, reauthentication) | device run on MRD-AL00 + second device | ⬜ adb touch-injection dead on EMUI 9 (tap/swipe/motionevent all swallowed, onboarding page never advances) — do the 2-minute manual walk (below), or use a device with working injection |

## Manual device walk (2 minutes, MRD-AL00)

1. Install latest debug APK (APP_ENV=local_home + 9804 override), open IMBoy.
2. Skip onboarding → sign in (smoke_alice / admin888).
3. 我的 → 设置 → 注销账号：勾选已阅读 → 点击注销 → 确认弹窗。
4. Expected: banner shows 注销申请已提交 + 预期完成时间 + 数据留存说明 + 撤销入口 (C3/C5/C6).
5. 撤销注销申请 → status returns to normal, account usable (C4).
6. Re-apply if desired for the store recording. Never touch the store console.

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
