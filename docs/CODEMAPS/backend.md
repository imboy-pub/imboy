<!-- Generated: 2026-04-17 | Routes: 498 | Handlers: 69 | Token estimate: ~900 -->

# Backend Codemap / 后端图谱

> 双语 / Bilingual.

## Route Segments / 路由分段 (`src/imboy_router.erl`)

| Segment | Auth | Examples |
|---|---|---|
| **open** | none | `/`, `/init`, `/ws`, `/refreshtoken`, `/passport/*`, `/app_version/check`, `/metrics`, `/health` |
| **api_v1** | `auth_middleware_api_v1` (HMAC + ts) | `/v1/e2ee/*`, `/v1/*` 安全敏感端点 |
| **auth (legacy)** | `auth_middleware` (JWT) | `/conversation/*`, `/msg/*`, `/user/*`, `/friend*`, `/group*`, `/moment/*`, `/wallet/*`, ... |
| **adm** | `adm_auth_middleware` | `/adm/*` 管理后台 |

中间件链 / chain: `cors_middleware` → `security_headers_middleware` → `throttle_middleware` → `auth_middleware*`.

## Handler → Logic → DS → Repo Map

| Domain | Handler | Logic | DS | Repo |
|---|---|---|---|---|
| User | `user_handler` | `user_logic` | `user_ds` | `user_repo` |
| Auth/Token | `passport_handler`, `auth_handler` | `passport_logic`, `auth_logic` | `auth_ds`, `token_ds` | `token_repo`, `user_repo` |
| Friend | `friend_handler`, `friend_category_handler` | `friend_logic` | `friend_ds`, `friend_category_ds` | `friend_repo` |
| Group | `group_handler` + 9 sub-handlers | `group_logic` + members/notice/file/album/task/vote/schedule | `group_ds` (+ 13 group_*_ds) | `group_repo` (+ subs) |
| Msg C2C | `msg_handler` | `msg_c2c_logic`, `messaging_logic` | `msg_c2c_ds`, `msg_store_ds` | `msg_c2c_repo`, `msg_archive_repo` |
| Msg C2G | (router) | `msg_c2g_logic` | `msg_c2g_ds` | `msg_c2g_repo` |
| Msg S2C | (worker) | `msg_s2c_logic` | `msg_s2c_ds` | `msg_s2c_repo` |
| WebSocket | `websocket_handler` | `websocket_logic` | `websocket_ds` | — |
| E2EE | `e2ee_handler`, `e2ee_social_handler`, `e2ee_transfer_handler` | `e2ee_logic` | `e2ee_social_ds`, `e2ee_transfer_ds`, `e2ee_local_backup_ds`, `compliance_key_ds` | `user_device_repo` (public_key) |
| Channel | `channel_handler` | `channel_logic` | `channel_ds` (+ subscribe/admin/order/message) | `channel_repo` (+ subs) |
| Moment | `moment_handler` | `moment_logic` | `moment_ds` | `moment_repo` |
| Conversation | `conversation_handler` | — | `conversation_pin_ds`, `conversation_mute_ds`, `conversation_delete_ds` | `conversation_repo` |
| Live Room | `live_room_handler` | `live_room_logic` | `live_room_ds` | `live_room_repo` |
| Wallet | `wallet_handler` | `wallet_logic` | `wallet_ds` | `wallet_repo` |
| Push | (worker) | `push_notification_logic` | `push_notification_ds`, `push_token_ds` | `push_token_repo` |
| App | `app_version_handler`, `app_upgrade_log_handler`, `app_feature_handler` | — | `app_version_ds`, `app_version_policy_ds`, `app_upgrade_log_ds` | `app_version_repo` |
| Admin | `adm_*_handler` ×17 | (admin uses ds 直连) | `adm_user_ds`, etc. | adm 复用业务 repo |

## Module Counts / 模块数

| Layer | Count | Path |
|---|---:|---|
| API Handler | 50 | `src/api/` |
| Admin Handler | 19 | `src/adm/` |
| Logic | 70 | `src/logic/` |
| DS | 77 | `src/ds/` |
| Repo | 71 | `src/repo/` |
| Lib | 43 | `src/lib/` |
| **Total .erl** | **330** | — |

## Conventions / 约定

- DB access **MUST** go through `elib_pg`; bypass forbidden.
- Strings with CJK use `<<"…"/utf8>>`.
- Errors via `?ERR_*` macros (`include/error_code.hrl`); response via `elib_response:error/3`.
- IDs: TSID via `elib_tsid:generate(Table)`; JSON integer transport.
- New module scaffolding: `make new t=imboy.{rest_handler|logic|ds|repository} n=foo_handler`.

## Tests / 测试

`test/{api,adm,logic,ds,repo,lib,common}` 目录与 `src/` 1:1；EUnit + meck；`make eunit`。
