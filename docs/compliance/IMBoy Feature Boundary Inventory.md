# IMBoy Feature Boundary Inventory

Version: `imboy-feature-inventory-v1`  
Baseline: Backend `092144a296c21d904ab1165649e1c6fff10f70b4`; Flutter `6b2163faab34e4f70c9910dd65ce3002ea2ee008`; Admin `10dedd0d851e3a147c807455b32fb1502fcd2233` (2026-09-03).

This inventory is the F-00 evidence ledger, not proof of build slicing. `imboy_feature:feature_names/0`, backed by `imboy_plugin_registry`, remains the feature-key source of truth; `imboy_policy_catalog:dependencies/1` remains the dependency source of truth. Unknown or inseparable boundaries stay in Base or are marked Unknown.

## Base boundary

| Key | Scope and owner | Evidence and dependencies | Why it cannot be disabled | Boundary/gap |
|---|---|---|---|---|
| `core` | Account, auth, session, device; Backend Identity, Flutter Identity | `passport_handler`, `auth_middleware`, `auth_logic`, `user_device_handler`, `user_session_*`; Flutter `modules/identity`, login/router roots; tables `user`, `user_device`, `user_session` | Every authenticated API and device session depends on it | Catalog key exists; account subdomains are Shared and not separately sliceable |
| `authorization` | JWT/API/admin RBAC; Backend Security | `auth_middleware`, `adm_auth_middleware`, `adm_acl`, `permission_*`; depends on `core` | Client flags are never authorization; all protected surfaces require it | Base static imports and middleware; no build boundary |
| `health_config_upgrade` | Health, policy/config and upgrade; Backend Ops | `health_handler`, `app_feature_handler`, `app_manifest_handler`, `adm_config_handler`, `r_upgrade`; `imboy_router:init/0` | Required to operate and verify an artifact safely | Shared router/application infrastructure; no build boundary |
| `security_audit_logging` | Audit/logging/rate-limit; Backend Security/Ops, Admin | `operation_log_*`, `security_audit_*`, `rate_limiter`, Admin `/logs`, `/system-health` | Removing it would remove accountability and abuse controls | Shared cross-cutting calls; no build boundary |
| `privacy_notice` | Privacy/terms surfaces; Product/Legal, Flutter/Web | Flutter privacy/terms pages and assets; static web privacy documents | Must remain reachable for any distributed client | Exact document/provider reconciliation remains an Architecture Gap |
| `account_deletion` | Apply/cancel/status/worker; Backend Identity/Privacy, Flutter | `user_deletion_handler`, `user_deletion_logic`, deletion worker/DS; Flutter account-security pages | User lifecycle and privacy obligation must survive optional-feature removal | Existing deletion completeness is P0 gap; keep Base, schema superset |
| `report_block_moderation` | UGC safety foundations; Backend Safety, Flutter, Admin | `report_handler`, `adm_report_handler`, `user_denylist_*`, moderation/review pages; tables `report_ticket`, denylist and audit tables | Required whenever messaging, group, channel, moment or other interaction exists | Target gates vary by feature; shared foundation is not safely separable |
| `messaging_group_workspace` | C2C/C2G, friend, group, workspace/project; Backend Messaging/Collaboration, Flutter, Admin, SDK | `message_handler`, WS dispatch in `imboy_ws`, friend/group/workspace handlers and layers; Flutter messaging/social_graph/group_collab roots; SDK message/friend/group; message/group/workspace/project tables | Product's interaction spine and dependency of optional group/channel surfaces | Large shared graph; intentionally Base, no F-03 rewrite |

## Catalog features

| Feature | Class / owner | Backend (REST, WS, modules, workers, tables) | Flutter / Admin / SDK / assets-permissions-providers | Dependencies, current gate, slicing boundary and Architecture Gap |
|---|---|---|---|---|
| `e2ee` | Optional / Security | E2EE/Olm/device handlers, logic/DS/repo and cleanup workers; key/envelope tables; C2C/C2G WS envelope path | Flutter `service/e2ee`, security pages; Admin compliance-key/settings; SDK e2ee; secure storage/crypto providers | Catalog dependency none; `imboy_policy` capability + route/handler checks. No compile boundary; security model must not be rewritten |
| `channel` | Optional / Channel | `channel_handler`, report target, channel logic/DS/repo; channel tables | Flutter channel tab/pages/assets; Admin `/channels`; SDK channel | Plugin manifest runtime gate. Build boundary absent; shares account, messaging, report and object storage |
| `location` | Optional / Growth | `location_handler` and location layers; location data | Flutter nearby page; location permission and AMap provider; no Admin entry; SDK status Unknown | Plugin runtime gate. Provider/permission slicing unproved; direct imports remain |
| `moment` | Optional / Social | `moment_handler`, report target and moment layers; moment/comment/like tables | Flutter moment tab/pages/media; Admin `/moments` and report target; SDK status Unknown | Plugin runtime gate. UGC safety/Base dependency implicit; no build boundary |
| `channel_discover` | Optional / Channel | `channel_handler:discover`; channel query layers | Flutter discover page; Admin shares channel module; SDK channel | Depends on `channel` in `imboy_policy_catalog`; action runtime-gated. No separate import/chunk boundary |
| `channel_invitation` | Optional / Channel | channel invitation actions and tables | Flutter invitation flows; Admin channel invitation route; SDK channel | Depends on `channel`; runtime action/route gate only; shared handler prevents physical separation today |
| `channel_order` | Optional / Commerce | channel order actions plus order/payment layers and tables | Flutter paid-channel/order UI; Admin orders/finance overlap; SDK order/wallet; payment providers | Depends on `channel`; runtime action gate only. Payment/provider boundary is Shared/Unknown and must remain off until separately accepted |
| `group_vote` | Optional / Collaboration | `group_vote_handler`, group sub-handler and layers; vote tables | Flutter group vote page; Admin group vote route; SDK Unknown | Plugin runtime handler gate; dependency on Base group is architectural but not catalog-declared. No compile boundary |
| `group_schedule` | Optional / Collaboration | `group_schedule_handler`, layers/workers; schedule tables | Flutter group schedule page/calendar assets; Admin schedule route; SDK Unknown | Plugin runtime handler gate; Base group dependency implicit. No compile boundary |
| `group_task` | Optional / Collaboration | `group_task_handler`, review/actions and layers; task tables | Flutter group task page; Admin task/review routes; SDK Unknown | Plugin runtime handler gate; Base group dependency implicit. No compile boundary |

## Published/shared features outside the catalog

| Area | Classification / owner | Evidence | Decision |
|---|---|---|---|
| Wallet, recharge, withdrawal, billing | Shared/Unknown / Commerce | Backend wallet/billing/payment handlers; Flutter wallet pages; Admin `modules/finance`; SDK wallet/order | Not a catalog key, overlaps `channel_order`; Architecture Gap, stays in Base graph until F-00 follow-up proves a safe owner/boundary |
| Live room / RTC/call | Unknown / Realtime | Flutter `FeatureKeys.liveRoom` locally disabled; call/LiveKit/TURN code and permissions | Not in backend catalog; cannot be selected by canonical manifest; fail closed as unknown |
| Bot, AI agent, MCP/plugin management | Shared/Unknown / Platform | bot/agent/MCP handlers, plugin loader, Admin plugin/MCP pages | Backend plugin infrastructure is not the product-feature catalog; no feature key or safe slicing boundary |
| QR, media/attachments, push/notifications | Shared / Platform | QR handlers/pages, Garage asset service, FCM/APNs and notification workers | Cross-feature infrastructure; keep with Base/schema superset. Provider and permission slicing deferred to F-06 |
| Admin pricing/license/SSO/system settings | Shared / Ops | Admin routes and corresponding backend handlers | Operational modules, not catalog product features; authorization/audit stay Base |

## Chain evidence and closure

The current chain is runtime-only: `imboy_plugin_registry:raw_manifests/0` -> `imboy_feature:feature_names/0` and `imboy_policy_catalog:dependencies/1` -> `imboy_policy:effective_features/0` -> router/handler guards -> `/api/v1/app/manifest` -> Flutter `AppFeatureRegistry`/`RouteFeatureGuard` and Admin `FeatureRoute`/sidebar. Static imports, router construction, OTP workers, dependencies, assets and permissions are not removed.

F-01/F-02 add a build-time ceiling contract. The generator rejects unknown/duplicate keys, missing/cyclic dependencies, Base disable attempts and schema drift. It emits contracts only; F-03/F-04/F-05/F-06 must later connect those contracts to Backend registration, Flutter import roots and Admin Vite imports, then prove artifact absence. Database migrations remain a compatible schema superset and disabling a feature never deletes data or safety/retention jobs.
