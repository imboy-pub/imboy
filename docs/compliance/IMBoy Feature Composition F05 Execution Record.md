# IMBoy Feature Composition F05 Execution Record

Date: 2026-09-03  
Scope: F-05 Admin module, route and menu slicing. No commit, push, deploy, publication or external action.

## Result

F-05 status: **PARTIAL**.

The generated composition now covers `moment`, `channel`, `channel_invitation`, `channel_order`, `group_vote`, `group_schedule`, `group_task` and `e2ee`. `App.tsx` consumes generated optional routes instead of declaring their lazy imports. `ReportCenterPage` uses the generated Moment panel factory, and Header channel search uses a generated factory, removing Base imports of optional Moment and Channel management surfaces.

`isAdminFeatureEnabled` now fails closed when a server flag names a feature outside `compiledProductFeatures`, including menu filtering when runtime flags are empty. Existing server feature flags, admin entries and RBAC checks remain runtime enforcement; Admin configuration cannot raise the compiled capability ceiling. The Backend feature endpoint exposes its compiled manifest hash/schema/features, and Admin rejects missing, malformed or mismatched contract fields instead of silently using cached flags.

## Evidence

| Check | Result |
|---|---|
| Generator unit tests | PASS, 16/16 |
| Canonical Admin typecheck | PASS |
| Canonical selected Vite build | PASS; emitted `moments-0rdDHlj7.js` |
| Base-only Admin typecheck | PASS |
| Base-only Vite build | PASS; 2,900 modules transformed |
| Base-only Vite manifest/chunk audit | PASS for Moment; no `moments-*.js`, `modules/moments`, `pages/moments` or Moment page symbols in `dist` |
| Channel-only build | PASS; Channel base pages emitted while invitation/order, Moment, group optional and E2EE page roots were absent |
| Base-only full optional-root audit | PASS; all generated optional Admin page roots absent, 2,879 modules transformed, main entry 200.70 kB |
| Backend manifest metadata tests | PASS, 4/4 focused EUnit tests |
| Admin routing/menu/feature/report tests | PASS, 110/110 |
| Manifest compatibility tests | PASS; only a complete matching payload is accepted; missing, malformed, hash/schema and feature-superset payloads throw visibly |
| Canonical output restoration | PASS; `sha256:b59aae27976015823e315e9a5ee42bb2af3970cec92c97df04b517b4674d1457` |

## Remaining F-05 work

The local static/module-graph boundary is complete. A logged-in Backend-integrated browser run is still required to prove selected and disabled menu/direct-URL/API behavior under real RBAC responses. A full release artifact and cross-artifact consistency remain F-07 work.
