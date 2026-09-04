# IMBoy Feature Composition F03 Execution Record

Date: 2026-09-03  
Scope: F-03 Backend build, route and release slicing only. No release boot, database migration, commit, push, deploy, external notification or production action.

## Preflight and ownership

| Item | Evidence |
|---|---|
| Backend baseline before feature-composition work | `092144a296c21d904ab1165649e1c6fff10f70b4` |
| Backend HEAD at final evidence capture | `5968eb7b36c5362e1a74ac31c478cc49658f29f5` |
| F-03 implementation files | `src/lib/imboy_feature.erl`, `src/lib/imboy_policy.erl`, `src/api/feature_gate_middleware.erl`, `src/imboy_app.erl`, `src/imboy_plugin_sup.erl`, `src/imboy_router.erl` |
| F-03 test files | `test/lib/imboy_feature_compiled_tests.erl`, `test/api/feature_gate_middleware_tests.erl`, `test/api/feature_route_http_tests.erl`; existing policy, feature and plugin supervisor suites |

The worktree remained shared and dirty throughout execution. Unrelated compliance, message delivery, sync, coverage and concurrent-session changes were preserved. The generator bootstrap repair is owned by the existing F-02 generator and tests; it only allows a clean build to parse the existing Erlang catalog sources when Beam files do not yet exist, and does not introduce another feature or dependency catalog.

## Implemented boundary

- `imboy_feature` reads the generated compiled feature set, filters API/Admin/plugin routes, and annotates optional routes with `required_feature`.
- `imboy_policy` computes runtime effective features as `compiled intersect runtime_enabled`; runtime configuration can further disable a compiled feature but cannot enable an omitted feature.
- `feature_gate_middleware` runs after authentication and rejects a route whose annotated feature is not effectively enabled.
- `imboy_plugin_sup` always starts the router and WebSocket registries, and starts each empty optional plugin supervisor only when one of its declared feature keys is compiled.
- `imboy_router` applies the same compile-time route filter to API, Admin and dynamically registered plugin routes.
- Existing health, authentication, report, block and moderation routes remain in Base. Channel, Moment, Location, E2EE and group-collaboration routes are absent from the Base-only dispatch.

## Evidence

| Check | Result |
|---|---|
| Full-selected `make compile` | PASS |
| Base-only `make compile` | PASS |
| Focused EUnit suites | PASS, 88/88 tests |
| `python3 -m unittest test/scripts/test_generate_product_features.py` | PASS, 8/8 tests |
| Base-only dispatch | 472 routes; optional route groups absent |
| Full-selected dispatch | 630 routes; optional routes carry the expected `required_feature` |
| Base-only plugin supervisor | 2 children: router registry and WebSocket action registry |
| Full-selected plugin supervisor | 6 children: 2 registries plus 4 optional plugin supervisors |
| Base-only policy | Channel and E2EE effective values are false |
| Base-only `make rel` | PASS; release and tarball assembled |
| Full-selected `make rel` | PASS; `_rel/imboy/imboy-1.0.0-alpha.71.tar.gz` created |
| Temporary Cowboy HTTP contract | PASS; manifest returned HTTP 200 with the compiled contract, while Base-only `/api/v1/channels/discover` returned HTTP 404 |
| Admin feature contract | PASS; authenticated config payload includes manifest hash/schema and `compiled_features` for the Admin superset guard |
| Canonical restore check | PASS; `sha256:b59aae27976015823e315e9a5ee42bb2af3970cec92c97df04b517b4674d1457` |
| `git diff --check` | PASS |

The full-selected release build emitted warnings that seven `erlang_migrate` object files were out of date. The command still exited 0 and assembled the release and tarball. These warnings are recorded rather than treated as F-03 release-boot evidence.

## Acceptance and limits

F-03 result: **PARTIAL**.

Route registration, runtime gating, optional supervisor selection, clean-build generation and both Base-only/full-selected release assembly are verified. A release was deliberately not started because `imboy_app:start/2` performs database migration; ordinary continuation does not authorize that state-changing action.

Base-only is a registration and runtime slice, not a physical Beam-package slice. Optional business modules still belong to the single `imboy` OTP application and remain in the Base-only release archive. Removing those Beam files would require a broader application/package boundary redesign across Message, Group, Channel and E2EE and is outside F-03.

The WebSocket registry currently contains Base C2C/C2G actions only, so there are no independently optional WebSocket actions to slice. The four optional plugin supervisors currently have no worker children.

F-04 through F-07 are assessed in their own execution records; their device, authenticated RBAC and physical dependency-removal limits do not change this F-03 result.
