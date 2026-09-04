# IMBoy Feature Composition F07 Execution Record

Date: 2026-09-03  
Scope: F-07 cross-repository artifact consistency and reproducible build matrix. No commit, push, deploy, publication or external action.

## Result

F-07 local artifact gate: **PASS**.

`run_product_feature_matrix.sh` builds Backend release, Flutter Android arm64 release APK and Admin production assets from one validated manifest, then restores the canonical `full-selected` outputs on exit. `verify_product_feature_artifacts.py` rejects stale generated files and verifies that all three runtime artifacts embed the same manifest hash and compiled-feature set. It also records repository HEAD, dirty state, worktree hash and artifact hashes in JSON evidence.

Flutter debug APKs store Dart code in `kernel_blob.bin`; AOT APKs store it in `libapp.so`. The verifier accepts either runtime representation and still applies the same marker checks.

## Evidence

| Check | Result |
|---|---|
| Generator and artifact verifier unit tests | PASS, 23/23 |
| Backend app manifest tests | PASS, 3/3 |
| Cross-client manifest compatibility | PASS; both app and Admin require hash, schema and `compiled_features`; missing, malformed or server-superset contracts fail closed |
| Base-only matrix | PASS; only `core`, manifest `sha256:e943ee9541241206e6f0b91502ddb173f06c086e83195c7a7cc95de2f4623712` |
| Full-selected matrix | PASS; 11 compiled features, manifest `sha256:b59aae27976015823e315e9a5ee42bb2af3970cec92c97df04b517b4674d1457` |
| Base evidence | `docs/compliance/feature-composition-evidence/base-only.json` |
| Full evidence | `docs/compliance/feature-composition-evidence/full-selected.json` |
| Canonical restoration | PASS; profile `full-selected` and canonical hash restored |

## Boundary

This gate proves local generated-source and built-artifact consistency. It does not prove real-device behavior, authenticated browser/RBAC behavior, payment-provider behavior, production deployment or release acceptance. F-05 and F-06 retain their documented PARTIAL boundaries, including shared Flutter plugin binaries and untouched protected iOS files.

The runner recognizes `overseas_baseline`, but fails closed until that manifest fixture exists; no synthetic profile is treated as evidence.

## Two-round self-test

Both independent static/test rounds passed the same local gate: generator/verifier Python tests; focused Backend EUnit tests plus compile; 30 focused Flutter tests plus scoped analysis; and focused Admin tests plus typecheck and ESLint.

After upgrading the matrix from debug to Android arm64 release artifacts, two further independent, fail-fast rounds each passed Base-only followed by Full-selected. Every cell assembled the Backend release/tarball, refreshed Flutter plugin metadata, built the release APK, built Admin production assets and ran cross-artifact verification. A stale ignored `GeneratedPluginRegistrant.java` initially made repeated profile switching fail; each matrix cell now removes only that Flutter-generated ignored file before dependency resolution. No failed exploratory run is counted as PASS.

Evidence worktree hashes exclude the evidence output directory itself, avoiding a self-referential hash that becomes stale when its JSON is overwritten. Source/generated contract changes remain included.

Whole-repository `flutter analyze` remains **BLOCKED outside this change scope**: archived text logs under `test/auto_test/reports/full_run_2026-09-01/logs/` use a `.dart` suffix and are parsed as invalid source, while an existing `channel_public_test.dart` expects symbols intentionally no longer exported by the canonical public barrel. Those user-owned files were not changed. This does not replace the passing scoped analysis, and no whole-repository green claim is made.
