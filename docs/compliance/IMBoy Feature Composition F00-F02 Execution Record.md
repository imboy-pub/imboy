# IMBoy Feature Composition F00-F02 Execution Record

Date: 2026-09-03  
Scope: F-00, F-01 and F-02 only. No commit, push, deploy, external notification or production action.

## Preflight

| Repository | Base SHA | Dirty files before work |
|---|---|---|
| Backend/product | `092144a296c21d904ab1165649e1c6fff10f70b4` | modified compliance Gap Matrix and Implementation Plan; untracked Feature Composition Execution Prompt, `elib_pg.24588.coverdata`, `test/ct.cover.spec` |
| Flutter | `6b2163faab34e4f70c9910dd65ce3002ea2ee008` | untracked `coverage/` |
| Admin | `10dedd0d851e3a147c807455b32fb1502fcd2233` | clean |

All pre-existing dirty files were protected. During execution an independent `make eunit-local COVER=1` process (PID 27967) modified `Makefile` and emitted additional `*.27967.coverdata`; those concurrent changes are preserved and are not F-00/F-01/F-02 evidence. Task-owned files are the inventory/record, `config/product-feature-manifest.json`, `scripts/generate_product_features.py`, `test/fixtures/product_features/`, `test/scripts/test_generate_product_features.py`, and the three generated contracts.

## F-00 Feature Boundary Inventory

- Owner: Product Architecture. Dependencies: repository instructions, compliance plan/gap matrix, Backend policy/plugin catalog, Flutter registry/manifest/route guard, Admin route/sidebar/feature hooks.
- Exclusive files: `docs/compliance/IMBoy Feature Boundary Inventory.md` and this record.
- Stop condition: catalog, Base, advertised/shared/Unknown surfaces, owners, dependencies, evidence and Architecture Gaps recorded without claiming slicing.
- Result: PASS for the inventory baseline. Evidence: `IMBoy Feature Boundary Inventory.md`.
- Residual risk: broad shared areas are evidence-led classifications, not artifact reachability proof; Wallet/RTC/AI/bot/provider boundaries remain Unknown/Shared for later inventory refinement.

## F-01 Canonical Product Feature Manifest

- Owner: Product Architecture. Dependency: accepted F-00 Base reference and existing Backend feature catalog.
- Exclusive files: `config/product-feature-manifest.json`, `test/fixtures/product_features/*.json`.
- Stop condition: versioned minimal manifest plus Base-only, selected and invalid inputs; no duplicate dependency truth source.
- Result: PASS. Canonical manifest is the full-selected fixture; Base-only and five invalid file fixtures cover unknown, duplicate, missing dependency, Base-disable and schema-version failures.
- Hash rule: SHA-256 of compact JSON with lexicographically sorted keys and sorted `selected_features`, over exactly `base_ref`, `product_id`, `profile`, `schema_version`, and `selected_features`; emitted as `sha256:<lowercase hex>`.
- Residual risk: schema version 1 intentionally has one product id and one Base reference. A new product/schema requires an explicit versioned change.

## F-02 Generator and Dependency Validation

- Owner: Build Systems. Dependency: F-01 plus compiled Backend `imboy_feature` and `imboy_policy_catalog` beams.
- Exclusive files: generator, Python test and generated Backend/Flutter/Admin contracts.
- Stop condition: runtime catalog reuse, deterministic output, `--check`, all requested negative checks, and equal schema/hash/list across outputs.
- Result: PASS for static contract generation. The generator queries `imboy_feature:feature_names/0` and `imboy_policy_catalog:dependencies/1`; it does not own a second feature/dependency catalog.
- Generated evidence: `include/generated/imboy_product_features.hrl`, Flutter `lib/app_core/feature_flags/generated_product_features.dart`, Admin `src/generated/productFeatures.ts`; all contain schema `1`, hash `sha256:b59aae27976015823e315e9a5ee42bb2af3970cec92c97df04b517b4674d1457`, and the same sorted compiled list.
- Residual risk: these contracts are deliberately not wired into Backend registration, Flutter imports or Admin Vite imports. That work and artifact absence proof belong to F-03 through F-07.

## Commands and results

| Command | Exit | Result |
|---|---:|---|
| `python3 -m unittest test/scripts/test_generate_product_features.py` | 0 | 7 tests passed; Base-only/selected, unknown, duplicate, missing dependency, Base disable, schema, cycle, duplicate JSON field, determinism, three-contract consistency and stale check |
| `make compile` | 0 | Backend compiled; GNU Make version warning only |
| `python3 scripts/generate_product_features.py` | 0 | three contracts generated from live Backend catalog |
| `python3 scripts/generate_product_features.py --check` | 0 | dependency/Base/schema/hash validation and stale-output check passed |
| invalid fixture loop | 0 | all five invalid files returned non-zero with the expected fail-closed reason |
| repeat generation plus `sha256sum --check` | 0 | all three generated files were byte-identical |
| Flutter `dart analyze` and Admin `bun run typecheck` | 0 | generated Dart and TypeScript contracts passed static analysis |
| `git diff --check` | 0 | no whitespace errors |

Acceptance result: F-00 PASS, F-01 PASS, F-02 PASS. Stop here. F-03 Backend slicing, F-04 Flutter compile-graph slicing, F-05 Admin module/chunk slicing, F-06 SDK/assets/permissions slicing and F-07 artifact consistency are not implemented or claimed.
