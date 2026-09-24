# IMBoy Enterprise Organization / Admin / Internal V1 Current-Main Closure Plan V1

> Date: 2026-09-24
> Nature: current-main-derived, isolated-worktree requalification and residual-gap closure contract
> Scope: run-scoped local worktrees and physical-device acceptance only
> Previous plan: `docs/plans/2026-09-23-enterprise-organization-admin-internal-v1-unified-plan-v2.1.md`
> Parallel run: `docs/roadmap/2026-09-24-livekit-single-service-closure-plan-v2.md`
> Integrity source: the adjacent `.sha256` sidecar; do not hard-code a self-referential hash in this file

## 0. Decision

The V2.1 implementation is already contained in the three local `main` branches. There is no V2.1 worktree left to merge. This closure derives new candidate branches from current `main`, but all writes, integration, tests, and final freeze occur in run-scoped worktrees while the LiveKit closure runs in parallel.

This plan does only four things:

1. Requalify a frozen snapshot of the current three-repository `main` state after later LiveKit integration.
2. Remove the three known migration-cycle baseline failures so the full local gate is green.
3. Complete Android and iOS physical-device organization journeys.
4. Rebuild SHA-bound manifests, ledgers, and the final report.

It does not write the shared `main` worktrees, merge its candidates back to `main`, add endpoints, scopes, schema, product capability, deployment work, or production adoption.

Current disposition at authoring time:

| Dimension | Status |
|---|---|
| V2.1 implementation | `INTEGRATED_TO_MAIN` |
| Historical frozen candidate | `LOCAL_CANDIDATE_PASS` at `64dd8060 / 365ed696 / eacc5301` |
| Current main qualification | `BLOCKED_SHA_DRIFT` |
| Full backend suite | `FAIL_BASELINE_DEBT` with 3 migration-cycle failures |
| Android device | `PARTIAL` |
| iOS device | `BLOCKED_NO_DEVICE` |
| External / Production | `NOT_EXECUTED` |
| Release | `RELEASE_NO_GO` |

Target terminal state:

```text
LOCAL_WORKTREE_CANDIDATE_PASS
+ FULL_SUITE_PASS
+ DEVICE_PASS_ANDROID
+ DEVICE_PASS_IOS
+ EXTERNAL_NOT_EXECUTED
+ PRODUCTION_NOT_EXECUTED
+ RELEASE_NO_GO
```

## 1. Verified Authoring Baseline

The coordinator must re-sample all facts in W0. These values are evidence for plan creation, not permission to skip W0.

| Repo | Branch | HEAD | State |
|---|---|---|---|
| `imboy` | `main` | `74b024c8b89f5979eb29c215c8855d00e22318af` | clean; latest commit tracks the independent LiveKit closure plan |
| `imboyadmin` | `main` | `365ed696943bbac4535e6e996cb4017dfcf0ccbb` | clean |
| `imboyapp` | `main` | `e38ac726b7a0b922d4671b9b9c92985077357efb` | clean |

Verified facts:

- Historical V2.1 candidates are ancestors of their corresponding `main` branches.
- No `codex/entv21-*` branch or V2.1 worktree remains registered.
- All three repositories currently register only their `main` worktree; persisted LiveKit run state and processes may still exist and remain foreign to this plan.
- LiveKit changed shared files including `src/imboy_router.erl` and Flutter `lib/store/api/user_api.dart`; old evidence cannot be rebound without rerunning its dependency closure.
- Current Internal bundle check is `25 paths / 31 operations / 0 $ref`; Postman has 31 requests.
- The historical ledger contains 21 PASS, 2 device-only NOT_RUN, 532 passing test counts, and 136 Oracle counts.
- The current verifier correctly returns `LOCAL_CANDIDATE_BLOCKED` because Backend and Flutter HEADs moved.

## 2. Scope And Non-Scope

### 2.1 Required Closure

- Capture the current three-repository SHA set and immediately create isolated integration worktrees from those exact commits.
- Prove all historical V2.1 commits are contained in current `main`.
- Re-run the 31 endpoint / 14 scope / 4 Human Directory API contract closure.
- Re-run authentication, Grant, boundary, cursor, idempotency, audit, schema-parity, and IDOR gates.
- Re-run Admin unit/type/build and nine-leaf real-backend browser acceptance.
- Re-run Flutter Organization analyze, contract, controller, widget, and persistence tests.
- Fix the three migration-cycle failures at their root cause and obtain `FULL_SUITE_PASS`.
- Complete the committed organization journey on Android and iOS physical devices.
- Generate fresh manifests and ledgers bound to the final isolated-worktree candidate SHAs.

### 2.2 Explicit Non-Scope

- No new Internal endpoint, scope, mutation surface, Human API, Admin feature, or Flutter feature.
- No speculative index, analytics domain, generic workflow engine, or dependency addition.
- No resurrection, adoption, or merge of residual LiveKit branches/run state merely because their commits appear related.
- No write, integration, commit, or test-artifact generation in the three shared `main` worktrees.
- No merge of this plan's final candidate branches back to `main`; that requires a later merge-readiness audit after the LiveKit run hands off.
- No deletion or cleanup of foreign branches, worktrees, processes, leases, databases, devices, or WIP.
- No push, PR, remote branch write, deployment, production migration, production credential, customer data, notification, publication, or release.
- No use of simulator evidence in place of Android or iOS physical-device acceptance.

Any required product or schema expansion becomes a proposal and ends the affected card as `BLOCKED_SCOPE_EXPANSION`.

## 3. Safety And Ownership

### 3.1 Coordinator

`A0` is the only coordinator and local integrator. Maximum active agents is 6 including A0. Completed seats may be reused.

A0 owns:

- W0 sampling, run control, leases, manifests, ledgers, worktree integration, final state calculation.
- Creation of run-scoped validation/fix worktrees.
- Exact-path integration into run-scoped candidate branches only after gates pass.
- Protection of all pre-existing staged, unstaged, untracked, ignored, and foreign-run material.

A0 must not:

- Reset, clean, stash, overwrite, blanket-stage, or force-remove anything.
- Treat process age, merged ancestry, or stale-looking leases as a handoff.
- Rewrite old V2.1 evidence to point at new SHAs.

### 3.2 Workers

| Worker | Ownership | Deliverable |
|---|---|---|
| A1 Backend | migration test harness and the minimum root-cause fix for the 3 known failures | focused commits, full backend gate evidence |
| A2 Contract/Security | read-only 31/14/4 extraction plus auth/boundary/cursor/idempotency/schema/security verification | evidence only; proposals on failure |
| A3 Admin | existing enterprise Admin tests and real-backend nine-leaf journey | evidence; code only for candidate-induced defects |
| A4 Flutter | existing Organization tests and Android/iOS physical-device journeys | evidence; code only for candidate-induced defects |
| F1 Independent review | ledger/hash/SHA/recovery verification | read-only final review |

All writers and A0 use isolated run-scoped worktrees and local branches. The shared `main` worktrees are read-only for this run. They are not alone in the workspace and must preserve concurrent changes. Git author and committer for authorized local commits is `leeyi <leeyisoft@qq.com>` using command-scoped environment variables; this does not authorize main integration, push, or publication.

## 4. Run Layout

```text
RUN_ID=ent-org-v21-closure-<UTC>-<random8>
RUN_ROOT=/Users/leeyi/project/imboy.pub/.Codex/runs/$RUN_ID
WORKTREE_ROOT=/Users/leeyi/project/imboy.pub/.Codex/worktrees/$RUN_ID

IMBOY_CANDIDATE=$WORKTREE_ROOT/imboy/integration
ADMIN_CANDIDATE=$WORKTREE_ROOT/imboyadmin/integration
APP_CANDIDATE=$WORKTREE_ROOT/imboyapp/integration
```

Candidate branches are `codex/$RUN_ID-imboy`, `codex/$RUN_ID-imboyadmin`, and `codex/$RUN_ID-imboyapp`. Worker branches fork from the matching captured base or candidate checkpoint; only A0 integrates worker commits into the three candidate branches.

Required durable files:

```text
control/run.json
control/baseline.json
control/leases.json
control/integration-ledger.json
control/acceptance-ledger.json
control/pre-test-candidate-manifest.json
evidence/<ACCEPTANCE_ID>/...
FINAL/candidate-manifest.json
FINAL/verifier-verdict.json
FINAL/final-report.md
```

The run must record commands, cwd, allowlisted environment, start/end time, exit code, test count, Oracle count, skipped count, stdout/stderr hashes, evidence hashes, and candidate SHA.

## 5. Execution Waves

### W0: Stable Baseline And Handoff Gate

1. Validate this plan against its adjacent `.sha256` file.
2. Read root and repository-local `AGENTS.md` / `CLAUDE.md` files.
3. Double-sample, at least 10 seconds apart: three repo HEAD/status/worktrees/branches, active run state, processes, ports, databases, migrations, devices, and WIP hashes. Record any drift caused by the parallel LiveKit closure.
4. Prove old candidates `64dd8060`, `365ed696`, and `eacc5301` are ancestors of current `main`.
5. Confirm no V2.1 branch/worktree remains. Do not modify remaining LiveKit resources.
6. Record any WIP or foreign-run path discovered at execution time as protected before continuing.
7. Wait for any active Git operation/index lock to finish; never delete the lock. Capture `BASE_SHA` for each repo and create the three integration worktrees from those exact commits.
8. Source-path overlap with LiveKit is allowed only because Git worktrees are isolated; record it as a future merge risk. Obtain formal handoff before using any runtime resource or device still owned by another run.

W0 may start read-only work immediately. Writer work is allowed only inside this run's worktrees and with independent database/port leases. Later movement of shared `main` does not invalidate the captured base, but must be recorded for the future merge audit. If runtime or device ownership cannot be resolved, continue all independent work and return `BLOCKED_ACTIVE_COORDINATOR_HANDOFF` only for the affected gates without deleting or adopting resources.

### W1: Current-Main Requalification

A2 runs the existing contract and security closure against a scratch PostgreSQL 18 database and loopback-only services.

Minimum Backend commands:

```bash
python3 api/flatten_internal.py --check
make compile
make contract-check
make arch-check
make security-gate
make eunit-local t=enterprise_internal_wiring_tests
make eunit-local t=enterprise_internal_pg_tests
make eunit-local t=enterprise_internal_read_pg_tests
make eunit-local t=enterprise_internal_cursor_v2_tests
make eunit-local t=enterprise_internal_sso_tests
make eunit-local t=enterprise_msg_asset_webhook_pg_tests
```

The exact current module names must be discovered from source before execution; missing renamed modules are not silently skipped.

Contract Oracle:

- Exactly 31 unique runtime IDs, OpenAPI operation IDs, and Postman requests.
- Exactly 14 fixed scopes and no wildcard.
- Exactly 4 Human Directory read APIs.
- Runtime/OpenAPI/Postman method+path set difference is zero.
- Security negative matrix asserts exact status, error class, visibility, and DB/audit effects.

### W2: Eliminate Full-Suite Baseline Debt

A1 reproduces the three known failures first:

- `enterprise_application_grant_pg_tests`: two migration-cycle failures.
- `enterprise_internal_foundation_pg_tests`: one migration-cycle failure.

Fix the shared root cause once. Prefer the existing migration helper or correct schema-migration contract; do not weaken assertions, skip cases, pin an obsolete migration head, or convert failures into expected results.

Required outcome:

```text
enterprise_application_grant_pg_tests: 8/8 PASS
enterprise_internal_foundation_pg_tests: 9/9 PASS
FULL_SUITE: PASS
REGRESSION_DELTA: 0
```

Run focused suites before the repository's complete EUnit gate. A complete gate that exits nonzero, runs zero tests, hides skips, or omits `-pa test` cannot pass.

### W3: Admin And Flutter Dependency Closure

Admin minimum gates:

```bash
bun test --isolate
bun run typecheck
bun run build
```

Then run the committed nine-leaf enterprise Playwright journey against the current local Backend. It must use real Cowboy responses and server-side Organization/Workspace/personal-resource filtering. No `page.route`, static JSON, or mock API may satisfy browser acceptance.

Flutter minimum gates:

```bash
flutter analyze
flutter test test/organization
```

Also run the exact committed Organization contract/controller/persistence tests discovered in the current tree. Prove request generation, stale-response rejection, refresh preservation, two-phase switch rollback, persistent-store atomicity, independent pagination, TSID losslessness, and Human Directory error envelopes.

### W4: Physical-Device Closure

Only after explicit device lease:

1. Android physical device: enterprise visible -> organization switch -> department tree -> search -> member detail -> start message.
2. iOS physical device: the same committed journey.
3. Preserve screenshots, device identifiers, OS versions, app commit, build hash, logs, and backend fixture identifiers.
4. Do not use real customer accounts or production services.

Android-only evidence is `DEVICE_PARTIAL`, not `DEVICE_PASS`. An unavailable iOS device yields `BLOCKED_NO_IOS_DEVICE`; it does not invalidate local worktree candidate qualification but prevents full device closure.

### W5: Integration And Final Freeze

1. A0 integrates only reviewed task commits, one functional commit at a time, into the three run-scoped candidate branches.
2. Never commit in or merge into the shared `main` worktrees. Never include protected foreign WIP. Use exact-path staging and verify the staged diff before every candidate commit.
3. Freeze the three final SHAs in `pre-test-candidate-manifest.json` before final reruns.
4. Any code, generated artifact, test configuration, or SHA drift invalidates dependent PASS entries and returns them to `NOT_RUN`.
5. F1 independently verifies every evidence hash, candidate SHA, test/Oracle rule, and current Git state.
6. Generate the final manifest and report only from the verified ledger; record `MERGE_TO_MAIN=NOT_PERFORMED` and the current main tips for a later merge-readiness audit.

## 6. Acceptance Ledger

| ID | Blocking for worktree candidate pass | Acceptance |
|---|---:|---|
| CL-00 | yes | plan hash, double sample, protected WIP, ownership and scratch resources valid |
| CL-01 | yes | old V2.1 candidates contained in current main; no V2.1 worktree/branch remains |
| CL-02 | yes | 31 endpoint / 14 scope / 4 Human API sets and artifacts agree |
| CL-03 | yes | current Backend focused contract/security/PG/HTTP gates pass |
| CL-04 | yes | the 3 migration-cycle failures are fixed; complete EUnit gate passes |
| CL-05 | yes | current Admin unit/type/build and real-backend nine-leaf browser gate pass |
| CL-06 | yes | current Flutter analyze and Organization local tests pass |
| CL-07 | yes | auth/Grant/boundary/IDOR/cursor/idempotency/audit/schema security matrix passes |
| CL-08 | no | Android physical-device Organization journey passes |
| CL-09 | no | iOS physical-device Organization journey passes |
| CL-10 | yes | final SHA/manifest/ledger/evidence hashes and clean candidate worktrees agree; shared main untouched |

Ledger rules:

- `test_kind=test`: `exit_code=0`, `test_count>0`, `skipped=0`.
- `test_kind=mechanical`: `exit_code=0`, `test_count=0`, `oracle_count>0`.
- Every PASS has current candidate SHA and resolvable evidence hashes.
- HTTP 200, screenshot, source existence, mock-only execution, zero tests, or an old PASS is insufficient.

## 7. Terminal States

```text
PLAN_READY
-> BLOCKED_ACTIVE_COORDINATOR_HANDOFF
-> REQUALIFICATION_IN_PROGRESS
-> LOCAL_WORKTREE_CANDIDATE_PASS
-> DEVICE_PARTIAL | DEVICE_PASS
-> EXTERNAL_NOT_EXECUTED
-> PRODUCTION_NOT_EXECUTED
-> RELEASE_NO_GO
```

`LOCAL_WORKTREE_CANDIDATE_PASS` requires CL-00 through CL-07 and CL-10. It does not require CL-08/CL-09, but the final report must keep `DEVICE_PASS=false` until both pass. It does not imply merge readiness against a later-moving `main`.

Any of these ends the run safely:

- `LOCAL_WORKTREE_CANDIDATE_PASS` with honest device status and `MERGE_TO_MAIN=NOT_PERFORMED`.
- A durable `BLOCKED_*` checkpoint after all safe independent work is complete.
- `NO_GO` when continuing would overwrite WIP, adopt foreign resources, weaken a security gate, or require scope expansion.

None of these states authorizes push, deployment, production migration, publication, or release.

## 8. Crash And Resume

On resume, read durable control in this order:

```text
FINAL/candidate-manifest.json, if hash-valid
control/pre-test-candidate-manifest.json
control/integration-ledger.json
control/acceptance-ledger.json
worker RESULT files
raw evidence
conversation summary last
```

Re-sample Git, worktrees, processes, ports, databases, migrations, devices, and protected WIP before continuing. Conflicting state becomes `BLOCKED_STATE_DRIFT`; never choose whichever record looks newest.

## 9. Final Report Contract

The final report must contain:

1. Exact terminal state and non-implications.
2. Three repo captured base SHA, candidate SHA, candidate branch/worktree, dirty state, integrated commits, and current main tip.
3. CL-00 through CL-10 results with test and Oracle counts.
4. The 31/14/4 set-difference results.
5. Full-suite results proving the three migration failures are gone.
6. Admin real-backend browser result.
7. Android and iOS physical-device results separately.
8. Plan, manifests, ledgers, reports, artifacts, logs, and WIP hashes.
9. Remaining blockers with exact resolution conditions.
10. Explicit `MERGE_TO_MAIN=NOT_PERFORMED / NO PUSH / NO DEPLOY / NO PRODUCTION / RELEASE_NO_GO`.

## 10. One-Shot ZCODE Coordinator Prompt

Submit the following prompt once. The coordinator must continue through safe work and must not stop after W0 or after producing another plan.

```text
你是本任务唯一协调器 A0。立即执行：
/Users/leeyi/project/imboy.pub/imboy/docs/plans/2026-09-24-enterprise-organization-admin-internal-v21-current-main-closure-plan-v1.md

完整读取计划及相邻 .sha256，校验哈希；读取工作区根级与 imboy、imboyadmin、imboyapp 的 AGENTS.md/CLAUDE.md。该计划是执行合同，不是让你再写一份方案。

目标：以执行时采集的三个本地 main SHA 为只读基线，立即创建本 run 独立 integration worktree/candidate branch；确认 V2.1 已完整包含；在这些 worktree 内重跑 31 endpoint / 14 scope / 4 Human Directory API、权限安全、Admin 真实后端浏览器和 Flutter Organization 闭包；从根因修复 3 个迁移回环失败，使 FULL_SUITE_PASS；在合法获得设备租约后完成 Android+iOS 真机组织旅程；重建绑定最终 worktree candidate SHA 的 manifest、ledger 和 final report。

当前 `docs/roadmap/2026-09-24-livekit-single-service-closure-plan-v2.md` 正在并行执行。先执行 W0，保护执行时发现的所有既有 staged/unstaged/untracked/ignored WIP；三个共享 main 工作树对本 run 永远只读。不得 reset、clean、stash、覆盖、blanket stage、删除或接管 foreign branch/worktree/process/lease/DB/device。等待任何进行中的 Git 操作自然结束后，从记录的 BASE_SHA 创建本 run worktree；不得删除 index.lock。源文件路径重叠只登记为未来合并风险，不阻塞隔离 worktree 写入；DB、端口、进程和真机必须独立租约，未 handoff 时只阻塞对应 runtime/device gate。

MAX_ACTIVE_AGENTS=6，包含 A0。按计划分派 A1 Backend、A2 Contract/Security、A3 Admin、A4 Flutter、F1 independent review；A0 与所有 writer 都使用 run-scoped isolated worktree，本地提交使用命令级身份 leeyi <leeyisoft@qq.com>。A0 只集成到本 run 的三个 candidate branch，严禁合并回 main；精确路径 stage，不混入用户或其他 run 的改动。worker 不是独占代码库，必须适配并保护并行变更。

不要重新实现 V2.1，不新增 endpoint/scope/schema/产品能力，不添加无必要依赖。修迁移测试必须修共享根因，不准 skip、降级断言、固定旧 head 或把失败改成 expected。测试失败时在合同范围内定位、修复、复测，不要第一次失败就停止；需要扩域时写 proposal 并将该卡标为 BLOCKED_SCOPE_EXPANSION。

所有 PASS 必须绑定当前 candidate SHA、非零 test/oracle、skipped=0 和可复算 evidence hash。旧 V2.1 PASS 只能作为历史证据，不能重绑。任何代码或 SHA 漂移使依赖项回到 NOT_RUN。设备必须分别报告 Android 和 iOS，模拟器、手工口述或单平台不能产生 DEVICE_PASS。

持续执行 W0->W1->W2->W3->W4->W5，直到：一，LOCAL_WORKTREE_CANDIDATE_PASS 并如实给出 DEVICE 状态和 MERGE_TO_MAIN=NOT_PERFORMED；二，所有安全独立工作完成后只剩真实 runtime handoff/设备/外部阻塞，已形成 durable BLOCKED checkpoint；或三，继续会破坏 WIP、安全边界或需要扩域，形成 NO_GO。不要在正常 wave 之间要求用户说“继续”。

禁止合并回 main、push、PR、远端写入、部署、生产 migration、生产凭证、真实客户数据、第三方通知、发布或 release。最终明确 MERGE_TO_MAIN=NOT_PERFORMED、EXTERNAL_NOT_EXECUTED、PRODUCTION_NOT_EXECUTED、RELEASE_NO_GO，并按计划第 9 节输出完整最终报告。
```
