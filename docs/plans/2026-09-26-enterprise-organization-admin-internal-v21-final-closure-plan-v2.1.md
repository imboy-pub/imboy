# IMBoy Enterprise Organization / Admin / Internal V1 V2.1 Final Closure Plan V2.1

> Status: execution contract, not a new product plan
> Date: 2026-09-26
> Source plan: `docs/plans/2026-09-23-enterprise-organization-admin-internal-v1-unified-plan-v2.1.md`
> Source plan SHA-256: `a866aa137eea856a9a6d9324d3cb46d3fb1f1fb8db73891808c85d1f17d23302`
> Historical run: `.Codex/runs/ent-org-internal-v21-20260923T110542Z-929b7d41`
> Incomplete closure run: `.Codex/runs/ent-org-v21-closure-20260924T042352Z-7cfd59c0`
> Pre-closure recovery: `.Codex/recovery/enterprise-v21-preclosure-20260926T143307Z/`

## 0. Executive Verdict

The V2.1 product implementation is already contained in all three current `main` histories. It must not be reimplemented or merged a second time. The remaining work is current-SHA requalification, missing evidence closure, a bounded final regression gate, user-approved device validation, and an auditable final verdict.

Current disposition at plan authoring time:

| Dimension | Status | Evidence-based conclusion |
|---|---|---|
| V2.1 implementation ancestry | `PASS` | Historical candidates `imboy 64dd8060`, `imboyadmin 365ed696`, `imboyapp eacc5301` are ancestors of current `main` |
| Dedicated closure fixes ancestry | `PASS` | `imboy d4113fb6`, `imboyadmin 342a674d`, and `imboyapp e38ac726` are ancestors of current `main` |
| Pre-closure Git convergence | `PASS` | 39/39 task branches had `git cherry plus=0`, were archived in verified complete-history bundles, and were deleted; the two empty RUN worktree directory trees were removed |
| Post-cleanup baseline | `FROZEN_FOR_AUTHORING` | `imboy e92fc932`, `imboyadmin 60a65af`, `imboyapp 1292175d`; execution still performs a fresh two-sample baseline |
| Current Internal contract shape | `PASS_STATIC_ONLY` | `25 paths / 31 operations / 0 $ref`; Postman has 31 requests and bundle has 31 operation IDs |
| Historical V2.1 final evidence | `STALE` | Historical verifier already reported current HEAD/evidence SHA drift |
| 2026-09-24 closure run | `INCOMPLETE_GOVERNANCE` | It has useful focused/Admin/Flutter evidence and a pre-test manifest, but no acceptance ledger, no `FINAL/`, and no final verifier verdict |
| Current-SHA CORE acceptance | `NOT_RUN` | No current candidate manifest and no 21/21 current-SHA CORE ledger |
| User-adjusted device gate | `NOT_RUN` | Android and macOS are currently discoverable, but no evidence is bound to the final candidate created by this run |
| Original V2.1 device gate | `NOT_REQUIRED_BY_USER` | iOS is best effort and non-blocking; never fabricate `DEVICE_PASS` when the exact Android+iOS original gate was not run |
| External/production/release | `NOT_AUTHORIZED` | `EXTERNAL_NOT_EXECUTED`, `PRODUCTION_NOT_EXECUTED`, `RELEASE_NO_GO` |

Therefore the plan is not currently 100% accepted. The implementation quality is high and the remaining risk is predominantly qualification/evidence drift, test isolation debt, and protected WIP handling rather than missing V2.1 product scope.

## 1. Meaning Of 100% For This Closure

This run may report `USER_ADJUSTED_LOCAL_COMPLETION=PASS` only when all of the following are true:

1. All 21 original V2.1 CORE acceptances are `PASS` on one frozen final candidate: `BASE-01`, `CON-01`, `CON-02`, `MIG-01`, `IDX-01`, `AUTH-01`, `CUR-01`, `IDEM-01`, `API-READ-01`, `API-SCOPE-01`, `ADM-01`, `ADM-02`, `ADM-03`, `ORG-01`, `APP-01`, `APP-02`, `SEC-01`, `API-SCHEMA-01`, `E2EE-LOCAL`, `DOC-01`, and `CAND-01`.
2. Every required Acceptance has current candidate SHA, command, exit code, non-zero test/oracle count, skipped count, environment identity, and resolvable evidence hash.
3. Android physical device and macOS App each pass the committed enterprise Organization journey twice on the final Flutter candidate SHA.
4. iOS is attempted only when safely available. Its failure/unavailability is recorded honestly as `OPTIONAL_BLOCKED` or `OPTIONAL_NOT_EXECUTED` and does not block the run.
5. Any closure fixes are safely integrated into the appropriate local `main`, or there are no candidate-only fixes because current `main` already equals the verified candidate.
6. Post-integration minimal smoke and candidate/main SHA reconciliation pass.
7. Final ledger, manifests, transition audit, evidence hashes, and report agree mechanically.

The final report must keep these statements separate:

```text
V2_1_CORE_ACCEPTANCE=PASS|FAIL|BLOCKED
USER_ADJUSTED_DEVICE_GATE=PASS|FAIL|BLOCKED
USER_ADJUSTED_LOCAL_COMPLETION=PASS|FAIL|BLOCKED
ORIGINAL_V2_1_ANDROID_IOS_DEVICE_PASS=PASS|NOT_EXECUTED|PARTIAL|BLOCKED
IOS_OPTIONAL=PASS|FAIL|OPTIONAL_BLOCKED|OPTIONAL_NOT_EXECUTED
MERGE_TO_MAIN=PERFORMED|NOT_NEEDED|BLOCKED_PROTECTED_WIP
EXTERNAL=NOT_EXECUTED
PRODUCTION=NOT_EXECUTED
RELEASE=NO_GO
```

`100%` means 100% of this plan's approved local acceptance profile. It does not mean production deployment, release, zero unrelated repository debt, or original Android+iOS `DEVICE_PASS` when iOS was not executed.

## 2. Scope And Non-Scope

In scope:

- Requalify the existing V2.1 implementation on current source.
- Repair only defects exposed by the 21 CORE acceptances or their test/runtime harnesses.
- Produce current-SHA Internal/OpenAPI/Postman, Backend, Admin, Flutter, security, migration, browser, Android, and macOS evidence.
- Create one authoritative ledger/verifier/final report.
- Safely integrate closure-only commits to local `main` when the protected-WIP and ancestry gates permit it.

Out of scope:

- New endpoint, scope, schema, product feature, Web tenant portal, or independent Web enterprise authentication domain.
- Push, PR, deployment, production migration, production credential, production data, publication, release, or third-party notification.
- Rewriting V2.1 merely because old evidence is stale.
- Fixing unrelated repository-wide warnings, quarantined tests, or baseline failures unless they invalidate a V2.1 Acceptance or prevent the one final gate from producing a trustworthy result.
- Resetting, cleaning, stashing, staging, committing, moving, overwriting, or deleting protected/foreign WIP.

## 3. Protected Current State

At authoring time the shared topology has only the three primary `main` worktrees plus the protected foreign detached `/private/tmp/intbe03-baseline`. The 39 patch-equivalent task branches and two empty RUN worktree directory trees have already been removed after verified recovery bundles were created. Execution must still re-sample twice and use the second stable sample as the actual baseline.

Known protected paths include:

```text
imboy:
  untracked docs/customer-service-v2/
  untracked docs/enterprise-upgrade/

imboyadmin:
  clean at authoring snapshot

imboyapp:
  staged macos/Podfile.lock
```

Authoring snapshot and protected hashes:

```text
imboy      e92fc93231435a1f57c47098564050d372298572
imboyadmin 60a65af11259a0b5806dc82dec3eb3843d14fb7e
imboyapp   1292175de67922592621f1b95467ede1804159f2

macos/Podfile.lock worktree/index blob:
  f7466070b0d0834155bb7a39f37272e62db039e2
macos/Podfile.lock cached diff SHA-256:
  dfa120849cef2b39f4f1953a31b1a2b333dd746546ea8713e918d8720142b015
design-directory aggregate SHA-256:
  7825316a7024478d05c72fe9e5d9dd0817c7330d3b486c164364ff6444cb05d9
```

These are not V2.1 closure inputs. Record path, status, worktree blob hash, index blob hash, diff hash, owner if known, and two-sample stability. Do not use `git stash`, `git reset`, `git clean`, blanket `git add`, or force removal.

The completed customer-service run and all old enterprise runs are read-only evidence sources. Its surviving iOS tunnel process is foreign and must not be killed or adopted. Existing processes, ports, databases, devices, branches, worktrees, and leases remain foreign until runtime ownership is mechanically proven or formally handed off.

## 4. Fast Execution Index

Startup order:

```text
F0 verify plan + sidecar
F1 inspect source plan acceptance definitions
F2 create/adopt new RUN_ROOT and A0 supervisor lock
F3 double-sample Git/WIP/process/port/DB/device state
F4 create isolated candidate worktrees from current main HEADs
F5 freeze contract snapshot and initial candidate SHA
F6 dispatch eligible cards by DAG
F7 run focused/L2 gates; repair only proven defects
F8 freeze final candidate
F9 run each repository L3 at most once
F10 run Android + macOS required device gate; iOS optional
F11 verifier computes 21/21 CORE and device profile
F12 safely integrate closure commits to main if needed
F13 post-integration smoke, cleanup owned worktrees/branches, final report
```

Initial eligible cards after `FC-00` passes:

```text
FC-01 Contract/Security
FC-02 Backend/Migration
FC-03 Admin/Browser
FC-04 Flutter Organization
```

Recovery rule: ordinary worker, test, or environment failure never stops unrelated eligible cards. Retry is finite. After budget exhaustion, block only the affected card and continue the DAG.

Only these may stop the whole run:

- Protected WIP would be overwritten or cannot be attributed safely.
- Security violation or destructive/production action is required.
- Base SHA drift cannot be reconciled without discarding commits.
- Migration state is corrupt outside a disposable run-scoped database.
- Unknown state may cause data loss or duplicate external effects.

## 5. A0 Supervisor And Durable Recovery

A0 is the sole Supervisor and shared-path integrator. It persists state under:

```text
/Users/leeyi/project/imboy.pub/.Codex/runs/<RUN_ID>/
  control/run.json
  control/supervisor.json
  control/supervisor.lock
  control/baseline.json
  control/workers.json
  control/task-queue.json
  control/leases.json
  control/transitions.jsonl
  control/integration-ledger.json
  control/acceptance-ledger.json
  control/pre-test-candidate-manifest.json
  evidence/<CARD_ID>/
  FINAL/candidate-manifest.json
  FINAL/verifier-verdict.json
  FINAL/final-report.md
```

Supervisor loop every 30 seconds:

1. Reconcile run state, task dependencies, worker heartbeat, `last_progress_at`, attempts, and retry budget.
2. Reconcile leases against process existence, ports, DB identity, devices, worktrees, branches, and candidate SHAs.
3. Reconcile acceptance entries and evidence hashes; stale candidate bindings return to `NOT_RUN`.
4. Reclaim a worker lease only after heartbeat timeout and process/worktree verification.
5. Dispatch all eligible independent cards while respecting ownership and resource limits.
6. Persist every transition before performing its side effect.

Timeouts and retry budgets:

| Item | Timeout | Automatic action | Maximum |
|---|---:|---|---:|
| Worker heartbeat | 120s | probe process/worktree, then `HEARTBEAT_TIMEOUT` | 2 probes |
| No real progress | 600s | save evidence, cancel owned process, reclaim lease | 2 worker replacements |
| Focused test | 15m | terminate owned process, classify, rebuild environment if applicable | 2 retries |
| Browser L2 | 20m | save trace/log, restart owned backend/browser | 2 retries |
| Backend L3 | 60m | save complete partial log, one clean-env rerun | 1 rerun |
| Admin L3 | 30m | one clean-env rerun | 1 rerun |
| Flutter plan bundle | 30m | one clean-env rerun | 1 rerun |
| Card attempts | n/a | `BLOCKED` and continue independent DAG | 3 total |

Failure classes:

```text
TRANSIENT         -> retry command within budget
WORKER_CRASH      -> reclaim lease, reassign from durable commit/evidence
WORKER_STALL      -> terminate only owned process, reclaim, reassign
ENVIRONMENT       -> rebuild only run-scoped environment, retry
TEST_FAILURE      -> reproduce once, then route root cause to owner
CODE_FAILURE      -> fix within card scope, focused retest
LEASE_CONFLICT    -> block affected runtime/device card, continue others
SCOPE_EXPANSION   -> block card; do not add product scope
BASELINE_DRIFT    -> reconcile ancestry/WIP; rebase only by new candidate commit
SECURITY_VIOLATION-> HARD_STOP
PROTECTED_WIP     -> HARD_STOP only if safe isolation/integration is impossible
UNKNOWN           -> retry diagnostics twice; HARD_STOP only if data-loss risk remains
```

A0 restart recovery:

- Adopt the run only after the old Supervisor heartbeat is stale and no live process owns the lock.
- Atomically archive the stale lock, increment `supervisor_epoch`, and persist the adoption transition.
- Read manifests, ledgers, task results, and raw evidence in that order; chat text is never authoritative.
- Reattach live owned workers when identity/lease/heartbeat agree; otherwise reclaim and redispatch.
- Repeated reconcile is idempotent: never duplicate commit, migration, message, test record, or device action.

## 6. DAG And Ownership

```text
FC-00 Baseline/Reconcile
  -> FC-01 Contract/Security
  -> FC-02 Backend/Migration
  -> FC-03 Admin/Browser
  -> FC-04 Flutter Organization

FC-01 + FC-02 + FC-03 + FC-04
  -> FC-05 Candidate Freeze + One-Time L3
  -> FC-06 Android/macOS Device Gate (+ optional iOS)
  -> FC-07 Acceptance Verifier
  -> FC-08 Main Integration/Post-Merge Smoke
  -> FC-09 Cleanup/Final Report
```

Maximum active agents is 6 including A0. Suggested owners:

| Card | Owner | Exclusive responsibility |
|---|---|---|
| FC-00 | A0 | baseline, leases, queue, worktrees, protected WIP |
| FC-01 | A1 | 31/14/4 contract, auth, scope, cursor, idempotency, security, docs |
| FC-02 | A2 | migration lifecycle, Backend focused and final L3 root cause |
| FC-03 | A3 | Admin unit/type/build and real-backend nine-leaf browser |
| FC-04 | A4 | Flutter Organization focused tests/analyze/contract |
| FC-05 | A0 + F1 | candidate freeze and exactly-once L3 scheduling |
| FC-06 | F2 | Android/macOS device execution; iOS optional |
| FC-07..09 | A0 + F1 | independent evidence review, verifier, integration, cleanup |

All writers use run-scoped worktrees. Workers are not alone in the codebase and must preserve parallel changes. A0 alone may touch candidate integration branches or shared routing/generated files. Every local commit uses command-scoped identity `leeyi <leeyisoft@qq.com>`.

## 7. Cards And Acceptance

### FC-00 Baseline, Handoff, And Isolation

Actions:

- Verify this plan and sidecar plus source-plan SHA.
- Double-sample all repository HEAD/status/worktrees, protected WIP hashes, relevant processes, ports, DBs, migrations, devices, old run state, and leases.
- Do not resume the 2026-09-24 closure run as if its removed worktrees were current. Create a new run and import old artifacts as `HISTORICAL_REFERENCE` only.
- Create three isolated candidate worktrees from execution-time `main` HEADs.
- Build disposable PostgreSQL 18 DBs and unique ports. Never use shared/production DBs.

PASS:

- Stable baseline, no unknown owner on any resource used by this run, protected WIP hashes recorded, isolated worktrees clean, source candidate contains historical V2.1 and closure commits.

### FC-01 Contract, Security, And Documentation

Required oracles:

- Runtime/OpenAPI/Postman exactly 31 operations, 25 paths, and pairwise method+path difference zero.
- Exactly 14 fixed scopes, no wildcard, and four Human Directory GET APIs.
- Current focused suites prove credential, Grant, tenant boundary, cursor, idempotency, audit, IDOR, response parity, E2EE local separation, and importable Postman placeholders.
- Reopen the old audit gap: write operations required by the frozen audit policy must each prove one atomic audit side effect. Do not accept the old `PARTIAL` security result as current PASS.

Bound acceptances:

```text
CON-01 CON-02 AUTH-01 CUR-01 IDEM-01 API-READ-01 API-SCOPE-01
SEC-01 API-SCHEMA-01 E2EE-LOCAL DOC-01 ORG-01
```

### FC-02 Backend And Migration

Required oracles:

- Fresh/current/up-down-up migration cycle on a disposable PG18 DB; expected version, dirty=false, schema assertions non-zero, rollback leaves no residue.
- Current focused V2.1 suites pass with zero failed/skipped and non-zero tests.
- Preserve the shared root-cause closure already represented by `d4113fb6`; do not reintroduce hardcoded migration head or shared-connection assertions.
- Run Backend full EUnit only once at frozen final candidate. If it exposes unrelated cross-suite debt, reproduce against the captured base. Fix test isolation when safely in scope; otherwise record a mechanically proven baseline debt without mislabeling the command PASS.

Bound acceptances:

```text
MIG-01 IDX-01 ADM-01
```

### FC-03 Admin And Real Browser

Required oracles:

- Focused enterprise tests, typecheck, and build pass.
- Real local backend/browser gate covers all nine enterprise leaves with real fixture data, server-enforced Organization/Workspace scope, URL/filter/network evidence, and no route mocking.
- Member/department write operations, CAS/conflict, authorization/revocation/cross-tenant negatives, group/channel side effects, and permission matrix are asserted where the current enterprise journey requires them.
- UI text remains i18n-backed, touch targets meet 44px, and the required 3 pages x 2 viewports x light/dark evidence set is complete when the UI candidate changes those surfaces.

Bound acceptances:

```text
ADM-02 ADM-03
```

### FC-04 Flutter Organization

Required oracles:

- Focused Organization/contact/API tests cover data, empty, error, multi-org, department tree, search, my department, permissions, and atomic organization switch.
- `flutter analyze` has no new relevant error/warning; TSIDs remain strings/`EntityId`; request/response contracts match current backend.
- No edit under protected `ios/*`, `macos/*`, `plugin/r_upgrade`, or staged `macos/Podfile.lock`.

Bound acceptances:

```text
APP-01 APP-02
```

### FC-05 Freeze And Efficient Test Ladder

Use the existing test ladder, not per-worker global testing:

| Level | When | Rule |
|---|---|---|
| L0 | every changed card | compile/static/diff checks only for affected paths |
| L1 | before card handoff | focused domain tests only |
| L2 | after integration | real PG/HTTP/browser journey for the affected domain |
| L3 | once after final freeze | at most once per repository; rerun only the repository whose SHA changes |

One-time final gates:

- Backend: one full EUnit run plus compile/contract/arch/security/diff gates.
- Admin: one full unit run, typecheck, build, and the real-backend Playwright gate.
- Flutter: plan-scoped Organization test bundle plus analyze. A repository-wide Flutter test is diagnostic, not required to rerun repeatedly; if run, unrelated pre-existing skips must be disclosed rather than rewritten as plan PASS.

Create `pre-test-candidate-manifest.json` before L3. Any code change invalidates only dependent acceptances and only the changed repository's L3.

### FC-06 Required Android And macOS; Optional iOS

Required on the final Flutter candidate SHA:

```text
Android device XWE6R19916004085: two consecutive runs
macOS device macos: two consecutive runs
```

Journey must prove login/test identity, enterprise visibility, organization switch, department tree, member detail, authorized navigation/message handoff, restart/reload persistence, and a negative permission/tenant boundary. Each run records app SHA, device/OS, command, exit, assertions, screenshots/logs, backend/DB oracle, and cleanup.

iOS device `00008140-000E30561E32801C` is best effort. Certificate trust, disconnect, unavailable device, or test failure is recorded but never blocks `USER_ADJUSTED_LOCAL_COMPLETION`. It must not be reported as PASS without the actual journey.

### FC-07 Mechanical Acceptance Verifier

The verifier ignores manually written overall status and recomputes:

- candidate SHA equals manifest and tested worktree;
- 21 CORE acceptances all PASS;
- every evidence file exists and hash matches;
- test entries have exit 0, tests > 0, failed 0, skipped 0;
- mechanical entries have exit 0 and oracle count > 0;
- protected WIP matches baseline;
- Android and macOS each have two current-SHA passing runs;
- iOS is reported separately;
- report/ledger/manifest hashes are mutually consistent.

### FC-08 Safe Main Integration

If no source/test fix was needed, set `MERGE_TO_MAIN=NOT_NEEDED` after proving candidate SHA equals captured/current `main` SHA.

If closure commits exist:

1. Re-sample shared `main` and protected WIP.
2. Require captured base to remain an ancestor and no overlapping dirty path.
3. Integrate only the unique per-repository candidate with `git merge --ff-only` when possible. Never merge every worker branch individually.
4. Do not stash/reset/clean protected WIP. If overlap prevents safe integration, preserve the complete candidate and set `BLOCKED_PROTECTED_WIP` rather than forcing it.
5. Run minimal post-merge smoke and re-check protected WIP bit-for-bit/index-for-index.

No push, PR, deploy, production migration, or publication.

### FC-09 Cleanup And Final Report

- Remove only this run's clean, merged worktrees with `git worktree remove`.
- Delete only this run's branches proven contained in `main`, using `git branch -d` only.
- Never use `--force` or `git branch -D`.
- Keep `RUN_ROOT`, evidence, final manifest, ledger, verifier, and report.
- Report every remaining branch/worktree/resource without deleting foreign state.

## 8. Transition Audit And Idempotency

Every transition appends one line to `control/transitions.jsonl`:

```json
{
  "timestamp":"RFC3339",
  "run_state_from":"EXECUTING",
  "run_state_to":"RECONCILING",
  "task_id":"FC-02",
  "worker_id":"A2-1",
  "attempt":2,
  "reason":"TEST_FAILURE",
  "candidate_sha":{"imboy":"40hex","imboyadmin":"40hex","imboyapp":"40hex"},
  "command_ref":"evidence/FC-02/attempt-2/command.json",
  "result_ref":"evidence/FC-02/attempt-2/RESULT.json",
  "evidence_sha256":"64hex"
}
```

Before any side effect, record an idempotency key based on `run_id + card + attempt + action + candidate_sha`. Reconcile must detect already-completed actions and must not duplicate commit, migration, audit row, external message, or ledger record.

## 9. Final Stop Conditions

Successful terminal state:

```text
V2_1_CORE_ACCEPTANCE=PASS (21/21)
USER_ADJUSTED_DEVICE_GATE=PASS (Android + macOS)
USER_ADJUSTED_LOCAL_COMPLETION=PASS
ORIGINAL_V2_1_ANDROID_IOS_DEVICE_PASS=<honest result>
IOS_OPTIONAL=<honest result>
MERGE_TO_MAIN=PERFORMED|NOT_NEEDED
EXTERNAL_NOT_EXECUTED
PRODUCTION_NOT_EXECUTED
RELEASE_NO_GO
```

Do not use `COMPLETED`, `100%`, or `PASS` when any required acceptance is missing, stale, hash-invalid, candidate-mismatched, zero-test, mock-only, skipped, or manually inferred.

## 10. One-Shot ZCODE Prompt

Copy the following prompt into the next session exactly once:

```text
你是本任务唯一 A0 Supervisor。立即执行，不要再写一份方案：

/Users/leeyi/project/imboy.pub/imboy/docs/plans/2026-09-26-enterprise-organization-admin-internal-v21-final-closure-plan-v2.1.md

先完整读取该计划、相邻 .sha256、源计划 `2026-09-23-enterprise-organization-admin-internal-v1-unified-plan-v2.1.md`，以及根级和三个仓库当前有效的 AGENTS.md/CLAUDE.md。校验两个计划 SHA。目标不是重做产品需求，也不是新增功能，而是在当前源码上把 V2.1 的 21 个 CORE Acceptance、Android 真机、macOS App、候选/账本/证据/main 集成收口到可机械证明的用户调整后本地 100% 完成。

已知事实只能作为启动线索，必须现场复验：历史 V2.1 三仓候选和 2026-09-24 closure 修复均已在当前 main 祖先链；旧 V2.1 verifier 因 SHA 漂移失效；旧 closure run 缺 acceptance ledger、FINAL 和最终 verifier，不能续写成 PASS；当前静态合同曾为 25 paths / 31 ops / 0 ref。旧 run 和客服 run 的证据只能用于找到命令/夹具/缺陷，禁止把旧 PASS 重绑到新 SHA。

Git 前置收敛已完成：39 个 patch-equivalent 任务分支已在 verified complete-history bundles 归档后删除，两个空 RUN worktree 目录树已清理；不得恢复或重新 merge 它们。当前受保护状态是 imboy 的两个 untracked 设计目录、imboyapp staged macos/Podfile.lock，以及 foreign detached `/private/tmp/intbe03-baseline`。启动时仍须双采样并记录 path/status/worktree hash/index blob/diff hash；不得 reset、clean、stash、覆盖、移动、提交或删除。所有 writer 在新 RUN_ID 的隔离 worktree 工作。旧客服 RUN 的 iOS tunnel 进程及现存端口、数据库、设备、worktree、branch、lease 一律先视为 foreign，机械证明所有权或正式 handoff 后才能使用。

A0 持久化 supervisor loop、heartbeat、last_progress、lease/watchdog、task queue、candidate SHA、acceptance ledger 和 transition audit。普通 TRANSIENT/WORKER_CRASH/WORKER_STALL/ENVIRONMENT/TEST_FAILURE/CODE_FAILURE 不得要求用户确认；有限重试、自动 reclaim/reassign，并继续其他 eligible cards。每卡最多 3 attempts、worker replacement 最多 2、环境重建最多 2；禁止无限 retry。只有 protected WIP 会被破坏、security violation、破坏性/生产动作、不可恢复 base drift、不可安全判断的 migration corruption、或未知且可能数据丢失时才 HARD_STOP。

MAX_ACTIVE_AGENTS=6（含 A0）。按 DAG 并行执行 FC-01 Contract/Security、FC-02 Backend/Migration、FC-03 Admin/Browser、FC-04 Flutter Organization；A0 负责 FC-00、候选集成、FC-05 freeze/L3、FC-07 verifier、FC-08 main integration、FC-09 final。每个 worker 派工必须包含 owned_paths、forbidden_paths、dependencies、commands、Acceptance IDs、evidence path、base/candidate SHA、lease、retry/stop conditions。worker 不是独占代码库，必须保护并适配并行变更。

效率规则：Worker 只跑 L0 affected checks + L1 focused tests；集成后跑 L2 真实 PG/HTTP/browser journey；冻结最终候选后每仓最多一次 L3。不得让每个 Worker 跑全局 EUnit、全量单页或全 Flutter suite。只有候选 SHA 改变的仓重跑其受影响 Acceptance 和该仓 L3。Backend 全 EUnit 只在最终冻结候选跑一次；Admin 全单测/typecheck/build 只跑一次；Flutter 运行计划指定 Organization bundle + analyze，仓库全量只能作为一次诊断，不得重复拖住 DAG。

测试失败不要第一次就停。先区分候选缺陷、基线债务、环境故障和 flaky；在 disposable scratch PG/独占端口重现。候选/计划相关缺陷在原范围修根因、提交、focused 复测；无关基线债务要在 captured base 同环境机械复现并如实披露，不能把失败命令写成 PASS。不得 skip、降级断言、放宽到任意 4xx/500、固定旧 migration head 或用 mock/截图/HTTP 200 冒充行为闭环。

设备门：Android 真机 XWE6R19916004085 和 macOS App `macos` 是 USER_ADJUSTED_DEVICE_GATE 必过项，各在最终 Flutter SHA 连续两轮完成登录/企业可见/切换/部门树/成员详情/授权跳转或消息交接/重启持久化/权限负例，并绑定命令、SHA、设备、日志、截图、后端/DB oracle。iOS 00008140-000E30561E32801C 仅 best effort、非阻断；不可用、证书信任失败或测试失败不得让 Run BLOCKED/HARD_STOP，也不得伪造 PASS，记录 OPTIONAL_BLOCKED/OPTIONAL_NOT_EXECUTED/真实结果。最终分别报告 USER_ADJUSTED_LOCAL_COMPLETION 和 ORIGINAL_V2_1_ANDROID_IOS_DEVICE_PASS。

候选修复验证完成后，只集成每仓唯一 candidate，不逐个 merge worker branch。若无修复且 candidate 等于 main，记 MERGE_TO_MAIN=NOT_NEEDED；若有修复，重新采样 main/WIP，base 仍为祖先且零 dirty overlap 时才 `git merge --ff-only`。不允许 stash WIP。若 protected WIP overlap 无法安全合并，保留完整候选并记 BLOCKED_PROTECTED_WIP，禁止强行完成。合并后运行最小 smoke，并证明 protected WIP worktree/index 状态 bit-for-bit 不变。

最终 verifier 必须从事实计算 21/21 CORE、Android/macOS 两轮、iOS 独立状态、三仓 SHA、evidence hash、test/oracle/skipped、WIP、manifest/ledger/report 一致性。只有全部 required 门通过，才输出 USER_ADJUSTED_LOCAL_COMPLETION=PASS。清理仅限本 RUN 已合并 clean worktree 和 `git branch -d` 可删除分支；禁止 --force、branch -D，保留 RUN_ROOT/evidence。

不得 push、PR、部署、生产迁移、使用生产凭证/真实客户数据、通知第三方、发布或 release。必须保持 EXTERNAL_NOT_EXECUTED、PRODUCTION_NOT_EXECUTED、RELEASE_NO_GO。不要在 W0、单 Worker、一次测试失败或普通环境故障后结束，也不要等待用户说继续；自动恢复并持续执行，直到完整 PASS，或留下证据充分且仅由真正 HARD_STOP 条件造成的终态。
```
