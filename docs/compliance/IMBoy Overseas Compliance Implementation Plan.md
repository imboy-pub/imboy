# IMBoy Overseas Compliance Implementation Plan

> Input: [Self-Audit](./IMBoy%20Overseas%20Compliance%20Self-Audit.md) and [Gap Matrix](./IMBoy%20Compliance%20Gap%20Matrix.md)
> Goal: overseas release baseline, not comprehensive legal compliance.
> Rule: reuse Handler -> Logic -> DS -> Repo, `imboy_policy`, `adm_acl`, report/denylist/audit tables and existing feature gates. Use one canonical product-feature manifest to define the build-time ceiling for Backend, Flutter and Admin. Do not rewrite Message/Group/Channel/Workspace/E2EE.

## Execution Rules

Each task must record base SHA, exact changed files, commands/results, evidence paths and residual risk. A task is not PASS when tests skip, infrastructure fails before assertions, or only mocks/static scans pass. Run repo preflight separately because the umbrella root is not Git.

Do not start tasks marked `LEGAL` until the named decision is supplied. Do not add KYC, identity documents, AI/video moderation, payment, CRM, creator economy or a generic global rules engine.

## Dependency and Merge Order

```text
F-00 -> F-01 -> F-02
F-02 -> F-03
F-02 -> F-04
F-02 -> F-05
F-03 + F-04 + F-05 -> F-06 -> F-07
F-07 -> F-08
F-07 -> F-09
F-07 -> L-01
F-07 -> D-01 + R-01 + B-01 + A-01 + T-01 + E-01
D-01 -> D-02 -> D-03 -> D-04
R-01 -> R-02 -> R-03 -> R-04
B-01 -> B-02
A-01 -> A-02
T-01 -> T-02 -> P-01
E-01 -> E-02

Merge order: feature inventory/manifest/generator -> Backend/Flutter/Admin slicing
             -> artifact consistency -> policy decisions/docs -> DB contracts
             -> backend logic/API -> Admin -> Flutter -> integration/release evidence.
```

## Feature Composition Architecture

The packaging requirement is a product/delivery architecture requirement, not a legal requirement. A canonical manifest defines `compiled_features`, the maximum capability set present in an artifact. Runtime configuration may only disable members of that set:

```text
effective_features = compiled_features intersect runtime_enabled_features
```

Disabled business modules, routes, screens, optional SDKs and assets should not enter the relevant release artifact. Shared infrastructure and a compatible database-schema superset may remain. Client or Admin manifests are never authorization sources; Backend authentication, authorization and safety enforcement remain mandatory.

Base is non-optional where required to operate any enabled interactive/UGC surface: account/auth/session/device, authorization, health/config/upgrade, security audit/logging, privacy notice and account deletion, plus report/block/moderation foundations whenever UGC or user interaction is compiled. The exact list must be proved by F-00 and validated by F-02, not duplicated by hand across repositories.

### Task F-00 - Feature Boundary Inventory

**Goal:** Map every product feature to its Backend routes/workers/modules, Flutter routes/screens/import roots, Admin routes/menu/modules, optional SDKs/assets/permissions, data tables and dependencies.

**Ownership / Files:** documentation-only owner; inspect all three repositories and `imboy-sdk-js`; write one inventory under `imboy/docs/compliance`. Do not edit application code.

**Tests / Evidence:** current-HEAD file/function references and negative evidence for ambiguous boundaries; classify each item as Base, optional, shared infrastructure or unknown.

**Acceptance:** every currently advertised/registered feature has one owner and dependency closure; Base decisions include reasons; unknown boundaries block slicing that feature.

**Stop:** if a feature cannot be separated without rewriting a core domain, record Architecture Gap and keep it in Base for the first implementation.

### Task F-01 - Canonical Product Feature Manifest

**Goal:** Define one human-edited manifest as the source for all three builds.

**Ownership / Files:** Backend/product-config owner exclusively owns the manifest and schema in the existing `imboy/config` or nearest established config location selected after inspection. Other repositories consume generated outputs only.

**Implementation:** minimal versioned schema containing product/profile identity, Base declaration/reference and selected optional features. Reuse `imboy_policy_catalog`, `imboy_profile_preset`, `imboy_feature:feature_names/0` and existing dependency knowledge; do not create a second policy catalog. Define canonical ordering and a manifest hash.

**Tests:** valid Base-only and selected-feature fixtures; unknown/duplicate feature, missing dependency, attempted Base disable and malformed version fail closed.

**Acceptance:** one file determines the build-time feature ceiling; schema explains what is and is not physically removed; no country-specific branches or runtime secrets are stored in it.

**Stop:** schema changes requiring a generic plugin framework are out of scope.

### Task F-02 - Generator and Dependency Validation

**Goal:** Generate deterministic, reviewable inputs for Backend, Flutter and Admin from F-01.

**Ownership / Files:** build-tooling owner exclusively owns one generator and generated-contract format; repository owners only own their generated adapters. Prefer an existing project scripting language/dependency.

**Implementation:** validate dependency closure and Base invariants; emit sorted registries plus manifest version/hash; `--check` detects stale outputs. Runtime-enabled values outside `compiled_features` are rejected or ignored fail closed with an explicit error.

**Tests:** golden/determinism test; cyclic/missing dependency; stale generated file; hash mismatch; Base-only/full-selected fixtures.

**Acceptance:** identical input is byte-stable; one command generates all contracts; one check command fails CI on drift before any product build.

**Stop:** do not add a template engine or build framework unless existing tools cannot emit the small static registries.

### Task F-03 - Backend Build, Route and Release Slicing

**Goal:** A disabled Backend feature is not registered or operational and, where OTP boundaries allow, its application modules/dependencies are absent from the release.

**Ownership / Files:** Backend owner; generated route/module allowlist, `imboy_router`, supervisors/workers and release configuration only. Preserve Handler -> Logic -> DS -> Repo.

**Implementation:** register REST/WS routes and workers from the compiled registry; enforce effective features server-side; exclude optional OTP applications/dependencies when separable. Do not rely on a Flutter/Admin flag.

**Tests:** raw REST/WS calls for disabled features return 404 or a stable feature-unavailable response; workers do not start; release inventory proves optional modules/dependencies absent where promised.

**Acceptance:** Base-only and selected-feature releases boot; enabled core flows pass; disabled routes/actions are unreachable; auth/RBAC/report/block gates remain enforced.

**Stop:** keep inseparable shared modules in Base and document the packaging ceiling instead of rewriting message/group/channel cores.

### Task F-04 - Flutter Compile-Graph Slicing

**Goal:** Build only Base screens plus selected feature screens and dependencies.

**Ownership / Files:** Flutter owner; generated `enabled_features.g.dart` or equivalent, route/page registry, feature entry imports and build script. Do not edit reserved `ios/*`, `macos/*` or `plugin/r_upgrade`.

**Implementation:** generated feature entrypoints must control imports and routes so disabled pages are absent from the Dart compilation graph. Keep route guards as defense in depth; do not claim runtime `if` statements are build slicing.

**Tests:** Base-only and selected builds; disabled deep links fail predictably; binary/symbol/import evidence demonstrates excluded feature roots; enabled navigation/API flows still work.

**Acceptance:** no static import from a Base entrypoint pulls a disabled feature into the build; optional permissions/assets/plugins are handled by F-06.

**Stop:** shared widgets remain shared; do not fork the application shell per feature combination.

### Task F-05 - Admin Module, Route and Menu Slicing

**Goal:** Admin contains Base operations plus management modules for compiled and runtime-enabled features only.

**Ownership / Files:** Admin owner; generated module registry, `App.tsx`, `Sidebar.tsx`, feature route wrappers and Vite build checks.

**Implementation:** generate route/menu entries and use generated or dynamic imports so disabled modules do not enter the Vite graph/chunks. Existing server capability checks remain authoritative at runtime. Admin cannot enable a feature absent from `compiled_features`.

**Tests:** Base-only and selected builds; menu/direct URL/API behavior; chunk/module manifest asserts disabled roots absent; hash mismatch fails visibly.

**Acceptance:** disabled feature has no menu, route or emitted chunk; enabled management remains protected by RBAC and Backend policy.

**Stop:** do not create a new Admin plugin runtime.

### Task F-06 - Optional SDK, Asset and Permission Slicing

**Goal:** Remove optional provider SDKs, assets and platform permissions when no compiled feature needs them.

**Ownership / Files:** each repository owner controls its dependency/build metadata; one coordinator owns the dependency-to-feature mapping. Exclusive edits per repository.

**Implementation:** derive only proven optional dependencies from F-00; preserve shared dependencies. Cover Flutter plugins/permissions/assets, Admin packages/chunks and Backend OTP applications/config providers.

**Tests:** dependency lock/build manifests, Android/iOS permission manifests, web chunks and Backend release application list for Base-only and selected builds.

**Acceptance:** optional SDK/permission/asset is absent when its last requiring feature is disabled; required security/network/storage foundations remain.

**Stop:** if package managers cannot conditionally resolve a dependency without multiple lockfiles or fragile rewrites, document it as retained shared build dependency and do not overpromise physical removal.

### Task F-07 - Three-Artifact Consistency and Build Matrix

**Goal:** Prevent Backend, Flutter and Admin feature drift.

**Ownership / Files:** release/CI owner; cross-repository check scripts and evidence ledger only after F-03..F-06 land.

**Implementation:** embed manifest schema version/hash and compiled feature list in each artifact; compare at build/deploy/startup. Test at least Base-only and full-selected presets. The matrix must accept an `overseas_baseline` preset when L-01 adds it later.

**Tests:** matching artifacts pass; stale hash, unsupported feature and Admin/App superset fail closed before release.

**Acceptance:** immutable evidence identifies manifest, three repository SHAs and artifact hashes; no matrix cell is PASS on skipped/pre-body failure.

### Task F-08 - Migration and Stored-Data Compatibility

**Goal:** Keep feature composition compatible with upgrades and existing installations without per-combination migration forks.

**Ownership / Files:** database/release owner; compatibility tests and runbook. Migrations change only when a proven data-lifecycle requirement exists.

**Implementation:** retain a schema superset by default; disabled features expose no route/worker. Define behavior for disabling a feature with existing data, re-enabling it, downgrade and retention/deletion jobs.

**Tests:** full -> reduced -> full profile against PostgreSQL; no data corruption, orphan worker or unauthorized access; retention/deletion remains active where policy requires.

**Acceptance:** disabling code does not silently delete data; privacy/retention duties are not disabled with the UI.

### Task F-09 - Packaging Contract and Operator Documentation

**Goal:** State exactly what a composed build includes and excludes.

**Ownership / Files:** product/release documentation owner under `imboy/docs`; no application files.

**Implementation:** supported features, immutable Base, manifest schema, build commands, output evidence, upgrade limits and known retained shared code/schema.

**Acceptance:** an operator can reproduce Base-only and selected builds from one manifest; claims distinguish absent business modules from retained shared infrastructure.

## Phase A: Launch Blockers

### Task L-01 - Build the Overseas Baseline Preset

**Goal:** Keep overseas v1 within ordinary IM/Workspace classification using the F-01 manifest contract.

**Inspect:** `src/lib/imboy_policy*.erl`, `src/lib/imboy_profile_preset.erl`, Flutter feature registry/route guards, Admin capability config.

**Modify:** add one `overseas_baseline` manifest preset after F-07. Default OFF: nearby people, public trending/discovery, live room, paid channel/wallet, AI marketplace/Bot external webhook unless separately accepted. Runtime policy may further disable compiled features but cannot add absent ones.

**Tests:** F-07 three-artifact build matrix plus Backend effective-policy, Flutter route/API visibility and Admin route/menu/chunk tests.

**Acceptance:** every disabled feature is absent from applicable artifacts, UI, direct route/deep link, REST and WebSocket action; manifest hashes match; core friend C2C/group/workspace/project/channel continues to work.

**Stop:** if a core domain is not safely separable, classify it as Base for this preset and record Architecture Gap; do not rewrite the core merely to shrink the artifact.

### Task D-01 — Account Deletion Request State

**Goal:** Represent when deletion was requested without relying on nonexistent `user.updated_at`.

**Files:** new migration in `priv/migrations/`; `user_logic.erl`, `user_ds.erl`, relevant repo/handler tests. Prefer a narrow `user_deletion_request` record over adding compliance fields throughout `user`.

**Implementation:** transactional request/cancel; explicit requested_at/status; idempotency; propagate DB failure; authenticated status endpoint. Keep configurable grace period.

**Tests:** request, duplicate, cancel, DB failure, invalid user, clock boundary, admin approve/reject concurrency on real PostgreSQL fixture.

**Acceptance:** request timestamp is queryable; API never returns success on rollback; no query references absent columns.

### Task D-02 — Data Disposition Manifest

**Goal:** Decide delete/anonymize/retain for every stored data class before coding cascade.

**Files:** `docs/compliance/data-disposition.yml` (or existing machine-readable governance location), schema validation test/script. No migration in this task.

**Implementation:** rows for account, identifiers, sessions/devices/tokens, social graph, messages, attachments, memberships/ownership, moments, reports/moderation/audit/security, payments, AI/Bot, logs, backups and each vendor. Fields: action, reason, retention, owner, deletion mechanism, legal-review status.

**Tests:** fail if a current `CREATE TABLE` is unmapped; allow explicit infrastructure/system exclusions with reason.

**Acceptance:** 100% applicable tables/providers mapped; no `TBD` for P0/P1 launch data; legal exceptions approved by owner/counsel.

### Task D-03 — Idempotent Deletion Orchestrator

**Goal:** Execute D-02 safely across relational data and asynchronous stores.

**Files:** `user_deletion_logic.erl`, a focused DS/repo module, supervisor/config, migration for job/tombstone if required, tests. Do not put all SQL back into `user_ds`.

**Implementation:** lock/claim request; ordered transactional DB delete/anonymize; transfer or close owned Workspace/Group/Channel/payment obligations per product decision; enqueue attachment/vendor deletion; revoke sessions immediately; retry with terminal status; retain only manifest-approved records.

**Tests:** seeded multi-domain PostgreSQL user; retry after partial external failure; ownership; payment balance; E2EE keys; object deletion mock plus one Garage integration; concurrent worker.

**Acceptance:** two runs produce same final state; no orphan/foreign-key failure; retained rows are anonymized and policy-backed; user cannot authenticate after request; status accurately reports pending/completed/failed.

### Task D-04 — App/Web Deletion Acceptance

**Goal:** Make Apple/Google deletion flows truthful and reviewable.

**Files:** existing Flutter logout page/API, static `/account-deletion`, status/confirmation UI, tests; store-console evidence checklist (no console mutation without explicit authorization).

**Implementation:** show grace period, retained categories/reasons, subscription/balance handling, completion/failure status; public web request or authenticated continuation matching Google requirements.

**Tests:** Android/iOS real-device flow, web link, offline/retry, reauthentication, cancellation during grace period, completion notification without exposing PII.

**Acceptance:** reviewer can find and initiate deletion in app; external URL works; seeded user reaches D-03 completed state; retained exceptions match notice.

### Task R-01 — First-class Report Targets and Evidence

**Goal:** Report the actual UGC object, especially messages.

**Files:** migration extending report model; `report_handler/logic/ds/repo`; Flutter report entry points; Admin report API/types/UI; tests.

**Implementation:** target kinds only for launch UGC; stable object ID/scope/author; structured evidence references; reason enum + optional text; duplicate and per-user rate control. Do not store message IDs in free text.

**E2EE:** reporter explicitly selects content; client submits minimum decrypted evidence and integrity/context metadata. Never grant server bulk decryption.

**Tests:** report user, C2C/C2G/channel message, group/channel/profile/avatar/file if enabled; deleted/edited target; duplicate, malicious flood, IDOR, E2EE evidence consent.

**Acceptance:** moderator resolves every enabled UGC type to a stable target/evidence record; unrelated content is inaccessible.

### Task R-02 — Moderation Case and Action Executor

**Goal:** Turn confirmed reports into small, auditable actions.

**Files:** minimal case/action migration and modules under existing ops governance; reuse group mute/kick, content delete and user status APIs; Admin UI/tests.

**Implementation:** `warning`, `content_removal`, scoped `mute`, `kick`, temporary/permanent account restriction only where existing primitives support them. Store actor, reason, policy, scope, start/end, result and reversal. Fail closed on unauthorized actions.

**Tests:** permission matrix; each action and expiry; duplicate/retry; reversal; target notification; action failure leaves case truthful.

**Acceptance:** confirmed violation results in recorded action or explicit no-action decision; status change alone is not enforcement; all accesses/actions are auditable.

### Task R-03 — Operational Moderation Queue

**Goal:** Make non-E2EE/public moderation queue real without scanning private E2EE plaintext.

**Files:** one policy entry point called by enabled profile/channel/moment/profile writes; existing moderation modules; Admin queue tests.

**Implementation:** deterministic keyword rules first; queue/quarantine semantics by content surface; no new AI provider. Define false-positive handling and SLA fields.

**Tests:** hit/no-hit, edits, Unicode normalization, approve/reject action, E2EE bypass to report-only model, queue failure policy.

**Acceptance:** supported non-E2EE UGC creates traceable decisions; E2EE message sends never expose plaintext to server moderation.

### Task R-04 — Appeal and Decision Notice (`LEGAL` scope gate)

**Goal:** Provide minimum user redress where product policy/DSA analysis requires it.

**Files:** case/action extension, user API/Flutter status page, Admin review page, tests.

**Implementation:** reasoned decision notice, one appeal, independent reviewer permission, final decision/reversal. Configure availability by policy profile, not country `if` statements.

**Tests:** eligible/ineligible, deadline, reviewer conflict, reversal, notification, privacy of reporter identity.

**Acceptance:** action -> notice -> appeal -> review -> final decision is fully auditable.

### Task B-01 — Shared Block Decision

**Goal:** Prevent direct contact bypass while preserving group/workspace semantics.

**Files:** reuse `user_denylist_ds/repo`; add one small safety decision module; call from friend, C2C, call, mention and invite boundaries; channel/profile/search only per approved policy.

**Implementation:** one directional/symmetric matrix approved by Product/Safety. Default: deny direct DM/call/friend request/direct mention/invite from blocked actor; shared-group content visibility remains a separate choice.

**Tests:** A blocks B and reverse direction across DM, call, friend, group/channel mention, group/channel/workspace invite, search/profile, existing shared group.

**Acceptance:** raw API/WS cannot bypass UI; DB failure behavior is explicitly fail-closed for direct contact; cache invalidates immediately.

### Task B-02 — Block UX Consistency

**Goal:** Match Flutter behavior and copy to B-01.

**Files:** existing denylist/contact/message/group/channel pages and i18n; focused widget/integration tests.

**Implementation:** show block state, unblock, predictable error, no claim that all shared content disappears unless B-01 policy says so.

**Acceptance:** two-account real-device matrix matches server responses and privacy copy.

### Task A-01 — Privileged Message Access Hardening

**Goal:** Eliminate unaudited broad private-message access.

**Files:** `adm_message_handler.erl`, `adm_acl`/role catalog, operation audit DS, policy defaults, Admin routes/UI, tests.

**Implementation:** default metadata mode; separate `messages:metadata:read`, `messages:content:read`, `messages:export`; require case/ticket + reason for content; append list/detail/export audit with actor/filter/result count; never reveal E2EE plaintext.

**Tests:** role matrix, missing reason/ticket, audit write failure, metadata/full modes, export, CSV injection regression, E2EE envelope handling.

**Acceptance:** Support cannot read content; Moderator sees only case-bound evidence; Security/Admin privileges are explicit; every content access/export has immutable audit.

### Task A-02 — Admin/Moderator Role Baseline

**Goal:** Seed least-privilege roles without replacing Workspace/Group roles.

**Files:** existing Admin RBAC seed/catalog, permission matrix UI, tests/docs.

**Implementation:** Platform Admin, Moderator, Security Admin, Support responsibilities; prohibit self-grant/escalation; sensitive export and IP access separate.

**Acceptance:** requested matrix answers who can report-review, user-data view, content removal, ban/unban, IP view, export, evidence, policy and audit logs; negative tests for every role.

## Phase B: Privacy and Release Evidence

### Task T-01 — Retention Policy Registry

**Goal:** One verifiable source for data-class retention.

**Files:** machine-readable policy under `docs/compliance` or existing config governance; validation script/tests; fix Loki comment/config mismatch.

**Implementation:** scope/profile/tenant override, duration/event/action/exception. Keep legal-hold unsupported unless counsel requires and designs it. Avoid per-country branches; policy profiles carry approved values.

**Acceptance:** messages, attachments, logs, audit/security/moderation, sessions/tokens, payments, backups, vendors all have owner-approved values; config renders or validates against policy.

### Task T-02 — Retention and Backup Enforcement

**Goal:** Prove expiration across DB, Garage, Loki and backups.

**Files:** narrowly scoped cleanup workers/scripts/config/runbooks/tests.

**Implementation:** bounded batches, locks, metrics, dry-run, retry, tombstones for backup/vendor propagation; no silent catch-as-success.

**Tests:** fake clock expiry/non-expiry/exception; Garage delete; Loki configured value; backup lifecycle; restore proves deleted data is not reintroduced without replaying tombstones.

**Acceptance:** evidence records counts and failures without PII; overdue deletion alerts; backup restore runbook includes deletion replay.

### Task P-01 — Complete User Export

**Goal:** Export the manifest-approved portable/access data without exposing secrets or other users.

**Files:** export logic/DS, async job/object delivery if needed, Flutter UI, tests.

**Implementation:** explicit field allowlists and categories; bounded async archive; encryption/expiry; ownership filtering; export audit. Scope determined by legal review, not a comment claiming GDPR completeness.

**Tests:** every category, other-user redaction, large dataset, expired link, concurrent/rate-limited request, schema drift.

**Acceptance:** D-02 categories marked export are present; secret/token/password/private keys absent; user can retrieve on real device.

### Task V-01 — Third-party and Data-flow Inventory

**Goal:** Bind actual build/runtime providers to disclosures.

**Files:** `docs/compliance/third-party-data-inventory.yml`, validation script, CI/release check; no SDK addition.

**Implementation:** provider, version, purpose, data fields, endpoint/region, optional/necessary, permission/consent, privacy URL, DPA/SCC status, retention, deletion API, build flavors. Cover FCM/APNs/Sentry/AMap/JPush/SMS/SMTP/LiveKit/TURN/payment/LLM/webhooks/CDN/object store.

**Acceptance:** every dependency/configured outbound host maps to an inventory row or explicit no-data rationale; unknown DPA/region blocks operated overseas release.

### Task V-02 — Logging and Crash Redaction

**Goal:** Prevent tokens, credentials, contact data, plaintext messages and report evidence from logs/Sentry.

**Files:** existing backend/Flutter log sinks and Sentry initialization; focused tests. Do not rewrite every call site unless sink cannot cover it.

**Implementation:** shared forbidden-key/value redaction; disable default breadcrumbs/request bodies as needed; environment-specific sampling and retention; safe actor IDs where necessary.

**Tests:** nested maps, headers, URLs, exceptions, E2EE plaintext/ciphertext, payment callbacks; assert raw secrets absent at sink.

**Acceptance:** one runnable regression corpus passes for backend and Flutter; provider-side settings captured as external evidence.

### Task V-03 — Privacy Notice and Consent Mapping (`LEGAL`)

**Goal:** Publish notices matching V-01/T-01/D-02 and actual launch profile.

**Files:** existing static/app privacy and terms documents, consent/permission UI only where optional processing requires it, version/acceptance record if counsel directs.

**Implementation:** distinguish necessary processing from optional analytics/location/personalization; no “accept all” for unrelated purposes; controller/contact values require explicit user confirmation before use.

**Acceptance:** app, web, Apple label, Google Data Safety and provider inventory reconcile mechanically; counsel approval recorded; no placeholder contact information.

### Task E-01 — E2EE Safety Contract

**Goal:** Document and test what server, admin, push, logs and report flows can see.

**Files:** existing E2EE policy docs/tests, message/report contracts. No E2EE rewrite.

**Implementation:** visibility matrix for C2C/C2G/attachments/multi-device/offline/backup/push; report evidence is explicit user disclosure; AI/moderation cannot receive E2EE plaintext by default.

**Acceptance:** Security/Product/Safety approve conflict resolution; tests fail if plaintext reaches server logs/admin/push in required mode.

### Task E-02 — E2EE Real-device Safety Acceptance

**Goal:** Validate two-account, multi-device behavior on Android+iOS.

**Tests:** encrypted C2C/C2G, offline, key rotation/revocation/recovery, attachment, block, report evidence, notification preview, admin metadata/content view.

**Acceptance:** device matrix has executed assertions and packet/log evidence; missing device/provider is BLOCKED, never PASS.

## Phase C: Conditional Launch Features

### Task C-01 — Attachment Safety

**Trigger:** file/image/video sharing remains enabled overseas.

**Implementation:** server-side MIME/size policy, quarantine lifecycle, hash, malware scanner adapter and URL safety rules. E2EE attachment scanning occurs only on explicit report evidence; no server decryption.

**Acceptance:** malicious/test file cannot become downloadable; scanner outage policy is explicit; object/delete retention is proven.

### Task C-02 — AI Identity and Safety

**Trigger:** AI Agent is enabled in launch profile.

**Implementation:** reuse account types; consistent AI badge in profile/chat/group/notifications/forwarding; AI output report target; provider inventory; E2EE exclusion.

**Acceptance:** user always knows the peer is automated before sending; can report output; no fake human presentation.

### Task C-03 — Paid Channel Store Billing (`LEGAL`)

**Trigger:** paid digital channel/subscription enabled in mobile apps.

**Implementation:** map product to current Apple/Google rules and approved regional exceptions; implement store billing only if required; price/period/renewal/cancel/refund/balance-close UI and tests.

**Acceptance:** written policy/legal decision, sandbox purchase/refund/cancel, account deletion balance handling, store reviewer evidence.

## Regional Decision Tasks

### Task J-01 — EU/EEA Legal Classification

**Deliverable:** counsel memo covering controller/processor split, GDPR lawful bases/rights/retention/transfers, Article 8 age handling, DSA service classification/exemptions/notice-action/redress/transparency. Output policy parameters, not code snippets.

### Task J-02 — US Legal Classification

**Deliverable:** launch-state/threshold analysis, COPPA audience/actual-knowledge decision, consumer/privacy/deletion duties and applicable intimate-image removal process. No universal under-13 branch without this decision.

### Task J-03 — Australia Legal Classification

**Deliverable:** Privacy Act/APP coverage, APP 8, IMBoy classification as relevant electronic service vs social media, applicable Class 1A/1B and age-restricted codes/standards and age-assurance duties as of launch date.

## Integrated Test Plan

| Suite | Required cases |
|---|---|
| Account | register/login/disable/delete/recovery; transaction failure and session revoke |
| Age | minor/adult/invalid/modification/restricted resource for each approved profile |
| Report | user/message/enabled UGC, duplicate/flood/deleted target/E2EE evidence/moderator processing |
| Block | DM/call/friend/group+channel mention/invite/search/profile/shared group |
| Moderation | warning/removal/mute/kick/temp/permanent restriction/expiry/reversal/appeal |
| Privacy | export/delete/anonymize/retain/object/vendor/backup/tombstone |
| Security | IDOR, CSRF/admin cookie, permission bypass, escalation, enumeration, rate limit, log redaction |
| Regional | EU/US/AU policy fixtures generated only from approved J-01..J-03 parameters |
| Store | real-device discoverability, report/block/delete, privacy links, Data Safety/label reconciliation |

## Compliance Release Gate

Run against a clean release candidate and attach immutable evidence:

```text
P0 = 0
P1 = 0
Critical security findings = 0

Feature manifest schema:                 PASS
Feature dependency closure:              PASS
Disabled Backend route/worker/module:     PASS
Disabled Flutter route/import root:       PASS
Disabled Admin route/menu/chunk:           PASS
Optional SDK/asset/permission inventory:  PASS
Three-artifact manifest hash match:       PASS
Base-only + full-selected build matrix:   PASS

Account:       PASS
Age:           PASS / N/A (with product + legal decision)
UGC:           PASS
Report:        PASS
Block:         PASS
Moderation:    PASS
Privacy:       PASS
Delete:        PASS
Export:        PASS / N/A (legal scope documented)
Retention:     PASS
E2EE:          PASS
Apple:         PASS / LEGAL REVIEW
Google Play:   PASS / LEGAL REVIEW
EU:            PASS / LEGAL REVIEW
US:            PASS / LEGAL REVIEW
Australia:     PASS / LEGAL REVIEW
```

Release outcome rules:

- `READY`: all applicable engineering/store gates PASS and legal reviews resolved.
- `CONDITIONAL`: no P0/P1, only explicitly disabled feature or jurisdiction conditions remain.
- `NO-GO`: any P0/P1, critical security finding, or required store gate fails.
- `NEEDS LEGAL REVIEW`: engineering evidence is complete but an applicability/interpretation decision remains.

Current result: **NO-GO**.
