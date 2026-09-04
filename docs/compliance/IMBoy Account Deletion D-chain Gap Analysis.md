# IMBoy Account Deletion D-chain Gap Analysis

Date: 2026-09-05
Scope: current-HEAD inspection (imboy `9d18ebad`) of the account-deletion chain against
`IMBoy Overseas Compliance Implementation Plan.md` Tasks D-01, D-02, D-03, D-04
(Phase A launch blockers, dependency order D-01 → D-02 → D-03 → D-04).

## Existing inventory (what is already built)

| Area | Asset | Location |
|---|---|---|
| User apply/cancel | `/api/v1/user/apply_logout`, `/api/v1/user/cancel_logout` routes; `user_logic:apply_logout/2` (tx: set status=2 + logout-apply log), `user_logic:cancel_logout/2` (set status=1) | `src/imboy_router.erl:128-129`, `src/logic/user_logic.erl:110-135` |
| Admin review | `/api/adm/user/logout_apply/{list,export,reject,approve}`; approve = status 2→-1 (已注销), reject = 2→1, both guarded `WHERE status = 2` (atomic) | `src/imboy_router.erl:841-844`, `src/ds/user_ds.erl:498-516` |
| Web page | static `/account-deletion` page, whitelisted for anonymous access | `src/imboy_router.erl:35-36,1104` |
| Auto-sweeper | `user_deletion_logic` gen_server: scans status=2 past retention grace, deletes; default disabled; config `user_deletion_{enabled,interval,retention_days,batch_size}` | `src/logic/user_deletion_logic.erl` |
| Deletion executor | `user_ds:delete_all_related_data/1,2` — one transaction, dependency-ordered hard delete over ~20 tables incl. sessions (`user_token`), devices, friends, groups (owner+member), E2EE backups + Olm identity/OTK/fallback keys, user row last | `src/ds/user_ds.erl:235-300` |
| Pre-deletion export | `user_ds:export_data/1` via `user_deletion_logic:export_user_data/1`; admin export endpoint | `src/logic/user_deletion_logic.erl:66`, `src/ds/user_ds.erl` |
| App page | 注销页 `lib/page/mine/logout_account/` (Flutter) | imboyapp |
| Tests | `test/logic/user_deletion_logic_tests.erl`, `test/logic/user_export_logic_tests.erl` | imboy test tree |

## Confirmed blocking bug

**`find_expired_logout_users/2` references a nonexistent column.** The sweep SQL reads
`WHERE status = 2 AND updated_at <= NOW() - ...` (`src/ds/user_ds.erl:481-491`), but the
`user` table has no `updated_at` column — the repo itself documents this one function
earlier (`reject_logout_apply` comment: "user 表无 updated_at 列（只有 created_at），不能写该字段").
Consequences: the sweep query fails at runtime every cycle, `delete_expired_users/2`
catches and returns 0, and **automatic post-grace-period deletion has never executed**.
This is exactly the failure mode D-01's goal statement targets ("without relying on
nonexistent `user.updated_at`").

## Gap list per task

### D-01 — Account Deletion Request State (partially met, blocking bug)

Missing vs spec:
- No narrow `user_deletion_request` record; request state lives as `user.status = 2`
  plus an audit log row. No queryable `requested_at` anywhere (the broken `updated_at`
  reference is the symptom). Needs new migration + record.
- `apply_logout` is not idempotent: re-apply while status=2 rewrites status and appends
  another log row instead of returning the existing request.
- No authenticated **status** endpoint (spec: "authenticated status endpoint"); UI cannot
  show pending/grace state from the server.
- Cancel path exists but has no request-record backing (nothing to cancel but the flag);
  no transactional cancel+log pairing symmetry with apply.

Already met: transactional apply (tx wraps status+log); configurable grace period
(`user_deletion_retention_days`); admin approve/reject concurrency guard (`WHERE status = 2`).

### D-02 — Data Disposition Manifest (not started)

- `docs/compliance/data-disposition.yml` does not exist; no schema-validation
  test/script; no delete/anonymize/retain mapping for current tables/providers.
- Note for the mapping exercise: `delete_all_related_data` covers ~20 tables but the
  schema contains many more (messages/conversation tables, attachments/Garage objects,
  workspace/organization/project tables, wallet/payment tables, report/moderation,
  audit/security logs). The trust_audit retention-vs-erasure decision is already
  flagged in code as a product decision (`src/ds/user_ds.erl` comment, gap-matrix E1)
  and belongs in D-02.

### D-03 — Idempotent Deletion Orchestrator (v1 exists, spec gaps)

Present: single-transaction dependency-ordered delete (atomic per user — partial
failure rolls back; sweeper retries next cycle), immediate session revocation via
`user_token` delete, E2EE key material cleanup, graceful "already gone" table skips.

Missing vs spec:
- No lock/claim on the request (multi-node sweep could double-claim; single tx makes it
  safe today but the claim model is required for external/async steps).
- No job/tombstone migration (spec: "migration for job/tombstone if required") — no
  per-run record, no terminal failed/completed status, no retry backoff.
- No attachment/vendor deletion: Garage S3 objects for the user's attachments are not
  enqueued or removed (spec: "enqueue attachment/vendor deletion", "one Garage
  integration" test).
- No ownership transfer/close for owned Workspace/Group/Channel (spec: "per product
  decision"); current code hard-deletes owned group rows, which orphans members'
  conversations rather than transferring ownership.
- No wallet/payment balance handling (spec test: "payment balance").
- No anonymize path: everything is hard delete; "retained rows are anonymized and
  policy-backed" has no implementation surface yet (depends on D-02 decisions).
- Messages/conversation data disposition undefined (neither deleted nor anonymized in
  current executor).

### D-04 — App/Web Deletion Acceptance (partially met)

Present: static `/account-deletion` page (anonymous-whitelisted, matches the "external
URL" requirement shape); Flutter 注销页; apply/cancel API.

Missing vs spec:
- No server-backed deletion status surfaced in UI (depends on D-01 status endpoint);
  no completion/failure state, no grace-period countdown.
- Retained categories/reasons and subscription/balance handling not displayed (depends
  on D-02/D-03 outcomes).
- Public web request flow (Google requirement: web-initiated deletion or authenticated
  continuation) — the static page is informational; no web request intake.
- Acceptance evidence not executed: Android/iOS real-device flow, web link, offline/
  retry, reauthentication, cancel during grace, completion notification without PII.

## Recommended execution order

1. **D-01**: migration `user_deletion_request` (id, user_id unique, requested_at,
   status, grace ref); make apply/cancel idempotent + tx; add authenticated status
   endpoint; fix the sweep SQL to read `requested_at` from the new record (unblocks
   the sweeper); keep admin guards working off the request state. Tests on real PG:
   request, duplicate, cancel, DB failure, invalid user, clock boundary, admin
   approve/reject concurrency.
2. **D-02**: author `data-disposition.yml` from current schema inventory + validation
   test (fail on unmapped `CREATE TABLE`); product decisions needed for trust_audit,
   messages, workspace ownership, wallet — these are owner decisions, surface them.
3. **D-03**: rework orchestrator onto the request record (claim/lock, job/tombstone,
   retry/terminal status), pluggable disposition executors per D-02 mapping, Garage
   enqueue + one integration test, ownership/wallet policy hooks.
4. **D-04**: status/confirmation UI, retained-categories notice, web intake, real-device
   acceptance + store-console evidence checklist.

Chain head is D-01; it also independently repairs the broken sweeper.
