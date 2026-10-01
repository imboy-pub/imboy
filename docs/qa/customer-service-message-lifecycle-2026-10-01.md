# 客服消息与结束并发 / Session message lifecycle

English summary: Visitor messages could be accepted after a session was closed. The shared sender guard now rejects closed sessions, and the existing canonical transaction rechecks the locked current session before its message audit commits. A close or assignment change rolls back message, policy and audit together. No new dependency, schema or public API.

- Base: `acd99003a2a23e6d26f415079bc0ad99eaaddb6b`. Contract SHA256: `7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599`. CS-01 / CS-02 partial progress.
- Widget, visitor and Seat message callers converge on cs_session_app. No interface handler edited; foreign main Widget work remains untouched.
- Production message.appended hook locks Seat with KEY SHARE before session FOR UPDATE, matching existing claim/transfer order and avoiding reversal through the event FK. Tenant/workspace/session keys remain parameterized. Assignment mismatch returns conflict for retry; closed returns session_already_closed through the existing HTTP mapping.
- Reuses cs_pg_tests existing real-store journey instead of duplicating 18 cases; adds three lifecycle checks and native EUnit XML reporting to the owned marker gate. Manual source review only; no independent reviewer claimed.

## 验证 / Checks

1. Baseline real DB: actual close then actual visitor message returned accepted=true rather than session_already_closed. Evidence retains the failed assertion with synthetic fixture IDs. The first report harness also had an unrelated unnamed-suite error, resolved using a named EUnit group.
2. Fixed real DB: actual close then message is rejected with zero message, enterprise audit and message.appended rows.
3. Controlled concurrency: a real SQL transaction holds the session while an actual canonical message writer reads the old state; pg_blocking_pids proves the writer is waiting. Commit closed or active/assigned state, then observe respectively closed/conflict and zero message/audit rows. After assignment conflict, retry succeeds exactly once. These two interleavings use controlled fixture row transitions, not concurrent real close/claim HTTP commands.
4. `IMBOY_DEPS_ROOT=/Users/leeyi/project/imboy.pub/imboy bash scripts/test/customer_service_internal_http_gate.sh`: exit 0, run `/tmp/imboy-seat-http.UKjpRE`. Eight top-level checks include real 42-operation Internal HTTP conformance and existing organization/workspace checks. Native XML independently proves 21 customer-service real DB cases, no skipped tests. All product source compiled to isolated beams; dependency metadata reused from main.
5. Isolated unit VM `/tmp/gz-message-lifecycle-unit-woy2uefl`: cs_application_tests, cs_list_contract_tests and cs_closure_tests, 45 pass. Formatting applied after the real gate; unit helpers compiled after formatting. Formatting and diff checks are recorded separately and do not imply another DB run.
6. Evidence is synthetic, owned disposable database only; notifications are local no-op. Evidence hashes in evidence/customer-service-message-lifecycle-2026-10-01/sha256.txt.

## 未完成 / Outstanding

This proves local lifecycle persistence and selected real-store cases. It does not prove real Widget browser → Seat UI → asset → transfer → close, revoked Seat HTTP authority, SSE/device behavior, complete frozen three-repository gate or production readiness. All six goals remain active.
