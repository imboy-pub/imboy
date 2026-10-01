# 坐席停用与消息提交 / Seat suspension during send

English summary: Per-request Seat authentication already rejects a disabled seat, but an in-flight canonical message could still commit after suspension. The existing transaction now reads enabled under a shared Seat row lock before locking the session; disabled Seat writes roll back. Visitors can continue leaving messages. No new schema, dependency or public route.

- Base `ce855b6e0abf75783c9b7a2e54af69ac7eb409e9`; contract SHA256 `7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599`. CS-02 partial progress.
- Reuses message audit hook and canonical rollback. KEY SHARE did not conflict with enabled updates; FOR SHARE does while permitting concurrent message readers. Seat-before-session order remains consistent with claim/transfer. actor_kind is server-derived; only Seat messages reject disabled state. seat_disabled propagates through existing 403 mapping.
- Baseline `/tmp/imboy-seat-http.pY1YUg`: actual claim/suspend then application send returned accepted=true; assertion failed (EUnit XML represents this as error). Synthetic baseline retained.
- Fixed `/tmp/imboy-seat-http.g86Iis`: actual claim/suspend rejects outbound with zero message/audit rows; visitor send succeeds; actual resume permits outbound. Controlled row-lock interleaving commits disabled Seat while canonical writer waits (pg_blocking_pids verified), then rejects and rolls back. The race uses a fixture UPDATE, not concurrent suspension HTTP.
- Runnable gate: `IMBOY_DEPS_ROOT=/Users/leeyi/project/imboy.pub/imboy bash scripts/test/customer_service_internal_http_gate.sh`, exit 0. Native XML proves 23 real DB checks, failures/errors/skipped all zero; eight top-level checks include existing 42-operation Internal HTTP conformance. All candidate product source compiled. Source hashes bind changed files. Notifications local no-op; owned disposable synthetic database only.
- erlfmt/diff checks pass. Manual source review only; no independent reviewer claimed. Evidence hashes in evidence/customer-service-seat-suspension-2026-10-01/sha256.txt.

This is local persistence evidence, not real Seat browser/JWT/SSE or production proof. Concurrent assignment/member revocation, full Widget→Seat→attachment journey, frozen three-repository gate and all six-goal acceptance remain incomplete.
