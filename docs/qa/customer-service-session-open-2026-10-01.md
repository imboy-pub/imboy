# 客服排队会话事务修复 / Atomic session opening

English summary: Session opening previously committed the queued row before appending its audit. A real PostgreSQL audit failure left a queued row despite an error response, preventing a clean retry. Session creation now uses an event-aware production store operation that inserts, audits and reads back through one connection and transaction. This is local transaction evidence, not production qualification.

- Source base: `9e11027a9e0c770014eca0bc2f3df89ceaf27936`. Contract SHA256: `7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599`. CS-01 / CS-03 partial progress.
- Both production callers (customer_service_facade and Widget support) converge on cs_session_app:open_session. No interface handler or foreign cs_widget_handler change.
- Reuses existing elib_pg transaction, write_event_or_rollback, fetch_session_in and unique open-conversation constraint. Adds insert_session/4 consistently to behaviour, contract catalogue, production implementations and test fake; existing insert_session/3 remains for existing infrastructure callers/fixtures. No production application still calls the unaudited variant.
- Removed the unused application append_event wrapper after moving audit persistence into the transaction. No new dependencies or migration.

## 验证 / Checks

1. Baseline: current-source compiled backend against owned disposable PostgreSQL with every real migration. CHECK rejection of session.opened audit returned an error but real count found one queued row (expected zero). Baseline log binds the exact failed count.
2. Fixed: repeat same rejection → zero session and audit rows. Remove CHECK, retry → queued row and one event. Duplicate open → conflict, counts remain one. Two concurrent application opens on a second synthetic scope → one success, one conflict, one session and one audit. Uses actual production port/store and database, no persistence mock.
3. `IMBOY_DEPS_ROOT=/Users/leeyi/project/imboy.pub/imboy bash scripts/test/customer_service_internal_http_gate.sh`: exit 0, 8 top-level checks, including real 42-operation Internal HTTP conformance, invitation/membership/workspace journeys and new session checks. Final run `/tmp/imboy-seat-http.HvuY13`; baseline `/tmp/imboy-seat-http.bUOYc3`.
4. All product Erlang compiled from candidate source into the run's own beams; dependency application metadata reused from main. No shared application beams overwritten.
5. Isolated unit VM: cs_application_tests, cs_list_contract_tests and cs_closure_tests → 45 pass. Unit beams and logs `/tmp/gz-session-open-unit-sjad8961`. Verifies fake application orchestration, port/implementation consistency and feature boundaries; does not substitute for real DB.
6. Manual source review only; no independent reviewer agent claimed. Existing tenant keys and conflict mapping preserved. No external notification service started; owned marker database and container removed by the existing harness cleanup.

Evidence: evidence/customer-service-session-open-2026-10-01/ with sha256.txt.

## 未完成 / Outstanding

No real Widget browser → Seat UI → attachment → transfer → close journey, SSE/device or production gate is established by this change. It fixes the queued-session audit transaction; other Widget orchestration and organization lifecycle concurrency require their own evidence. Six-goal acceptance remains incomplete.
