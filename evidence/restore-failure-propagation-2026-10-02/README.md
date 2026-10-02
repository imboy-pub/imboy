# Restore failure propagation and isolated clone

Restore/list/TimescaleDB post_restore errors now fail the real restore and drill entrypoints. EXIT preserves the original status before cleanup and emits a failure metric of 0.

bash scripts/test/restore_failure_test.sh: before 2 PASS / 6 FAIL; after 8 PASS. Local Docker/curl doubles exercise actual script entrypoints without network/database access. bash scripts/test/restore_guard_test.sh: 43 PASS. bash -n and shellcheck -x: all three affected scripts pass. Independent read-only review: APPROVE.

A disposable PostgreSQL 18/TimescaleDB container was actually dumped with pg_dump -Fc and restored through restore_pg.sh into a separate clone. Synthetic attachment metadata, enterprise file links and hypertable messages match exactly; timescaledb.restoring is off; exit 0. Source hashes, data hashes and replay driver are archived. Attempt 1 failed fixture startup due to vendor image init hooks; preserved separately. The successful retry uses the existing empty-init-directory fixture pattern.

This proves synthetic metadata/message restoration, not production recovery, Garage object consistency, historical ciphertext or RPO/RTO. The earlier full backend pair remains bound to a3cb1386 and does not qualify these new script changes.
