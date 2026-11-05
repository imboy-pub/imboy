#!/usr/bin/env bash
set -euo pipefail
umask 077
: "${EB_DIRECT_TEST_HOST:?}" "${EB_DIRECT_TEST_PORT:?}" "${EB_DIRECT_TEST_USER:?}" "${EB_DIRECT_TEST_DB:?}"
case "$EB_DIRECT_TEST_DB" in c08_eb_retire_proof_*) ;; *) echo 'requires an isolated proof fixture' >&2; exit 2;; esac
args=(-h "$EB_DIRECT_TEST_HOST" -p "$EB_DIRECT_TEST_PORT" -U "$EB_DIRECT_TEST_USER" -d "$EB_DIRECT_TEST_DB" -X -q -v ON_ERROR_STOP=1)
logs=$(mktemp -d "${TMPDIR:-/tmp}/eb-direct-retire-proof.XXXXXXXX")
PGAPPNAME=eb_retire_delete psql "${args[@]}" > "$logs/retire.log" 2>&1 <<'SQL' &
BEGIN;
SET LOCAL imboy.enterprise_purge='on';
UPDATE attach_pending SET upload_expires_at=clock_timestamp()-interval '1 hour'
 WHERE object_key=(SELECT object_key FROM enterprise_asset_direct_link WHERE binding_state='bound' LIMIT 1);
UPDATE attachment SET gc_verified=true
 WHERE path=(SELECT object_key FROM enterprise_asset_direct_link WHERE binding_state='bound' LIMIT 1);
SELECT fn_eb_direct_retire(organization_id,workspace_id,asset_id)
 FROM enterprise_asset_direct_link WHERE binding_state='bound' LIMIT 1;
DELETE FROM enterprise_asset WHERE id=(SELECT asset_id FROM enterprise_direct_retirement_audit LIMIT 1);
SELECT pg_sleep(5);
ROLLBACK;
SQL
retire=$!
ready=0
for _ in {1..50}; do
    [[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM pg_stat_activity WHERE application_name='eb_retire_delete' AND wait_event='PgSleep'")" = 1 ]] && { ready=1; break; }
    sleep 0.1
done
[[ "$ready" = 1 ]] || { wait "$retire" || true; echo 'retirement did not reach delete' >&2; exit 1; }
PGAPPNAME=eb_retire_proof_insert psql "${args[@]}" > "$logs/proof.log" 2>&1 <<'SQL' &
BEGIN;
INSERT INTO enterprise_direct_scan_proof(asset_id,object_key,winner_fence,receipt_sha256,ruleset_sha256,expires_at)
SELECT asset_id,object_key,winner_fence,scan_receipt_sha256,scan_ruleset_digest,clock_timestamp()+interval '1 minute'
 FROM enterprise_asset_direct_link WHERE binding_state='bound' LIMIT 1;
ROLLBACK;
SQL
proof=$!
blocked=0
for _ in {1..50}; do
    [[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM pg_stat_activity x JOIN pg_stat_activity y ON y.pid=ANY(pg_blocking_pids(x.pid)) WHERE x.application_name='eb_retire_proof_insert' AND y.application_name='eb_retire_delete'")" = 1 ]] && { blocked=1; break; }
    sleep 0.1
done
wait "$retire"
wait "$proof"
[[ "$blocked" = 1 ]] || { echo 'proof insert did not wait for asset deletion' >&2; exit 1; }
[[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM enterprise_asset_direct_link WHERE binding_state='bound'")" = 1 ]] || { echo 'fixture was not preserved' >&2; exit 1; }
echo "retirement/proof FK lock race passed; fixture retained; logs: $logs"
