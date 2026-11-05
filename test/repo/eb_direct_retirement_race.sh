#!/usr/bin/env bash
set -euo pipefail
umask 077
: "${EB_DIRECT_TEST_HOST:?}" "${EB_DIRECT_TEST_PORT:?}" "${EB_DIRECT_TEST_USER:?}" "${EB_DIRECT_TEST_DB:?}"
case "$EB_DIRECT_TEST_DB" in c08_eb_retire_*) ;; *) echo 'requires an isolated retirement fixture' >&2; exit 2;; esac
args=(-h "$EB_DIRECT_TEST_HOST" -p "$EB_DIRECT_TEST_PORT" -U "$EB_DIRECT_TEST_USER" -d "$EB_DIRECT_TEST_DB" -X -q -v ON_ERROR_STOP=1)
logs=$(mktemp -d "${TMPDIR:-/tmp}/eb-direct-retire-race.XXXXXXXX")
PGAPPNAME=eb_retire_owner psql "${args[@]}" > "$logs/retire.log" 2>&1 <<'SQL' &
BEGIN;
SET LOCAL imboy.enterprise_purge = 'on';
UPDATE attach_pending SET upload_expires_at=clock_timestamp()-interval '1 hour'
 WHERE object_key=(SELECT object_key FROM enterprise_asset_direct_link WHERE binding_state='bound' LIMIT 1);
UPDATE attachment SET gc_verified=true
 WHERE path=(SELECT object_key FROM enterprise_asset_direct_link WHERE binding_state='bound' LIMIT 1);
SELECT fn_eb_direct_retire(organization_id,workspace_id,asset_id)
 FROM enterprise_asset_direct_link WHERE binding_state='bound' LIMIT 1;
SELECT pg_sleep(5);
ROLLBACK;
SQL
owner=$!
ready=0
for _ in {1..50}; do
    [[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM pg_stat_activity WHERE application_name='eb_retire_owner' AND wait_event='PgSleep'")" = 1 ]] && { ready=1; break; }
    sleep 0.1
done
[[ "$ready" = 1 ]] || { wait "$owner" || true; echo 'retirement did not acquire locks' >&2; exit 1; }
PGAPPNAME=eb_retire_hold psql "${args[@]}" > "$logs/hold.log" 2>&1 <<'SQL' &
BEGIN;
INSERT INTO enterprise_retention_hold(id,organization_id,workspace_id,scope_type,reason_code)
SELECT (SELECT coalesce(max(id),0)+1 FROM enterprise_retention_hold),organization_id,workspace_id,'workspace','test'
 FROM enterprise_asset_direct_link WHERE binding_state='bound' LIMIT 1;
ROLLBACK;
SQL
hold=$!
blocked=0
for _ in {1..50}; do
    [[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM pg_stat_activity x JOIN pg_stat_activity y ON y.pid=ANY(pg_blocking_pids(x.pid)) WHERE x.application_name='eb_retire_hold' AND y.application_name='eb_retire_owner'")" = 1 ]] && { blocked=1; break; }
    sleep 0.1
done
wait "$owner"
wait "$hold"
[[ "$blocked" = 1 ]] || { echo 'hold did not wait for retirement workspace lock' >&2; exit 1; }
[[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM enterprise_asset_direct_link WHERE binding_state='bound'")" = 1 ]] || { echo 'fixture was not preserved' >&2; exit 1; }
echo "retirement/hold lock race passed; fixture retained; logs: $logs"
