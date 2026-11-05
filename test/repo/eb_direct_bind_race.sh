#!/usr/bin/env bash
set -euo pipefail
umask 077
: "${EB_DIRECT_TEST_HOST:?}" "${EB_DIRECT_TEST_PORT:?}" "${EB_DIRECT_TEST_USER:?}" "${EB_DIRECT_TEST_DB:?}"
case "$EB_DIRECT_TEST_DB" in c08_eb_real_*) ;; *) echo 'requires an isolated real EB fixture' >&2; exit 2;; esac
args=(-h "$EB_DIRECT_TEST_HOST" -p "$EB_DIRECT_TEST_PORT" -U "$EB_DIRECT_TEST_USER" -d "$EB_DIRECT_TEST_DB" -X -q -v ON_ERROR_STOP=1)
logs=$(mktemp -d "${TMPDIR:-/tmp}/eb-direct-bind-race.XXXXXXXX")
subject="jsonb_build_object('kind','seat','principal','99006100','org_id','99006101')"
fence="(SELECT winner_fence FROM enterprise_asset_direct_link WHERE asset_id=99006105)"
run_race() {
    local tag="$1" blocker_sql="$2" expiry="$3" a="eb_bind_${1}_blocker" b="eb_bind_${1}_caller" ready=0 blocked=0
    PGAPPNAME="$a" psql "${args[@]}" -c "BEGIN; $blocker_sql; SELECT pg_sleep(5); COMMIT" > "$logs/$tag-blocker.log" 2>&1 &
    local blocker=$!
    for _ in {1..50}; do
        [[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM pg_stat_activity WHERE application_name='$a' AND wait_event='PgSleep'")" = 1 ]] && { ready=1; break; }
        sleep 0.1
    done
    [[ "$ready" = 1 ]] || { wait "$blocker" || true; echo 'blocker did not acquire lock' >&2; return 1; }
    PGAPPNAME="$b" psql "${args[@]}" -c "SELECT fn_eb_direct_bind(99006101,99006102,99006105,$subject,$fence,repeat('a',64),repeat('b',64),$expiry)" > "$logs/$tag-caller.log" 2>&1 &
    local caller=$!
    for _ in {1..50}; do
        [[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM pg_stat_activity x JOIN pg_stat_activity y ON y.pid=ANY(pg_blocking_pids(x.pid)) WHERE x.application_name='$b' AND y.application_name='$a'")" = 1 ]] && { blocked=1; break; }
        sleep 0.1
    done
    wait "$blocker"
    if wait "$caller"; then echo 'unauthorized binding unexpectedly succeeded' >&2; return 1; fi
    [[ "$blocked" = 1 ]] || { echo 'caller did not wait for blocker' >&2; return 1; }
    grep -q "$4" "$logs/$tag-caller.log"
}
run_race expiry "SELECT id FROM workspace WHERE id=99006102 FOR UPDATE" "extract(epoch FROM clock_timestamp())::bigint+2" 'enterprise direct scan expired'
run_race revoke "UPDATE organization_business_identity_assignment SET status='ended',ended_at=now() WHERE id=99006108" "extract(epoch FROM clock_timestamp())::bigint+600" 'enterprise direct subject unauthorized'
psql "${args[@]}" -c "UPDATE organization_business_identity_assignment SET status='active',ended_at=NULL WHERE id=99006108"
PGAPPNAME=eb_bind_gc_blocker psql "${args[@]}" -c "BEGIN; SELECT fn_eb_direct_bind(99006101,99006102,99006105,$subject,$fence,repeat('a',64),repeat('b',64),extract(epoch FROM clock_timestamp())::bigint+600); SELECT pg_sleep(5); COMMIT" > "$logs/gc-blocker.log" 2>&1 &
blocker=$!
ready=0
for _ in {1..50}; do
    [[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM pg_stat_activity WHERE application_name='eb_bind_gc_blocker' AND wait_event='PgSleep'")" = 1 ]] && { ready=1; break; }
    sleep 0.1
done
[[ "$ready" = 1 ]] || { wait "$blocker" || true; echo 'bind did not acquire attachment lock' >&2; exit 1; }
PGAPPNAME=eb_bind_gc_writer psql "${args[@]}" -c "UPDATE attachment SET lifecycle_state='delete_intent' WHERE id=(SELECT confirmed_attachment_id FROM attach_pending WHERE object_key=(SELECT object_key FROM enterprise_asset_direct_link WHERE asset_id=99006105))" > "$logs/gc-writer.log" 2>&1 &
writer=$!
blocked=0
for _ in {1..50}; do
    [[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM pg_stat_activity x JOIN pg_stat_activity y ON y.pid=ANY(pg_blocking_pids(x.pid)) WHERE x.application_name='eb_bind_gc_writer' AND y.application_name='eb_bind_gc_blocker'")" = 1 ]] && { blocked=1; break; }
    sleep 0.1
done
wait "$blocker"
if wait "$writer"; then echo 'GC unexpectedly deleted a bound attachment' >&2; exit 1; fi
[[ "$blocked" = 1 ]] || { echo 'GC did not wait for binding lock' >&2; exit 1; }
grep -q 'enterprise direct reference still live' "$logs/gc-writer.log"
[[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM enterprise_asset e JOIN enterprise_asset_direct_link l ON l.asset_id=e.id WHERE e.id=99006105 AND e.status='active' AND l.binding_state='bound'")" = 1 ]]
echo "receipt expiry, revocation and GC races passed; fixture retained; logs: $logs"
