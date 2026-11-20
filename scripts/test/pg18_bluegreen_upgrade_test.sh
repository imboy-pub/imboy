#!/usr/bin/env bash
# Isolated synthetic fault injection. No Docker daemon, network or database.
# Requires Linux, Bash and GNU coreutils; no third-party test framework.
# Usage: bash scripts/test/pg18_bluegreen_upgrade_test.sh [script-under-test]
# TEST_FILTER=rsync_failure TEST_OUTPUT_DIR=/absolute/parent may select/retain runs.
# Every run retains its fake data, command journal and stdout/stderr; no cleanup.
set -euo pipefail
test_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
subject=${1:-"$test_dir/../pg18_bluegreen_upgrade.sh"}
subject="$(cd "$(dirname "$subject")" && pwd)/$(basename "$subject")"
mock="$test_dir/fixtures/pg18_bluegreen/mock_command.sh"
[ -f "$subject" ] && [ -f "$mock" ] || { echo 'Missing script or fixture' >&2; exit 2; }
output_parent=${TEST_OUTPUT_DIR:-${TMPDIR:-/tmp}}
mkdir -p "$output_parent"
suite_root=$(mktemp -d "$output_parent/pg18-bg-test.XXXXXXXX")
printf 'Artifacts: %s\nScript: %s\n' "$suite_root" "$subject"
cp "$subject" "$suite_root/script-under-test.sh"
sha256sum "$suite_root/script-under-test.sh" > "$suite_root/subject.sha256"
sha256sum "${BASH_SOURCE[0]}" "$mock" > "$suite_root/harness.sha256"
cp "${BASH_SOURCE[0]}" "$suite_root/test-driver.sh"
cp "$mock" "$suite_root/mock-command.sh"
mock="$suite_root/mock-command.sh"
/bin/bash --version > "$suite_root/bash-version.txt"
printf 'case\tresult\texit_code\treason\n' > "$suite_root/results.tsv"
passed=0; failed=0; case_index=0
base_epoch=$(/usr/bin/date +%s)
printf 'started_utc=%s\n' "$(/usr/bin/date -u +%Y-%m-%dT%H:%M:%SZ)" > "$suite_root/timing.txt"

has() { grep -Eq "$1" "$case_root/events.log"; }
missing() { ! has "$1"; }
check() { if ! "$@"; then reasons+=("$*"); fi; }
old_data() { [ -f "$case_root/data/pg18/sentinel" ] && [ "$(cat "$case_root/data/pg18/sentinel")" = OLD ]; }
old_online() { old_data && [ "$(cat "$case_root/model/image")" = test/old:18 ] && [ "$(cat "$case_root/model/pg")" = 1 ]; }
backend_off() { [ "$(cat "$case_root/model/backend")" = 0 ]; }
pg_off() { [ "$(cat "$case_root/model/pg")" = 0 ]; }
no_success() { ! grep -Eq '已回滚到旧镜像|尚未换目录，旧容器\+backend 已恢复|已回滚。|cutover 完成|cutover 失败已自动回滚' "$case_root/output.log"; }
state_zero() { [ -f "$case_root/data/pg18_upgrade.state" ] && grep -Eq '^SWAPPED=(0|.0.)$' "$case_root/data/pg18_upgrade.state"; }
phase_is() { grep -Eq "^PHASE=.?$1.?$" "$case_root/data/pg18_upgrade.state"; }
recovery_order() {
  # Any backend start must follow PG readiness for that same image. Rollback to
  # old must never start backend on new, even though the new PG still runs.
  missing '^UNSAFE_BACKEND_START$' && missing '^backend:start:new:' &&
    awk '
      /^pg:up:new$/ {new_pg=1; backend_stopped=0}
      /^backend:stop$/ {if (new_pg) backend_stopped=1}
      /^pg:stop$/ {if (new_pg && !backend_stopped) bad=1}
      END {exit bad}
    ' "$case_root/events.log"
}
snapshot_intact() { [ -f "$case_root/data/pg18.$case_ts/sentinel" ] && [ "$(cat "$case_root/data/pg18.$case_ts/sentinel")" = OLD ]; }
failed_data_retained() { [ -f "$case_root/data/pg18.failed_$case_ts/sentinel" ] && [ "$(cat "$case_root/data/pg18.failed_$case_ts/sentinel")" = NEW ]; }

run_case() {
  local scenario=$1 command=${2:-cutover}
  if [ -n "${TEST_FILTER:-}" ] && [[ "$scenario" != *"$TEST_FILTER"* ]]; then return; fi
  case_index=$((case_index+1))
  case_root="$suite_root/$scenario"
  case_epoch=$((base_epoch + case_index))
  case_ts=$(/usr/bin/date -u -d "@$case_epoch" +%Y%m%dT%H%M%S)
  # The baseline hard-codes /tmp guard/counts names. Never reuse or delete an
  # earlier run's files; choose another synthetic timestamp if either exists.
  while [ -e "/tmp/pg18_bg_rb_${case_ts}.flag" ] || [ -e "/tmp/pgbg_counts_old.$case_ts" ]; do
    case_epoch=$((case_epoch+1))
    case_ts=$(/usr/bin/date -u -d "@$case_epoch" +%Y%m%dT%H%M%S)
  done
  mkdir -p "$case_root"/{model,bin,repo/scripts,data/pg18,tmp}
  cp "$subject" "$case_root/repo/scripts/pg18_bluegreen_upgrade.sh"
  cat > "$case_root/repo/scripts/backup_pg.sh" <<'BACKUP'
#!/usr/bin/env bash
printf 'backup:run\n' >> "$MOCK_ROOT/events.log"
if [ "$MOCK_SCENARIO" = all_backup_failure ]; then
  printf 'fault:backup\n' >> "$MOCK_ROOT/events.log"; exit 23
fi
BACKUP
  cat > "$case_root/repo/scripts/restore_smoke.sh" <<'RESTORE'
#!/usr/bin/env bash
printf 'restore_smoke:run\n' >> "$MOCK_ROOT/events.log"
if [ "$MOCK_SCENARIO" = all_restore_failure ]; then
  printf 'fault:restore_smoke\n' >> "$MOCK_ROOT/events.log"; exit 23
fi
RESTORE
  printf 'OLD\n' > "$case_root/data/pg18/sentinel"
  printf '1\n' > "$case_root/model/exists"
  printf '1\n' > "$case_root/model/pg"
  printf '1\n' > "$case_root/model/backend"
  printf 'old\n' > "$case_root/model/ready"
  printf 'test/old:18\n' > "$case_root/model/image"
  : > "$case_root/events.log"
  : > "$case_root/compose.yml"
  for name in docker rsync mv curl sleep date stat tee rm rmdir; do
    cat > "$case_root/bin/$name" <<'SHIM'
#!/usr/bin/env bash
exec /bin/bash "$PG18_MOCK_SCRIPT" "${0##*/}" "$@"
SHIM
    chmod +x "$case_root/bin/$name"
  done
  if [ "$command" = rollback ]; then
    mkdir -p "$case_root/data/pg18.$case_ts"
    cp "$case_root/data/pg18/sentinel" "$case_root/data/pg18.$case_ts/sentinel"
    printf 'NEW\n' > "$case_root/data/pg18/sentinel"
    printf 'test/new:18\n' > "$case_root/model/image"
    printf 'new\n' > "$case_root/model/ready"
    phase=swapped; writes_released=0
    if [ "$scenario" = manual_write_guard ]; then phase=complete; writes_released=1; fi
    cat > "$case_root/data/pg18_upgrade.state" <<STATE
TS=$case_ts
OLD_IMAGE=test/old:18
NEW_IMAGE=test/new:18
SWAPPED=1
PHASE=$phase
WRITES_RELEASED=$writes_released
STATE
  fi
  if [ "$command" = verify ]; then
    printf 'test/new:18\n' > "$case_root/model/image"
    printf 'new\n' > "$case_root/model/ready"
  fi
  case "$scenario" in
    stale_state|malformed_state)
      state_ts=$case_ts
      [ "$scenario" != malformed_state ] || state_ts=invalid
      cat > "$case_root/data/pg18_upgrade.state" <<STATE
TS=$state_ts
OLD_IMAGE=test/old:18
NEW_IMAGE=test/new:18
SWAPPED=1
PHASE=complete
WRITES_RELEASED=1
STATE
      cp "$case_root/data/pg18_upgrade.state" "$case_root/initial-state"
      ;;
    cleanup_report)
      mkdir -p "$case_root/data/pg18_old_$case_ts" "$case_root/data/pg18.$case_ts"
      printf 'synthetic archive\n' > "$case_root/data/pg18_cold_$case_ts.tar"
      ;;
  esac
  local rc=0 skip_backup=1
  case "$scenario" in all_backup_failure|all_restore_failure) skip_backup=0 ;; esac
  env -i PATH="$case_root/bin:/usr/bin:/bin" \
    TMPDIR="$case_root/tmp" PG18_MOCK_SCRIPT="$mock" \
    MOCK_ROOT="$case_root" MOCK_SCENARIO="$scenario" MOCK_TS="$case_ts" \
    COMPOSE_FILE="$case_root/compose.yml" DATA_DIR="$case_root/data" \
    PG_SERVICE=pg PG_CONTAINER=pg BACKEND_SERVICE=backend \
    PG_OLD_IMAGE=test/old:18 PG_NEW_IMAGE=test/new:18 \
    BACKEND_HEALTH_URL=http://synthetic.invalid/healthz \
    SMOKE_TABLES=synthetic_table ALTERABLE_EXTS=timescaledb \
    LOG_FILE="$case_root/upgrade.log" YES=1 SKIP_BACKUP="$skip_backup" SKIP_COLD_SNAPSHOT=1 \
    /bin/bash "$case_root/repo/scripts/pg18_bluegreen_upgrade.sh" "$command" \
      > "$case_root/output.log" 2>&1 || rc=$?
  printf '%s\n' "$rc" > "$case_root/exit_code"
  if [ -f "$case_root/data/pg18_upgrade.state" ]; then
    actual_ts=$(sed -n 's/^TS=//p' "$case_root/data/pg18_upgrade.state")
    if [[ "$actual_ts" =~ ^[0-9]{8}T[0-9]{6}(_[0-9]+)?$ ]]; then case_ts=$actual_ts; fi
  fi
  reasons=()
  check missing '^UNSUPPORTED:|^UNSAFE_PATH:|^blocked_delete:'
  # Prove the intended fault was actually reached, not merely an earlier exit.
  case "$scenario" in
    rsync_failure) check has '^fault:rsync$' ;;
    rsync_partial_failure) check has '^fault:rsync_partial$' ;;
    logger_failure) check has '^fault:logger$' ;;
    verify_failure) check has '^sql:extension:new$' ;;
    old_start_failure) check has '^fault:old_start$' ;;
    old_readiness_failure) check has '^fault:old_ready$' ;;
    rollback_pg_stop_failure|manual_pg_stop_failure) check has '^fault:pg_stop$' ;;
    backend_stop_failure|manual_backend_stop_failure) check has '^fault:backend_stop$' ;;
    first_rename_failure) check has '^fault:first_rename$' ;;
    second_rename_failure) check has '^fault:second_rename$' ;;
    restore_rename_failure) check has '^fault:restore_rename$' ;;
    new_start_failure) check has '^fault:new_start$' ;;
    baseline_lookup_failure) check has '^fault:baseline_lookup$' ;;
    baseline_rows_failure) check has '^fault:baseline_rows$' ;;
    hyper_query_failure) check has '^fault:hyper_query$' ;;
    verify_query_failure) check has '^fault:verify_query$' ;;
    alter_failure) check has '^fault:alter$' ;;
    baseline_missing_after_swap|baseline_empty_after_swap|baseline_invalid_after_swap)
      check has "^fault:$scenario$"; check test -f "$case_root/baseline.retained" ;;
  esac
  case "$scenario" in
    success|rsync_no_stats|checksum_contract)
      check test "$rc" -eq 0
      check has '^pg:up:new$'
      check has '^backend:start:new:pg=1:ready=new$'
      check snapshot_intact
      if [ "$scenario" = checksum_contract ]; then check has '^rsync:checksum=1$'; fi
      ;;
    all_precopy_failure|all_backup_failure|all_restore_failure)
      check test "$rc" -ne 0
      check has '^fault:(precopy|backup|restore_smoke)$'
      check missing '^backend:stop$|^rsync:delta$|^pg:up:new$|^mv:old_to_anchor$'
      check old_online
      ;;
    backend_stop_failure|manual_backend_stop_failure|manual_pg_stop_failure)
      check test "$rc" -ne 0
      check missing '^mv:'
      check missing '^backend:start:'
      check no_success
      ;;
    manual_success)
      check test "$rc" -eq 0
      check old_online
      check recovery_order
      check state_zero
      check phase_is rolled_back
      ;;
    manual_write_guard)
      check test "$rc" -ne 0
      check missing '^backend:stop$|^pg:stop$|^mv:|^pg:up:'
      check snapshot_intact
      check no_success
      check grep -q 'ALLOW_DATA_LOSS=1' "$case_root/output.log"
      ;;
    state_failure)
      check test "$rc" -ne 0
      check has '^fault:state_publish$'
      check missing '^mv:old_to_anchor$|^pg:up:new$|^backend:start:'
      check old_data
      ;;
    second_state_failure)
      check test "$rc" -ne 0
      check backend_off
      check old_data
      check missing '^mv:old_to_anchor$|^pg:up:new$|^backend:start:'
      check has '^fault:second_state_publish$'
      check has '^state:prior_preserved$'
      check cmp -s "$case_root/data/pg18_upgrade.state" "$case_root/initial-state"
      check no_success
      ;;
    rollback_state_publish_failure)
      check test "$rc" -ne 0
      check has '^fault:rollback_state_publish$'
      check old_online
      check backend_off
      check missing '^backend:start:'
      check failed_data_retained
      check no_success
      ;;
    stale_state|malformed_state)
      check test "$rc" -ne 0
      check missing '^backend:stop$|^pg:stop$|^mv:|^pg:up:'
      check cmp -s "$case_root/data/pg18_upgrade.state" "$case_root/initial-state"
      check old_online
      ;;
    cleanup_report)
      check test "$rc" -ne 0
      check missing '^blocked_delete:|^backend:stop$|^pg:stop$|^mv:'
      check test -f "$case_root/data/pg18_cold_$case_ts.tar"
      check test -d "$case_root/data/pg18.$case_ts"
      check old_online
      ;;
    verify_standalone_query_failure|verify_standalone_logs_failure)
      check test "$rc" -ne 0
      check has '^fault:(verify_query|logs)$'
      check missing '^backend:stop$|^pg:stop$|^mv:|^pg:up:'
      ;;
    old_start_failure|old_readiness_failure|rollback_pg_stop_failure|restore_rename_failure)
      check test "$rc" -ne 0
      check backend_off
      check missing '^backend:start:'
      check no_success
      case "$scenario" in
        old_start_failure|old_readiness_failure) check old_data; check failed_data_retained ;;
        rollback_pg_stop_failure) check missing '^mv:new_to_failed$|^mv:anchor_to_canonical$'; check snapshot_intact ;;
        restore_rename_failure) check snapshot_intact; check failed_data_retained ;;
      esac
      ;;
    health_failure)
      check test "$rc" -ne 0
      check has '^write:new$'
      check backend_off
      check pg_off
      check missing '^pg:up:old$|^mv:anchor_to_canonical$|^backend:start:old:'
      check snapshot_intact
      check test -f "$case_root/data/pg18/writes"
      check phase_is manual_required
      check no_success
      ;;
    *)
      check test "$rc" -ne 0
      check old_online
      check recovery_order
      check state_zero
      check missing '^pg:rm$'
      case "$scenario" in
        rsync_failure|rsync_partial_failure|logger_failure|baseline_lookup_failure|baseline_rows_failure|hyper_query_failure)
          check missing '^mv:old_to_anchor$|^pg:up:new$' ;;
        verify_failure|verify_query_failure|alter_failure) check failed_data_retained ;;
        baseline_missing_after_swap|baseline_empty_after_swap|baseline_invalid_after_swap) check failed_data_retained ;;
      esac
      ;;
  esac
  if [ "${#reasons[@]}" = 0 ]; then
    passed=$((passed+1)); printf 'PASS %-28s exit=%s\n' "$scenario" "$rc"
    printf '%s\tPASS\t%s\t-\n' "$scenario" "$rc" >> "$suite_root/results.tsv"
  else
    failed=$((failed+1)); printf 'FAIL %-28s exit=%s %s\n' "$scenario" "$rc" "${reasons[*]}"
    printf '%s\tFAIL\t%s\t%s\n' "$scenario" "$rc" "${reasons[*]}" >> "$suite_root/results.tsv"
  fi
}

run_case success
run_case rsync_no_stats
run_case checksum_contract
run_case rsync_failure
run_case rsync_partial_failure
run_case logger_failure
run_case verify_failure
run_case old_start_failure
run_case old_readiness_failure
run_case rollback_pg_stop_failure
run_case backend_stop_failure
run_case first_rename_failure
run_case second_rename_failure
run_case restore_rename_failure
run_case new_start_failure
run_case state_failure
run_case second_state_failure
run_case rollback_state_publish_failure
run_case baseline_lookup_failure
run_case baseline_rows_failure
run_case hyper_query_failure
run_case verify_query_failure
run_case alter_failure
run_case baseline_missing_after_swap
run_case baseline_empty_after_swap
run_case baseline_invalid_after_swap
run_case health_failure
run_case all_backup_failure all
run_case all_restore_failure all
run_case all_precopy_failure all
run_case manual_success rollback
run_case manual_write_guard rollback
run_case manual_backend_stop_failure rollback
run_case manual_pg_stop_failure rollback
run_case stale_state
run_case malformed_state
run_case verify_standalone_query_failure verify
run_case verify_standalone_logs_failure verify
run_case cleanup_report cleanup
printf 'Summary: PASS=%s FAIL=%s\nArtifacts: %s\n' "$passed" "$failed" "$suite_root"
printf 'elapsed_seconds=%s\nfinished_utc=%s\n' "$(( $(/usr/bin/date +%s) - base_epoch ))" \
  "$(/usr/bin/date -u +%Y-%m-%dT%H:%M:%SZ)" >> "$suite_root/timing.txt"
[ "$failed" -eq 0 ]
