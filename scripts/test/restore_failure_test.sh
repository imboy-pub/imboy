#!/usr/bin/env bash
# Exercise real restore entrypoints with local Docker/curl doubles; no DB or network.
set -euo pipefail
cd "$(dirname "$0")/../.."
TASK_DIR="$(mktemp -d)"
trap 'rm -rf "$TASK_DIR"' EXIT
mkdir "$TASK_DIR/bin" "$TASK_DIR/backups"
touch "$TASK_DIR/backups/synthetic_20261002.dump"
cat > "$TASK_DIR/bin/docker" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$*" >> "$CALLS"
case "$*" in
  'ps '*) echo fixture_pg ;;
  *'pg_restore --list'*)
    [ "$SCENARIO" != list_failure ] || exit 2
    echo '1; 0 0 EXTENSION - timescaledb' ;;
  *'pg_restore -U'*) [ "$SCENARIO" != restore_failure ] || exit 2 ;;
  *'timescaledb_post_restore()'*) [ "$SCENARIO" != post_failure ] || exit 2 ;;
  *'information_schema.tables'*) echo 20 ;;
  *'SELECT count(*) FROM public.'*) echo 3 ;;
esac
SH
cat > "$TASK_DIR/bin/curl" <<'SH'
#!/usr/bin/env bash
cat > "$METRICS"
SH
chmod +x "$TASK_DIR/bin/docker" "$TASK_DIR/bin/curl"
FAILURES=0
for ENTRY in restore smoke; do
  for SCENARIO in success restore_failure post_failure list_failure; do
    CALLS="$TASK_DIR/calls"; METRICS="$TASK_DIR/metrics"
    : > "$CALLS"; : > "$METRICS"
    ARGS=(scripts/restore_pg.sh "$TASK_DIR/backups/synthetic_20261002.dump" --target fixture_restore)
    [ "$ENTRY" != smoke ] || ARGS=(scripts/restore_smoke.sh)
    RC=0
    env PATH="$TASK_DIR/bin:$PATH" PG_CONTAINER=fixture_pg POSTGRES_DB=synthetic \
      POSTGRES_USER=fixture_user FORCE=1 BACKUP_DIR="$TASK_DIR/backups" \
      SCENARIO="$SCENARIO" CALLS="$CALLS" METRICS="$METRICS" \
      PUSHGATEWAY_URL=http://fixture.invalid bash "${ARGS[@]}" > "$TASK_DIR/output" 2>&1 || RC=$?
    EXPECTED=0; [ "$SCENARIO" = success ] || EXPECTED=1
    ACTUAL=0; [ "$RC" -eq 0 ] || ACTUAL=1
    VALID=1
    [ "$ACTUAL" = "$EXPECTED" ] || VALID=0
    if [ "$ENTRY" = smoke ]; then
      STATUS=1; [ "$EXPECTED" -eq 0 ] || STATUS=0
      grep -qx "imboy_restore_drill_last_status $STATUS" "$METRICS" || VALID=0
      grep -q 'DROP DATABASE IF EXISTS "imboy_smoke_' "$CALLS" || VALID=0
    fi
    if [ "$VALID" -eq 1 ]; then
      printf 'PASS %s %s\n' "$ENTRY" "$SCENARIO"
    else
      printf 'FAIL %s %s rc=%s\n' "$ENTRY" "$SCENARIO" "$RC"
      FAILURES=$((FAILURES + 1))
    fi
  done
done
[ "$FAILURES" -eq 0 ]
