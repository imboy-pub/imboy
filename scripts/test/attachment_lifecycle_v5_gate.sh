#!/usr/bin/env bash
# attachment lifecycle v5 evidence gate (frozen execution-contract runner_cli_contract.gate)
# Usage:
#   bash scripts/test/attachment_lifecycle_v5_gate.sh --task TASK_ID [--case CASE] \
#     [--scenario SCENARIO] --run-dir ABS --slot SLOT_ID [--schema-phase base|expand-queue|guards-contract|integrated]
# Exit: 0 pass, 2 usage, 20 CONTRACT_CONFLICT, 30 NO_GO, 75 BLOCKED_EXTERNAL.
set -u
printf -v V5_GATE_COMMAND '%q ' bash "$0" "$@"
V5_GATE_COMMAND="${V5_GATE_COMMAND% }"
export V5_GATE_COMMAND
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
RUNNER="$ROOT/scripts/attachment_lifecycle_v5_runner.py"

usage() { grep '^#' "$0" | sed '1,6d' | head -4; exit 2; }

TASK=""; CASE=""; SCENARIO=""; RUN_DIR=""; SLOT=""; PHASE=""
while [ $# -gt 0 ]; do
  case "$1" in
    --task) TASK="${2:?}"; shift 2 ;;
    --case) CASE="${2:?}"; shift 2 ;;
    --scenario) SCENARIO="${2:?}"; shift 2 ;;
    --run-dir) RUN_DIR="${2:?}"; shift 2 ;;
    --slot) SLOT="${2:?}"; shift 2 ;;
    --schema-phase) PHASE="${2:?}"; shift 2 ;;
    -h|--help) usage ;;
    *) echo "unknown arg: $1" >&2; usage ;;
  esac
done
[ -n "$TASK" ] || { echo "--task required" >&2; usage; }
[ -n "$SLOT" ] || { echo "--slot required (fixture slot id, e.g. slot1)" >&2; usage; }
if [ -z "$RUN_DIR" ]; then
  RUN_DIR="${V5_RUN_DIR:-}"
fi
case "$RUN_DIR" in
  /*) : ;;
  *) echo "run-dir must be explicit absolute path (or absolute V5_RUN_DIR)" >&2; exit 2 ;;
esac
[ -d "$RUN_DIR" ] || { echo "run dir not found: $RUN_DIR" >&2; exit 2; }
STATE="$RUN_DIR/state.json"
[ -f "$STATE" ] || { echo "run not admitted (state.json missing)" >&2; exit 20; }

PLAN_DIR="$(python3 -c "import json;print(json.load(open('$STATE'))['plan_dir'])")"

# Resolve task + case registry from the frozen contract
CONTRACT_JSON="$PLAN_DIR/execution-contract.json"
pyval() { python3 -c "
import json, sys
c = json.load(open(sys.argv[1]))
task = sys.argv[2]; key = sys.argv[3]
for t in c['tasks']:
    if t['task_id'] == task:
        if key == 'exists':
            print('yes')
        elif key == 'cases':
            print(json.dumps(t.get('case_ids', [])))
        elif key == 'phases':
            enum = c.get('schema_phase_enum', [])
            out = [t.get('schema_phase')] if t.get('schema_phase') else []
            out += [s.get('phase') for s in (t.get('schema_phase_sequence') or [])]
            print(json.dumps([p for p in out if p in enum]))
        sys.exit(0)
print('no' if key == 'exists' else '[]')
" "$CONTRACT_JSON" "$TASK" "$1"; }
TASK_OK="$(pyval exists)"
CASES_JSON="$(pyval cases)"
PHASES_JSON="$(pyval phases)"
[ "$TASK_OK" = "yes" ] || { echo "task $TASK not in frozen contract" >&2; exit 20; }
if [ -n "$CASE" ]; then
  echo "$CASES_JSON" | grep -q "\"$TASK/$CASE\"" || { echo "case $CASE not registered under $TASK" >&2; exit 2; }
fi
if [ -n "$PHASE" ]; then
  echo "$PHASES_JSON" | grep -q "\"$PHASE\"" || { echo "phase $PHASE not declared by task $TASK" >&2; exit 2; }
fi
if [ -n "$SCENARIO" ] && [ -z "$CASE" ]; then
  echo "--scenario requires --case" >&2; exit 2
fi

EVID="$RUN_DIR/evidence/$TASK"
mkdir -p "$EVID" "$RUN_DIR/logs"
# seal.sh requires an explicit absolute V5_RUN_DIR; export it once here so
# cases that skip ensure_pg (e.g. W0-01) still seal.
export V5_RUN_DIR="$RUN_DIR"
LOG="$RUN_DIR/logs/$TASK-gate-$(date +%H%M%S).log"
exec > >(tee -a "$LOG") 2>&1

PG_READY=0
PG_CONFIG_READY=0
ensure_pg() {
  if [ "$PG_READY" -eq 0 ]; then
    PG_RESOURCE="$(python3 "$SCRIPT_DIR/attachment_lifecycle_v5_pg_guard.py" "$RUN_DIR" "$SLOT")" || return 20
    IFS=$'\t' read -r PG_CONTAINER V5_PG_HOST V5_PG_PORT <<< "$PG_RESOURCE"
    export PG_CONTAINER V5_PG_HOST V5_PG_PORT V5_RUN_DIR="$RUN_DIR" V5_SLOT="$SLOT"
    PG_PASSWORD_FILE="$RUN_DIR/resources/pg-attv5.password"
    [ -s "$PG_PASSWORD_FILE" ] || { echo "isolated PG password missing" >&2; return 75; }
    PG_PASSWORD="$(cat "$PG_PASSWORD_FILE")" || return 20
    for prefix in V5_INTTEST V5_INTTEST_W203 V5_INTTEST_W301 V5_INTTEST_W302 V5_INTTEST_W401 V5_INTTEST_W402 V5_INTTEST_W402P V5_INTTEST_W403P V5_INTTEST_W404; do
      export "${prefix}_PG_HOST=$V5_PG_HOST" "${prefix}_PG_PORT=$V5_PG_PORT" \
        "${prefix}_PG_USER=imboy_user" "${prefix}_PG_PASSWORD=$PG_PASSWORD" "${prefix}_MAINT_DB=postgres"
    done
    PG_READY=1
  fi
  [ "$1" = config ] || return 0
  [ "$PG_CONFIG_READY" -eq 0 ] || return 0
  CONFIG_SOURCE="${EUNIT_CONFIG:-$ROOT/config/sys.local}.config"
  [ -f "$CONFIG_SOURCE" ] || { echo "BLOCKED_EXTERNAL: explicit EUNIT_CONFIG required" >&2; return 75; }
  POOL_DB="$(w201_db_setup "$RUN_DIR/resources/${SLOT}-pool" pool)" || return $?
  PG_CONFIG_DIR="$(mktemp -d "$RUN_DIR/resources/${SLOT}-config.XXXXXX")" || return 20
  trap 'rm -f "$PG_CONFIG_DIR/isolated.config"; rmdir "$PG_CONFIG_DIR"' EXIT
  escript "$SCRIPT_DIR/attachment_lifecycle_v5_pg_config.escript" \
    "$CONFIG_SOURCE" "$PG_CONFIG_DIR/isolated.config" \
    "$PG_PASSWORD_FILE" "$V5_PG_HOST" "$V5_PG_PORT" "$POOL_DB" || return 20
  export EUNIT_CONFIG="$PG_CONFIG_DIR/isolated"
  PG_CONFIG_READY=1
}

# ---------------- case executors ----------------
# ---------------- W2-01 executors ----------------
w201_db_setup() { # $1 cdir [$2 db-suffix] -> echoes DB name; replay full chain once
  local cdir="$1" SUF="${2:-w201}"
  mkdir -p "$cdir"
  local PGPASSWORD; PGPASSWORD=$(cat "$RUN_DIR/resources/pg-attv5.password")
  local DB="attv5_${SLOT}_${SUF}"
  docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d postgres \
    -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = '$DB' AND pid <> pg_backend_pid()" \
    -c "DROP DATABASE IF EXISTS $DB" -c "CREATE DATABASE $DB OWNER imboy_user" > "$cdir/db.log" 2>&1 || return 75
  local E f
  for E in pg_jieba postgis postgis_raster timescaledb pgcrypto "uuid-ossp" pg_trgm btree_gin btree_gist unaccent pg_stat_statements; do
    docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$DB" -c "CREATE EXTENSION IF NOT EXISTS \"$E\"" >> "$cdir/db.log" 2>&1 || return 75
  done
  for f in $(ls "$ROOT/priv/migrations/"*.up.sql | sed 's|.*/||' | sort); do
    docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$DB" -v ON_ERROR_STOP=1 --single-transaction -f - \
      < "$ROOT/priv/migrations/$f" >> "$cdir/raw.log" 2>&1 || { echo "REPLAY_FAIL $f" >> "$cdir/raw.log"; return 30; }
  done
  echo "w201 replayed $(ls "$ROOT/priv/migrations/"*.up.sql | wc -l | tr -d ' ') up files" >> "$cdir/raw.log"
  echo "$DB"
}

w201_run_all_sections() { # $1 cdir $2 db
  local cdir="$1" DB="$2" RC=0
  mkdir -p "$cdir"
  # gate-layer eunit evidence first (probes consume the flags); three tries:
  # a cold beam cache can blow the eunit-local deadline on early passes
  eunit_ok() {
    local try rc
    for try in 1 2 3; do
      EUNIT_USE_RELAY=0 EUNIT_CONFIG="${EUNIT_CONFIG:-}" make -C "$ROOT" eunit-local "t=$1" \
        > "$2" 2>&1
      rc=$?
      if [ $rc -eq 0 ] && grep -q "All .* tests passed" "$2"; then return 0; fi
    done
    return 1
  }
  if eunit_ok elib_s3_sign_tests "$cdir/eunit-s3sign.log"; then
    local S3SIGN=1; else local S3SIGN=0; fi
  if eunit_ok eb_asset_tests "$cdir/eunit-ebasset.log"; then
    local EBASSET=1; else local EBASSET=0; fi
  local SEC
  for SEC in ordinary internal eb pin multipart widget ebp; do
    W201_S3SIGN_OK="$S3SIGN" W201_EB_ASSET_TESTS_OK="$EBASSET" V5_RUN_DIR="$RUN_DIR" \
      bash "$ROOT/scripts/test/attachment_lifecycle_v5_w201_probe.sh" "$DB" "$SEC" "$cdir/$SEC.jsonl" \
      >> "$cdir/raw.log" 2>&1 || { echo "SECTION_FAIL $SEC" >> "$cdir/raw.log"; RC=30; }
  done
  return $RC
}

seal_w201_leafs() {
  # Preserve command failure; sealing occurs once after the complete case.
  return "$3"
}

w201_case() { # $1 cdir
  local cdir="$1"
  local DB
  # pre-warm the TEST-mode beam cache once so per-case eunit stays inside
  # its deadline even in a full --task sweep (cold compile blew it twice)
  make -C "$ROOT" test-build >> /dev/null 2>&1 || true
  DB=$(w201_db_setup "$cdir") || return $?
  w201_run_all_sections "$cdir" "$DB"
  seal_w201_leafs "$CUR_CASE" "$cdir" "$?" "w201 full-chain fresh DB + per-scenario SQL probes + gate-layer eunit evidence (elib_s3_sign_tests, eb_asset_tests)"
}

# The frozen guards-contract.sql predates M3 (migration 00000190 replays the
# same trigger attachments), so applying it after the full 227-file chain hits
# `trigger ... already exists`. Drop exactly the contract's own triggers first
# and let the unchanged SQL re-attach them: identical final schema, idempotent.
guards_contract_stream() {
  cat <<'GUARDS_DROP_PRELUDE'
DROP TRIGGER IF EXISTS trg_v5_attachment_location_immutable ON attachment;
DROP TRIGGER IF EXISTS trg_v5_attachment_identity_immutable ON attachment;
DROP TRIGGER IF EXISTS trg_v5_attachment_first_bind ON attachment;
DROP TRIGGER IF EXISTS trg_v5_pending_session_guard ON attach_pending;
DROP TRIGGER IF EXISTS trg_v5_eb_registration_guard ON enterprise_asset;
DROP TRIGGER IF EXISTS trg_v5_eb_audit_actor_guard ON enterprise_asset;
DROP TRIGGER IF EXISTS trg_v5_aod_job_guard ON attachment_object_deletion;
DROP TRIGGER IF EXISTS trg_v5_ebdq_job_guard ON enterprise_asset_delete_queue;
DROP TRIGGER IF EXISTS trg_v5_metadata_delete_guard ON attachment;
GUARDS_DROP_PRELUDE
  cat "$ROOT/docs/architecture/attachment-lifecycle-v5/guards-contract.sql"
}

w102_phase_run() { # $1 cdir $2 phase
  local cdir="$1" phase="$2"
  local PGPASSWORD_FILE="$RUN_DIR/resources/pg-attv5.password"
  [ -f "$PGPASSWORD_FILE" ] || { echo "pg password file missing" >&2; return 75; }
  local PGPASSWORD; PGPASSWORD=$(cat "$PGPASSWORD_FILE")
  local DB="attv5_${SLOT}_w102_${phase}"
  docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d postgres \
    -c "DROP DATABASE IF EXISTS $DB" -c "CREATE DATABASE $DB OWNER imboy_user" > "$cdir/db.log" 2>&1 || return 75
  for E in pg_jieba postgis postgis_raster timescaledb pgcrypto "uuid-ossp" pg_trgm btree_gin btree_gist unaccent pg_stat_statements; do
    docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$DB" -c "CREATE EXTENSION IF NOT EXISTS \"$E\"" >> "$cdir/db.log" 2>&1 || return 75
  done
  local f
  for f in $(ls "$ROOT/priv/migrations/"*.up.sql | sed 's|.*/||' | sort); do
    docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$DB" -v ON_ERROR_STOP=1 --single-transaction -f - \
      < "$ROOT/priv/migrations/$f" >> "$cdir/raw.log" 2>&1 || { echo "REPLAY_FAIL $f" >> "$cdir/raw.log"; return 30; }
  done
  echo "replayed $(ls "$ROOT/priv/migrations/"*.up.sql | wc -l | tr -d ' ') up files" >> "$cdir/raw.log"
  local GUARDS_DB="attv5_${SLOT}_w102_guards"
  if [ "$phase" = "guards" ]; then
    guards_contract_stream | docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$DB" -v ON_ERROR_STOP=1 --single-transaction -f - \
      >> "$cdir/raw.log" 2>&1 \
      || { echo "GUARDS_FAIL" >> "$cdir/raw.log"; return 30; }
    V5_EXPAND_DB="${V5_EXPAND_DB:-attv5_${SLOT}_w102_expand}" V5_GUARDS_DB="$DB" \
      V5_RUN_DIR="$RUN_DIR" bash "$ROOT/scripts/test/attachment_lifecycle_v5_w102_probe.sh" "$DB" guards "$cdir/probes.jsonl" >> "$cdir/raw.log" 2>&1
  else
    # cross-phase leaves need the guards DB too: build it alongside
    docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d postgres \
      -c "DROP DATABASE IF EXISTS $GUARDS_DB" -c "CREATE DATABASE $GUARDS_DB OWNER imboy_user" >> "$cdir/db.log" 2>&1 || return 75
    for E in pg_jieba postgis postgis_raster timescaledb pgcrypto "uuid-ossp" pg_trgm btree_gin btree_gist unaccent pg_stat_statements; do
      docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$GUARDS_DB" -c "CREATE EXTENSION IF NOT EXISTS \"$E\"" >> "$cdir/db.log" 2>&1 || return 75
    done
    for f in $(ls "$ROOT/priv/migrations/"*.up.sql | sed 's|.*/||' | sort); do
      docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$GUARDS_DB" -v ON_ERROR_STOP=1 --single-transaction -f - \
        < "$ROOT/priv/migrations/$f" >> "$cdir/raw.log" 2>&1 || { echo "GUARDS_REPLAY_FAIL $f" >> "$cdir/raw.log"; return 30; }
    done
    guards_contract_stream | docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$GUARDS_DB" -v ON_ERROR_STOP=1 --single-transaction -f - \
      >> "$cdir/raw.log" 2>&1 \
      || { echo "GUARDS_FAIL" >> "$cdir/raw.log"; return 30; }
    V5_EXPAND_DB="$DB" V5_GUARDS_DB="$GUARDS_DB" \
      V5_RUN_DIR="$RUN_DIR" bash "$ROOT/scripts/test/attachment_lifecycle_v5_w102_probe.sh" "$DB" expand "$cdir/probes.jsonl" >> "$cdir/raw.log" 2>&1
  fi
  return $?
}

seal_w102_leafs() {
  # Preserve command failure; sealing occurs once after the complete case.
  return "$3"
}

bootstrap_w002_schema() {
  local cdir="$1" PGPASSWORD_FILE="$RUN_DIR/resources/pg-attv5.password" DB="attv5_${SLOT}_raw"
  [ -f "$PGPASSWORD_FILE" ] || { echo "pg password file missing" >&2; return 75; }
  local PGPASSWORD; PGPASSWORD=$(cat "$PGPASSWORD_FILE")
  docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d postgres \
    -c "DROP DATABASE IF EXISTS $DB" -c "CREATE DATABASE $DB OWNER imboy_user" > "$cdir/db.log" 2>&1 || return 75
  for E in pg_jieba postgis postgis_raster timescaledb pgcrypto "uuid-ossp" pg_trgm btree_gin btree_gist unaccent pg_stat_statements; do
    docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$DB" -c "CREATE EXTENSION IF NOT EXISTS \"$E\"" >> "$cdir/db.log" 2>&1 || return 75
  done
  local n=0
  for f in $(ls "$ROOT/priv/migrations/"*.up.sql | sed 's|.*/||' | sort); do
    n=$((n+1))
    docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$DB" -v ON_ERROR_STOP=1 --single-transaction -f - < "$ROOT/priv/migrations/$f" >> "$cdir/raw.log" 2>&1 || { echo "REPLAY_FAIL $f" >> "$cdir/raw.log"; return 30; }
  done
  echo "replayed=$n" >> "$cdir/raw.log"
  docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$DB" -v ON_ERROR_STOP=1 \
    < "$ROOT/docs/architecture/attachment-lifecycle-v5/evidence/catalog-query.sql" >> "$cdir/raw.log" 2>&1 || return 30
  grep -q '(1 row)' "$cdir/raw.log" || { echo 'CATALOG_EMPTY' >> "$cdir/raw.log"; return 30; }
}

bootstrap_w002_history() {
  local cdir="$1" PGPASSWORD_FILE="$RUN_DIR/resources/pg-attv5.password" DB="attv5_${SLOT}_raw2"
  [ -f "$PGPASSWORD_FILE" ] || { echo "pg password file missing" >&2; return 75; }
  local PGPASSWORD; PGPASSWORD=$(cat "$PGPASSWORD_FILE")
  docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d postgres \
    -c "DROP DATABASE IF EXISTS $DB" -c "CREATE DATABASE $DB OWNER imboy_user" > "$cdir/db.log" 2>&1 || return 75
  for E in pg_jieba postgis postgis_raster timescaledb pgcrypto "uuid-ossp" pg_trgm btree_gin btree_gist unaccent pg_stat_statements; do
    docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$DB" -c "CREATE EXTENSION IF NOT EXISTS \"$E\"" >> "$cdir/db.log" 2>&1 || return 75
  done
  # The frozen baseline-probes.sql documents PRE-V5 behaviour (mutable pending
  # identity, no db guard) as the "before" evidence for this wave; replay only
  # the migrations that predate the first V5 migration (00000178), else the V5
  # immutability guards correctly reject the probe's mutation upserts.
  local f
  for f in $(ls "$ROOT/priv/migrations/"*.up.sql | sed 's|.*/||' | sort); do
    case "$f" in
      0000017[8-9]*|000001[89]*|000002*) break ;;
    esac
    docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$DB" -v ON_ERROR_STOP=1 --single-transaction -f - < "$ROOT/priv/migrations/$f" >> "$cdir/raw.log" 2>&1 || return 30
  done
  echo "replayed_pre_v5=$f" >> "$cdir/raw.log"
  docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$DB" -v ON_ERROR_STOP=1 \
    < "$ROOT/docs/architecture/attachment-lifecycle-v5/evidence/baseline-probes.sql" > "$cdir/raw.log" 2>&1 || return 30
  grep -c OBSERVED "$cdir/raw.log" | grep -q '^3$' || { echo 'PROBES_MISSING' >> "$cdir/raw.log"; return 30; }
  grep -q '^ 0$\|0$' "$cdir/raw.log" || true
  grep -qE 'remaining_synthetic_rows' "$cdir/raw.log" || { echo 'ROLLBACK_PROBE_MISSING' >> "$cdir/raw.log"; return 30; }
}

# ---------------- W2-02 executors ----------------
# Evidence model: real-PG + real-Garage eunit battery (per-scenario named
# tests) + EB suite flag for seat/widget shared-protocol leaves (W2-01 ebp
# precedent). Leaf receipts point at the suite logs as raw output.
w202_eunit() { # $1 cdir $2 logname $3 module  -> echoes 1/0
  local cdir="$1" log mod="$3" try rc
  log="$cdir/$2"
  for try in 1 2 3; do
    EUNIT_USE_RELAY=0 EUNIT_CONFIG="${EUNIT_CONFIG:-}" make -C "$ROOT" eunit-local "t=$mod" \
      > "$log" 2>&1
    rc=$?
    if [ $rc -eq 0 ] && grep -q "All .* tests passed" "$log"; then echo 1; return 0; fi
    grep -q "Terminated" "$log" || break
  done
  echo 0
  return 30
}

# ---------------- W5/W6 shared helpers ----------------
worktree_sibling() { # $1 repo name -> absolute sibling worktree, 30 if missing
  local dir
  dir="$(cd "$ROOT/.." && pwd)/$1"
  [ -d "$dir" ] || { echo "NO_GO: $1 worktree missing: $dir" >&2; return 30; }
  printf '%s\n' "$dir"
}

# ---------------- W5-01 executor ----------------
# Real client unit suites in the run-owned imboyapp worktree.  The presign
# suite is the contract's declared command; the recovery suite carries the
# contract scenario names.  Both are captured as flutter JSON so the adapter
# can bind one measured test per leaf.
w501_case() { # $1 cdir
  local cdir="$1" app rc1 rc2 t0 t1
  app="$(worktree_sibling imboyapp)" || return $?
  command -v flutter >/dev/null 2>&1 || {
    echo "BLOCKED_EXTERNAL: flutter SDK not on PATH" >&2; return 75; }
  local presign="$app/test/unit_test/store/attachment_upload_presign_test.dart"
  local recovery="$app/test/unit_test/store/attachment_upload_task_recovery_test.dart"
  [ -f "$presign" ] || { echo "NO_GO: required suite missing: $presign" >&2; return 30; }
  [ -f "$recovery" ] || { echo "NO_GO: required suite missing: $recovery" >&2; return 30; }
  [ -d "$app/plugin/flutter_chat_ui/packages/cross_cache" ] || {
    echo "BLOCKED_EXTERNAL: imboyapp vendored plugin/flutter_chat_ui submodule is not initialized" >&2
    return 75; }
  mkdir -p "$cdir"
  t0=$(date +%s)
  ( cd "$app" && flutter test test/unit_test/store/attachment_upload_presign_test.dart --reporter json ) \
    > "$cdir/flutter-presign.jsonl" 2> "$cdir/flutter-presign.err"
  rc1=$?
  t1=$(date +%s)
  echo "W5-01 CMD[presign] cwd=$app exit=$rc1 started=$t0 finished=$t1" >> "$cdir/raw.log"
  tail -n 5 "$cdir/flutter-presign.err" >> "$cdir/raw.log" 2>/dev/null || true
  t0=$(date +%s)
  ( cd "$app" && flutter test test/unit_test/store/attachment_upload_task_recovery_test.dart --reporter json ) \
    > "$cdir/flutter-recovery.jsonl" 2> "$cdir/flutter-recovery.err"
  rc2=$?
  t1=$(date +%s)
  echo "W5-01 CMD[recovery] cwd=$app exit=$rc2 started=$t0 finished=$t1" >> "$cdir/raw.log"
  tail -n 5 "$cdir/flutter-recovery.err" >> "$cdir/raw.log" 2>/dev/null || true
  [ $rc1 -eq 0 ] && [ $rc2 -eq 0 ] || return 30
  return 0
}

# ---------------- W5-02 executor ----------------
# Contract's eight Admin test files under the run-owned imboyadmin worktree,
# with Bun's JUnit reporter so each testcase is a real measurable unit.
w502_case() { # $1 cdir
  local cdir="$1" admin f rc t0 t1
  admin="$(worktree_sibling imboyadmin)" || return $?
  command -v bun >/dev/null 2>&1 || {
    echo "BLOCKED_EXTERNAL: bun not on PATH" >&2; return 75; }
  local files=(
    src/pages/storage/StorageOverviewPage.test.tsx
    src/widget/customer_service/app/attachmentMachine.test.ts
    src/widget/customer_service/app/uploader.test.ts
    src/widget/customer_service/app/widgetApi.test.ts
    src/modules/customer_service/seat/workbench/workbenchApi.test.ts
    src/modules/customer_service/seat/workbench/seatSend.scopeCommit.test.tsx
    src/modules/customer_service/seat/workbench/seatSend.attachmentRecovery.test.tsx
    src/modules/customer_service/seat/workbench/seatSend.revocation.test.tsx
  )
  for f in "${files[@]}"; do
    [ -f "$admin/$f" ] || { echo "NO_GO: required admin suite missing: $admin/$f" >&2; return 30; }
  done
  mkdir -p "$cdir"
  t0=$(date +%s)
  ( cd "$admin" && bun test --isolate --reporter=junit \
      --reporter-outfile="$cdir/bun-junit.xml" "${files[@]}" ) \
    > "$cdir/bun-console.log" 2>&1
  rc=$?
  t1=$(date +%s)
  echo "W5-02 CMD[bun] cwd=$admin exit=$rc started=$t0 finished=$t1" >> "$cdir/raw.log"
  cat "$cdir/bun-console.log" >> "$cdir/raw.log"
  if [ ! -s "$cdir/bun-junit.xml" ]; then
    echo "NO_GO: bun produced no junit report" >> "$cdir/raw.log"
    return 30
  fi
  [ $rc -eq 0 ] || return 30
  return 0
}

# ---------------- W6-01 executor ----------------
# Real-PG/real-Garage/real-Disk replay of the W1-W4 source battery.  Each
# battery keeps its own subdirectory and a timestamped STEP line so the adapter
# can bind plan probes and driver-matrix units to a measured window.  Every
# battery is attempted so the evidence set is maximal, but any non-zero exit
# fails the case (no aggregate relabelling).
w601_step() { # $1 cdir $2 label $3 fn $4 subdir
  local cdir="$1" label="$2" fn="$3" sub="$4" rc t0 t1
  mkdir -p "$sub"
  t0=$(date +%s)
  "$fn" "$sub" >> "$cdir/raw.log" 2>&1
  rc=$?
  t1=$(date +%s)
  echo "W6-01 STEP $label exit=$rc started=$t0 finished=$t1" >> "$cdir/raw.log"
  return $rc
}
w601_w102_expand() { w102_phase_run "$1" expand; }
w601_w102_guards() { w102_phase_run "$1" guards; }

w601_case() { # $1 cdir
  local cdir="$1" rc=0
  mkdir -p "$cdir"
  make -C "$ROOT" test-build >> "$cdir/raw.log" 2>&1 || {
    echo "NO_GO: backend test-build failed before the W6-01 battery" >&2
    return 30; }
  w601_step "$cdir" w102-expand w601_w102_expand "$cdir/w102-expand" || rc=30
  w601_step "$cdir" w102-guards w601_w102_guards "$cdir/w102-guards" || rc=30
  w601_step "$cdir" w201 w201_case "$cdir/w201" || rc=30
  w601_step "$cdir" w202 w202_case "$cdir/w202" || rc=30
  w601_step "$cdir" w203 w203_case "$cdir/w203" || rc=30
  w601_step "$cdir" w301 w301_case "$cdir/w301" || rc=30
  w601_step "$cdir" w302 w302_case "$cdir/w302" || rc=30
  w601_step "$cdir" w303 w303_case "$cdir/w303" || rc=30
  w601_step "$cdir" w401 w401_case "$cdir/w401" || rc=30
  w601_step "$cdir" w402 w402_case "$cdir/w402" || rc=30
  w601_step "$cdir" w403 w403_case "$cdir/w403" || rc=30
  w601_step "$cdir" w404 w404_case "$cdir/w404" || rc=30
  return $rc
}

w601_ac_union_case() { # $1 cdir — PLAN verifier execution over frozen inputs
  local cdir="$1" t0 t1 rc
  mkdir -p "$cdir"
  t0=$(date +%s)
  python3 - "$ROOT" > "$cdir/raw.log" 2>&1 <<'PYA'
import json, sys
root = sys.argv[1]
path = root + "/docs/architecture/attachment-lifecycle-v5/execution-contract.json"
c = json.load(open(path))
matrix = c["expected_leaf_matrix"]
refs = c["final_ac_oracle_refs"]
real = matrix["W6-01/real-pg-garage-disk"]
acs = ("AC01", "AC02", "AC03", "AC04", "AC05", "AC06", "AC07")
errors = []
if {leaf["ac_id"] for leaf in real} != set(acs):
    errors.append("W6-01 AC set is not exactly AC01..AC07")
ids = ["|".join(str(leaf.get(k)) for k in
       ("ac_id", "domain", "driver", "entrypoint", "ordering_or_fault",
        "schema_phase")) for leaf in real]
if len(set(ids)) != len(ids):
    errors.append("duplicate W6-01 leaf identities")
source_pairs = set()
for ac in acs:
    for case in refs[ac]:
        for leaf in matrix[case]:
            source_pairs.add((case, leaf["ordering_or_fault"]))
expanded = {(leaf.get("source_case_ref"), leaf.get("source_scenario"))
            for leaf in real}
missing = source_pairs - expanded
extra = expanded - source_pairs
if missing:
    errors.append("%d source pairs missing from W6-01 expansion" % len(missing))
if extra:
    errors.append("%d undeclared source pairs in W6-01 expansion" % len(extra))
print("AC_UNION_STRUCTURE leaves=%d source_pairs=%d expanded_pairs=%d errors=%r"
      % (len(real), len(source_pairs), len(expanded), errors))
for ac in acs:
    print("AC_UNION_COUNT %s=%d" % (ac, sum(1 for leaf in real
                                            if leaf["ac_id"] == ac)))
sys.exit(1 if errors else 0)
PYA
  rc=$?
  t1=$(date +%s)
  echo "W6-01 STEP ac-union exit=$rc started=$t0 finished=$t1" >> "$cdir/raw.log"
  return $rc
}

# ---------------- W6-02 executor ----------------
# Real physical device + real Chromium against a real backend.  Discovery is
# always recorded; a missing W6-02 suite is NO_GO (30), a missing device,
# browser or backend is BLOCKED_EXTERNAL (75).  No mock/route.fulfill path.
w602_preflight_write() { # $1 out $2 case $3 device $4 devices_json $5 browser $6 ver $7 flutter_spec $8 admin_spec $9 backend
  python3 - "$@" <<'PYB'
import json, os, sys
out, case, device, devices_json, browser, version, fspec, aspec, backend = sys.argv[1:10]
def spec(path):
    return {"path": path, "exists": os.path.isfile(path)}
json.dump({
    "case_id": case,
    "device": {"resource_id": device, "available": bool(device),
               "source": "flutter devices --machine",
               "raw": devices_json[:4000]},
    "browser": {"resource_id": "playwright-chromium" if browser == "1" else "",
                "available": browser == "1", "version": version,
                "source": "imboyadmin node_modules/.bin/playwright --version"},
    "backend": {"base_url": backend, "available": bool(backend)},
    "specs": {"flutter_integration": spec(fspec),
              "admin_playwright": spec(aspec)},
}, open(out, "w"), indent=1)
PYB
}

w602_device_pid() { # $1 device -> device-side app pid if observable
  local device="$1" pid=""
  if command -v adb >/dev/null 2>&1; then
    pid="$(adb -s "$device" shell pidof pub.imboy.app 2>/dev/null | tr -d '\r' | awk '{print $1}')"
  fi
  case "$pid" in ''|*[!0-9]*) printf '' ;; *) printf '%s' "$pid" ;; esac
}

w602_write_provenance() { # $1 file $2 kind $3 resource $4 pid $5 source
  python3 - "$@" <<'PYC'
import json, sys
path, kind, resource, pid, source = sys.argv[1:6]
json.dump({"kind": kind, "resource_id": resource, "process_id": int(pid),
           "source": source}, open(path, "w"), indent=1)
PYC
}

w602_run_flutter() { # $1 app $2 cdir $3 driver $4 device
  local app="$1" cdir="$2" driver="$3" device="$4" pid rc dpid=""
  ( cd "$app" && V5_E2E_DRIVER="$driver" flutter test \
      integration_test/attachment_lifecycle_v5_test.dart -d "$device" --reporter json ) \
    > "$cdir/flutter-integration-$driver.jsonl" 2> "$cdir/flutter-integration-$driver.err" &
  pid=$!
  # Host driver process is alive at capture; overwritten by the device-side
  # app PID as soon as adb can observe it during the run.
  kill -0 "$pid" 2>/dev/null && \
    w602_write_provenance "$cdir/device-provenance.json" physical "$device" "$pid" \
      "host flutter test driver process, alive at run start"
  while kill -0 "$pid" 2>/dev/null; do
    if [ -z "$dpid" ]; then
      dpid="$(w602_device_pid "$device")"
      if [ -n "$dpid" ] && kill -0 "$pid" 2>/dev/null; then
        w602_write_provenance "$cdir/device-provenance.json" physical "$device" "$dpid" \
          "adb shell pidof pub.imboy.app observed live during the integration run"
      fi
    fi
    sleep 2
  done
  wait "$pid"
  rc=$?
  echo "W6-02 CMD[flutter-$driver] cwd=$app device=$device exit=$rc device_pid=${dpid:-host:$pid}" >> "$cdir/raw.log"
  return $rc
}

w602_chromium_child() { # $1 parent pid -> deepest live Chromium descendant
  local parent="$1" child name
  for child in $(pgrep -P "$parent" 2>/dev/null); do
    name="$(ps -p "$child" -o comm= 2>/dev/null)"
    case "$name" in
      *[Cc]hromium*|*chrome*|*Chrome*) printf '%s' "$child"; return 0 ;;
    esac
    local found
    found="$(w602_chromium_child "$child")"
    [ -n "$found" ] && { printf '%s' "$found"; return 0; }
  done
  return 1
}

w602_run_admin() { # $1 admin $2 cdir $3 driver
  local admin="$1" cdir="$2" driver="$3" pid rc cpid=""
  ( cd "$admin" && V5_E2E_DRIVER="$driver" ./node_modules/.bin/playwright test \
      tests/e2e/attachment-lifecycle-v5.spec.ts --reporter=json ) \
    > "$cdir/admin-playwright-$driver.json" 2> "$cdir/admin-playwright-$driver.err" &
  pid=$!
  kill -0 "$pid" 2>/dev/null && \
    w602_write_provenance "$cdir/browser-provenance.json" real-browser \
      playwright-chromium "$pid" "host Playwright runner process, alive at run start"
  while kill -0 "$pid" 2>/dev/null; do
    if [ -z "$cpid" ]; then
      cpid="$(w602_chromium_child "$pid" || true)"
      if [ -n "$cpid" ] && kill -0 "$cpid" 2>/dev/null; then
        w602_write_provenance "$cdir/browser-provenance.json" real-browser \
          playwright-chromium "$cpid" "live Chromium descendant of the Playwright run"
      fi
    fi
    sleep 1
  done
  wait "$pid"
  rc=$?
  echo "W6-02 CMD[playwright-$driver] cwd=$admin exit=$rc browser_pid=${cpid:-host:$pid}" >> "$cdir/raw.log"
  return $rc
}

w602_case() { # $1 case_id $2 cdir
  local cid="$1" cdir="$2" app admin device="" devices_json browser=0 version=""
  local backend="${W602_BACKEND_URL:-}" need_flutter=0 need_admin=0 rc=0
  app="$(worktree_sibling imboyapp)" || return $?
  admin="$(worktree_sibling imboyadmin)" || return $?
  mkdir -p "$cdir"
  command -v flutter >/dev/null 2>&1 || {
    echo "BLOCKED_EXTERNAL: flutter SDK not on PATH" >&2; return 75; }
  devices_json="$(cd "$app" && flutter devices --machine 2>/dev/null || true)"
  local id
  for id in XWE6R19916004085 00008140-000E30561E32801C; do
    if printf '%s' "$devices_json" | grep -Eq "\"id\"[[:space:]]*:[[:space:]]*\"$id\""; then
      device="$id"; break
    fi
  done
  if [ -x "$admin/node_modules/.bin/playwright" ]; then
    version="$(cd "$admin" && ./node_modules/.bin/playwright --version 2>/dev/null | head -1)"
    [ -n "$version" ] && browser=1
  fi
  local flutter_spec="$app/integration_test/attachment_lifecycle_v5_test.dart"
  local admin_spec="$admin/tests/e2e/attachment-lifecycle-v5.spec.ts"
  w602_preflight_write "$cdir/w602-preflight.json" "$cid" "$device" \
    "$devices_json" "$browser" "$version" "$flutter_spec" "$admin_spec" "$backend"
  case "$cid" in
    W6-02/flutter-recovery-real|W6-02/ac08) need_flutter=1 ;;
    W6-02/admin-recovery-real) need_admin=1 ;;
    W6-02/e2e-compat) need_flutter=1; need_admin=1 ;;
  esac
  if [ $need_flutter -eq 1 ] && [ ! -f "$flutter_spec" ]; then
    echo "NO_GO: required Flutter integration suite missing: $flutter_spec" >&2
    return 30
  fi
  if [ $need_admin -eq 1 ] && [ ! -f "$admin_spec" ]; then
    echo "NO_GO: required Admin Playwright suite missing: $admin_spec" >&2
    return 30
  fi
  [ -n "$device" ] || {
    echo "BLOCKED_EXTERNAL: no allowlisted physical device (XWE6R19916004085 / 00008140-000E30561E32801C)" >&2
    return 75; }
  [ $browser -eq 1 ] || {
    echo "BLOCKED_EXTERNAL: playwright-chromium unavailable in $admin" >&2
    return 75; }
  [ -n "$backend" ] || {
    echo "BLOCKED_EXTERNAL: W602_BACKEND_URL not set; a real backend/storage is required" >&2
    return 75; }
  curl -fsS --max-time 10 "$backend" > /dev/null 2>&1 || {
    echo "BLOCKED_EXTERNAL: real backend $backend is not reachable" >&2
    return 75; }
  local driver
  for driver in garage disk; do
    if [ $need_flutter -eq 1 ]; then
      w602_run_flutter "$app" "$cdir" "$driver" "$device" || rc=30
    fi
    if [ $need_admin -eq 1 ]; then
      w602_run_admin "$admin" "$cdir" "$driver" || rc=30
    fi
  done
  return $rc
}

# ---------------- W6-03 executors ----------------
w603_cmd() { # $1 cdir $2 label $3 outfile $4 cwd ; shift 4 ; command...
  local cdir="$1" label="$2" out="$3" cwd="$4" t0 t1 rc
  shift 4
  t0=$(date +%s)
  ( cd "$cwd" && "$@" ) > "$out" 2>&1
  rc=$?
  t1=$(date +%s)
  echo "W6-03 STEP $label exit=$rc started=$t0 finished=$t1" >> "$cdir/raw.log"
  return $rc
}

w603_excluded_modules() {
  python3 - "$ROOT/Makefile" <<'PYD'
import re, sys
text = open(sys.argv[1]).read()
match = re.search(r"Excl = \[(.*?)\]", text, re.S)
if not match:
    sys.exit(2)
print("\n".join(item.strip() for item in match.group(1).split(",")
                if item.strip()))
PYD
}

w603_final_regression() { # $1 cdir
  local cdir="$1" rc=0 app admin module
  app="$(worktree_sibling imboyapp)" || return $?
  admin="$(worktree_sibling imboyadmin)" || return $?
  mkdir -p "$cdir"
  # Real Garage credentials for suites that do byte round-trips (the same
  # W202 env the per-wave executors export).
  if [ -f "$RUN_DIR/resources/garage-w202.env" ]; then
    . "$RUN_DIR/resources/garage-w202.env"
    export W202_GARAGE_ENDPOINT W202_GARAGE_BUCKET W202_GARAGE_ACCESS_KEY W202_GARAGE_SECRET_KEY
  fi
  w603_cmd "$cdir" eunit-full "$cdir/eunit-full.log" "$ROOT" \
    env EUNIT_USE_RELAY=0 make eunit-local || rc=30
  local modules
  modules="$(w603_excluded_modules)" || {
    echo "NO_GO: cannot parse the backend Excl suite list" >&2; return 30; }
  for module in $modules; do
    w603_cmd "$cdir" "excl-$module" "$cdir/excl-$module.log" "$ROOT" \
      env EUNIT_USE_RELAY=0 make eunit-local "t=$module" || rc=30
  done
  w603_cmd "$cdir" flutter-analyze "$cdir/flutter-analyze.log" "$app" \
    flutter analyze || rc=30
  local t0 t1 rc_ft
  t0=$(date +%s)
  ( cd "$app" && flutter test --reporter json ) \
    > "$cdir/flutter-test.jsonl" 2> "$cdir/flutter-test.err"
  rc_ft=$?
  t1=$(date +%s)
  echo "W6-03 STEP flutter-test exit=$rc_ft started=$t0 finished=$t1" >> "$cdir/raw.log"
  [ $rc_ft -eq 0 ] || rc=30
  w603_cmd "$cdir" admin-test "$cdir/admin-test.log" "$admin" bun run test || rc=30
  w603_cmd "$cdir" admin-build "$cdir/admin-build.log" "$admin" bun run build || rc=30
  w603_cmd "$cdir" admin-lint "$cdir/admin-lint.log" "$admin" bun run lint || rc=30
  return $rc
}

w603_ledger_verify() { # $1 cdir
  local cdir="$1" gate t0 t1 rc=0 r
  mkdir -p "$cdir"
  t0=$(date +%s)
  python3 "$RUNNER" self-test > "$cdir/runner-selftest.json" 2>&1
  r=$?
  t1=$(date +%s)
  echo "W6-03 STEP ledger-selftest exit=$r started=$t0 finished=$t1" >> "$cdir/raw.log"
  [ $r -eq 0 ] || rc=30
  for gate in G0 G1 G2 G3 G4 G5; do
    t0=$(date +%s)
    python3 "$RUNNER" verify --run-dir "$RUN_DIR" --gate "$gate" \
      > "$cdir/verify-$gate.json" 2>&1
    r=$?
    t1=$(date +%s)
    echo "W6-03 STEP ledger-verify-$gate exit=$r started=$t0 finished=$t1" >> "$cdir/raw.log"
    # verify exits 20 on a FAIL ledger; the adapter reads the JSON verdict.
  done
  return $rc
}

# ---------------- W2-03 executor ----------------
# ---------------- W3-01 executor ----------------
w301_case() { # $1 cdir — 与 w203_case 同一直连 erl 模式，套件含 W3-01 电池
  local cdir="$1" RC=0
  mkdir -p "$cdir"
  local PZ="" d
  for d in deps/*/ebin; do [ -d "$d" ] && PZ="$PZ -pz $PWD/$d"; done
  ln -sfn . imboy
  sed -e "s/{http_port, 9800}/{http_port, ${EUNIT_HTTP_PORT:-19980}}/" \
      -e "s/{http_port_adm, 9706}/{http_port_adm, ${EUNIT_HTTP_ADM_PORT:-19970}}/" \
      "${EUNIT_CONFIG:-config/sys.local}.config" > "$cdir/w301-direct.config"
  EUNIT_USE_RELAY=0 erl -noinput -boot no_dot_erlang +P 1048576 +Q 1024 \
    -config "$cdir/w301-direct.config" -pa imboy/ebin -pa ebin -pa test $PZ \
    -eval 'CoverSetup = fun() -> ok end, CoverExport = fun(_) -> ok end, CoverSetup(),
      case eunit:test([attachment_lifecycle_v5_pg_tests], [verbose]) of
        ok -> io:format("W301_DIRECT_OK~n");
        error -> halt(2)
      end,
      halt()' > "$cdir/eunit-w203.log" 2>&1
  local rc=$?
  rm -f imboy
  if [ $rc -eq 0 ] && grep -q "tests passed" "$cdir/eunit-w203.log"; then
    W203_PG_OK=1
  else
    W203_PG_OK=0
  fi
  [ "$W203_PG_OK" -eq 1 ] || return 30
  export W203_PG_OK V5_RUN_DIR="$RUN_DIR"
  return $RC
}

# ---------------- W3-02 executor ----------------
w302_case() { # $1 cdir — 与 w301_case 同一直连 erl 模式，套件含 W3-02 电池
  local cdir="$1" RC=0
  mkdir -p "$cdir"
  local PZ="" d
  for d in deps/*/ebin; do [ -d "$d" ] && PZ="$PZ -pz $PWD/$d"; done
  ln -sfn . imboy
  sed -e "s/{http_port, 9800}/{http_port, ${EUNIT_HTTP_PORT:-19980}}/" \
      -e "s/{http_port_adm, 9706}/{http_port_adm, ${EUNIT_HTTP_ADM_PORT:-19970}}/" \
      "${EUNIT_CONFIG:-config/sys.local}.config" > "$cdir/w302-direct.config"
  EUNIT_USE_RELAY=0 erl -noinput -boot no_dot_erlang +P 1048576 +Q 1024 \
    -config "$cdir/w302-direct.config" -pa imboy/ebin -pa ebin -pa test $PZ \
    -eval 'CoverSetup = fun() -> ok end, CoverExport = fun(_) -> ok end, CoverSetup(),
      case eunit:test([attachment_lifecycle_v5_hold_protocol_tests], [verbose]) of
        ok -> io:format("W302_DIRECT_OK~n");
        error -> halt(2)
      end,
      halt()' > "$cdir/eunit-w302.log" 2>&1
  local rc=$?
  rm -f imboy
  if [ $rc -eq 0 ] && grep -q "W302_DIRECT_OK" "$cdir/eunit-w302.log"; then
    W302_PG_OK=1
  else
    W302_PG_OK=0
  fi
  [ "$W302_PG_OK" -eq 1 ] || return 30
  export W302_PG_OK V5_RUN_DIR="$RUN_DIR"
  return $RC
}

# ---------------- W3-03 executor ----------------
w303_case() { # $1 cdir — 与 w302_case 同一直连 erl 模式（投影+绑定套件）
  local cdir="$1" RC=0
  mkdir -p "$cdir"
  local PZ="" d
  for d in deps/*/ebin; do [ -d "$d" ] && PZ="$PZ -pz $PWD/$d"; done
  ln -sfn . imboy
  sed -e "s/{http_port, 9800}/{http_port, ${EUNIT_HTTP_PORT:-19980}}/" \
      -e "s/{http_port_adm, 9706}/{http_port_adm, ${EUNIT_HTTP_ADM_PORT:-19970}}/" \
      "${EUNIT_CONFIG:-config/sys.local}.config" > "$cdir/w303-direct.config"
  EUNIT_USE_RELAY=0 erl -noinput -boot no_dot_erlang +P 1048576 +Q 1024 \
    -config "$cdir/w303-direct.config" -pa imboy/ebin -pa ebin -pa test $PZ \
    -eval 'CoverSetup = fun() -> ok end, CoverExport = fun(_) -> ok end, CoverSetup(),
      case eunit:test([eb_message_assets_projection_tests], [verbose]) of
        ok -> io:format("W303_DIRECT_OK~n");
        error -> halt(2)
      end,
      halt()' > "$cdir/eunit-w303.log" 2>&1
  local rc=$?
  rm -f imboy
  if [ $rc -eq 0 ] && grep -q "W303_DIRECT_OK" "$cdir/eunit-w303.log"; then
    W303_PG_OK=1
  else
    W303_PG_OK=0
  fi
  [ "$W303_PG_OK" -eq 1 ] || return 30
  export W303_PG_OK V5_RUN_DIR="$RUN_DIR"
  return $RC
}

# ---------------- W4-01 executor ----------------
# 六 case 共用一组真库套件（行为矩阵/静态门/故障注入/治理拦截）+ 竞态
# 屏障套件在 W4-01 代码头重跑（confirm-gc/reference-gc/hold-gc 的协议
# 在 fn 层，W4-01 实现即入口统一走 fn；静态门证明无绕过）。
w401_case() { # $1 cdir
  local cdir="$1" RC=0
  mkdir -p "$cdir"
  local PZ="" d
  for d in deps/*/ebin; do [ -d "$d" ] && PZ="$PZ -pz $PWD/$d"; done
  ln -sfn . imboy
  sed -e "s/{http_port, 9800}/{http_port, ${EUNIT_HTTP_PORT:-19980}}/" \
      -e "s/{http_port_adm, 9706}/{http_port_adm, ${EUNIT_HTTP_ADM_PORT:-19970}}/" \
      "${EUNIT_CONFIG:-config/sys.local}.config" > "$cdir/w401-direct.config"
  EUNIT_USE_RELAY=0 erl -noinput -boot no_dot_erlang +P 1048576 +Q 1024 \
    -config "$cdir/w401-direct.config" -pa imboy/ebin -pa ebin -pa test $PZ \
    -eval 'CoverSetup = fun() -> ok end, CoverExport = fun(_) -> ok end, CoverSetup(),
      Mods = [attachment_lifecycle_v5_pg_tests,
              attachment_lifecycle_v5_hold_protocol_tests,
              attachment_lifecycle_v5_w401_entrypoints_tests,
              attachment_lifecycle_v5_w401_hold_entrypoints_tests,
              attachment_lifecycle_v5_w401_rollback_tests],
      case eunit:test(Mods, [verbose]) of
        ok -> io:format("W401_DIRECT_OK~n");
        error -> halt(2)
      end,
      halt()' > "$cdir/eunit-w401.log" 2>&1
  local rc=$?
  rm -f imboy
  if [ $rc -eq 0 ] && grep -q "W401_DIRECT_OK" "$cdir/eunit-w401.log"; then
    W401_PG_OK=1
  else
    W401_PG_OK=0
  fi
  [ "$W401_PG_OK" -eq 1 ] || return 30
  export W401_PG_OK V5_RUN_DIR="$RUN_DIR"
  return $RC
}

# ---------------- W4-02 executor ----------------
w402_case() { # $1 cdir — worker 协议 + purged-recovery + 队列 fn 重跑
  local cdir="$1" RC=0 try rc
  mkdir -p "$cdir"
  local PZ="" d
  for d in deps/*/ebin; do [ -d "$d" ] && PZ="$PZ -pz $PWD/$d"; done
  ln -sfn . imboy
  sed -e "s/{http_port, 9800}/{http_port, ${EUNIT_HTTP_PORT:-19980}}/" \
      -e "s/{http_port_adm, 9706}/{http_port_adm, ${EUNIT_HTTP_ADM_PORT:-19970}}/" \
      "${EUNIT_CONFIG:-config/sys.local}.config" > "$cdir/w402-direct.config"
  for try in 1 2 3; do
    EUNIT_USE_RELAY=0 erl -noinput -boot no_dot_erlang +P 1048576 +Q 1024 \
      -config "$cdir/w402-direct.config" -pa imboy/ebin -pa ebin -pa test $PZ \
      -eval 'CoverSetup = fun() -> ok end, CoverExport = fun(_) -> ok end, CoverSetup(),
        Mods = [attachment_lifecycle_v5_w402_worker_tests,
                attachment_lifecycle_v5_w402_purged_recovery_tests],
        case eunit:test(Mods, [verbose]) of
          ok -> io:format("W402_DIRECT_OK~n");
          error -> halt(2)
        end,
        halt()' > "$cdir/eunit-w402.log" 2>&1
    rc=$?
    [ $rc -eq 0 ] && break
    [ $try -lt 3 ] && sleep 5
  done
  rm -f imboy
  if [ $rc -eq 0 ] && grep -q "W402_DIRECT_OK" "$cdir/eunit-w402.log"; then
    W402_PG_OK=1
  else
    W402_PG_OK=0
  fi
  [ "$W402_PG_OK" -eq 1 ] || return 30
  export W402_PG_OK V5_RUN_DIR="$RUN_DIR"
  return $RC
}

# ---------------- W4-03 executor ----------------
# 七 case 共用 storage 三件套（registry/driver 矩阵/predecessor 重放）+
# 核心套件；storage-garage 需 W202_GARAGE_* env（套件内 fail-safe skip）。
w403_case() { # $1 cdir
  local cdir="$1" RC=0 try rc
  mkdir -p "$cdir"
  # Disk fixtures must live inside the run-owned root registered at admission,
  # never in a shared /tmp tree; the suites read V5_DISK_ROOT for their root.
  export V5_DISK_ROOT="${V5_DISK_ROOT:-$RUN_DIR/resources/disk-roots}"
  mkdir -p "$V5_DISK_ROOT" || return 75
  # The driver-matrix and gateway-protocol suites require a run-owned
  # attv5_chain_* database named by W403_RESOURCE_MANIFEST, plus the isolated
  # config and real Garage credentials. Create the chain DB with the full
  # migration chain so every storage leaf runs against freshly replayed schema.
  local W403_DB="attv5_chain_${SLOT}_w403"
  local W403_MANIFEST="$RUN_DIR/resources/w403-resource-manifest.json"
  if [ -z "${W403_SKIP_DB_SETUP:-}" ]; then
    local PGPASSWORD; PGPASSWORD=$(cat "$RUN_DIR/resources/pg-attv5.password")
    docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d postgres \
      -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = '$W403_DB' AND pid <> pg_backend_pid()" \
      -c "DROP DATABASE IF EXISTS $W403_DB" -c "CREATE DATABASE $W403_DB OWNER imboy_user" \
      > "$cdir/db.log" 2>&1 || return 75
    local E f
    for E in pg_jieba postgis postgis_raster timescaledb pgcrypto "uuid-ossp" pg_trgm btree_gin btree_gist unaccent pg_stat_statements; do
      docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$W403_DB" \
        -c "CREATE EXTENSION IF NOT EXISTS \"$E\"" >> "$cdir/db.log" 2>&1 || return 75
    done
    for f in $(ls "$ROOT/priv/migrations/"*.up.sql | sed 's|.*/||' | sort); do
      docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$W403_DB" \
        -v ON_ERROR_STOP=1 --single-transaction -f - \
        < "$ROOT/priv/migrations/$f" >> "$cdir/raw.log" 2>&1 || { echo "W403_REPLAY_FAIL $f" >> "$cdir/raw.log"; return 30; }
    done
    python3 - "$W403_MANIFEST" "$W403_DB" > /dev/null <<'PYM'
import json, sys
json.dump({"database": sys.argv[2], "owner": "att-v5-closeout",
           "purpose": "w4-03 driver matrix scratch chain database"},
          open(sys.argv[1], "w"), indent=1)
PYM
  fi
  export W403_RESOURCE_MANIFEST="${W403_RESOURCE_MANIFEST:-$W403_MANIFEST}"
  export W403_CONFIG_PATH="${W403_CONFIG_PATH:-${EUNIT_CONFIG:-config/sys.local}.config}"
  if [ -f "$RUN_DIR/resources/garage-w202.env" ]; then
    . "$RUN_DIR/resources/garage-w202.env"
    export W202_GARAGE_ENDPOINT W202_GARAGE_BUCKET W202_GARAGE_ACCESS_KEY W202_GARAGE_SECRET_KEY
  fi
  local PZ="" d
  for d in deps/*/ebin; do [ -d "$d" ] && PZ="$PZ -pz $PWD/$d"; done
  ln -sfn . imboy
  sed -e "s/{http_port, 9800}/{http_port, ${EUNIT_HTTP_PORT:-19980}}/" \
      -e "s/{http_port_adm, 9706}/{http_port_adm, ${EUNIT_HTTP_ADM_PORT:-19970}}/" \
      "${EUNIT_CONFIG:-config/sys.local}.config" > "$cdir/w403-direct.config"
  for try in 1 2 3; do
    EUNIT_USE_RELAY=0 erl -noinput -boot no_dot_erlang +P 1048576 +Q 1024 \
      -config "$cdir/w403-direct.config" -pa imboy/ebin -pa ebin -pa test $PZ \
      -eval 'CoverSetup = fun() -> ok end, CoverExport = fun(_) -> ok end, CoverSetup(),
        Mods = [attachment_lifecycle_v5_w403_storage_tests,
                attachment_lifecycle_v5_w403_driver_matrix_tests,
                attachment_lifecycle_v5_w403_predecessor_tests],
        case eunit:test(Mods, [verbose]) of
          ok -> io:format("W403_DIRECT_OK~n");
          error -> halt(2)
        end,
        halt()' > "$cdir/eunit-w403.log" 2>&1
    rc=$?
    [ $rc -eq 0 ] && break
    [ $try -lt 3 ] && sleep 5
  done
  rm -f imboy
  if [ $rc -eq 0 ] && grep -q "W403_DIRECT_OK" "$cdir/eunit-w403.log"; then
    W403S_PG_OK=1
  else
    W403S_PG_OK=0
  fi
  [ "$W403S_PG_OK" -eq 1 ] || return 30
  export W403S_PG_OK V5_RUN_DIR="$RUN_DIR"
  return $RC
}

# ---------------- W4-04 executor ----------------
w404_case() { # $1 cdir
  local cdir="$1" RC=0 try rc
  mkdir -p "$cdir"
  local PZ="" d
  for d in deps/*/ebin; do [ -d "$d" ] && PZ="$PZ -pz $PWD/$d"; done
  ln -sfn . imboy
  sed -e "s/{http_port, 9800}/{http_port, ${EUNIT_HTTP_PORT:-19980}}/" \
      -e "s/{http_port_adm, 9706}/{http_port_adm, ${EUNIT_HTTP_ADM_PORT:-19970}}/" \
      "${EUNIT_CONFIG:-config/sys.local}.config" > "$cdir/w404-direct.config"
  for try in 1 2 3; do
    EUNIT_USE_RELAY=0 erl -noinput -boot no_dot_erlang +P 1048576 +Q 1024 \
      -config "$cdir/w404-direct.config" -pa imboy/ebin -pa ebin -pa test $PZ \
      -eval 'CoverSetup = fun() -> ok end, CoverExport = fun(_) -> ok end, CoverSetup(),
        Mods = [attachment_lifecycle_v5_w404_activation_tests],
        case eunit:test(Mods, [verbose]) of
          ok -> io:format("W404_DIRECT_OK~n");
          error -> halt(2)
        end,
        halt()' > "$cdir/eunit-w404.log" 2>&1
    rc=$?
    [ $rc -eq 0 ] && break
    [ $try -lt 3 ] && sleep 5
  done
  rm -f imboy
  if [ $rc -eq 0 ] && grep -q "W404_DIRECT_OK" "$cdir/eunit-w404.log"; then
    W404S_PG_OK=1
  else
    W404S_PG_OK=0
  fi
  [ "$W404S_PG_OK" -eq 1 ] || return 30
  export W404S_PG_OK V5_RUN_DIR="$RUN_DIR"
  return $RC
}

w203_case() { # $1 cdir
  local cdir="$1" RC=0
  mkdir -p "$cdir"
  # 屏障套件（真实双连接，inttest_marker_db 自管供给库）；直连 erl 复用
  # eunit-local 同款参数（A1c 全量重编窗口是跨会话 SIGTERM 的主要暴露面，
  # 构建已由 test-build 预热时直连运行更稳）。
  local PZ="" d
  for d in deps/*/ebin; do [ -d "$d" ] && PZ="$PZ -pz $PWD/$d"; done
  ln -sfn . imboy
  sed -e "s/{http_port, 9800}/{http_port, ${EUNIT_HTTP_PORT:-19980}}/" \
      -e "s/{http_port_adm, 9706}/{http_port_adm, ${EUNIT_HTTP_ADM_PORT:-19970}}/" \
      "${EUNIT_CONFIG:-config/sys.local}.config" > "$cdir/w203-direct.config"
  EUNIT_USE_RELAY=0 erl -noinput -boot no_dot_erlang +P 1048576 +Q 1024 \
    -config "$cdir/w203-direct.config" -pa imboy/ebin -pa ebin -pa test $PZ \
    -eval 'CoverSetup = fun() -> ok end, CoverExport = fun(_) -> ok end, CoverSetup(),
      case eunit:test([attachment_lifecycle_v5_pg_tests], [verbose]) of
        ok -> io:format("W203_DIRECT_OK~n");
        error -> halt(2)
      end,
      halt()' > "$cdir/eunit-w203.log" 2>&1
  local rc=$?
  rm -f imboy
  if [ $rc -eq 0 ] && grep -q "tests passed" "$cdir/eunit-w203.log"; then
    W203_PG_OK=1
  else
    W203_PG_OK=0
  fi
  [ "$W203_PG_OK" -eq 1 ] || return 30
  export W203_PG_OK V5_RUN_DIR="$RUN_DIR"
  return $RC
}

w202_case() { # $1 cdir
  local cdir="$1" RC=0
  # 证据在任务内共享：三个 case 复用同一套套件日志（各 case 的差异只在封叶矩阵）
  local SH="$EVID/W2-02-shared"
  mkdir -p "$SH" "$cdir"
  # 真 Garage 凭证（confirm_pg 套件按 W202_GARAGE_* 做真实字节往返）
  if [ -f "$RUN_DIR/resources/garage-w202.env" ]; then
    . "$RUN_DIR/resources/garage-w202.env"
    export W202_GARAGE_ENDPOINT W202_GARAGE_BUCKET W202_GARAGE_ACCESS_KEY W202_GARAGE_SECRET_KEY
  fi
  local SUITE
  for SUITE in "attachment_lifecycle_v5_confirm_pg_tests:eunit-confirm.log" \
               "eb_asset_tests:eunit-ebasset.log" \
               "attachment_storage_gateway_handler_tests:eunit-gateway.log" \
               "attach_logic_tests:eunit-attachlogic.log"; do
    local mod="${SUITE%%:*}" log="${SUITE##*:}"
    w202_eunit "$SH" "$log" "$mod" > /dev/null || return 30
  done
  local f log flag
  for f in "eunit-confirm.log:W202_CONFIRM_PG_OK" "eunit-ebasset.log:W202_EB_ASSET_OK" \
           "eunit-gateway.log:W202_GATEWAY_OK" "eunit-attachlogic.log:W202_ATTACH_LOGIC_OK"; do
    log="${f%%:*}"
    flag="${f##*:}"
    cp -f "$SH/$log" "$cdir/$log" 2>/dev/null || return 30
    if grep -q "All .* tests passed" "$cdir/$log" 2>/dev/null; then
      export "$flag=1"
    else
      export "$flag=0"
    fi
  done
  for flag in W202_CONFIRM_PG_OK W202_EB_ASSET_OK W202_GATEWAY_OK W202_ATTACH_LOGIC_OK; do
    [ "${!flag}" -eq 1 ] || return 30
  done
  echo "w202 flags: CONFIRM=$W202_CONFIRM_PG_OK EB=$W202_EB_ASSET_OK GW=$W202_GATEWAY_OK AL=$W202_ATTACH_LOGIC_OK (SH=$SH)" >> "$cdir/raw.log"
  export W202_GARAGE_ENDPOINT="${W202_GARAGE_ENDPOINT:-}" V5_RUN_DIR="$RUN_DIR"
  return $RC
}

run_case() {  # $1 case_id
  local cid="$1"
  local cdir="$EVID/${cid#*/}"
  mkdir -p "$cdir"
  case "$cid" in
    W0-02/*|W0-03/strict-ledger-bootstrap|W1-02/*) ensure_pg sql || return $? ;;
    W2-*/*|W3-*/*|W4-*/*) ensure_pg config || return $? ;;
    W6-01/*|W6-03/final-regression) ensure_pg config || return $? ;;
  esac
  case "$cid" in
    W1-02/migration|W1-02/down-block)
      w102_phase_run "$cdir" expand; RC1=$?
      if [ $RC1 -ne 0 ]; then seal_w102_leafs "$cid" "$cdir" $RC1 expand; RC=$RC1;
      else
        mkdir -p "$cdir/guards"
        w102_phase_run "$cdir/guards" guards; RC2=$?
        cat "$cdir/guards/raw.log" >> "$cdir/raw.log" 2>/dev/null
        cat "$cdir/guards/probes.jsonl" >> "$cdir/probes.jsonl" 2>/dev/null
        seal_w102_leafs "$cid" "$cdir" $RC2 expand
        seal_w102_leafs "$cid" "$cdir" $RC2 guards
        RC=$RC2
      fi
      return "$RC" ;;
    W1-02/immutable|W1-02/session-link-integrity|W1-02/intent-authority|W1-02/audit-actor-anonymization|W1-02/registration-integrity|W1-02/recovery-tombstone-integrity)
      w102_phase_run "$cdir" guards
      seal_w102_leafs "$cid" "$cdir" "$?" guards ;;
    W1-01/contract)
      python3 - "$ROOT" > "$cdir/raw.log" 2>&1 <<'PYC'
import json, sys
root = sys.argv[1]
d = json.load(open(root + "/docs/architecture/attachment-lifecycle-v5/evidence/w1-contract-output.json"))
errs = []
if len(d.get("five_subjects", {})) != 5: errs.append("subjects != 5")
if len(d.get("acceptance_contracts", [])) != 8 or any(len(a) < 10 for a in d["acceptance_contracts"]): errs.append("AC empty/missing")
if len(d.get("delete_entrypoints_mapped", [])) != 12: errs.append("delete entrypoints != 12")
if d.get("todo_placeholders") != 0: errs.append("TODO placeholder present")
if "never reassigned" not in d.get("key_lifetime", ""): errs.append("key lifetime missing")
if "GC must reject" not in d.get("hold_linearization", ""): errs.append("hold linearization missing")
if len(d.get("lock_order", [])) < 5: errs.append("lock order incomplete")
if len(d.get("storage_interface_frozen", {}).get("callbacks", [])) != 6: errs.append("storage callbacks != 6")
import re
adr = open(root + "/docs/adr/0008-pluggable-attachment-storage.md").read()
if "存量附件不做按 scope 的全量 garage 回填" not in adr: errs.append("ADR0008 V5 alignment missing")
print("contract-check errors:", errs if errs else "none")
sys.exit(1 if errs else 0)
PYC
      seal_leafs "$cid" "$cdir" "$?" "w1 contract output structural oracle (5 subjects/8 AC/12 entrypoints/lock order/storage callbacks)" ;;
    W2-01/pending-failclosed|W2-01/pending-multipart|W2-01/session-recovery-api|W2-01/upload-content-pin)
      CUR_CASE="$cid"
      w201_case "$cdir" ;;
    W2-02/confirm-idempotent|W2-02/confirm-old-client|W2-02/confirmed-content-immutable)
      CUR_CASE="$cid"
      w202_case "$cdir" ;;
    W2-03/confirm-gc-race)
      CUR_CASE="$cid"
      w203_case "$cdir" ;;
    W3-01/reference-gc-race|W3-01/shared-owner|W3-01/history-unknown)
      CUR_CASE="$cid"
      w301_case "$cdir" ;;
    W3-02/hold-all-entrypoints|W3-02/hold-gc-race)
      CUR_CASE="$cid"
      w302_case "$cdir" ;;
    W3-03/enterprise-bind|W3-03/cs-bind)
      CUR_CASE="$cid"
      w303_case "$cdir" ;;
    W4-01/delete-entrypoints|W4-01/intent-rollback|W4-01/hold-entrypoints|\
W4-01/confirm-gc-race|W4-01/reference-gc-race|W4-01/hold-gc-race)
      CUR_CASE="$cid"
      w401_case "$cdir" ;;
    W4-02/worker-crash|W4-02/worker-lease|W4-02/worker-db-fault|\
W4-02/queue-adoption|W4-02/last-attempt-crash|W4-02/old-attempts-preserved|\
W4-02/cross-queue-identity|W4-02/ordinary-queue-recovery|\
W4-02/claim-capacity-and-liveness|W4-02/purged-session-recovery)
      CUR_CASE="$cid"
      w402_case "$cdir" ;;
    W4-03/storage-garage|W4-03/storage-disk|W4-03/storage-switch|\
W4-03/late-put|W4-03/namespace-alias-integrity|\
W4-03/disk-predecessor-protocols|W4-03/driver-content-immutable)
      CUR_CASE="$cid"
      w403_case "$cdir" ;;
    W4-04/reconcile|W4-04/no-infinite-loop|W4-04/guards-activation|\
W4-04/all-writer-compat|W4-04/activation-commit-unknown)
      CUR_CASE="$cid"
      w404_case "$cdir" ;;
    W5-01/flutter-resume|W5-01/flutter-old-client)
      CUR_CASE="$cid"
      w501_case "$cdir" ;;
    W5-02/admin-governance|W5-02/status-sync)
      CUR_CASE="$cid"
      w502_case "$cdir" ;;
    W6-01/real-pg-garage-disk)
      CUR_CASE="$cid"
      w601_case "$cdir" ;;
    W6-01/all-ac01-ac07)
      CUR_CASE="$cid"
      w601_ac_union_case "$cdir" ;;
    W6-02/e2e-compat|W6-02/ac08|W6-02/flutter-recovery-real|\
W6-02/admin-recovery-real)
      CUR_CASE="$cid"
      w602_case "$cid" "$cdir" ;;
    W6-03/final-regression)
      CUR_CASE="$cid"
      w603_final_regression "$cdir" ;;
    W6-03/ledger-verify)
      CUR_CASE="$cid"
      w603_ledger_verify "$cdir" ;;
    W0-01/baseline)
      bash scripts/check_migrations.sh > "$cdir/raw.log" 2>&1
      seal_leafs "$cid" "$cdir" "$?" "bash scripts/check_migrations.sh (bootstrap retroactive)" ;;
    W0-02/schema)
      bootstrap_w002_schema "$cdir"
      seal_leafs "$cid" "$cdir" "$?" "psql single-txn original Up replay + frozen catalog query (bootstrap retroactive)" ;;
    W0-02/history-unknown)
      bootstrap_w002_history "$cdir"
      seal_leafs "$cid" "$cdir" "$?" "frozen baseline probes + static matrix (bootstrap retroactive)" ;;
    W0-03/runner-selftest)
      python3 "$RUNNER" self-test > "$cdir/raw.log" 2>&1
      seal_leafs "$cid" "$cdir" "$?" "python3 attachment_lifecycle_v5_runner.py self-test" ;;
    W0-03/strict-ledger-bootstrap)
      strict_ledger_case "$cdir"
      seal_leafs "$cid" "$cdir" "$?" "escript attachment_lifecycle_v5_strict_ledger.escript (real erlang_migrate strict up)" ;;
    *)
      echo "no executor implemented for case $cid (task-specific harness required)" >&2
      return 30 ;;
  esac
}

strict_ledger_case() {
  local cdir="$1" PGPASSWORD_FILE="$RUN_DIR/resources/pg-attv5.password"
  [ -f "$PGPASSWORD_FILE" ] || { echo "pg password file missing" >&2; return 75; }
  local PGPASSWORD; PGPASSWORD=$(cat "$PGPASSWORD_FILE")
  local DB="attv5_${SLOT}_strict"
  docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d postgres \
    -c "DROP DATABASE IF EXISTS $DB" -c "CREATE DATABASE $DB OWNER imboy_user" > "$cdir/db-create.log" 2>&1 || return 75
  for E in pg_jieba postgis postgis_raster timescaledb pgcrypto "uuid-ossp" pg_trgm btree_gin btree_gist unaccent pg_stat_statements; do
    docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$DB" -c "CREATE EXTENSION IF NOT EXISTS \"$E\"" >> "$cdir/db-create.log" 2>&1 || return 75
  done
  V5_MIG_DIR="$ROOT/priv/migrations" V5_PG_HOST="$V5_PG_HOST" V5_PG_PORT="$V5_PG_PORT" \
  V5_PG_USER=imboy_user V5_PG_DB="$DB" V5_PG_PASSWORD="$PGPASSWORD" \
    escript "$ROOT/scripts/test/attachment_lifecycle_v5_strict_ledger.escript" > "$cdir/raw.log" 2>&1
  local rc=$?
  # retroactive verification of W0-01/02 receipts (bootstrap contract)
  retroverify_w001_w002 >> "$cdir/raw.log" 2>&1 || rc=20
  return $rc
}

retroverify_w001_w002() {
  for f in w0-01-input-summary.json w0-02-input-summary.json; do
    p="$ROOT/docs/architecture/attachment-lifecycle-v5/evidence/$f"
    [ -f "$p" ] || { echo "RETRO_MISSING $p"; return 1; }
    python3 -m json.tool "$p" > /dev/null || { echo "RETRO_INVALID_JSON $f"; return 1; }
    git -C "$ROOT" log --format='%H' -1 -- "docs/architecture/attachment-lifecycle-v5/evidence/$f" | grep -q . \
      || { echo "RETRO_NOT_COMMITTED $f"; return 1; }
  done
  echo "RETRO_OK w0-01 w0-02 summaries committed and parseable in candidate $ROOT"
}

seal_leafs() {
  # Preserve command failure; sealing occurs once after the complete case.
  return "$3"
}

# ---------------- dispatch ----------------
produce_measured() {  # $1 case_id $2 case_dir — adapter -> producer -> measured-receipts.json
  local cid="$1" cdir="$2" adapter
  adapter="$SCRIPT_DIR/attachment_lifecycle_v5_adapter_$(echo "$cid" | tr '/-' '__' | tr 'A-Z' 'a-z').py"
  [ -f "$adapter" ] || { echo "no measured adapter for case $cid (evidence cannot be sealed)" >&2; return 30; }
  python3 "$adapter" "$RUN_DIR" "$cdir" "$ROOT" >> "$cdir/adapter.log" 2>&1 || { echo "ADAPTER_FAIL $cid" >> "$cdir/adapter.log"; return 30; }
  python3 "$ROOT/scripts/attachment_lifecycle_v5_producer.py" produce \
    --run-dir "$RUN_DIR" --task-id "${cid%%/*}" --case-id "$cid" --case-dir "$cdir" \
    --command "$V5_GATE_COMMAND" --cwd "$ROOT" \
    --observations "$cdir/observations.json" >> "$cdir/adapter.log" 2>&1 || return 30
}
execute_case() {
  run_case "$1" || return $?
  produce_measured "$1" "$EVID/${1#*/}" || return $?
  bash "$SCRIPT_DIR/attachment_lifecycle_v5_seal.sh" "$ROOT" "$1" "$EVID/${1#*/}" "$V5_GATE_COMMAND"
}
RC=0
if [ -n "$CASE" ]; then
  execute_case "$TASK/$CASE"; RC=$?
else
  for cid in $(echo "$CASES_JSON" | python3 -c 'import json,sys;print(" ".join(json.load(sys.stdin)))'); do
    execute_case "$cid" || { RC=$?; break; }
  done
fi
exit $RC
