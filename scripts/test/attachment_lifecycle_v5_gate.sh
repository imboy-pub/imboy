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
    docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$DB" -v ON_ERROR_STOP=1 --single-transaction -f - \
      < "$ROOT/docs/architecture/attachment-lifecycle-v5/guards-contract.sql" >> "$cdir/raw.log" 2>&1 \
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
    docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$GUARDS_DB" -v ON_ERROR_STOP=1 --single-transaction -f - \
      < "$ROOT/docs/architecture/attachment-lifecycle-v5/guards-contract.sql" >> "$cdir/raw.log" 2>&1 \
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
  for f in $(ls "$ROOT/priv/migrations/"*.up.sql | sed 's|.*/||' | sort); do
    docker exec -i "$PG_CONTAINER" psql -X -U imboy_user -d "$DB" -v ON_ERROR_STOP=1 --single-transaction -f - < "$ROOT/priv/migrations/$f" >> "$cdir/raw.log" 2>&1 || return 30
  done
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
