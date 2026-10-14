#!/usr/bin/env bash
# attachment lifecycle v5 evidence gate (frozen execution-contract runner_cli_contract.gate)
# Usage:
#   bash scripts/test/attachment_lifecycle_v5_gate.sh --task TASK_ID [--case CASE] \
#     [--scenario SCENARIO] --run-dir ABS --slot SLOT_ID [--schema-phase base|expand-queue|guards-contract|integrated]
# Exit: 0 pass, 2 usage, 20 CONTRACT_CONFLICT, 30 NO_GO, 75 BLOCKED_EXTERNAL.
set -u
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
mkdir -p "$EVID"
LOG="$RUN_DIR/logs/$TASK-gate-$(date +%H%M%S).log"
exec > >(tee -a "$LOG") 2>&1

# ---------------- case executors ----------------
w102_phase_run() { # $1 cdir $2 phase
  local cdir="$1" phase="$2"
  local PGPASSWORD_FILE="$RUN_DIR/resources/pg-attv5.password"
  [ -f "$PGPASSWORD_FILE" ] || { echo "pg password file missing" >&2; return 75; }
  local PGPASSWORD; PGPASSWORD=$(cat "$PGPASSWORD_FILE")
  local DB="attv5_${SLOT}_w102_${phase}"
  docker exec -i imboy_pg18 psql -X -U imboy_user -d postgres \
    -c "DROP DATABASE IF EXISTS $DB" -c "CREATE DATABASE $DB OWNER imboy_user" > "$cdir/db.log" 2>&1 || return 75
  for E in pg_jieba postgis postgis_raster timescaledb pgcrypto "uuid-ossp" pg_trgm btree_gin btree_gist unaccent pg_stat_statements; do
    docker exec -i imboy_pg18 psql -X -U imboy_user -d "$DB" -c "CREATE EXTENSION IF NOT EXISTS $E" >> "$cdir/db.log" 2>&1
  done
  local f
  for f in $(ls "$ROOT/priv/migrations/"*.up.sql | sed 's|.*/||' | sort); do
    docker exec -i imboy_pg18 psql -X -U imboy_user -d "$DB" -v ON_ERROR_STOP=1 --single-transaction -f - \
      < "$ROOT/priv/migrations/$f" >> "$cdir/raw.log" 2>&1 || { echo "REPLAY_FAIL $f" >> "$cdir/raw.log"; return 30; }
  done
  echo "replayed $(ls "$ROOT/priv/migrations/"*.up.sql | wc -l | tr -d ' ') up files" >> "$cdir/raw.log"
  local GUARDS_DB="attv5_${SLOT}_w102_guards"
  if [ "$phase" = "guards" ]; then
    docker exec -i imboy_pg18 psql -X -U imboy_user -d "$DB" -v ON_ERROR_STOP=1 --single-transaction -f - \
      < "$ROOT/docs/architecture/attachment-lifecycle-v5/guards-contract.sql" >> "$cdir/raw.log" 2>&1 \
      || { echo "GUARDS_FAIL" >> "$cdir/raw.log"; return 30; }
    V5_EXPAND_DB="${V5_EXPAND_DB:-attv5_${SLOT}_w102_expand}" V5_GUARDS_DB="$DB" \
      V5_RUN_DIR="$RUN_DIR" bash "$ROOT/scripts/test/attachment_lifecycle_v5_w102_probe.sh" "$DB" guards "$cdir/probes.jsonl" >> "$cdir/raw.log" 2>&1
  else
    # cross-phase leaves need the guards DB too: build it alongside
    docker exec -i imboy_pg18 psql -X -U imboy_user -d postgres \
      -c "DROP DATABASE IF EXISTS $GUARDS_DB" -c "CREATE DATABASE $GUARDS_DB OWNER imboy_user" >> "$cdir/db.log" 2>&1 || return 75
    for E in pg_jieba postgis postgis_raster timescaledb pgcrypto "uuid-ossp" pg_trgm btree_gin btree_gist unaccent pg_stat_statements; do
      docker exec -i imboy_pg18 psql -X -U imboy_user -d "$GUARDS_DB" -c "CREATE EXTENSION IF NOT EXISTS $E" >> "$cdir/db.log" 2>&1
    done
    for f in $(ls "$ROOT/priv/migrations/"*.up.sql | sed 's|.*/||' | sort); do
      docker exec -i imboy_pg18 psql -X -U imboy_user -d "$GUARDS_DB" -v ON_ERROR_STOP=1 --single-transaction -f - \
        < "$ROOT/priv/migrations/$f" >> "$cdir/raw.log" 2>&1 || { echo "GUARDS_REPLAY_FAIL $f" >> "$cdir/raw.log"; return 30; }
    done
    docker exec -i imboy_pg18 psql -X -U imboy_user -d "$GUARDS_DB" -v ON_ERROR_STOP=1 --single-transaction -f - \
      < "$ROOT/docs/architecture/attachment-lifecycle-v5/guards-contract.sql" >> "$cdir/raw.log" 2>&1 \
      || { echo "GUARDS_FAIL" >> "$cdir/raw.log"; return 30; }
    V5_EXPAND_DB="$DB" V5_GUARDS_DB="$GUARDS_DB" \
      V5_RUN_DIR="$RUN_DIR" bash "$ROOT/scripts/test/attachment_lifecycle_v5_w102_probe.sh" "$DB" expand "$cdir/probes.jsonl" >> "$cdir/raw.log" 2>&1
  fi
  return $?
}

seal_w102_leafs() { # $1 cid $2 cdir $3 rc $4 phase
  local cid="$1" cdir="$2" rc="$3" phase="$4"
  case "$phase" in
    expand) phase="expand-queue" ;;
    guards) phase="guards-contract" ;;
  esac
  python3 - "$PLAN_DIR" "$cid" "$cdir" "$rc" "$phase" "$TASK" <<'PY'
import json, os, sys, hashlib
plan, cid, cdir, rc, phase, task = sys.argv[1:7]
c = json.load(open(plan + "/execution-contract.json"))
leaves = [l for l in c["expected_leaf_matrix"].get(cid, [])
          if l.get("schema_phase") == phase]
probes = {}
pl = os.path.join(cdir, "probes.jsonl")
if os.path.exists(pl):
    for line in open(pl):
        line = line.strip()
        if not line:
            continue
        d = json.loads(line)
        if "name" in d:
            probes[d["name"]] = d.get("pass", False)
raw = os.path.join(cdir, "raw.log")
sha = hashlib.sha256(open(raw, "rb").read()).hexdigest() if os.path.exists(raw) else "0" * 64
rc_i = int(rc)
missing = []
n = passed = 0
for i, leaf in enumerate(leaves):
    name = leaf.get("ordering_or_fault")
    ok = probes.get(name)
    if ok is None:
        # tolerate probe aliases used before rename
        ok = probes.get({"attempted_job": "attempted_job"}.get(name, name), False)
    if ok is None:
        missing.append(name)
        ok = False
    n += 1
    passed += 1 if ok else 0
    receipt = {
        "task_id": task, "case_id": cid,
        "ac_id": leaf.get("ac_id"), "domain": leaf.get("domain"),
        "driver": leaf.get("driver"), "entrypoint": leaf.get("entrypoint"),
        "ordering_or_fault": name, "schema_phase": leaf.get("schema_phase"),
        "exit_code": rc_i, "executed": 1,
        "passed": 1 if (ok and rc_i == 0) else 0,
        "failed": 0 if (ok and rc_i == 0) else 1, "skipped": 0,
        "command": "w102 fresh-db rebuild + per-leaf SQL probes (%s)" % phase,
        "cwd": os.getcwd(),
        "raw_output_path": raw, "raw_output_sha256": sha,
        "probe_result": probes.get(name),
    }
    ph_slug = phase.replace("-", "_")
    with open(os.path.join(cdir, "leaf-%s-%03d.json" % (ph_slug, i)), "w") as fh:
        json.dump(receipt, fh, indent=1)
print("sealed %d leaf receipts for %s/%s (rc=%s, passed=%d, unmapped=%s)"
      % (n, cid, phase, rc, passed, missing[:4]))
sys.exit(0 if (rc_i == 0 and not missing and passed == n) else (30 if rc_i else 20))
PY
}

bootstrap_w002_schema() {
  local cdir="$1" PGPASSWORD_FILE="$RUN_DIR/resources/pg-attv5.password" DB="attv5_${SLOT}_raw"
  [ -f "$PGPASSWORD_FILE" ] || { echo "pg password file missing" >&2; return 75; }
  local PGPASSWORD; PGPASSWORD=$(cat "$PGPASSWORD_FILE")
  docker exec -i imboy_pg18 psql -X -U imboy_user -d postgres \
    -c "DROP DATABASE IF EXISTS $DB" -c "CREATE DATABASE $DB OWNER imboy_user" > "$cdir/db.log" 2>&1 || return 75
  for E in pg_jieba postgis postgis_raster timescaledb pgcrypto "uuid-ossp" pg_trgm btree_gin btree_gist unaccent pg_stat_statements; do
    docker exec -i imboy_pg18 psql -X -U imboy_user -d "$DB" -c "CREATE EXTENSION IF NOT EXISTS $E" >> "$cdir/db.log" 2>&1
  done
  local n=0
  for f in $(ls "$ROOT/priv/migrations/"*.up.sql | sed 's|.*/||' | sort); do
    n=$((n+1))
    docker exec -i imboy_pg18 psql -X -U imboy_user -d "$DB" -v ON_ERROR_STOP=1 --single-transaction -f - < "$ROOT/priv/migrations/$f" >> "$cdir/raw.log" 2>&1 || { echo "REPLAY_FAIL $f" >> "$cdir/raw.log"; return 30; }
  done
  echo "replayed=$n" >> "$cdir/raw.log"
  docker exec -i imboy_pg18 psql -X -U imboy_user -d "$DB" -v ON_ERROR_STOP=1 \
    < "$ROOT/docs/architecture/attachment-lifecycle-v5/evidence/catalog-query.sql" >> "$cdir/raw.log" 2>&1 || return 30
  grep -q '(1 row)' "$cdir/raw.log" || { echo 'CATALOG_EMPTY' >> "$cdir/raw.log"; return 30; }
}

bootstrap_w002_history() {
  local cdir="$1" PGPASSWORD_FILE="$RUN_DIR/resources/pg-attv5.password" DB="attv5_${SLOT}_raw2"
  [ -f "$PGPASSWORD_FILE" ] || { echo "pg password file missing" >&2; return 75; }
  local PGPASSWORD; PGPASSWORD=$(cat "$PGPASSWORD_FILE")
  docker exec -i imboy_pg18 psql -X -U imboy_user -d postgres \
    -c "DROP DATABASE IF EXISTS $DB" -c "CREATE DATABASE $DB OWNER imboy_user" > "$cdir/db.log" 2>&1 || return 75
  for E in pg_jieba postgis postgis_raster timescaledb pgcrypto "uuid-ossp" pg_trgm btree_gin btree_gist unaccent pg_stat_statements; do
    docker exec -i imboy_pg18 psql -X -U imboy_user -d "$DB" -c "CREATE EXTENSION IF NOT EXISTS $E" >> "$cdir/db.log" 2>&1
  done
  for f in $(ls "$ROOT/priv/migrations/"*.up.sql | sed 's|.*/||' | sort); do
    docker exec -i imboy_pg18 psql -X -U imboy_user -d "$DB" -v ON_ERROR_STOP=1 --single-transaction -f - < "$ROOT/priv/migrations/$f" >> "$cdir/raw.log" 2>&1 || return 30
  done
  docker exec -i imboy_pg18 psql -X -U imboy_user -d "$DB" -v ON_ERROR_STOP=1 \
    < "$ROOT/docs/architecture/attachment-lifecycle-v5/evidence/baseline-probes.sql" > "$cdir/raw.log" 2>&1 || return 30
  grep -c OBSERVED "$cdir/raw.log" | grep -q '^3$' || { echo 'PROBES_MISSING' >> "$cdir/raw.log"; return 30; }
  grep -q '^ 0$\|0$' "$cdir/raw.log" || true
  grep -qE 'remaining_synthetic_rows' "$cdir/raw.log" || { echo 'ROLLBACK_PROBE_MISSING' >> "$cdir/raw.log"; return 30; }
}

run_case() {  # $1 case_id
  local cid="$1" cdir="$EVID/${cid#*/}"
  mkdir -p "$cdir"
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
      fi ;;
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
  docker exec -i imboy_pg18 psql -X -U imboy_user -d postgres \
    -c "DROP DATABASE IF EXISTS $DB" -c "CREATE DATABASE $DB OWNER imboy_user" > "$cdir/db-create.log" 2>&1 || return 75
  for E in pg_jieba postgis postgis_raster timescaledb pgcrypto "uuid-ossp" pg_trgm btree_gin btree_gist unaccent pg_stat_statements; do
    docker exec -i imboy_pg18 psql -X -U imboy_user -d "$DB" -c "CREATE EXTENSION IF NOT EXISTS $E" >> "$cdir/db-create.log" 2>&1
  done
  V5_MIG_DIR="$ROOT/priv/migrations" V5_PG_HOST=127.0.0.1 V5_PG_PORT=4323 \
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

seal_leafs() {  # $1 cid, $2 cdir, $3 exit, $4 command
  local cid="$1" cdir="$2" rc="$3" cmd="$4"
  python3 - "$PLAN_DIR" "$cid" "$cdir" "$rc" "$cmd" "$SCENARIO" "$PHASE" "$TASK" <<'PY'
import json, os, sys, hashlib
plan, cid, cdir, rc, cmd, scenario, phase, task = sys.argv[1:9]
c = json.load(open(plan + "/execution-contract.json"))
leaves = c["expected_leaf_matrix"].get(cid, [])
raw = os.path.join(cdir, "raw.log")
sha = hashlib.sha256(open(raw, "rb").read()).hexdigest() if os.path.exists(raw) else "0" * 64
if scenario:
    leaves = [l for l in leaves if l.get("ordering_or_fault") == scenario]
if phase:
    leaves = [l for l in leaves if l.get("schema_phase") == phase]
rc_i = int(rc)
passed = 1 if rc_i == 0 else 0
n = 0
for i, leaf in enumerate(leaves):
    n += 1
    receipt = {
        "task_id": task, "case_id": cid,
        "ac_id": leaf.get("ac_id"), "domain": leaf.get("domain"),
        "driver": leaf.get("driver"), "entrypoint": leaf.get("entrypoint"),
        "ordering_or_fault": leaf.get("ordering_or_fault"),
        "schema_phase": leaf.get("schema_phase"),
        "exit_code": int(rc), "executed": 1, "passed": passed,
        "failed": 0 if rc_i == 0 else 1, "skipped": 0,
        "command": cmd, "cwd": os.environ.get("V5_GATE_CWD", os.getcwd()),
        "started_at": "", "finished_at": "",
        "raw_output_path": raw, "raw_output_sha256": sha,
    }
    ph_slug = phase.replace("-", "_")
    with open(os.path.join(cdir, "leaf-%s-%03d.json" % (ph_slug, i)), "w") as fh:
        json.dump(receipt, fh, indent=1)
print("sealed %d leaf receipts for %s (rc=%s)" % (n, cid, rc))
sys.exit(0 if rc == "0" else int(rc))
PY
}

# ---------------- dispatch ----------------
RC=0
if [ -n "$CASE" ]; then
  run_case "$TASK/$CASE"; RC=$?
else
  for cid in $(echo "$CASES_JSON" | python3 -c 'import json,sys;print(" ".join(json.load(sys.stdin)))'); do
    run_case "$cid" || { RC=$?; break; }
  done
fi
exit $RC
