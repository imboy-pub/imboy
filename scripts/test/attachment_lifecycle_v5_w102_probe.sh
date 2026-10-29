#!/usr/bin/env bash
# W1-02 per-leaf SQL probes against a real expand-phase scratch database.
# Usage: probe.sh <DB> <phase:expand|guards> <outfile>
# Each probe prints one JSON line {"name":..., "pass":bool}; gate seals them
# into leaf receipts keyed by ordering_or_fault.
#
# W1-02 closeout revision: every leaf below is bound to an executed action
# (real migration up/down replay, real function invocation under the real
# role, real trigger/constraint rejection, or an injected-and-rolled-back
# fault).  Read-only constants, CASE-only predicates and "SELECT 'ok'" rows
# were replaced by the actions they claimed to measure; no assertion was
# removed.
set -u
DB="$1"; PHASE="$2"; OUT="$3"
RUN_DIR="${V5_RUN_DIR:?explicit run required}"
SLOT="${V5_SLOT:?explicit fixture slot required}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
MIG_DIR="$ROOT/priv/migrations"
GUARDS_SQL="$ROOT/docs/architecture/attachment-lifecycle-v5/guards-contract.sql"
# Dedicated rollback-cycle scratch database (allowlisted name, gate-unused):
# real M1/M2 up/down/up replay with attributable single-fixture down blocks.
CYCLE_DB="${V5_CYCLE_DB:-attv5_${SLOT}_w102_queue}"
EXPAND_DB="${V5_EXPAND_DB:-attv5_${SLOT}_w102_expand}"
GUARDS_DB="${V5_GUARDS_DB:-$DB}"
[ "$EXPAND_DB" != "$GUARDS_DB" ] || {
  echo "CONTRACT_CONFLICT: expand and guards databases must be distinct" >&2; exit 20;
}
[ "$CYCLE_DB" != "$DB" ] && [ "$CYCLE_DB" != "$EXPAND_DB" ] && [ "$CYCLE_DB" != "$GUARDS_DB" ] || {
  echo "CONTRACT_CONFLICT: cycle database must be distinct" >&2; exit 20;
}
# Never inherit a caller-selected SQL target; only this script switches to CYCLE_DB.
unset PROBE_DB
# Allowlist preflight before Docker: the main database plus any explicitly
# overridden target must already be authorized in the run receipt. The default
# cycle/expand names are internal constants authorized with the run itself.
DB_CHECKS=("$DB")
[ -z "${V5_EXPAND_DB:-}" ] || DB_CHECKS+=("$V5_EXPAND_DB")
[ -z "${V5_GUARDS_DB:-}" ] || DB_CHECKS+=("$V5_GUARDS_DB")
[ -z "${V5_CYCLE_DB:-}" ] || DB_CHECKS+=("$V5_CYCLE_DB")
python3 - "$SCRIPT_DIR" "$RUN_DIR" "$SLOT" "${DB_CHECKS[@]}" <<'PYDB' || exit 20
import hashlib
import json
from pathlib import Path
import sys

sys.path.insert(0, sys.argv[1])
from attachment_lifecycle_v5_pg_guard import authorized_database

try:
    root = Path(sys.argv[2]).resolve()
    state = json.loads((root / "state.json").read_text())
    resource = json.loads((root / "resources/postgres.json").read_text())
    raw = Path(state["authorization_path"]).read_bytes()
    if hashlib.sha256(raw).hexdigest() != state["authorization_sha256"]:
        raise ValueError("authorization changed")
    allowlist = json.loads(raw)["resource_allowlist"]
    for database in sys.argv[4:]:
        authorized_database(allowlist, resource["allowed_slots"], sys.argv[3], database)
except (OSError, ValueError, KeyError, TypeError):
    print("CONTRACT_CONFLICT: W1-02 databases not explicitly authorized", file=sys.stderr)
    sys.exit(20)
PYDB
PG_RESOURCE="$(python3 "$SCRIPT_DIR/attachment_lifecycle_v5_pg_guard.py" "$RUN_DIR" "$SLOT" "$DB")" || exit 20
IFS=$'\t' read -r PG_CONTAINER PG_HOST PG_PORT <<< "$PG_RESOURCE"
# Bind the audited connection identity before any exec: the loopback endpoint
# must actually be live, otherwise the probe refuses to touch the container.
python3 - "$PG_HOST" "$PG_PORT" <<'PYTCP' || exit 20
import socket, sys
s = socket.socket()
s.settimeout(5)
try:
    s.connect((sys.argv[1], int(sys.argv[2])))
except OSError:
    print("BLOCKED_EXTERNAL: isolated PostgreSQL endpoint is not reachable", file=sys.stderr)
    sys.exit(20)
finally:
    s.close()
PYTCP
PROBE_TMP="$(mktemp -d "$RUN_DIR/resources/w102-probe.XXXXXX")" || exit 20
WRITER_JOB=""
cleanup() {
  [ -n "$WRITER_JOB" ] && kill "$WRITER_JOB" 2>/dev/null
  docker exec -i -e PGPASSWORD="$PASS" "$PG_CONTAINER" psql -X -U imboy_user -d postgres \
    -q -c "DROP DATABASE IF EXISTS \"$CYCLE_DB\"" >/dev/null 2>&1
  rm -f "$PROBE_TMP/out.txt" "$PROBE_TMP/err.txt" "$PROBE_TMP/writer.log"
  rmdir "$PROBE_TMP" 2>/dev/null
}
PASS="$(cat "$RUN_DIR/resources/pg-attv5.password")" || exit 20
export PGPASSWORD="$PASS"
trap cleanup EXIT
PSQL() { docker exec -i -e PGPASSWORD="$PASS" "$PG_CONTAINER" psql -X -U imboy_user -d "${PROBE_DB:-$DB}" -v ON_ERROR_STOP=1 "$@"; }
ONDB() { local db="$1"; shift; docker exec -i -e PGPASSWORD="$PASS" "$PG_CONTAINER" psql -X -U imboy_user -d "$db" -v ON_ERROR_STOP=1 "$@"; }
PSQL_RAW() { docker exec -i -e PGPASSWORD="$PASS" "$PG_CONTAINER" psql -X -U imboy_user -d "${PROBE_DB:-$DB}" -v ON_ERROR_STOP=1 "$@"; }
: > "$OUT"

ok=0; bad=0
sanitize() { printf '%s' "$1" | tr '\n\r\t' '   ' | tr -d '"\\' | cut -c1-220; }
emit() { # name pass detail  (every row carries the seeded storage driver)
  local name="$1" good="$2" detail; detail="$(sanitize "${3:-}")"
  if [ "$good" = 1 ]; then ok=$((ok+1)); printf '{"name":"%s","pass":true,"driver":"%s"}\n' "$name" "${V5_PROBE_DRIVER:-}" >> "$OUT"
  else bad=$((bad+1)); printf '{"name":"%s","pass":false,"detail":"%s","driver":"%s"}\n' "$name" "$detail" "${V5_PROBE_DRIVER:-}" >> "$OUT"; fi
}
probe() { # name expected(fail=unverified / err:<fragment> / else must-equal-value) sql
  local name="$1" expected="$2" sql="$3" rc val
  printf '%s\n' "$sql" | PSQL_RAW -tAq > "$PROBE_TMP/out.txt" 2>"$PROBE_TMP/err.txt"
  rc=$?
  if [ "$expected" = "fail" ]; then
    emit "$name" 0 "unverified rejection: exact SQLSTATE and state assertions required (rc=$rc)"
    return
  fi
  case "$expected" in
    err:*)
      local frag="${expected#err:}"
      if [ $rc -ne 0 ] && grep -qF -- "$frag" "$PROBE_TMP/err.txt"; then
        ok=$((ok+1)); echo "{\"name\":\"$name\",\"pass\":true,\"driver\":\"${V5_PROBE_DRIVER:-}\"}" >> "$OUT"
      else
        bad=$((bad+1)); echo "{\"name\":\"$name\",\"pass\":false,\"detail\":\"want error containing [$frag] rc=$rc got $(sanitize "$(tail -2 "$PROBE_TMP/err.txt" | tr '\n' ' ')")\",\"driver\":\"${V5_PROBE_DRIVER:-}\"}" >> "$OUT"
      fi
      return ;;
  esac
  val="$(tail -1 "$PROBE_TMP/out.txt")"
  if [ $rc -eq 0 ] && [ "$val" = "$expected" ]; then ok=$((ok+1)); echo "{\"name\":\"$name\",\"pass\":true,\"driver\":\"${V5_PROBE_DRIVER:-}\"}" >> "$OUT"
  else bad=$((bad+1)); echo "{\"name\":\"$name\",\"pass\":false,\"detail\":\"rc=$rc val=$(sanitize "$val") want=$(sanitize "$expected") $(sanitize "$(head -c160 "$PROBE_TMP/err.txt")")\",\"driver\":\"${V5_PROBE_DRIVER:-}\"}" >> "$OUT"; fi
}
sqlval() { printf '%s\n' "$1" | PSQL_RAW -tAq 2>/dev/null | tail -1; }
sqlrc() { printf '%s\n' "$1" | PSQL_RAW -tAq >"$PROBE_TMP/out.txt" 2>"$PROBE_TMP/err.txt"; }
now_ms() { python3 -c 'import time;print(int(time.time()*1000))'; }

# ============ real migration replay helpers ============
M_UP178="$MIG_DIR/00000178_attachment_lifecycle_v5_expand.up.sql"
M_UP179="$MIG_DIR/00000179_attachment_lifecycle_v5_queue.up.sql"
M_DOWN178="$MIG_DIR/00000178_attachment_lifecycle_v5_expand.down.sql"
M_DOWN179="$MIG_DIR/00000179_attachment_lifecycle_v5_queue.down.sql"
for f in "$M_UP178" "$M_UP179" "$M_DOWN178" "$M_DOWN179"; do [ -f "$f" ] || exit 20; done
DOWN178="$(cat "$M_DOWN178")"
DOWN179="$(cat "$M_DOWN179")"
UP178="$(cat "$M_UP178")"
UP179="$(cat "$M_UP179")"
replay_file() { # db file  -> rc
  ONDB "$1" --single-transaction -q -f - < "$2" >"$PROBE_TMP/out.txt" 2>"$PROBE_TMP/err.txt"
}
replay_all_upto() { # db last-prefix-exclusive (files < prefix)
  local db="$1" upto="$2" f
  ONDB "$db" -q -c "DROP DATABASE IF EXISTS \"$db\"" >/dev/null 2>&1 || true
  ONDB postgres -q -c "DROP DATABASE IF EXISTS \"$db\"" -c "CREATE DATABASE \"$db\" OWNER imboy_user" >/dev/null 2>>"$PROBE_TMP/err.txt" || return 1
  local exts="pg_jieba postgis postgis_raster timescaledb pgcrypto uuid-ossp pg_trgm btree_gin btree_gist unaccent pg_stat_statements" e
  for e in $exts; do ONDB "$db" -q -c "CREATE EXTENSION IF NOT EXISTS \"$e\"" >/dev/null 2>&1 || return 1; done
  for f in $(ls "$MIG_DIR"/*.up.sql | sed 's|.*/||' | sort | awk -v u="$upto" '$0 < u'); do
    ONDB "$db" --single-transaction -q -f - < "$MIG_DIR/$f" >>"$PROBE_TMP/out.txt" 2>>"$PROBE_TMP/err.txt" || return 1
  done
  return 0
}
down_blocked() { # db file -> 0 when refused by the frozen V5 down predicate
  local rc
  replay_file "$1" "$2"; rc=$?
  [ $rc -ne 0 ] && grep -qF 'V5_DOWN_BLOCKED_USE_FORWARD_FIX' "$PROBE_TMP/err.txt"
}
down_applied() { # db file -> 0 when the down applies on empty state
  replay_file "$1" "$2"
}
cycle_q() { ONDB "$CYCLE_DB" -tAq -c "$1" 2>/dev/null | tail -1; }

# ---------------- shared fixtures ----------------
seed() {
PSQL -q <<'SQL'
BEGIN;
INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id)
SELECT 'v5p/seed/1','b1','private',101 WHERE NOT EXISTS (SELECT 1 FROM attach_pending WHERE object_key='v5p/seed/1');
INSERT INTO attachment(id,file_hash256,mime_type,ext,name,path,url,size,scope,creator_user_id,referer_time,status)
SELECT 900000101,'h1','image/png','png','a.png','v5p/seed/1','',10,'private',101,0,0
WHERE NOT EXISTS (SELECT 1 FROM attachment WHERE id=900000101);
INSERT INTO "user"(id,account,password,reg_ip,reg_cosv)
SELECT 101,'v5probe101','x','127.0.0.1','probe' WHERE NOT EXISTS (SELECT 1 FROM "user" WHERE id=101);
INSERT INTO "user"(id,account,password,reg_ip,reg_cosv)
SELECT 102,'v5probe102','x','127.0.0.1','probe' WHERE NOT EXISTS (SELECT 1 FROM "user" WHERE id=102);
INSERT INTO "user"(id,account,password,reg_ip,reg_cosv)
SELECT 202,'v5probe202','x','127.0.0.1','probe' WHERE NOT EXISTS (SELECT 1 FROM "user" WHERE id=202);
INSERT INTO "user"(id,account,password,reg_ip,reg_cosv)
SELECT 99000101,'v5actor','x','127.0.0.1','probe' WHERE NOT EXISTS (SELECT 1 FROM "user" WHERE id=99000101);
INSERT INTO "user"(id,account,password,reg_ip,reg_cosv)
SELECT 99000102,'v5actor2','x','127.0.0.1','probe' WHERE NOT EXISTS (SELECT 1 FROM "user" WHERE id=99000102);
INSERT INTO organization(id,name,owner_id)
SELECT 98000101,'v5probe-org',101 WHERE NOT EXISTS (SELECT 1 FROM organization WHERE id=98000101);
INSERT INTO workspace(organization_id,id,name,owner_id)
SELECT 98000101,97000101,'v5probe-ws',101 WHERE NOT EXISTS (SELECT 1 FROM workspace WHERE id=97000101);
INSERT INTO enterprise_application(id,organization_id,application_key,name)
SELECT 96000101,98000101,'v5probe-app','probe' WHERE NOT EXISTS (SELECT 1 FROM enterprise_application WHERE id=96000101);
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status,uploaded_by_user_id)
SELECT 910000101,98000101,97000101,'v5e/actor','v5hash','active',101
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=910000101);
COMMIT;
SQL
}
seed_enterprise_extras() {
PSQL -q > /dev/null 2>&1 <<SQL
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status,storage_driver,storage_namespace,storage_container,storage_locator_sha256)
SELECT 910000102,98000101,97000101,'v5e/sub1','v5hash','pending_confirm',101
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=910000102);
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status,storage_driver,storage_namespace,storage_container,storage_locator_sha256)
SELECT 910000103,98000101,97000101,'v5e/auth','v5hash','active','$ST_DRV','$ST_NS','$ST_CT','$ST_LOC'
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=910000103);
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status)
SELECT v.id,98000101,97000101,v.key,'v5hash','active'
FROM (VALUES (910000104,'v5e/liveref'),(910000105,'v5e/hold'),(910000106,'v5e/retain'),
             (910000107,'v5e/auditfail'),(910000108,'v5e/metafail'),(910000109,'v5e/sub2'),
             (910000110,'v5e/sub3'),(910000111,'v5e/sub4'),(910000112,'v5e/legacy'))
     AS v(id,key)
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset e WHERE e.id=v.id);
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status)
SELECT 910000999,98000101,97000101,'v5e/widget','v5hash','pending_confirm'
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=910000999);
SQL
  # per-run EB fixtures (disjoint 910xxx band): never touched by the EB
  # registration guard's immutable registration columns, so no reset UPDATE
  # is needed before fn_v5_register_eb.
  PSQL -q > /dev/null 2>&1 <<SQL
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status)
SELECT $EA_REG,98000101,97000101,'v5e/reg1-$RUN_TAG','v5hash','pending_confirm'
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=$EA_REG);
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status,storage_driver,storage_namespace,storage_container,storage_locator_sha256)
SELECT $EA_REG2,98000101,97000101,'v5e/reg2-$RUN_TAG','v5hash','active','$ST_DRV','$ST_NS','$ST_CT','$ST_LOC'
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=$EA_REG2);
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status)
SELECT $EA_LEGACY,98000101,97000101,'v5e/legacy-$RUN_TAG','v5hash','active'
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=$EA_LEGACY);
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status,storage_driver,storage_namespace,storage_container,storage_locator_sha256)
SELECT $EA_AUTH,98000101,97000101,'v5e/auth-$RUN_TAG','v5hash','active','$ST_DRV','$ST_NS','$ST_CT','$ST_LOC'
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=$EA_AUTH);
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status)
SELECT $EA_SUB1,98000101,97000101,'v5e/sub1-$RUN_TAG','v5hash','pending_confirm'
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=$EA_SUB1);
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status)
SELECT $EA_SUB2,98000101,97000101,'v5e/sub2-$RUN_TAG','v5hash','active'
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=$EA_SUB2);
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status)
SELECT $EA_SUB3,98000101,97000101,'v5e/sub3-$RUN_TAG','v5hash','active'
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=$EA_SUB3);
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status)
SELECT $EA_SUB4,98000101,97000101,'v5e/sub4-$RUN_TAG','v5hash','active'
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=$EA_SUB4);
SQL
}

REG='{"subject":{"kind":"user","principal":"101"},"declaration":{"filename":"a.png","mime":"image/png","size_bytes":"10","scope":"private"}}'
REG202='{"subject":{"kind":"user","principal":"202"},"declaration":{"filename":"a.png","mime":"image/png","size_bytes":"10","scope":"private"}}'
CONTENT='{"version":1,"sha256":"'"$(printf 'a%.0s' {1..64})"'","size_bytes":"10","mime":"image/png"}'
LOC64="$(printf 'b%.0s' {1..64})"
LOCA="$(printf 'a%.0s' {1..64})"
LOCB="$(printf 'c%.0s' {1..64})"
# W1-02 closeout: storage-driver parameterization for fixture seeding.
# V5_PROBE_DRIVER=garage (default) reproduces every existing identity literal
# verbatim; V5_PROBE_DRIVER=disk seeds the positive fixtures with the
# consistent ('disk','v5nsd','v5cd') triple plus a locator digest derived by
# the same rule as $LOC64 (printf '<hex char>%.0s' {1..64}); 'f' is 64-hex
# conformant and disjoint from every existing locator constant.  The
# allowlist-style case below is also the JSON-safety guarantee for the
# "driver" key emitted on every probes.jsonl row (garage|disk only, no
# quoting/backslash/control characters possible).  Negative literals
# (LOCA/LOCB mismatch pairs, partial/wrong-driver arguments and the
# fixture-internal ns/ct labels like ns1/c1/v5bf) keep their own semantics in
# both modes; only their driver component follows V5_PROBE_DRIVER.
V5_PROBE_DRIVER="${V5_PROBE_DRIVER:-garage}"
case "$V5_PROBE_DRIVER" in
  garage) ST_DRV='garage'; ST_NS='v5ns'; ST_CT='v5c'; ST_LOC="$LOC64" ;;
  disk)   ST_DRV='disk';   ST_NS='v5nsd'; ST_CT='v5cd'; ST_LOC="$(printf 'f%.0s' {1..64})" ;;
  *) echo "bad V5_PROBE_DRIVER='$V5_PROBE_DRIVER' (garage|disk)" >&2; exit 2 ;;
esac
# Per-run unique fixture namespace: guards DBs survive across manual probe
# invocations (the gate always rebuilds them, but idempotent probes must not
# depend on that), and V5 guards forbid DELETE cleanup of the permanent rows
# earlier rounds leave behind.  Unique keys/paths/ids per invocation replace
# DELETE-and-reuse seeding entirely.
RUN_TAG="$(date +%H%M%S)$(printf '%04d' $((RANDOM % 10000)))"
ID_OFF=$((10#${RUN_TAG: -6} * 7))
# Disjoint id bands per fixture group (all offsets share ID_OFF so ids inside a
# run stay pairwise distinct): 910xxxxx enterprise assets (EA_*), 920xxxxx
# backfill batch, 930xxxxx session-link attachments, 940xxxxx GC-link
# attachment.  Fixed ids 900000101..900000334 stay reserved for the base seeds.
EA_OFF=$((ID_OFF % 90000))
EA_REG=$((910000102 + EA_OFF))
EA_REG2=$((910000103 + EA_OFF))
EA_LEGACY=$((910000112 + EA_OFF))
EA_AUTH=$((910000200 + EA_OFF))
EA_HOLD_ASSET=$((910000130 + EA_OFF))
EA_SUB1=$((910000300 + EA_OFF))
EA_SUB2=$((910000301 + EA_OFF))
EA_SUB3=$((910000310 + EA_OFF))
EA_SUB4=$((910000311 + EA_OFF))
EA_WIDGET=$((910000999 + EA_OFF))
GID=$((940000001 + ID_OFF % 100000))
LID1=$((930000001 + ID_OFF % 100000))
LID2=$((930000002 + ID_OFF % 100000))
LID3=$((930000003 + ID_OFF % 100000))
LID4=$((930000004 + ID_OFF % 100000))
LID5=$((930000005 + ID_OFF % 100000))
LID6=$((930000006 + ID_OFF % 100000))

# ============ migration (both phases) ============
migration_probes() {
seed
seed_enterprise_extras
probe base_read_compat "4|3" "SELECT (SELECT count(*) FROM information_schema.columns WHERE table_name='attach_pending' AND column_name IN ('client_upload_id','upload_registration','upload_content','session_state'))::text||'|'||(SELECT count(*) FROM information_schema.columns WHERE table_name='attachment_object_deletion' AND column_name IN ('job_state','lease_token','storage_driver'))::text"
probe pk_fk_attempts_preserved "t" "SELECT convalidated FROM pg_constraint WHERE conname='attachment_object_deletion_pkey'"
probe pk_fk_attempts_preserved2 "0" "SELECT count(*) FROM pg_constraint WHERE conname IN ('workspace_attachment_retention_attachment_id_fkey','enterprise_asset_delete_queue_org_ws_asset_pkey') AND NOT convalidated"
probe ddl_rollback "fail" "DO \$\$ BEGIN ALTER TABLE attach_pending ADD COLUMN v5_probe_col int; RAISE EXCEPTION 'injected ddl fault'; END \$\$"
probe ddl_rollback2 "0" "SELECT count(*) FROM information_schema.columns WHERE table_name='attach_pending' AND column_name='v5_probe_col'"
probe partial_location_null_rejected "fail" "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace) VALUES (900000102,'v5p/x1','private',101,'garage','ns1')"
probe recovery_id_unique_owner_conflict "fail" "INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,client_upload_id) VALUES ('v5p/u2','b','private',202,'11111111-1111-1111-1111-111111111111'); INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,client_upload_id) VALUES ('v5p/u3','b','private',303,'11111111-1111-1111-1111-111111111111')"
probe renew_expiry_monotonic_and_deadline_fixed "fail" "INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,client_upload_id,upload_registration,session_deadline,upload_expires_at) VALUES ('v5p/u4','b','private',202,'22222222-2222-2222-2222-222222222222','$REG',now()+interval '1 hour',now()+interval '2 hours')"

# --- real: protected legacy NULL session.  A legacy writer creates a row with
# no V5 identity/location; every V5 entry point must refuse to attach a
# capability and must leave the row byte-identical.  The refusal must carry
# exact SQLSTATE 22023, and the FULL row (to_jsonb: location, deadline, expiry,
# identity and state columns) is snapshotted before/after to prove no field
# moved.
PSQL -q > /dev/null 2>&1 <<'SQL'
INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id)
SELECT 'v5p/legacy-null','b','private',101
WHERE NOT EXISTS (SELECT 1 FROM attach_pending WHERE object_key='v5p/legacy-null');
SQL
LEGACY_SNAP1="$(sqlval "SELECT md5(to_jsonb(p)::text) FROM attach_pending p WHERE object_key='v5p/legacy-null'")"
sqlrc "DO \$v5p\$ DECLARE v_state text; v_msg text; BEGIN
  BEGIN
    PERFORM fn_v5_pin_pending_content('v5p/legacy-null','$CONTENT'::jsonb);
  EXCEPTION WHEN OTHERS THEN
    GET STACKED DIAGNOSTICS v_state = RETURNED_SQLSTATE, v_msg = MESSAGE_TEXT;
    IF v_state <> '22023' OR v_msg NOT LIKE '%v5 upload frozen location unavailable%' THEN
      RAISE EXCEPTION 'unexpected legacy-null refusal: state=% msg=%', v_state, v_msg USING ERRCODE='P0001';
    END IF;
    RETURN;
  END;
  RAISE EXCEPTION 'legacy NULL session pin unexpectedly accepted' USING ERRCODE='P0001';
END \$v5p\$;
SELECT '22023:refused'"
LEGACY_RC=$?
LEGACY_OUT="$(tail -1 "$PROBE_TMP/out.txt")"
LEGACY_SNAP2="$(sqlval "SELECT md5(to_jsonb(p)::text) FROM attach_pending p WHERE object_key='v5p/legacy-null'")"
if [ "$LEGACY_RC" -eq 0 ] && [ "$LEGACY_OUT" = "22023:refused" ] && [ -n "$LEGACY_SNAP1" ] && \
   [ "$LEGACY_SNAP1" = "$LEGACY_SNAP2" ] && \
   [ "$(sqlval "SELECT count(*) FROM attach_pending WHERE object_key='v5p/legacy-null' AND client_upload_id IS NULL AND upload_registration IS NULL AND upload_content IS NULL AND session_state='registered' AND storage_driver IS NULL AND session_deadline IS NULL AND upload_expires_at IS NULL")" = "1" ]; then
  emit old_null_session_protected 1 "legacy NULL session: fn_v5_pin_pending_content refused with exact SQLSTATE 22023 ('v5 upload frozen location unavailable'); full-row to_jsonb snapshot identical before/after (location/deadline/expiry/identity untouched, no capability attached)"
else
  emit old_null_session_protected 0 "rc=$LEGACY_RC out=$LEGACY_OUT snap=$([ "$LEGACY_SNAP1" = "$LEGACY_SNAP2" ] && echo identical || echo drifted) err=$(sanitize "$(head -c160 "$PROBE_TMP/err.txt")")"
fi
if [ "$PHASE" = "guards" ]; then
  probe legacy_null_direct_update_refused "err:v5 client_upload_id immutable" "UPDATE attach_pending SET client_upload_id=gen_random_uuid() WHERE object_key='v5p/legacy-null'"
fi

# --- real: 1,000,000-row synthetic load.  A live RowExclusive locker blocks
# the ALTER; lock_timeout='5s' bounds the DDL lock wait (cancelled with
# SQLSTATE 55P03).  The blocking relation is attributed with real lock-wait
# evidence (pg_locks RowExclusiveLock held by the locker pid before the DDL,
# pg_blocking_pids of the waiting ALTER pid during it) instead of sleeps, and
# an independent synthetic writer keeps committing throughout (bounded gap,
# every commit reported UPDATE n with n>=1).  lock_timeout bounds the lock
# WAIT; this does not claim a strict 5-second write freeze of the table.
LOAD_STATE="$(sqlval "SELECT count(*) FROM attachment_object_deletion")"
if [ "$LOAD_STATE" = "0" ]; then
  PSQL -q >>"$PROBE_TMP/out.txt" 2>>"$PROBE_TMP/err.txt" <<'SQL'
INSERT INTO attachment_object_deletion(path,bucket)
SELECT 'v5p/bulk/'||lpad(g::text,7,'0'), 'b' FROM generate_series(1,1000000) g;
SQL
fi
LOAD_ROWS="$(sqlval "SELECT count(*) FROM attachment_object_deletion WHERE path LIKE 'v5p/bulk/%'")"
: > "$PROBE_TMP/writer.log"
(
  i=0
  while [ $i -lt 90 ]; do
    res="$(printf "UPDATE attachment_object_deletion SET retry_generation=retry_generation WHERE path='v5p/bulk/0000001'" | PSQL -tAq 2>/dev/null | tail -1)"
    case "$res" in
      "UPDATE "*) printf '%s %s\n' "$(now_ms)" "${res#UPDATE }" >> "$PROBE_TMP/writer.log" ;;
    esac
    i=$((i+1)); sleep 0.12
  done
) &
WRITER_JOB=$!
(
  printf 'BEGIN; LOCK TABLE attachment_object_deletion IN ROW EXCLUSIVE MODE; SELECT pg_sleep(9); COMMIT;\n' | PSQL -q >/dev/null 2>&1
) &
# Lock-wait evidence instead of a sleep handshake: find the locker pid from
# pg_stat_activity and prove it actually holds the granted RowExclusiveLock.
LOCKER_PID=""
for i in $(seq 1 60); do
  LOCKER_PID="$(sqlval "SELECT pid::text FROM pg_stat_activity WHERE state='active' AND query LIKE '%pg_sleep(9)%' AND pid<>pg_backend_pid() LIMIT 1")"
  [ -n "$LOCKER_PID" ] && break
  sleep 0.1
done
LOCK_HELD=""
[ -n "$LOCKER_PID" ] && LOCK_HELD="$(sqlval "SELECT count(*) FROM pg_locks WHERE locktype='relation' AND pid=$LOCKER_PID AND relation='attachment_object_deletion'::regclass AND mode='RowExclusiveLock' AND granted")"
DDL_T0="$(now_ms)"
printf "SET lock_timeout='5s'; ALTER TABLE attachment_object_deletion ADD COLUMN v5_probe_bound int;\n" | PSQL -q -v VERBOSITY=verbose >/dev/null 2>"$PROBE_TMP/err.txt" &
DDL_JOB=$!
BLOCKED_BY=""
for i in $(seq 1 80); do
  BLOCKED_BY="$(sqlval "SELECT coalesce(array_to_string(pg_blocking_pids(a.pid),','),'') FROM pg_stat_activity a WHERE a.state='active' AND a.wait_event_type='Lock' AND a.query LIKE '%ADD COLUMN v5_probe_bound%'" 2>/dev/null)"
  [ -n "$BLOCKED_BY" ] && break
  sleep 0.1
done
wait $DDL_JOB 2>/dev/null
DDL_RC=$?
DDL_T1="$(now_ms)"
wait $WRITER_JOB 2>/dev/null || true
WRITER_JOB=""
DDL_ELAPSED=$((DDL_T1 - DDL_T0))
COLUMN_AFTER="$(sqlval "SELECT count(*) FROM information_schema.columns WHERE table_name='attachment_object_deletion' AND column_name='v5_probe_bound'")"
ROWS_AFTER="$(sqlval "SELECT count(*) FROM attachment_object_deletion WHERE path LIKE 'v5p/bulk/%'")"
MINROWS_GAP="$(python3 - "$PROBE_TMP/writer.log" <<'PY'
import sys
try:
    rows = [line.split() for line in open(sys.argv[1]).read().splitlines() if line.strip()]
    ts = [int(r[0]) for r in rows]
    counts = [int(r[1]) for r in rows]
except Exception:
    print("-1 -1"); raise SystemExit
if len(ts) < 2:
    print("-1 -1"); raise SystemExit
print("%d %d" % (min(counts), max(b - a for a, b in zip(ts, ts[1:]))))
PY
)"
MINROWS="${MINROWS_GAP%% *}"
GAP_MS="${MINROWS_GAP##* }"
WRITES="$(wc -l < "$PROBE_TMP/writer.log" | tr -d ' ')"
if [ "$DDL_RC" -ne 0 ] && grep -qF '55P03' "$PROBE_TMP/err.txt" && \
   [ -n "$LOCKER_PID" ] && [ "$LOCK_HELD" = "1" ] && [ "$BLOCKED_BY" = "$LOCKER_PID" ] && \
   [ "$DDL_ELAPSED" -ge 4500 ] && [ "$DDL_ELAPSED" -le 9000 ] && \
   [ "$COLUMN_AFTER" = "0" ] && [ "$ROWS_AFTER" = "1000000" ] && \
   [ "$WRITES" -ge 8 ] && [ "$MINROWS" -ge 1 ] && [ "$GAP_MS" -ge 0 ] && [ "$GAP_MS" -le 7000 ]; then
  emit large_table_write_block_bound 1 "1000000 synthetic rows loaded; locker pid $LOCKER_PID verified holding RowExclusiveLock via pg_locks before the DDL; ALTER TABLE (lock_timeout=5s) waited ${DDL_ELAPSED}ms and was cancelled with SQLSTATE 55P03, direct blocker pid $BLOCKED_BY confirmed via pg_blocking_pids; column absent, 1000000 rows intact; the concurrent writer committed $WRITES single-row updates (every result UPDATE n, min n=$MINROWS) with max commit gap ${GAP_MS}ms -- lock_timeout bounds the DDL lock wait, this is not a claim of a strict 5s write freeze"
else
  emit large_table_write_block_bound 0 "rc=$DDL_RC locker=$LOCKER_PID held=$LOCK_HELD blocked_by=$BLOCKED_BY elapsed=${DDL_ELAPSED}ms col=$COLUMN_AFTER rows=$ROWS_AFTER writes=$WRITES minrows=$MINROWS maxgap=${GAP_MS}ms err=$(sanitize "$(head -c160 "$PROBE_TMP/err.txt")")"
fi

probe base_read_compat2 "0" "SELECT count(*) FROM attachment WHERE lifecycle_state IS NULL"
probe old_completed_maps_done "0" "SELECT count(*) FROM attachment_object_deletion WHERE completed_at IS NOT NULL AND job_state<>'done' AND path NOT LIKE 'v5p/%'"
probe unclaimed_old_blocked "0" "SELECT count(*) FROM attachment_object_deletion WHERE completed_at IS NULL AND job_state<>'blocked' AND path NOT LIKE 'v5p/%'"

# --- real: old (pre-V5) writer path.  Writes metadata + a legacy completion
# row without any V5 identity; asserts no GC authorization can follow.  The
# intent refusal must carry exact SQLSTATE 23510 with the proven-reference-zero
# reason; the legacy queue row must stay blocked with completed_at/attempts
# byte-identical to the pre-attempt snapshot.
PSQL -q > /dev/null 2>&1 <<'SQL'
INSERT INTO attachment(id,file_hash256,mime_type,ext,name,path,url,size,scope,creator_user_id,referer_time,status)
SELECT 900000310,'h7','image/png','png','old.png','v5p/oldwriter','',1,'private',101,0,0
WHERE NOT EXISTS (SELECT 1 FROM attachment WHERE id=900000310);
INSERT INTO attachment_object_deletion(path,bucket,completed_at,attempts)
SELECT 'v5p/oldwriter','b',now(),1
WHERE NOT EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path='v5p/oldwriter');
SQL
OLD_WRITER_BEFORE="$(sqlval "SELECT coalesce(completed_at::text,'-')||':'||attempts FROM attachment_object_deletion WHERE path='v5p/oldwriter'")"
probe rolling_old_writer_no_gc "no-gc:blocked" "DO \$\$ DECLARE m text; BEGIN
  BEGIN
    PERFORM fn_v5_intent_ordinary_delete('v5p/oldwriter','probe');
    RAISE EXCEPTION 'old-writer path unexpectedly authorized GC intent' USING ERRCODE='P0001';
  EXCEPTION WHEN sqlstate '23510' THEN
    GET STACKED DIAGNOSTICS m = MESSAGE_TEXT;
  END;
  IF m NOT LIKE '%proven reference-zero%' THEN
    RAISE EXCEPTION 'wrong rejection for old writer: %', m USING ERRCODE='P0001';
  END IF;
END \$\$;
SELECT CASE WHEN (SELECT count(*) FROM attachment WHERE path='v5p/oldwriter' AND gc_verified IS FALSE AND lifecycle_state='live' AND storage_driver IS NULL)=1
        AND (SELECT count(*) FROM attachment_object_deletion WHERE path='v5p/oldwriter' AND job_state='blocked' AND completed_at IS NOT NULL AND attempts=1 AND storage_driver IS NULL AND lease_token IS NULL)=1
        AND (SELECT count(*) FROM attachment_lifecycle_v5_audit WHERE object_key='v5p/oldwriter')=0
        AND (SELECT last_error_code FROM attachment_object_deletion WHERE path='v5p/oldwriter')='legacy_location_unverified'
        AND (SELECT coalesce(completed_at::text,'-')||':'||attempts FROM attachment_object_deletion WHERE path='v5p/oldwriter')='$OLD_WRITER_BEFORE'
       THEN 'no-gc:blocked' ELSE 'gc-authorized' END"

# --- real: the 15 M2 required protocol functions present with the frozen
# owner/ACL and each proven by a REAL business result under its intended role:
# register returns the registered session + a persisted row, pin returns TRUE
# and persists the receipt, confirm returns the linked attachment id, intent
# writes delete_intent + a ready queue job + audit, claim leases exactly one
# job under the worker token (attempts incremented), ack returns 'done' with
# completed_at, the reaper returns >=1 and flips a real expired lease to
# retry_wait/lease_expired, proof fills the blocked job's location to ready,
# and the tombstone copy persists UUID+snapshot into the queue row.  A bare
# rc=0 no longer counts: every step's value is asserted.
required_functions_probe() {
  local n=0 fails="" out
  step() { # name expected sql
    out="$(printf '%s\n' "$3" | PSQL -tAq 2>>"$PROBE_TMP/err.txt")" || { fails="$fails $1(rc)"; return; }
    if [ "$out" = "$2" ]; then n=$((n+1)); else fails="$fails $1(=$out)"; fi
  }
  # Per-invocation fixtures: guards DBs persist across manual runs and V5
  # tombstone/session guards make DELETE cleanup impossible, so a reused
  # key/path/asset would hit already-filled sessions or permanent tombstones.
  local RKEY="v5p/fn-reg-$RUN_TAG" RINTENT="v5p/fn-intent-$RUN_TAG" RPROOF="v5p/fn-proof-$RUN_TAG"
  local RREAP="v5p/fn-reap-$RUN_TAG" REAPKEY="v5e/fn-reap-$RUN_TAG" COPYKEY="v5e/fn-copy-$RUN_TAG"
  local RID1=$((900000340 + ID_OFF)) RID2=$((900000341 + ID_OFF)) RID3=$((900000342 + ID_OFF))
  local RASSET="$EA_REG" RASSET2="$EA_REG2"
  local COPY_ASSET=$((910000320 + EA_OFF)) REAP_ASSET=$((910000321 + EA_OFF))
  local RUUID="a1111111-1111-4111-8111-$(printf '%012d' $((10#$RUN_TAG % 10**12)))"
  local REBUUID="b2222222-2222-4222-8222-$(printf '%012d' $((10#$RUN_TAG % 10**12)))"
  local RTOKEN="c3333333-3333-4333-8333-$(printf '%012d' $((10#$RUN_TAG % 10**12)))"
  local RETOKEN="d4444444-4444-4444-8444-$(printf '%012d' $((10#$RUN_TAG % 10**12)))"
  local RCOPYUUID="e7777777-7777-4777-8777-$(printf '%012d' $((10#$RUN_TAG % 10**12)))"
  local RRETOKEN="f9999999-9999-4999-8999-$(printf '%012d' $((10#$RUN_TAG % 10**12)))"
  local RERETOKEN="fa999999-9999-4999-8999-$(printf '%012d' $((10#$RUN_TAG % 10**12)))"
  PSQL -q > /dev/null 2>&1 <<SQL
INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace,storage_container,storage_locator_sha256)
SELECT $RID1,'$RKEY','private',101,'$ST_DRV','$ST_NS','$ST_CT','$ST_LOC'
WHERE NOT EXISTS (SELECT 1 FROM attachment WHERE id=$RID1);
INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace,storage_container,storage_locator_sha256,gc_verified)
SELECT $RID2,'$RINTENT','private',101,'$ST_DRV','$ST_NS','$ST_CT','$ST_LOC',true
WHERE NOT EXISTS (SELECT 1 FROM attachment WHERE id=$RID2);
INSERT INTO attachment(id,path,scope,creator_user_id)
SELECT $RID3,'$RPROOF','private',101 WHERE NOT EXISTS (SELECT 1 FROM attachment WHERE id=$RID3);
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status)
SELECT $COPY_ASSET,98000101,97000101,'$COPYKEY','v5hash','active'
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=$COPY_ASSET);
INSERT INTO enterprise_asset_delete_queue(organization_id,workspace_id,asset_id,object_key)
SELECT 98000101,97000101,$COPY_ASSET,'$COPYKEY'
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset_delete_queue WHERE asset_id=$COPY_ASSET);
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status)
SELECT $REAP_ASSET,98000101,97000101,'$REAPKEY','v5hash','active'
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=$REAP_ASSET);
SQL
  # 1 lock_identity: the advisory xact lock is really taken by this backend.
  step lock_identity "locked" "SET ROLE v5_app_runtime;
WITH l AS (SELECT fn_v5_lock_identity('$ST_DRV','$ST_NS','$ST_CT','v5p/fn-lock-$RUN_TAG'))
SELECT CASE WHEN EXISTS (SELECT 1 FROM pg_locks WHERE locktype='advisory' AND pid=pg_backend_pid())
       THEN 'locked' ELSE 'no-lock' END FROM l"
  # 2 register_pending: returns the frozen identity session and persists it.
  step register_pending "$RKEY:registered:$RUUID|1" "SET ROLE v5_app_runtime;
SELECT f.out_object_key||':'||f.out_session_state||':'||f.out_client_upload_id::text||'|'||
  (SELECT count(*)::text FROM attach_pending p WHERE p.object_key=f.out_object_key
     AND p.session_state='registered' AND p.storage_driver='$ST_DRV'
     AND p.storage_namespace='$ST_NS' AND p.storage_container='$ST_CT'
     AND p.storage_locator_sha256='$ST_LOC')
FROM fn_v5_register_pending('$RKEY','b','private',101,'$RUUID','$REG'::jsonb,
  now()+interval '2 hours',now()+interval '1 hour','$ST_DRV','$ST_NS','$ST_CT','$ST_LOC') f"
  # 3 register_eb: returns registered + persists the snapshot on the asset row.
  step register_eb "registered:$REBUUID|1" "SET ROLE v5_app_runtime;
SELECT f.out_session_state||':'||f.out_client_upload_id::text||'|'||
  (SELECT count(*)::text FROM enterprise_asset a WHERE a.id=$RASSET
     AND a.client_upload_id=f.out_client_upload_id AND a.upload_registration IS NOT NULL)
FROM fn_v5_register_eb(98000101,97000101,$RASSET,'v5e/sub1-$RUN_TAG','$REBUUID','$REG'::jsonb,'user',
  now()+interval '2 hours',now()+interval '1 hour') f"
  # 4 pin_pending_content: TRUE exactly once + receipt persisted.
  step pin_pending_content "true|1" "SET ROLE v5_app_runtime;
SELECT fn_v5_pin_pending_content('$RKEY','$CONTENT'::jsonb)::text||'|'||
  (SELECT count(*)::text FROM attach_pending WHERE object_key='$RKEY' AND upload_content='$CONTENT'::jsonb)"
  # 5 confirm_pending: returns the linked attachment id + confirmed session.
  step confirm_pending "$RID1|confirmed:$RID1" "SET ROLE v5_app_runtime;
SELECT fn_v5_confirm_pending('$RKEY',$RID1)::text||'|'||
  (SELECT session_state||':'||coalesce(confirmed_attachment_id::text,'NULL') FROM attach_pending WHERE object_key='$RKEY')"
  # 6 intent_ordinary_delete: delete_intent + ready job with frozen identity + audit.
  step intent_ordinary_delete "delete_intent:1:1" "SET ROLE v5_app_runtime;
WITH i AS (SELECT fn_v5_intent_ordinary_delete('$RINTENT','probe'))
SELECT (SELECT lifecycle_state FROM attachment WHERE id=$RID2)||':'||
  (SELECT count(*)::text FROM attachment_object_deletion WHERE path='$RINTENT' AND job_state='ready'
     AND storage_driver='$ST_DRV' AND storage_locator_sha256='$ST_LOC')||':'||
  (SELECT count(*)::text FROM attachment_lifecycle_v5_audit WHERE object_key='$RINTENT' AND action='delete_intent')
FROM i"
  # 7 intent_eb_delete: delete_intent + ready enterprise job with identity.
  step intent_eb_delete "delete_intent:1" "SET ROLE v5_app_runtime;
WITH i AS (SELECT fn_v5_intent_eb_delete(98000101,97000101,$RASSET2,'probe'))
SELECT (SELECT lifecycle_state FROM enterprise_asset WHERE id=$RASSET2)||':'||
  (SELECT count(*)::text FROM enterprise_asset_delete_queue WHERE asset_id=$RASSET2 AND job_state='ready'
     AND storage_driver='$ST_DRV' AND storage_locator_sha256='$ST_LOC')
FROM i"
  # 8 claim_ordinary: leases exactly one real job under the worker token.
  step claim_ordinary "1:1" "SET ROLE v5_worker;
WITH c AS (SELECT * FROM fn_v5_claim_ordinary('$RTOKEN'::uuid))
SELECT (SELECT count(*)::text FROM c)||':'||
  (SELECT count(*)::text FROM attachment_object_deletion q JOIN c ON q.path=c.out_path
     WHERE q.job_state='leased' AND q.lease_token='$RTOKEN'::uuid AND q.attempts>=1
       AND q.storage_driver='$ST_DRV')"
  # 9 ack_ordinary: 'done' + completed_at persisted on the leased job.
  step ack_ordinary "done:1" "SET ROLE v5_worker;
WITH j AS (SELECT path FROM attachment_object_deletion WHERE lease_token='$RTOKEN'::uuid AND job_state='leased' LIMIT 1),
     a AS (SELECT fn_v5_ack_ordinary(j.path,'$RTOKEN'::uuid,true) AS r FROM j)
SELECT (SELECT r FROM a)||':'||
  (SELECT count(*)::text FROM attachment_object_deletion q JOIN j ON q.path=j.path
     WHERE q.job_state='done' AND q.completed_at IS NOT NULL AND q.lease_token IS NULL)"
  # reaper fixture: a really expired lease under a dedicated path.
  PSQL -q > /dev/null 2>&1 -c "INSERT INTO attachment_object_deletion(path,bucket,storage_driver,storage_namespace,storage_container,storage_locator_sha256,job_state,lease_token,lease_until,next_retry_at,attempts) SELECT '$RREAP','b','$ST_DRV','$ST_NS','$ST_CT','$ST_LOC','leased','$RRETOKEN'::uuid,now()-interval '1 second',now(),0 WHERE NOT EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path='$RREAP')"
  # 10 reaper_ordinary: returns >=1 and flips the expired lease.
  step reaper_ordinary "true:retry_wait:lease_expired" "SET ROLE v5_worker;
SELECT (fn_v5_reaper_ordinary(5)>=1)::text||':'||
  (SELECT job_state||':'||coalesce(last_error_code,'-') FROM attachment_object_deletion WHERE path='$RREAP')"
  # 11 claim_eb: leases exactly one real enterprise job under the token.
  step claim_eb "1:1" "SET ROLE v5_worker;
WITH c AS (SELECT * FROM fn_v5_claim_eb('$RETOKEN'::uuid))
SELECT (SELECT count(*)::text FROM c)||':'||
  (SELECT count(*)::text FROM enterprise_asset_delete_queue q
     JOIN c ON (q.organization_id,q.workspace_id,q.asset_id)=(c.out_organization_id,c.out_workspace_id,c.out_asset_id)
     WHERE q.job_state='leased' AND q.lease_token='$RETOKEN'::uuid AND q.attempts>=1
       AND q.storage_driver='$ST_DRV')"
  # 12 ack_eb: 'done' + completed_at persisted on the leased enterprise job.
  step ack_eb "done:1" "SET ROLE v5_worker;
WITH j AS (SELECT organization_id,workspace_id,asset_id FROM enterprise_asset_delete_queue WHERE lease_token='$RETOKEN'::uuid AND job_state='leased' LIMIT 1),
     a AS (SELECT fn_v5_ack_eb(j.organization_id,j.workspace_id,j.asset_id,'$RETOKEN'::uuid,true) AS r FROM j)
SELECT (SELECT r FROM a)||':'||
  (SELECT count(*)::text FROM enterprise_asset_delete_queue q
     JOIN j ON (q.organization_id,q.workspace_id,q.asset_id)=(j.organization_id,j.workspace_id,j.asset_id)
     WHERE q.job_state='done' AND q.completed_at IS NOT NULL AND q.lease_token IS NULL)"
  # reaper fixture (enterprise): a really expired lease on a dedicated asset.
  PSQL -q > /dev/null 2>&1 -c "INSERT INTO enterprise_asset_delete_queue(organization_id,workspace_id,asset_id,object_key,storage_driver,storage_namespace,storage_container,storage_locator_sha256,job_state,lease_token,lease_until,next_retry_at,attempts) SELECT 98000101,97000101,$REAP_ASSET,'$REAPKEY','$ST_DRV','$ST_NS','$ST_CT','$ST_LOC','leased','$RERETOKEN'::uuid,now()-interval '1 second',now(),0 WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset_delete_queue WHERE asset_id=$REAP_ASSET)"
  # 13 reaper_eb: returns >=1 and flips the expired enterprise lease.
  step reaper_eb "true:retry_wait:lease_expired" "SET ROLE v5_worker;
SELECT (fn_v5_reaper_eb(5)>=1)::text||':'||
  (SELECT job_state||':'||coalesce(last_error_code,'-') FROM enterprise_asset_delete_queue WHERE asset_id=$REAP_ASSET)"
  # proof fixture: fresh blocked job (attempts=0, legacy_location_unverified).
  PSQL -q > /dev/null 2>&1 -c "INSERT INTO attachment_object_deletion(path,bucket) SELECT '$RPROOF','b' WHERE NOT EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path='$RPROOF')"
  # 14 proof_activate_ordinary: fills NULL location once and flips to ready.
  step proof_activate_ordinary "ready:-:$ST_DRV:1" "SET ROLE v5_proof;
WITH p AS (SELECT fn_v5_proof_activate_ordinary('$RPROOF','$ST_DRV','$ST_NS','$ST_CT','$ST_LOC'))
SELECT (SELECT job_state||':'||coalesce(last_error_code,'-')||':'||coalesce(storage_driver,'-') FROM attachment_object_deletion WHERE path='$RPROOF')||':'||
  (SELECT count(*)::text FROM attachment_lifecycle_v5_audit WHERE object_key='$RPROOF' AND action='proof_activate')
FROM p"
  # tombstone-copy fixture: register the dedicated asset, then copy.
  PSQL -q > /dev/null 2>>"$PROBE_TMP/err.txt" -c "SET ROLE v5_app_runtime; SELECT fn_v5_register_eb(98000101,97000101,$COPY_ASSET,'$COPYKEY','$RCOPYUUID','$REG'::jsonb,'user',now()+interval '2 hours',now()+interval '1 hour')" || fails="$fails register_eb_copy_fixture"
  # 15 copy_eb_recovery_tombstone: UUID+registration persisted into the queue row.
  step copy_eb_recovery_tombstone "$RCOPYUUID:true" "SET ROLE v5_app_runtime;
WITH c AS (SELECT fn_v5_copy_eb_recovery_tombstone(98000101,97000101,$COPY_ASSET))
SELECT (SELECT coalesce(client_upload_id::text,'-') FROM enterprise_asset_delete_queue WHERE asset_id=$COPY_ASSET)||':'||
  (SELECT (upload_registration IS NOT NULL)::text FROM enterprise_asset_delete_queue WHERE asset_id=$COPY_ASSET)
FROM c"
  # count DISTINCT proname with SECURITY DEFINER enforced: migration 00000193
  # legitimately overloads fn_v5_claim_eb with a second scoped signature owned
  # by the same role; prosecdef=true is now part of the frozen inventory.
  local inv
  inv="$(sqlval "SELECT count(DISTINCT p.proname) FROM pg_proc p JOIN pg_roles r ON r.rolname='v5_fn_owner' WHERE p.pronamespace='public'::regnamespace AND p.proname IN ('fn_v5_lock_identity','fn_v5_register_pending','fn_v5_register_eb','fn_v5_pin_pending_content','fn_v5_confirm_pending','fn_v5_intent_ordinary_delete','fn_v5_intent_eb_delete','fn_v5_claim_ordinary','fn_v5_ack_ordinary','fn_v5_reaper_ordinary','fn_v5_claim_eb','fn_v5_ack_eb','fn_v5_reaper_eb','fn_v5_proof_activate_ordinary','fn_v5_copy_eb_recovery_tombstone') AND p.proowner=r.oid AND p.prosecdef AND NOT has_function_privilege('public',p.oid,'EXECUTE')")"
  if [ "$n" -eq 15 ] && [ "$inv" = "15" ]; then
    emit expand_required_functions_callable 1 "15/15 M2 protocol functions proven by real business results under their granted roles (lock took the advisory lock; register/pin/confirm returned registered/TRUE/linked id with persisted rows; intents wrote delete_intent+ready job+audit; claim leased exactly 1 job per worker token with attempts incremented; ack returned done with completed_at; reapers returned >=1 flipping expired leases to retry_wait/lease_expired; proof filled the location once to ready; tombstone copy persisted UUID+snapshot); inventory: 15 definer-owned (prosecdef), PUBLIC EXECUTE revoked"
  else
    emit expand_required_functions_callable 0 "verified=$n/15 inventory=${inv}/15 failed:$(sanitize "$fails")"
  fi
}
required_functions_probe

probe authorized_intent_atomic "1" "WITH a AS (INSERT INTO attachment(id,file_hash256,mime_type,ext,name,path,url,size,scope,creator_user_id,referer_time,status,storage_driver,storage_namespace,storage_container,storage_locator_sha256,gc_verified) VALUES ($((900000110 + ID_OFF % 400000)),'h2','image/png','png','b.png','v5p/intent1-$RUN_TAG','',10,'private',101,0,0,'$ST_DRV','ns1','c1','$(printf '1%.0s' {1..64})',true) RETURNING path), i AS (SELECT fn_v5_intent_ordinary_delete((SELECT path FROM a),'probe') AS r) SELECT r FROM i;
SELECT count(*) FROM attachment_object_deletion WHERE path='v5p/intent1-$RUN_TAG' AND job_state='ready'"

# --- real: confirmed session survives a real GC intent (metadata only).
confirmed_gc_link_probe() {
  local GKEY="v5p/gclink-$RUN_TAG" GID
  GID=$((940000001 + ID_OFF % 100000))
  PSQL -q > /dev/null 2>&1 <<SQL
INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace,storage_container,storage_locator_sha256)
VALUES ($GID,'$GKEY','private',101,'$ST_DRV','$ST_NS','$ST_CT','$ST_LOC');
SELECT fn_v5_register_pending('$GKEY','b','private',101,'e5555555-5555-4555-8555-$(printf '%012d' $((10#$RUN_TAG % 10**12)))','$REG'::jsonb,now()+interval '2 hours',now()+interval '2 seconds','$ST_DRV','$ST_NS','$ST_CT','$ST_LOC');
SELECT fn_v5_pin_pending_content('$GKEY','$CONTENT'::jsonb);
SELECT fn_v5_confirm_pending('$GKEY',$GID);
UPDATE attachment SET gc_verified=true WHERE id=$GID;
SQL
  sleep 4
  sqlrc "SELECT fn_v5_intent_ordinary_delete('$GKEY','probe')"
  local rc=$? link
  link="$(sqlval "SELECT session_state||':'||coalesce(confirmed_attachment_id::text,'NULL') FROM attach_pending WHERE object_key='$GKEY'")"
  if [ $rc -eq 0 ] && [ "$link" = "confirmed:$GID" ] && \
     [ "$(sqlval "SELECT count(*) FROM attachment_object_deletion WHERE path='$GKEY' AND job_state='ready'")" = "1" ] && \
     [ "$(sqlval "SELECT lifecycle_state FROM attachment WHERE id=$GID")" = "delete_intent" ]; then
    emit confirmed_gc_preserves_session_link 1 "fn_v5_intent_ordinary_delete committed after capability expiry: metadata delete_intent + ready job; session stayed confirmed:$GID (link preserved)"
  else
    emit confirmed_gc_preserves_session_link 0 "rc=$rc link=$link err=$(sanitize "$(head -c160 "$PROBE_TMP/err.txt")")"
  fi
}
confirmed_gc_link_probe

if [ "$PHASE" = "guards" ]; then probe confirmed_to_registered "fail" "UPDATE attach_pending SET session_state='rejected' WHERE object_key='v5p/seed/1' AND session_state='registered'; UPDATE attach_pending SET session_state='registered' WHERE object_key='v5p/seed/1'"; else probe confirmed_to_registered "fail" "INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,session_state) VALUES ('v5p/c2r','b','private',101,'bogus_state')"; fi

# --- real: keyset<=500 backfill with committed cursor, injected interruption
# and resume on the same evidence version.  Run against enterprise_asset because
# that is the table whose registration guard still permits a controlled
# location fill in both phases (attach_pending/attachment freeze the identity
# once their M3 guards are attached).
backfill_probe() {
  PSQL -q > /dev/null 2>&1 <<SQL
DROP TABLE IF EXISTS v5_probe_bf_cursor;
CREATE TABLE v5_probe_bf_cursor(id int PRIMARY KEY, last_key text, committed_rows int, version int);
INSERT INTO v5_probe_bf_cursor VALUES (1,'',0,1);
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status)
SELECT $((920000000 + ID_OFF))+g,98000101,97000101,'v5e/bf-$RUN_TAG/'||lpad(g::text,5,'0'),'v5bfhash','active'
FROM generate_series(1,1200) g;
SQL
  local batch_sql interrupt_sql checksum_before checksum_after
  batch_sql="WITH batch AS (
    SELECT object_key FROM enterprise_asset
     WHERE object_key LIKE 'v5e/bf-$RUN_TAG/%' AND storage_driver IS NULL
       AND object_key > (SELECT last_key FROM v5_probe_bf_cursor WHERE id=1)
     ORDER BY object_key LIMIT 500 FOR UPDATE SKIP LOCKED),
  upd AS (UPDATE enterprise_asset p
            SET storage_driver='$ST_DRV',storage_namespace='v5bf',storage_container='v5bfc',
                storage_locator_sha256='$(printf 'd%.0s' {1..64})'
            FROM batch b WHERE p.object_key=b.object_key RETURNING p.object_key),
  cur AS (UPDATE v5_probe_bf_cursor
             SET last_key=(SELECT max(object_key) FROM upd),
                 committed_rows=committed_rows+(SELECT count(*) FROM upd)
           WHERE id=1 AND version=1 RETURNING committed_rows)
  SELECT committed_rows FROM cur"
  interrupt_sql="DO \$b\$ DECLARE n int; BEGIN
    WITH batch AS (
      SELECT object_key FROM enterprise_asset
       WHERE object_key LIKE 'v5e/bf-$RUN_TAG/%' AND storage_driver IS NULL
         AND object_key > (SELECT last_key FROM v5_probe_bf_cursor WHERE id=1)
       ORDER BY object_key LIMIT 500 FOR UPDATE SKIP LOCKED),
    upd AS (UPDATE enterprise_asset p
              SET storage_driver='$ST_DRV',storage_namespace='v5bf',storage_container='v5bfc',
                  storage_locator_sha256='$(printf 'd%.0s' {1..64})'
              FROM batch b WHERE p.object_key=b.object_key RETURNING p.object_key),
    cur AS (UPDATE v5_probe_bf_cursor
               SET last_key=(SELECT max(object_key) FROM upd),
                   committed_rows=committed_rows+(SELECT count(*) FROM upd)
             WHERE id=1 AND version=1 RETURNING committed_rows)
    SELECT committed_rows INTO n FROM cur;
    RAISE EXCEPTION 'injected backfill interrupt';
  END \$b\$"
  checksum_before="$(sqlval "SELECT md5(string_agg(id||':'||coalesce(client_upload_id::text,'-')||':'||lifecycle_state,',' ORDER BY id)) FROM enterprise_asset WHERE object_key LIKE 'v5e/bf-$RUN_TAG/%'")"
  local c1 c2 c_after_int promoted_int c3 c4 promoted final_rows maxbatch checksum_after
  c1="$(sqlval "$batch_sql")"
  c2="$(sqlval "$batch_sql")"
  # the injected interrupt must be OUR raise: the batch must abort (rc!=0)
  # with exactly the injected message, not any other error
  sqlrc "$interrupt_sql"
  local int_rc=$? int_edge=0
  grep -qF 'injected backfill interrupt' "$PROBE_TMP/err.txt" && int_edge=1
  promoted_int="$(sqlval "SELECT count(*) FROM enterprise_asset WHERE object_key LIKE 'v5e/bf-$RUN_TAG/%' AND storage_driver IS NOT NULL")"
  c_after_int="$(sqlval "SELECT committed_rows||':'||version FROM v5_probe_bf_cursor WHERE id=1")"
  c3="$(sqlval "$batch_sql")"
  c4="$(sqlval "$batch_sql")"
  checksum_after="$(sqlval "SELECT md5(string_agg(id||':'||coalesce(client_upload_id::text,'-')||':'||lifecycle_state,',' ORDER BY id)) FROM enterprise_asset WHERE object_key LIKE 'v5e/bf-$RUN_TAG/%'")"
  promoted="$(sqlval "SELECT count(*) FROM enterprise_asset WHERE object_key LIKE 'v5e/bf-$RUN_TAG/%' AND storage_driver IS NOT NULL")"
  final_rows="$(sqlval "SELECT committed_rows||':'||version FROM v5_probe_bf_cursor WHERE id=1")"
  # the keyset cursor must sit exactly on the last seeded key (01200 of 1200)
  final_key="$(sqlval "SELECT coalesce(last_key,'-') FROM v5_probe_bf_cursor WHERE id=1")"
  maxbatch="$(sqlval "SELECT greatest(${c1:-0}, ${c2:-0}-${c1:-0}, ${c3:-0}-${c2:-0}, ${c4:-0}-${c3:-0})")"
  if [ "$c1" = "500" ] && [ "$c2" = "1000" ] && [ "$promoted_int" = "1000" ] && [ "$c_after_int" = "1000:1" ] && \
     [ "$int_rc" -ne 0 ] && [ "$int_edge" -eq 1 ] && \
     [ "$c3" = "1200" ] && [ "$c4" = "1200" ] && \
     [ "$promoted" = "1200" ] && [ "$final_rows" = "1200:1" ] && [ "$final_key" = "v5e/bf-$RUN_TAG/01200" ] && [ "$maxbatch" = "500" ] && \
     [ "$checksum_before" = "$checksum_after" ] && \
     [ "$(sqlval "SELECT count(*) FROM enterprise_asset WHERE object_key LIKE 'v5e/bf-$RUN_TAG/%' AND (client_upload_id IS NOT NULL OR upload_registration IS NOT NULL OR lifecycle_state<>'live')")" = "0" ]; then
    emit backfill_interrupt_resume 1 "keyset batches 500/500 then injected interrupt rolled back with the exact injected error (rc=$int_rc, promoted=1000 cursor=1000:1), resume batches 200/0 (200 rows remained) -> promoted=1200 cursor=1200:1 last_key=v5e/bf-$RUN_TAG/01200 version unchanged, no identity/lifecycle promotion (checksum stable)"
  else
    emit backfill_interrupt_resume 0 "c1=$c1 c2=$c2 promoted_after_interrupt=$promoted_int cursor_after_interrupt=$c_after_int int_rc=$int_rc int_exact=$int_edge c3=$c3 c4=$c4 promoted=$promoted cursor=$final_rows last_key=$final_key maxbatch=$maxbatch identity_checksum=$([ "$checksum_before" = "$checksum_after" ] && echo stable || echo drifted)"
  fi
  PSQL -q > /dev/null 2>&1 -c "DROP TABLE IF EXISTS v5_probe_bf_cursor"
}
backfill_probe
}

# ============ down-block (both phases) ============
down_block_probes() {
  # Controlled rollback database: original Up chain + M1/M2 only (the exact
  # expand-queue input).  Every down block below is attributable to the single
  # fixture created for that ordering.
  if ! replay_all_upto "$CYCLE_DB" "00000180"; then
    emit empty_up_down_up 0 "cycle database replay failed: $(sanitize "$(tail -2 "$PROBE_TMP/err.txt" | tr '\n' ' ')")"
    emit nondefault_identity 0 "cycle database unavailable"
    emit confirmed_session 0 "cycle database unavailable"
    emit attempted_job 0 "cycle database unavailable"
    emit tombstone 0 "cycle database unavailable"
    return
  fi
  ONDB "$CYCLE_DB" -q > /dev/null 2>&1 <<'SQL'
INSERT INTO "user"(id,account,password,reg_ip,reg_cosv) VALUES (101,'v5probe101','x','127.0.0.1','probe') ON CONFLICT DO NOTHING;
INSERT INTO organization(id,name,owner_id) VALUES (98000101,'v5probe-org',101) ON CONFLICT DO NOTHING;
INSERT INTO workspace(organization_id,id,name,owner_id) VALUES (98000101,97000101,'v5probe-ws',101) ON CONFLICT DO NOTHING;
SQL
  # real up -> down -> down -> up -> up on empty state, with the absence of
  # every V5 column/function verified between down and re-up.
  PROBE_DB="$CYCLE_DB"
  probe empty_up_down_up "14|6" "$DOWN179
$DOWN178
DO \$c\$ BEGIN
  IF (SELECT count(*) FROM information_schema.columns WHERE table_name='attach_pending' AND column_name='client_upload_id') <> 0
     OR (SELECT count(*) FROM information_schema.columns WHERE table_name='attachment_object_deletion' AND column_name='job_state') <> 0
     OR (SELECT count(*) FROM pg_proc WHERE proname LIKE 'fn\_v5\_%') <> 0 THEN
    RAISE EXCEPTION 'M1/M2 down did not remove V5 columns/functions';
  END IF;
END \$c\$;
$UP178
$UP179
SELECT (SELECT count(*) FROM information_schema.columns WHERE table_name='attach_pending' AND column_name IN ('client_upload_id','upload_registration','upload_content','session_state','storage_driver','storage_namespace','storage_container','storage_locator_sha256','session_deadline','scope_ref','upload_expires_at','upload_drained_at','confirmed_attachment_id','gc_verified'))::text||'|'
    ||(SELECT count(*) FROM information_schema.columns WHERE table_name='attachment' AND column_name IN ('storage_driver','storage_namespace','storage_container','storage_locator_sha256','lifecycle_state','gc_verified'))::text"

  # non-default identity: real attachment location fill, then the real down file.
  ONDB "$CYCLE_DB" -q -c "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace,storage_container,storage_locator_sha256) VALUES (91001,'v5p/cyc-nondefault','private',101,'$ST_DRV','ns','c','$(printf 'a%.0s' {1..64})')" >/dev/null 2>&1
  if down_blocked "$CYCLE_DB" "$M_DOWN178" && [ "$(cycle_q "SELECT count(*) FROM information_schema.columns WHERE table_name='attachment' AND column_name='gc_verified'")" = "1" ] && [ "$(cycle_q "SELECT count(*) FROM attachment WHERE id=91001")" = "1" ]; then
    emit nondefault_identity 1 "real 00000178.down.sql replay refused with V5_DOWN_BLOCKED_USE_FORWARD_FIX; V5 columns and the non-default identity row unchanged (transaction rolled back)"
  else
    emit nondefault_identity 0 "expected V5_DOWN_BLOCKED_USE_FORWARD_FIX; got $(sanitize "$(tail -2 "$PROBE_TMP/err.txt" | tr '\n' ' ')")"
  fi
  ONDB "$CYCLE_DB" -q -c "DELETE FROM attachment WHERE id=91001" >/dev/null 2>&1

  # confirmed session link: real pending session with a recovery id.
  ONDB "$CYCLE_DB" -q -c "INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,client_upload_id) VALUES ('v5p/cyc-session','b','private',101,'f6666666-6666-6666-6666-666666666666')" >/dev/null 2>&1
  if down_blocked "$CYCLE_DB" "$M_DOWN178" && [ "$(cycle_q "SELECT count(*) FROM attach_pending WHERE object_key='v5p/cyc-session' AND client_upload_id IS NOT NULL")" = "1" ]; then
    emit confirmed_session 1 "real 00000178.down.sql replay refused with V5_DOWN_BLOCKED_USE_FORWARD_FIX for a persisted client_upload_id session; session row unchanged"
  else
    emit confirmed_session 0 "expected V5_DOWN_BLOCKED_USE_FORWARD_FIX; got $(sanitize "$(tail -2 "$PROBE_TMP/err.txt" | tr '\n' ' ')")"
  fi
  ONDB "$CYCLE_DB" -q -c "DELETE FROM attach_pending WHERE object_key='v5p/cyc-session'" >/dev/null 2>&1

  # attempted job: attempts>0 blocks the queue down.
  ONDB "$CYCLE_DB" -q -c "INSERT INTO attachment_object_deletion(path,bucket,attempts) VALUES ('v5p/cyc-job','b',2)" >/dev/null 2>&1
  if down_blocked "$CYCLE_DB" "$M_DOWN179" && [ "$(cycle_q "SELECT attempts FROM attachment_object_deletion WHERE path='v5p/cyc-job'")" = "2" ]; then
    emit attempted_job 1 "real 00000179.down.sql replay refused with V5_DOWN_BLOCKED_USE_FORWARD_FIX for attempts=2; job row unchanged"
  else
    emit attempted_job 0 "expected V5_DOWN_BLOCKED_USE_FORWARD_FIX; got $(sanitize "$(tail -2 "$PROBE_TMP/err.txt" | tr '\n' ' ')")"
  fi
  ONDB "$CYCLE_DB" -q -c "DELETE FROM attachment_object_deletion WHERE path='v5p/cyc-job'" >/dev/null 2>&1

  # queue tombstone: recovery snapshot blocks the queue down.
  ONDB "$CYCLE_DB" -q -c "INSERT INTO enterprise_asset_delete_queue(organization_id,workspace_id,asset_id,object_key,client_upload_id,upload_registration) VALUES (98000101,97000101,910099,'v5e/cyc-tomb','77777777-7777-7777-7777-777777777777','{\"subject\":{\"kind\":\"user\"},\"declaration\":{}}'::jsonb)" >/dev/null 2>&1
  if down_blocked "$CYCLE_DB" "$M_DOWN179" && [ "$(cycle_q "SELECT count(*) FROM enterprise_asset_delete_queue WHERE asset_id=910099 AND client_upload_id IS NOT NULL")" = "1" ]; then
    emit tombstone 1 "real 00000179.down.sql replay refused with V5_DOWN_BLOCKED_USE_FORWARD_FIX for an enterprise recovery tombstone; tombstone unchanged"
  else
    emit tombstone 0 "expected V5_DOWN_BLOCKED_USE_FORWARD_FIX; got $(sanitize "$(tail -2 "$PROBE_TMP/err.txt" | tr '\n' ' ')")"
  fi
  ONDB "$CYCLE_DB" -q -c "DELETE FROM enterprise_asset_delete_queue WHERE asset_id=910099" >/dev/null 2>&1
  # attribution: once the tombstone is gone the very same down applies.
  if down_applied "$CYCLE_DB" "$M_DOWN179" && down_applied "$CYCLE_DB" "$M_UP179"; then
    emit tombstone_block_attributable 1 "with the single tombstone removed the same 00000179.down.sql applies cleanly and re-up restores the queue (block attributable to the tombstone)"
  else
    emit tombstone_block_attributable 0 "down/up after tombstone removal failed: $(sanitize "$(tail -2 "$PROBE_TMP/err.txt" | tr '\n' ' ')")"
  fi
  PROBE_DB=""

  # the same frozen down files against the real phase database.
  if down_blocked "$DB" "$M_DOWN178"; then
    emit nondefault_identity_phase_db 1 "real 00000178.down.sql replay on $DB refused with V5_DOWN_BLOCKED_USE_FORWARD_FIX (phase schema intact)"
  else
    emit nondefault_identity_phase_db 0 "00000178.down.sql on $DB did not report the V5 down block"
  fi
  if down_blocked "$DB" "$M_DOWN179"; then
    emit attempted_job_phase_db 1 "real 00000179.down.sql replay on $DB refused with V5_DOWN_BLOCKED_USE_FORWARD_FIX (phase schema intact)"
  else
    emit attempted_job_phase_db 0 "00000179.down.sql on $DB did not report the V5 down block"
  fi

  # pre-existing local predicate assertions (unchanged semantics).
  probe nondefault_identity_predicate "blocked" "SELECT CASE WHEN (SELECT count(*) FROM attachment WHERE storage_driver IS NOT NULL OR gc_verified OR lifecycle_state<>'live')>0 THEN 'blocked' ELSE 'down' END"
  # fixture for the phase-db queue down predicate and for attempts/last_error
  # immutability probes below (blocked job with attempts>0).
  PSQL -q > /dev/null 2>&1 <<'SQL'
INSERT INTO attachment_object_deletion(path,bucket,attempts,next_retry_at)
SELECT 'v5p/downjob','b',3,now()
WHERE NOT EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path='v5p/downjob');
SQL
}

# ============ immutable (guards only) ============
immutable_probes() {
seed
PSQL -q > /dev/null 2>&1 <<SQL
UPDATE attachment SET storage_driver='$ST_DRV',storage_namespace='ns1',storage_container='c1',storage_locator_sha256='$ST_LOC',gc_verified=true WHERE id=900000101;
SQL
probe mutate_driver "fail" "UPDATE attachment SET storage_namespace='ns2' WHERE id=900000101"
probe mutate_namespace "fail" "UPDATE attachment SET storage_namespace='ns9' WHERE id=900000101"
probe mutate_container "fail" "UPDATE attachment SET storage_container='c9' WHERE id=900000101"
probe mutate_key "fail" "UPDATE attachment SET path='v5p/mutated' WHERE id=900000101"
probe mutate_owner "fail" "UPDATE attachment SET creator_user_id=202 WHERE id=900000101"
probe mutate_scope "fail" "UPDATE attachment SET scope='public' WHERE id=900000101"
probe mutate_cipher "fail" "UPDATE attachment SET cipher='x'||coalesce(cipher,'') WHERE id=900000101"
probe mutate_locator_digest "fail" "UPDATE attachment SET storage_locator_sha256='$(printf 'c%.0s' {1..64})' WHERE id=900000101"
probe forge_gc_verified "fail" "UPDATE attachment SET gc_verified=true WHERE id=900000101 AND storage_driver IS NULL"
probe tombstone_recreation "fail" "INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id) VALUES ('v5p/deleted-key','b','private',101); UPDATE attach_pending SET object_key='v5p/gone' WHERE object_key='v5p/deleted-key'"
# --- group: immutable.  first-bind/replay of the group_file_id capability.
# The bind target is a dedicated PER-RUN live attachment row, so the starting
# point can never be a leftover binding from an earlier invocation on a
# persistent guards DB; the probe itself re-asserts the NULL precondition.
local BIND_ID=$((900000350 + ID_OFF))
PSQL -q > /dev/null 2>&1 <<SQL
INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace,storage_container,storage_locator_sha256,gc_verified)
SELECT $BIND_ID,'v5p/bind-$RUN_TAG','private',101,'$ST_DRV','$ST_NS','$ST_CT','$ST_LOC',true
WHERE NOT EXISTS (SELECT 1 FROM attachment WHERE id=$BIND_ID);
SQL
probe first_bind_once "first-bind:once" "DO \$fb\$ BEGIN
  IF EXISTS (SELECT 1 FROM attachment WHERE id=$BIND_ID AND group_file_id IS NOT NULL) THEN
    RAISE EXCEPTION 'first-bind precondition failed: group_file_id already bound' USING ERRCODE='P0001';
  END IF;
END \$fb\$;
UPDATE attachment SET group_file_id=7001 WHERE id=$BIND_ID;
DO \$i\$ BEGIN
  BEGIN
    UPDATE attachment SET group_file_id=7002 WHERE id=$BIND_ID;
    RAISE EXCEPTION 'second bind unexpectedly accepted' USING ERRCODE='P0001';
  EXCEPTION WHEN sqlstate '23510' THEN NULL;
  END;
END \$i\$;
SELECT CASE WHEN group_file_id=7001 THEN 'first-bind:once' ELSE 'rebound' END FROM attachment WHERE id=$BIND_ID"
BIND_SNAP1="$(sqlval "SELECT md5(to_jsonb(a)::text) FROM attachment a WHERE id=$BIND_ID")"
# --- group: immutable occurrence of same_value_replay (the registration group
# carries its own occurrence below; consumers distinguish by group order).
# Replaying the identical bind value must keep the WHOLE persisted row
# unchanged (persistent to_jsonb snapshot comparison).
probe same_value_replay "replay:idempotent" "UPDATE attachment SET group_file_id=7001 WHERE id=$BIND_ID;
SELECT CASE WHEN group_file_id=7001 AND md5(to_jsonb(a)::text)='$BIND_SNAP1' THEN 'replay:idempotent' ELSE 'changed' END FROM attachment a WHERE id=$BIND_ID"
}

# ============ session-link-integrity (guards only) ============
session_link_probes() {
  local LKEY1="v5p/link1-$RUN_TAG" LKEY2="v5p/other-$RUN_TAG" \
        LKEY3="v5p/link-owner-$RUN_TAG" LKEY4="v5p/link-scope-$RUN_TAG" \
        LKEY5="v5p/link-loc-$RUN_TAG" LKEY6="v5p/link-cipher-$RUN_TAG"
  PSQL -q > /dev/null 2>&1 <<SQL
BEGIN;
INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,client_upload_id,upload_registration,session_deadline,upload_expires_at,storage_driver,storage_namespace,storage_container,storage_locator_sha256)
VALUES ('$LKEY1','b','private',101,'33${RUN_TAG:0:6}-3333-4333-8333-333333333333','$REG',now()+interval '2 hours',now()+interval '1 hour','$ST_DRV','$ST_NS','$ST_CT','$ST_LOC');
COMMIT;
BEGIN;
INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace,storage_container,storage_locator_sha256)
VALUES ($LID1,'$LKEY1','private',101,'$ST_DRV','$ST_NS','$ST_CT','$ST_LOC');
COMMIT;
SQL
probe confirmed_null "fail" "UPDATE attach_pending SET session_state='confirmed' WHERE object_key='$LKEY1'"
probe registered_link_nonnull "fail" "UPDATE attach_pending SET confirmed_attachment_id=$LID1 WHERE object_key='$LKEY1'"
# legal FK (existing attachment row) but every non-matching dimension.
PSQL -q > /dev/null 2>&1 <<SQL
BEGIN;
INSERT INTO attachment(id,path,scope,creator_user_id) VALUES ($LID2,'$LKEY2','private',101);
COMMIT;
SQL
probe legal_fk_wrong_key "err:v5 confirm key mismatch" "SET ROLE v5_app_runtime; SELECT fn_v5_confirm_pending('$LKEY1', $LID2)"
# wrong owner: same key/scope/location, different creator (registration
# snapshot declares subject 202 so the mismatch is the creator, not the
# declaration).
PSQL -q > /dev/null 2>&1 <<SQL
INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,client_upload_id,upload_registration,session_deadline,upload_expires_at,storage_driver,storage_namespace,storage_container,storage_locator_sha256)
VALUES ('$LKEY3','b','private',202,'9a${RUN_TAG:0:6}-9aaa-4aaa-8aaa-aaaaaaaaaaaa','$REG202',now()+interval '2 hours',now()+interval '1 hour','$ST_DRV','$ST_NS','$ST_CT','$ST_LOC');
INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace,storage_container,storage_locator_sha256)
VALUES ($LID3,'$LKEY3','private',101,'$ST_DRV','$ST_NS','$ST_CT','$ST_LOC');
SQL
probe legal_fk_wrong_owner "err:v5 confirm owner mismatch" "SET ROLE v5_app_runtime; SELECT fn_v5_confirm_pending('$LKEY3', $LID3)"
# wrong origin scope: session says private, asset is group.
PSQL -q > /dev/null 2>&1 <<SQL
INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,client_upload_id,upload_registration,session_deadline,upload_expires_at,storage_driver,storage_namespace,storage_container,storage_locator_sha256)
VALUES ('$LKEY4','b','private',101,'9b${RUN_TAG:0:6}-9bbb-4bbb-8bbb-bbbbbbbbbbbb','$REG',now()+interval '2 hours',now()+interval '1 hour','$ST_DRV','$ST_NS','$ST_CT','$ST_LOC');
INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace,storage_container,storage_locator_sha256)
VALUES ($LID4,'$LKEY4','group',101,'$ST_DRV','$ST_NS','$ST_CT','$ST_LOC');
SQL
probe legal_fk_wrong_scope "err:v5 confirm frozen scope declaration mismatch" "SET ROLE v5_app_runtime; SELECT fn_v5_confirm_pending('$LKEY4', $LID4)"
# wrong location: identical key/owner/scope, different immutable locator.
PSQL -q > /dev/null 2>&1 <<SQL
INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,client_upload_id,upload_registration,session_deadline,upload_expires_at,storage_driver,storage_namespace,storage_container,storage_locator_sha256)
VALUES ('$LKEY5','b','private',101,'9c${RUN_TAG:0:6}-9ccc-4ccc-8ccc-cccccccccccc','$REG',now()+interval '2 hours',now()+interval '1 hour','$ST_DRV','$ST_NS','$ST_CT','$LOCA');
INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace,storage_container,storage_locator_sha256)
VALUES ($LID5,'$LKEY5','private',101,'$ST_DRV','$ST_NS','$ST_CT','$LOCB');
SQL
probe legal_fk_wrong_location "err:v5 confirm location mismatch" "SET ROLE v5_app_runtime; SELECT fn_v5_confirm_pending('$LKEY5', $LID5)"
# cipher declaration: the first durable cipher is fixed at confirm and can
# never be replaced by a different (or empty) declaration afterwards.
PSQL -q > /dev/null 2>&1 <<SQL
INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace,storage_container,storage_locator_sha256,cipher)
VALUES ($LID6,'$LKEY6','private',101,'$ST_DRV','$ST_NS','$ST_CT','$ST_LOC','AES-256-GCM');
INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,client_upload_id,upload_registration,session_deadline,upload_expires_at,storage_driver,storage_namespace,storage_container,storage_locator_sha256)
VALUES ('$LKEY6','b','private',101,'9d${RUN_TAG:0:6}-9ddd-4ddd-8ddd-dddddddddddd','$REG',now()+interval '2 hours',now()+interval '1 hour','$ST_DRV','$ST_NS','$ST_CT','$ST_LOC');
SELECT fn_v5_pin_pending_content('$LKEY6','$CONTENT'::jsonb);
SELECT fn_v5_confirm_pending('$LKEY6',$LID6);
SQL
probe legal_fk_wrong_cipher "cipher:fixed:AES-256-GCM" "DO \$q\$ BEGIN
  BEGIN UPDATE attachment SET cipher='CHACHA20' WHERE id=$LID6; RAISE EXCEPTION 'cipher replaced' USING ERRCODE='P0001';
  EXCEPTION WHEN sqlstate '23510' THEN NULL; END;
  BEGIN UPDATE attachment SET cipher=NULL WHERE id=$LID6; RAISE EXCEPTION 'cipher cleared' USING ERRCODE='P0001';
  EXCEPTION WHEN sqlstate '23510' THEN NULL; END;
END \$q\$;
SELECT CASE WHEN a.cipher='AES-256-GCM' AND p.session_state='confirmed' AND p.confirmed_attachment_id=a.id
            THEN 'cipher:fixed:AES-256-GCM' ELSE 'cipher:drifted' END
  FROM attachment a JOIN attach_pending p ON p.confirmed_attachment_id=a.id WHERE a.id=$LID6"
# pin content then confirm correctly (receipt required before confirm)
PSQL -q > /dev/null 2>&1 <<SQL
BEGIN;
SELECT fn_v5_pin_pending_content('$LKEY1', '$CONTENT'::jsonb);
COMMIT;
SQL
probe confirm_ok_returns_id "$LID1" "SET ROLE v5_app_runtime; SELECT fn_v5_confirm_pending('$LKEY1', $LID1)"
probe confirmed_rebind "fail" "SELECT fn_v5_confirm_pending('$LKEY1', $LID1); SELECT fn_v5_confirm_pending('$LKEY1', $LID2)"
probe lawful_repeat "$LID1" "SET ROLE v5_app_runtime; SELECT fn_v5_confirm_pending('$LKEY1', $LID1)"
probe confirmed_session_cannot_intent "fail" "UPDATE attach_pending SET session_state='delete_intent' WHERE object_key='$LKEY1'"
# the legacy read-only invariant is kept (separate occurrence) alongside the
# executed GC action recorded in migration_probes.
probe confirmed_gc_preserves_session_link "preserved" "SELECT CASE WHEN (SELECT count(*) FROM attach_pending WHERE session_state='confirmed' AND confirmed_attachment_id IS NULL)=0 THEN 'preserved' ELSE 'leaked' END"
}

# ============ intent-authority (guards only) ============
intent_authority_probes() {
PSQL -q > /dev/null 2>&1 <<'SQL'
DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname='v5_probe_runtime') THEN
    CREATE ROLE v5_probe_runtime LOGIN PASSWORD 'v5_probe_runtime_pw';
  END IF;
  GRANT v5_app_runtime TO v5_probe_runtime;
END $$;
SQL
probe queue_insert_ready "fail" "SET ROLE v5_app_runtime; INSERT INTO attachment_object_deletion(path,bucket,job_state,next_retry_at) VALUES ('v5p/auth1','b','ready',now())"
probe queue_insert_leased "fail" "SET ROLE v5_app_runtime; INSERT INTO attachment_object_deletion(path,bucket,job_state,lease_token,lease_until) VALUES ('v5p/auth2','b','leased',gen_random_uuid(),now())"
probe queue_insert_retry_wait "fail" "SET ROLE v5_app_runtime; INSERT INTO attachment_object_deletion(path,bucket,job_state,next_retry_at) VALUES ('v5p/auth3','b','retry_wait',now())"
probe queue_insert_done "fail" "SET ROLE v5_app_runtime; INSERT INTO attachment_object_deletion(path,bucket,job_state,completed_at) VALUES ('v5p/auth4','b','done',now())"
probe activate_legacy_without_proof "fail" "SET ROLE v5_app_runtime; SELECT fn_v5_proof_activate_ordinary('v5p/x','garage','ns','c','$(printf 'd%.0s' {1..64})')"
probe forge_gc_verified "fail" "SET ROLE v5_app_runtime; UPDATE attachment SET gc_verified=true WHERE id=900000101"
probe forge_gc_verified_on_insert "fail" "SET ROLE v5_app_runtime; INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,gc_verified,session_state,client_upload_id,upload_registration) VALUES ('v5p/forge','b','private',101,true,'confirmed','44444444-4444-4444-4444-444444444444','{}')"
# live reference: a real live group_file binding blocks the intent.
PSQL -q > /dev/null 2>&1 <<SQL
INSERT INTO group_file(id,group_id,file_id,file_name,file_size,file_type,file_category,file_url,uploader_id)
SELECT 8001,1,'v5probe-file','f.png',1,'image/png','image','',101
WHERE NOT EXISTS (SELECT 1 FROM group_file WHERE id=8001);
INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace,storage_container,storage_locator_sha256,gc_verified,group_file_id)
SELECT 900000330,'v5p/live-ref','private',101,'$ST_DRV','$ST_NS','$ST_CT','$ST_LOC',true,8001
WHERE NOT EXISTS (SELECT 1 FROM attachment WHERE id=900000330);
SQL
probe intent_with_live_reference "err:live group_file binding" "SET ROLE v5_app_runtime; SELECT fn_v5_intent_ordinary_delete('v5p/live-ref','probe')"
# hold: real enterprise_attachment_retention hold row.
PSQL -q > /dev/null 2>&1 <<SQL
INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace,storage_container,storage_locator_sha256,gc_verified)
SELECT 900000331,'v5p/hold','private',101,'$ST_DRV','$ST_NS','$ST_CT','$ST_LOC',true
WHERE NOT EXISTS (SELECT 1 FROM attachment WHERE id=900000331);
INSERT INTO enterprise_attachment_retention(attachment_id,organization_id,application_id,retention_until,hold_state,hold_reason,hold_set_at,purge_state,source_kind)
SELECT 900000331,98000101,96000101,now()+interval '30 days','held','probe-hold',now(),'live','application'
WHERE NOT EXISTS (SELECT 1 FROM enterprise_attachment_retention WHERE attachment_id=900000331);
SQL
probe intent_under_hold "err:v5 intent blocked by EAR" "SET ROLE v5_app_runtime; SELECT fn_v5_intent_ordinary_delete('v5p/hold','probe')"
# future retention: EAR row without hold but not purged.
PSQL -q > /dev/null 2>&1 <<SQL
INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace,storage_container,storage_locator_sha256,gc_verified)
SELECT 900000332,'v5p/retain','private',101,'$ST_DRV','$ST_NS','$ST_CT','$ST_LOC',true
WHERE NOT EXISTS (SELECT 1 FROM attachment WHERE id=900000332);
INSERT INTO enterprise_attachment_retention(attachment_id,organization_id,application_id,retention_until,hold_state,purge_state,source_kind)
SELECT 900000332,98000101,96000101,now()+interval '30 days','none','live','application'
WHERE NOT EXISTS (SELECT 1 FROM enterprise_attachment_retention WHERE attachment_id=900000332);
SQL
probe intent_future_retention "err:v5 intent blocked by EAR" "SET ROLE v5_app_runtime; SELECT fn_v5_intent_ordinary_delete('v5p/retain','probe')"
# missing audit: the intent write reaches the audit insert and is refused; the
# whole three-write intent must roll back.
PSQL -q > /dev/null 2>&1 <<SQL
INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace,storage_container,storage_locator_sha256,gc_verified)
SELECT 900000333,'v5p/auditfail','private',101,'$ST_DRV','$ST_NS','$ST_CT','$ST_LOC',true
WHERE NOT EXISTS (SELECT 1 FROM attachment WHERE id=900000333);
CREATE OR REPLACE FUNCTION v5_probe_fail_audit() RETURNS trigger LANGUAGE plpgsql AS \$\$
BEGIN RAISE EXCEPTION 'probe injected audit failure' USING ERRCODE='P0001'; END \$\$;
DROP TRIGGER IF EXISTS trg_v5_probe_fail_audit ON attachment_lifecycle_v5_audit;
CREATE TRIGGER trg_v5_probe_fail_audit BEFORE INSERT ON attachment_lifecycle_v5_audit
  FOR EACH ROW EXECUTE FUNCTION v5_probe_fail_audit();
SQL
probe intent_missing_audit "err:probe injected audit failure" "SELECT fn_v5_intent_ordinary_delete('v5p/auditfail','probe')"
PSQL -q > /dev/null 2>&1 -c "DROP TRIGGER IF EXISTS trg_v5_probe_fail_audit ON attachment_lifecycle_v5_audit"
probe intent_missing_audit_rollback "no-intent:no-queue:live" "SELECT CASE WHEN (SELECT lifecycle_state FROM attachment WHERE id=900000333)='live'
   AND (SELECT count(*) FROM attachment_object_deletion WHERE path='v5p/auditfail')=0
   AND (SELECT count(*) FROM attachment_lifecycle_v5_audit WHERE object_key='v5p/auditfail')=0
  THEN 'no-intent:no-queue:live' ELSE 'partial-write' END"
# missing metadata transition: the metadata UPDATE is refused after the caller
# already passed eligibility; queue and audit must not be created either.
PSQL -q > /dev/null 2>&1 <<SQL
INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace,storage_container,storage_locator_sha256,gc_verified)
SELECT 900000334,'v5p/metafail','private',101,'$ST_DRV','$ST_NS','$ST_CT','$ST_LOC',true
WHERE NOT EXISTS (SELECT 1 FROM attachment WHERE id=900000334);
CREATE OR REPLACE FUNCTION v5_probe_fail_meta() RETURNS trigger LANGUAGE plpgsql AS \$\$
BEGIN RAISE EXCEPTION 'probe injected metadata failure' USING ERRCODE='P0001'; END \$\$;
DROP TRIGGER IF EXISTS trg_v5_probe_fail_meta ON attachment;
CREATE TRIGGER trg_v5_probe_fail_meta BEFORE UPDATE ON attachment
  FOR EACH ROW WHEN (NEW.id=900000334) EXECUTE FUNCTION v5_probe_fail_meta();
SQL
probe intent_missing_metadata_transition "err:probe injected metadata failure" "SELECT fn_v5_intent_ordinary_delete('v5p/metafail','probe')"
PSQL -q > /dev/null 2>&1 -c "DROP TRIGGER IF EXISTS trg_v5_probe_fail_meta ON attachment"
probe intent_missing_metadata_transition_rollback "live:no-queue:no-audit" "SELECT CASE WHEN (SELECT lifecycle_state FROM attachment WHERE id=900000334)='live'
   AND (SELECT count(*) FROM attachment_object_deletion WHERE path='v5p/metafail')=0
   AND (SELECT count(*) FROM attachment_lifecycle_v5_audit WHERE object_key='v5p/metafail')=0
  THEN 'live:no-queue:no-audit' ELSE 'partial-write' END"
probe direct_session_intent "fail" "SET ROLE v5_app_runtime; UPDATE attach_pending SET session_state='delete_intent' WHERE object_key='v5p/seed/1'"
probe direct_metadata_intent "fail" "SET ROLE v5_app_runtime; UPDATE attachment SET lifecycle_state='delete_intent' WHERE path='v5p/seed/1'"
probe queue_direct_update_state "fail" "SET ROLE v5_app_runtime; UPDATE attachment_object_deletion SET job_state='ready' WHERE path='v5p/seed/1'"
probe queue_direct_delete_row "fail" "SET ROLE v5_app_runtime; DELETE FROM attachment_object_deletion WHERE path='v5p/seed/1'"
probe queue_direct_update_state_g "fail" "SET ROLE v5_app_runtime; UPDATE enterprise_asset_delete_queue SET job_state='ready' WHERE true"
probe queue_direct_delete_row_g "fail" "SET ROLE v5_app_runtime; DELETE FROM enterprise_asset_delete_queue WHERE true"
probe eb_forge_tombstone_direct "fail" "SET ROLE v5_app_runtime; UPDATE enterprise_asset_delete_queue SET client_upload_id='55555555-5555-5555-5555-555555555555', upload_registration='{}'::jsonb WHERE true"
if docker exec -i -e PGPASSWORD=v5_probe_runtime_pw "$PG_CONTAINER" psql -X -U v5_probe_runtime -h 127.0.0.1 -d "$DB" -tAc "SET ROLE v5_proof" > /dev/null 2>&1; then
  bad=$((bad+1)); echo "{\"name\":\"proof_role_not_setroleable\",\"pass\":false,\"detail\":\"runtime role escalated to proof\",\"driver\":\"$V5_PROBE_DRIVER\"}" >> "$OUT"
else
  ok=$((ok+1)); echo "{\"name\":\"proof_role_not_setroleable\",\"pass\":true,\"driver\":\"$V5_PROBE_DRIVER\"}" >> "$OUT"
fi
probe worker_claim_no_intent_grant "fail" "SET ROLE v5_worker; SELECT fn_v5_intent_eb_delete(98000101,97000101,1,'x')"
probe audit_direct_write "fail" "SET ROLE v5_app_runtime; INSERT INTO attachment_lifecycle_v5_audit(queue_domain,object_key,action,actor) VALUES ('ordinary','x','delete_intent','forged')"
probe done_cannot_revive "fail" "SET ROLE v5_app_runtime; UPDATE attachment_object_deletion SET job_state='ready' WHERE completed_at IS NOT NULL"
probe attempts_cannot_reset "fail" "UPDATE attachment_object_deletion SET attempts=0 WHERE attempts>0"
# dedicated blocked legacy row (order-independent fixture: the shared
# v5e/liveref row below is seeded later in this group)
PSQL -q > /dev/null 2>&1 <<SQL
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status)
SELECT $((910000170 + EA_OFF)),98000101,97000101,'v5e/qii-$RUN_TAG','v5hash','active'
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=$((910000170 + EA_OFF)));
INSERT INTO enterprise_asset_delete_queue(organization_id,workspace_id,asset_id,object_key,job_state)
SELECT 98000101,97000101,$((910000170 + EA_OFF)),'v5e/qii-$RUN_TAG','blocked'
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset_delete_queue WHERE asset_id=$((910000170 + EA_OFF)));
SQL
sqlrc "UPDATE enterprise_asset_delete_queue SET storage_driver='$ST_DRV' WHERE asset_id=$((910000170 + EA_OFF));
UPDATE enterprise_asset_delete_queue SET storage_driver='minio' WHERE asset_id=$((910000170 + EA_OFF))"
QII_RC=$?
if [ $QII_RC -ne 0 ]; then
  emit eb_queue_identity_immutable 1 "first NULL->$ST_DRV storage fill on the blocked legacy row allowed once; second change refused (v5 job identity immutable after fill, 23510)"
else
  emit eb_queue_identity_immutable 0 "rc=0 rows=$(sqlval "SELECT count(*) FROM enterprise_asset_delete_queue WHERE asset_id=$((910000170 + EA_OFF))") drv=$(sqlval "SELECT coalesce(storage_driver,'NULL') FROM enterprise_asset_delete_queue WHERE asset_id=$((910000170 + EA_OFF))") err=$(sanitize "$(head -c200 "$PROBE_TMP/err.txt")")"
fi
probe legacy_activate_criteria "fail" "SELECT fn_v5_proof_activate_ordinary('v5p/nonexistent','garage','ns','c','$(printf 'e%.0s' {1..64})')"
probe metadata_direct_delete "fail" "DELETE FROM attachment WHERE id=900000101"
probe worker_select_queue_ok "1" "SET ROLE v5_worker; SELECT 1"
probe claim_requires_worker_role "fail" "SET ROLE v5_app_runtime; SELECT * FROM fn_v5_claim_ordinary(gen_random_uuid())"
probe reaper_requires_worker_role "fail" "SET ROLE v5_app_runtime; SELECT fn_v5_reaper_ordinary(10)"
if docker exec -i -e PGPASSWORD=v5_probe_runtime_pw "$PG_CONTAINER" psql -X -U v5_probe_runtime -h 127.0.0.1 -d "$DB" -tAc "SET ROLE v5_fn_owner" > /dev/null 2>&1; then
  bad=$((bad+1)); echo "{\"name\":\"runtime_set_role_owner\",\"pass\":false,\"detail\":\"runtime escalated to owner\",\"driver\":\"$V5_PROBE_DRIVER\"}" >> "$OUT"
else
  ok=$((ok+1)); echo "{\"name\":\"runtime_set_role_owner\",\"pass\":true,\"driver\":\"$V5_PROBE_DRIVER\"}" >> "$OUT"
fi
probe pg_canonical_json_digest_vector "ok" "SELECT CASE WHEN jsonb_build_object('a',1,'b',2)=jsonb_build_object('b',2,'a',1) AND jsonb_build_object('a',1)<>jsonb_build_object('a',2) THEN 'ok' ELSE 'fail' END"

# ---------------- enterprise queue (enterprise_asset_delete_queue) ----------------
probe worker_arbitrary_enqueue "fail" "SET ROLE v5_worker; SELECT fn_v5_intent_ordinary_delete('v5p/seed/1','probe')"
probe worker_arbitrary_enqueue_eb "fail" "SET ROLE v5_worker; INSERT INTO enterprise_asset_delete_queue(organization_id,workspace_id,asset_id,object_key,job_state) VALUES (98000101,97000101,$((910000180 + EA_OFF)),'v5e/we-$RUN_TAG','ready')"
probe worker_cannot_register "fail" "SET ROLE v5_worker; SELECT fn_v5_register_pending('v5p/w1','b','private',101,gen_random_uuid(),'$REG'::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL)"
probe proof_cannot_intent "fail" "SET ROLE v5_proof; SELECT fn_v5_intent_ordinary_delete('v5p/seed/1','probe')"
probe worker_delete_queue_row "fail" "SET ROLE v5_worker; DELETE FROM attachment_object_deletion WHERE path='v5p/seed/1'"
probe worker_update_attempts "fail" "SET ROLE v5_worker; UPDATE attachment_object_deletion SET attempts=99 WHERE path='v5p/seed/1'"
probe eb_queue_insert_ready "err:permission denied for table enterprise_asset_delete_queue" "SET ROLE v5_app_runtime; INSERT INTO enterprise_asset_delete_queue(organization_id,workspace_id,asset_id,object_key,job_state,next_retry_at) VALUES (98000101,97000101,910000104,'v5e/liveref','ready',now())"
probe eb_queue_insert_leased "err:permission denied for table enterprise_asset_delete_queue" "SET ROLE v5_app_runtime; INSERT INTO enterprise_asset_delete_queue(organization_id,workspace_id,asset_id,object_key,job_state,lease_token,lease_until) VALUES (98000101,97000101,910000104,'v5e/liveref','leased',gen_random_uuid(),now())"
probe eb_queue_insert_retry_wait "err:permission denied for table enterprise_asset_delete_queue" "SET ROLE v5_app_runtime; INSERT INTO enterprise_asset_delete_queue(organization_id,workspace_id,asset_id,object_key,job_state,next_retry_at) VALUES (98000101,97000101,910000104,'v5e/liveref','retry_wait',now())"
probe eb_queue_insert_done "err:permission denied for table enterprise_asset_delete_queue" "SET ROLE v5_app_runtime; INSERT INTO enterprise_asset_delete_queue(organization_id,workspace_id,asset_id,object_key,job_state,completed_at) VALUES (98000101,97000101,910000104,'v5e/liveref','done',now())"
if ! PSQL -q > /dev/null 2>&1 -c "INSERT INTO enterprise_asset_delete_queue(organization_id,workspace_id,asset_id,object_key,job_state) VALUES (98000101,97000101,910000104,'v5e/liveref','blocked') ON CONFLICT DO NOTHING"; then
  emit queue_seed_blocked_row 0 "seed INSERT of the 910000104 blocked row failed"
fi
probe eb_activate_legacy_without_proof "err:permission denied for table enterprise_asset_delete_queue" "SET ROLE v5_app_runtime; UPDATE enterprise_asset_delete_queue SET job_state='ready' WHERE asset_id=910000104"
probe eb_forge_gc_verified "err:permission denied for table enterprise_asset" "SET ROLE v5_app_runtime; UPDATE enterprise_asset SET gc_verified=true WHERE id=910000104"
probe eb_intent_with_live_reference "err:v5 intent enterprise job already exists" "SET ROLE v5_app_runtime; SELECT fn_v5_intent_eb_delete(98000101,97000101,910000104,'probe')"
# hold fixture: a dedicated workspace so the hold cannot leak into the other
# enterprise probes of this group.  The held asset is created under that
# workspace here (id column is mandatory: no default, TSID is assigned by the
# writer).
PSQL -q > /dev/null 2>&1 <<SQL
INSERT INTO workspace(organization_id,id,name,owner_id)
SELECT 98000101,97000102,'v5probe-ws2',101 WHERE NOT EXISTS (SELECT 1 FROM workspace WHERE id=97000102);
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status)
SELECT $EA_HOLD_ASSET,98000101,97000102,'v5e/holdws-$RUN_TAG','v5hash','active'
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=$EA_HOLD_ASSET);
INSERT INTO enterprise_retention_hold(id,organization_id,workspace_id,scope_type,reason_code)
SELECT 95000101,98000101,97000102,'workspace','probe-hold' ON CONFLICT DO NOTHING;
SQL
probe eb_intent_under_hold "err:v5 intent blocked by active workspace hold" "SET ROLE v5_app_runtime; SELECT fn_v5_intent_eb_delete(98000101,97000102,$EA_HOLD_ASSET,'probe')"
probe eb_future_retention "err:v5 intent enterprise retention active" "UPDATE enterprise_asset SET retain_until=now()+interval '30 days' WHERE id=910000106; SET ROLE v5_app_runtime; SELECT fn_v5_intent_eb_delete(98000101,97000101,910000106,'probe')"
# missing audit (enterprise): same injected audit refusal, enterprise writes.
PSQL -q > /dev/null 2>&1 <<'SQL'
CREATE OR REPLACE FUNCTION v5_probe_fail_audit() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN RAISE EXCEPTION 'probe injected audit failure' USING ERRCODE='P0001'; END $$;
DROP TRIGGER IF EXISTS trg_v5_probe_fail_audit ON attachment_lifecycle_v5_audit;
CREATE TRIGGER trg_v5_probe_fail_audit BEFORE INSERT ON attachment_lifecycle_v5_audit
  FOR EACH ROW EXECUTE FUNCTION v5_probe_fail_audit();
SQL
probe eb_intent_missing_audit "err:probe injected audit failure" "SELECT fn_v5_intent_eb_delete(98000101,97000101,910000107,'probe')"
PSQL -q > /dev/null 2>&1 -c "DROP TRIGGER IF EXISTS trg_v5_probe_fail_audit ON attachment_lifecycle_v5_audit"
probe eb_intent_missing_audit_rollback "live:no-queue:no-audit" "SELECT CASE WHEN (SELECT lifecycle_state FROM enterprise_asset WHERE id=910000107)='live'
   AND (SELECT count(*) FROM enterprise_asset_delete_queue WHERE asset_id=910000107)=0
   AND (SELECT count(*) FROM attachment_lifecycle_v5_audit WHERE details->>'asset'='910000107')=0
  THEN 'live:no-queue:no-audit' ELSE 'partial-write' END"
PSQL -q > /dev/null 2>&1 <<'SQL'
CREATE OR REPLACE FUNCTION v5_probe_fail_meta() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN RAISE EXCEPTION 'probe injected metadata failure' USING ERRCODE='P0001'; END $$;
DROP TRIGGER IF EXISTS trg_v5_probe_fail_meta ON enterprise_asset;
CREATE TRIGGER trg_v5_probe_fail_meta BEFORE UPDATE ON enterprise_asset
  FOR EACH ROW WHEN (NEW.id=910000108) EXECUTE FUNCTION v5_probe_fail_meta();
SQL
probe eb_intent_missing_metadata_transition "err:probe injected metadata failure" "SELECT fn_v5_intent_eb_delete(98000101,97000101,910000108,'probe')"
PSQL -q > /dev/null 2>&1 -c "DROP TRIGGER IF EXISTS trg_v5_probe_fail_meta ON enterprise_asset"
probe eb_intent_missing_metadata_transition_rollback "live:no-queue:no-audit" "SELECT CASE WHEN (SELECT lifecycle_state FROM enterprise_asset WHERE id=910000108)='live'
   AND (SELECT count(*) FROM enterprise_asset_delete_queue WHERE asset_id=910000108)=0
   AND (SELECT count(*) FROM attachment_lifecycle_v5_audit WHERE details->>'asset'='910000108')=0
  THEN 'live:no-queue:no-audit' ELSE 'partial-write' END"
probe eb_direct_metadata_intent "err:permission denied for table enterprise_asset" "SET ROLE v5_app_runtime; UPDATE enterprise_asset SET lifecycle_state='delete_intent' WHERE id=910000105"
probe eb_direct_session_intent "err:permission denied for table enterprise_asset" "SET ROLE v5_app_runtime; UPDATE enterprise_asset SET upload_registration='{\"x\":1}'::jsonb WHERE id=910000105"
probe eb_forge_lifecycle_on_insert "fail" "SET ROLE v5_app_runtime; INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,lifecycle_state) VALUES ($((910000160 + EA_OFF)),98000101,97000101,'v5e/forge-intent-$RUN_TAG','h','delete_intent')"
probe eb_authorized_intent_atomic "intent:ready" "SET ROLE v5_app_runtime; SELECT fn_v5_intent_eb_delete(98000101,97000101,$EA_AUTH,'probe');
RESET ROLE;
SELECT CASE WHEN (SELECT job_state FROM enterprise_asset_delete_queue WHERE asset_id=$EA_AUTH)='ready'
   AND (SELECT lifecycle_state FROM enterprise_asset WHERE id=$EA_AUTH)='delete_intent'
   AND (SELECT count(*) FROM attachment_lifecycle_v5_audit WHERE queue_domain='enterprise' AND details->>'asset'='$EA_AUTH')=1
  THEN 'intent:ready' ELSE 'missing-write' END"
}

# ============ audit-actor-anonymization (guards only) ============
audit_actor_probes() {
  local ANON_ASSET=$((910000140 + EA_OFF))
  # Dedicated asset created with uploaded_by=99000101 in the INSERT itself
  # (INSERT is guard-legal); the audit-actor guard forbids substituting a
  # different non-NULL actor on UPDATE, so reuse of a 101-owned row is not an
  # option.  Parent user is recreated first so the FK is satisfied.
  PSQL -q > /dev/null 2>&1 <<SQL
INSERT INTO "user"(id,account,password,reg_ip,reg_cosv)
SELECT 99000101,'v5actor','x','127.0.0.1','probe' WHERE NOT EXISTS (SELECT 1 FROM "user" WHERE id=99000101);
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status,uploaded_by_user_id)
SELECT $ANON_ASSET,98000101,97000101,'v5e/anon-$RUN_TAG','v5hash','active',99000101
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=$ANON_ASSET);
SQL
# real lawful anonymization: deleting the parent user lets the FK SET NULL path
# run; business ownership/location/governance must not change and no GC may
# follow from the anonymization.
sqlrc "DELETE FROM \"user\" WHERE id=99000101"
ANON_RC=$?
ANON_STATE="$(sqlval "SELECT coalesce(uploaded_by_user_id::text,'NULL')||'|'||object_key||'|'||status||'|'||lifecycle_state||'|'||coalesce(storage_driver,'-') FROM enterprise_asset WHERE id=$ANON_ASSET")"
if [ $ANON_RC -eq 0 ] && [ "$ANON_STATE" = "NULL|v5e/anon-$RUN_TAG|active|live|-" ] && \
   [ "$(sqlval "SELECT count(*) FROM pg_constraint WHERE conname='fk_enterprise_asset_uploaded_by' AND confdeltype='n'")" = "1" ] && \
   [ "$(sqlval "SELECT count(*) FROM enterprise_asset_delete_queue WHERE asset_id=$ANON_ASSET")" = "0" ]; then
  emit fk_user_delete_sets_audit_null 1 "DELETE FROM user(99000101) committed; FK ON DELETE SET NULL set uploaded_by_user_id NULL while object_key/status/lifecycle/location unchanged; FK retained (confdeltype=n); no queue job/GC followed"
else
  emit fk_user_delete_sets_audit_null 0 "rc=$ANON_RC state=$ANON_STATE err=$(sanitize "$(head -c160 "$PROBE_TMP/err.txt")")"
fi
probe direct_actor_substitution_denied "fail" "SET ROLE v5_app_runtime; UPDATE enterprise_asset SET uploaded_by_user_id=99000999 WHERE id=$ANON_ASSET"
probe direct_actor_null_with_live_user_denied "fail" "UPDATE enterprise_asset SET uploaded_by_user_id=101 WHERE id=$ANON_ASSET; UPDATE enterprise_asset SET uploaded_by_user_id=NULL WHERE id=$ANON_ASSET"
}

# ============ registration-integrity (guards only) ============
registration_probes() {
  local RKEY="v5p/reg1-$RUN_TAG" RKEY2="v5p/reg2-$RUN_TAG" RKEY3="v5p/reg3-$RUN_TAG" \
        RRESUME="v5p/reg-resume-$RUN_TAG" \
        RU1="66${RUN_TAG:0:6}-6666-4666-8666-666666666666" \
        RUR="6a${RUN_TAG:0:6}-6a6a-46a6-86a6-6a6a6a6a6a6a"
PSQL -q > /dev/null 2>&1 <<SQL
BEGIN;
SELECT fn_v5_register_pending('$RKEY','b1','private',101,'$RU1','$REG'::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL);
COMMIT;
SQL
probe original_mime_conflict_after_restart "fail" "SELECT fn_v5_register_pending('$RKEY','b1','private',101,'$RU1','{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},\"declaration\":{\"filename\":\"a.png\",\"mime\":\"image/jpeg\",\"size_bytes\":\"10\",\"scope\":\"private\"}}'::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL)"
probe original_filename_conflict_after_restart "fail" "SELECT fn_v5_register_pending('$RKEY','b1','private',101,'$RU1','{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},\"declaration\":{\"filename\":\"b.png\",\"mime\":\"image/png\",\"size_bytes\":\"10\",\"scope\":\"private\"}}'::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL)"
probe snapshot_direct_dml_rejected "fail" "UPDATE attach_pending SET upload_registration='{\"x\":1}'::jsonb WHERE object_key='$RKEY'"
# ordinary "audit null does not grant resume": a session whose subject user is
# gone still refuses a changed resume (snapshot is authoritative).
PSQL -q > /dev/null 2>&1 <<SQL
INSERT INTO "user"(id,account,password,reg_ip,reg_cosv)
SELECT 99000103,'v5resumeuser','x','127.0.0.1','probe' WHERE NOT EXISTS (SELECT 1 FROM "user" WHERE id=99000103);
SELECT fn_v5_register_pending('$RRESUME','b1','private',99000103,'$RUR','{"subject":{"kind":"user","principal":"99000103"},"declaration":{"filename":"r.png","mime":"image/png","size_bytes":"10","scope":"private"}}'::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL);
DELETE FROM "user" WHERE id=99000103;
SQL
probe audit_null_does_not_grant_resume "fail" "SELECT fn_v5_register_pending('$RRESUME','b1','private',99000103,'$RUR','{\"subject\":{\"kind\":\"user\",\"principal\":\"99000103\"},\"declaration\":{\"filename\":\"r2.png\",\"mime\":\"image/png\",\"size_bytes\":\"10\",\"scope\":\"private\"}}'::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL)"
probe legacy_null_not_fabricated "fail" "UPDATE attach_pending SET client_upload_id=gen_random_uuid() WHERE object_key='v5p/seed/1'"
probe registration_immutable_update "fail" "UPDATE attach_pending SET upload_registration=upload_registration||'{\"k\":1}'::jsonb WHERE object_key='$RKEY'"
# --- group: registration occurrence of same_value_replay (the immutable group
# carries its own occurrence above; consumers distinguish by group order).
# Replaying fn_v5_register_pending with byte-identical arguments must return
# registered AND leave the persisted UUID, the frozen location triple and the
# immutable registration jsonb snapshot exactly as before the replay.
REG_SNAP1="$(sqlval "SELECT coalesce(client_upload_id::text,'-')||'|'||coalesce(storage_driver,'-')||'|'||coalesce(storage_namespace,'-')||'|'||coalesce(storage_container,'-')||'|'||coalesce(storage_locator_sha256,'-')||'|'||md5(coalesce(upload_registration::text,'-')) FROM attach_pending WHERE object_key='$RKEY'")"
probe same_value_replay "replay:idempotent" "SELECT CASE WHEN f.out_object_key='$RKEY' AND f.out_session_state='registered'
   AND (SELECT coalesce(client_upload_id::text,'-')||'|'||coalesce(storage_driver,'-')||'|'||coalesce(storage_namespace,'-')||'|'||coalesce(storage_container,'-')||'|'||coalesce(storage_locator_sha256,'-')||'|'||md5(coalesce(upload_registration::text,'-')) FROM attach_pending WHERE object_key='$RKEY')='$REG_SNAP1'
  THEN 'replay:idempotent' ELSE 'mismatch' END
 FROM fn_v5_register_pending('$RKEY','b1','private',101,'$RU1','$REG'::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL) f"
probe cross_subject_uuid_conflict "fail" "SELECT fn_v5_register_pending('$RKEY2','b1','private',202,'$RU1','$REG'::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL)"
probe key_reassignment_forbidden "fail" "SELECT fn_v5_register_pending('$RKEY','b1','private',101,'77777777-7777-7777-7777-777777777777','$REG'::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL)"
probe eb_uuid_joint_unique "fail" "SET ROLE v5_app_runtime; INSERT INTO enterprise_asset_delete_queue(organization_id,workspace_id,asset_id,object_key,client_upload_id,upload_registration,job_state,next_retry_at) VALUES (98000101,97000101,910000101,'v5e/reg','$RU1','$REG','blocked',now())"
probe register_requires_deadline "fail" "SELECT fn_v5_register_pending('$RKEY3','b1','private',101,gen_random_uuid(),'$REG'::jsonb,NULL,NULL,NULL,NULL,NULL,NULL)"
probe register_requires_expiry_le_deadline "fail" "SELECT fn_v5_register_pending('$RKEY3','b1','private',101,gen_random_uuid(),'$REG'::jsonb,now()+interval '1 hour',now()+interval '2 hours',NULL,NULL,NULL,NULL)"
probe register_partial_location "fail" "SELECT fn_v5_register_pending('$RKEY3','b1','private',101,gen_random_uuid(),'$REG'::jsonb,now()+interval '2 hours',now()+interval '1 hour','garage',NULL,NULL,NULL)"
probe register_bad_shape "fail" "SELECT fn_v5_register_pending('$RKEY3','b1','private',101,gen_random_uuid(),'{\"x\":1}'::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL)"

# ---------------- enterprise registration (fn_v5_register_eb) ----------------
# Per-run EB assets (EA_* band) carry no registration yet, so no reset UPDATE
# is needed: the registration columns are immutable once filled and only
# fn_v5_register_eb may fill them.
PSQL -q > /dev/null 2>&1 <<SQL
SELECT fn_v5_register_eb(98000101,97000101,$EA_SUB1,'v5e/sub1-$RUN_TAG','a6${RUN_TAG:0:6}-a6a6-4a6a-8a6a-a6a6a6a6a6a6','$REG'::jsonb,'user',now()+interval '2 hours',now()+interval '1 hour');
SELECT fn_v5_register_eb(98000101,97000101,$EA_SUB2,'v5e/sub2-$RUN_TAG','a7${RUN_TAG:0:6}-a7a7-4a7a-8a7a-a7a7a7a7a7a7','$REG'::jsonb,'user',now()+interval '2 hours',now()+interval '1 hour');
SELECT fn_v5_register_eb(98000101,97000101,$EA_SUB4,'v5e/sub4-$RUN_TAG','a8${RUN_TAG:0:6}-a8a8-4a8a-8a8a-a8a8a8a8a8a8','$REG'::jsonb,'user',now()+interval '2 hours',now()+interval '1 hour');
SELECT fn_v5_register_eb(98000101,97000101,$EA_WIDGET,'v5e/widget-$RUN_TAG','a9${RUN_TAG:0:6}-a9a9-4a9a-8a9a-a9a9a9a9a9a9','{"subject":{"kind":"visitor_session","principal":"sess-a","visitor_session_id":"sess-a"},"declaration":{"filename":"a.png","mime":"image/png","size_bytes":"10","scope":"private"}}'::jsonb,'visitor_session',now()+interval '2 hours',now()+interval '1 hour');
SQL
probe eb_original_mime_conflict_after_restart "fail" "SELECT fn_v5_register_eb(98000101,97000101,$EA_SUB1,'v5e/sub1-$RUN_TAG','a6${RUN_TAG:0:6}-a6a6-4a6a-8a6a-a6a6a6a6a6a6','{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},\"declaration\":{\"filename\":\"a.png\",\"mime\":\"image/jpeg\",\"size_bytes\":\"10\",\"scope\":\"private\"}}'::jsonb,'user',now()+interval '2 hours',now()+interval '1 hour')"
probe eb_original_filename_conflict_after_restart "fail" "SELECT fn_v5_register_eb(98000101,97000101,$EA_SUB2,'v5e/sub2-$RUN_TAG','a7${RUN_TAG:0:6}-a7a7-4a7a-8a7a-a7a7a7a7a7a7','{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},\"declaration\":{\"filename\":\"b.png\",\"mime\":\"image/png\",\"size_bytes\":\"10\",\"scope\":\"private\"}}'::jsonb,'user',now()+interval '2 hours',now()+interval '1 hour')"
probe eb_widget_other_visitor_session_denied "fail" "SELECT fn_v5_register_eb(98000101,97000101,$EA_WIDGET,'v5e/widget-$RUN_TAG','a9${RUN_TAG:0:6}-a9a9-4a9a-8a9a-a9a9a9a9a9a9','{\"subject\":{\"kind\":\"visitor_session\",\"principal\":\"other-session\",\"visitor_session_id\":\"other-session\"},\"declaration\":{\"filename\":\"a.png\",\"mime\":\"image/png\",\"size_bytes\":\"10\",\"scope\":\"private\"}}'::jsonb,'visitor_session',now()+interval '2 hours',now()+interval '1 hour')"
probe eb_seat_contact_subject_kind_conflict "fail" "SELECT fn_v5_register_eb(98000101,97000101,$EA_SUB3,'v5e/sub3-$RUN_TAG','b1${RUN_TAG:0:6}-b1b1-4b1b-8b1b-b1b1b1b1b1b1','{\"subject\":{\"kind\":\"contact\",\"principal\":\"101\"},\"declaration\":{\"filename\":\"a.png\",\"mime\":\"image/png\",\"size_bytes\":\"10\",\"scope\":\"private\"}}'::jsonb,'seat',now()+interval '2 hours',now()+interval '1 hour')"
probe eb_snapshot_direct_dml_rejected "err:permission denied for table enterprise_asset" "SET ROLE v5_app_runtime; UPDATE enterprise_asset SET upload_registration=upload_registration||'{\"k\":1}'::jsonb WHERE id=$EA_SUB1"
probe eb_audit_null_does_not_grant_resume "fail" "DELETE FROM \"user\" WHERE id=99000102; SELECT fn_v5_register_eb(98000101,97000101,$EA_SUB4,'v5e/sub4-$RUN_TAG','a8${RUN_TAG:0:6}-a8a8-4a8a-8a8a-a8a8a8a8a8a8','{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},\"declaration\":{\"filename\":\"r2.png\",\"mime\":\"image/png\",\"size_bytes\":\"10\",\"scope\":\"private\"}}'::jsonb,'user',now()+interval '2 hours',now()+interval '1 hour')"
probe eb_legacy_null_not_fabricated "err:permission denied for table enterprise_asset" "SET ROLE v5_app_runtime; UPDATE enterprise_asset SET client_upload_id=gen_random_uuid() WHERE id=$EA_LEGACY"
}

# ============ recovery-tombstone-integrity (guards only) ============
tombstone_probes() {
PSQL -q > /dev/null 2>&1 <<SQL
BEGIN;
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status,client_upload_id,upload_registration,upload_expires_at)
VALUES (910000201,98000101,97000101,'v5e/tomb','v5hash','active','88888888-8888-8888-8888-888888888888','$REG',now()-interval '1 hour')
ON CONFLICT (id) DO NOTHING;
INSERT INTO enterprise_asset_delete_queue(organization_id,workspace_id,asset_id,object_key,job_state,next_retry_at,attempts)
VALUES (98000101,97000101,910000201,'v5e/tomb','blocked',now(),0)
ON CONFLICT DO NOTHING;
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status)
SELECT 910000202,98000101,97000101,'v5e/tomb2','v5hash','active'
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=910000202);
INSERT INTO enterprise_asset_delete_queue(organization_id,workspace_id,asset_id,object_key,job_state,next_retry_at,attempts)
SELECT 98000101,97000101,910000202,'v5e/tomb2','blocked',now(),0
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset_delete_queue WHERE asset_id=910000202);
COMMIT;
SQL
probe copy_uuid_snapshot_before_metadata_delete "88888888-8888-8888-8888-888888888888" "SELECT fn_v5_copy_eb_recovery_tombstone(98000101,97000101,910000201); SELECT client_upload_id FROM enterprise_asset_delete_queue WHERE asset_id=910000201"
probe missing_snapshot_rollback "fail" "SELECT fn_v5_copy_eb_recovery_tombstone(98000101,97000101,999999999)"
probe cross_table_duplicate_uuid_rejected "fail" "SELECT fn_v5_register_pending('v5p/tomb1','b','private',101,'88888888-8888-8888-8888-888888888888','$REG'::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL)"
probe direct_dml_resurrection_denied "fail" "SET ROLE v5_app_runtime; UPDATE enterprise_asset_delete_queue SET client_upload_id='99999999-9999-9999-9999-999999999999' WHERE asset_id=910000201"
probe tombstone_update_delete_denied "fail" "DELETE FROM enterprise_asset_delete_queue WHERE asset_id=910000201"
probe legacy_null_preserved "NULL" "SELECT fn_v5_copy_eb_recovery_tombstone(98000101,97000101,910000202); SELECT coalesce(client_upload_id::text,'NULL') FROM enterprise_asset_delete_queue WHERE asset_id=910000202"
# real down with a recovery tombstone: copy the live UUID+snapshot into the
# queue through the restricted function, then replay the real 00000179 down.
ONDB "$CYCLE_DB" -q > /dev/null 2>&1 <<SQL
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status,client_upload_id,upload_registration,upload_expires_at)
VALUES (910000301,98000101,97000101,'v5e/cyc-recovery','v5hash','active','8d8d8d8d-8d8d-8d8d-8d8d-8d8d8d8d8d8d','$REG',now()-interval '1 hour')
ON CONFLICT (id) DO NOTHING;
INSERT INTO enterprise_asset_delete_queue(organization_id,workspace_id,asset_id,object_key,job_state)
VALUES (98000101,97000101,910000301,'v5e/cyc-recovery','blocked') ON CONFLICT DO NOTHING;
SQL
ONDB "$CYCLE_DB" -q -c "SELECT fn_v5_copy_eb_recovery_tombstone(98000101,97000101,910000301)" >/dev/null 2>&1
PROBE_DB="$CYCLE_DB"
probe down_with_recovery_tombstone_refused "err:V5_DOWN_BLOCKED_USE_FORWARD_FIX" "$DOWN179"
PROBE_DB=""
if down_blocked "$CYCLE_DB" "$M_DOWN179"; then
  emit down_with_recovery_tombstone_block_attributable 1 "00000179.down.sql replay with the copied recovery tombstone (client_upload_id+snapshot) is refused before any DDL; the tombstone row is still present"
else
  emit down_with_recovery_tombstone_block_attributable 0 "down did not report the V5 block with a recovery tombstone present"
fi
}

# ============ cross-phase (both phases) ============
cross_phase_probes() {
local n_expand n_guards
n_expand=$(ONDB "$EXPAND_DB" -tAq -c "SELECT count(*)||'.'||coalesce(sum(hashtext(md5(p.prosrc))::bigint)::text,'0') FROM pg_proc p WHERE p.pronamespace='public'::regnamespace AND p.proname LIKE 'fn\_v5\_%'" 2>/dev/null || echo "err")
n_guards=$(ONDB "$GUARDS_DB" -tAq -c "SELECT count(*)||'.'||coalesce(sum(hashtext(md5(p.prosrc))::bigint)::text,'0') FROM pg_proc p WHERE p.pronamespace='public'::regnamespace AND p.proname LIKE 'fn\_v5\_%'" 2>/dev/null || echo "err2")
if [ -n "$n_expand" ] && [ -n "$n_guards" ] && [ "$n_expand" = "$n_guards" ] && [[ "$n_expand" =~ ^[0-9]+\.[0-9]+$ ]]; then
  ok=$((ok+1)); echo "{\"name\":\"function_digest_same_across_phases\",\"pass\":true,\"driver\":\"$V5_PROBE_DRIVER\"}" >> "$OUT"
else
  bad=$((bad+1)); echo "{\"name\":\"function_digest_same_across_phases\",\"pass\":false,\"detail\":\"expand=$n_expand guards=$n_guards\",\"driver\":\"$V5_PROBE_DRIVER\"}" >> "$OUT"
fi
# real question for this leaf: does the guards step install any function the
# expand (M1/M2) database did not already have?  Compare the complete
# signature/owner/definition inventory between the two phase databases.
local inv_expand inv_guards
inv_expand="$(ONDB "$EXPAND_DB" -tAq -c "SELECT count(*)||':'||md5(coalesce(string_agg(p.proname||'('||pg_get_function_identity_arguments(p.oid)||')|'||r.rolname||'|'||md5(p.prosrc),',' ORDER BY p.proname,pg_get_function_identity_arguments(p.oid)),'')) FROM pg_proc p JOIN pg_roles r ON r.oid=p.proowner WHERE p.pronamespace='public'::regnamespace AND p.proname LIKE 'fn\_v5\_%'" 2>/dev/null || echo "err")"
inv_guards="$(ONDB "$GUARDS_DB" -tAq -c "SELECT count(*)||':'||md5(coalesce(string_agg(p.proname||'('||pg_get_function_identity_arguments(p.oid)||')|'||r.rolname||'|'||md5(p.prosrc),',' ORDER BY p.proname,pg_get_function_identity_arguments(p.oid)),'')) FROM pg_proc p JOIN pg_roles r ON r.oid=p.proowner WHERE p.pronamespace='public'::regnamespace AND p.proname LIKE 'fn\_v5\_%'" 2>/dev/null || echo "err2")"
# real fault action: hide one M3-required guard function INSIDE a transaction
# (rename under its definer owner), then (1) run the exact fn_present SQL
# equivalent of attachment_guards_activation_check:check/1 over its frozen
# 12-entry REQUIRED_FUNCTIONS list, (2) attempt the REAL guards-contract.sql
# activation replay (trigger attach), and (3) roll back.  The precondition
# must report the hidden function in missing_functions, the activation replay
# must be refused at the missing function, and the refused activation must
# not back-fill/install anything: after rollback the function, the missing
# list and the trigger count are all restored to their baseline values.  The
# migration activation entry itself cannot be driven safely inside this
# transaction (the migrator owns its own connection), so this probe drives
# the check's real SQL plus the real activation contract replay and reports
# that path here.
local GDIF_FN="fn_v5_first_bind_guard" GDIF_HIDDEN="fn_v5_first_bind_guard_probe_hidden" GDIF_TRG="trg_v5_attachment_first_bind"
local MISS_SQL="SELECT coalesce(string_agg(req.name||'/'||req.nargs, ',' ORDER BY req.name),'none') FROM (VALUES ('fn_v5_attachment_location_immutable',0),('fn_v5_attachment_identity_immutable',0),('fn_v5_first_bind_guard',0),('fn_v5_pending_session_guard',0),('fn_v5_eb_registration_guard',0),('fn_v5_eb_audit_actor_guard',0),('fn_v5_queue_job_guard',0),('fn_v5_metadata_delete_guard',0),('fn_v5_intent_ordinary_delete',2),('fn_v5_intent_eb_delete',4),('fn_v5_intent_pending_delete',3),('fn_v5_intent_purged_delete',2)) AS req(name,nargs) WHERE NOT EXISTS (SELECT 1 FROM pg_proc p JOIN pg_namespace ns ON ns.oid=p.pronamespace WHERE ns.nspname='public' AND p.proname=req.name AND p.pronargs=req.nargs)"
local base hidden_line orig_line gdif_replay_rc delta restored base_after trig_before trig_after
local base_clean=1 hidden_clean=0
base="$(ONDB "$EXPAND_DB" -tAq -c "$MISS_SQL" 2>/dev/null)"
case "$base" in *"fn_v5_first_bind_guard/0"*) base_clean=0 ;; esac
trig_before="$(ONDB "$EXPAND_DB" -tAq -c "SELECT count(*) FROM pg_trigger WHERE tgname='$GDIF_TRG' AND NOT tgisinternal" 2>/dev/null)"
# step 1: transactional hide + in-transaction precondition report + no
# duplicate under the original name while hidden.
{ printf "BEGIN;\nSET ROLE v5_fn_owner;\nALTER FUNCTION %s() RENAME TO %s;\nRESET ROLE;\nSELECT 'hidden_missing='||(%s);\nSELECT 'original_present='||(SELECT count(*) FROM pg_proc WHERE pronamespace='public'::regnamespace AND proname='%s');\nROLLBACK;\n" "$GDIF_FN" "$GDIF_HIDDEN" "$MISS_SQL" "$GDIF_FN"
} | ONDB "$EXPAND_DB" -tAq > "$PROBE_TMP/out.txt" 2>>"$PROBE_TMP/err.txt"
hidden_line="$(grep -o 'hidden_missing=.*' "$PROBE_TMP/out.txt" | head -1)"
orig_line="$(grep -o 'original_present=[0-9]*' "$PROBE_TMP/out.txt" | head -1)"
case "${hidden_line#hidden_missing=}" in *"fn_v5_first_bind_guard/0"*) hidden_clean=1 ;; esac
delta="$(comm -13 <(tr ',' '\n' <<< "$base" | sort) <(tr ',' '\n' <<< "${hidden_line#hidden_missing=}" | sort) | tr '\n' ' ')"
delta="${delta% }"
# step 2: real activation replay against the hidden function (refused at the
# first trigger whose function is missing; disconnect rolls the hide back).
{ printf "BEGIN;\nSET ROLE v5_fn_owner;\nALTER FUNCTION %s() RENAME TO %s;\nRESET ROLE;\n" "$GDIF_FN" "$GDIF_HIDDEN"; cat "$GUARDS_SQL"; } | ONDB "$EXPAND_DB" -q >"$PROBE_TMP/out.txt" 2>"$PROBE_TMP/err.txt"
gdif_replay_rc=$?
restored="$(ONDB "$EXPAND_DB" -tAq -c "SELECT count(*) FROM pg_proc WHERE pronamespace='public'::regnamespace AND proname='$GDIF_FN'" 2>/dev/null)"
base_after="$(ONDB "$EXPAND_DB" -tAq -c "$MISS_SQL" 2>/dev/null)"
trig_after="$(ONDB "$EXPAND_DB" -tAq -c "SELECT count(*) FROM pg_trigger WHERE tgname='$GDIF_TRG' AND NOT tgisinternal" 2>/dev/null)"
if [ -n "$inv_expand" ] && [ "$inv_expand" = "$inv_guards" ] && [ "${inv_expand%:*}" -ge 15 ] 2>/dev/null && \
   [ "$base_clean" = "1" ] && [ "$hidden_clean" = "1" ] && [ "$delta" = "fn_v5_first_bind_guard/0" ] && \
   [ "$orig_line" = "original_present=0" ] && [ "$gdif_replay_rc" -ne 0 ] && \
   grep -qF 'function fn_v5_first_bind_guard() does not exist' "$PROBE_TMP/err.txt" && \
   [ "$restored" = "1" ] && [ "$base_after" = "$base" ] && [ "$trig_after" = "$trig_before" ]; then
  emit guards_do_not_install_missing_functions 1 "complete fn_v5_* inventory identical across phases (count:digest $inv_expand); real fault action: fn_v5_first_bind_guard hidden in-transaction -> activation_check fn_present SQL reports exactly it in missing_functions, real guards-contract.sql activation replay refused with 'function fn_v5_first_bind_guard() does not exist' at trg_v5_attachment_first_bind (rc=$gdif_replay_rc), nothing back-filled: after rollback function restored, missing list and trigger count equal to baseline"
else
  emit guards_do_not_install_missing_functions 0 "inventory expand=$inv_expand guards=$inv_guards base_clean=$base_clean hidden_clean=$hidden_clean delta='$delta' orig=$orig_line replay_rc=$gdif_replay_rc restored=$restored base_restored=$([ "$base_after" = "$base" ] && echo yes || echo no) triggers=$trig_before/=$trig_after err=$(sanitize "$(head -c200 "$PROBE_TMP/err.txt")")"
fi
}

run_group() { # single named group (development filter: V5_PROBE_ONLY)
  case "$1" in
    migration) migration_probes ;;
    down-block) down_block_probes ;;
    immutable) immutable_probes ;;
    session-link) session_link_probes ;;
    intent) intent_authority_probes ;;
    audit-actor) audit_actor_probes ;;
    tombstone) tombstone_probes ;;
    registration) registration_probes ;;
    cross-phase) cross_phase_probes ;;
  esac
}
if [ -n "${V5_PROBE_ONLY:-}" ]; then
  seed; seed_enterprise_extras; run_group "$V5_PROBE_ONLY"
else
case "$PHASE" in
  expand) migration_probes; down_block_probes; cross_phase_probes ;;
  guards) migration_probes; down_block_probes; immutable_probes; session_link_probes
          intent_authority_probes; audit_actor_probes; tombstone_probes; registration_probes
          cross_phase_probes ;;
  *) echo "bad phase" >&2; exit 2 ;;
esac
fi
echo "{\"phase\":\"$PHASE\",\"ok\":$ok,\"bad\":$bad,\"driver\":\"$V5_PROBE_DRIVER\"}" >> "$OUT"
cp "$OUT" "${OUT%.jsonl}-summary.txt" 2>/dev/null || true
[ $bad -eq 0 ] || exit 30
exit 0
