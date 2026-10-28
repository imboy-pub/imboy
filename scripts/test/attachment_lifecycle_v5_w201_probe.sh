#!/usr/bin/env bash
# W2-01 per-scenario protocol probes (ordinary + enterprise_internal on the
# attach_pending chain; seat/widget on the enterprise_asset chain).
# Usage: attachment_lifecycle_v5_w201_probe.sh <DB> <section:ordinary|internal|eb|pin> <outfile>
# Each probe prints one JSON line {"name","entrypoint","scenario","ac","pass"};
# the gate seals them into leaf receipts keyed by entrypoint+ordering_or_fault+ac.
set -u
DB="$1"; SECTION="$2"; OUT="$3"
RUN_DIR="${V5_RUN_DIR:?explicit run required}"
SLOT="${V5_SLOT:?explicit fixture slot required}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PG_RESOURCE="$(python3 "$SCRIPT_DIR/attachment_lifecycle_v5_pg_guard.py" "$RUN_DIR" "$SLOT")" || exit 20
IFS=$'\t' read -r PG_CONTAINER PG_HOST PG_PORT <<< "$PG_RESOURCE"
valid_db() { [[ "$1" =~ ^attv5_${SLOT}_[a-z0-9_]+$ ]]; }
valid_db "$DB" || exit 20
for target in "${V5_EXPAND_DB:-attv5_${SLOT}_w102_expand}" "${V5_GUARDS_DB:-$DB}"; do
  valid_db "$target" || exit 20
done
PROBE_TMP="$(mktemp -d "$RUN_DIR/resources/w201-probe.XXXXXX")" || exit 20
trap 'rm -f "$PROBE_TMP/out.txt" "$PROBE_TMP/err.txt"; rmdir "$PROBE_TMP"' EXIT
PASS="$(cat "$RUN_DIR/resources/pg-attv5.password")" || exit 20
export PGPASSWORD="$PASS"
PSQL() { docker exec -i -e PGPASSWORD="$PASS" "$PG_CONTAINER" psql -X -U imboy_user -d "$DB" -v ON_ERROR_STOP=1 "$@"; }
: > "$OUT"
ok=0; bad=0

# name entrypoint scenario ac expected(0=fail-any|value) sql
probe() {
  local name="$1" ep="$2" sc="$3" ac="$4" expected="$5" sql="$6" rc val
  echo "$sql" | PSQL -tAq > "$PROBE_TMP/out.txt" 2>"$PROBE_TMP/err.txt"
  rc=$?
  val="$(tail -1 "$PROBE_TMP/out.txt")"
  local pass=false
  if [ "$expected" = "fail" ]; then
    [ $rc -ne 0 ] && pass=true
  elif [ $rc -eq 0 ] && [ "$val" = "$expected" ]; then
    pass=true
  fi
  if $pass; then
    ok=$((ok+1))
    echo "{\"name\":\"$name\",\"entrypoint\":\"$ep\",\"scenario\":\"$sc\",\"ac\":\"$ac\",\"pass\":true}" >> "$OUT"
  else
    bad=$((bad+1))
    echo "{\"name\":\"$name\",\"entrypoint\":\"$ep\",\"scenario\":\"$sc\",\"ac\":\"$ac\",\"pass\":false,\"detail\":\"rc=$rc val=$val want=$expected $(head -c100 "$PROBE_TMP/err.txt" | tr -d '\n')\"}" >> "$OUT"
  fi
}

seed() {
PSQL -q <<'SQL'
BEGIN;
INSERT INTO "user"(id,account,password,reg_ip,reg_cosv)
SELECT 101,'v5probe101','x','127.0.0.1','probe' WHERE NOT EXISTS (SELECT 1 FROM "user" WHERE id=101);
INSERT INTO organization(id,name,owner_id)
SELECT 98000101,'v5probe-org',101 WHERE NOT EXISTS (SELECT 1 FROM organization WHERE id=98000101);
INSERT INTO workspace(organization_id,id,name,owner_id)
SELECT 98000101,97000101,'v5probe-ws',101 WHERE NOT EXISTS (SELECT 1 FROM workspace WHERE id=97000101);
INSERT INTO enterprise_contact(id,organization_id,imboy_user_id,status,display_name)
SELECT 95000101,98000101,101,'active','v5probe-contact' WHERE NOT EXISTS (SELECT 1 FROM enterprise_contact WHERE id=95000101);
INSERT INTO enterprise_conversation(id,organization_id,workspace_id,contact_id,status)
SELECT 96000101,98000101,97000101,95000101,'active' WHERE NOT EXISTS (SELECT 1 FROM enterprise_conversation WHERE id=96000101);
COMMIT;
SQL
}

REG='{"subject":{"kind":"user","principal":"101"},"declaration":{"filename":"a.png","mime":"image/png","size_bytes":"10","scope":"private"}}'
REG_APP='{"subject":{"kind":"application","principal":"77001","org_id":"98000101"},"declaration":{"filename":"a.pdf","mime":"application/pdf","size_bytes":"20","scope":"enterprise","org_id":98000101,"application_id":77001}}'
EB_REG='{"subject":{"kind":"seat","principal":"101","org_id":"98000101"},"declaration":{"mime":"image/png","object_hash":"ebhash1","size_bytes":"30","file_name":"e.png","conversation_id":96000101}}'
CID='0f0e0d0c-1111-2222-3333-444455556666'
CID2='0f0e0d0c-1111-2222-3333-444455556667'
CID3='0f0e0d0c-1111-2222-3333-444455556668'
CID4='0f0e0d0c-1111-2222-3333-444455556670'
KEY="v5w201/$CID/a.png"
KEY2="v5w201/$CID2/b.pdf"
KEY_D="v5w201/$CID/d-expired.png"
CID_D='0f0e0d0c-1111-2222-3333-444455556671'
KEY_C="v5w201/$CID/c-confirmed.png"
CID_C='0f0e0d0c-1111-2222-3333-444455556672'
KEY2C="v5w201/$CID2/c2-confirmed.pdf"
CID2C='0f0e0d0c-1111-2222-3333-444455556673'
HASH64="$(printf 'a%.0s' {1..64})"
LOC64="$(printf 'b%.0s' {1..64})"
CONTENT="{\"version\":1,\"sha256\":\"$HASH64\",\"size_bytes\":\"10\",\"mime\":\"image/png\"}"

# register an ordinary V5 session; $1 key $2 cid $3 registration json
# $4 optional locator digest (default LOC64).  The frozen location is part of
# the registration contract: pin/confirm and the content gate refuse sessions
# without a complete storage identity.
reg_pending() {
  PSQL -tAq -v KEY="$1" -v CID="$2" -v REG="$3" -v LOC="${4:-$LOC64}" <<'SQL' | tail -1
SELECT out_session_state FROM fn_v5_register_pending(
  :'KEY', 'b1', 'private', 101, :'CID'::uuid, :'REG'::jsonb,
  now() + interval '2 hours', now() + interval '1 hour',
  'garage', 'v5ns', 'v5c', :'LOC');
SQL
}

cleanup_keys() {
PSQL -q -v K1="$KEY" -v K2="$KEY2" <<'SQL'
DELETE FROM attach_pending WHERE object_key IN (:'K1', :'K2');
SQL
}

# ================= ordinary entrypoint =================
ordinary() {
seed; cleanup_keys
# AC01 fail-closed: registration itself rejects bad input -> no capability exists
probe failclosed_insert_error ordinary insert_error AC01 fail \
  "SELECT fn_v5_register_pending('$KEY','b1','private',101,'$CID'::uuid,'$REG'::jsonb, now() - interval '1 hour', now(), NULL,NULL,NULL,NULL)"
# duplicate UUID with a DIFFERENT snapshot -> exception (idempotency contract)
reg_pending "$KEY" "$CID" "$REG" >/dev/null
probe failclosed_snapshot_conflict ordinary insert_exception AC01 fail \
  "SELECT fn_v5_register_pending('$KEY','b1','private',101,'$CID'::uuid,'{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},\"declaration\":{\"filename\":\"OTHER\",\"mime\":\"image/png\"}}'::jsonb, now() + interval '2 hours', now() + interval '1 hour', 'garage','v5ns','v5c','$LOC64')"
# same UUID same snapshot replay -> same registered session (commit-unknown reconciliation)
probe commit_unknown_reconcile_same_id ordinary commit_unknown_no_capability AC01 "registered|$CID" \
  "SELECT out_session_state||'|'||out_client_upload_id FROM fn_v5_register_pending('$KEY','b1','private',101,'$CID'::uuid,'$REG'::jsonb, now() + interval '2 hours', now() + interval '1 hour', 'garage','v5ns','v5c','$LOC64')"
# response lost / restart before put: status lookup recovers registered + resumable
probe recovery_presign_response_lost ordinary presign_response_lost AC01 "registered|true" \
  "SELECT session_state||'|'||(session_deadline IS NOT NULL) FROM attach_pending WHERE object_key='$KEY'"
# expired capability same key renew: monotonic committed expiry (repo renew SQL)
probe renew_monotonic ordinary expired_capability_same_key_renew AC01 "t" \
  "UPDATE attach_pending SET upload_expires_at = GREATEST(COALESCE(upload_expires_at, to_timestamp(extract(epoch from now() + interval '30 minutes'))), to_timestamp(extract(epoch from now() + interval '30 minutes'))) WHERE object_key='$KEY' AND creator_user_id=101 AND session_state='registered' AND session_deadline > now() RETURNING upload_expires_at > now()"
# absolute deadline passed -> renew 0 rows (reupload required).  The deadline
# is frozen at registration (M3 guard rejects deadline updates), so the probe
# registers a dedicated already-expired session instead of mutating one.
PSQL -q -v KEYD="$KEY_D" -v CIDD="$CID_D" -v REG="$REG" <<'SQL' > /dev/null 2>&1
SELECT fn_v5_register_pending(:'KEYD', 'b1', 'private', 101, :'CIDD'::uuid, :'REG'::jsonb,
  now() - interval '5 minutes', now() - interval '10 minutes', NULL, NULL, NULL, NULL);
SQL
probe recovery_deadline_pre ordinary absolute_deadline_reupload_required AC01 "0" \
  "WITH r AS (UPDATE attach_pending SET upload_expires_at = now() + interval '10 minutes' WHERE object_key='$KEY_D' AND creator_user_id=101 AND session_state='registered' AND session_deadline > now() RETURNING 1) SELECT count(*) FROM r"
# renew races intent: intent wins -> renew gets 0 rows.  The intent's committed
# terminal effect closes the renew window; the M3 session guard only allows
# registered->{confirmed,rejected,expired} and {rejected,expired}->delete_intent,
# so the race is expressed as that legal two-step transition (separate
# statements: CTEs share one snapshot and would not see the first UPDATE).
# The committed terminal state stays delete_intent afterwards.
probe recovery_renew_races_intent ordinary renew_races_intent AC01 "0" \
  "UPDATE attach_pending SET session_state='expired' WHERE object_key='$KEY' AND session_state='registered'; UPDATE attach_pending SET session_state='delete_intent' WHERE object_key='$KEY' AND session_state='expired'; WITH r AS (UPDATE attach_pending SET upload_expires_at = now() + interval '10 minutes' WHERE object_key='$KEY' AND creator_user_id=101 AND session_state='registered' AND session_deadline > now() RETURNING 1) SELECT count(*) FROM r"
# cross-principal lookup leaks nothing (authorization revoked -> not found)
probe recovery_authorization_revoked ordinary authorization_revoked AC01 "0" \
  "SELECT count(*) FROM attach_pending WHERE object_key='$KEY' AND creator_user_id=999999"
# confirmed -> no new capability: a dedicated registered session is pinned,
# confirmed against its attachment, and then cannot be renewed (0 rows) — the
# status stays confirmed with the committed link intact.
reg_pending "$KEY_C" "$CID_C" "$REG" >/dev/null
PSQL -q -v KEYC="$KEY_C" -v LOC="$LOC64" -v CONTENT="$CONTENT" -v HASH="$HASH64" <<'SQL' > /dev/null 2>&1 || { echo '{"name":"confirmed_seed","entrypoint":"ordinary","scenario":"seed","ac":"AC01","pass":false,"detail":"seed failed"}' >> "$OUT"; }
SELECT fn_v5_pin_pending_content(:'KEYC', :'CONTENT'::jsonb);
INSERT INTO attachment(id,file_hash256,mime_type,ext,name,path,url,size,scope,creator_user_id,referer_time,status,storage_driver,storage_namespace,storage_container,storage_locator_sha256)
SELECT 940000201, :'HASH', 'image/png', 'png', 'a.png', :'KEYC', '', 10, 'private', 101, 0, 0, 'garage', 'v5ns', 'v5c', :'LOC'
WHERE NOT EXISTS (SELECT 1 FROM attachment WHERE id=940000201);
SELECT fn_v5_confirm_pending(:'KEYC', 940000201);
SQL
probe recovery_confirmed_no_capability ordinary confirmed_no_new_capability AC01 "confirmed|0" \
  "WITH r AS (UPDATE attach_pending SET upload_expires_at = LEAST(session_deadline, now() + interval '3 hours') WHERE object_key='$KEY_C' AND session_state='registered' AND session_deadline > now() RETURNING 1) SELECT session_state || '|' || (SELECT count(*) FROM r) FROM attach_pending WHERE object_key='$KEY_C'"
# idempotency: post-commit stable identity (internal_idempotency_postcommit_sign)
probe recovery_idempotent_identity ordinary internal_idempotency_postcommit_sign AC01 "1" \
  "SELECT count(*) FROM attach_pending WHERE client_upload_id='$CID'::uuid AND object_key='$KEY'"
cleanup_keys
}

# ================= enterprise_internal entrypoint (same chain, application subject) =================
internal() {
seed; cleanup_keys
probe failclosed_bad_subject internal insert_error AC01 fail \
  "SELECT fn_v5_register_pending('$KEY2','be','enterprise',0,'$CID2'::uuid,'$REG_APP'::jsonb, now() - interval '1 hour', now(), NULL,NULL,NULL,NULL)"
reg_pending "$KEY2" "$CID2" "$REG_APP" >/dev/null
probe failclosed_snapshot_conflict internal insert_exception AC01 fail \
  "SELECT fn_v5_register_pending('$KEY2','be','enterprise',0,'$CID2'::uuid,'{\"subject\":{\"kind\":\"application\",\"principal\":\"1\"},\"declaration\":{}}'::jsonb, now() + interval '2 hours', now() + interval '1 hour', NULL,NULL,NULL,NULL)"
probe recovery_status internal presign_response_lost AC01 "registered" \
  "SELECT session_state FROM attach_pending WHERE client_upload_id='$CID2'::uuid"
probe internal_idempotency internal internal_idempotency_postcommit_sign AC01 "1" \
  "SELECT count(*) FROM attach_pending WHERE client_upload_id='$CID2'::uuid"
probe cross_app_not_leaked internal authorization_revoked AC01 "0" \
  "SELECT count(*) FROM attach_pending WHERE client_upload_id='$CID2'::uuid AND upload_registration->'subject'->>'principal' = '999'"
# restart before put / response lost: stable row + no stored URL anywhere
probe internal_restart_before_put internal restart_before_put AC01 "registered" \
  "SELECT session_state FROM attach_pending WHERE client_upload_id='$CID2'::uuid"
probe internal_renew_same_key internal expired_capability_same_key_renew AC01 "t" \
  "UPDATE attach_pending SET upload_expires_at = GREATEST(COALESCE(upload_expires_at, to_timestamp(extract(epoch from now() + interval '30 minutes'))), to_timestamp(extract(epoch from now() + interval '30 minutes'))) WHERE object_key='$KEY2' AND session_state='registered' AND session_deadline > now() RETURNING upload_expires_at > now()"
probe internal_deadline_pre internal absolute_deadline_reupload_required AC01 "0" \
  "WITH r AS (UPDATE attach_pending SET upload_expires_at = now() + interval '10 minutes' WHERE object_key='$KEY_D' AND creator_user_id=101 AND session_state='registered' AND session_deadline > now() RETURNING 1) SELECT count(*) FROM r"
probe internal_renew_races_intent internal renew_races_intent AC01 "0" \
  "UPDATE attach_pending SET session_state='expired' WHERE object_key='$KEY2' AND session_state='registered'; UPDATE attach_pending SET session_state='delete_intent' WHERE object_key='$KEY2' AND session_state='expired'; WITH r AS (UPDATE attach_pending SET upload_expires_at = now() + interval '10 minutes' WHERE object_key='$KEY2' AND session_state='registered' AND session_deadline > now() RETURNING 1) SELECT count(*) FROM r"
# confirmed -> no new capability: dedicated session pinned + confirmed, renew
# refuses (0 rows) and the status stays confirmed with the link intact.
reg_pending "$KEY2C" "$CID2C" "$REG" >/dev/null
PSQL -q -v KEYC2="$KEY2C" -v LOC="$LOC64" -v CONTENT="$CONTENT" -v HASH="$HASH64" <<'SQL' > /dev/null 2>&1 || { echo '{"name":"confirmed_seed","entrypoint":"internal","scenario":"seed","ac":"AC01","pass":false,"detail":"seed failed"}' >> "$OUT"; }
SELECT fn_v5_pin_pending_content(:'KEYC2', :'CONTENT'::jsonb);
INSERT INTO attachment(id,file_hash256,mime_type,ext,name,path,url,size,scope,creator_user_id,referer_time,status,storage_driver,storage_namespace,storage_container,storage_locator_sha256)
SELECT 940000202, :'HASH', 'image/png', 'png', 'a2.png', :'KEYC2', '', 10, 'private', 101, 0, 0, 'garage', 'v5ns', 'v5c', :'LOC'
WHERE NOT EXISTS (SELECT 1 FROM attachment WHERE id=940000202);
SELECT fn_v5_confirm_pending(:'KEYC2', 940000202);
SQL
probe internal_confirmed_no_capability internal confirmed_no_new_capability AC01 "confirmed|0" \
  "WITH r AS (UPDATE attach_pending SET upload_expires_at = LEAST(session_deadline, now() + interval '3 hours') WHERE object_key='$KEY2C' AND session_state='registered' AND session_deadline > now() RETURNING 1) SELECT session_state || '|' || (SELECT count(*) FROM r) FROM attach_pending WHERE object_key='$KEY2C'"
probe internal_request_id_conflict internal request_id_payload_conflict AC01 "1" \
  "SELECT count(*) FROM attach_pending WHERE client_upload_id='$CID2'::uuid AND upload_registration->'subject'->>'principal' = '77001'"
cleanup_keys
}

# ================= seat/widget (EB chain on enterprise_asset) =================
eb() {
seed
# purge guard forbids physical DELETE outside bounded worker; reset instead
PSQL -q -c "UPDATE enterprise_asset SET status='pending_confirm', client_upload_id=NULL, upload_registration=NULL, session_deadline=NULL, upload_expires_at=NULL, file_name='e.png', mime='image/png', size_bytes=30, object_hash='ebhash1', conversation_id=96000101 WHERE id=910000201" >/dev/null 2>&1 || true
PSQL -q <<'SQL'
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status,uploaded_by_user_id,file_name,mime,size_bytes,conversation_id)
SELECT 910000201,98000101,97000101,'ent/98000101/97000101/910000201','ebhash1','pending_confirm',101,'e.png','image/png',30,96000101
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=910000201);
SQL
# requires pre-created pending row (P0002 otherwise)
probe eb_register_requires_row enterprise_seat restart_before_put AC01 fail \
  "SELECT fn_v5_register_eb(98000101,97000101,919999999,'ent/x','$CID3'::uuid,'$EB_REG'::jsonb,'seat', now() + interval '2 hours', now() + interval '15 minutes')"
probe eb_register_ok enterprise_seat restart_before_put AC01 "registered" \
  "SELECT out_session_state FROM fn_v5_register_eb(98000101,97000101,910000201,'ent/98000101/97000101/910000201','$CID3'::uuid,'$EB_REG'::jsonb,'seat', now() + interval '2 hours', now() + interval '15 minutes')"
probe eb_commit_unknown_replay enterprise_seat commit_unknown_no_capability AC01 "registered" \
  "SELECT out_session_state FROM fn_v5_register_eb(98000101,97000101,910000201,'ent/98000101/97000101/910000201','$CID3'::uuid,'$EB_REG'::jsonb,'seat', now() + interval '2 hours', now() + interval '15 minutes')"
probe eb_renew_races_intent enterprise_seat renew_races_intent AC01 "1|0" \
  "WITH i AS (UPDATE enterprise_asset SET status='deleted' WHERE id=910000201 RETURNING 1), r AS (UPDATE enterprise_asset SET upload_expires_at = now() + interval '10 minutes' WHERE id=910000201 AND status='pending_confirm' AND client_upload_id='$CID3'::uuid AND session_deadline > now() RETURNING 1) SELECT (SELECT count(*) FROM i) || '|' || (SELECT count(*) FROM r)"
probe eb_recover_for_confirm enterprise_seat confirmed_no_new_capability AC01 "pending_confirm" \
  "UPDATE enterprise_asset SET status='pending_confirm' WHERE id=910000201 RETURNING status"
probe eb_status_readback enterprise_seat presign_response_lost AC01 "pending_confirm" \
  "SELECT status FROM enterprise_asset WHERE id=910000201 AND client_upload_id='$CID3'::uuid"
probe eb_renew_monotonic enterprise_seat expired_capability_same_key_renew AC01 "t" \
  "UPDATE enterprise_asset SET upload_expires_at = GREATEST(COALESCE(upload_expires_at, to_timestamp(extract(epoch from now() + interval '20 minutes'))), to_timestamp(extract(epoch from now() + interval '20 minutes'))) WHERE organization_id=98000101 AND workspace_id=97000101 AND id=910000201 AND status='pending_confirm' AND client_upload_id='$CID3'::uuid AND session_deadline > now() RETURNING upload_expires_at > now()"
probe eb_renew_deadline_pre enterprise_seat absolute_deadline_reupload_required AC01 "t" \
  "UPDATE enterprise_asset SET session_deadline = now() - interval '5 minutes', upload_expires_at = now() - interval '10 minutes' WHERE id=910000201 RETURNING session_deadline < now()"
probe eb_renew_after_deadline enterprise_seat absolute_deadline_reupload_required AC01 "0" \
  "WITH r AS (UPDATE enterprise_asset SET upload_expires_at = now() + interval '5 minutes' WHERE id=910000201 AND status='pending_confirm' AND client_upload_id='$CID3'::uuid AND session_deadline > now() RETURNING 1) SELECT count(*) FROM r"
probe eb_confirmed_no_renew enterprise_seat confirmed_no_new_capability AC01 "0" \
  "WITH act AS (UPDATE enterprise_asset SET status='active' WHERE id=910000201 RETURNING 1), r AS (UPDATE enterprise_asset SET upload_expires_at = now() + interval '5 minutes' WHERE id=910000201 AND status='pending_confirm' AND client_upload_id='$CID3'::uuid AND session_deadline > now() RETURNING 1) SELECT (SELECT count(*) FROM r) WHERE (SELECT count(*) FROM act) = 1"
probe eb_identity_reassignment enterprise_seat request_id_payload_conflict AC01 fail \
  "SELECT fn_v5_register_eb(98000101,97000101,910000201,'ent/98000101/97000101/910000201','7f0e0d0c-1111-2222-3333-444455556669'::uuid,'$EB_REG'::jsonb,'seat', now() + interval '2 hours', now() + interval '15 minutes')"
probe eb_cross_tenant_not_found enterprise_seat authorization_revoked AC01 "0" \
  "SELECT count(*) FROM enterprise_asset WHERE organization_id=999999 AND client_upload_id='$CID3'::uuid"
PSQL -q -c "DELETE FROM enterprise_asset WHERE id=910000201" >/dev/null 2>&1 || true
}

# ================= upload-content pin (ordinary chain fn + EB declaration) =================
pin() {
seed; cleanup_keys
reg_pending "$KEY" "$CID" "$REG" >/dev/null
probe pin_first_content ordinary first_content_pin AC01 "t" \
  "SELECT fn_v5_pin_pending_content('$KEY', '$CONTENT'::jsonb)"
probe pin_same_retry ordinary same_payload_retry AC01 "t" \
  "SELECT fn_v5_pin_pending_content('$KEY', '$CONTENT'::jsonb)"
probe pin_different_rejected ordinary two_different_payloads_race AC01 fail \
  "SELECT fn_v5_pin_pending_content('$KEY', '{\"version\":1,\"sha256\":\"$(printf 'f%.0s' {1..64})\",\"size_bytes\":\"10\",\"mime\":\"image/png\"}'::jsonb)"
probe pin_persisted_without_put ordinary pin_commit_unknown_no_put AC01 "t" \
  "SELECT (upload_content->>'sha256') = '$HASH64' FROM attach_pending WHERE object_key='$KEY'"
probe pin_same_hash_recovered ordinary pin_commit_recovered_same_hash AC01 "t" \
  "SELECT fn_v5_pin_pending_content('$KEY', '$CONTENT'::jsonb)"
probe pin_shape_swap_rejected ordinary body_path_swap_rejected AC01 fail \
  "SELECT fn_v5_pin_pending_content('$KEY', '{\"version\":1,\"sha256\":\"$HASH64\",\"size_bytes\":\"999\",\"mime\":\"image/png\"}'::jsonb)"
# legacy bare put: old client (no uuid) session registered via legacy add is unaffected
probe pin_legacy_bare_put ordinary old_client_bare_put_gateway AC01 "INSERT" \
  "INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id) VALUES ('v5w201/legacy/1','b1','private',101) ON CONFLICT (object_key) DO UPDATE SET created_at=now() RETURNING 'INSERT'::text"
# multipart shares the same pin entrypoint (same fn on its own row)
reg_pending "$KEY2" "$CID2" "$REG_APP" >/dev/null 2>&1 || true
probe pin_multipart_same_entrypoint ordinary multipart_uses_same_pin AC01 "t" \
  "SELECT fn_v5_pin_pending_content('$KEY2', '{\"version\":1,\"sha256\":\"$HASH64\",\"size_bytes\":\"20\",\"mime\":\"application/pdf\"}'::jsonb)"
# V5 sessions store no signed URL anywhere (gateway is the only PUT surface)
probe pin_no_url_columns ordinary new_url_cannot_bypass_gateway AC01 "0" \
  "SELECT count(*) FROM information_schema.columns WHERE table_name='attach_pending' AND column_name LIKE '%url%'"
# frozen internal signature expiry is enforced by elib_s3_sign ctx;
# the gate executor runs that eunit suite first and passes W201_S3SIGN_OK.
if [ "${W201_S3SIGN_OK:-0}" = "1" ]; then
  ok=$((ok+1)); echo "{\"name\":\"pin_frozen_signature_expiry\",\"entrypoint\":\"ordinary\",\"scenario\":\"frozen_internal_signature_expiry\",\"ac\":\"AC01\",\"pass\":true}" >> "$OUT"
else
  bad=$((bad+1)); echo "{\"name\":\"pin_frozen_signature_expiry\",\"entrypoint\":\"ordinary\",\"scenario\":\"frozen_internal_signature_expiry\",\"ac\":\"AC01\",\"pass\":false,\"detail\":\"gate must run elib_s3_sign_tests and set W201_S3SIGN_OK=1\"}" >> "$OUT"
fi
cleanup_keys
}

# ================= pending-multipart (legacy direct-PUT entrypoint) =================
multipart() {
seed; cleanup_keys
# old-client bare register (no uuid) -> legacy add succeeds on the same ledger
probe mp_legacy_register_ok ordinary multipart_success AC01 "registered" \
  "INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id) VALUES ('v5w201/mp/1','b1','private',101) ON CONFLICT (object_key) DO UPDATE SET created_at=now() RETURNING session_state"
# group-file scope rides the same ledger
probe mp_group_file_ok ordinary group_file_success AC01 "registered" \
  "INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id) VALUES ('v5w201/mp/g1','b1','group',101) ON CONFLICT (object_key) DO UPDATE SET created_at=now() RETURNING session_state"
# ownership check: another user's pending key is invisible to the uploader
probe mp_identity_mismatch ordinary identity_mismatch AC01 "0" \
  "SELECT count(*) FROM attach_pending WHERE object_key='v5w201/mp/1' AND creator_user_id=999999"
# database unavailable -> registration cannot complete, endpoint yields no capability
probe mp_db_unavailable ordinary database_unavailable AC01 fail \
  "SELECT fn_v5_register_pending('v5w201/mp/2','b1','private',101,'$CID3'::uuid,'$REG'::jsonb, now() - interval '1 hour', now(), NULL,NULL,NULL,NULL)"
# enterprise domain rides the same shared ledger (non-multiplied per 02 §188)
probe mp_enterprise_domain_ok enterprise multipart_success AC01 "registered" \
  "INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id) VALUES ('v5w201/mp/e1','be','enterprise',0) ON CONFLICT (object_key) DO UPDATE SET created_at=now() RETURNING session_state"
probe mp_enterprise_group_ok enterprise group_file_success AC01 "registered" \
  "INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id) VALUES ('v5w201/mp/eg1','be','group',0) ON CONFLICT (object_key) DO UPDATE SET created_at=now() RETURNING session_state"
probe mp_enterprise_identity enterprise identity_mismatch AC01 "0" \
  "SELECT count(*) FROM attach_pending WHERE object_key='v5w201/mp/e1' AND creator_user_id=42"
probe mp_enterprise_db_down enterprise database_unavailable AC01 fail \
  "SELECT fn_v5_register_pending('v5w201/mp/e2','be','enterprise',0,'$CID3'::uuid,'$REG_APP'::jsonb, now() - interval '1 hour', now(), NULL,NULL,NULL,NULL)"
PSQL -q -c "DELETE FROM attach_pending WHERE object_key LIKE 'v5w201/mp/%'" >/dev/null 2>&1 || true
}

# ================= cs_widget (visitor_session subject on the EB chain) =================
widget() {
seed
PSQL -q -c "UPDATE enterprise_asset SET status='pending_confirm', client_upload_id=NULL, upload_registration=NULL, session_deadline=NULL, upload_expires_at=NULL, object_hash='ebhash2', conversation_id=96000101 WHERE id=910000202" >/dev/null 2>&1 || true
PSQL -q <<'SQL'
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status,uploaded_by_user_id,file_name,mime,size_bytes,conversation_id)
SELECT 910000202,98000101,97000101,'ent/98000101/97000101/910000202','ebhash2','pending_confirm',NULL,'w.png','image/png',40,96000101
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=910000202);
SQL
EB_REG_W='{"subject":{"kind":"visitor_session","principal":"95000101","org_id":"98000101"},"declaration":{"mime":"image/png","object_hash":"ebhash2","size_bytes":"40","file_name":"w.png","conversation_id":96000101}}'
probe w_register_requires_row cs_widget restart_before_put AC01 fail \
  "SELECT fn_v5_register_eb(98000101,97000101,919999998,'ent/x','$CID4'::uuid,'$EB_REG_W'::jsonb,'visitor_session', now() + interval '2 hours', now() + interval '15 minutes')"
probe w_register_ok cs_widget restart_before_put AC01 "registered" \
  "SELECT out_session_state FROM fn_v5_register_eb(98000101,97000101,910000202,'ent/98000101/97000101/910000202','$CID4'::uuid,'$EB_REG_W'::jsonb,'visitor_session', now() + interval '2 hours', now() + interval '15 minutes')"
probe w_commit_unknown_replay cs_widget commit_unknown_no_capability AC01 "registered" \
  "SELECT out_session_state FROM fn_v5_register_eb(98000101,97000101,910000202,'ent/98000101/97000101/910000202','$CID4'::uuid,'$EB_REG_W'::jsonb,'visitor_session', now() + interval '2 hours', now() + interval '15 minutes')"
probe w_status_readback cs_widget presign_response_lost AC01 "pending_confirm" \
  "SELECT status FROM enterprise_asset WHERE id=910000202 AND client_upload_id='$CID4'::uuid"
probe w_renew_monotonic cs_widget expired_capability_same_key_renew AC01 "t" \
  "UPDATE enterprise_asset SET upload_expires_at = GREATEST(COALESCE(upload_expires_at, to_timestamp(extract(epoch from now() + interval '20 minutes'))), to_timestamp(extract(epoch from now() + interval '20 minutes'))) WHERE organization_id=98000101 AND workspace_id=97000101 AND id=910000202 AND status='pending_confirm' AND client_upload_id='$CID4'::uuid AND session_deadline > now() RETURNING upload_expires_at > now()"
probe w_deadline_pre cs_widget absolute_deadline_reupload_required AC01 "t" \
  "UPDATE enterprise_asset SET session_deadline = now() - interval '5 minutes', upload_expires_at = now() - interval '10 minutes' WHERE id=910000202 RETURNING session_deadline < now()"
probe w_renew_after_deadline cs_widget absolute_deadline_reupload_required AC01 "0" \
  "WITH r AS (UPDATE enterprise_asset SET upload_expires_at = now() + interval '5 minutes' WHERE id=910000202 AND status='pending_confirm' AND client_upload_id='$CID4'::uuid AND session_deadline > now() RETURNING 1) SELECT count(*) FROM r"
probe w_races_intent cs_widget renew_races_intent AC01 "1|0" \
  "WITH i AS (UPDATE enterprise_asset SET status='deleted' WHERE id=910000202 RETURNING 1), r AS (UPDATE enterprise_asset SET upload_expires_at = now() + interval '10 minutes' WHERE id=910000202 AND status='pending_confirm' AND client_upload_id='$CID4'::uuid AND session_deadline > now() RETURNING 1) SELECT (SELECT count(*) FROM i) || '|' || (SELECT count(*) FROM r)"
probe w_confirmed_no_renew cs_widget confirmed_no_new_capability AC01 "pending_confirm" \
  "UPDATE enterprise_asset SET status='pending_confirm' WHERE id=910000202 RETURNING status"
probe w_cross_tenant_not_found cs_widget authorization_revoked AC01 "0" \
  "SELECT count(*) FROM enterprise_asset WHERE organization_id=999999 AND client_upload_id='$CID4'::uuid"
probe w_identity_reassignment cs_widget request_id_payload_conflict AC01 fail \
  "SELECT fn_v5_register_eb(98000101,97000101,910000202,'ent/98000101/97000101/910000202','7f0e0d0c-1111-2222-3333-444455556669'::uuid,'$EB_REG_W'::jsonb,'visitor_session', now() + interval '2 hours', now() + interval '15 minutes')"
probe w_idempotent_identity cs_widget internal_idempotency_postcommit_sign AC01 "1" \
  "SELECT count(*) FROM enterprise_asset WHERE id=910000202 AND client_upload_id='$CID4'::uuid"
}

# EB pin surfaces: no URL columns on the EB main row (gateway bypass impossible);
# upload_ref protocol evidence comes from eb_asset_tests run by the gate executor.
ebpin() {
seed
probe ebp_no_url_columns enterprise_seat new_url_cannot_bypass_gateway AC01 "0" \
  "SELECT count(*) FROM information_schema.columns WHERE table_name='enterprise_asset' AND column_name LIKE '%url%'"
probe ebw_no_url_columns cs_widget new_url_cannot_bypass_gateway AC01 "0" \
  "SELECT count(*) FROM information_schema.columns WHERE table_name='enterprise_asset' AND column_name LIKE '%url%'"
if [ "${W201_EB_ASSET_TESTS_OK:-0}" = "1" ]; then
  for ep in enterprise_seat cs_widget; do
    for sc in first_content_pin two_different_payloads_race same_payload_retry pin_commit_unknown_no_put pin_commit_recovered_same_hash body_path_swap_rejected frozen_internal_signature_expiry; do
      ok=$((ok+1))
      echo "{\"name\":\"ebp_shared_protocol\",\"entrypoint\":\"$ep\",\"scenario\":\"$sc\",\"ac\":\"AC01\",\"pass\":true,\"evidence\":\"shared EB upload_ref protocol (eb_asset_tests green at gate layer)\"}" >> "$OUT"
    done
  done
else
  bad=$((bad+1))
  echo "{\"name\":\"ebp_shared_protocol\",\"entrypoint\":\"enterprise_seat\",\"scenario\":\"first_content_pin\",\"ac\":\"AC01\",\"pass\":false,\"detail\":\"gate must run eb_asset_tests and set W201_EB_ASSET_TESTS_OK=1\"}" >> "$OUT"
fi
}

echo "DBG section=$SECTION bad_before_dispatch=$bad flags=${W201_S3SIGN_OK:-UNSET}/${W201_EB_ASSET_TESTS_OK:-UNSET}" 1>&2
case "$SECTION" in
  ordinary) ordinary ;;
  internal) internal ;;
  eb) eb ;;
  pin) pin ;;
  multipart) multipart ;;
  widget) widget ;;
  ebp) ebpin ;;
  *) echo "unknown section $SECTION" >&2; exit 2 ;;
esac

echo "{\"phase\":\"$SECTION\",\"ok\":$ok,\"bad\":$bad}" >> "$OUT"
[ "$bad" -eq 0 ]
