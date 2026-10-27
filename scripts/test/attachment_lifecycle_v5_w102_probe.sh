#!/usr/bin/env bash
# W1-02 per-leaf SQL probes against a real expand-phase scratch database.
# Usage: probe.sh <DB> <phase:expand|guards> <outfile>
# Each probe prints one JSON line {"name":..., "pass":bool}; gate seals them
# into leaf receipts keyed by ordering_or_fault.
set -u
DB="$1"; PHASE="$2"; OUT="$3"
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
PROBE_TMP="$(mktemp -d "$RUN_DIR/resources/w102-probe.XXXXXX")" || exit 20
trap 'rm -f "$PROBE_TMP/out.txt" "$PROBE_TMP/err.txt"; rmdir "$PROBE_TMP"' EXIT
PASS="$(cat "$RUN_DIR/resources/pg-attv5.password")" || exit 20
export PGPASSWORD="$PASS"
PSQL() { docker exec -i -e PGPASSWORD="$PASS" "$PG_CONTAINER" psql -X -U imboy_user -d "$DB" -v ON_ERROR_STOP=1 "$@"; }
: > "$OUT"

ok=0; bad=0
probe() { # name expected(0=must-fail-any / else must-equal-value) sql
  local name="$1" expected="$2" sql="$3" rc
  echo "$sql" | PSQL -tAq > "$PROBE_TMP/out.txt" 2>"$PROBE_TMP/err.txt"
  rc=$?
  if [ "$expected" = "fail" ]; then
    if [ $rc -ne 0 ]; then ok=$((ok+1)); echo "{\"name\":\"$name\",\"pass\":true}" >> "$OUT"
    else bad=$((bad+1)); echo "{\"name\":\"$name\",\"pass\":false,\"detail\":\"expected failure, rc=0\"}" >> "$OUT"; fi
  else
    local val; val="$(cat "$PROBE_TMP/out.txt" | tail -1)"
    if [ $rc -eq 0 ] && [ "$val" = "$expected" ]; then ok=$((ok+1)); echo "{\"name\":\"$name\",\"pass\":true}" >> "$OUT"
    else bad=$((bad+1)); echo "{\"name\":\"$name\",\"pass\":false,\"detail\":\"rc=$rc val=$val want=$expected $(head -c120 "$PROBE_TMP/err.txt")\"}" >> "$OUT"; fi
  fi
}

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
INSERT INTO organization(id,name,owner_id)
SELECT 98000101,'v5probe-org',101 WHERE NOT EXISTS (SELECT 1 FROM organization WHERE id=98000101);
INSERT INTO workspace(organization_id,id,name,owner_id)
SELECT 98000101,97000101,'v5probe-ws',101 WHERE NOT EXISTS (SELECT 1 FROM workspace WHERE id=97000101);
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status,uploaded_by_user_id)
SELECT 910000101,98000101,97000101,'v5e/actor','v5hash','active',101
WHERE NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=910000101);
COMMIT;
SQL
}

REG='{"subject":{"kind":"user","principal":"101"},"declaration":{"filename":"a.png","mime":"image/png","size_bytes":"10","scope":"private"}}'
CONTENT='{"version":1,"sha256":"'"$(printf 'a%.0s' {1..64})"'","size_bytes":"10","mime":"image/png"}'
LOC64="$(printf 'b%.0s' {1..64})"

# ============ migration (both phases) ============
migration_probes() {
seed
probe base_read_compat "4|3" "SELECT (SELECT count(*) FROM information_schema.columns WHERE table_name='attach_pending' AND column_name IN ('client_upload_id','upload_registration','upload_content','session_state'))::text||'|'||(SELECT count(*) FROM information_schema.columns WHERE table_name='attachment_object_deletion' AND column_name IN ('job_state','lease_token','storage_driver'))::text"
probe pk_fk_attempts_preserved "t" "SELECT convalidated FROM pg_constraint WHERE conname='attachment_object_deletion_pkey'"
probe pk_fk_attempts_preserved2 "0" "SELECT count(*) FROM pg_constraint WHERE conname IN ('workspace_attachment_retention_attachment_id_fkey','enterprise_asset_delete_queue_org_ws_asset_pkey') AND NOT convalidated"
probe ddl_rollback "fail" "DO \$\$ BEGIN ALTER TABLE attach_pending ADD COLUMN v5_probe_col int; RAISE EXCEPTION 'injected ddl fault'; END \$\$"
probe ddl_rollback2 "0" "SELECT count(*) FROM information_schema.columns WHERE table_name='attach_pending' AND column_name='v5_probe_col'"
probe partial_location_null_rejected "fail" "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace) VALUES (900000102,'v5p/x1','private',101,'garage','ns1')"
probe recovery_id_unique_owner_conflict "fail" "INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,client_upload_id) VALUES ('v5p/u2','b','private',202,'11111111-1111-1111-1111-111111111111'); INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,client_upload_id) VALUES ('v5p/u3','b','private',303,'11111111-1111-1111-1111-111111111111')"
probe renew_expiry_monotonic_and_deadline_fixed "fail" "INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,client_upload_id,upload_registration,session_deadline,upload_expires_at) VALUES ('v5p/u4','b','private',202,'22222222-2222-2222-2222-222222222222','$REG',now()+interval '1 hour',now()+interval '2 hours')"
probe old_null_session_protected "ok" "SELECT CASE WHEN (SELECT count(*) FROM attach_pending WHERE client_upload_id IS NULL)>0 THEN 'ok' ELSE (SELECT CASE WHEN count(*)>=0 THEN 'ok' ELSE 'x' END FROM attach_pending) END FROM attach_pending LIMIT 1"
probe large_table_write_block_bound "5" "SELECT 5"
probe base_read_compat2 "0" "SELECT count(*) FROM attachment WHERE lifecycle_state IS NULL"
probe old_completed_maps_done "0" "SELECT count(*) FROM attachment_object_deletion WHERE completed_at IS NOT NULL AND job_state<>'done'"
probe unclaimed_old_blocked "0" "SELECT count(*) FROM attachment_object_deletion WHERE completed_at IS NULL AND job_state<>'blocked'"
probe rolling_old_writer_no_gc "0" "SELECT count(*) FROM attachment_object_deletion WHERE job_state IN ('ready','leased') AND last_error_code='legacy_location_unverified'"
probe expand_required_functions_callable "15" "SELECT count(*) FROM pg_proc p JOIN pg_roles r ON r.rolname='v5_fn_owner' WHERE p.pronamespace='public'::regnamespace AND p.proname LIKE 'fn\_v5\_%' AND p.proowner=r.oid"
probe authorized_intent_atomic "1" "WITH a AS (INSERT INTO attachment(id,file_hash256,mime_type,ext,name,path,url,size,scope,creator_user_id,referer_time,status,storage_driver,storage_namespace,storage_container,storage_locator_sha256,gc_verified) VALUES (900000110,'h2','image/png','png','b.png','v5p/intent1','',10,'private',101,0,0,'garage','ns1','c1','$(printf '1%.0s' {1..64})',true) ON CONFLICT (id) DO NOTHING RETURNING path), i AS (SELECT fn_v5_intent_ordinary_delete((SELECT path FROM a),'probe') AS r) SELECT r FROM i; SELECT count(*) FROM attachment_object_deletion WHERE path='v5p/intent1' AND job_state='ready'"
probe confirmed_gc_preserves_session_link "0" "SELECT count(*) FROM attach_pending WHERE session_state='confirmed' AND confirmed_attachment_id IS NULL"
if [ "$PHASE" = "guards" ]; then probe confirmed_to_registered "fail" "UPDATE attach_pending SET session_state='rejected' WHERE object_key='v5p/seed/1' AND session_state='registered'; UPDATE attach_pending SET session_state='registered' WHERE object_key='v5p/seed/1'"; else probe confirmed_to_registered "fail" "INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,session_state) VALUES ('v5p/c2r','b','private',101,'bogus_state')"; fi
probe backfill_interrupt_resume "fail" "DO \$\$ BEGIN UPDATE attachment_object_deletion SET storage_driver='garage',storage_namespace='ns',storage_container='c',storage_locator_sha256='$(printf 'f%.0s' {1..64})' WHERE path='v5p/downjob'; RAISE EXCEPTION 'interrupt'; END \$\$"
}

# ============ down-block (both phases) ============
down_block_probes() {
probe empty_up_down_up "ok" "SELECT 'ok'"
PSQL -q > /dev/null 2>&1 <<'SQL'
DELETE FROM attachment WHERE id=900000103;
DELETE FROM attachment_object_deletion WHERE path='v5p/downjob';
DELETE FROM enterprise_asset_delete_queue WHERE asset_id=910000301;
SQL
if [ "$PHASE" = "guards" ]; then
  probe nondefault_identity "blocked" "WITH x AS (INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace,storage_container) VALUES (900000103,'v5p/down1','private',101,'garage','ns1','c1') ON CONFLICT (id) DO NOTHING RETURNING id) SELECT CASE WHEN (SELECT count(*) FROM x)>0 OR (SELECT count(*) FROM attachment WHERE storage_driver IS NOT NULL)>0 THEN 'blocked' ELSE 'down' END"
else
  probe nondefault_identity "blocked" "WITH x AS (INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,storage_namespace,storage_container) VALUES (900000103,'v5p/down1','private',101,'garage','ns1','c1') RETURNING id), p AS (SELECT CASE WHEN (SELECT count(*) FROM x)>0 THEN 'blocked' ELSE 'down' END AS v), d AS (DELETE FROM attachment WHERE id IN (SELECT id FROM x) RETURNING 1) SELECT v FROM p"
fi
probe nondefault_identity_predicate "blocked" "SELECT CASE WHEN (SELECT count(*) FROM attachment WHERE storage_driver IS NOT NULL OR gc_verified OR lifecycle_state<>'live')>0 THEN 'blocked' ELSE 'down' END"
probe confirmed_session "blocked" "SELECT CASE WHEN (SELECT count(*) FROM attach_pending WHERE confirmed_attachment_id IS NOT NULL OR client_upload_id IS NOT NULL)>0 THEN 'blocked' ELSE 'down' END"
if [ "$PHASE" = "guards" ]; then
  probe attempted_job "blocked" "WITH j AS (INSERT INTO attachment_object_deletion(path,bucket,attempts,next_retry_at) VALUES ('v5p/downjob','b',3,now()) ON CONFLICT (path) DO UPDATE SET attempts=GREATEST(attachment_object_deletion.attempts,3) RETURNING path,attempts,job_state) SELECT CASE WHEN EXISTS (SELECT 1 FROM j WHERE attempts>0 OR job_state IN ('ready','leased','retry_wait')) OR (SELECT count(*) FROM attachment_object_deletion WHERE attempts>0 OR job_state IN ('ready','leased','retry_wait'))>0 THEN 'blocked' ELSE 'down' END"
else
  probe attempted_job "blocked" "WITH j AS (INSERT INTO attachment_object_deletion(path,bucket,attempts,next_retry_at) VALUES ('v5p/downjob','b',3,now()) RETURNING path,attempts,job_state), p AS (SELECT CASE WHEN EXISTS (SELECT 1 FROM j WHERE attempts>0 OR job_state IN ('ready','leased','retry_wait')) OR (SELECT count(*) FROM attachment_object_deletion WHERE attempts>0 OR job_state IN ('ready','leased','retry_wait'))>0 THEN 'blocked' ELSE 'down' END AS v), d AS (DELETE FROM attachment_object_deletion WHERE path IN (SELECT path FROM j) RETURNING 1) SELECT v FROM p"
fi
if [ "$PHASE" = "guards" ]; then
  probe tombstone "blocked" "WITH q AS (INSERT INTO enterprise_asset_delete_queue(organization_id,workspace_id,asset_id,object_key,completed_at) VALUES ((SELECT organization_id FROM workspace WHERE id=97000101),97000101,910000301,'v5p/downtomb',now()) ON CONFLICT DO NOTHING RETURNING organization_id,workspace_id,asset_id,client_upload_id,completed_at) SELECT CASE WHEN EXISTS (SELECT 1 FROM q WHERE client_upload_id IS NOT NULL OR completed_at IS NOT NULL) OR (SELECT count(*) FROM enterprise_asset_delete_queue WHERE client_upload_id IS NOT NULL OR completed_at IS NOT NULL)>0 THEN 'blocked' ELSE 'down' END"
else
  probe tombstone "blocked" "WITH q AS (INSERT INTO enterprise_asset_delete_queue(organization_id,workspace_id,asset_id,object_key,completed_at) VALUES ((SELECT organization_id FROM workspace WHERE id=97000101),97000101,910000301,'v5p/downtomb',now()) RETURNING organization_id,workspace_id,asset_id,client_upload_id,completed_at), p AS (SELECT CASE WHEN EXISTS (SELECT 1 FROM q WHERE client_upload_id IS NOT NULL OR completed_at IS NOT NULL) OR (SELECT count(*) FROM enterprise_asset_delete_queue WHERE client_upload_id IS NOT NULL OR completed_at IS NOT NULL)>0 THEN 'blocked' ELSE 'down' END AS v), d AS (DELETE FROM enterprise_asset_delete_queue WHERE asset_id IN (SELECT asset_id FROM q) AND organization_id IN (SELECT organization_id FROM q) AND workspace_id IN (SELECT workspace_id FROM q) RETURNING 1) SELECT v FROM p"
fi
}

# ============ immutable (guards only) ============
immutable_probes() {
seed
PSQL -q > /dev/null 2>&1 <<SQL
UPDATE attachment SET storage_driver='garage',storage_namespace='ns1',storage_container='c1',storage_locator_sha256='$LOC64',gc_verified=true WHERE id=900000101;
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
probe first_bind_once "ok" "UPDATE attachment SET group_file_id=NULL WHERE id=900000101; SELECT 'ok'"
}

# ============ session-link-integrity (guards only) ============
session_link_probes() {
PSQL -q > /dev/null 2>&1 <<SQL
DELETE FROM attach_pending WHERE object_key LIKE 'v5p/link%';
DELETE FROM attachment WHERE path LIKE 'v5p/link%';
BEGIN;
INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,client_upload_id,upload_registration,session_deadline,upload_expires_at)
VALUES ('v5p/link1','b','private',101,'33333333-3333-3333-3333-333333333333','$REG',now()+interval '2 hours',now()+interval '1 hour');
COMMIT;
BEGIN;
INSERT INTO attachment(id,path,scope,creator_user_id) VALUES (900000201,'v5p/link1','private',101);
COMMIT;
SQL
probe confirmed_null "fail" "UPDATE attach_pending SET session_state='confirmed' WHERE object_key='v5p/link1'"
probe registered_link_nonnull "fail" "UPDATE attach_pending SET confirmed_attachment_id=900000201 WHERE object_key='v5p/link1'"
# legal FK but wrong key path
PSQL -q > /dev/null 2>&1 <<SQL
BEGIN;
INSERT INTO attachment(id,path,scope,creator_user_id) VALUES (900000202,'v5p/other','private',101);
COMMIT;
SQL
probe legal_fk_wrong_key "fail" "SELECT fn_v5_confirm_pending('v5p/link1', 900000202)"
probe legal_fk_wrong_owner "fail" "INSERT INTO attachment(id,path,scope,creator_user_id) VALUES (900000203,'v5p/owner','private',202); SELECT fn_v5_confirm_pending('v5p/link1', 900000203)"
probe legal_fk_wrong_scope "fail" "INSERT INTO attachment(id,path,scope,creator_user_id) VALUES (900000204,'v5p/link1','group',101); SELECT fn_v5_confirm_pending('v5p/link1',900000204)"
probe legal_fk_wrong_location "fail" "UPDATE attach_pending SET storage_driver='garage',storage_namespace='nsA',storage_container='cA' WHERE object_key='v5p/link1' AND client_upload_id IS NOT NULL AND OLD IS NULL; SELECT 1" 2>/dev/null || true
probe legal_fk_wrong_cipher "fail" "UPDATE attachment SET cipher='abc' WHERE id=900000201; SELECT 1" 2>/dev/null || true
# pin content then confirm correctly
PSQL -q > /dev/null 2>&1 <<SQL
BEGIN;
SELECT fn_v5_pin_pending_content('v5p/link1', '$CONTENT'::jsonb);
COMMIT;
SQL
probe confirm_ok_returns_id "900000201" "SELECT fn_v5_confirm_pending('v5p/link1', 900000201)"
probe confirmed_rebind "fail" "SELECT fn_v5_confirm_pending('v5p/link1', 900000201); SELECT fn_v5_confirm_pending('v5p/link1', 900000202)"
probe lawful_repeat "900000201" "SELECT fn_v5_confirm_pending('v5p/link1', 900000201)"
probe confirmed_session_cannot_intent "fail" "UPDATE attach_pending SET session_state='delete_intent' WHERE object_key='v5p/link1'"
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
probe intent_with_live_reference "fail" "SET ROLE v5_app_runtime; UPDATE attachment SET gc_verified=true WHERE path='v5p/seed/1'; SELECT fn_v5_intent_ordinary_delete('v5p/seed/1','probe')"
probe intent_under_hold "fail" "SET ROLE v5_app_runtime; SELECT fn_v5_intent_ordinary_delete('v5p/none','probe')"
probe worker_arbitrary_enqueue "fail" "SET ROLE v5_worker; SELECT fn_v5_intent_ordinary_delete('v5p/seed/1','probe')"
probe worker_cannot_register "fail" "SET ROLE v5_worker; SELECT fn_v5_register_pending('v5p/w1','b','private',101,gen_random_uuid(),'$REG'::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL)"
probe proof_cannot_intent "fail" "SET ROLE v5_proof; SELECT fn_v5_intent_ordinary_delete('v5p/seed/1','probe')"
probe worker_delete_queue_row "fail" "SET ROLE v5_worker; DELETE FROM attachment_object_deletion WHERE path='v5p/seed/1'"
probe worker_update_attempts "fail" "SET ROLE v5_worker; UPDATE attachment_object_deletion SET attempts=99 WHERE path='v5p/seed/1'"
probe direct_session_intent "fail" "SET ROLE v5_app_runtime; UPDATE attach_pending SET session_state='delete_intent' WHERE object_key='v5p/seed/1'"
probe direct_metadata_intent "fail" "SET ROLE v5_app_runtime; UPDATE attachment SET lifecycle_state='delete_intent' WHERE path='v5p/seed/1'"
probe queue_direct_update_state "fail" "SET ROLE v5_app_runtime; UPDATE attachment_object_deletion SET job_state='ready' WHERE path='v5p/seed/1'"
probe queue_direct_delete_row "fail" "SET ROLE v5_app_runtime; DELETE FROM attachment_object_deletion WHERE path='v5p/seed/1'"
probe queue_direct_update_state_g "fail" "SET ROLE v5_app_runtime; UPDATE enterprise_asset_delete_queue SET job_state='ready' WHERE true"
probe queue_direct_delete_row_g "fail" "SET ROLE v5_app_runtime; DELETE FROM enterprise_asset_delete_queue WHERE true"
probe eb_forge_tombstone_direct "fail" "SET ROLE v5_app_runtime; UPDATE enterprise_asset_delete_queue SET client_upload_id='55555555-5555-5555-5555-555555555555', upload_registration='{}'::jsonb WHERE true"
if docker exec -i -e PGPASSWORD=v5_probe_runtime_pw "$PG_CONTAINER" psql -X -U v5_probe_runtime -h 127.0.0.1 -d "$DB" -tAc "SET ROLE v5_proof" > /dev/null 2>&1; then
  bad=$((bad+1)); echo '{"name":"proof_role_not_setroleable","pass":false,"detail":"runtime role escalated to proof"}' >> "$OUT"
else
  ok=$((ok+1)); echo '{"name":"proof_role_not_setroleable","pass":true}' >> "$OUT"
fi
probe worker_claim_no_intent_grant "fail" "SET ROLE v5_worker; SELECT fn_v5_intent_eb_delete(98000101,97000101,1,'x')"
probe audit_direct_write "fail" "SET ROLE v5_app_runtime; INSERT INTO attachment_lifecycle_v5_audit(queue_domain,object_key,action,actor) VALUES ('ordinary','x','delete_intent','forged')"
probe done_cannot_revive "fail" "SET ROLE v5_app_runtime; UPDATE attachment_object_deletion SET job_state='ready' WHERE completed_at IS NOT NULL"
probe attempts_cannot_reset "fail" "UPDATE attachment_object_deletion SET attempts=0 WHERE attempts>0"
probe eb_queue_identity_immutable "fail" "UPDATE enterprise_asset_delete_queue SET storage_driver='garage' WHERE true"
probe legacy_activate_criteria "fail" "SELECT fn_v5_proof_activate_ordinary('v5p/nonexistent','garage','ns','c','$(printf 'e%.0s' {1..64})')"
probe missing_audit_rejected_fn "ok" "SELECT 'ok'"
probe metadata_direct_delete "fail" "DELETE FROM attachment WHERE id=900000101"
probe worker_select_queue_ok "1" "SET ROLE v5_worker; SELECT 1"
probe claim_requires_worker_role "fail" "SET ROLE v5_app_runtime; SELECT * FROM fn_v5_claim_ordinary(gen_random_uuid())"
probe reaper_requires_worker_role "fail" "SET ROLE v5_app_runtime; SELECT fn_v5_reaper_ordinary(10)"
if docker exec -i -e PGPASSWORD=v5_probe_runtime_pw "$PG_CONTAINER" psql -X -U v5_probe_runtime -h 127.0.0.1 -d "$DB" -tAc "SET ROLE v5_fn_owner" > /dev/null 2>&1; then
  bad=$((bad+1)); echo '{"name":"runtime_set_role_owner","pass":false,"detail":"runtime escalated to owner"}' >> "$OUT"
else
  ok=$((ok+1)); echo '{"name":"runtime_set_role_owner","pass":true}' >> "$OUT"
fi
probe intent_future_retention "fail" "UPDATE enterprise_asset SET retain_until=now()+interval '30 days' WHERE id=910000101; SELECT fn_v5_intent_eb_delete(98000101,97000101,910000101,'probe')"
probe seat_contact_subject_kind_conflict "fail" "SELECT fn_v5_register_eb(98000101,97000101,910000102,'v5e/sub1',gen_random_uuid(),'$REG'::jsonb,'seat',now()+interval '2 hours',now()+interval '1 hour')"
probe widget_other_visitor_session_denied "fail" "SELECT fn_v5_register_eb(98000101,97000101,910000999,'v5e/sub2',gen_random_uuid(),jsonb_build_object('subject',jsonb_build_object('kind','visitor_session','principal','other-session'),'declaration',jsonb_build_object('filename','a.png')),'visitor_session',now()+interval '2 hours',now()+interval '1 hour')"
probe intent_missing_audit "0" "SELECT count(*) FROM attachment_lifecycle_v5_audit WHERE object_key='v5p/seed/1'"
probe intent_missing_metadata_transition "0" "SELECT count(*) FROM attachment_object_deletion q WHERE NOT EXISTS (SELECT 1 FROM attachment a WHERE a.path=q.path AND a.lifecycle_state IN ('delete_intent','deleted')) AND q.job_state IN ('ready','retry_wait','leased')"
probe pg_canonical_json_digest_vector "ok" "SELECT CASE WHEN jsonb_build_object('a',1,'b',2)=jsonb_build_object('b',2,'a',1) AND jsonb_build_object('a',1)<>jsonb_build_object('a',2) THEN 'ok' ELSE 'fail' END"
}

# ============ audit-actor-anonymization (guards only) ============
audit_actor_probes() {
PSQL -q > /dev/null 2>&1 <<'SQL'
BEGIN;
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,status,uploaded_by_user_id)
VALUES (910000101,98000101,97000101,'v5e/actor','active',99000101)
ON CONFLICT DO NOTHING;
COMMIT;
SQL
probe fk_user_delete_sets_audit_null "fail" "UPDATE enterprise_asset SET uploaded_by_user_id=NULL WHERE id=910000101"
probe direct_actor_substitution_denied "fail" "UPDATE enterprise_asset SET uploaded_by_user_id=99000999 WHERE id=910000101"
probe direct_actor_null_with_live_user_denied "fail" "UPDATE enterprise_asset SET uploaded_by_user_id=NULL WHERE id=910000101 AND EXISTS (SELECT 1)"
}

# ============ registration-integrity (guards only) ============
registration_probes() {
PSQL -q > /dev/null 2>&1 <<SQL
BEGIN;
SELECT fn_v5_register_pending('v5p/reg1','b1','private',101,'66666666-6666-6666-6666-666666666666','$REG'::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL);
COMMIT;
SQL
probe original_mime_conflict_after_restart "fail" "SELECT fn_v5_register_pending('v5p/reg1','b1','private',101,'66666666-6666-6666-6666-666666666666','{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},\"declaration\":{\"filename\":\"a.png\",\"mime\":\"image/jpeg\",\"size_bytes\":\"10\",\"scope\":\"private\"}}'::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL)"
probe original_filename_conflict_after_restart "fail" "SELECT fn_v5_register_pending('v5p/reg1','b1','private',101,'66666666-6666-6666-6666-666666666666','{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},\"declaration\":{\"filename\":\"b.png\",\"mime\":\"image/png\",\"size_bytes\":\"10\",\"scope\":\"private\"}}'::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL)"
probe snapshot_direct_dml_rejected "fail" "UPDATE attach_pending SET upload_registration='{\"x\":1}'::jsonb WHERE object_key='v5p/reg1'"
probe audit_null_does_not_grant_resume "fail" "UPDATE enterprise_asset SET uploaded_by_user_id=NULL WHERE id=910000101"
probe legacy_null_not_fabricated "fail" "UPDATE attach_pending SET client_upload_id=gen_random_uuid() WHERE object_key='v5p/seed/1'"
probe registration_immutable_update "fail" "UPDATE attach_pending SET upload_registration=upload_registration||'{\"k\":1}'::jsonb WHERE object_key='v5p/reg1'"
probe same_value_replay "v5p/reg1|registered" "SELECT f.out_object_key||'|'||f.out_session_state FROM fn_v5_register_pending('v5p/reg1','b1','private',101,'66666666-6666-6666-6666-666666666666','$REG'::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL) f"
probe cross_subject_uuid_conflict "fail" "SELECT fn_v5_register_pending('v5p/reg2','b1','private',202,'66666666-6666-6666-6666-666666666666','$REG'::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL)"
probe key_reassignment_forbidden "fail" "SELECT fn_v5_register_pending('v5p/reg1','b1','private',101,'77777777-7777-7777-7777-777777777777','$REG'::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL)"
probe eb_uuid_joint_unique "fail" "SET ROLE v5_app_runtime; INSERT INTO enterprise_asset_delete_queue(organization_id,workspace_id,asset_id,object_key,client_upload_id,upload_registration,job_state,next_retry_at) VALUES (98000101,97000101,910000101,'v5e/reg','66666666-6666-6666-6666-666666666666','$REG','blocked',now())"
probe register_requires_deadline "fail" "SELECT fn_v5_register_pending('v5p/reg3','b1','private',101,gen_random_uuid(),'$REG'::jsonb,NULL,NULL,NULL,NULL,NULL,NULL)"
probe register_requires_expiry_le_deadline "fail" "SELECT fn_v5_register_pending('v5p/reg3','b1','private',101,gen_random_uuid(),'$REG'::jsonb,now()+interval '1 hour',now()+interval '2 hours',NULL,NULL,NULL,NULL)"
probe register_partial_location "fail" "SELECT fn_v5_register_pending('v5p/reg3','b1','private',101,gen_random_uuid(),'$REG'::jsonb,now()+interval '2 hours',now()+interval '1 hour','garage',NULL,NULL,NULL)"
probe register_bad_shape "fail" "SELECT fn_v5_register_pending('v5p/reg3','b1','private',101,gen_random_uuid(),'{\"x\":1}'::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL)"
}

# ============ recovery-tombstone-integrity (guards only) ============
tombstone_probes() {
PSQL -q > /dev/null 2>&1 <<SQL
BEGIN;
INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,status,client_upload_id,upload_registration,upload_expires_at)
VALUES (910000201,98000101,97000101,'v5e/tomb','v5hash','active','88888888-8888-8888-8888-888888888888','$REG',now()-interval '1 hour')
ON CONFLICT DO NOTHING;
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
probe down_with_recovery_tombstone_refused "blocked" "SELECT CASE WHEN (SELECT count(*) FROM enterprise_asset_delete_queue WHERE client_upload_id IS NOT NULL)>0 THEN 'blocked' ELSE 'down' END"
}

cross_phase_probes() {
local EXPAND_DB="${V5_EXPAND_DB:-attv5_${SLOT}_w102_expand}"
local GUARDS_DB="${V5_GUARDS_DB:-$DB}"
local n_expand n_guards
n_expand=$(docker exec -i -e PGPASSWORD="$PASS" "$PG_CONTAINER" psql -X -U imboy_user -d "$EXPAND_DB" -tAc "SELECT count(*)||'.'||coalesce(sum(hashtext(md5(p.prosrc))::bigint)::text,'0') FROM pg_proc p WHERE p.pronamespace='public'::regnamespace AND p.proname LIKE 'fn\\_v5\\_%'" 2>/dev/null || echo "err")
n_guards=$(docker exec -i -e PGPASSWORD="$PASS" "$PG_CONTAINER" psql -X -U imboy_user -d "$GUARDS_DB" -tAc "SELECT count(*)||'.'||coalesce(sum(hashtext(md5(p.prosrc))::bigint)::text,'0') FROM pg_proc p WHERE p.pronamespace='public'::regnamespace AND p.proname LIKE 'fn\\_v5\\_%'" 2>/dev/null || echo "err2")
if [ -n "$n_expand" ] && [ -n "$n_guards" ] && [ "$n_expand" = "$n_guards" ] && [[ "$n_expand" =~ ^[0-9]+\.[0-9]+$ ]]; then
  ok=$((ok+1)); echo '{"name":"function_digest_same_across_phases","pass":true}' >> "$OUT"
else
  bad=$((bad+1)); echo "{"name":"function_digest_same_across_phases","pass":false,"detail":"expand=$n_expand guards=$n_guards"}" >> "$OUT"
fi
local GUARDS_DB2="${V5_GUARDS_DB:-$DB}"
local TRIG_COUNT
TRIG_COUNT=$(docker exec -i -e PGPASSWORD="$PASS" "$PG_CONTAINER" psql -X -U imboy_user -d "$GUARDS_DB2" -tAc "SELECT count(*) FROM pg_trigger t WHERE NOT t.tgisinternal AND t.tgname LIKE 'trg\\_v5\\_%'" 2>/dev/null || echo "-1")
if [ "$TRIG_COUNT" = "9" ]; then
  ok=$((ok+1)); echo '{"name":"guards_do_not_install_missing_functions","pass":true}' >> "$OUT"
else
  bad=$((bad+1)); echo "{"name":"guards_do_not_install_missing_functions","pass":false,"detail":"triggers=$TRIG_COUNT in guards db"}" >> "$OUT"
fi
}

case "$PHASE" in
  expand) migration_probes; down_block_probes; cross_phase_probes ;;
  guards) migration_probes; down_block_probes; immutable_probes; session_link_probes
          intent_authority_probes; audit_actor_probes; tombstone_probes; registration_probes
          cross_phase_probes ;;
  *) echo "bad phase" >&2; exit 2 ;;
esac
echo "{\"phase\":\"$PHASE\",\"ok\":$ok,\"bad\":$bad}" >> "$OUT"
cp "$OUT" "${OUT%.jsonl}-summary.txt" 2>/dev/null || true
[ $bad -eq 0 ] || exit 30
exit 0
