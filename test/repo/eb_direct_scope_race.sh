#!/usr/bin/env bash
set -euo pipefail
umask 077
: "${EB_DIRECT_TEST_HOST:?}" "${EB_DIRECT_TEST_PORT:?}" "${EB_DIRECT_TEST_USER:?}" "${EB_DIRECT_TEST_DB:?}"
case "$EB_DIRECT_TEST_DB" in c08_eb_*) ;; *) echo 'requires an isolated C08 EB test database' >&2; exit 2;; esac
args=(-h "$EB_DIRECT_TEST_HOST" -p "$EB_DIRECT_TEST_PORT" -U "$EB_DIRECT_TEST_USER" -d "$EB_DIRECT_TEST_DB" -X -q -v ON_ERROR_STOP=1)
logs=$(mktemp -d "${TMPDIR:-/tmp}/eb-direct-scope.XXXXXXXX")
psql "${args[@]}" > "$logs/seed.log" 2>&1 <<'SQL'
BEGIN;
SET LOCAL session_replication_role=replica;
INSERT INTO public."user"(id,password,account,reg_ip,reg_cosv) VALUES
    (99006300,'x','eb-scope-race','127.0.0.1','x'),(99006399,'x','eb-scope-race-owner','127.0.0.1','x');
INSERT INTO organization(id,name,owner_id,status) VALUES(99006301,'eb-scope-race',99006399,'active');
INSERT INTO workspace(id,name,owner_id,status,type,organization_id) VALUES(99006302,'eb-scope-race',99006300,'active','project',99006301);
INSERT INTO organization_member(organization_id,user_id,role,status) VALUES
    (99006301,99006300,'member','active'),(99006301,99006399,'owner','active');
INSERT INTO organization_business_identity(id,organization_id,function_key,display_name,status) VALUES(99006305,99006301,'sales','race','active');
INSERT INTO organization_business_identity_assignment(id,organization_id,business_identity_id,function_key,user_id,status) VALUES(99006306,99006301,99006305,'sales',99006300,'active');
INSERT INTO organization_business_identity(id,organization_id,function_key,display_name,status) VALUES
    (99006314,99006301,'customer_service','intake','active'),
    (99006315,99006301,'customer_service','assignee','active');
INSERT INTO organization_business_identity_assignment(id,organization_id,business_identity_id,function_key,user_id,status) VALUES(99006316,99006301,99006315,'customer_service',99006300,'active');
INSERT INTO customer_service_seat(organization_id,business_identity_id,enabled) VALUES(99006301,99006315,true);
INSERT INTO enterprise_contact(id,organization_id,status,display_name) VALUES(99006303,99006301,'active','race');
INSERT INTO enterprise_conversation(id,organization_id,workspace_id,contact_id,business_identity_id,status) VALUES(99006304,99006301,99006302,99006303,99006305,'active');
INSERT INTO enterprise_conversation(id,organization_id,workspace_id,contact_id,business_identity_id,status) VALUES(99006317,99006301,99006302,99006303,99006314,'active');
INSERT INTO customer_service_session(id,organization_id,workspace_id,contact_id,conversation_id,business_identity_id,status)
VALUES(99006318,99006301,99006302,99006303,99006317,99006315,'active');
INSERT INTO customer_service_session(id,organization_id,workspace_id,contact_id,conversation_id,status,closed_at)
VALUES(99006308,99006301,99006302,99006303,99006304,'closed',now());
INSERT INTO customer_service_widget_installation(id,organization_id,public_widget_id,display_name,consent_version,status)
VALUES(99006310,99006301,'eb-scope-race-widget','fixture','1','active');
INSERT INTO customer_service_visit_token(id,organization_id,contact_id,token_digest,expires_at,widget_installation_id)
VALUES(99006311,99006301,99006303,repeat('d',64),now()+interval '1 hour',99006310);
UPDATE customer_service_session SET visit_token_id=99006311 WHERE id=99006308;
INSERT INTO enterprise_asset(id,organization_id,workspace_id,conversation_id,uploaded_by_user_id,
    object_key,object_hash,mime,size_bytes,status,client_upload_id,upload_registration,lifecycle_state,
    session_deadline,upload_expires_at,storage_driver,storage_namespace,storage_container,storage_locator_sha256)
VALUES (99006312,99006301,99006302,99006304,99006300,'enterprise/test/eb-scope-pin',
    repeat('a',64),'image/png',7,'pending_confirm','aba94340-99fa-4afb-8fe9-67e56c8c6177',
    jsonb_build_object('protocol','direct-v1','subject',
        jsonb_build_object('kind','seat','principal','99006300','org_id','99006301'),
        'declaration',jsonb_build_object('conversation_id',99006304)),
    'live',now()+interval '1 hour',now()+interval '20 minutes','garage','fixture-ns','fixture-bucket',repeat('b',64));
SET LOCAL session_replication_role=origin;
SELECT fn_eb_direct_reserve(99006301,99006302,99006312,
    jsonb_build_object('kind','seat','principal','99006300','org_id','99006301'));
SET LOCAL session_replication_role=replica;
UPDATE attach_pending SET direct_state='FINALIZING',
    direct_expected=jsonb_build_object('version',1,'sha256',repeat('a',64),'size_bytes',7,'mime','image/png')
 WHERE object_key='enterprise/test/eb-scope-pin';
INSERT INTO attach_direct_attempt(object_key,attempt_no,attempt_state,source_key,target_key,target_driver,
    target_namespace,target_container,profile_digest,dispatch_cas_uuid,dispatched_at,proof_sha256,proof_size,proof_received_at)
VALUES ('enterprise/test/eb-scope-pin',1,'PROOF_VERIFIED','direct-s/test/scope','direct-c/test/scope',
    'garage','fixture-ns','fixture-bucket',repeat('b',64),'aba94340-99fa-4afb-8fe9-67e56c8c6178',now(),repeat('a',64),7,now());
INSERT INTO attachment(id,path,url,scope,creator_user_id,mime_type,file_hash256,size,status,
    storage_driver,storage_namespace,storage_container,storage_locator_sha256,lifecycle_state)
VALUES (99006313,'enterprise/test/eb-scope-pin','fixture-bucket','private',0,'image/png',repeat('a',64),7,1,
    'garage','fixture-ns','fixture-bucket',repeat('b',64),'live');
COMMIT;
SQL
subject="jsonb_build_object('kind','seat','principal','99006300','org_id','99006301')"
probe="SELECT public.fn_eb_direct_scope_lock(99006301,99006302,99006304,$subject)"
cs_probe="SELECT public.fn_eb_direct_scope_lock(99006301,99006302,99006317,$subject)"
visitor="jsonb_build_object('kind','visitor_session','principal','99006303','org_id','99006301','session_id',99006308,'token_id',99006311,'installation_id',99006310)"
visitor_probe="SELECT public.fn_eb_direct_scope_lock(99006301,99006302,99006304,$visitor)"
[[ "$(psql "${args[@]}" -At -c "$probe")" = t ]]
[[ "$(psql "${args[@]}" -At -c "$visitor_probe")" = t ]]
race() {
    local tag="$1" writer="$2" query="${3:-$probe}" a="eb_${1}_reader" b="eb_${1}_writer" locked=0 blocked=0
    PGAPPNAME="$a" psql "${args[@]}" -c "BEGIN; $query; SELECT pg_sleep(8); COMMIT" > "$logs/$tag-reader.log" 2>&1 &
    local reader=$!
    for _ in {1..50}; do
        [[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM pg_stat_activity WHERE application_name='$a' AND wait_event='PgSleep'")" = 1 ]] && { locked=1; break; }
        sleep 0.1
    done
    [[ "$locked" = 1 ]] || { wait "$reader" || true; echo 'reader did not acquire locks' >&2; return 1; }
    PGAPPNAME="$b" psql "${args[@]}" -c "$writer" > "$logs/$tag-writer.log" 2>&1 &
    local writer_pid=$!
    for _ in {1..50}; do
        [[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM pg_stat_activity x JOIN pg_stat_activity y ON y.pid=ANY(pg_blocking_pids(x.pid)) WHERE x.application_name='$b' AND y.application_name='$a'")" = 1 ]] && { blocked=1; break; }
        sleep 0.1
    done
    wait "$reader"
    wait "$writer_pid"
    [[ "$blocked" = 1 ]] || { echo 'writer did not wait for scope lock' >&2; return 1; }
}
race suspend "UPDATE organization_member SET status='suspended' WHERE organization_id=99006301 AND user_id=99006300"
[[ "$(psql "${args[@]}" -At -c "$probe")" = f ]]
psql "${args[@]}" -c "UPDATE organization_member SET status='active' WHERE organization_id=99006301 AND user_id=99006300"
race identity "UPDATE organization_business_identity SET status='retired' WHERE id=99006305"
[[ "$(psql "${args[@]}" -At -c "$probe")" = f ]]
psql "${args[@]}" -c "UPDATE organization_business_identity SET status='active' WHERE id=99006305"
[[ "$(psql "${args[@]}" -At -c "$cs_probe")" = t ]]
race identity_cs "UPDATE organization_business_identity SET status='retired' WHERE id=99006315" "$cs_probe"
[[ "$(psql "${args[@]}" -At -c "$cs_probe")" = f ]]
psql "${args[@]}" -c "UPDATE organization_business_identity SET status='active' WHERE id=99006315"
PGAPPNAME=eb_token_deadline_blocker psql "${args[@]}" -c \
    "BEGIN; UPDATE customer_service_visit_token SET expires_at=now()+interval '2 seconds' WHERE id=99006311; SELECT pg_sleep(5); COMMIT" \
    > "$logs/token-deadline-blocker.log" 2>&1 &
blocker=$!
locked=0
for _ in {1..50}; do
    [[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM pg_stat_activity WHERE application_name='eb_token_deadline_blocker' AND wait_event='PgSleep'")" = 1 ]] && { locked=1; break; }
    sleep 0.1
done
[[ "$locked" = 1 ]] || { wait "$blocker" || true; echo 'deadline blocker did not acquire lock' >&2; exit 1; }
PGAPPNAME=eb_token_deadline_reader psql "${args[@]}" -At -c "$visitor_probe" > "$logs/token-deadline-reader.log" 2>&1 &
reader=$!
blocked=0
for _ in {1..50}; do
    [[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM pg_stat_activity x JOIN pg_stat_activity y ON y.pid=ANY(pg_blocking_pids(x.pid)) WHERE x.application_name='eb_token_deadline_reader' AND y.application_name='eb_token_deadline_blocker'")" = 1 ]] && { blocked=1; break; }
    sleep 0.1
done
wait "$blocker"
wait "$reader"
[[ "$blocked" = 1 && "$(cat "$logs/token-deadline-reader.log")" = f ]]
psql "${args[@]}" -c "UPDATE customer_service_visit_token SET expires_at=now()+interval '1 hour' WHERE id=99006311"
race token "UPDATE customer_service_visit_token SET revoked_at=now() WHERE id=99006311" "$visitor_probe"
[[ "$(psql "${args[@]}" -At -c "$visitor_probe")" = f ]]
psql "${args[@]}" -c "UPDATE customer_service_visit_token SET revoked_at=NULL WHERE id=99006311"
race installation "UPDATE customer_service_widget_installation SET status='revoked',revoked_at=now() WHERE id=99006310" "$visitor_probe"
[[ "$(psql "${args[@]}" -At -c "$visitor_probe")" = f ]]
psql "${args[@]}" -c "UPDATE customer_service_widget_installation SET status='active',revoked_at=NULL WHERE id=99006310"
race visitor_read "BEGIN; SELECT id FROM customer_service_widget_installation WHERE id=99006310 FOR UPDATE; SELECT id FROM customer_service_visit_token WHERE id=99006311 FOR UPDATE; SELECT id FROM customer_service_session WHERE id=99006308 FOR UPDATE; COMMIT" "$visitor_probe"
PGAPPNAME=eb_visitor_read_first psql "${args[@]}" -c "BEGIN; SELECT id FROM customer_service_widget_installation WHERE id=99006310 FOR UPDATE; SELECT id FROM customer_service_visit_token WHERE id=99006311 FOR UPDATE; SELECT pg_sleep(5); SELECT id FROM customer_service_session WHERE id=99006308 FOR UPDATE; COMMIT" > "$logs/read-first.log" 2>&1 &
read_first=$!
ready=0
for _ in {1..50}; do
    [[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM pg_stat_activity WHERE application_name='eb_visitor_read_first' AND wait_event='PgSleep'")" = 1 ]] && { ready=1; break; }
    sleep 0.1
done
[[ "$ready" = 1 ]] || { wait "$read_first" || true; echo 'visitor read did not acquire locks' >&2; exit 1; }
PGAPPNAME=eb_visitor_scope_second psql "${args[@]}" -At -c "$visitor_probe" > "$logs/scope-second.log" 2>&1 &
scope_second=$!
blocked=0
for _ in {1..50}; do
    [[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM pg_stat_activity x JOIN pg_stat_activity y ON y.pid=ANY(pg_blocking_pids(x.pid)) WHERE x.application_name='eb_visitor_scope_second' AND y.application_name='eb_visitor_read_first'")" = 1 ]] && { blocked=1; break; }
    sleep 0.1
done
wait "$read_first"
wait "$scope_second"
[[ "$blocked" = 1 && "$(cat "$logs/scope-second.log")" = t ]] || { echo 'read/scope lock ordering failed' >&2; exit 1; }
race session "INSERT INTO customer_service_session(id,organization_id,workspace_id,contact_id,conversation_id,status) VALUES(99006307,99006301,99006302,99006303,99006304,'queued')"
PGAPPNAME=eb_pin_reserve psql "${args[@]}" -c "BEGIN; SELECT id FROM workspace WHERE id=99006302 FOR UPDATE; SELECT pg_sleep(5); SELECT fn_eb_direct_reserve(99006301,99006302,99006312,$subject); COMMIT" > "$logs/pin-reserve.log" 2>&1 &
reserve=$!
locked=0
for _ in {1..50}; do
    [[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM pg_stat_activity WHERE application_name='eb_pin_reserve' AND wait_event='PgSleep'")" = 1 ]] && { locked=1; break; }
    sleep 0.1
done
[[ "$locked" = 1 ]] || { wait "$reserve" || true; echo 'reserve did not acquire workspace lock' >&2; exit 1; }
PGAPPNAME=eb_pin_writer psql "${args[@]}" -c "BEGIN; SET LOCAL ROLE v5_app_runtime; SELECT fn_dv1_winner_pin('enterprise/test/eb-scope-pin',1,'direct-c/test/scope',repeat('a',64)); COMMIT" > "$logs/pin-writer.log" 2>&1 &
pin=$!
blocked=0
for _ in {1..50}; do
    [[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM pg_stat_activity x JOIN pg_stat_activity y ON y.pid=ANY(pg_blocking_pids(x.pid)) WHERE x.application_name='eb_pin_writer' AND y.application_name='eb_pin_reserve'")" = 1 ]] && { blocked=1; break; }
    sleep 0.1
done
wait "$reserve"
wait "$pin"
[[ "$blocked" = 1 ]]
[[ "$(psql "${args[@]}" -At -c "SELECT count(*) FROM attach_pending WHERE object_key='enterprise/test/eb-scope-pin' AND direct_state='CONFIRMED' AND confirmed_attachment_id=99006313")" = 1 ]]
echo "scope lock races passed; isolated fixture retained; logs: $logs"
