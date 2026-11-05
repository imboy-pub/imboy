\set ON_ERROR_STOP on
\set EB_DIRECT_KEEP_FIXTURE 1
\ir eb_direct_reservation_pg.sql
-- Synthetic confirmed rows; their frozen targets do not exist in the local
-- Garage bucket. The legitimate upload is created separately by the app gate.
SET LOCAL session_replication_role=replica;
INSERT INTO public."user"(id,password,account,reg_ip,reg_cosv)
VALUES(99006099,'x','proof-fair-owner','127.0.0.1','x');
INSERT INTO organization_member(organization_id,user_id,role,status)
VALUES(99006001,99006099,'owner','active');
INSERT INTO enterprise_asset(id,organization_id,workspace_id,conversation_id,object_key,object_hash,status)
SELECT 99006030+n,99006001,99006002,99006004,'enterprise/test/proof-fail-'||n,repeat('a',64),'pending_confirm'
FROM generate_series(1,20) n;
INSERT INTO attach_pending
SELECT (jsonb_populate_record(NULL::attach_pending,to_jsonb(p)||jsonb_build_object(
 'object_key','enterprise/test/proof-fail-'||n,'client_upload_id',NULL,
 'confirmed_attachment_id',99008000+n,'storage_namespace','eb-real-garage',
 'storage_container','imboy-closeout-v3','storage_locator_sha256',:'locator_digest'))).*
FROM attach_pending p CROSS JOIN generate_series(1,20) n WHERE p.object_key='enterprise/test/eb-direct-seat';
INSERT INTO attachment
SELECT (jsonb_populate_record(NULL::attachment,to_jsonb(a)||jsonb_build_object(
 'id',99008000+n,'path','enterprise/test/proof-fail-'||n,
 'storage_namespace','eb-real-garage','storage_container','imboy-closeout-v3',
 'storage_locator_sha256',:'locator_digest','storage_physical_key','direct-c/test/proof-missing-'||n))).*
FROM attachment a CROSS JOIN generate_series(1,20) n WHERE a.id=99006010;
INSERT INTO attach_direct_attempt(object_key,attempt_no,attempt_state,source_key,target_key,target_driver,
 target_namespace,target_container,profile_digest,dispatch_cas_uuid,dispatched_at,proof_sha256,proof_size,proof_received_at)
SELECT 'enterprise/test/proof-fail-'||n,1,'WINNER','direct-s/test/proof-missing-'||n,
 'direct-c/test/proof-missing-'||n,'garage','eb-real-garage','imboy-closeout-v3',:'locator_digest',
 'abb94340-99fa-4afb-8fe9-67e56c8c6128'::uuid,now(),repeat('a',64),7,now()
FROM generate_series(1,20) n;
INSERT INTO enterprise_asset_direct_link(asset_id,object_key,organization_id,workspace_id,conversation_id,subject)
SELECT 99006030+n,'enterprise/test/proof-fail-'||n,99006001,99006002,99006004,
 jsonb_build_object('kind','seat','principal','99006000','org_id','99006001') FROM generate_series(1,20) n;
SET LOCAL session_replication_role=origin;
COMMIT;
