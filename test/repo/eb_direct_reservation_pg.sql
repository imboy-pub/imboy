\set ON_ERROR_STOP on
BEGIN;
-- Synthetic fixture rows are rolled back; production triggers stay enabled for
-- the reservation itself.
SET LOCAL session_replication_role = replica;
INSERT INTO public."user"(id,password,account,reg_ip,reg_cosv)
VALUES (99006000,'x','eb-direct-reservation-fixture','127.0.0.1','x');
INSERT INTO public.organization(id,name,owner_id,status)
VALUES (99006001,'eb-direct-reservation-fixture',99006099,'active');
INSERT INTO public.workspace(id,name,owner_id,status,type,organization_id)
VALUES (99006002,'eb-direct-reservation-fixture',99006000,'active','project',99006001);
INSERT INTO public.organization_member(organization_id,user_id,status)
VALUES (99006001,99006000,'active');
INSERT INTO public.organization_business_identity(id,organization_id,function_key,display_name,status)
VALUES (99006011,99006001,'sales','fixture','active');
INSERT INTO public.organization_business_identity_assignment
    (id,organization_id,business_identity_id,function_key,user_id,status)
VALUES (99006012,99006001,99006011,'sales',99006000,'active');
INSERT INTO public.organization_business_identity(id,organization_id,function_key,display_name,status)
VALUES (99006021,99006001,'customer_service','intake','active'),
       (99006022,99006001,'customer_service','assignee','active');
INSERT INTO public.organization_business_identity_assignment
    (id,organization_id,business_identity_id,function_key,user_id,status)
VALUES (99006023,99006001,99006022,'customer_service',99006000,'active');
INSERT INTO public.customer_service_seat(organization_id,business_identity_id,enabled)
VALUES (99006001,99006022,true);
INSERT INTO public.enterprise_contact(id,organization_id,status,display_name)
VALUES (99006003,99006001,'active','fixture');
INSERT INTO public.enterprise_conversation(id,organization_id,workspace_id,contact_id,business_identity_id,status)
VALUES (99006004,99006001,99006002,99006003,99006011,'active');
INSERT INTO public.enterprise_conversation(id,organization_id,workspace_id,contact_id,business_identity_id,status)
VALUES (99006019,99006001,99006002,99006003,99006021,'active');
INSERT INTO public.customer_service_session(id,organization_id,workspace_id,contact_id,conversation_id,business_identity_id,status)
VALUES (99006020,99006001,99006002,99006003,99006019,99006022,'active');
INSERT INTO public.customer_service_session(id,organization_id,workspace_id,contact_id,conversation_id,status)
VALUES (99006007,99006001,99006002,99006003,99006004,'queued');
INSERT INTO public.customer_service_widget_installation
    (id,organization_id,public_widget_id,display_name,consent_version,status)
VALUES (99006016,99006001,'eb-direct-test-widget','fixture','1','active');
INSERT INTO public.customer_service_visit_token
    (id,organization_id,contact_id,token_digest,expires_at,widget_installation_id)
VALUES (99006015,99006001,99006003,repeat('d',64),now()+interval '1 hour',99006016);
UPDATE public.customer_service_session SET visit_token_id=99006015 WHERE id=99006007;
INSERT INTO public.customer_service_widget_installation
    (id,organization_id,public_widget_id,display_name,consent_version,status)
VALUES (99006017,99006001,'eb-direct-other-widget','fixture','1','active');
INSERT INTO public.customer_service_visit_token
    (id,organization_id,contact_id,token_digest,expires_at,widget_installation_id)
VALUES (99006018,99006001,99006003,repeat('e',64),now()+interval '1 hour',99006017);
INSERT INTO public.enterprise_asset
    (id,organization_id,workspace_id,conversation_id,uploaded_by_user_id,object_key,
     object_hash,mime,size_bytes,status,client_upload_id,upload_registration,lifecycle_state,
     session_deadline,upload_expires_at,storage_driver,storage_namespace,storage_container,
     storage_locator_sha256)
VALUES
    (99006005,99006001,99006002,99006004,99006000,'enterprise/test/eb-direct-seat',
     repeat('a',64),'image/png',7,'pending_confirm','abb94340-99fa-4afb-8fe9-67e56c8c6123',
     jsonb_build_object('protocol','direct-v1','subject',jsonb_build_object('kind','seat','principal','99006000','org_id','99006001'),
                        'declaration',jsonb_build_object('conversation_id',99006004)),
     'live',now()+interval '1 hour',now()+interval '20 minutes','garage','fixture-ns','fixture-bucket',repeat('b',64)),
    (99006006,99006001,99006002,99006004,NULL,'enterprise/test/eb-direct-visitor',
     repeat('c',64),'image/png',7,'pending_confirm','abb94340-99fa-4afb-8fe9-67e56c8c6124',
     jsonb_build_object('protocol','direct-v1','subject',jsonb_build_object('kind','visitor_session','principal','99006003',
                         'org_id','99006001','session_id',99006007,
                         'token_id',99006015,'installation_id',99006016),
                        'declaration',jsonb_build_object('conversation_id',99006004)),
     'live',now()+interval '1 hour',now()+interval '20 minutes','garage','fixture-ns','fixture-bucket',repeat('b',64));
INSERT INTO public.enterprise_asset
    (id,organization_id,workspace_id,conversation_id,uploaded_by_user_id,object_key,
     object_hash,mime,size_bytes,status,client_upload_id,upload_registration,lifecycle_state,
     session_deadline,upload_expires_at,storage_driver,storage_namespace,storage_container,
     storage_locator_sha256)
SELECT 99006009,organization_id,workspace_id,conversation_id,uploaded_by_user_id,
       'enterprise/test/eb-direct-legacy',object_hash,mime,size_bytes,status,
       'abb94340-99fa-4afb-8fe9-67e56c8c6125',upload_registration - 'protocol',lifecycle_state,
       session_deadline,upload_expires_at,storage_driver,storage_namespace,storage_container,
       storage_locator_sha256 FROM public.enterprise_asset WHERE id=99006005;
INSERT INTO public.enterprise_asset
    (id,organization_id,workspace_id,conversation_id,uploaded_by_user_id,object_key,
     object_hash,mime,size_bytes,status,client_upload_id,upload_registration,lifecycle_state,
     session_deadline,upload_expires_at,storage_driver,storage_namespace,storage_container,
     storage_locator_sha256)
SELECT 99006014,organization_id,workspace_id,conversation_id,uploaded_by_user_id,
       'enterprise/test/eb-direct-wrong-session',object_hash,mime,size_bytes,status,
       'abb94340-99fa-4afb-8fe9-67e56c8c6127',
       jsonb_set(upload_registration,'{subject,session_id}','99006008'::jsonb),lifecycle_state,
       session_deadline,upload_expires_at,storage_driver,storage_namespace,storage_container,
       storage_locator_sha256 FROM public.enterprise_asset WHERE id=99006006;
SET LOCAL session_replication_role = origin;
SET LOCAL ROLE v5_app_runtime;
DO $$ BEGIN
    BEGIN
        PERFORM public.fn_eb_direct_reserve(99006001,99006002,99006005,
            jsonb_build_object('kind','seat','principal','99006000','org_id','99006001'));
        RAISE EXCEPTION 'unapproved runtime reservation accepted';
    EXCEPTION WHEN insufficient_privilege THEN NULL;
    END;
    BEGIN
        INSERT INTO public.enterprise_asset_direct_link(asset_id,object_key,organization_id,
            workspace_id,conversation_id,subject)
        VALUES (99006005,'forged',99006001,99006002,99006004,'{}'::jsonb);
        RAISE EXCEPTION 'direct insert accepted';
    EXCEPTION WHEN insufficient_privilege THEN NULL;
    END;
END $$;
RESET ROLE;
DO $$
DECLARE
    seat jsonb := jsonb_build_object('kind','seat','principal','99006000','org_id','99006001');
    visitor jsonb := jsonb_build_object('kind','visitor_session','principal','99006003',
        'org_id','99006001','session_id',99006007,'token_id',99006015,'installation_id',99006016);
    k text;
BEGIN
    IF NOT public.fn_eb_direct_scope_lock(99006001,99006002,99006004,seat)
       OR NOT public.fn_eb_direct_scope_lock(99006001,99006002,99006019,seat) THEN
        RAISE EXCEPTION 'active identity rejected';
    END IF;
    UPDATE public.organization_business_identity SET status='retired' WHERE id=99006011;
    IF public.fn_eb_direct_scope_lock(99006001,99006002,99006004,seat) THEN
        RAISE EXCEPTION 'retired sales identity accepted';
    END IF;
    UPDATE public.organization_business_identity SET status='active' WHERE id=99006011;
    UPDATE public.organization_business_identity SET status='retired' WHERE id=99006022;
    IF public.fn_eb_direct_scope_lock(99006001,99006002,99006019,seat) THEN
        RAISE EXCEPTION 'retired current customer service identity accepted';
    END IF;
    UPDATE public.organization_business_identity SET status='active' WHERE id=99006022;
    UPDATE public.organization_business_identity SET status='retired' WHERE id=99006021;
    IF public.fn_eb_direct_scope_lock(99006001,99006002,99006019,seat) THEN
        RAISE EXCEPTION 'retired intake identity accepted';
    END IF;
    UPDATE public.organization_business_identity SET status='active' WHERE id=99006021;
    IF public.fn_eb_direct_scope_lock(99006001,99006002,99006004,
        jsonb_set(jsonb_set(visitor,'{token_id}','99006018'::jsonb),'{installation_id}','99006017'::jsonb)) THEN
        RAISE EXCEPTION 'other installation accepted for original session';
    END IF;
    k := public.fn_eb_direct_reserve(99006001,99006002,99006005,seat);
    IF k <> 'enterprise/test/eb-direct-seat'
       OR public.fn_eb_direct_reserve(99006001,99006002,99006005,seat) <> k THEN
        RAISE EXCEPTION 'seat reservation not idempotent';
    END IF;
    IF public.fn_eb_direct_reserve(99006001,99006002,99006006,visitor)
       <> 'enterprise/test/eb-direct-visitor' THEN
        RAISE EXCEPTION 'visitor reservation failed';
    END IF;
    BEGIN
        PERFORM public.fn_eb_direct_reserve(99006001,99006002,99006006,
            jsonb_set(visitor,'{session_id}','99006008'::jsonb));
        RAISE EXCEPTION 'different visitor session accepted';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    BEGIN
        PERFORM public.fn_eb_direct_reserve(99006001,99006002,99006014,
            jsonb_set(visitor,'{session_id}','99006008'::jsonb));
        RAISE EXCEPTION 'nonexistent visitor session accepted';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    BEGIN
        PERFORM public.fn_eb_direct_reserve(99006001,99006002,99006005,
            jsonb_set(seat,'{principal}','"99006099"'::jsonb));
        RAISE EXCEPTION 'different seat accepted';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    BEGIN
        PERFORM public.fn_eb_direct_reserve(99006099,99006002,99006005,seat);
        RAISE EXCEPTION 'different organization accepted';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    BEGIN
        PERFORM public.fn_eb_direct_reserve(99006001,99006002,99006009,seat);
        RAISE EXCEPTION 'legacy session accepted as direct';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    UPDATE public.organization_member SET status='suspended'
     WHERE organization_id=99006001 AND user_id=99006000;
    BEGIN
        PERFORM public.fn_eb_direct_reserve(99006001,99006002,99006005,seat);
        RAISE EXCEPTION 'suspended seat accepted';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    UPDATE public.organization_member SET status='active'
     WHERE organization_id=99006001 AND user_id=99006000;
    UPDATE public.customer_service_visit_token SET revoked_at=now() WHERE id=99006015;
    BEGIN
        PERFORM public.fn_eb_direct_reserve(99006001,99006002,99006006,visitor);
        RAISE EXCEPTION 'revoked visitor token accepted';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    UPDATE public.customer_service_visit_token SET revoked_at=NULL WHERE id=99006015;
    UPDATE public.customer_service_widget_installation SET status='revoked',revoked_at=now()
     WHERE id=99006016;
    BEGIN
        PERFORM public.fn_eb_direct_reserve(99006001,99006002,99006006,visitor);
        RAISE EXCEPTION 'revoked installation accepted';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    UPDATE public.customer_service_widget_installation SET status='active',revoked_at=NULL
     WHERE id=99006016;
END $$;
-- Prepare a synthetic proved attempt, then execute the actual pin as the
-- restricted application role with all production triggers enabled.
SET LOCAL session_replication_role = replica;
UPDATE public.attach_pending SET direct_state='FINALIZING',
    direct_expected=jsonb_build_object('version',1,'sha256',repeat('a',64),'size_bytes',7,'mime','image/png')
 WHERE object_key='enterprise/test/eb-direct-seat';
INSERT INTO public.attach_direct_attempt
    (object_key,attempt_no,attempt_state,source_key,target_key,target_driver,target_namespace,
     target_container,profile_digest,dispatch_cas_uuid,dispatched_at,proof_sha256,proof_size,proof_received_at)
VALUES ('enterprise/test/eb-direct-seat',1,'PROOF_VERIFIED','direct-s/test/seat','direct-c/test/seat',
    'garage','fixture-ns','fixture-bucket',repeat('b',64),
    'abb94340-99fa-4afb-8fe9-67e56c8c6126',now(),repeat('a',64),7,now());
INSERT INTO public.attachment
    (id,path,url,scope,creator_user_id,mime_type,file_hash256,size,status,
     storage_driver,storage_namespace,storage_container,storage_locator_sha256,lifecycle_state)
VALUES (99006010,'enterprise/test/eb-direct-seat','fixture-bucket',
    'private',0,'image/png',repeat('a',64),7,1,'garage','fixture-ns','fixture-bucket',repeat('b',64),'live');
SET LOCAL session_replication_role = origin;
UPDATE public.organization_member SET status='suspended'
 WHERE organization_id=99006001 AND user_id=99006000;
SET LOCAL ROLE v5_app_runtime;
DO $$ BEGIN
    BEGIN
        PERFORM public.fn_dv1_winner_pin('enterprise/test/eb-direct-seat',1,
                                        'direct-c/test/seat',repeat('a',64));
        RAISE EXCEPTION 'suspended seat pin accepted';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
END $$;
RESET ROLE;
UPDATE public.organization_member SET status='active'
 WHERE organization_id=99006001 AND user_id=99006000;
SET LOCAL ROLE v5_app_runtime;
SELECT public.fn_dv1_winner_pin('enterprise/test/eb-direct-seat',1,'direct-c/test/seat',repeat('a',64));
RESET ROLE;
SET LOCAL session_replication_role = replica;
UPDATE public.attach_pending SET direct_state='FINALIZING',
    direct_expected=jsonb_build_object('version',1,'sha256',repeat('c',64),'size_bytes',7,'mime','image/png')
 WHERE object_key='enterprise/test/eb-direct-visitor';
INSERT INTO public.attach_direct_attempt
    (object_key,attempt_no,attempt_state,source_key,target_key,target_driver,target_namespace,
     target_container,profile_digest,dispatch_cas_uuid,dispatched_at,proof_sha256,proof_size,proof_received_at)
VALUES ('enterprise/test/eb-direct-visitor',1,'PROOF_VERIFIED','direct-s/test/visitor','direct-c/test/visitor',
    'garage','fixture-ns','fixture-bucket',repeat('b',64),
    'abb94340-99fa-4afb-8fe9-67e56c8c6128',now(),repeat('c',64),7,now());
INSERT INTO public.attachment
    (id,path,url,scope,creator_user_id,mime_type,file_hash256,size,status,
     storage_driver,storage_namespace,storage_container,storage_locator_sha256,lifecycle_state)
VALUES (99006020,'enterprise/test/eb-direct-visitor','fixture-bucket',
    'private',0,'image/png',repeat('c',64),7,1,'garage','fixture-ns','fixture-bucket',repeat('b',64),'live');
SET LOCAL session_replication_role = origin;
UPDATE public.customer_service_visit_token SET revoked_at=now() WHERE id=99006015;
SET LOCAL ROLE v5_app_runtime;
DO $$ BEGIN
    BEGIN
        PERFORM public.fn_dv1_winner_pin('enterprise/test/eb-direct-visitor',1,
                                        'direct-c/test/visitor',repeat('c',64));
        RAISE EXCEPTION 'revoked visitor pin accepted';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
END $$;
RESET ROLE;
UPDATE public.customer_service_visit_token SET revoked_at=NULL WHERE id=99006015;
SET LOCAL ROLE v5_app_runtime;
SELECT public.fn_dv1_winner_pin('enterprise/test/eb-direct-visitor',1,'direct-c/test/visitor',repeat('c',64));
RESET ROLE;
DO $$ BEGIN
    IF (SELECT count(*) FROM public.enterprise_asset_direct_link WHERE asset_id IN (99006005,99006006)) <> 2
       OR NOT EXISTS (SELECT 1 FROM public.attach_pending WHERE object_key='enterprise/test/eb-direct-seat'
                      AND direct_state='CONFIRMED' AND confirmed_attachment_id=99006010)
       OR NOT EXISTS (SELECT 1 FROM public.attach_pending WHERE object_key='enterprise/test/eb-direct-visitor'
                      AND direct_state='CONFIRMED' AND confirmed_attachment_id=99006020) THEN
        RAISE EXCEPTION 'reservation or pin failed';
    END IF;
END $$;
\if :{?EB_DIRECT_KEEP_FIXTURE}
\else
ROLLBACK;
\endif
