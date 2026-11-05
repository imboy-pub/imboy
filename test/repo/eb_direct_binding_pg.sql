\set ON_ERROR_STOP on
\set EB_DIRECT_KEEP_FIXTURE 1
\ir eb_direct_reservation_pg.sql

SET LOCAL ROLE v5_app_runtime;
DO $$ BEGIN
    BEGIN
        INSERT INTO public.enterprise_asset(id,organization_id,workspace_id,conversation_id,
            object_key,object_hash,status,upload_registration)
        VALUES(99006031,99006001,99006002,99006004,'enterprise/test/forged-active',repeat('a',64),
            'active',jsonb_build_object('protocol','direct-v1','subject','{}'::jsonb,'declaration','{}'::jsonb));
        RAISE EXCEPTION 'direct active insert accepted';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
END $$;
RESET ROLE;
SET LOCAL session_replication_role=replica;
INSERT INTO public.enterprise_asset(id,organization_id,workspace_id,conversation_id,
    object_key,object_hash,status,lifecycle_state)
VALUES(99006030,99006001,99006002,99006004,'enterprise/test/legacy-active',repeat('a',64),'active','live');
SET LOCAL session_replication_role=origin;
SET LOCAL ROLE v5_app_runtime;
DO $$ BEGIN
    BEGIN
        PERFORM public.fn_v5_register_eb(99006001,99006002,99006030,'enterprise/test/legacy-active',
            'aba94340-99fa-4afb-8fe9-67e56c8c6190',
            jsonb_build_object('protocol','direct-v1','subject',jsonb_build_object('kind','seat'),
                               'declaration','{}'::jsonb),'seat',now()+interval '1 hour',now()+interval '20 minutes');
        RAISE EXCEPTION 'active legacy row converted to direct';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
END $$;
RESET ROLE;

-- Synthetic receipt digests exercise DB fencing only; HMAC verification is
-- tested independently by the external Worker/application tests.
DO $$
DECLARE
    seat jsonb := jsonb_build_object('kind','seat','principal','99006000','org_id','99006001');
    fence jsonb;
    expiry bigint := extract(epoch FROM now())::bigint + 600;
    result bigint;
BEGIN
    SELECT jsonb_build_object('object_key',p.object_key,'attachment_id',a.id,
        'creator_user_id',p.creator_user_id,'scope',p.scope,'upload_registration',p.upload_registration,
        'attempt_no',w.attempt_no,'dispatch_cas_uuid',w.dispatch_cas_uuid,
        'storage_driver',a.storage_driver,'storage_namespace',a.storage_namespace,
        'storage_container',a.storage_container,'storage_locator_sha256',a.storage_locator_sha256,
        'storage_physical_key',a.storage_physical_key,'file_hash256',a.file_hash256,
        'size',a.size,'mime_type',a.mime_type) INTO fence
      FROM attach_pending p JOIN attachment a ON a.id=p.confirmed_attachment_id
      JOIN attach_direct_attempt w ON w.object_key=p.object_key AND w.attempt_state='WINNER'
     WHERE p.object_key='enterprise/test/eb-direct-seat';
    IF fence IS NULL THEN RAISE EXCEPTION 'missing synthetic winner'; END IF;
    BEGIN
        UPDATE enterprise_asset_direct_link SET binding_state='bound' WHERE asset_id=99006005;
        RAISE EXCEPTION 'bound row without evidence accepted';
    EXCEPTION WHEN check_violation THEN NULL;
    END;
    BEGIN
        PERFORM public.fn_eb_direct_bind(99006001,99006002,99006005,seat,
            jsonb_set(fence,'{attempt_no}','2'::jsonb),repeat('a',64),repeat('b',64),expiry);
        RAISE EXCEPTION 'wrong winner attempt accepted';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    BEGIN
        PERFORM public.fn_eb_direct_bind(99006001,99006002,99006005,seat,fence,
            repeat('a',64),repeat('b',64),extract(epoch FROM now())::bigint-1);
        RAISE EXCEPTION 'expired receipt accepted';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    -- Model a corrupt pre-existing asset without disabling guards for the bind.
    PERFORM set_config('session_replication_role','replica',true);
    UPDATE enterprise_asset SET storage_namespace='wrong-namespace' WHERE id=99006005;
    PERFORM set_config('session_replication_role','origin',true);
    BEGIN
        PERFORM public.fn_eb_direct_bind(99006001,99006002,99006005,seat,fence,
            repeat('a',64),repeat('b',64),expiry);
        RAISE EXCEPTION 'mismatched asset storage accepted';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    PERFORM set_config('session_replication_role','replica',true);
    UPDATE enterprise_asset SET storage_namespace=fence->>'storage_namespace' WHERE id=99006005;
    PERFORM set_config('session_replication_role','origin',true);
    result := public.fn_eb_direct_bind(99006001,99006002,99006005,seat,fence,
        repeat('a',64),repeat('b',64),expiry);
    IF result <> 99006005 OR public.fn_eb_direct_bind(99006001,99006002,99006005,seat,fence,
        repeat('c',64),repeat('b',64),expiry) <> result THEN
        RAISE EXCEPTION 'direct binding or replay failed';
    END IF;
    IF NOT EXISTS (SELECT 1 FROM enterprise_asset e JOIN enterprise_asset_direct_link l ON l.asset_id=e.id
                   WHERE e.id=99006005 AND e.status='active' AND l.binding_state='bound'
                     AND e.upload_content->>'sha256'=repeat('a',64)) THEN
        RAISE EXCEPTION 'binding was not atomic';
    END IF;
    BEGIN
        UPDATE enterprise_asset SET status='deleted', lifecycle_state='deleted' WHERE id=99006005;
        RAISE EXCEPTION 'direct reference was retired without a retirement transaction';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    BEGIN
        UPDATE attachment SET lifecycle_state='delete_intent' WHERE id=99006010;
        RAISE EXCEPTION 'ordinary GC accepted live enterprise reference';
    EXCEPTION WHEN SQLSTATE '23510' THEN
        IF SQLERRM <> 'enterprise direct reference still live' THEN RAISE; END IF;
    END;
    BEGIN
        INSERT INTO enterprise_asset_delete_queue(asset_id) VALUES (99006005);
        RAISE EXCEPTION 'legacy enterprise delete queue accepted direct winner';
    EXCEPTION WHEN SQLSTATE '23510' THEN
        IF SQLERRM <> 'enterprise direct deletion requires ordinary lifecycle' THEN RAISE; END IF;
    END;
END $$;
SET LOCAL ROLE v5_app_runtime;
DO $$ BEGIN
    BEGIN
        PERFORM public.fn_eb_direct_bind(99006001,99006002,99006005,'{}'::jsonb,'{}'::jsonb,
            repeat('a',64),repeat('b',64),extract(epoch FROM now())::bigint+60);
        RAISE EXCEPTION 'runtime direct binding accepted';
    EXCEPTION WHEN insufficient_privilege THEN NULL;
    END;
    BEGIN
        UPDATE enterprise_asset SET status='active' WHERE id=99006006;
        RAISE EXCEPTION 'unverified runtime activation accepted';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    BEGIN
        UPDATE enterprise_asset SET status='deleted' WHERE id=99006005;
        RAISE EXCEPTION 'runtime retired a direct asset without authorization';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
END $$;
RESET ROLE;
ROLLBACK;
