\set ON_ERROR_STOP on
\set EB_DIRECT_KEEP_FIXTURE 1
\ir eb_direct_reservation_pg.sql
-- Pass immutable fixture values through transaction-local settings for the
-- role-separated statements. These are synthetic digests, not Worker evidence.
SELECT set_config('eb_test.fence',(SELECT fence::text FROM fn_eb_direct_scan_projection(99006005)),true);
SET LOCAL ROLE v5_app_runtime;
DO $$ BEGIN
    BEGIN
        PERFORM fn_eb_direct_record_scan(99006005,current_setting('eb_test.fence')::jsonb,repeat('a',64),repeat('b',64),extract(epoch FROM clock_timestamp())::bigint+300);
        RAISE EXCEPTION 'runtime could publish proof';
    EXCEPTION WHEN insufficient_privilege THEN NULL;
    END;
    BEGIN
        PERFORM fn_eb_direct_bind(99006001,99006002,99006005,
            jsonb_build_object('kind','seat','principal','99006000','org_id','99006001'),
            current_setting('eb_test.fence')::jsonb,repeat('a',64),repeat('b',64),extract(epoch FROM clock_timestamp())::bigint+300);
        RAISE EXCEPTION 'runtime bound without independent proof';
    EXCEPTION WHEN SQLSTATE '23510' THEN
        IF SQLERRM <> 'enterprise direct independent proof required' THEN RAISE; END IF;
    END;
END $$;
RESET ROLE;
SET LOCAL ROLE eb_direct_proof_worker;
DO $$ DECLARE f jsonb; BEGIN
    SELECT fence INTO f FROM fn_eb_direct_scan_projection(99006005);
    IF f IS DISTINCT FROM current_setting('eb_test.fence')::jsonb THEN RAISE EXCEPTION 'projection differs'; END IF;
    IF fn_eb_direct_record_scan(99006005,jsonb_set(f,'{attempt_no}','2'::jsonb),repeat('a',64),repeat('b',64),extract(epoch FROM clock_timestamp())::bigint+300) THEN
        RAISE EXCEPTION 'wrong winner was accepted';
    END IF;
    IF NOT fn_eb_direct_record_scan(99006005,f,repeat('a',64),repeat('b',64),extract(epoch FROM clock_timestamp())::bigint+300) THEN
        RAISE EXCEPTION 'trusted proof rejected';
    END IF;
END $$;
RESET ROLE;
SELECT set_config('eb_test.expiry',(SELECT extract(epoch FROM expires_at)::bigint::text FROM enterprise_direct_scan_proof WHERE asset_id=99006005),true);
SET LOCAL ROLE v5_app_runtime;
DO $$ BEGIN
    BEGIN
        PERFORM fn_eb_direct_bind(99006001,99006002,99006005,
            jsonb_build_object('kind','seat','principal','99006000','org_id','99006001'),
            current_setting('eb_test.fence')::jsonb,repeat('c',64),repeat('b',64),current_setting('eb_test.expiry')::bigint);
        RAISE EXCEPTION 'forged receipt accepted';
    EXCEPTION WHEN SQLSTATE '23510' THEN
        IF SQLERRM <> 'enterprise direct independent proof required' THEN RAISE; END IF;
    END;
    PERFORM fn_eb_direct_bind(99006001,99006002,99006005,
        jsonb_build_object('kind','seat','principal','99006000','org_id','99006001'),
        current_setting('eb_test.fence')::jsonb,repeat('a',64),repeat('b',64),current_setting('eb_test.expiry')::bigint);
END $$;
RESET ROLE;
ROLLBACK;
