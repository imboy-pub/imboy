\set ON_ERROR_STOP on
\set EB_DIRECT_KEEP_FIXTURE 1
\ir eb_direct_reservation_pg.sql
SET LOCAL session_replication_role=replica;
INSERT INTO enterprise_asset(id,organization_id,workspace_id,conversation_id,object_key,object_hash,status)
SELECT 99007000+n,99006001,99006002,99006004,'enterprise/test/fair-'||n,repeat('a',64),'pending_confirm'
FROM generate_series(1,25) n;
INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,session_state,direct_state,confirmed_attachment_id)
SELECT 'enterprise/test/fair-'||n,'fixture-bucket','private',0,'confirmed','CONFIRMED',99006010
FROM generate_series(1,25) n;
INSERT INTO enterprise_asset_direct_link(asset_id,object_key,organization_id,workspace_id,conversation_id,subject)
SELECT 99007000+n,'enterprise/test/fair-'||n,99006001,99006002,99006004,'{}'::jsonb
FROM generate_series(1,25) n;
SET LOCAL session_replication_role=origin;
SET LOCAL ROLE eb_direct_proof_worker;
DO $$ DECLARE r record; n integer := 0; BEGIN
    FOR r IN SELECT asset_id FROM fn_eb_direct_scan_candidates(20) LOOP
        PERFORM fn_eb_direct_scan_failed(r.asset_id);
        n := n+1;
    END LOOP;
    IF n <> 20 THEN RAISE EXCEPTION 'expected 20 initial jobs'; END IF;
    IF NOT EXISTS (SELECT 1 FROM fn_eb_direct_scan_candidates(20) WHERE asset_id=99007025) THEN
        RAISE EXCEPTION 'later job starved behind failures';
    END IF;
END $$;
RESET ROLE;
DO $$ BEGIN
    IF (SELECT count(*) FROM enterprise_direct_scan_retry WHERE retry_after>clock_timestamp()) <> 20 THEN
        RAISE EXCEPTION 'failure backoff was not persisted';
    END IF;
END $$;
ROLLBACK;
