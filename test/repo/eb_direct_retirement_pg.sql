-- Run only against an isolated clone containing a synthetic confirmed/bound asset.
BEGIN;
SET LOCAL imboy.enterprise_purge = 'on';
DO $$
DECLARE v_link enterprise_asset_direct_link%ROWTYPE; v_attachment attachment%ROWTYPE;
BEGIN
    SELECT * INTO STRICT v_link FROM enterprise_asset_direct_link WHERE binding_state='bound' LIMIT 1;
    SELECT * INTO STRICT v_attachment FROM attachment WHERE path=v_link.object_key;
    IF has_function_privilege('v5_app_runtime','fn_eb_direct_retire(bigint,bigint,bigint)','EXECUTE')
       OR has_table_privilege('v5_app_runtime','enterprise_direct_retire_guard','INSERT') THEN
        RAISE EXCEPTION 'runtime may forge retirement';
    END IF;
    IF NOT has_function_privilege('imboy_enterprise_purge_worker','fn_eb_direct_retire(bigint,bigint,bigint)','EXECUTE') THEN
        RAISE EXCEPTION 'purge role cannot retire';
    END IF;
    UPDATE attach_pending SET upload_expires_at=clock_timestamp()-interval '1 hour' WHERE object_key=v_link.object_key;
    UPDATE attachment SET gc_verified=false WHERE path=v_link.object_key;
    BEGIN
        PERFORM fn_eb_direct_retire(v_link.organization_id,v_link.workspace_id,v_link.asset_id);
        RAISE EXCEPTION 'unproven attachment unexpectedly retired';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    UPDATE attachment SET gc_verified=true WHERE path=v_link.object_key;
    BEGIN
        UPDATE enterprise_asset SET retain_until=clock_timestamp()+interval '1 hour' WHERE id=v_link.asset_id;
        BEGIN
            PERFORM fn_eb_direct_retire(v_link.organization_id,v_link.workspace_id,v_link.asset_id);
            RAISE EXCEPTION 'active retention unexpectedly retired';
        EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
        END;
        RAISE EXCEPTION 'rollback retention fixture' USING ERRCODE='P1111';
    EXCEPTION WHEN SQLSTATE 'P1111' THEN NULL;
    END;
    BEGIN
        INSERT INTO enterprise_retention_hold(id,organization_id,workspace_id,scope_type,reason_code)
        SELECT coalesce(max(id),0)+1,v_link.organization_id,v_link.workspace_id,'workspace','test'
          FROM enterprise_retention_hold;
        BEGIN
            PERFORM fn_eb_direct_retire(v_link.organization_id,v_link.workspace_id,v_link.asset_id);
            RAISE EXCEPTION 'active hold unexpectedly retired';
        EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
        END;
        RAISE EXCEPTION 'rollback hold fixture' USING ERRCODE='P1112';
    EXCEPTION WHEN SQLSTATE 'P1112' THEN NULL;
    END;
    BEGIN
        PERFORM set_config('session_replication_role','replica',true);
        UPDATE attachment SET storage_physical_key='direct-c/test/tampered' WHERE path=v_link.object_key;
        PERFORM set_config('session_replication_role','origin',true);
        BEGIN
            PERFORM fn_eb_direct_retire(v_link.organization_id,v_link.workspace_id,v_link.asset_id);
            RAISE EXCEPTION 'mismatched winner unexpectedly retired';
        EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
        END;
        RAISE EXCEPTION 'rollback corruption fixture' USING ERRCODE='P1113';
    EXCEPTION WHEN SQLSTATE 'P1113' THEN NULL;
    END;
    PERFORM fn_eb_direct_retire(v_link.organization_id,v_link.workspace_id,v_link.asset_id);
    IF EXISTS (SELECT 1 FROM enterprise_asset_direct_link WHERE asset_id=v_link.asset_id)
       OR NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=v_link.asset_id AND status='deleted' AND lifecycle_state='deleted')
       OR NOT EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path=v_link.object_key
                      AND storage_physical_key=v_attachment.storage_physical_key AND job_state='ready') THEN
        RAISE EXCEPTION 'retirement did not atomically preserve physical queue identity';
    END IF;
    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue WHERE asset_id=v_link.asset_id)
       OR EXISTS (SELECT 1 FROM enterprise_direct_retire_guard) THEN
        RAISE EXCEPTION 'unexpected duplicate queue or retirement marker';
    END IF;
    DELETE FROM enterprise_asset WHERE id=v_link.asset_id;
    IF NOT FOUND THEN RAISE EXCEPTION 'guarded metadata purge failed'; END IF;
END $$;
ROLLBACK;
