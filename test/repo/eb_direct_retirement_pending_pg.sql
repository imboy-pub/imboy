-- Isolated clone only. Replica mode is used solely to seed a synthetic row;
-- reservation and retirement run with the production triggers enabled.
BEGIN;
SET LOCAL imboy.enterprise_purge = 'on';
SET LOCAL session_replication_role = replica;
INSERT INTO public.enterprise_asset
    (id,organization_id,workspace_id,conversation_id,uploaded_by_user_id,object_key,
     object_hash,mime,size_bytes,status,client_upload_id,upload_registration,lifecycle_state,
     session_deadline,upload_expires_at,storage_driver,storage_namespace,storage_container,
     storage_locator_sha256)
SELECT 99006225,organization_id,workspace_id,conversation_id,uploaded_by_user_id,
       'enterprise/test/eb-retire-reserved',object_hash,mime,size_bytes,'pending_confirm',
       'abb94340-99fa-4afb-8fe9-67e56c8c6199',upload_registration,'live',
       now()+interval '3 seconds',now()+interval '2 seconds',storage_driver,storage_namespace,
       storage_container,storage_locator_sha256
  FROM enterprise_asset WHERE id=(SELECT asset_id FROM enterprise_asset_direct_link WHERE binding_state='bound' LIMIT 1);
SET LOCAL session_replication_role = origin;
DO $$
DECLARE v_asset enterprise_asset%ROWTYPE;
BEGIN
    SELECT * INTO STRICT v_asset FROM enterprise_asset WHERE id=99006225;
    PERFORM fn_eb_direct_reserve(v_asset.organization_id,v_asset.workspace_id,v_asset.id,
                                 v_asset.upload_registration->'subject');
    BEGIN
        PERFORM fn_eb_direct_retire(v_asset.organization_id,v_asset.workspace_id,v_asset.id);
        RAISE EXCEPTION 'live reservation unexpectedly retired';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    PERFORM pg_sleep(3.5);
    PERFORM fn_eb_direct_retire(v_asset.organization_id,v_asset.workspace_id,v_asset.id);
    IF NOT EXISTS (SELECT 1 FROM attach_pending WHERE object_key=v_asset.object_key
                   AND direct_state='EXPIRED' AND session_state='expired')
       OR EXISTS (SELECT 1 FROM enterprise_asset_direct_link WHERE asset_id=v_asset.id)
       OR EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path=v_asset.object_key)
       OR NOT EXISTS (SELECT 1 FROM enterprise_asset WHERE id=v_asset.id AND status='deleted') THEN
        RAISE EXCEPTION 'unused reservation retirement mismatch';
    END IF;
    DELETE FROM enterprise_asset WHERE id=v_asset.id;
END $$;
ROLLBACK;
