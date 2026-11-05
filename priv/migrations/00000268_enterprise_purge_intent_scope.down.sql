DO $m$
DECLARE d text;
BEGIN
 SELECT pg_get_functiondef('public.fn_v5_intent_eb_delete(bigint,bigint,bigint,text)'::regprocedure) INTO d;
 IF position($old$    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue q
               WHERE q.organization_id = p_org AND q.workspace_id = p_ws
                 AND q.asset_id = p_asset) THEN
        IF v_a.lifecycle_state = 'delete_intent' AND EXISTS (
            SELECT 1 FROM enterprise_asset_delete_queue q
             WHERE q.organization_id = p_org AND q.workspace_id = p_ws AND q.asset_id = p_asset
               AND q.object_key IS NOT DISTINCT FROM v_a.object_key
               AND q.client_upload_id IS NOT DISTINCT FROM v_a.client_upload_id
               AND q.upload_registration IS NOT DISTINCT FROM v_a.upload_registration
               AND q.storage_driver IS NOT DISTINCT FROM v_a.storage_driver
               AND q.storage_namespace IS NOT DISTINCT FROM v_a.storage_namespace
               AND q.storage_container IS NOT DISTINCT FROM v_a.storage_container
               AND q.storage_locator_sha256 IS NOT DISTINCT FROM v_a.storage_locator_sha256
               AND q.storage_physical_key IS NOT DISTINCT FROM v_a.storage_physical_key
        ) THEN
            RETURN;
        END IF;
        RAISE EXCEPTION 'v5 intent enterprise job identity conflict' USING ERRCODE = '23505';
    END IF;
    IF v_a.lifecycle_state <> 'live' THEN
        RAISE EXCEPTION 'v5 intent non-live enterprise asset' USING ERRCODE = '23510';
    END IF;$old$ in d) = 0 THEN RAISE EXCEPTION 'unexpected enterprise intent contract'; END IF;
 d := replace(d, $old$    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue q
               WHERE q.organization_id = p_org AND q.workspace_id = p_ws
                 AND q.asset_id = p_asset) THEN
        IF v_a.lifecycle_state = 'delete_intent' AND EXISTS (
            SELECT 1 FROM enterprise_asset_delete_queue q
             WHERE q.organization_id = p_org AND q.workspace_id = p_ws AND q.asset_id = p_asset
               AND q.object_key IS NOT DISTINCT FROM v_a.object_key
               AND q.client_upload_id IS NOT DISTINCT FROM v_a.client_upload_id
               AND q.upload_registration IS NOT DISTINCT FROM v_a.upload_registration
               AND q.storage_driver IS NOT DISTINCT FROM v_a.storage_driver
               AND q.storage_namespace IS NOT DISTINCT FROM v_a.storage_namespace
               AND q.storage_container IS NOT DISTINCT FROM v_a.storage_container
               AND q.storage_locator_sha256 IS NOT DISTINCT FROM v_a.storage_locator_sha256
               AND q.storage_physical_key IS NOT DISTINCT FROM v_a.storage_physical_key
        ) THEN
            RETURN;
        END IF;
        RAISE EXCEPTION 'v5 intent enterprise job identity conflict' USING ERRCODE = '23505';
    END IF;
    IF v_a.lifecycle_state <> 'live' THEN
        RAISE EXCEPTION 'v5 intent non-live enterprise asset' USING ERRCODE = '23510';
    END IF;$old$, $new$    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue
               WHERE organization_id = p_org AND workspace_id = p_ws
                 AND asset_id = p_asset) THEN
        RAISE EXCEPTION 'v5 intent enterprise job already exists'
            USING ERRCODE = '23505';
    END IF;$new$);
 IF position($old$                 AND h.released_at IS NULL
                 AND (h.scope_type = 'workspace'
                   OR (h.scope_type = 'conversation' AND h.scope_conversation_id = v_a.conversation_id)
                   OR (h.scope_type = 'message' AND h.scope_message_id = v_a.message_id))) THEN$old$ in d) = 0 THEN RAISE EXCEPTION 'unexpected enterprise intent contract'; END IF;
 d := replace(d, $old$                 AND h.released_at IS NULL
                 AND (h.scope_type = 'workspace'
                   OR (h.scope_type = 'conversation' AND h.scope_conversation_id = v_a.conversation_id)
                   OR (h.scope_type = 'message' AND h.scope_message_id = v_a.message_id))) THEN$old$, $new$                 AND h.released_at IS NULL) THEN$new$);
 IF position($old$    IF v_a.retain_until IS NOT NULL AND v_a.retain_until > now() THEN$old$ in d) = 0 THEN RAISE EXCEPTION 'unexpected enterprise intent contract'; END IF;
 d := replace(d, $old$    IF v_a.retain_until IS NOT NULL AND v_a.retain_until > now() THEN$old$, $new$    IF v_a.lifecycle_state <> 'live' THEN
        RAISE EXCEPTION 'v5 intent non-live enterprise asset' USING ERRCODE = '23510';
    END IF;
    IF v_a.retain_until IS NOT NULL AND v_a.retain_until > now() THEN$new$);
 -- Preserve the physical-key projection: reverting it could delete a different object.
 -- Preserve the upload-capability guard across downgrade.
 EXECUTE d;
END $m$;
