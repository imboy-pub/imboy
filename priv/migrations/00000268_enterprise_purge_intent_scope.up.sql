DO $m$
DECLARE d text;
BEGIN
 SELECT pg_get_functiondef('public.fn_v5_intent_eb_delete(bigint,bigint,bigint,text)'::regprocedure) INTO d;
 IF position($old$    IF v_a.lifecycle_state <> 'live' THEN
        RAISE EXCEPTION 'v5 intent non-live enterprise asset' USING ERRCODE = '23510';
    END IF;
$old$ in d) = 0 THEN RAISE EXCEPTION 'unexpected enterprise intent contract'; END IF;
 d := replace(d, $old$    IF v_a.lifecycle_state <> 'live' THEN
        RAISE EXCEPTION 'v5 intent non-live enterprise asset' USING ERRCODE = '23510';
    END IF;
$old$, $new$$new$);
 IF position($old$                 AND h.released_at IS NULL) THEN$old$ in d) = 0 THEN RAISE EXCEPTION 'unexpected enterprise intent contract'; END IF;
 d := replace(d, $old$                 AND h.released_at IS NULL) THEN$old$, $new$                 AND h.released_at IS NULL
                 AND (h.scope_type = 'workspace'
                   OR (h.scope_type = 'conversation' AND h.scope_conversation_id = v_a.conversation_id)
                   OR (h.scope_type = 'message' AND h.scope_message_id = v_a.message_id))) THEN$new$);
 IF position($old$    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue
               WHERE organization_id = p_org AND workspace_id = p_ws
                 AND asset_id = p_asset) THEN
        RAISE EXCEPTION 'v5 intent enterprise job already exists'
            USING ERRCODE = '23505';
    END IF;$old$ in d) = 0 THEN RAISE EXCEPTION 'unexpected enterprise intent contract'; END IF;
 d := replace(d, $old$    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue
               WHERE organization_id = p_org AND workspace_id = p_ws
                 AND asset_id = p_asset) THEN
        RAISE EXCEPTION 'v5 intent enterprise job already exists'
            USING ERRCODE = '23505';
    END IF;$old$, $new$    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue q
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
    END IF;$new$);
 IF position($old$         storage_locator_sha256, job_state, next_retry_at, last_error_code)$old$ in d) > 0 THEN
  d := replace(d, $old$         storage_locator_sha256, job_state, next_retry_at, last_error_code)$old$, $new$         storage_locator_sha256, storage_physical_key, job_state, next_retry_at, last_error_code)$new$);
 ELSIF position($new$         storage_locator_sha256, storage_physical_key, job_state, next_retry_at, last_error_code)$new$ in d) = 0 THEN
  RAISE EXCEPTION 'unexpected enterprise intent contract';
 END IF;
 IF position($old$            v_a.storage_locator_sha256,
            CASE WHEN v_ready$old$ in d) > 0 THEN
  d := replace(d, $old$            v_a.storage_locator_sha256,
            CASE WHEN v_ready$old$, $new$            v_a.storage_locator_sha256, v_a.storage_physical_key,
            CASE WHEN v_ready$new$);
 ELSIF position($new$            v_a.storage_locator_sha256, v_a.storage_physical_key,
            CASE WHEN v_ready$new$ in d) = 0 THEN
  RAISE EXCEPTION 'unexpected enterprise intent contract';
 END IF;
 IF position($guard$    IF v_a.client_upload_id IS NOT NULL AND
       (v_a.upload_expires_at IS NULL OR v_a.upload_expires_at > clock_timestamp()) THEN
        RAISE EXCEPTION 'v5 intent enterprise upload capability still live or unknown'
            USING ERRCODE = '23510';
    END IF;
$guard$ in d) = 0 THEN
  IF position($anchor$    IF EXISTS (SELECT 1 FROM enterprise_retention_hold h$anchor$ in d) = 0 THEN
   RAISE EXCEPTION 'unexpected enterprise intent contract';
  END IF;
  d := replace(d, $anchor$    IF EXISTS (SELECT 1 FROM enterprise_retention_hold h$anchor$, $guard$    IF v_a.client_upload_id IS NOT NULL AND
       (v_a.upload_expires_at IS NULL OR v_a.upload_expires_at > clock_timestamp()) THEN
        RAISE EXCEPTION 'v5 intent enterprise upload capability still live or unknown'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM enterprise_retention_hold h$guard$);
 END IF;
 EXECUTE d;
END $m$;
