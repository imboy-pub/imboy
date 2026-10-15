-- V5 DOWN 阻断（03 M4）：非空库禁止回退；空库恢复 186 版 eb intent
-- （无 ordinary queue 反向查重）。
DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM enterprise_asset LIMIT 1)
       OR EXISTS (SELECT 1 FROM enterprise_asset_delete_queue LIMIT 1) THEN
        RAISE EXCEPTION 'V5_DOWN_BLOCKED_USE_FORWARD_FIX';
    END IF;
END;
$$;

CREATE OR REPLACE FUNCTION fn_v5_intent_eb_delete(p_org bigint, p_ws bigint,
                                                  p_asset bigint, p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a enterprise_asset%ROWTYPE;
BEGIN
    SELECT * INTO v_a FROM enterprise_asset
        WHERE organization_id = p_org AND workspace_id = p_ws AND id = p_asset
        FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 intent unknown enterprise asset' USING ERRCODE = 'P0002';
    END IF;
    IF v_a.lifecycle_state <> 'live' THEN
        RAISE EXCEPTION 'v5 intent non-live enterprise asset' USING ERRCODE = '23510';
    END IF;
    IF v_a.retain_until IS NOT NULL AND v_a.retain_until > now() THEN
        RAISE EXCEPTION 'v5 intent enterprise retention active' USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM enterprise_retention_hold h
               WHERE h.organization_id = p_org AND h.workspace_id = p_ws
                 AND h.released_at IS NULL) THEN
        RAISE EXCEPTION 'v5 intent blocked by active workspace hold'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue
               WHERE organization_id = p_org AND workspace_id = p_ws
                 AND asset_id = p_asset) THEN
        RAISE EXCEPTION 'v5 intent enterprise job already exists'
            USING ERRCODE = '23505';
    END IF;
    PERFORM pg_advisory_xact_lock(hashtextextended(
        'v5-eb-upload:' || coalesce(v_a.client_upload_id::text, p_asset::text), 169));
    UPDATE enterprise_asset SET lifecycle_state = 'delete_intent'
        WHERE id = p_asset AND lifecycle_state = 'live';
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 intent lost eligibility race' USING ERRCODE = '40001';
    END IF;
    INSERT INTO enterprise_asset_delete_queue
        (organization_id, workspace_id, asset_id, object_key,
         client_upload_id, upload_registration,
         storage_driver, storage_namespace, storage_container,
         storage_locator_sha256, job_state, next_retry_at, last_error_code)
    VALUES (p_org, p_ws, p_asset, v_a.object_key,
            v_a.client_upload_id, v_a.upload_registration,
            v_a.storage_driver, v_a.storage_namespace, v_a.storage_container,
            v_a.storage_locator_sha256, 'ready', now(), NULL);
    INSERT INTO attachment_lifecycle_v5_audit
        (queue_domain, object_key, storage_namespace, action, reason, actor, details)
    VALUES ('enterprise', v_a.object_key, v_a.storage_namespace, 'delete_intent',
            p_reason, current_user,
            jsonb_build_object('org', p_org, 'ws', p_ws, 'asset', p_asset));
END;
$$;
ALTER FUNCTION fn_v5_intent_eb_delete(bigint, bigint, bigint, text)
    OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_intent_eb_delete(bigint, bigint, bigint, text)
    FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_intent_eb_delete(bigint, bigint, bigint, text)
    TO v5_app_runtime;
