-- W4-04/M3 前置补强（02 §4 行 47 双向查重）：fn_v5_intent_eb_delete 补
-- ordinary queue（attachment_object_deletion）同物理身份反向查重——
-- 原 179 版只查企业队列自体；同 (namespace,container,object_key) 已在
-- 普通队列（blocked 含 legacy 接管行）时企业意图拒绝（23510），与
-- fn_v5_intent_ordinary_delete 的跨域检查对称。其余逻辑逐字保留 179
-- 原文（186 已加身份分支）。

CREATE OR REPLACE FUNCTION fn_v5_intent_eb_delete(p_org bigint, p_ws bigint,
                                                  p_asset bigint, p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a enterprise_asset%ROWTYPE; v_ready boolean;
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
    -- W4-04 双向查重（02 §4）：普通队列已有同物理身份（含 legacy blocked
    -- 接管行）时拒绝——身份永不复用。
    v_ready := v_a.storage_driver IS NOT NULL AND v_a.storage_driver IN ('garage', 'disk')
        AND v_a.storage_namespace IS NOT NULL AND v_a.storage_namespace <> ''
        AND v_a.storage_container IS NOT NULL AND v_a.storage_container <> ''
        AND v_a.storage_locator_sha256 ~ '^[0-9a-f]{64}$';
    INSERT INTO enterprise_asset_delete_queue
        (organization_id, workspace_id, asset_id, object_key,
         client_upload_id, upload_registration,
         storage_driver, storage_namespace, storage_container,
         storage_locator_sha256, job_state, next_retry_at, last_error_code)
    VALUES (p_org, p_ws, p_asset, v_a.object_key,
            v_a.client_upload_id, v_a.upload_registration,
            v_a.storage_driver, v_a.storage_namespace, v_a.storage_container,
            v_a.storage_locator_sha256,
            CASE WHEN v_ready THEN 'ready' ELSE 'blocked' END,
            CASE WHEN v_ready THEN now() ELSE now() END,
            CASE WHEN v_ready THEN NULL ELSE 'legacy_location_unverified' END);
    INSERT INTO attachment_lifecycle_v5_audit
        (queue_domain, object_key, storage_namespace, action, reason, actor, details)
    VALUES ('enterprise', v_a.object_key, v_a.storage_namespace, 'delete_intent',
            p_reason, current_user,
            jsonb_build_object('org', p_org, 'ws', p_ws, 'asset', p_asset,
                               'job_state', CASE WHEN v_ready THEN 'ready'
                                                  ELSE 'blocked' END));
END;
$$;
ALTER FUNCTION fn_v5_intent_eb_delete(bigint, bigint, bigint, text)
    OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_intent_eb_delete(bigint, bigint, bigint, text)
    FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_intent_eb_delete(bigint, bigint, bigint, text)
    TO v5_app_runtime;
