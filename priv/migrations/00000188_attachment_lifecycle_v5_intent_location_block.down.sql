-- V5 DOWN 阻断（03 M4）：非空库禁止回退；空库可逆移除本迁移新增内容。
DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM attach_pending LIMIT 1)
       OR EXISTS (SELECT 1 FROM attachment LIMIT 1)
       OR EXISTS (SELECT 1 FROM enterprise_asset LIMIT 1)
       OR EXISTS (SELECT 1 FROM attachment_object_deletion LIMIT 1)
       OR EXISTS (SELECT 1 FROM enterprise_asset_delete_queue LIMIT 1) THEN
        RAISE EXCEPTION 'V5_DOWN_BLOCKED_USE_FORWARD_FIX';
    END IF;
END;
$$;

-- 恢复各 fn 为本迁移前的版本（186 pending / 187 purged / 179 eb 原文）。
-- pending：见 00000186（无身份分支版）。
CREATE OR REPLACE FUNCTION fn_v5_intent_pending_delete(p_object_key text,
                                                       p_reason text, p_mode text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_p attach_pending%ROWTYPE; v_a attachment%ROWTYPE;
BEGIN
    IF p_mode NOT IN ('reject', 'sweep') THEN
        RAISE EXCEPTION 'v5 pending intent unknown mode' USING ERRCODE = '23514';
    END IF;
    SELECT * INTO v_p FROM attach_pending WHERE object_key = p_object_key FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 pending intent unknown session' USING ERRCODE = 'P0002';
    END IF;
    IF p_mode = 'sweep' THEN
        IF v_p.session_state NOT IN ('registered', 'rejected', 'expired') THEN
            RAISE EXCEPTION 'v5 pending sweep illegal from %', v_p.session_state
                USING ERRCODE = '23510';
        END IF;
        IF v_p.upload_expires_at IS NULL OR v_p.upload_expires_at > now() THEN
            RAISE EXCEPTION 'v5 pending intent upload capability not exhausted'
                USING ERRCODE = '23510';
        END IF;
        IF v_p.session_state = 'registered' THEN
            UPDATE attach_pending SET session_state = 'expired'
                WHERE object_key = p_object_key AND session_state = 'registered';
            IF NOT FOUND THEN
                RAISE EXCEPTION 'v5 pending intent lost eligibility race'
                    USING ERRCODE = '40001';
            END IF;
        END IF;
    ELSE
        IF v_p.session_state <> 'registered' THEN
            RAISE EXCEPTION 'v5 pending reject illegal from %', v_p.session_state
                USING ERRCODE = '23510';
        END IF;
    END IF;
    SELECT * INTO v_a FROM attachment WHERE path = p_object_key;
    IF FOUND AND v_a.lifecycle_state <> 'deleted' THEN
        RAISE EXCEPTION 'v5 pending intent on metadata-backed asset'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path = p_object_key) THEN
        RAISE EXCEPTION 'v5 intent job already exists (tombstone)'
            USING ERRCODE = '23505';
    END IF;
    PERFORM fn_v5_lock_identity(v_p.storage_driver, v_p.storage_namespace,
                                v_p.storage_container, v_p.object_key);
    PERFORM pg_advisory_xact_lock(hashtextextended(v_p.object_key, 168));
    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue q
               WHERE q.storage_namespace = v_p.storage_namespace
                 AND q.storage_container = v_p.storage_container
                 AND q.object_key = v_p.object_key) THEN
        RAISE EXCEPTION 'v5 intent cross-domain identity conflict'
            USING ERRCODE = '23510';
    END IF;
    IF p_mode = 'reject' THEN
        UPDATE attach_pending SET session_state = 'rejected'
            WHERE object_key = p_object_key AND session_state = 'registered';
        IF NOT FOUND THEN
            RAISE EXCEPTION 'v5 pending intent lost eligibility race'
                USING ERRCODE = '40001';
        END IF;
    END IF;
    UPDATE attach_pending SET session_state = 'delete_intent'
        WHERE object_key = p_object_key
          AND session_state IN ('rejected', 'expired');
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 pending intent lost eligibility race' USING ERRCODE = '40001';
    END IF;
    INSERT INTO attachment_object_deletion
        (path, bucket, storage_driver, storage_namespace, storage_container,
         storage_locator_sha256, job_state, next_retry_at, last_error_code)
    VALUES (v_p.object_key, v_p.bucket, v_p.storage_driver, v_p.storage_namespace,
            v_p.storage_container, v_p.storage_locator_sha256,
            'ready', now(), NULL);
    INSERT INTO attachment_lifecycle_v5_audit
        (queue_domain, object_key, storage_namespace, action, reason, actor, details)
    VALUES ('ordinary', v_p.object_key, v_p.storage_namespace, 'delete_intent',
            p_reason, current_user,
            jsonb_build_object('pending_session', true, 'mode', p_mode,
                               'creator', v_p.creator_user_id));
END;
$$;

-- purged：恢复 187 原文（ready 直插）。
CREATE OR REPLACE FUNCTION fn_v5_intent_purged_delete(p_path text, p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a attachment%ROWTYPE;
BEGIN
    SELECT * INTO v_a FROM attachment WHERE path = p_path FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 purged intent unknown attachment' USING ERRCODE = 'P0002';
    END IF;
    IF v_a.lifecycle_state <> 'live' THEN
        RAISE EXCEPTION 'v5 purged intent non-live asset' USING ERRCODE = '23510';
    END IF;
    IF NOT EXISTS (
        SELECT 1 FROM enterprise_attachment_retention ear
        WHERE ear.attachment_id = v_a.id AND ear.purge_state = 'purged'
    ) THEN
        RAISE EXCEPTION 'v5 purged intent requires EAR purge terminal state'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path = p_path) THEN
        RAISE EXCEPTION 'v5 intent job already exists (tombstone)'
            USING ERRCODE = '23505';
    END IF;
    PERFORM fn_v5_lock_identity(v_a.storage_driver, v_a.storage_namespace,
                                v_a.storage_container, v_a.path);
    PERFORM pg_advisory_xact_lock(hashtextextended(v_a.path, 168));
    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue q
               WHERE q.storage_namespace = v_a.storage_namespace
                 AND q.storage_container = v_a.storage_container
                 AND q.object_key = v_a.path) THEN
        RAISE EXCEPTION 'v5 intent cross-domain identity conflict'
            USING ERRCODE = '23510';
    END IF;
    UPDATE attachment SET lifecycle_state = 'delete_intent'
        WHERE id = v_a.id AND lifecycle_state = 'live';
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 purged intent lost eligibility race' USING ERRCODE = '40001';
    END IF;
    INSERT INTO attachment_object_deletion
        (path, bucket, storage_driver, storage_namespace, storage_container,
         storage_locator_sha256, job_state, next_retry_at, last_error_code)
    VALUES (v_a.path, v_a.url, v_a.storage_driver, v_a.storage_namespace,
            v_a.storage_container, v_a.storage_locator_sha256,
            'ready', now(), NULL);
    INSERT INTO attachment_lifecycle_v5_audit
        (queue_domain, object_key, storage_namespace, action, reason, actor, details)
    VALUES ('ordinary', v_a.path, v_a.storage_namespace, 'delete_intent',
            p_reason, current_user,
            jsonb_build_object('attachment_id', v_a.id, 'purge_domain', 'ear'));
END;
$$;

-- eb：恢复 179 原文。
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
