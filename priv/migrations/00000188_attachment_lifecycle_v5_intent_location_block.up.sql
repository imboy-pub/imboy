-- W4-01 入口收口（02 §4/§5 + 04 G4「不可证明的 pending 保持 blocked 并明示」）：
-- 三个意图函数统一身份分支。storage 身份四元组在 W4-03 storage registry
-- 落地前，新写入的 metadata 行（attachment / attach_pending /
-- enterprise_asset）恒为 NULL（迁移 178 加列后由 W4-03 registry 填充）——
-- 完整身份 → ready job（现状不变）；身份缺失 → blocked job
-- （last_error_code='legacy_location_unverified'，字节保留可解释，由
-- fn_v5_proof_activate_ordinary / W4-03 激活），不得以 CK 异常暴露。
-- 各函数既有资格谓词（gc_verified / 治理终态 / hold / 能力窗 / 查重）
-- 逐字保留；锁序不变。

-- ---------------------------------------------------------------- pending (184 v2)
CREATE OR REPLACE FUNCTION fn_v5_intent_pending_delete(p_object_key text,
                                                       p_reason text, p_mode text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_p attach_pending%ROWTYPE; v_a attachment%ROWTYPE; v_ready boolean;
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
    -- W4-01 身份分支：完整 → ready；缺失 → blocked（proof 激活），不以 CK 暴露。
    v_ready := v_p.storage_driver IS NOT NULL AND v_p.storage_namespace IS NOT NULL
        AND v_p.storage_container IS NOT NULL
        AND v_p.storage_locator_sha256 ~ '^[0-9a-f]{64}$';
    INSERT INTO attachment_object_deletion
        (path, bucket, storage_driver, storage_namespace, storage_container,
         storage_locator_sha256, job_state, next_retry_at, last_error_code)
    VALUES (v_p.object_key, v_p.bucket, v_p.storage_driver, v_p.storage_namespace,
            v_p.storage_container, v_p.storage_locator_sha256,
            CASE WHEN v_ready THEN 'ready' ELSE 'blocked' END,
            CASE WHEN v_ready THEN now() ELSE now() END,
            CASE WHEN v_ready THEN NULL ELSE 'legacy_location_unverified' END);
    INSERT INTO attachment_lifecycle_v5_audit
        (queue_domain, object_key, storage_namespace, action, reason, actor, details)
    VALUES ('ordinary', v_p.object_key, v_p.storage_namespace, 'delete_intent',
            p_reason, current_user,
            jsonb_build_object('pending_session', true, 'mode', p_mode,
                               'creator', v_p.creator_user_id,
                               'job_state', CASE WHEN v_ready THEN 'ready'
                                                 ELSE 'blocked' END));
END;
$$;

-- ---------------------------------------------------------------- purged (185 v2)
CREATE OR REPLACE FUNCTION fn_v5_intent_purged_delete(p_path text, p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a attachment%ROWTYPE; v_ready boolean;
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
    v_ready := v_a.storage_driver IS NOT NULL AND v_a.storage_namespace IS NOT NULL
        AND v_a.storage_container IS NOT NULL
        AND v_a.storage_locator_sha256 ~ '^[0-9a-f]{64}$';
    INSERT INTO attachment_object_deletion
        (path, bucket, storage_driver, storage_namespace, storage_container,
         storage_locator_sha256, job_state, next_retry_at, last_error_code)
    VALUES (v_a.path, v_a.url, v_a.storage_driver, v_a.storage_namespace,
            v_a.storage_container, v_a.storage_locator_sha256,
            CASE WHEN v_ready THEN 'ready' ELSE 'blocked' END,
            CASE WHEN v_ready THEN now() ELSE now() END,
            CASE WHEN v_ready THEN NULL ELSE 'legacy_location_unverified' END);
    INSERT INTO attachment_lifecycle_v5_audit
        (queue_domain, object_key, storage_namespace, action, reason, actor, details)
    VALUES ('ordinary', v_a.path, v_a.storage_namespace, 'delete_intent',
            p_reason, current_user,
            jsonb_build_object('attachment_id', v_a.id, 'purge_domain', 'ear',
                               'job_state', CASE WHEN v_ready THEN 'ready'
                                                  ELSE 'blocked' END));
END;
$$;

-- ---------------------------------------------------------------- eb (179 v2)
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
    -- W4-01 身份分支：缺失 → blocked（proof 激活），不以 CK 暴露。
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
