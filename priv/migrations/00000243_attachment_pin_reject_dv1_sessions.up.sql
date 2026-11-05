-- 00000243 attachment pin/recheck 拒绝 direct-v1 会话（V3-20 冻结合同收口）
--
-- 语义：fn_v5_pending_content_gate 只服务 v5 上传链（attachment_upload_logic /
-- attachment_storage_gateway_handler 的 register/recheck_content_pin）。dv1 会话
-- （attach_pending.direct_state 非空，ck_dv1_pending_direct_state 与
-- upload_registration->>'protocol'='direct-v1' 双向绑定，迁移 200）的内容改写
-- 必须走 dv1 自己的定位链；经 v5 pin 即绕开冻结身份改写内容 → 55000 拒绝
-- （attach_pending_repo:content_pin_result 映射 session_not_renewable）。
-- dv1 正向链不经过本 gate（fn_dv1_* 自有定位链），加拒不影响 direct 上传。
CREATE OR REPLACE FUNCTION fn_v5_pending_content_gate(p_key text, p_content jsonb,
                                                      p_identity jsonb, p_recheck boolean)
RETURNS boolean LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_before attach_pending%ROWTYPE; v_p attach_pending%ROWTYPE;
        v_a attachment%ROWTYPE; v_identity jsonb;
BEGIN
    SELECT * INTO v_before FROM attach_pending WHERE object_key=p_key;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 upload unknown session' USING ERRCODE='P0002';
    END IF;
    PERFORM fn_v5_lock_ordinary_upload(p_key);
    SELECT * INTO v_p FROM attach_pending WHERE object_key=p_key FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 upload unknown session' USING ERRCODE='P0002';
    END IF;
    IF (v_p.storage_driver,v_p.storage_namespace,v_p.storage_container,
        v_p.storage_locator_sha256,v_p.upload_registration,v_p.client_upload_id)
       IS DISTINCT FROM
       (v_before.storage_driver,v_before.storage_namespace,v_before.storage_container,
        v_before.storage_locator_sha256,v_before.upload_registration,v_before.client_upload_id)
       OR v_p.storage_driver IS NULL OR v_p.storage_namespace IS NULL
       OR v_p.storage_container IS NULL OR v_p.storage_locator_sha256 IS NULL THEN
        RAISE EXCEPTION 'v5 upload frozen location unavailable' USING ERRCODE='22023';
    END IF;
    v_identity := jsonb_build_object('driver',v_p.storage_driver,
        'namespace',v_p.storage_namespace,'container',v_p.storage_container,
        'key',p_key,'locator_digest',v_p.storage_locator_sha256);
    IF p_recheck AND p_identity IS DISTINCT FROM v_identity THEN
        RAISE EXCEPTION 'v5 upload frozen location mismatch' USING ERRCODE='22023';
    END IF;
    -- clock_timestamp is evaluated after lock waits, unlike transaction now().
    -- direct-v1 会话一律不可经 v5 pin/recheck 改写（迁移 243）。
    IF v_p.client_upload_id IS NULL OR v_p.upload_registration IS NULL
       OR v_p.session_deadline IS NULL OR v_p.upload_expires_at IS NULL
       OR least(v_p.session_deadline,v_p.upload_expires_at) <= clock_timestamp()
       OR (NOT p_recheck AND v_p.session_state <> 'registered')
       OR (p_recheck AND v_p.session_state NOT IN ('registered','confirmed'))
       OR v_p.direct_state IS NOT NULL THEN
        RAISE EXCEPTION 'v5 upload session not writable' USING ERRCODE='55000';
    END IF;
    IF EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path=p_key)
       OR EXISTS (SELECT 1 FROM enterprise_asset_delete_queue q
            WHERE q.client_upload_id=v_p.client_upload_id
               OR (q.storage_driver=v_p.storage_driver
                   AND q.storage_namespace=v_p.storage_namespace
                   AND q.storage_container=v_p.storage_container AND q.object_key=p_key))
       OR EXISTS (SELECT 1 FROM enterprise_asset a
            WHERE a.storage_driver=v_p.storage_driver
              AND a.storage_namespace=v_p.storage_namespace
              AND a.storage_container=v_p.storage_container AND a.object_key=p_key) THEN
        RAISE EXCEPTION 'v5 upload identity tombstoned or shared' USING ERRCODE='23505';
    END IF;
    SELECT * INTO v_a FROM attachment WHERE path=p_key FOR UPDATE;
    IF FOUND THEN
        IF v_a.lifecycle_state <> 'live'
           OR (v_a.storage_driver,v_a.storage_namespace,v_a.storage_container,
               v_a.storage_locator_sha256)
              IS DISTINCT FROM (v_p.storage_driver,v_p.storage_namespace,
                                v_p.storage_container,v_p.storage_locator_sha256)
           OR (v_p.session_state='confirmed' AND v_p.confirmed_attachment_id<>v_a.id) THEN
            RAISE EXCEPTION 'v5 upload asset not writable' USING ERRCODE='55000';
        END IF;
    ELSIF v_p.session_state='confirmed' THEN
        RAISE EXCEPTION 'v5 upload confirmed asset missing' USING ERRCODE='55000';
    END IF;
    IF NOT fn_v5_upload_receipt_matches(p_content,v_p.upload_registration->'declaration')
       OR (v_p.upload_content IS NOT NULL AND v_p.upload_content IS DISTINCT FROM p_content)
       OR (p_recheck AND v_p.upload_content IS DISTINCT FROM p_content) THEN
        RAISE EXCEPTION 'v5 upload receipt or declaration mismatch' USING ERRCODE='23510';
    END IF;
    IF NOT p_recheck AND v_p.upload_content IS NULL THEN
        UPDATE attach_pending SET upload_content=p_content WHERE object_key=p_key;
    END IF;
    RETURN true;
END;
$$;
