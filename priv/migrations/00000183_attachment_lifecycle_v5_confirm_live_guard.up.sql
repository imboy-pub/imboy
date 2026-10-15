-- W2-03 confirm/GC 状态协议屏障（02 §4）：fn_v5_confirm_pending 在锁住
-- attachment 行后复核 lifecycle_state——GC intent 已先提交（delete_intent/
-- deleted）时 confirm 必须落败（23510），两个提交顺序唯一胜出；confirm
-- 先提交则 intent 在自身锁内复核资格后照常裁决。函数体与 180 逐字一致，
-- 仅新增 lifecycle 守卫。

DROP FUNCTION IF EXISTS fn_v5_confirm_pending(text, bigint);

CREATE OR REPLACE FUNCTION fn_v5_confirm_pending(p_object_key text, p_attachment_id bigint)
RETURNS bigint LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_p attach_pending%ROWTYPE; v_a attachment%ROWTYPE; v_count integer;
BEGIN
    SELECT * INTO v_p FROM attach_pending WHERE object_key = p_object_key FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 confirm unknown session' USING ERRCODE = 'P0002';
    END IF;
    SELECT * INTO v_a FROM attachment WHERE id = p_attachment_id FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 confirm unknown asset' USING ERRCODE = 'P0002';
    END IF;
    -- W2-03 屏障：GC intent 先提交（delete_intent/deleted）时 confirm 落败。
    IF v_a.lifecycle_state IS NOT NULL AND v_a.lifecycle_state <> 'live' THEN
        RAISE EXCEPTION 'v5 confirm on non-live asset' USING ERRCODE = '23510';
    END IF;
    IF v_p.session_state = 'confirmed' THEN
        IF v_p.confirmed_attachment_id = p_attachment_id THEN
            RETURN v_p.confirmed_attachment_id;  -- legal replay returns original
        END IF;
        RAISE EXCEPTION 'v5 confirmed link immutable' USING ERRCODE = '23510';
    END IF;
    IF v_p.session_state <> 'registered' THEN
        RAISE EXCEPTION 'v5 confirm illegal from %', v_p.session_state
            USING ERRCODE = '23510';
    END IF;
    -- Identity/owner/scope match: pending path vs asset path, creator, scope.
    IF v_a.path IS DISTINCT FROM v_p.object_key THEN
        RAISE EXCEPTION 'v5 confirm key mismatch' USING ERRCODE = '23510';
    END IF;
    IF v_a.creator_user_id IS DISTINCT FROM v_p.creator_user_id THEN
        RAISE EXCEPTION 'v5 confirm owner mismatch' USING ERRCODE = '23510';
    END IF;
    IF v_a.scope IS DISTINCT FROM v_p.scope THEN
        RAISE EXCEPTION 'v5 confirm scope mismatch' USING ERRCODE = '23510';
    END IF;
    IF (v_a.storage_driver, v_a.storage_namespace, v_a.storage_container,
        v_a.storage_locator_sha256)
       IS DISTINCT FROM
       (v_p.storage_driver, v_p.storage_namespace, v_p.storage_container,
        v_p.storage_locator_sha256) THEN
        RAISE EXCEPTION 'v5 confirm location mismatch' USING ERRCODE = '23510';
    END IF;
    IF v_p.upload_content IS NULL THEN
        RAISE EXCEPTION 'v5 confirm requires pinned content receipt'
            USING ERRCODE = '23510';
    END IF;
    UPDATE attach_pending SET session_state = 'confirmed',
            confirmed_attachment_id = p_attachment_id
        WHERE object_key = p_object_key AND session_state = 'registered';
    GET DIAGNOSTICS v_count = ROW_COUNT;
    IF v_count <> 1 THEN
        RAISE EXCEPTION 'v5 confirm lost race' USING ERRCODE = '40001';
    END IF;
    RETURN p_attachment_id;
END;
$$;
ALTER FUNCTION fn_v5_confirm_pending(text, bigint) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_confirm_pending(text, bigint) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_confirm_pending(text, bigint) TO v5_app_runtime;
