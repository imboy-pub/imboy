-- W2-02 真库证据发现的修复：fn_v5_confirm_pending 原声明 RETURNS integer
--（int4），而 attachment.id 为 TSID（int8 量级），RETURN p_attachment_id
-- 触发 int84 溢出（22003）——W2-01 的 SQL 探针用 900000201 恰好塞进 int4，
-- 未暴露。改为 RETURNS bigint（DROP 后重建；CREATE OR REPLACE 不允许改
-- 返回类型）。函数体与 179 逐字一致，仅返回类型变化。

DROP FUNCTION IF EXISTS fn_v5_confirm_pending(text, bigint);

CREATE FUNCTION fn_v5_confirm_pending(p_object_key text, p_attachment_id bigint)
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

