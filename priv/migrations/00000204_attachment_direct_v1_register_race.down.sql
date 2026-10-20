-- down: 恢复 201 版 fn_dv1_register 并移除唯一索引
DROP INDEX IF EXISTS uk_attach_pending_client_upload_id;

CREATE FUNCTION fn_dv1_register(p_key text, p_bucket text, p_scope text,
                                p_scope_ref text, p_uid bigint,
                                p_client_upload_id uuid, p_mime text,
                                p_file_name text, p_driver text,
                                p_namespace text, p_container text,
                                p_locator text)
RETURNS text LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_existing attach_pending%ROWTYPE;
BEGIN
    PERFORM pg_advisory_xact_lock(hashtextextended(p_key, 168));
    -- 每 subject 并发 live direct 会话配额（step-7 / T46；锁后检查）
    PERFORM fn_dv1_quota_check(p_uid);
    -- 幂等恢复：同 client_upload_id 的 live direct 会话
    IF p_client_upload_id IS NOT NULL THEN
        SELECT * INTO v_existing FROM attach_pending
            WHERE client_upload_id = p_client_upload_id
              AND session_state IN ('registered', 'confirmed')
            ORDER BY created_at DESC LIMIT 1 FOR UPDATE;
        IF FOUND THEN
            IF v_existing.direct_state IS NULL THEN
                -- 旧 V5 会话占用同 client_upload_id：拒绝复用（协议混用禁止）
                RAISE EXCEPTION 'dv1 register client_upload_id belongs to legacy session'
                    USING ERRCODE = '23510';
            END IF;
            IF v_existing.creator_user_id <> p_uid
               OR v_existing.upload_registration->'declaration'->>'mime' IS DISTINCT FROM p_mime
               OR v_existing.upload_registration->'declaration'->>'file_name' IS DISTINCT FROM p_file_name
               OR v_existing.upload_registration->'declaration'->>'scope' IS DISTINCT FROM p_scope THEN
                RAISE EXCEPTION 'dv1 register declaration conflict' USING ERRCODE = '23510';
            END IF;
            RETURN v_existing.object_key;
        END IF;
    END IF;
    INSERT INTO attach_pending
        (object_key, bucket, scope, creator_user_id, client_upload_id, session_state,
         direct_state, upload_registration, session_deadline,
         storage_driver, storage_namespace, storage_container, storage_locator_sha256)
    VALUES
        (p_key, p_bucket, p_scope, p_uid, p_client_upload_id, 'registered',
         'RESERVED',
         jsonb_build_object(
            'protocol', 'direct-v1',
            'subject', jsonb_build_object('kind', 'user', 'principal', p_uid::text),
            'declaration', jsonb_build_object(
                'scope', p_scope,
                'scope_ref', coalesce(p_scope_ref, ''),
                'mime', p_mime,
                'file_name', p_file_name)),
         now() + interval '7200 seconds',
         p_driver, p_namespace, p_container, p_locator);
    RETURN p_key;
END;
$$;

-- ---------------------------------------------------------------- declare
-- expected 冻结（I03 与 actual proof 分开）。RESERVED→DECLARED；
-- 已 DECLARED 时同值幂等、异值冲突（T03）。
CREATE FUNCTION fn_dv1_declare(p_key text, p_sha256 text, p_size bigint, p_mime text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_direct text; v_existing jsonb; v_deadline timestamptz;
BEGIN
    PERFORM pg_advisory_xact_lock(hashtextextended(p_key, 168));
    SELECT direct_state, direct_expected, session_deadline INTO v_direct, v_existing, v_deadline
        FROM attach_pending WHERE object_key = p_key FOR UPDATE;
    IF NOT FOUND THEN
