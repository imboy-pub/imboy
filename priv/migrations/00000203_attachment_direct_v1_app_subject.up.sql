-- 00000203 direct-v1 application 主体会话（step-13：Admin AI 头像，协议 §5）
-- Admin 身份域隔离：头像会话主体为 application（creator_user_id=0，
-- subject kind=application 与 V5 guard 契合），绝不借用 Human 凭据。
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

CREATE FUNCTION fn_dv1_register_application(p_key text, p_bucket text, p_scope text,
                                            p_app_id text, p_org_id text,
                                            p_client_upload_id uuid, p_mime text,
                                            p_file_name text, p_driver text,
                                            p_namespace text, p_container text,
                                            p_locator text)
RETURNS text LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_existing attach_pending%ROWTYPE;
BEGIN
    PERFORM pg_advisory_xact_lock(hashtextextended(p_key, 168));
    PERFORM fn_dv1_quota_check_app(p_app_id);
    IF p_client_upload_id IS NOT NULL THEN
        SELECT * INTO v_existing FROM attach_pending
            WHERE client_upload_id = p_client_upload_id
              AND session_state IN ('registered', 'confirmed')
            ORDER BY created_at DESC LIMIT 1 FOR UPDATE;
        IF FOUND THEN
            IF v_existing.direct_state IS NULL THEN
                RAISE EXCEPTION 'dv1 register client_upload_id belongs to legacy session'
                    USING ERRCODE = '23510';
            END IF;
            IF v_existing.upload_registration->'subject'->>'kind' IS DISTINCT FROM 'application'
               OR v_existing.upload_registration->'subject'->>'principal' IS DISTINCT FROM p_app_id
               OR v_existing.upload_registration->'declaration'->>'mime' IS DISTINCT FROM p_mime
               OR v_existing.upload_registration->'declaration'->>'file_name' IS DISTINCT FROM p_file_name THEN
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
        (p_key, p_bucket, p_scope, 0, p_client_upload_id, 'registered',
         'RESERVED',
         jsonb_build_object(
            'protocol', 'direct-v1',
            'subject', jsonb_build_object(
                'kind', 'application', 'principal', p_app_id, 'org_id', p_org_id),
            'declaration', jsonb_build_object(
                'scope', p_scope,
                'scope_ref', '',
                'application_id', p_app_id,
                'org_id', p_org_id,
                'mime', p_mime,
                'file_name', p_file_name)),
         now() + interval '7200 seconds',
         p_driver, p_namespace, p_container, p_locator);
    RETURN p_key;
END;
$$;

-- application 主体配额：每 app 并发 live direct 会话 ≤4（与 user 同预算口径）
CREATE FUNCTION fn_dv1_quota_check_app(p_app_id text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_live integer;
BEGIN
    SELECT count(*) INTO v_live FROM attach_pending
        WHERE creator_user_id = 0
          AND upload_registration->'subject'->>'principal' = p_app_id
          AND direct_state IN ('RESERVED', 'DECLARED', 'GRANTED', 'FINALIZING')
          AND (session_deadline IS NULL OR session_deadline > now());
    IF v_live >= 4 THEN
        RAISE EXCEPTION 'dv1 session quota exceeded (%)', v_live USING ERRCODE = '23514';
    END IF;
END;
$$;
