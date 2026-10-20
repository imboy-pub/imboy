-- down: 恢复 204 版 fn_dv1_register（无 scope_ref 比较）与 203 版
-- fn_dv1_register_application（无竞态三件套）。纯函数替换，无数据形态
-- 变化，无需数据阻断。
CREATE OR REPLACE FUNCTION fn_dv1_register(p_key text, p_bucket text, p_scope text,
                                p_scope_ref text, p_uid bigint,
                                p_client_upload_id uuid, p_mime text,
                                p_file_name text, p_driver text,
                                p_namespace text, p_container text,
                                p_locator text)
RETURNS text LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_existing attach_pending%ROWTYPE; v_new_key text;
BEGIN
    IF p_client_upload_id IS NOT NULL THEN
        PERFORM pg_advisory_xact_lock(hashtextextended(p_client_upload_id::text, 169));
    END IF;
    PERFORM pg_advisory_xact_lock(hashtextextended(p_key, 168));
    LOOP
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
                IF v_existing.creator_user_id <> p_uid
                   OR v_existing.upload_registration->'declaration'->>'mime' IS DISTINCT FROM p_mime
                   OR v_existing.upload_registration->'declaration'->>'file_name' IS DISTINCT FROM p_file_name
                   OR v_existing.upload_registration->'declaration'->>'scope' IS DISTINCT FROM p_scope THEN
                    RAISE EXCEPTION 'dv1 register declaration conflict' USING ERRCODE = '23510';
                END IF;
                RETURN v_existing.object_key;
            END IF;
        END IF;
        PERFORM fn_dv1_quota_check(p_uid);
        IF p_client_upload_id IS NOT NULL THEN
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
                 p_driver, p_namespace, p_container, p_locator)
            ON CONFLICT (client_upload_id) WHERE client_upload_id IS NOT NULL
            DO NOTHING
            RETURNING object_key INTO v_new_key;
            IF v_new_key IS NOT NULL THEN
                RETURN v_new_key;
            END IF;
        ELSE
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
                 p_driver, p_namespace, p_container, p_locator)
            RETURNING object_key INTO v_new_key;
            RETURN v_new_key;
        END IF;
    END LOOP;
END;
$$;

CREATE OR REPLACE FUNCTION fn_dv1_register_application(p_key text, p_bucket text, p_scope text,
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
