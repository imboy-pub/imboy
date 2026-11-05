DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM attach_pending
        WHERE direct_state IS NOT NULL AND session_state = 'registered'
          AND (upload_registration->'declaration'->>'cipher' IS NOT NULL
               OR upload_registration->'declaration'->>'anchor_msg_id' IS NOT NULL
               OR coalesce(upload_registration->'declaration'->>'purpose', 'attachment') <> 'attachment')) THEN
        RAISE EXCEPTION 'direct attachment contract sessions must be settled before rollback';
    END IF;
END;
$$;
DROP TRIGGER trg_dv1_attachment_contract_guard ON attach_pending;
DROP FUNCTION fn_dv1_attachment_contract_guard();
CREATE OR REPLACE FUNCTION fn_dv1_register(p_key text, p_bucket text, p_scope text,
                                p_scope_ref text, p_uid bigint,
                                p_client_upload_id uuid, p_mime text,
                                p_file_name text, p_driver text,
                                p_namespace text, p_container text,
                                p_locator text)
RETURNS text LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_existing attach_pending%ROWTYPE; v_new_key text;
BEGIN
    -- 同 client_upload_id 并发注册先串行化（key 锁随机不互斥，见 204）
    IF p_client_upload_id IS NOT NULL THEN
        PERFORM pg_advisory_xact_lock(hashtextextended(p_client_upload_id::text, 169));
    END IF;
    PERFORM pg_advisory_xact_lock(hashtextextended(p_key, 168));
    LOOP
        -- 幂等恢复：同 client_upload_id 的 live direct 会话；声明四元组
        -- （scope/scope_ref/mime/file_name）全等才复用，任一异值 23510。
        IF p_client_upload_id IS NOT NULL THEN
            SELECT * INTO v_existing FROM attach_pending
                WHERE client_upload_id = p_client_upload_id
                ORDER BY created_at DESC LIMIT 1 FOR UPDATE;
            IF FOUND THEN
                IF v_existing.direct_state IS NULL THEN
                    RAISE EXCEPTION 'dv1 register client_upload_id belongs to legacy session'
                        USING ERRCODE = '23510';
                END IF;
                IF v_existing.session_state NOT IN ('registered', 'confirmed') THEN
                    RAISE EXCEPTION 'dv1 register client_upload_id belongs to terminal session' USING ERRCODE = '23510';
                END IF;
                IF v_existing.creator_user_id <> p_uid
                   OR v_existing.upload_registration->'declaration'->>'mime' IS DISTINCT FROM p_mime
                   OR v_existing.upload_registration->'declaration'->>'file_name' IS DISTINCT FROM p_file_name
                   OR v_existing.upload_registration->'declaration'->>'scope' IS DISTINCT FROM p_scope
                   OR v_existing.upload_registration->'declaration'->>'scope_ref'
                       IS DISTINCT FROM coalesce(p_scope_ref, '') THEN
                    RAISE EXCEPTION 'dv1 register declaration conflict' USING ERRCODE = '23510';
                END IF;
                RETURN v_existing.object_key;
            END IF;
        END IF;
        -- 每 subject 并发 live direct 会话配额（仅新建会话消耗）
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
            RAISE EXCEPTION 'dv1 register concurrent conflict' USING ERRCODE = '40001';
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

DROP FUNCTION fn_dv1_register(text,text,text,text,bigint,uuid,text,text,text,text,text,text,text,text,text);

CREATE OR REPLACE FUNCTION fn_dv1_register_application(p_key text, p_bucket text, p_scope text,
                                            p_app_id text, p_org_id text,
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
                ORDER BY created_at DESC LIMIT 1 FOR UPDATE;
            IF FOUND THEN
                IF v_existing.direct_state IS NULL THEN
                    RAISE EXCEPTION 'dv1 register client_upload_id belongs to legacy session'
                        USING ERRCODE = '23510';
                END IF;
                IF v_existing.session_state NOT IN ('registered', 'confirmed') THEN
                    RAISE EXCEPTION 'dv1 register client_upload_id belongs to terminal session' USING ERRCODE = '23510';
                END IF;
                -- 主体三元组（kind/principal/org_id）+ 声明（mime/file_name）
                -- 全等才复用；org_id 异值 = 同 app 换 org，按异声明拒绝。
                IF v_existing.upload_registration->'subject'->>'kind' IS DISTINCT FROM 'application'
                   OR v_existing.upload_registration->'subject'->>'principal' IS DISTINCT FROM p_app_id
                   OR v_existing.upload_registration->'subject'->>'org_id' IS DISTINCT FROM p_org_id
                   OR v_existing.upload_registration->'declaration'->>'scope' IS DISTINCT FROM p_scope
                   OR v_existing.upload_registration->'declaration'->>'mime' IS DISTINCT FROM p_mime
                   OR v_existing.upload_registration->'declaration'->>'file_name' IS DISTINCT FROM p_file_name THEN
                    RAISE EXCEPTION 'dv1 register declaration conflict' USING ERRCODE = '23510';
                END IF;
                RETURN v_existing.object_key;
            END IF;
        END IF;
        PERFORM fn_dv1_quota_check_app(p_app_id);
        IF p_client_upload_id IS NOT NULL THEN
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
                 p_driver, p_namespace, p_container, p_locator)
            ON CONFLICT (client_upload_id) WHERE client_upload_id IS NOT NULL
            DO NOTHING
            RETURNING object_key INTO v_new_key;
            IF v_new_key IS NOT NULL THEN
                RETURN v_new_key;
            END IF;
            RAISE EXCEPTION 'dv1 register concurrent conflict' USING ERRCODE = '40001';
        ELSE
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
                 p_driver, p_namespace, p_container, p_locator)
            RETURNING object_key INTO v_new_key;
            RETURN v_new_key;
        END IF;
    END LOOP;
END;
$$;

