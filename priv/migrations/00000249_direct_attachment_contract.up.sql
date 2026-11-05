CREATE OR REPLACE FUNCTION fn_dv1_register(p_key text, p_bucket text, p_scope text,
                                p_scope_ref text, p_uid bigint,
                                p_client_upload_id uuid, p_mime text,
                                p_file_name text, p_driver text,
                                p_namespace text, p_container text,
                                p_locator text, p_cipher text, p_purpose text, p_anchor text)
RETURNS text LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_existing attach_pending%ROWTYPE; v_new_key text;
BEGIN
    IF p_cipher IS NOT NULL AND p_cipher <> 'AES-256-GCM' THEN
        RAISE EXCEPTION 'invalid cipher' USING ERRCODE = '22023';
    END IF;
    IF p_purpose IS NULL OR p_purpose NOT IN ('attachment','message') OR
       (p_scope = 'group' AND p_purpose = 'message' AND
        (p_anchor IS NULL OR octet_length(p_anchor) NOT BETWEEN 1 AND 128)) OR
       ((p_scope <> 'group' OR p_purpose <> 'message') AND p_anchor IS NOT NULL) THEN
        RAISE EXCEPTION 'invalid attachment purpose or anchor' USING ERRCODE = '22023';
    END IF;
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
                        USING ERRCODE = 'P1002';
                END IF;
                IF v_existing.session_state NOT IN ('registered', 'confirmed') THEN
                    RAISE EXCEPTION 'dv1 register client_upload_id belongs to terminal session' USING ERRCODE = 'P1001';
                END IF;
                IF v_existing.creator_user_id <> p_uid
                   OR v_existing.upload_registration->'declaration'->>'mime' IS DISTINCT FROM p_mime
                   OR v_existing.upload_registration->'declaration'->>'file_name' IS DISTINCT FROM p_file_name
                   OR v_existing.upload_registration->'declaration'->>'scope' IS DISTINCT FROM p_scope
                   OR v_existing.upload_registration->'declaration'->>'scope_ref'
                       IS DISTINCT FROM coalesce(p_scope_ref, '')
                   OR v_existing.upload_registration->'declaration'->>'cipher' IS DISTINCT FROM p_cipher
                   OR coalesce(v_existing.upload_registration->'declaration'->>'purpose', 'attachment') IS DISTINCT FROM p_purpose
                   OR v_existing.upload_registration->'declaration'->>'anchor_msg_id' IS DISTINCT FROM p_anchor THEN
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
                        'file_name', p_file_name,
                        'cipher', p_cipher,
                        'purpose', p_purpose,
                        'anchor_msg_id', p_anchor)),
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
                        'file_name', p_file_name,
                        'cipher', p_cipher,
                        'purpose', p_purpose,
                        'anchor_msg_id', p_anchor)),
                 now() + interval '7200 seconds',
                 p_driver, p_namespace, p_container, p_locator)
            RETURNING object_key INTO v_new_key;
            RETURN v_new_key;
        END IF;
    END LOOP;
END;
$$;

CREATE OR REPLACE FUNCTION fn_dv1_register(p_key text, p_bucket text, p_scope text,
    p_scope_ref text, p_uid bigint, p_client_upload_id uuid, p_mime text,
    p_file_name text, p_driver text, p_namespace text, p_container text, p_locator text)
RETURNS text LANGUAGE sql SECURITY DEFINER SET search_path = public AS $$
    SELECT fn_dv1_register(p_key, p_bucket, p_scope, p_scope_ref, p_uid, p_client_upload_id,
                           p_mime, p_file_name, p_driver, p_namespace, p_container, p_locator,
                           NULL::text, 'attachment'::text, NULL::text);
$$;

CREATE FUNCTION fn_dv1_attachment_contract_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path = public AS $$
DECLARE v_a attachment%ROWTYPE; v_decl jsonb;
BEGIN
    IF OLD.direct_state IS NOT NULL AND OLD.direct_expected IS NOT NULL
       AND NEW.direct_expected IS DISTINCT FROM OLD.direct_expected THEN
        RAISE EXCEPTION 'direct expected immutable' USING ERRCODE = '23510';
    END IF;
    IF OLD.direct_state IS NOT NULL AND OLD.session_state = 'registered'
       AND NEW.session_state = 'confirmed' THEN
        v_decl := OLD.upload_registration->'declaration';
        SELECT * INTO v_a FROM attachment WHERE id = NEW.confirmed_attachment_id;
        IF NOT FOUND OR v_a.cipher IS DISTINCT FROM v_decl->>'cipher'
           OR v_a.anchor_msg_id IS DISTINCT FROM v_decl->>'anchor_msg_id' THEN
            RAISE EXCEPTION 'direct attachment contract mismatch' USING ERRCODE = '23510';
        END IF;
    END IF;
    RETURN NEW;
END;
$$;
CREATE TRIGGER trg_dv1_attachment_contract_guard BEFORE UPDATE ON attach_pending
FOR EACH ROW EXECUTE FUNCTION fn_dv1_attachment_contract_guard();

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
                        USING ERRCODE = 'P1002';
                END IF;
                IF v_existing.session_state NOT IN ('registered', 'confirmed') THEN
                    RAISE EXCEPTION 'dv1 register client_upload_id belongs to terminal session' USING ERRCODE = 'P1001';
                END IF;
                -- 主体三元组（kind/principal/org_id）+ 声明（mime/file_name）
                -- 全等才复用；org_id 异值 = 同 app 换 org，按异声明拒绝。
                IF v_existing.upload_registration->'subject'->>'kind' IS DISTINCT FROM 'application'
                   OR (CASE WHEN p_org_id = '0'
                       AND p_app_id ~* '^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$'
                       AND (v_existing.upload_registration->'subject'->>'principal') ~* '^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$'
                       THEN (v_existing.upload_registration->'subject'->>'principal')::uuid IS DISTINCT FROM p_app_id::uuid
                       ELSE v_existing.upload_registration->'subject'->>'principal' IS DISTINCT FROM p_app_id END)
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

REVOKE ALL ON FUNCTION fn_dv1_register(text,text,text,text,bigint,uuid,text,text,text,text,text,text,text,text,text) FROM PUBLIC;
REVOKE ALL ON FUNCTION fn_dv1_register(text,text,text,text,bigint,uuid,text,text,text,text,text,text) FROM PUBLIC;
REVOKE ALL ON FUNCTION fn_dv1_register_application(text,text,text,text,text,uuid,text,text,text,text,text,text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_dv1_register(text,text,text,text,bigint,uuid,text,text,text,text,text,text,text,text,text) TO v5_app_runtime;
GRANT EXECUTE ON FUNCTION fn_dv1_register(text,text,text,text,bigint,uuid,text,text,text,text,text,text) TO v5_app_runtime;
GRANT EXECUTE ON FUNCTION fn_dv1_register_application(text,text,text,text,text,uuid,text,text,text,text,text,text) TO v5_app_runtime;
