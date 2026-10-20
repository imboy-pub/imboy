-- 00000205 direct-v1 register 竞态修复第二辑（code review P2）
-- 两项对称补齐（204 只修了 user 版，且幂等比较漏比声明成员）：
--   1) fn_dv1_register_application 补 204 三件套：client_upload_id 的
--      advisory 锁(169) + INSERT ON CONFLICT DO NOTHING + 冲突回落幂等
--      恢复。此前：同 uuid 并发注册撞 204 的全表唯一索引裸露 23505，
--      Admin 头像域幂等合同（T02）失效（fail-closed 但不可恢复复用）。
--   2) 幂等恢复的声明比较补全：
--      user 版（fn_dv1_register）补 scope_ref——同 id 换群重传不再静默
--      复用旧会话（T03 "同 id 异声明拒绝"字面合同）；
--      application 版（fn_dv1_register_application）补 org_id——同 app
--      换 org 同理拒绝。
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

-- ------------------------------------------------ fn_dv1_register（204 版 + scope_ref 比较）
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
            -- 输给并发胜者：uuid 锁已持，下一轮循环走幂等恢复
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

-- ------------------------------------------------ fn_dv1_register_application（203 版 + 204 三件套 + org_id 比较）
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
                  AND session_state IN ('registered', 'confirmed')
                ORDER BY created_at DESC LIMIT 1 FOR UPDATE;
            IF FOUND THEN
                IF v_existing.direct_state IS NULL THEN
                    RAISE EXCEPTION 'dv1 register client_upload_id belongs to legacy session'
                        USING ERRCODE = '23510';
                END IF;
                -- 主体三元组（kind/principal/org_id）+ 声明（mime/file_name）
                -- 全等才复用；org_id 异值 = 同 app 换 org，按异声明拒绝。
                IF v_existing.upload_registration->'subject'->>'kind' IS DISTINCT FROM 'application'
                   OR v_existing.upload_registration->'subject'->>'principal' IS DISTINCT FROM p_app_id
                   OR v_existing.upload_registration->'subject'->>'org_id' IS DISTINCT FROM p_org_id
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
