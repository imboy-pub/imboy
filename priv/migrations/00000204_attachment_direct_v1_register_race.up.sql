-- step-7a REAL_PG_RACE 实证缺陷修复：同 client_upload_id 并发注册可产生多个
-- 会话（幂等仅靠 SELECT FOR UPDATE；advisory 锁按 p_key，而每次注册 p_key
-- 随机 → 并发首注册互不串行；client_upload_id 无唯一约束兜底）。
-- 修复：1) registered/confirmed 会话的 client_upload_id 部分唯一索引；
--       2) 按 client_upload_id 追加 advisory 锁（同 uuid 并发串行化）；
--       3) INSERT ON CONFLICT DO NOTHING，冲突方回落幂等恢复（同声明复用
--          现有会话；异声明/legacy 占用 → 23510；配额仅在新建时检查）。

CREATE UNIQUE INDEX IF NOT EXISTS uk_attach_pending_client_upload_id
    ON attach_pending (client_upload_id)
    WHERE client_upload_id IS NOT NULL
      AND session_state IN ('registered', 'confirmed');

CREATE OR REPLACE FUNCTION fn_dv1_register(p_key text, p_bucket text, p_scope text,
                                p_scope_ref text, p_uid bigint,
                                p_client_upload_id uuid, p_mime text,
                                p_file_name text, p_driver text,
                                p_namespace text, p_container text,
                                p_locator text)
RETURNS text LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_existing attach_pending%ROWTYPE; v_new_key text;
BEGIN
    -- 同 client_upload_id 并发注册先串行化（201 的锁按 p_key，随机键互不互斥）
    IF p_client_upload_id IS NOT NULL THEN
        PERFORM pg_advisory_xact_lock(hashtextextended(p_client_upload_id::text, 169));
    END IF;
    PERFORM pg_advisory_xact_lock(hashtextextended(p_key, 168));
    LOOP
        -- 幂等恢复：同 client_upload_id 的 live direct 会话（语义同 201）
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
        -- 每 subject 并发 live direct 会话配额（step-7 / T46；仅新建会话消耗）
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
