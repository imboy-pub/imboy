-- 00000275 direct-v1 EB 企业域主体会话（V3-C04：企业与客服后端全认证面 S3-only）
-- 五类认证面（Member/Seat/Visitor 企业域）正文切 direct-v1：subject kind =
-- member|seat|contact（登录 user uid 或访客 contact），org/workspace/
-- conversation/message 声明入 registration（幂等比较全量，跨空间不复用）。
-- 与 00000203（application 主体）同模式；既有 fn_dv1_register（个人）、
-- fn_dv1_register_application（Admin 头像）零改动。
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

-- ---------------------------------------------------------------- register
-- EB direct register：无写能力（协议 §1.2 RESERVED / R08）。
-- p_subject_kind ∈ ('member','seat','contact')；member/seat 的 p_principal
-- 是登录 user uid（creator_user_id 落值）；contact 的 principal 是访客
-- contact id（creator_user_id=0，与 application 主体靠 kind 隔离）。
-- client_upload_id 幂等（T02）：同主体同 id 同声明 → 返回原 session；
-- 同 id 异主体/异声明 → 23510（T03，跨 org/workspace/conversation 不得复用）。
CREATE FUNCTION fn_dv1_register_eb(p_key text, p_bucket text, p_scope text,
                                   p_subject_kind text, p_principal text,
                                   p_org_id text, p_workspace_id text,
                                   p_conversation_id text, p_message_id text,
                                   p_client_upload_id uuid, p_mime text,
                                   p_file_name text, p_driver text,
                                   p_namespace text, p_container text,
                                   p_locator text)
RETURNS text LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_existing attach_pending%ROWTYPE;
        v_creator bigint;
BEGIN
    IF p_subject_kind NOT IN ('member', 'seat', 'contact') THEN
        RAISE EXCEPTION 'dv1 register eb unknown subject kind %', p_subject_kind
            USING ERRCODE = '23514';
    END IF;
    v_creator := CASE WHEN p_subject_kind = 'contact' THEN 0
                      ELSE p_principal::bigint END;
    PERFORM pg_advisory_xact_lock(hashtextextended(p_key, 168));
    -- EB 主体并发 live direct 会话配额（与 user/app 同预算口径）
    PERFORM fn_dv1_quota_check_eb(p_subject_kind, p_principal);
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
            IF v_existing.upload_registration->'subject'->>'kind' IS DISTINCT FROM p_subject_kind
               OR v_existing.upload_registration->'subject'->>'principal' IS DISTINCT FROM p_principal
               OR v_existing.upload_registration->'declaration'->>'org_id' IS DISTINCT FROM p_org_id
               OR v_existing.upload_registration->'declaration'->>'workspace_id' IS DISTINCT FROM p_workspace_id
               OR v_existing.upload_registration->'declaration'->>'conversation_id' IS DISTINCT FROM p_conversation_id
               OR v_existing.upload_registration->'declaration'->>'message_id' IS DISTINCT FROM p_message_id
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
        (p_key, p_bucket, p_scope, v_creator, p_client_upload_id, 'registered',
         'RESERVED',
         jsonb_build_object(
            'protocol', 'direct-v1',
            'subject', jsonb_build_object(
                'kind', p_subject_kind, 'principal', p_principal),
            'declaration', jsonb_build_object(
                'scope', p_scope,
                'scope_ref', '',
                'org_id', p_org_id,
                'workspace_id', p_workspace_id,
                'conversation_id', p_conversation_id,
                'message_id', p_message_id,
                'mime', p_mime,
                'file_name', p_file_name)),
         now() + interval '7200 seconds',
         p_driver, p_namespace, p_container, p_locator);
    RETURN p_key;
END;
$$;

-- EB 主体配额：每 (kind, principal) 并发 live direct 会话 ≤4
-- （按 subject kind+principal 精确匹配，contact 与 application 互不干扰）
CREATE FUNCTION fn_dv1_quota_check_eb(p_kind text, p_principal text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_live integer;
BEGIN
    SELECT count(*) INTO v_live FROM attach_pending
        WHERE upload_registration->'subject'->>'kind' = p_kind
          AND upload_registration->'subject'->>'principal' = p_principal
          AND direct_state IN ('RESERVED', 'DECLARED', 'GRANTED', 'FINALIZING')
          AND (session_deadline IS NULL OR session_deadline > now());
    IF v_live >= 4 THEN
        RAISE EXCEPTION 'dv1 session quota exceeded (%)', v_live USING ERRCODE = '23514';
    END IF;
END;
$$;
