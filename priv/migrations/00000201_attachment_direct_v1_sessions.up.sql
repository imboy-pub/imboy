-- 00000201 direct-v1 会话受限函数（step-6）
-- Contract: docs/architecture/attachment-direct-transfer-v1-protocol.md §1/§4
-- direct_state 单跳推进全部经本迁移的受限函数；expected 冻结在独立列
-- direct_expected（upload_registration 受 V5 guard 不可变，V5 语义不破）。
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

-- expected 字节声明（declare 冻结后不可变；CONFIRMED 前与 proof 复核）。
ALTER TABLE attach_pending
    ADD COLUMN direct_expected jsonb,
    ADD CONSTRAINT ck_dv1_expected_shape CHECK (
        direct_expected IS NULL OR (
            jsonb_typeof(direct_expected) = 'object'
            AND direct_expected ? 'sha256'
            AND direct_expected->>'sha256' ~ '^[0-9a-f]{64}$'
            AND direct_expected ? 'size_bytes'
            AND direct_expected->>'size_bytes' ~ '^(0|[1-9][0-9]*)$'
            AND direct_expected ? 'mime'
            AND length(direct_expected->>'mime') > 0
            AND coalesce(direct_expected->>'version','') ~ '^[1-9][0-9]*$'));

-- ---------------------------------------------------------------- register
-- direct register：无写能力（协议 §1.2 RESERVED / R08）。
-- client_upload_id 幂等（T02）：同 uid 同 id 同声明 → 返回原 session；
-- 同 id 异声明 → 23510（T03，不换 grant）。key 终身不复用沿 V5 触发器。
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
        RAISE EXCEPTION 'dv1 declare unknown session' USING ERRCODE = 'P0002';
    END IF;
    IF v_deadline IS NOT NULL AND v_deadline < now() THEN
        RAISE EXCEPTION 'dv1 declare session deadline exceeded' USING ERRCODE = '23510';
    END IF;
    IF v_direct NOT IN ('RESERVED', 'DECLARED') THEN
        RAISE EXCEPTION 'dv1 declare illegal from %', v_direct USING ERRCODE = '23510';
    END IF;
    IF v_existing IS NOT NULL THEN
        IF v_existing->>'sha256' = p_sha256
           AND v_existing->>'size_bytes' = p_size::text
           AND v_existing->>'mime' = p_mime THEN
            RETURN; -- 幂等（T02）
        END IF;
        RAISE EXCEPTION 'dv1 declare expected conflict' USING ERRCODE = '23510';
    END IF;
    UPDATE attach_pending SET
        direct_expected = jsonb_build_object(
            'version', '1', 'sha256', p_sha256,
            'size_bytes', p_size::text, 'mime', p_mime),
        direct_state = CASE WHEN v_direct = 'RESERVED' THEN 'DECLARED' ELSE v_direct END
        WHERE object_key = p_key;
END;
$$;

-- ---------------------------------------------------------------- grant
-- DECLARED→GRANTED + 第一个 attempt（target 在发包前持久化，I05/I12）。
-- 返回 attempt_no；staging URL 签发在事务提交后（I02：COMMIT 先于签名）。
CREATE FUNCTION fn_dv1_grant(p_key text, p_staging_key text, p_target_key text,
                             p_driver text, p_namespace text, p_container text,
                             p_profile_digest text)
RETURNS integer LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_direct text; v_no integer; v_deadline timestamptz;
BEGIN
    PERFORM pg_advisory_xact_lock(hashtextextended(p_key, 168));
    SELECT direct_state, session_deadline INTO v_direct, v_deadline
        FROM attach_pending WHERE object_key = p_key FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'dv1 grant unknown session' USING ERRCODE = 'P0002';
    END IF;
    IF v_deadline IS NOT NULL AND v_deadline < now() THEN
        RAISE EXCEPTION 'dv1 grant session deadline exceeded' USING ERRCODE = '23510';
    END IF;
    IF v_direct <> 'DECLARED' THEN
        RAISE EXCEPTION 'dv1 grant illegal from %', v_direct USING ERRCODE = '23510';
    END IF;
    UPDATE attach_pending SET direct_state = 'GRANTED'
        WHERE object_key = p_key AND direct_state = 'DECLARED';
    SELECT fn_dv1_attempt_create(p_key, p_staging_key, p_target_key,
                                 p_driver, p_namespace, p_container, p_profile_digest)
        INTO v_no;
    RETURN v_no;
END;
$$;

-- ---------------------------------------------------------------- finalize
-- dispatch 前持久化（GRANTED/FINALIZING → FINALIZING；attempt 必须仍 CREATED）。
CREATE FUNCTION fn_dv1_begin_finalize(p_key text, p_attempt_no integer)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_direct text;
BEGIN
    PERFORM pg_advisory_xact_lock(hashtextextended(p_key, 168));
    SELECT direct_state INTO v_direct FROM attach_pending
        WHERE object_key = p_key FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'dv1 finalize unknown session' USING ERRCODE = 'P0002';
    END IF;
    IF v_direct NOT IN ('GRANTED', 'FINALIZING') THEN
        RAISE EXCEPTION 'dv1 finalize illegal from %', v_direct USING ERRCODE = '23510';
    END IF;
    IF v_direct = 'GRANTED' THEN
        UPDATE attach_pending SET direct_state = 'FINALIZING'
            WHERE object_key = p_key AND direct_state = 'GRANTED';
    END IF;
    -- attempt 存在且仍 CREATED（DISPATCHED 已发过 → 拒绝重复 dispatch，每
    -- attempt 恰一次写授权）；P0002 透传。
    PERFORM 1 FROM attach_direct_attempt
        WHERE object_key = p_key AND attempt_no = p_attempt_no
          AND attempt_state = 'CREATED' FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'dv1 finalize attempt not dispatchable' USING ERRCODE = '23510';
    END IF;
END;
$$;

-- FAILED（预算耗尽/不可恢复）：GRANTED/FINALIZING → FAILED + rejected 两跳。
-- staging/unknown candidate 的清理账归 step-7（此处只做状态收敛）。
CREATE FUNCTION fn_dv1_fail(p_key text, p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
BEGIN
    PERFORM pg_advisory_xact_lock(hashtextextended(p_key, 168));
    -- 单语句双跳（同 fn_dv1_cancel：ck_dv1_direct_sync 不接受中间态）
    UPDATE attach_pending SET
        direct_state = 'FAILED', session_state = 'rejected'
        WHERE object_key = p_key
          AND direct_state IN ('GRANTED', 'FINALIZING')
          AND session_state = 'registered';
    IF NOT FOUND THEN
        RAISE EXCEPTION 'dv1 fail illegal state' USING ERRCODE = '23510';
    END IF;
END;
$$;

-- direct 会话 reconcile 统计（恢复入口：FINALIZING 停留会话与其 attempt 账本）。
CREATE FUNCTION fn_dv1_reconcile_list(p_limit integer)
RETURNS TABLE(out_object_key text, out_direct_state text,
              out_attempt_no integer, out_attempt_state text,
              out_dispatched_at timestamptz, out_session_deadline timestamptz)
LANGUAGE sql SECURITY DEFINER SET search_path = public AS $$
    SELECT p.object_key, p.direct_state, a.attempt_no, a.attempt_state,
           a.dispatched_at, p.session_deadline
        FROM attach_pending p
        JOIN attach_direct_attempt a ON a.object_key = p.object_key
        WHERE p.direct_state = 'FINALIZING'
          AND a.attempt_state IN ('DISPATCHED', 'COPY_COMPLETED', 'PROOF_VERIFIED')
        ORDER BY p.object_key, a.attempt_no
        LIMIT least(p_limit, 500)
$$;
