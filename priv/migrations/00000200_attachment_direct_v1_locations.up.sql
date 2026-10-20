-- 00000200 direct-v1 位置映射与 attempt 账本（step-4）
-- Contract: docs/architecture/attachment-direct-transfer-v1-protocol.md §1/§4/§6
-- 设计要点：
--   * 不改 attach_pending.session_state / attachment.lifecycle_state 既有枚举语义；
--     direct 会话另挂 direct_state（可空），与 session_state 的同步关系由 CHECK 冻结。
--   * 物理 key 与逻辑 key 分离：storage_physical_key IS NULL 表示"物理键 = 逻辑
--     path"（历史行兼容，reader 回落）；direct 行确认时必须非 NULL（受限函数保证）。
--   * attach_direct_attempt 是副作用账本（每 attempt 一行），业务真源仍在
--     attach_pending/attachment；不进只能删除的队列。
--   * 迁移号 200 起（197/198 已占，199 缓冲），与协议 §4 预约一致。
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

-- ---------------------------------------------------------------- 1. pending direct 子状态
ALTER TABLE attach_pending
    ADD COLUMN direct_state text;

ALTER TABLE attach_pending
    ADD CONSTRAINT ck_dv1_pending_direct_state CHECK (
        direct_state IS NULL OR direct_state IN
            ('RESERVED', 'DECLARED', 'GRANTED', 'FINALIZING',
             'CONFIRMED', 'FAILED', 'EXPIRED', 'CANCELLED'));

-- direct 会话必须声明 protocol=direct-v1；非 direct 会话 direct_state 恒 NULL。
ALTER TABLE attach_pending
    ADD CONSTRAINT ck_dv1_direct_protocol CHECK (
        (direct_state IS NULL AND coalesce(upload_registration->>'protocol', '') <> 'direct-v1')
        OR (direct_state IS NOT NULL AND upload_registration->>'protocol' = 'direct-v1'));

-- 与 session_state 的联动（协议 §1.2 同步列）。旧 V5 会话不受影响。
ALTER TABLE attach_pending
    ADD CONSTRAINT ck_dv1_direct_sync CHECK (
        direct_state IS NULL
        OR (direct_state IN ('RESERVED', 'DECLARED', 'GRANTED', 'FINALIZING')
            AND session_state = 'registered')
        OR (direct_state = 'CONFIRMED' AND session_state = 'confirmed')
        OR (direct_state = 'FAILED' AND session_state = 'rejected')
        OR (direct_state = 'EXPIRED' AND session_state = 'expired')
        OR (direct_state = 'CANCELLED'
            AND session_state IN ('rejected', 'expired', 'delete_intent', 'deleted')));

-- direct CONFIRMED 必须挂 winner attempt（与 ck_v5_session_link 同事务语义）。
ALTER TABLE attach_pending
    ADD CONSTRAINT ck_dv1_confirmed_winner CHECK (
        direct_state IS DISTINCT FROM 'CONFIRMED'
        OR confirmed_attachment_id IS NOT NULL);

-- ---------------------------------------------------------------- 2. attempt 账本
CREATE TABLE attach_direct_attempt (
    id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    object_key text NOT NULL,
    attempt_no integer NOT NULL,
    attempt_state text NOT NULL,
    -- 精确 source/target 在 DISPATCH 前持久化（协议 §1.3）
    source_key text NOT NULL,
    target_key text NOT NULL,
    target_driver text NOT NULL,
    target_namespace text NOT NULL,
    target_container text NOT NULL,
    profile_digest text NOT NULL,
    dispatch_cas_uuid uuid NOT NULL,
    dispatched_at timestamptz,
    proof_sha256 text,
    proof_size bigint,
    proof_received_at timestamptz,
    worker_receipt jsonb,
    last_error_code text,
    created_at timestamptz NOT NULL DEFAULT now(),
    updated_at timestamptz NOT NULL DEFAULT now(),
    CONSTRAINT fk_dv1_attempt_pending
        FOREIGN KEY (object_key) REFERENCES attach_pending(object_key) ON DELETE CASCADE,
    CONSTRAINT uk_dv1_attempt_no UNIQUE (object_key, attempt_no),
    CONSTRAINT ck_dv1_attempt_state CHECK (attempt_state IN
        ('CREATED', 'DISPATCHED', 'COPY_COMPLETED', 'PROOF_VERIFIED',
         'WINNER', 'UNKNOWN_QUARANTINED', 'GC_CLAIMED', 'REJECTED', 'ABANDONED')),
    -- 每 session 最大写 attempt 数 = 3（协议 §1.3 预算冻结）
    CONSTRAINT ck_dv1_attempt_no CHECK (attempt_no BETWEEN 1 AND 3),
    -- 客户端无 sealed 写：target 与 source 必须不同且 target 每次唯一
    CONSTRAINT ck_dv1_attempt_target_ne_source CHECK (target_key <> source_key),
    CONSTRAINT ck_dv1_attempt_target_shape CHECK (
        target_key LIKE 'direct-c/%' AND length(target_key) <= 512),
    CONSTRAINT ck_dv1_attempt_identity CHECK (
        target_driver IN ('garage')
        AND target_namespace <> '' AND target_container <> ''),
    CONSTRAINT ck_dv1_attempt_proof CHECK (
        (proof_sha256 IS NULL AND proof_size IS NULL AND proof_received_at IS NULL)
        OR (proof_sha256 ~ '^[0-9a-f]{64}$' AND proof_size >= 0 AND proof_received_at IS NOT NULL)),
    -- 只有明确成功响应才能 COPY_COMPLETED；超时/丢失一律 UNKNOWN_QUARANTINED
    CONSTRAINT ck_dv1_attempt_dispatched_ts CHECK (
        attempt_state IN ('CREATED') OR dispatched_at IS NOT NULL),
    -- proof 只出现在 COPY_COMPLETED 之后的态
    CONSTRAINT ck_dv1_attempt_proof_states CHECK (
        proof_sha256 IS NULL
        OR attempt_state IN ('COPY_COMPLETED', 'PROOF_VERIFIED', 'WINNER',
                             'UNKNOWN_QUARANTINED', 'GC_CLAIMED', 'REJECTED'))
);

CREATE INDEX i_dv1_attempt_state ON attach_direct_attempt (attempt_state, created_at);
CREATE INDEX i_dv1_attempt_target ON attach_direct_attempt (target_key)
    WHERE attempt_state IN ('DISPATCHED', 'COPY_COMPLETED', 'PROOF_VERIFIED',
                            'UNKNOWN_QUARANTINED');

COMMENT ON TABLE attach_direct_attempt IS
    'direct-v1 每次可能外部写入的唯一 attempt 账本；attempt_state 单跳推进，UNKNOWN_QUARANTINED 本轮无晋升边';

-- ---------------------------------------------------------------- 3. 物理 key 映射列
-- NULL = 物理键与逻辑 path 相同（历史行兼容）；direct 行确认时由受限函数写入非 NULL。
ALTER TABLE attachment
    ADD COLUMN storage_physical_key text,
    ADD CONSTRAINT ck_dv1_attachment_pkey_shape CHECK (
        storage_physical_key IS NULL OR (storage_physical_key <> '' AND length(storage_physical_key) <= 512));

ALTER TABLE enterprise_asset
    ADD COLUMN storage_physical_key text,
    ADD CONSTRAINT ck_dv1_ea_pkey_shape CHECK (
        storage_physical_key IS NULL OR (storage_physical_key <> '' AND length(storage_physical_key) <= 512));

ALTER TABLE attachment_object_deletion
    ADD COLUMN storage_physical_key text,
    ADD CONSTRAINT ck_dv1_aod_pkey_shape CHECK (
        storage_physical_key IS NULL OR (storage_physical_key <> '' AND length(storage_physical_key) <= 512));

ALTER TABLE enterprise_asset_delete_queue
    ADD COLUMN storage_physical_key text,
    ADD CONSTRAINT ck_dv1_ebdq_pkey_shape CHECK (
        storage_physical_key IS NULL OR (storage_physical_key <> '' AND length(storage_physical_key) <= 512));

-- 删除意图捕获强化：executable 状态的历史行四元组已由 ck_v5_aod_identity /
-- ck_v5_ebdq_identity 保证；direct 行在 intent 写入时必须带完整物理身份
-- （physical_key 非 NULL）——由下方捕获函数与 ck_dv1_aod_direct_identity 双保险。
ALTER TABLE attachment_object_deletion
    ADD CONSTRAINT ck_dv1_aod_direct_identity CHECK (
        storage_physical_key IS NULL OR (
            storage_driver IS NOT NULL AND storage_namespace IS NOT NULL
            AND storage_container IS NOT NULL AND storage_locator_sha256 ~ '^[0-9a-f]{64}$'));

-- ---------------------------------------------------------------- 4. 受限函数
-- 4.1 attempt 创建：锁 pending 行后原子分配 attempt_no（≤3 预算硬门）。
CREATE FUNCTION fn_dv1_attempt_create(p_key text, p_source_key text, p_target_key text,
                                      p_driver text, p_namespace text, p_container text,
                                      p_profile_digest text)
RETURNS integer LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_no integer; v_state text; v_direct text; v_uuid uuid := gen_random_uuid();
BEGIN
    PERFORM 1 FROM attach_pending WHERE object_key = p_key FOR UPDATE;
    SELECT session_state, direct_state INTO v_state, v_direct
        FROM attach_pending WHERE object_key = p_key;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'dv1 attempt unknown session' USING ERRCODE = 'P0002';
    END IF;
    -- 仅 GRANTED/FINALIZING 可创建新 attempt；FAILED/EXPIRED/CANCELLED/CONFIRMED 关闭。
    IF v_direct IS NULL OR v_direct NOT IN ('GRANTED', 'FINALIZING') THEN
        RAISE EXCEPTION 'dv1 attempt illegal session direct_state %', v_direct
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM attach_direct_attempt
               WHERE object_key = p_key AND attempt_state = 'WINNER') THEN
        RAISE EXCEPTION 'dv1 attempt winner already pinned' USING ERRCODE = '23510';
    END IF;
    SELECT coalesce(max(attempt_no), 0) + 1 INTO v_no
        FROM attach_direct_attempt WHERE object_key = p_key;
    IF v_no > 3 THEN
        RAISE EXCEPTION 'dv1 attempt budget exhausted' USING ERRCODE = '23514';
    END IF;
    INSERT INTO attach_direct_attempt
        (object_key, attempt_no, attempt_state, source_key, target_key,
         target_driver, target_namespace, target_container, profile_digest, dispatch_cas_uuid)
    VALUES (p_key, v_no, 'CREATED', p_source_key, p_target_key,
            p_driver, p_namespace, p_container, p_profile_digest, v_uuid);
    RETURN v_no;
END;
$$;

-- 4.2 attempt 状态单跳 CAS。合法转换白名单内置；调用方不能随意改态。
CREATE FUNCTION fn_dv1_attempt_transition(p_key text, p_attempt_no integer,
                                          p_from text, p_to text, p_error text DEFAULT NULL)
RETURNS boolean LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
BEGIN
    IF (p_from, p_to) NOT IN (
        ('CREATED', 'DISPATCHED'),
        ('CREATED', 'ABANDONED'),
        ('DISPATCHED', 'COPY_COMPLETED'),
        ('DISPATCHED', 'UNKNOWN_QUARANTINED'),
        ('COPY_COMPLETED', 'PROOF_VERIFIED'),
        ('COPY_COMPLETED', 'REJECTED'),
        ('PROOF_VERIFIED', 'WINNER'),
        ('PROOF_VERIFIED', 'REJECTED'),
        ('PROOF_VERIFIED', 'GC_CLAIMED')) THEN
        RAISE EXCEPTION 'dv1 attempt illegal transition % -> %', p_from, p_to
            USING ERRCODE = '23510';
    END IF;
    -- UNKNOWN_QUARANTINED 无晋升边（协议 §1.3）：任何出边都不在白名单，
    -- 这里再显式拒绝以防未来白名单扩张。
    IF p_from = 'UNKNOWN_QUARANTINED' THEN
        RAISE EXCEPTION 'dv1 unknown attempt has no promotion edge' USING ERRCODE = '23510';
    END IF;
    UPDATE attach_direct_attempt
        SET attempt_state = p_to, last_error_code = p_error, updated_at = now(),
            dispatched_at = CASE WHEN p_to = 'DISPATCHED' THEN now() ELSE dispatched_at END
        WHERE object_key = p_key AND attempt_no = p_attempt_no AND attempt_state = p_from;
    RETURN FOUND;
END;
$$;

-- 4.3 winner pin：PROOF_VERIFIED -> WINNER 单跳 + pending CONFIRMED 同事务。
--     attachment 行若已存在（confirm 流程先建行后 pin），原子落物理映射；
--     expected 复核（hash/size 与 declaration 一致）在调用方（step-6）持锁完成。
CREATE FUNCTION fn_dv1_winner_pin(p_key text, p_attempt_no integer,
                                  p_physical_key text, p_proof_sha256 text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a attach_direct_attempt%ROWTYPE; v_direct text;
BEGIN
    -- 锁序沿协议 §4：identity -> advisory(168) -> pending -> attempt -> attachment
    PERFORM pg_advisory_xact_lock(hashtextextended(p_key, 168));
    PERFORM 1 FROM attach_pending WHERE object_key = p_key FOR UPDATE;
    SELECT * INTO v_a FROM attach_direct_attempt
        WHERE object_key = p_key AND attempt_no = p_attempt_no FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'dv1 winner unknown attempt' USING ERRCODE = 'P0002';
    END IF;
    IF v_a.attempt_state <> 'PROOF_VERIFIED' THEN
        RAISE EXCEPTION 'dv1 winner illegal from %', v_a.attempt_state USING ERRCODE = '23510';
    END IF;
    IF v_a.proof_sha256 IS DISTINCT FROM p_proof_sha256 THEN
        RAISE EXCEPTION 'dv1 winner digest mismatch' USING ERRCODE = '23510';
    END IF;
    IF p_physical_key IS NULL OR p_physical_key = ''
            OR p_physical_key <> v_a.target_key THEN
        RAISE EXCEPTION 'dv1 winner physical key mismatch' USING ERRCODE = '23510';
    END IF;
    SELECT direct_state INTO v_direct FROM attach_pending WHERE object_key = p_key;
    IF v_direct <> 'FINALIZING' THEN
        RAISE EXCEPTION 'dv1 winner session not FINALIZING' USING ERRCODE = '23510';
    END IF;
    -- 与 GC claim 互斥：WINNER 先提交则该 attempt 的 GC claim 被状态门拒绝。
    UPDATE attach_direct_attempt
        SET attempt_state = 'WINNER', updated_at = now()
        WHERE object_key = p_key AND attempt_no = p_attempt_no
          AND attempt_state = 'PROOF_VERIFIED';
    IF NOT FOUND THEN
        RAISE EXCEPTION 'dv1 winner lost eligibility race' USING ERRCODE = '40001';
    END IF;
    -- 位置四元组沿用 pending 的 register 冻结值（locator 是 registry 位置
    -- 摘要，不是内容 sha256）；本函数只补物理键。
    UPDATE attachment SET
        storage_physical_key = p_physical_key,
        storage_driver = v_a.target_driver,
        storage_namespace = v_a.target_namespace,
        storage_container = v_a.target_container
        WHERE path = p_key AND storage_physical_key IS NULL;
    -- attempt proof 同时作为 V5 content receipt（guard 要求 pinned receipt；
    -- direct 的字节证据=attempt 账本 proof，语义强于旧单头 GET 校验）。
    UPDATE attach_pending SET
        upload_content = jsonb_build_object(
            'version', '1',
            'sha256', p_proof_sha256,
            'size_bytes', coalesce(v_a.proof_size, 0)::text,
            'mime', coalesce(
                (SELECT upload_registration->'declaration'->>'mime'
                 FROM attach_pending WHERE object_key = p_key),
                'application/octet-stream'))
        WHERE object_key = p_key AND upload_content IS NULL;
    -- confirm 流程必须已建 attachment 行（同事务）；pending 三字段一次跳转，
    -- 满足既有 fn_v5_pending_session_guard 与 ck_v5_session_link 两跳语义。
    UPDATE attach_pending SET
        direct_state = 'CONFIRMED', session_state = 'confirmed',
        confirmed_attachment_id = (
            SELECT id FROM attachment WHERE path = p_key ORDER BY id LIMIT 1)
        WHERE object_key = p_key AND direct_state = 'FINALIZING';
    IF NOT FOUND THEN
        RAISE EXCEPTION 'dv1 winner pending transition lost race' USING ERRCODE = '40001';
    END IF;
END;
$$;

-- 4.4 删除意图物理身份捕获（direct 行）：主行删除事务内先固化五元组。
--     现有 fn_v5_intent_ordinary_delete 捕获四元组；direct 行额外要求
--     physical_key 一并写入——新函数包装：direct attachment 必须带物理键。
CREATE FUNCTION fn_dv1_intent_capture_physical(p_key text)
RETURNS text LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a attachment%ROWTYPE;
BEGIN
    SELECT * INTO v_a FROM attachment WHERE path = p_key FOR UPDATE;
    IF NOT FOUND THEN RETURN NULL; END IF;
    IF v_a.storage_driver IS NULL THEN
        RAISE EXCEPTION 'dv1 intent legacy location has no direct identity'
            USING ERRCODE = '23510';
    END IF;
    RETURN coalesce(v_a.storage_physical_key, v_a.path);
END;
$$;

-- 4.5 claim 投影扩展（返回列变化超出 OR REPLACE 能力：OUT 行类型不同，
--     需 DROP 后重建；权限授予保持与迁移 179 相同）：
--     worker 取任务时携带物理键；历史行 NULL（worker 侧回落逻辑键）。
DROP FUNCTION IF EXISTS fn_v5_claim_ordinary(uuid);

CREATE FUNCTION fn_v5_claim_ordinary(p_token uuid)
RETURNS TABLE(out_path text, out_bucket text, out_storage_driver text,
              out_storage_namespace text, out_storage_container text,
              out_storage_locator_sha256 text, out_storage_physical_key text,
              out_attempts integer)
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
BEGIN
    -- plpgsql UPDATE..RETURNING 必须有去向（RETURN QUERY 惯用法；修复 179 同型缺陷）
    RETURN QUERY
    UPDATE attachment_object_deletion q
        SET job_state = 'leased', lease_token = p_token,
            lease_until = clock_timestamp() + interval '120 seconds',
            attempts = q.attempts + 1, last_error_code = NULL
        WHERE q.attempts < 8
          AND q.job_state IN ('ready', 'retry_wait')
          AND q.next_retry_at <= clock_timestamp()
          AND q.path = (SELECT path FROM attachment_object_deletion
                        WHERE attempts < 8
                          AND job_state IN ('ready', 'retry_wait')
                          AND next_retry_at <= clock_timestamp()
                        ORDER BY next_retry_at, path
                        FOR UPDATE SKIP LOCKED LIMIT 1)
        RETURNING q.path, q.bucket, q.storage_driver, q.storage_namespace,
                  q.storage_container, q.storage_locator_sha256,
                  q.storage_physical_key, q.attempts;
END;
$$;
ALTER FUNCTION fn_v5_claim_ordinary(uuid) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_claim_ordinary(uuid) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_claim_ordinary(uuid) TO v5_worker;

DROP FUNCTION IF EXISTS fn_v5_claim_eb(uuid);

CREATE FUNCTION fn_v5_claim_eb(p_token uuid)
RETURNS TABLE(out_organization_id bigint, out_workspace_id bigint, out_asset_id bigint,
              out_object_key text, out_storage_driver text, out_storage_namespace text,
              out_storage_container text, out_storage_locator_sha256 text,
              out_storage_physical_key text, out_attempts integer)
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
BEGIN
    -- 同上：RETURN QUERY 惯用法（修复 179 同型缺陷）
    RETURN QUERY
    UPDATE enterprise_asset_delete_queue q
        SET job_state = 'leased', lease_token = p_token,
            lease_until = clock_timestamp() + interval '120 seconds',
            attempts = q.attempts + 1, last_error_code = NULL
        WHERE q.attempts < 8
          AND q.job_state IN ('ready', 'retry_wait')
          AND q.next_retry_at <= clock_timestamp()
          AND (q.organization_id, q.workspace_id, q.asset_id) = (
              SELECT organization_id, workspace_id, asset_id
                  FROM enterprise_asset_delete_queue
                  WHERE attempts < 8
                    AND job_state IN ('ready', 'retry_wait')
                    AND next_retry_at <= clock_timestamp()
                  ORDER BY next_retry_at, organization_id, workspace_id, asset_id
                  FOR UPDATE SKIP LOCKED LIMIT 1)
        RETURNING q.organization_id, q.workspace_id, q.asset_id, q.object_key,
                  q.storage_driver, q.storage_namespace, q.storage_container,
                  q.storage_locator_sha256, q.storage_physical_key, q.attempts;
END;
$$;
ALTER FUNCTION fn_v5_claim_eb(uuid) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_claim_eb(uuid) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_claim_eb(uuid) TO v5_worker;
