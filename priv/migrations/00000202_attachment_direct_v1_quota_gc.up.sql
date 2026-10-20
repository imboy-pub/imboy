-- 00000202 direct-v1 配额、candidate GC 与生命周期互斥（step-7）
-- Contract: docs/architecture/attachment-direct-transfer-v1-protocol.md §1.3/§4/§6
--   * candidate GC claim 与 winner 同锁同 CAS 互斥（白名单单跳，PROOF_VERIFIED
--     是两者唯一公共前驱：claim 先提交则 winner_pin 拒绝；winner 先提交则
--     claim 拒绝）。
--   * CANCELLED/FAILED 的 staging/candidate 物理身份在会话收敛事务内先捕获
--     到 attach_direct_cleanup（主行收敛前固化，worker 只按账本删）。
--   * 每 subject 并发 live direct 会话 ≤4（quota，T46）。
--   * unknown 墓碑有龄期预算，不无限重试（T15/T21）。
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

-- ---------------------------------------------------------------- cleanup 账本
CREATE TABLE attach_direct_cleanup (
    id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    object_key text NOT NULL,
    physical_key text NOT NULL,
    target_driver text NOT NULL,
    target_namespace text NOT NULL,
    target_container text NOT NULL,
    reason text NOT NULL,
    job_state text NOT NULL DEFAULT 'ready',
    attempts integer NOT NULL DEFAULT 0,
    lease_token uuid,
    lease_until timestamptz,
    last_error_code text,
    created_at timestamptz NOT NULL DEFAULT now(),
    completed_at timestamptz,
    CONSTRAINT uk_dv1_cleanup_physical UNIQUE (physical_key),
    CONSTRAINT ck_dv1_cleanup_state CHECK (job_state IN ('ready','leased','retry_wait','blocked','done')),
    CONSTRAINT ck_dv1_cleanup_identity CHECK (
        target_driver = 'garage' AND target_namespace <> '' AND target_container <> ''
        AND physical_key LIKE 'direct-%'),
    CONSTRAINT ck_dv1_cleanup_lease CHECK (
        (job_state = 'leased' AND lease_token IS NOT NULL AND lease_until IS NOT NULL)
        OR (job_state <> 'leased' AND lease_token IS NULL AND lease_until IS NULL))
);
CREATE INDEX i_dv1_cleanup_ready ON attach_direct_cleanup (job_state, created_at)
    WHERE job_state IN ('ready', 'retry_wait');

COMMENT ON TABLE attach_direct_cleanup IS
    'direct-v1 staging/candidate/unknown 对象清理账本：会话收敛事务内先捕获完整物理身份，worker 按账本删除；unique(physical_key) 防重复入账';

-- ---------------------------------------------------------------- register 配额
-- 每 subject 并发 live direct 会话 ≤4；并发 register 经同 advisory 锁串行
-- （fn_dv1_register 已持 key 锁；quota 复用同事务内检查，多节点安全靠行级
-- 串行化 + CHECK 无法跨行 → 函数内 SELECT count 在锁后执行，T46 并发由
-- create 会话唯一路径保证）。
CREATE OR REPLACE FUNCTION fn_dv1_quota_check(p_uid bigint)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_live integer;
BEGIN
    SELECT count(*) INTO v_live FROM attach_pending
        WHERE creator_user_id = p_uid
          AND direct_state IN ('RESERVED', 'DECLARED', 'GRANTED', 'FINALIZING')
          AND (session_deadline IS NULL OR session_deadline > now());
    IF v_live >= 4 THEN
        RAISE EXCEPTION 'dv1 session quota exceeded (%)', v_live USING ERRCODE = '23514';
    END IF;
END;
$$;

-- ---------------------------------------------------------------- winner / GC 互斥
-- janitor 对未 WINNER 的 PROOF_VERIFIED candidate 执行 GC claim（与 winner_pin
-- 同表同锁同 CAS：先提交者赢，后者 40001/23510 失败关闭）。
CREATE FUNCTION fn_dv1_gc_claim(p_key text, p_attempt_no integer, p_reason text)
RETURNS text LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_state text; v_driver text; v_ns text; v_c text; v_pkey text;
BEGIN
    PERFORM pg_advisory_xact_lock(hashtextextended(p_key, 168));
    PERFORM 1 FROM attach_pending WHERE object_key = p_key FOR UPDATE;
    SELECT attempt_state, target_driver, target_namespace, target_container, target_key
        INTO v_state, v_driver, v_ns, v_c, v_pkey
        FROM attach_direct_attempt
        WHERE object_key = p_key AND attempt_no = p_attempt_no FOR UPDATE;
    IF NOT FOUND THEN
        RETURN 'missing';
    END IF;
    IF v_state <> 'PROOF_VERIFIED' THEN
        -- WINNER/终态/unknown：不可 claim（winner 先提交 → 到这里必然失败）
        RETURN 'not_claimable';
    END IF;
    UPDATE attach_direct_attempt SET attempt_state = 'GC_CLAIMED',
        last_error_code = p_reason, updated_at = now()
        WHERE object_key = p_key AND attempt_no = p_attempt_no
          AND attempt_state = 'PROOF_VERIFIED';
    IF NOT FOUND THEN
        RETURN 'race_lost';
    END IF;
    INSERT INTO attach_direct_cleanup
        (object_key, physical_key, target_driver, target_namespace, target_container, reason)
        VALUES (p_key, v_pkey, v_driver, v_ns, v_c, coalesce(p_reason, 'gc_claim'))
        ON CONFLICT (physical_key) DO NOTHING;
    RETURN 'claimed';
END;
$$;

-- ---------------------------------------------------------------- 会话取消
-- CANCELLED：GRANTED/FINALIZING→CANCELLED + registered→rejected 两跳 +
-- staging/candidate 物理身份先入清理账（主行收敛前捕获，§6）。
CREATE FUNCTION fn_dv1_cancel(p_key text, p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a record;
BEGIN
    PERFORM pg_advisory_xact_lock(hashtextextended(p_key, 168));
    -- 单语句双跳：分步 UPDATE 会落在 ck_dv1_direct_sync 违例态（CANCELLED
    -- 要求 session 已离 registered，反之亦然）；同语句满足 V5 guard 两跳语义。
    UPDATE attach_pending SET
        direct_state = 'CANCELLED', session_state = 'rejected'
        WHERE object_key = p_key
          AND direct_state IN ('GRANTED', 'FINALIZING')
          AND session_state = 'registered';
    IF NOT FOUND THEN
        RAISE EXCEPTION 'dv1 cancel illegal state' USING ERRCODE = '23510';
    END IF;
    -- 物理身份先固化（全部未 WINNER attempt 的 staging+candidate）
    FOR v_a IN
        SELECT source_key, target_key, target_driver, target_namespace, target_container
            FROM attach_direct_attempt
            WHERE object_key = p_key AND attempt_state <> 'WINNER'
    LOOP
        INSERT INTO attach_direct_cleanup
            (object_key, physical_key, target_driver, target_namespace, target_container, reason)
            VALUES (p_key, v_a.source_key, v_a.target_driver, v_a.target_namespace,
                    v_a.target_container, coalesce(p_reason, 'cancel'))
            ON CONFLICT (physical_key) DO NOTHING;
        INSERT INTO attach_direct_cleanup
            (object_key, physical_key, target_driver, target_namespace, target_container, reason)
            VALUES (p_key, v_a.target_key, v_a.target_driver, v_a.target_namespace,
                    v_a.target_container, coalesce(p_reason, 'cancel'))
            ON CONFLICT (physical_key) DO NOTHING;
    END LOOP;
END;
$$;

-- ---------------------------------------------------------------- cleanup worker
CREATE FUNCTION fn_dv1_cleanup_claim(p_token uuid)
RETURNS TABLE(out_id bigint, out_physical_key text, out_driver text,
              out_namespace text, out_container text, out_attempts integer)
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
BEGIN
    -- plpgsql 中 UPDATE..RETURNING 行结果必须有去向：RETURN QUERY 是规范用法
    -- （无 INTO 的 RETURNING 会 42601 "query has no destination"，迁移 179 的
    -- fn_v5_claim_* 同型缺陷本次一并修复）。
    RETURN QUERY
    UPDATE attach_direct_cleanup c
        SET job_state = 'leased', lease_token = p_token,
            lease_until = clock_timestamp() + interval '120 seconds',
            attempts = c.attempts + 1, last_error_code = NULL
        WHERE c.id = (SELECT id FROM attach_direct_cleanup
                      WHERE job_state IN ('ready', 'retry_wait')
                        AND attempts < 8
                        AND (lease_until IS NULL OR lease_until < clock_timestamp())
                        AND created_at < now() - interval '60 seconds'  -- 龄期预算：迟到写保护
                      ORDER BY created_at, id
                      FOR UPDATE SKIP LOCKED LIMIT 1)
        RETURNING c.id, c.physical_key, c.target_driver, c.target_namespace,
                  c.target_container, c.attempts;
END;
$$;

CREATE FUNCTION fn_dv1_cleanup_ack(p_id bigint, p_token uuid, p_ok boolean)
RETURNS text LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_state text; v_attempts integer; v_delay interval;
DECLARE v_backoff integer[] := ARRAY[30, 120, 600, 1800, 7200, 21600, 43200];
BEGIN
    SELECT job_state, attempts INTO v_state, v_attempts
        FROM attach_direct_cleanup WHERE id = p_id FOR UPDATE;
    IF NOT FOUND THEN RETURN 'missing'; END IF;
    IF v_state <> 'leased' THEN RETURN 'not_leased'; END IF;
    IF p_ok THEN
        UPDATE attach_direct_cleanup SET job_state = 'done', completed_at = now(),
            lease_token = NULL, lease_until = NULL, last_error_code = NULL
            WHERE id = p_id AND lease_token = p_token AND job_state = 'leased';
        IF NOT FOUND THEN RETURN 'stale_token'; END IF;
        RETURN 'done';
    END IF;
    v_delay := v_backoff[least(v_attempts, 7)] * (1 + random() * 0.2) * interval '1 second';
    UPDATE attach_direct_cleanup SET
        job_state = CASE WHEN v_attempts >= 8 THEN 'blocked' ELSE 'retry_wait' END,
        lease_until = NULL, lease_token = NULL,
        last_error_code = CASE WHEN v_attempts >= 8 THEN 'max_attempts' ELSE 'delete_failed' END
        WHERE id = p_id AND lease_token = p_token AND job_state = 'leased';
    -- retry_wait 退避（无独立 next_retry_at 列：用 lease_until 空窗承载）
    UPDATE attach_direct_cleanup SET lease_until = now() + v_delay
        WHERE id = p_id AND job_state = 'retry_wait';
    RETURN CASE WHEN v_attempts >= 8 THEN 'blocked' ELSE 'retry_wait' END;
END;
$$;

-- ---------------------------------------------------------------- 容量账
-- candidate/unknown/winner 物理容量（G19）：attempt 账本聚合视图。
CREATE VIEW v_dv1_attempt_capacity AS
SELECT object_key,
       count(*) FILTER (WHERE attempt_state = 'WINNER') AS winner_count,
       coalesce(sum(proof_size) FILTER (WHERE attempt_state = 'WINNER'), 0) AS winner_bytes,
       coalesce(sum(proof_size) FILTER (WHERE attempt_state IN
           ('COPY_COMPLETED','PROOF_VERIFIED','GC_CLAIMED','REJECTED')), 0) AS candidate_bytes,
       coalesce(sum(proof_size) FILTER (WHERE attempt_state = 'UNKNOWN_QUARANTINED'), 0) AS unknown_bytes
    FROM attach_direct_attempt GROUP BY object_key;
