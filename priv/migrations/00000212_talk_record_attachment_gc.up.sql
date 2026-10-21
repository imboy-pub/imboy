-- 00000212_talk_record_attachment_gc.up.sql
-- 面聊记录附件接入统一附件生命周期（DESIGN §10.2/§10.3/§10.4；T06 卡交付 6）。
--
-- 前置：00000211（talk_record_v1 17 表，含 talk_record_attachment_ref）。
-- 本迁移三件事（全部可逆，down 与 185 版函数体逐字对齐恢复）：
--   1. talk_record_attachment_ref.lease_until —— processing lease（§10.3
--      complete/合成/转码：任务处理期间保持源文件"期限明确的引用"，取消/超时
--      都能释放）。语义：state='bound' 且 lease_until IS NULL = 无期限业务引用；
--      lease_until 非空 = 有期限处理引用，仅 lease_until > now() 期间对 GC 有效。
--      状态机保持 209 的 bound/released 两态不动（lease 到期由清扫器惰性转
--      released，谓词按时间比较，不依赖清扫先行）。
--   2. fn_v5_intent_ordinary_delete 资格谓词扩展：在 185 版（W3-01 锁后复核）
--      基础上增加 talk_record_attachment_ref 有效引用守卫——pre-lock 快速拒绝
--      + 对象锁后复核（与 group_file 同款双检；对齐 02 §5"intent 在对象锁内
--      重查全部引用"）。绑定事务经 bind_reference_lock_tx 与本函数同一对象锁
--      线性化：bind 先提交 => intent 23510 拒绝；intent 先提交 => 墓碑拒绝新增
--      引用。没有本守卫时 talk_record 附件会被 GC 当作零引用资产误删。
--   3. GC 谓词支撑索引（lease 清扫）。T04 已建 i_trar_attachment (attachment_id)
--      WHERE state='bound'，覆盖按附件反查；本迁移只补 lease 到期扫描索引。
--
-- attachment.scope 无 CHECK/enum（varchar(16) 自由文本，迁移 13），'talk_record'
-- 11 字符可直接落库——无需扩 attachment 表（先查现表结论，见 T06 报告）。
-- 禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
SET LOCAL lock_timeout = '5s';

-- ---------------------------------------------------------------- 1) lease 列
ALTER TABLE talk_record_attachment_ref
    ADD COLUMN IF NOT EXISTS lease_until timestamp with time zone;

COMMENT ON COLUMN talk_record_attachment_ref.lease_until IS
    'processing lease 到期时间（§10.3）：NULL=无期限业务引用；非空=有期限处理引用，到期即对 GC 失效，由清扫器惰性转 released';

-- lease 到期清扫（release_expired_leases）支撑索引
CREATE INDEX IF NOT EXISTS i_trar_lease_sweep
    ON talk_record_attachment_ref (lease_until)
    WHERE state = 'bound' AND lease_until IS NOT NULL;

-- --------------------------------- 2) intent 资格谓词扩展（185 基底 + talk_record 守卫）
-- 与 00000185 完全同构，仅新增两处 talk_record_attachment_ref 守卫：
--   a) pre-lock eligibility 段（EAR 检查后）；
--   b) 对象锁后复核段（moment 检查后）。
-- 有效引用 = state='bound' 且 (lease_until IS NULL OR lease_until > now())。
DROP FUNCTION IF EXISTS fn_v5_intent_ordinary_delete(text, text);

CREATE FUNCTION fn_v5_intent_ordinary_delete(p_path text, p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a attachment%ROWTYPE; v_p attach_pending%ROWTYPE; v_bucket text;
BEGIN
    SELECT * INTO v_a FROM attachment WHERE path = p_path FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 intent unknown attachment' USING ERRCODE = 'P0002';
    END IF;
    -- Eligibility conjunction, re-checked under lock; unknown => not eligible.
    IF v_a.lifecycle_state <> 'live' THEN
        RAISE EXCEPTION 'v5 intent non-live asset' USING ERRCODE = '23510';
    END IF;
    IF NOT v_a.gc_verified THEN
        RAISE EXCEPTION 'v5 intent requires proven reference-zero (gc_verified)'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM workspace_attachment_retention
               WHERE attachment_id = v_a.id) THEN
        RAISE EXCEPTION 'v5 intent blocked by workspace retention'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM enterprise_attachment_retention
               WHERE attachment_id = v_a.id
                 AND (hold_state = 'held'
                      OR purge_state IS DISTINCT FROM 'purged')) THEN
        RAISE EXCEPTION 'v5 intent blocked by EAR' USING ERRCODE = '23510';
    END IF;
    IF v_a.group_file_id IS NOT NULL
       AND EXISTS (SELECT 1 FROM group_file WHERE id = v_a.group_file_id) THEN
        RAISE EXCEPTION 'v5 intent blocked by live group_file binding'
            USING ERRCODE = '23510';
    END IF;
    -- T06（00000212）：talk_record 有效业务引用（含未到期 processing lease）
    -- 阻止普通域删除意图——pre-lock 快速拒绝。
    IF EXISTS (SELECT 1 FROM talk_record_attachment_ref trar
               WHERE trar.attachment_id = v_a.id
                 AND trar.state = 'bound'
                 AND (trar.lease_until IS NULL OR trar.lease_until > now())) THEN
        RAISE EXCEPTION 'v5 intent blocked by talk_record reference'
            USING ERRCODE = '23510';
    END IF;
    SELECT * INTO v_p FROM attach_pending WHERE object_key = p_path;
    IF FOUND AND (v_p.upload_expires_at IS NULL OR v_p.upload_expires_at > now()) THEN
        RAISE EXCEPTION 'v5 intent upload capability not exhausted'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path = p_path) THEN
        RAISE EXCEPTION 'v5 intent job already exists (tombstone)'
            USING ERRCODE = '23505';
    END IF;
    -- Object lock ordering: identity lock, then queue path lock (seed 168).
    PERFORM fn_v5_lock_identity(v_a.storage_driver, v_a.storage_namespace,
                                v_a.storage_container, v_a.path);
    PERFORM pg_advisory_xact_lock(hashtextextended(v_a.path, 168));
    -- Cross-domain same-identity rejection.
    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue q
               WHERE q.storage_namespace = v_a.storage_namespace
                 AND q.storage_container = v_a.storage_container
                 AND q.object_key = v_a.path) THEN
        RAISE EXCEPTION 'v5 intent cross-domain identity conflict'
            USING ERRCODE = '23510';
    END IF;
    -- W3-01：锁后资格复核（02 §4——取锁前的候选查询不构成授权）：
    -- 绑定事务与 intent 在同一对象锁上线性化，先提交者唯一胜出。
    IF v_a.group_file_id IS NOT NULL
       AND EXISTS (SELECT 1 FROM group_file gf WHERE gf.id = v_a.group_file_id) THEN
        RAISE EXCEPTION 'v5 intent blocked by live group_file binding'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM user_collect uc
               WHERE uc.attach_file_hash256 <> '' AND uc.attach_file_hash256 = v_a.file_hash256
                 AND uc.status = 1) THEN
        RAISE EXCEPTION 'v5 intent blocked by collect reference'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM moment_post mp
               WHERE position(v_a.path in mp.media::text) > 0) THEN
        RAISE EXCEPTION 'v5 intent blocked by moment media reference'
            USING ERRCODE = '23510';
    END IF;
    -- T06（00000212）：锁后复核 talk_record 有效引用（gc_verified 为缓存标志，
    -- 绑定可能晚于上次 refresh —— 02 §5"intent 在对象锁内重查全部引用"）。
    IF EXISTS (SELECT 1 FROM talk_record_attachment_ref trar
               WHERE trar.attachment_id = v_a.id
                 AND trar.state = 'bound'
                 AND (trar.lease_until IS NULL OR trar.lease_until > now())) THEN
        RAISE EXCEPTION 'v5 intent blocked by talk_record reference'
            USING ERRCODE = '23510';
    END IF;
    UPDATE attachment SET lifecycle_state = 'delete_intent'
        WHERE id = v_a.id AND lifecycle_state = 'live';
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 intent lost eligibility race' USING ERRCODE = '40001';
    END IF;
    INSERT INTO attachment_object_deletion
        (path, bucket, storage_driver, storage_namespace, storage_container,
         storage_locator_sha256, job_state, next_retry_at, last_error_code)
    VALUES (v_a.path, v_a.url, v_a.storage_driver, v_a.storage_namespace,
            v_a.storage_container, v_a.storage_locator_sha256,
            'ready', now(), NULL);
    INSERT INTO attachment_lifecycle_v5_audit
        (queue_domain, object_key, storage_namespace, action, reason, actor, details)
    VALUES ('ordinary', v_a.path, v_a.storage_namespace, 'delete_intent',
            p_reason, current_user,
            jsonb_build_object('attachment_id', v_a.id,
                               'locator', v_a.storage_locator_sha256));
END;
$$;

ALTER FUNCTION fn_v5_intent_ordinary_delete(text, text) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_intent_ordinary_delete(text, text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_intent_ordinary_delete(text, text) TO v5_app_runtime;

-- 锁后复核新增读取的引用表授权（SECURITY DEFINER 以 v5_fn_owner 执行；对齐 185 惯例）
GRANT SELECT ON public.talk_record_attachment_ref TO v5_fn_owner;
