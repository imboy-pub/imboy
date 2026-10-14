-- W4-01 入口收口（02 §4/§5）：EAR purge 专用删除意图。企业应用附件
-- （attachment scope=enterprise）的治理 purge 在同事务置 purge_state=
-- 'purged'（治理终态）后，字节删除收口到删除队列——本函数以「同事务
-- 已 purged 的 EAR 行」为资格权威（代替 gc_verified：密文企业附件的
-- 统一资格谓词恒 false，purge 治理终态即授权），锁后复核：
--   attachment live；EAR 行存在且 purge_state='purged'；无 tombstone。
-- 锁序与普通域意图一致：identity 锁（seed 1685）→ path advisory（seed 168）；
-- 字节由 Worker 删，行由队列 ack 协议删（02 §4：不在 DB 事务中网络 DELETE）。

CREATE FUNCTION fn_v5_intent_purged_delete(p_path text, p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a attachment%ROWTYPE;
BEGIN
    SELECT * INTO v_a FROM attachment WHERE path = p_path FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 purged intent unknown attachment' USING ERRCODE = 'P0002';
    END IF;
    IF v_a.lifecycle_state <> 'live' THEN
        RAISE EXCEPTION 'v5 purged intent non-live asset' USING ERRCODE = '23510';
    END IF;
    IF NOT EXISTS (
        SELECT 1 FROM enterprise_attachment_retention ear
        WHERE ear.attachment_id = v_a.id AND ear.purge_state = 'purged'
    ) THEN
        RAISE EXCEPTION 'v5 purged intent requires EAR purge terminal state'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path = p_path) THEN
        RAISE EXCEPTION 'v5 intent job already exists (tombstone)'
            USING ERRCODE = '23505';
    END IF;
    PERFORM fn_v5_lock_identity(v_a.storage_driver, v_a.storage_namespace,
                                v_a.storage_container, v_a.path);
    PERFORM pg_advisory_xact_lock(hashtextextended(v_a.path, 168));
    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue q
               WHERE q.storage_namespace = v_a.storage_namespace
                 AND q.storage_container = v_a.storage_container
                 AND q.object_key = v_a.path) THEN
        RAISE EXCEPTION 'v5 intent cross-domain identity conflict'
            USING ERRCODE = '23510';
    END IF;
    UPDATE attachment SET lifecycle_state = 'delete_intent'
        WHERE id = v_a.id AND lifecycle_state = 'live';
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 purged intent lost eligibility race' USING ERRCODE = '40001';
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
            jsonb_build_object('attachment_id', v_a.id, 'purge_domain', 'ear'));
END;
$$;
ALTER FUNCTION fn_v5_intent_purged_delete(text, text) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_intent_purged_delete(text, text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_intent_purged_delete(text, text) TO v5_app_runtime;
