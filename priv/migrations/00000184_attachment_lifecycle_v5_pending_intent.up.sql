-- W4-01 入口收口（02 §2/§5）：pending 域删除意图函数。拒绝上传 / pending
-- 清理入口不再直接删字节——同事务完成 session 状态迁移、ready job 与审计；
-- 字节由 Worker 删。两种模式（资格谓词均在函数内锁后复核，不接受调用方
-- 布尔作为资格证明）：
--   'reject'：主动拒绝（confirm 内容复核失败即撤权）。registered → rejected
--             → delete_intent 同事务两跳（触发器强制合法转换）；不查能力窗
--             ——主动拒绝本身即写权限耗尽的权威动作。
--   'sweep' ：回收清扫。仅 rejected/expired 起点；上传能力窗必须已耗尽。
-- 公共资格：同 path 无 live metadata 行（有则必须走
-- fn_v5_intent_ordinary_delete，metadata 域资格在其锁内复核）；无既有
-- tombstone。锁序与 metadata intent 一致：identity 锁（seed 1685）→
-- path advisory（seed 168）。

CREATE FUNCTION fn_v5_intent_pending_delete(p_object_key text, p_reason text,
                                            p_mode text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_p attach_pending%ROWTYPE; v_a attachment%ROWTYPE;
BEGIN
    IF p_mode NOT IN ('reject', 'sweep') THEN
        RAISE EXCEPTION 'v5 pending intent unknown mode' USING ERRCODE = '23514';
    END IF;
    SELECT * INTO v_p FROM attach_pending WHERE object_key = p_object_key FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 pending intent unknown session' USING ERRCODE = 'P0002';
    END IF;
    IF p_mode = 'sweep' THEN
        IF v_p.session_state NOT IN ('rejected', 'expired') THEN
            RAISE EXCEPTION 'v5 pending sweep illegal from %', v_p.session_state
                USING ERRCODE = '23510';
        END IF;
        IF v_p.upload_expires_at IS NULL OR v_p.upload_expires_at > now() THEN
            RAISE EXCEPTION 'v5 pending intent upload capability not exhausted'
                USING ERRCODE = '23510';
        END IF;
    ELSE
        IF v_p.session_state <> 'registered' THEN
            RAISE EXCEPTION 'v5 pending reject illegal from %', v_p.session_state
                USING ERRCODE = '23510';
        END IF;
    END IF;
    SELECT * INTO v_a FROM attachment WHERE path = p_object_key;
    IF FOUND AND v_a.lifecycle_state <> 'deleted' THEN
        RAISE EXCEPTION 'v5 pending intent on metadata-backed asset'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path = p_object_key) THEN
        RAISE EXCEPTION 'v5 intent job already exists (tombstone)'
            USING ERRCODE = '23505';
    END IF;
    -- Object lock ordering: identity lock, then queue path lock (seed 168) —
    -- 与 fn_v5_intent_ordinary_delete 同序同域。
    PERFORM fn_v5_lock_identity(v_p.storage_driver, v_p.storage_namespace,
                                v_p.storage_container, v_p.object_key);
    PERFORM pg_advisory_xact_lock(hashtextextended(v_p.object_key, 168));
    -- Cross-domain same-identity rejection.
    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue q
               WHERE q.storage_namespace = v_p.storage_namespace
                 AND q.storage_container = v_p.storage_container
                 AND q.object_key = v_p.object_key) THEN
        RAISE EXCEPTION 'v5 intent cross-domain identity conflict'
            USING ERRCODE = '23510';
    END IF;
    IF p_mode = 'reject' THEN
        UPDATE attach_pending SET session_state = 'rejected'
            WHERE object_key = p_object_key AND session_state = 'registered';
        IF NOT FOUND THEN
            RAISE EXCEPTION 'v5 pending intent lost eligibility race'
                USING ERRCODE = '40001';
        END IF;
    END IF;
    UPDATE attach_pending SET session_state = 'delete_intent'
        WHERE object_key = p_object_key
          AND session_state IN ('rejected', 'expired');
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 pending intent lost eligibility race' USING ERRCODE = '40001';
    END IF;
    INSERT INTO attachment_object_deletion
        (path, bucket, storage_driver, storage_namespace, storage_container,
         storage_locator_sha256, job_state, next_retry_at, last_error_code)
    VALUES (v_p.object_key, v_p.bucket, v_p.storage_driver, v_p.storage_namespace,
            v_p.storage_container, v_p.storage_locator_sha256,
            'ready', now(), NULL);
    INSERT INTO attachment_lifecycle_v5_audit
        (queue_domain, object_key, storage_namespace, action, reason, actor, details)
    VALUES ('ordinary', v_p.object_key, v_p.storage_namespace, 'delete_intent',
            p_reason, current_user,
            jsonb_build_object('pending_session', true, 'mode', p_mode,
                               'creator', v_p.creator_user_id));
END;
$$;
ALTER FUNCTION fn_v5_intent_pending_delete(text, text, text) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_intent_pending_delete(text, text, text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_intent_pending_delete(text, text, text) TO v5_app_runtime;
