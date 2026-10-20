-- direct-v1 配额/GC DOWN：有未完成 cleanup 账数据禁止回退。
DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM attach_direct_cleanup WHERE job_state <> 'done' LIMIT 1) THEN
        RAISE EXCEPTION 'DV1_DOWN_BLOCKED_USE_FORWARD_FIX';
    END IF;
END
$$;

DROP VIEW IF EXISTS v_dv1_attempt_capacity;
DROP FUNCTION IF EXISTS fn_dv1_cleanup_ack(bigint, uuid, boolean);
DROP FUNCTION IF EXISTS fn_dv1_cleanup_claim(uuid);
DROP FUNCTION IF EXISTS fn_dv1_cancel(text, text);
DROP FUNCTION IF EXISTS fn_dv1_gc_claim(text, integer, text);
DROP FUNCTION IF EXISTS fn_dv1_quota_check(bigint);
DROP TABLE IF EXISTS attach_direct_cleanup;
