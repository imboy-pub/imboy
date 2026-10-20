-- direct-v1 会话函数 DOWN：有 direct 会话数据禁止回退。
DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM attach_pending WHERE direct_state IS NOT NULL LIMIT 1)
       OR EXISTS (SELECT 1 FROM attach_pending WHERE direct_expected IS NOT NULL LIMIT 1) THEN
        RAISE EXCEPTION 'DV1_DOWN_BLOCKED_USE_FORWARD_FIX';
    END IF;
END
$$;

DROP FUNCTION IF EXISTS fn_dv1_reconcile_list(integer);
DROP FUNCTION IF EXISTS fn_dv1_fail(text, text);
DROP FUNCTION IF EXISTS fn_dv1_begin_finalize(text, integer);
DROP FUNCTION IF EXISTS fn_dv1_grant(text, text, text, text, text, text, text);
DROP FUNCTION IF EXISTS fn_dv1_declare(text, text, bigint, text);
DROP FUNCTION IF EXISTS fn_dv1_register(text, text, text, text, bigint, uuid, text, text, text, text, text, text);

ALTER TABLE attach_pending
    DROP CONSTRAINT IF EXISTS ck_dv1_expected_shape,
    DROP COLUMN IF EXISTS direct_expected;
