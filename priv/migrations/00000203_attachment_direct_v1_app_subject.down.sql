-- direct-v1 application 主体 DOWN：有该类会话数据禁止回退。
DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM attach_pending
               WHERE direct_state IS NOT NULL
                 AND creator_user_id = 0 LIMIT 1) THEN
        RAISE EXCEPTION 'DV1_DOWN_BLOCKED_USE_FORWARD_FIX';
    END IF;
END
$$;

DROP FUNCTION IF EXISTS fn_dv1_quota_check_app(text);
DROP FUNCTION IF EXISTS fn_dv1_register_application(text, text, text, text, text, uuid, text, text, text, text, text, text);
