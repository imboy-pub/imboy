-- V5 DOWN 阻断（03 M4）：非空库禁止回退；空库可逆移除本迁移新增内容。
DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM attach_pending LIMIT 1)
       OR EXISTS (SELECT 1 FROM attachment LIMIT 1)
       OR EXISTS (SELECT 1 FROM attachment_object_deletion LIMIT 1) THEN
        RAISE EXCEPTION 'V5_DOWN_BLOCKED_USE_FORWARD_FIX';
    END IF;
END;
$$;

DROP FUNCTION IF EXISTS fn_v5_intent_pending_delete(text, text, text);
