-- 回滚 000034: 移除 channel_message.edited_at
-- Rollback: drop channel_message.edited_at

ALTER TABLE public.channel_message DROP COLUMN IF EXISTS edited_at;
