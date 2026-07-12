-- 迁移 000034: channel_message 加 edited_at（频道消息编辑时间）
--   支撑频道消息编辑能力（P1 消息编辑补齐）：编辑成功后写入编辑时间，
--   客户端据此展示「已编辑」标记。
--   Adds channel_message.edited_at to support channel message editing.

ALTER TABLE public.channel_message ADD COLUMN IF NOT EXISTS edited_at timestamptz;
COMMENT ON COLUMN public.channel_message.edited_at IS '最后编辑时间 / Last edited time';
