-- ================================================================
-- 频道消息撤回能力
-- 版本: 00000066
-- 日期: 2026-02-22
-- 说明: 为 channel_message 增加撤回字段
-- ================================================================

ALTER TABLE IF EXISTS public.channel_message
    ADD COLUMN IF NOT EXISTS revoked BOOLEAN NOT NULL DEFAULT FALSE,
    ADD COLUMN IF NOT EXISTS revoked_at TIMESTAMPTZ NULL,
    ADD COLUMN IF NOT EXISTS revoked_by BIGINT NULL;

COMMENT ON COLUMN public.channel_message.revoked IS '是否已撤回';
COMMENT ON COLUMN public.channel_message.revoked_at IS '撤回时间';
COMMENT ON COLUMN public.channel_message.revoked_by IS '撤回操作人';

CREATE INDEX IF NOT EXISTS i_channel_message_revoked
    ON public.channel_message(channel_id, revoked, created_at DESC);
