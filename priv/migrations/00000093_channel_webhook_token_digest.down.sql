-- WH-02 down：删除摘要/轮换列与索引，恢复旧 token 唯一索引。
DROP INDEX IF EXISTS public.idx_channel_webhook_grace_digest;
DROP INDEX IF EXISTS public.uk_channel_webhook_token_digest;
ALTER TABLE public.channel_webhook
    DROP COLUMN IF EXISTS last_used_at,
    DROP COLUMN IF EXISTS grace_until,
    DROP COLUMN IF EXISTS grace_digest,
    DROP COLUMN IF EXISTS token_prefix,
    DROP COLUMN IF EXISTS token_digest;
CREATE UNIQUE INDEX IF NOT EXISTS uk_channel_webhook_token
    ON public.channel_webhook (token);
