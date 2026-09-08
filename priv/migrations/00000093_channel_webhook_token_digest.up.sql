-- WH-02：Channel incoming webhook token 摘要化与轮换（PDT-01 webhook 契约 §3）。
-- 1) token_digest：SHA-256 hex（认证精确索引）；新 webhook 不再写明文 token 列；
-- 2) 存量明文回填摘要（双读期：认证先查 digest，未命中读明文列并惰性回填），
--    明文列保留至迁移截止期（v1.1 删除，见 E2E-01 runbook），不在日志打印；
-- 3) rotate 轮换：旧 token 摘要移入 grace_digest，宽限窗 grace_until 内仍可用，
--    过期后稳定 404；
-- 4) last_used_at 供 list 展示。

ALTER TABLE public.channel_webhook
    ADD COLUMN IF NOT EXISTS token_digest      text NOT NULL DEFAULT '',
    ADD COLUMN IF NOT EXISTS token_prefix      text NOT NULL DEFAULT '',
    ADD COLUMN IF NOT EXISTS grace_digest      text NOT NULL DEFAULT '',
    ADD COLUMN IF NOT EXISTS grace_until       timestamptz,
    ADD COLUMN IF NOT EXISTS last_used_at      timestamptz;

-- 存量明文回填摘要（双读期认证仍可走明文列，命中后惰性回填 digest）
UPDATE public.channel_webhook
SET token_digest = encode(sha256(token::bytea), 'hex'),
    token_prefix = left(token, 8)
WHERE token <> '' AND token_digest = '';

DROP INDEX IF EXISTS public.uk_channel_webhook_token;
CREATE UNIQUE INDEX IF NOT EXISTS uk_channel_webhook_token_digest
    ON public.channel_webhook (token_digest);
CREATE INDEX IF NOT EXISTS idx_channel_webhook_grace_digest
    ON public.channel_webhook (grace_digest);
