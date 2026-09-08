-- WH-01：Bot 凭证安全 + 出站 Webhook outbox 可靠交付。
-- 1) bot 表：api_token 摘要+前缀（明文列进入迁移截止期，认证只走摘要）；
--    verify_token_enc（AEAD 密文，可认证存储）。明文列保留至迁移截止
--    （v1.1 删除，见迁移注释与 E2E-01 runbook）；认证/投递路径一律不再读明文。
-- 2) bot_delivery outbox：主消息路径只写 outbox，不依赖外部 HTTP 成功；
--    delivery_id 主键 + idempotency_key 唯一 + correlation_id（TRACE-00）。
-- 3) bot_delivery_attempt：每次投递尝试的审计（attempt、status class、latency、
--    截断错误；不存响应正文与 secret）。

ALTER TABLE public.bot
    ADD COLUMN IF NOT EXISTS api_token_digest text NOT NULL DEFAULT '',
    ADD COLUMN IF NOT EXISTS api_token_prefix text NOT NULL DEFAULT '',
    ADD COLUMN IF NOT EXISTS verify_token_enc text NOT NULL DEFAULT '',
    ADD COLUMN IF NOT EXISTS token_migrated boolean NOT NULL DEFAULT false;

-- 存量明文 token 回填摘要（认证精确索引）；明文列保留至迁移截止期
UPDATE public.bot
SET api_token_digest = encode(sha256(api_token::bytea), 'hex'),
    api_token_prefix = left(api_token, 8),
    token_migrated = false
WHERE api_token IS NOT NULL AND api_token <> '' AND api_token_digest = '';

CREATE INDEX IF NOT EXISTS bot_api_token_digest_idx ON public.bot (api_token_digest);

CREATE TABLE IF NOT EXISTS public.bot_delivery (
    delivery_id     text PRIMARY KEY,
    bot_id          text        NOT NULL,
    event_type      text        NOT NULL DEFAULT 'message',
    payload         jsonb       NOT NULL DEFAULT '{}'::jsonb,
    reply_context   text        NOT NULL DEFAULT '',
    correlation_id  text        NOT NULL,
    idempotency_key text        NOT NULL,
    status          text        NOT NULL DEFAULT 'pending'
                    CONSTRAINT bot_delivery_status_check CHECK (status IN (
                        'pending', 'retry', 'success', 'dead')),
    attempt_count   integer     NOT NULL DEFAULT 0,
    next_retry_at   timestamptz NOT NULL DEFAULT NOW(),
    webhook_host    text        NOT NULL DEFAULT '',
    pinned_ip       text        NOT NULL DEFAULT '',
    created_at      timestamptz NOT NULL DEFAULT NOW(),
    updated_at      timestamptz NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS bot_delivery_due_idx
    ON public.bot_delivery (status, next_retry_at);
CREATE INDEX IF NOT EXISTS bot_delivery_bot_idx
    ON public.bot_delivery (bot_id, created_at DESC);
CREATE UNIQUE INDEX IF NOT EXISTS bot_delivery_idem_uq
    ON public.bot_delivery (idempotency_key);

CREATE TABLE IF NOT EXISTS public.bot_delivery_attempt (
    id             text PRIMARY KEY,
    delivery_id    text        NOT NULL REFERENCES public.bot_delivery (delivery_id) ON DELETE CASCADE,
    attempt_no     integer     NOT NULL,
    status_class   text        NOT NULL DEFAULT '',
    http_status    integer,
    latency_ms     integer,
    error_trunc    text        NOT NULL DEFAULT '',
    created_at     timestamptz NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS bot_delivery_attempt_idx
    ON public.bot_delivery_attempt (delivery_id, attempt_no);
