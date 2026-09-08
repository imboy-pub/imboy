-- WH-01 down：删除 outbox 双表与 bot 凭证列。
DROP TABLE IF EXISTS public.bot_delivery_attempt;
DROP TABLE IF EXISTS public.bot_delivery;
DROP INDEX IF EXISTS public.bot_api_token_digest_idx;
ALTER TABLE public.bot
    DROP COLUMN IF EXISTS token_migrated,
    DROP COLUMN IF EXISTS verify_token_enc,
    DROP COLUMN IF EXISTS api_token_prefix,
    DROP COLUMN IF EXISTS api_token_digest;
