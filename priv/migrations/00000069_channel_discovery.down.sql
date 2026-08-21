-- 00000069_channel_discovery.down.sql
-- 回滚频道发现页增强

BEGIN;

DROP TRIGGER IF EXISTS trg_fts_channel_update ON public.channel;
DROP FUNCTION IF EXISTS fts_channel_trigger();
DROP TABLE IF EXISTS public.fts_channel;

ALTER TABLE public.channel DROP COLUMN IF EXISTS featured_at;
ALTER TABLE public.channel DROP COLUMN IF EXISTS is_featured;
ALTER TABLE public.channel DROP COLUMN IF EXISTS category_id;

DROP TABLE IF EXISTS public.channel_category;

COMMIT;