DROP TRIGGER IF EXISTS imboy_for_fts_user ON public."user";

DROP FUNCTION IF EXISTS public.imboy_user_for_fts_fun;

DROP TABLE IF EXISTS public."fts_user" CASCADE;
DROP TABLE IF EXISTS public."article" CASCADE;
DROP INDEX IF EXISTS user_fts_gin_idex;
DROP INDEX IF EXISTS article_fts_gin_index;
ALTER TABLE public."article" DROP COLUMN IF EXISTS "fts";
