DROP TRIGGER IF EXISTS imboy_for_user_collect ON public."user_collect";

DROP FUNCTION IF EXISTS public.imboy_user_collect_fun;

DROP TABLE IF EXISTS public."user_collect" CASCADE;
DROP INDEX IF EXISTS i_user_collect_UserId_Status_Kind;
