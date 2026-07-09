-- Phase 1 T1.1 回滚 / rollback
DROP TABLE IF EXISTS public.ai_agent;
ALTER TABLE public."user" DROP COLUMN IF EXISTS account_type;
