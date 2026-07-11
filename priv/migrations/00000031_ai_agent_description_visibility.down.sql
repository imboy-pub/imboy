-- 回滚 000031：删除 visibility + description 两列（连带 visibility 索引）
DROP INDEX IF EXISTS public.idx_ai_agent_visibility;
ALTER TABLE public.ai_agent DROP COLUMN IF EXISTS visibility;
ALTER TABLE public.ai_agent DROP COLUMN IF EXISTS description;
