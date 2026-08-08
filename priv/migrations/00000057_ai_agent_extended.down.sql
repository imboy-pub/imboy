-- 迁移 000057 回滚：移除 ai_agent 扩展属性列
-- Reverts 000057: drops the extended ai_agent attribute columns.

ALTER TABLE public.ai_agent DROP COLUMN IF EXISTS category;
ALTER TABLE public.ai_agent DROP COLUMN IF EXISTS voice_id;
ALTER TABLE public.ai_agent DROP COLUMN IF EXISTS greeting;
ALTER TABLE public.ai_agent DROP COLUMN IF EXISTS capabilities;
ALTER TABLE public.ai_agent DROP COLUMN IF EXISTS temperature;
