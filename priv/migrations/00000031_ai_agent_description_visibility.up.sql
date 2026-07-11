-- 迁移 000031: ai_agent 加 description（专属简介）+ visibility（可发现性）
--   /api/v1/agent/list 发现端点原以 owner_uid=0 近似「官方助手」、description 借用
--   user.sign，均为占位口径。本迁移补两列改为真实语义：
--     description  — agent 专属简介（独立于 user.sign）
--     visibility   — 0=私有（默认，仅归属者可见）1=公开可发现
--   backfill 把现有 owner_uid=0 的官方助手置 visibility=1，避免加列后它们从
--   发现列表消失（新列默认 0）。
--   Adds ai_agent.description + visibility; backfills existing official
--   (owner_uid=0) agents to visibility=1 so discovery stays intact.

ALTER TABLE public.ai_agent ADD COLUMN IF NOT EXISTS description text NOT NULL DEFAULT ''::text;
COMMENT ON COLUMN public.ai_agent.description IS 'agent 专属简介（发现卡片用，独立于 user.sign）';

ALTER TABLE public.ai_agent ADD COLUMN IF NOT EXISTS visibility smallint NOT NULL DEFAULT 0;
COMMENT ON COLUMN public.ai_agent.visibility IS '可发现性 0=私有（默认）1=公开可发现';

CREATE INDEX IF NOT EXISTS idx_ai_agent_visibility ON public.ai_agent(visibility);

-- backfill：现有官方助手保持可被发现
UPDATE public.ai_agent SET visibility = 1 WHERE owner_uid = 0;
