-- 迁移 000057: ai_agent 扩展可定制属性（imboyadmin 助手定制化）
--   category     — 分类标签（客服/办公/娱乐…），admin 列表可筛选
--   voice_id     — 语音音色标识（预留 TTS 接线位，业界音色枚举）
--   greeting     — 开场白（冷启动欢迎语，对齐 ai_agent_proactive:send_welcome）
--   capabilities — 能力开关 {knowledge, group_reply, proactive}（jsonb，收口已交付的三套基建）
--   temperature  — LLM 采样温度 0~1（默认 0.7，创造 vs 稳定，透传 imboy_llm_openai Opts）
--   Extends ai_agent with admin-customizable attributes for the assistant
--   management page (category/voice/greeting/capabilities/temperature).

ALTER TABLE public.ai_agent ADD COLUMN IF NOT EXISTS category varchar(40) NOT NULL DEFAULT '';
COMMENT ON COLUMN public.ai_agent.category IS '分类标签（客服/办公/娱乐等）';

ALTER TABLE public.ai_agent ADD COLUMN IF NOT EXISTS voice_id varchar(40) NOT NULL DEFAULT '';
COMMENT ON COLUMN public.ai_agent.voice_id IS '语音音色标识（预留 TTS 接线位）';

ALTER TABLE public.ai_agent ADD COLUMN IF NOT EXISTS greeting text NOT NULL DEFAULT '';
COMMENT ON COLUMN public.ai_agent.greeting IS '开场白（冷启动欢迎语）';

ALTER TABLE public.ai_agent ADD COLUMN IF NOT EXISTS capabilities jsonb NOT NULL DEFAULT '{}';
COMMENT ON COLUMN public.ai_agent.capabilities IS '能力开关 {knowledge, group_reply, proactive}';

ALTER TABLE public.ai_agent ADD COLUMN IF NOT EXISTS temperature real NOT NULL DEFAULT 0.7;
COMMENT ON COLUMN public.ai_agent.temperature IS 'LLM 采样温度 0~1（默认 0.7）';

CREATE INDEX IF NOT EXISTS idx_ai_agent_category ON public.ai_agent(category);
