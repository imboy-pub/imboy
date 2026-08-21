-- P4: 为现有 LLM Provider 创建默认 Agent（幂等数据迁移）
-- 为每个已注册的 LLM provider 创建一个默认的 Agent 账号，
-- 使用户可以通过 C2C 私聊直接与 AI 助手对话（替代旧的 bot_* 前缀）。
--
-- 幂等设计：INSERT ... ON CONFLICT DO NOTHING，可安全重复执行。
-- 用户 ID 使用 1000000000000000000 起始偏移，确保不与 TSID 生成值冲突。

BEGIN;

-- 1. ark 默认 Agent
INSERT INTO public."user" (id, account, nickname, password, status, account_type, reg_ip, reg_cosv, created_at)
SELECT 1000000000000000001, 'agent_ark', 'Ark 助手', '', 1, 1, '127.0.0.1', 'migration', now()
WHERE NOT EXISTS (SELECT 1 FROM public.ai_agent WHERE provider = 'ark');

INSERT INTO public.ai_agent (user_id, provider, model, system_prompt, owner_uid, status, description, visibility)
SELECT 1000000000000000001, 'ark', 'doubao-lite-4k', '你是一个有帮助的AI助手，请专业、友善地回答用户问题。', 0, 1, 'Ark 平台 AI 助手', 1
WHERE NOT EXISTS (SELECT 1 FROM public.ai_agent WHERE provider = 'ark');

-- 2. bailian 默认 Agent
INSERT INTO public."user" (id, account, nickname, password, status, account_type, reg_ip, reg_cosv, created_at)
SELECT 1000000000000000002, 'agent_bailian', '百炼助手', '', 1, 1, '127.0.0.1', 'migration', now()
WHERE NOT EXISTS (SELECT 1 FROM public.ai_agent WHERE provider = 'bailian');

INSERT INTO public.ai_agent (user_id, provider, model, system_prompt, owner_uid, status, description, visibility)
SELECT 1000000000000000002, 'bailian', 'qwen3.7-flash', '你是一个有帮助的AI助手，请专业、友善地回答用户问题。', 0, 1, '百炼平台 AI 助手', 1
WHERE NOT EXISTS (SELECT 1 FROM public.ai_agent WHERE provider = 'bailian');

COMMIT;