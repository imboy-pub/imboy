-- 回滚 000058：移除 AI Agent 角色模板及其版本

DROP INDEX IF EXISTS public.idx_ai_agent_role_version_role_state;
DROP INDEX IF EXISTS public.idx_ai_agent_role_one_published;
DROP TABLE IF EXISTS public.ai_agent_role_version;
DROP INDEX IF EXISTS public.idx_ai_agent_role_status_updated;
DROP TABLE IF EXISTS public.ai_agent_role;
