-- 迁移 000058：AI Agent 可复用角色模板与版本
-- 角色负责行为配置（提示词、能力门控、知识策略），助手只绑定角色并保留运行身份。

CREATE TABLE IF NOT EXISTS public.ai_agent_role (
    code varchar(40) PRIMARY KEY,
    name varchar(80) NOT NULL,
    description text NOT NULL DEFAULT '',
    status smallint NOT NULL DEFAULT 1 CHECK (status IN (0, 1)),
    active_version integer NOT NULL DEFAULT 0 CHECK (active_version >= 0),
    created_by bigint NOT NULL DEFAULT 0,
    created_at timestamptz NOT NULL DEFAULT NOW(),
    updated_at timestamptz NOT NULL DEFAULT NOW()
);

COMMENT ON TABLE public.ai_agent_role IS 'AI Agent 可复用角色模板';
COMMENT ON COLUMN public.ai_agent_role.code IS '稳定的业务角色编码，绑定助手时使用';
COMMENT ON COLUMN public.ai_agent_role.active_version IS '当前生效的已发布版本号，0 表示尚未发布';

CREATE INDEX IF NOT EXISTS idx_ai_agent_role_status_updated
    ON public.ai_agent_role(status, updated_at DESC);

CREATE TABLE IF NOT EXISTS public.ai_agent_role_version (
    id bigint PRIMARY KEY,
    role_code varchar(40) NOT NULL REFERENCES public.ai_agent_role(code) ON DELETE RESTRICT,
    version integer NOT NULL CHECK (version > 0),
    state varchar(16) NOT NULL DEFAULT 'draft'
        CHECK (state IN ('draft', 'published', 'archived')),
    system_prompt text NOT NULL,
    capabilities jsonb NOT NULL DEFAULT '{}',
    knowledge_policy jsonb NOT NULL DEFAULT '{}',
    created_by bigint NOT NULL DEFAULT 0,
    published_by bigint,
    created_at timestamptz NOT NULL DEFAULT NOW(),
    published_at timestamptz,
    UNIQUE (role_code, version)
);

COMMENT ON TABLE public.ai_agent_role_version IS 'AI Agent 角色配置版本，发布后供运行时继承';
COMMENT ON COLUMN public.ai_agent_role_version.capabilities IS '固定能力策略，不允许助手实例绕过角色授权';
COMMENT ON COLUMN public.ai_agent_role_version.knowledge_policy IS '知识库按需检索策略，避免每轮注入完整知识库';

CREATE UNIQUE INDEX IF NOT EXISTS idx_ai_agent_role_one_published
    ON public.ai_agent_role_version(role_code)
    WHERE state = 'published';

CREATE INDEX IF NOT EXISTS idx_ai_agent_role_version_role_state
    ON public.ai_agent_role_version(role_code, state, version DESC);
