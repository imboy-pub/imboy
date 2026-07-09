-- Phase 1 T1.1: AI Agent 账号身份 / AI Agent account identity
-- user 表加账号类型（最小侵入，一个字段）；agent 元数据独立表避免 user 表膨胀
-- Add account_type to user; keep agent metadata in a dedicated table.

ALTER TABLE public."user" ADD COLUMN IF NOT EXISTS account_type smallint NOT NULL DEFAULT 0;
COMMENT ON COLUMN public."user".account_type IS '账号类型 0=human 1=ai_agent 2=system_bot';

CREATE TABLE IF NOT EXISTS public.ai_agent (
    user_id        bigint       NOT NULL,                       -- 关联 user.id（agent 也是一等 user 账号）
    provider       varchar(40)  NOT NULL,                       -- llm_providers key（qianfan/openai/...）
    model          varchar(80)  DEFAULT ''::character varying NOT NULL,
    role_id        varchar(40)  DEFAULT ''::character varying NOT NULL,   -- 对应 ai_roles
    system_prompt  text         DEFAULT ''::text NOT NULL,
    owner_uid      bigint       DEFAULT 0 NOT NULL,             -- 归属/创建者（计费与权限）
    trigger_policy jsonb        NOT NULL DEFAULT '{}'::jsonb,   -- 群触发规则 {mention,suffix_q,keywords[],group_allowlist[]}
    status         smallint     NOT NULL DEFAULT 1,             -- 1=启用 0=停用
    created_at     timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at     timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (user_id),
    CONSTRAINT fk_ai_agent_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE
);
CREATE INDEX IF NOT EXISTS idx_ai_agent_owner ON public.ai_agent(owner_uid);
CREATE INDEX IF NOT EXISTS idx_ai_agent_status ON public.ai_agent(status);
COMMENT ON TABLE public.ai_agent IS 'AI Agent 元数据（provider/role/owner/群触发策略）';
