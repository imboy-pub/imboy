-- Phase 3 T3.5: MCP 治理 / MCP governance
-- 管理员审批 MCP 客户端接入 + 按 tool 授权 + 审计。
-- client 身份 = owner_uid（一个用户的 MCP 接入视为一个 client，MVP 足够）。
-- status: pending/approved/revoked。

CREATE TABLE IF NOT EXISTS public.mcp_client (
    client_id   bigint       NOT NULL,
    owner_uid   bigint       NOT NULL,
    name        varchar(80)  DEFAULT ''::character varying NOT NULL,
    description text         DEFAULT ''::text NOT NULL,
    status      varchar(16)  DEFAULT 'pending'::character varying NOT NULL,
    reason      text         DEFAULT ''::text NOT NULL,
    created_at  timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    approved_at timestamp with time zone,
    updated_at  timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (client_id),
    CONSTRAINT uniq_mcp_client_owner UNIQUE (owner_uid)
);
CREATE INDEX IF NOT EXISTS idx_mcp_client_status ON public.mcp_client(status);
COMMENT ON TABLE public.mcp_client IS 'MCP 客户端接入登记与审批状态（owner_uid 唯一）';

CREATE TABLE IF NOT EXISTS public.mcp_client_grant (
    id         bigint       NOT NULL,
    client_id  bigint       NOT NULL,
    tool_name  varchar(80)  NOT NULL,
    enabled    boolean      DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (id),
    CONSTRAINT uniq_mcp_grant_client_tool UNIQUE (client_id, tool_name)
);
CREATE INDEX IF NOT EXISTS idx_mcp_grant_client ON public.mcp_client_grant(client_id);
COMMENT ON TABLE public.mcp_client_grant IS 'MCP 客户端按 tool 的授权开关（enforce=true 时生效）';

CREATE TABLE IF NOT EXISTS public.mcp_audit_log (
    id         bigint       NOT NULL,
    client_id  bigint       DEFAULT 0 NOT NULL,
    owner_uid  bigint       DEFAULT 0 NOT NULL,
    action     varchar(24)  NOT NULL,
    tool       varchar(80)  DEFAULT ''::character varying NOT NULL,
    actor_uid  bigint       DEFAULT 0 NOT NULL,
    detail     jsonb        DEFAULT '{}'::jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (id)
);
CREATE INDEX IF NOT EXISTS idx_mcp_audit_client ON public.mcp_audit_log(client_id);
CREATE INDEX IF NOT EXISTS idx_mcp_audit_created ON public.mcp_audit_log(created_at DESC);
COMMENT ON TABLE public.mcp_audit_log IS 'MCP 治理审计：客户端 tool 调用行为 + 管理员审批动作';
