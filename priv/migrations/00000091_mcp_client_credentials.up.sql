-- MCP-01：MCP Client 独立身份与凭证（PDT-01 mcp_client 契约的数据库落地）。
-- 1) 去除「一 owner 一 client」唯一约束（保留 owner 归属列，同 owner 可多 client）；
-- 2) 新增稳定 client_key、credential 摘要/前缀、到期、最近使用、禁用位。
-- 凭证明文只在创建响应返回一次；库中仅存 SHA-256 摘要（高熵 token 精确索引查找）。

ALTER TABLE public.mcp_client
    DROP CONSTRAINT IF EXISTS uniq_mcp_client_owner;

ALTER TABLE public.mcp_client
    ADD COLUMN IF NOT EXISTS client_key        text NOT NULL DEFAULT '',
    ADD COLUMN IF NOT EXISTS credential_digest text NOT NULL DEFAULT '',
    ADD COLUMN IF NOT EXISTS credential_prefix text NOT NULL DEFAULT '',
    ADD COLUMN IF NOT EXISTS expires_at        timestamptz,
    ADD COLUMN IF NOT EXISTS last_used_at      timestamptz,
    ADD COLUMN IF NOT EXISTS disabled          boolean NOT NULL DEFAULT false;

-- 存量行回填唯一 client_key（空默认值会撞唯一索引）；legacy 前缀标识非凭证行
UPDATE public.mcp_client SET client_key = 'mck-legacy-' || client_id::text
WHERE client_key = '';

CREATE UNIQUE INDEX IF NOT EXISTS mcp_client_client_key_uq
    ON public.mcp_client (client_key);
CREATE INDEX IF NOT EXISTS mcp_client_digest_idx
    ON public.mcp_client (credential_digest);
CREATE INDEX IF NOT EXISTS mcp_client_owner_idx
    ON public.mcp_client (owner_uid);
