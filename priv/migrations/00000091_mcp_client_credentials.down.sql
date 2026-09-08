-- MCP-01 down：删除凭证列与索引。
-- 注意：owner 唯一约束不在此恢复（历史数据可能已有同 owner 多 client，
-- 重建唯一约束会失败）；如需恢复单 client 语义应先人工清重。
DROP INDEX IF EXISTS public.mcp_client_owner_idx;
DROP INDEX IF EXISTS public.mcp_client_digest_idx;
DROP INDEX IF EXISTS public.mcp_client_client_key_uq;

ALTER TABLE public.mcp_client
    DROP COLUMN IF EXISTS disabled,
    DROP COLUMN IF EXISTS last_used_at,
    DROP COLUMN IF EXISTS expires_at,
    DROP COLUMN IF EXISTS credential_prefix,
    DROP COLUMN IF EXISTS credential_digest,
    DROP COLUMN IF EXISTS client_key;
