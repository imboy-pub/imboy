-- 00000095_organization_foundation.down.sql
-- 安全回滚：先解除 workspace 归属（FK + 可空列），再删除 organization 表。
-- 不删除 workspace 任何既有行/列之外的产物；organization 表内的机构数据随表删除（expand-first 下未回填历史）。

ALTER TABLE workspace DROP CONSTRAINT IF EXISTS fk_workspace_organization;
DROP INDEX IF EXISTS i_workspace_organization_id;
ALTER TABLE workspace DROP COLUMN IF EXISTS organization_id;

DROP INDEX IF EXISTS i_organization_status;
DROP INDEX IF EXISTS i_organization_owner_id;
DROP TABLE IF EXISTS organization;
