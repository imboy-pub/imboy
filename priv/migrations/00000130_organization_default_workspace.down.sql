-- 迁移 00000130 down：回滚 Organization Default Workspace（纯 expand 迁移）。
-- 关系数据随表删除；workspace(organization_id, id) 唯一索引一并移除。
-- 有数据后按计划 ORG-05 ROLLBACK：可短期切回 legacy min-ID 读法过渡，
-- 不做破坏性生产 down——本文件仅在本迁移是当前 head 时被 erlang_migrate（strict）执行。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

DROP TRIGGER IF EXISTS trg_organization_default_workspace_active_guard
    ON organization_default_workspace;
DROP FUNCTION IF EXISTS fn_organization_default_workspace_active_guard();

DROP TABLE IF EXISTS organization_default_workspace;

DROP INDEX IF EXISTS uq_workspace_org_id;
