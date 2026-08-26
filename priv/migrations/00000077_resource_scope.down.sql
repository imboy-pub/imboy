-- 00000077_resource_scope.down.sql
-- 回滚 channel/"group" 的 scope 归属列与子集触发器
--
-- ⚠️ 回滚前置条件（部署顺序约束）：
--   必须先回滚应用代码到本迁移之前版本（无任何 channel/group 的 scope/workspace_id 引用），
--   再执行本回滚。00000078 已先行回滚（其 project 外键目标 workspace 不受本文件影响，
--   但 project.owner 复合外键引用 workspace_member，与本文件无依赖；仍按 78→77→76 逆序）。
-- 迁移契约：禁止 BEGIN/COMMIT（erlang_migrate 外层单事务包裹）。
-- 数据说明：down 删除 scope/workspace_id 列即丢弃归属信息（不可逆）；workspace 群回到普通群。

-- ============================================================
-- Phase 1: 移除子集触发器（逻辑变更②④ 的触发器部分）
-- ============================================================
DROP TRIGGER IF EXISTS trg_workspace_member_remove_guard ON workspace_member;
DROP TRIGGER IF EXISTS trg_group_member_ws_subset ON group_member;
DROP FUNCTION IF EXISTS fn_workspace_member_remove_guard();
DROP FUNCTION IF EXISTS fn_group_member_ws_subset_check();

-- ============================================================
-- Phase 2: 回滚 "group" 归属列（逆依赖：先索引/约束/外键，后列）
-- ============================================================
ALTER TABLE "group" DROP CONSTRAINT IF EXISTS fk_group_workspace;
DROP INDEX IF EXISTS i_group_scope_ws;
DROP INDEX IF EXISTS i_group_scope_personal;
ALTER TABLE "group" DROP CONSTRAINT IF EXISTS chk_group_scope_xor;
ALTER TABLE "group" DROP COLUMN IF EXISTS workspace_id;
ALTER TABLE "group" DROP COLUMN IF EXISTS scope;

-- ============================================================
-- Phase 3: 回滚 channel 归属列
-- ============================================================
ALTER TABLE channel DROP CONSTRAINT IF EXISTS fk_channel_workspace;
DROP INDEX IF EXISTS i_channel_scope_ws;
DROP INDEX IF EXISTS i_channel_scope_personal;
ALTER TABLE channel DROP CONSTRAINT IF EXISTS chk_channel_scope_xor;
ALTER TABLE channel DROP COLUMN IF EXISTS workspace_id;
ALTER TABLE channel DROP COLUMN IF EXISTS scope;
