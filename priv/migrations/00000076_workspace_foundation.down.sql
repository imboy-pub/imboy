-- 00000076_workspace_foundation.down.sql
-- 回滚工作区地基（workspace + workspace_member）
--
-- ⚠️ 回滚前置条件（部署顺序约束）：
--   必须先回滚应用代码到本迁移之前版本（无任何 workspace/workspace_member 引用），
--   再执行本回滚。00000077/00000078 已先行回滚（本表是其外键目标）。
-- 迁移契约：禁止 BEGIN/COMMIT（erlang_migrate 外层单事务包裹）。
-- 触发器 trg_workspace_member_remove_guard 定义在 00000077，随其先行删除；此处只回滚表。

-- 子表在前（FK 依赖逆序）
DROP TABLE IF EXISTS workspace_member;
DROP TABLE IF EXISTS workspace;
