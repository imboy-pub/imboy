-- 00000078_project_foundation.down.sql
-- 回滚项目地基（project_event / project_task / project，W0 版）
--
-- ⚠️ 回滚前置条件（部署顺序约束）：
--   必须先回滚应用代码到本迁移之前版本（无任何 project/project_task/project_event 引用），
--   再执行本回滚。本文件不触碰 workspace/workspace_member/channel/"group"（属 76/77）。
-- 迁移契约：禁止 BEGIN/COMMIT（erlang_migrate 外层单事务包裹）。
-- 数据说明：down 删除三表即丢弃全部项目/任务/事件数据（不可逆）；生产回滚只能前滚兼容。

-- Phase 1: 触发器与函数（project 表上的约束触发器随表删除，此处显式先删以保幂等语义清晰）
DROP TRIGGER IF EXISTS trg_project_owner_membership_active ON project;
DROP FUNCTION IF EXISTS fn_project_owner_membership_active();

-- Phase 2: 表（FK 依赖逆序：事件/任务 → 项目）
DROP TABLE IF EXISTS project_event;
DROP TABLE IF EXISTS project_task;
DROP TABLE IF EXISTS project;
