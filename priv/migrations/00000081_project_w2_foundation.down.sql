-- 00000081_project_w2_foundation.down.sql
-- Channel-first-class W2 回滚：删除 project_member / project_milestone / project_channel_rel /
-- project.links / project_event W2 事件值 / channel 复合唯一，恢复 W0 schema 形态。
--
-- 回滚顺序：触发器/函数 → 表（子表先于引用目标）→ links 列 → event CHECK → channel 约束。
--
-- ⚠️ 数据语义：
--   * 三张新表删除即丢失其数据（成员/里程碑/关联），符合"新能力整体回滚"预期。
--   * chk_project_event_type 恢复 W0 五值：若库中已写入 W2 事件类型行，恢复 CHECK 将失败
--     （23514）——这是有意的安全特性（不静默删除审计数据）；此时应先归档/迁移事件行再执行 down。
--   * project.links 列删除即丢失手工链接数据（Resources 聚合载体）。

-- ============================================================
-- Phase 6 逆序：project_event CHECK 恢复 W0 五值
-- ============================================================
ALTER TABLE project_event DROP CONSTRAINT IF EXISTS chk_project_event_type;
ALTER TABLE project_event ADD CONSTRAINT chk_project_event_type CHECK (
    event_type = ANY (ARRAY[
        'project_created'::text,
        'project_status'::text,
        'task_created'::text,
        'task_status'::text,
        'task_assignee'::text
    ]));

-- ============================================================
-- Phase 5 逆序：links 形状触发器 + project.links 列
-- ============================================================
DROP TRIGGER IF EXISTS trg_project_links_shape ON project;
DROP FUNCTION IF EXISTS fn_project_links_shape_check();
ALTER TABLE project DROP COLUMN IF EXISTS links;

-- ============================================================
-- Phase 4 逆序：project_channel_rel（子表，先删）
-- ============================================================
DROP TABLE IF EXISTS project_channel_rel;

-- ============================================================
-- Phase 3 逆序：project_milestone
-- ============================================================
DROP TABLE IF EXISTS project_milestone;

-- ============================================================
-- Phase 2 逆序：project_member（触发器/函数先于表删除）
-- ============================================================
DROP TRIGGER IF EXISTS trg_workspace_member_remove_guard_pm ON workspace_member;
DROP FUNCTION IF EXISTS fn_workspace_member_remove_guard_pm();
DROP TRIGGER IF EXISTS trg_project_member_ws_active ON project_member;
DROP FUNCTION IF EXISTS fn_project_member_ws_active_check();
DROP TABLE IF EXISTS project_member;

-- ============================================================
-- Phase 1 逆序：channel 复合唯一
-- ============================================================
ALTER TABLE channel DROP CONSTRAINT IF EXISTS uk_channel_id_workspace;
