-- ===================================================================
-- 群作业软删除补齐
-- Migration: 00000067_group_task_soft_delete.sql
-- Description: 为 group_task 增加 deleted_at 并补齐软删除查询索引
-- Date: 2026-02-23
-- ===================================================================

ALTER TABLE public.group_task
    ADD COLUMN IF NOT EXISTS deleted_at TIMESTAMPTZ DEFAULT NULL;

CREATE INDEX IF NOT EXISTS idx_group_task_deleted_at
    ON public.group_task (deleted_at)
    WHERE deleted_at IS NULL;

CREATE INDEX IF NOT EXISTS idx_group_task_group_status_alive
    ON public.group_task (group_id, status, id DESC)
    WHERE deleted_at IS NULL;

COMMENT ON COLUMN public.group_task.deleted_at IS '软删除时间戳，NULL 表示未删除';
