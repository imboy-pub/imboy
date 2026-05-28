-- 群作业功能表
-- 创建时间: 2026-02-17

-- 群作业表
CREATE TABLE IF NOT EXISTS group_task (
    id BIGSERIAL PRIMARY KEY,
    group_id BIGINT NOT NULL,
    task_id VARCHAR(40) NOT NULL UNIQUE,
    title VARCHAR(200) NOT NULL,
    description TEXT,
    creator_id BIGINT NOT NULL,
    deadline TIMESTAMPTZ,
    status SMALLINT DEFAULT 1,
    -- 状态: 1待完成 2进行中 3已截止
    attachment TEXT,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- 群作业分配表
CREATE TABLE IF NOT EXISTS group_task_assignment (
    id BIGSERIAL PRIMARY KEY,
    task_id VARCHAR(40) NOT NULL,
    user_id BIGINT NOT NULL,
    status SMALLINT DEFAULT 0,
    -- 状态: 0待完成 1进行中 2已提交 3已批改
    submitted_at TIMESTAMPTZ,
    content TEXT,
    -- 提交内容
    attachment TEXT,
    -- 附件URL
    score INTEGER,
    -- 评分
    comment TEXT,
    -- 评语
    reviewed_by BIGINT,
    -- 批改人ID
    reviewed_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
    UNIQUE(task_id, user_id)
);

-- 创建索引
CREATE INDEX IF NOT EXISTS idx_group_task_group_id ON group_task(group_id);
CREATE INDEX IF NOT EXISTS idx_group_task_task_id ON group_task(task_id);
CREATE INDEX IF NOT EXISTS idx_group_task_status ON group_task(status);
CREATE INDEX IF NOT EXISTS idx_group_task_assignment_task_id ON group_task_assignment(task_id);
CREATE INDEX IF NOT EXISTS idx_group_task_assignment_user_id ON group_task_assignment(user_id);
CREATE INDEX IF NOT EXISTS idx_group_task_assignment_status ON group_task_assignment(status);

-- 添加注释
COMMENT ON TABLE group_task IS '群作业表';
COMMENT ON TABLE group_task_assignment IS '群作业分配表';
COMMENT ON COLUMN group_task.task_id IS '作业唯一标识（HashID编码）';
COMMENT ON COLUMN group_task.status IS '状态: 1待完成 2进行中 3已截止';
COMMENT ON COLUMN group_task_assignment.status IS '状态: 0待完成 1进行中 2已提交 3已批改';

-- soft delete indexes (000434)
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
