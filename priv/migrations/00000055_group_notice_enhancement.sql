-- ===================================================================
-- 群公告功能完善 - 添加新字段
-- Migration: 00000055_group_notice_enhancement.sql
-- Description: 为 group_notice 表添加置顶、软删除、已读统计等功能
-- Date: 2026-02-16
-- ===================================================================

-- 添加 pinned 字段（置顶标记）
ALTER TABLE public.group_notice ADD COLUMN IF NOT EXISTS pinned BOOLEAN DEFAULT FALSE;

-- 添加 deleted_at 字段（软删除）
ALTER TABLE public.group_notice ADD COLUMN IF NOT EXISTS deleted_at TIMESTAMPTZ DEFAULT NULL;

-- 添加 read_count 字段（已读数量）
ALTER TABLE public.group_notice ADD COLUMN IF NOT EXISTS read_count INTEGER DEFAULT 0;

-- 添加 title 字段（公告标题）
ALTER TABLE public.group_notice ADD COLUMN IF NOT EXISTS title VARCHAR(200) DEFAULT '';

-- 添加索引：优化置顶公告查询
CREATE INDEX IF NOT EXISTS i_group_notice_pinned ON public.group_notice (group_id, pinned, deleted_at) WHERE deleted_at IS NULL;

-- 添加索引：优化软删除查询
CREATE INDEX IF NOT EXISTS i_group_notice_deleted_at ON public.group_notice (deleted_at) WHERE deleted_at IS NULL;

-- 添加注释
COMMENT ON COLUMN public.group_notice.pinned IS '是否置顶';
COMMENT ON COLUMN public.group_notice.deleted_at IS '软删除时间戳';
COMMENT ON COLUMN public.group_notice.read_count IS '已读用户数量';
COMMENT ON COLUMN public.group_notice.title IS '公告标题';

-- ===================================================================
-- 说明：
-- 1. pinned: 用于标记公告是否置顶，置顶的公告在列表中排在最前面
-- 2. deleted_at: 用于软删除，NULL 表示未删除，非 NULL 表示已删除
-- 3. read_count: 记录已阅读该公告的用户数量
-- 4. title: 公告标题，方便用户快速了解公告内容
-- 5. 索引优化了置顶和未删除的查询性能
-- ===================================================================
