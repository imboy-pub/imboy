-- Table: public.group_notice

-- DROP TABLE IF EXISTS public."group_notice";

-- 导入数据后，需要更新自增长ID
-- select setval('"group_notice_id_seq"', (select max(id) from public."group_notice"));

CREATE TABLE IF NOT EXISTS public."group_notice"
(
    id BIGSERIAL PRIMARY KEY,
    group_id bigint NOT NULL,
    user_id bigint NOT NULL,
    edit_user_id bigint NOT NULL DEFAULT 0,
    body varchar(2000) DEFAULT '',
    status smallint NOT NULL DEFAULT 0,
    expired_at timestamptz DEFAULT CURRENT_TIMESTAMP NULL,
    updated_at timestamptz DEFAULT CURRENT_TIMESTAMP NULL,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.group_notice OWNER to imboy_user;

COMMENT ON TABLE public.group_notice IS '群组公告记录表';

COMMENT ON COLUMN public.group_notice.id IS '主键 自增长ID';

COMMENT ON COLUMN public.group_notice.group_id IS '群组ID';

COMMENT ON COLUMN public.group_notice.user_id IS '创建用户ID';

COMMENT ON COLUMN public.group_notice.body IS '公告类容';
COMMENT ON COLUMN public.group_notice.status IS '状态 0 待发布  1 已发布 2 取消发布';
COMMENT ON COLUMN public.group_notice.updated_at IS '有效期截止时间';
COMMENT ON COLUMN public.group_notice.updated_at IS '更新截止时间';
COMMENT ON COLUMN public.group_notice.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';

-- index
CREATE INDEX IF NOT EXISTS i_Gid_Status_ExpiredAt ON public.group_notice (group_id, status, expired_at asc);

-- notice enhancement indexes (000423)
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
