-- Migration: user_group_category table
-- 群组分类表：用户可以将群组分配到不同分类中
-- Date: 2026-02-17

-- Table: public.user_group_category
CREATE TABLE IF NOT EXISTS public."user_group_category"
(
    id BIGSERIAL PRIMARY KEY,
    user_id bigint NOT NULL,
    category_name varchar(50) NOT NULL,
    sort_order integer DEFAULT 0,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE IF EXISTS public.user_group_category OWNER to imboy_user;

COMMENT ON TABLE public.user_group_category IS '群组分类表：用户自定义群组分组';
COMMENT ON COLUMN public.user_group_category.id IS '主键，自增长ID';
COMMENT ON COLUMN public.user_group_category.user_id IS '分类所属用户ID';
COMMENT ON COLUMN public.user_group_category.category_name IS '分类名称';
COMMENT ON COLUMN public.user_group_category.sort_order IS '排序顺序，数字越小越靠前';
COMMENT ON COLUMN public.user_group_category.created_at IS '创建时间';

-- Index: idx_user_group_category_user
-- 用户分组查询优化索引
CREATE INDEX IF NOT EXISTS idx_user_group_category_user ON public.user_group_category(user_id, sort_order);

-- Unique constraint: 用户下分类名称唯一
CREATE UNIQUE INDEX IF NOT EXISTS idx_user_group_category_user_name ON public.user_group_category(user_id, category_name);

-- Add category_id column to group_member table
-- 群组成员关联的分组ID
ALTER TABLE public.group_member ADD COLUMN IF NOT EXISTS category_id bigint DEFAULT 0;

COMMENT ON COLUMN public.group_member.category_id IS '群组分类ID，0表示未分类';
