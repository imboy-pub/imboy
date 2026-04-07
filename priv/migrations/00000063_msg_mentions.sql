-- ====================================================================
-- @提及功能增强 - 添加 mentions 字段到 msg_c2g 表
-- ====================================================================
-- Migration: 00000053_msg_mentions.sql
-- Description: 添加 JSONB 类型的 mentions 字段到群聊消息表，用于存储 @提及信息
-- Author: TDD Implementation
-- Date: 2026-02-17
-- ====================================================================

-- 1. 添加 mentions 字段到 msg_c2g 表
ALTER TABLE public.msg_c2g ADD COLUMN IF NOT EXISTS mentions JSONB DEFAULT '[]';

-- 2. 添加注释
COMMENT ON COLUMN public.msg_c2g.mentions IS '@提及信息，JSON数组格式，包含被@的用户ID列表，例如: [123, 456] 或 ["all"] 表示@所有人';

-- 3. 创建 GIN 索引用于 JSONB 查询优化
CREATE INDEX IF NOT EXISTS i_msg_c2g_mentions ON public.msg_c2g USING GIN (mentions);

-- 4. 说明：
-- 这里不创建“按 uid 变量绑定”的表达式索引，PostgreSQL 不支持带运行时变量
-- 的索引定义语法。保留通用 GIN 索引即可覆盖 mentions 包含类查询。

-- 5. 创建@消息记录表（可选，用于优化查询性能）
CREATE TABLE IF NOT EXISTS public.msg_mention
(
    id BIGSERIAL PRIMARY KEY,
    msg_id varchar(40) NOT NULL,
    group_id bigint NOT NULL,
    mentioned_uid bigint NOT NULL,            -- 被@的用户ID
    from_uid bigint NOT NULL,                 -- 发送者ID
    is_read boolean NOT NULL DEFAULT false,   -- 是否已读
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,

    -- 注意：msg_c2g 使用复合主键 (id, created_at) 且 msg_id 无唯一约束，
    -- 因此不能建立外键引用。通过应用层保证一致性。
    CONSTRAINT uk_msg_mention UNIQUE (msg_id, mentioned_uid)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.msg_mention OWNER to imboy_user;

COMMENT ON TABLE public.msg_mention IS '@消息记录表，用于快速查询@我的消息';
COMMENT ON COLUMN public.msg_mention.msg_id IS '消息ID';
COMMENT ON COLUMN public.msg_mention.group_id IS '群组ID';
COMMENT ON COLUMN public.msg_mention.mentioned_uid IS '被@的用户ID';
COMMENT ON COLUMN public.msg_mention.from_uid IS '@消息的发送者ID';
COMMENT ON COLUMN public.msg_mention.is_read IS '是否已读';
COMMENT ON COLUMN public.msg_mention.created_at IS '创建时间';

-- 6. 创建索引用于查询@我的消息
CREATE INDEX IF NOT EXISTS i_msg_mention_uid_read ON public.msg_mention(mentioned_uid, is_read, created_at DESC);
CREATE INDEX IF NOT EXISTS i_msg_mention_group_uid ON public.msg_mention(group_id, mentioned_uid, created_at DESC);
CREATE INDEX IF NOT EXISTS i_msg_mention_msg_id ON public.msg_mention(msg_id);

-- ====================================================================
-- 迁移完成
-- ====================================================================
