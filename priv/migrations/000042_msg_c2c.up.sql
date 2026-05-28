-- Table: public.msg_c2c

-- 导入数据后，需要更新自增长ID
-- select setval('"msg_c2c_id_seq"', (select max(id) from public."msg_c2c"));

-- DROP TABLE IF EXISTS public."msg_c2c";

-- 1. 创建普通表（使用复合主键）
CREATE TABLE IF NOT EXISTS public.msg_c2c (
    id BIGSERIAL,
    from_id bigint NOT NULL,
    to_id bigint NOT NULL,
    msg_id varchar(40) NOT NULL,
    msg_type varchar(40) NOT NULL,
    e2ee JSONB,
    payload text NOT NULL,
    server_ts timestamptz DEFAULT CURRENT_TIMESTAMP,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (id, created_at)  -- 包含分区键的复合主键
);

ALTER TABLE IF EXISTS public.msg_c2c OWNER TO imboy_user;
-- 2. 创建其他索引
CREATE UNIQUE INDEX IF NOT EXISTS uk_c2c_MsgId_CreatedAt ON public.msg_c2c (msg_id, created_at);
CREATE INDEX IF NOT EXISTS i_c2c_ToId ON public.msg_c2c (to_id);
CREATE INDEX IF NOT EXISTS i_c2c_FromId ON public.msg_c2c (from_id);
CREATE INDEX IF NOT EXISTS i_c2c_MsgType ON public.msg_c2c (msg_type);
CREATE INDEX IF NOT EXISTS i_c2c_e2ee ON msg_c2c((e2ee IS NOT NULL)) WHERE e2ee IS NOT NULL;

-- 3. 转换为超表
SELECT create_hypertable(
    'msg_c2c',
    'created_at',
    chunk_time_interval => INTERVAL '7 days',
    if_not_exists => TRUE
);

-- 4. 设置压缩策略(根据实际数据特征调整)
ALTER TABLE msg_c2c SET (
    timescaledb.compress,
    timescaledb.compress_orderby = 'created_at DESC'
);

-- 5. 添加策略(可选)
SELECT add_compression_policy('msg_c2c', INTERVAL '3 days');
SELECT add_retention_policy('msg_c2c', INTERVAL '12 months');


-- 注释
COMMENT ON TABLE public.msg_c2c IS '单聊消息临时存储表';
COMMENT ON COLUMN public.msg_c2c.id IS '主键 自增长ID';

COMMENT ON COLUMN public.msg_c2c.from_id IS '消息发送人user id';
COMMENT ON COLUMN public.msg_c2c.to_id IS '消息接收人user_id';
COMMENT ON COLUMN public.msg_c2c.server_ts IS '消息服务器接受毫秒时间戳';

COMMENT ON COLUMN public.msg_c2c.msg_id IS '消息唯一标识';
COMMENT ON COLUMN public.msg_c2c.msg_type IS '消息格式类型： text image audio video file 等';

COMMENT ON COLUMN public.msg_c2c.payload IS '消息体明文json格式加密后的文本，数据结构参考文档';
COMMENT ON COLUMN public.msg_c2c.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';

-- fulltext search indexes (000521)
-- 消息全文搜索索引创建
-- 针对私聊消息和群聊消息创建中文分词的全文搜索索引

-- NOTE:
-- pure_migrations executes each SQL file in a transaction, and TimescaleDB
-- hypertables do not support CONCURRENTLY index creation.
CREATE INDEX IF NOT EXISTS idx_msg_c2c_payload_fts
ON msg_c2c USING GIN (to_tsvector('jiebacfg', payload::text));

CREATE INDEX IF NOT EXISTS idx_msg_c2g_payload_fts
ON msg_c2g USING GIN (to_tsvector('jiebacfg', payload::text));

-- pinned message indexes (000522)
-- 消息置顶功能数据库迁移
-- 添加 pinned 列并创建相应索引

-- 为单聊表添加置顶字段
ALTER TABLE msg_c2c ADD COLUMN IF NOT EXISTS pinned BOOLEAN DEFAULT FALSE;

-- 为群聊表添加置顶字段
ALTER TABLE msg_c2g ADD COLUMN IF NOT EXISTS pinned BOOLEAN DEFAULT FALSE;

-- 为单聊消息创建置顶索引（只索引已置顶的消息）
CREATE INDEX IF NOT EXISTS idx_msg_c2c_pinned ON msg_c2c(to_id, pinned) WHERE pinned = TRUE;

-- 为群聊消息创建置顶索引（只索引已置顶的消息）
CREATE INDEX IF NOT EXISTS idx_msg_c2g_pinned ON msg_c2g(to_id, pinned) WHERE pinned = TRUE;

-- reply column indexes (000523)
-- ================================================================
-- 引用回复功能迁移脚本
-- 为 msg_c2c 和 msg_c2g 表添加引用回复字段
--
-- 功能描述：
-- 1. 支持单聊和群聊消息引用回复
-- 2. 存储被引用消息的信息
-- 3. 支持嵌套引用（回复的回复）
--
-- 作者：Claude Code
-- 日期：2026-02-16
-- ================================================================

-- 为单聊消息表添加引用回复字段
ALTER TABLE public.msg_c2c ADD COLUMN IF NOT EXISTS reply_to_msg_id varchar(40);
ALTER TABLE public.msg_c2c ADD COLUMN IF NOT EXISTS reply_to_from_id bigint;
ALTER TABLE public.msg_c2c ADD COLUMN IF NOT EXISTS reply_snippet text;

COMMENT ON COLUMN public.msg_c2c.reply_to_msg_id IS '被引用回复的消息ID';
COMMENT ON COLUMN public.msg_c2c.reply_to_from_id IS '被引用消息的发送者ID';
COMMENT ON COLUMN public.msg_c2c.reply_snippet IS '被引用消息的摘要（前50字符）';

-- 为群聊消息表添加引用回复字段
ALTER TABLE public.msg_c2g ADD COLUMN IF NOT EXISTS reply_to_msg_id varchar(40);
ALTER TABLE public.msg_c2g ADD COLUMN IF NOT EXISTS reply_to_from_id bigint;
ALTER TABLE public.msg_c2g ADD COLUMN IF NOT EXISTS reply_snippet text;

COMMENT ON COLUMN public.msg_c2g.reply_to_msg_id IS '被引用回复的消息ID';
COMMENT ON COLUMN public.msg_c2g.reply_to_from_id IS '被引用消息的发送者ID';
COMMENT ON COLUMN public.msg_c2g.reply_snippet IS '被引用消息的摘要（前50字符）';

-- 添加索引以优化查询性能
CREATE INDEX IF NOT EXISTS idx_msg_c2c_reply ON public.msg_c2c(reply_to_msg_id) WHERE reply_to_msg_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_msg_c2g_reply ON public.msg_c2g(reply_to_msg_id) WHERE reply_to_msg_id IS NOT NULL;

-- burn after column/indexes (000527)
-- ============================================================
-- Migration: 00000077_msg_burn_after
-- Description: 消息自毁功能 — 添加过期时间字段
--
-- 设计说明：
--   expire_at 为 NULL 表示不自毁（默认行为）
--   expire_at 非 NULL 时，msg_burn_logic GenServer 定期扫描并删除
--   客户端发送消息时可指定 expire_secs (5/30/60/300/3600/86400)
--   服务端计算 expire_at = created_at + expire_secs
-- ============================================================

-- 1. msg_c2c 添加 expire_at
ALTER TABLE public.msg_c2c ADD COLUMN IF NOT EXISTS expire_at TIMESTAMPTZ DEFAULT NULL;

-- 2. msg_c2g 添加 expire_at
ALTER TABLE public.msg_c2g ADD COLUMN IF NOT EXISTS expire_at TIMESTAMPTZ DEFAULT NULL;

-- 3. 索引加速过期查询（部分索引，只索引有自毁设置的消息）
CREATE INDEX IF NOT EXISTS i_c2c_expire_at
    ON public.msg_c2c (expire_at) WHERE expire_at IS NOT NULL;

CREATE INDEX IF NOT EXISTS i_c2g_expire_at
    ON public.msg_c2g (expire_at) WHERE expire_at IS NOT NULL;

-- 4. msg_store 永久归档表也添加（审计需要知道原始自毁设置）
ALTER TABLE public.msg_store ADD COLUMN IF NOT EXISTS expire_at TIMESTAMPTZ DEFAULT NULL;

COMMENT ON COLUMN public.msg_c2c.expire_at IS '消息自毁时间，NULL=不自毁';
COMMENT ON COLUMN public.msg_c2g.expire_at IS '消息自毁时间，NULL=不自毁';
COMMENT ON COLUMN public.msg_store.expire_at IS '消息原始自毁时间（归档保留）';
