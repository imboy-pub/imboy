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
