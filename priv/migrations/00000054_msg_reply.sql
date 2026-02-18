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
