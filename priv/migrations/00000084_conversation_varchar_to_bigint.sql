-- Migration: 00000084_conversation_varchar_to_bigint.sql
-- 将 conversation 系列表的 varchar ID 字段迁移为 bigint (TSID)
-- 前提: TSID 迁移(00000080)已将 id 列从 BIGSERIAL 改为 BIGINT
-- 本迁移处理剩余的 varchar(40) ID 字段

-- ============================================================
-- 1. conversation 表 — user_id, peer_id, last_msg_id
-- ============================================================

-- user_id: varchar(40) → bigint
ALTER TABLE IF EXISTS public.conversation
    ALTER COLUMN user_id TYPE bigint USING NULLIF(user_id, '')::bigint;
ALTER TABLE IF EXISTS public.conversation
    ALTER COLUMN user_id SET DEFAULT 0;

-- peer_id: varchar(40) → bigint
ALTER TABLE IF EXISTS public.conversation
    ALTER COLUMN peer_id TYPE bigint USING NULLIF(peer_id, '')::bigint;
ALTER TABLE IF EXISTS public.conversation
    ALTER COLUMN peer_id SET DEFAULT 0;

-- last_msg_id: varchar(40) → bigint
ALTER TABLE IF EXISTS public.conversation
    ALTER COLUMN last_msg_id TYPE bigint USING NULLIF(last_msg_id, '')::bigint;
ALTER TABLE IF EXISTS public.conversation
    ALTER COLUMN last_msg_id SET DEFAULT 0;

-- 重建唯一索引（类型变更后自动重建）
DROP INDEX IF EXISTS uk_cvt_UserId_Type_PeerId;
CREATE UNIQUE INDEX IF NOT EXISTS uk_cvt_UserId_Type_PeerId
    ON public.conversation (user_id, type, peer_id);

-- ============================================================
-- 2. conversation_pin 表 — conversation_id
-- ============================================================

-- 删除依赖 conversation_id 的约束和索引
ALTER TABLE IF EXISTS public.conversation_pin
    DROP CONSTRAINT IF EXISTS conversation_pin_user_id_conversation_id_conversation_type_key;
DROP INDEX IF EXISTS idx_conversation_pin_conversation;

-- conversation_id: varchar(40) → bigint
ALTER TABLE IF EXISTS public.conversation_pin
    ALTER COLUMN conversation_id TYPE bigint USING conversation_id::bigint;

-- 重建唯一约束和索引
ALTER TABLE public.conversation_pin
    ADD CONSTRAINT conversation_pin_user_conversation_uk
    UNIQUE (user_id, conversation_id, conversation_type);

CREATE INDEX IF NOT EXISTS idx_conversation_pin_conversation
    ON public.conversation_pin (conversation_id, conversation_type);

-- ============================================================
-- 3. conversation_delete 表 — conversation_id
-- ============================================================

-- 删除依赖 conversation_id 的约束和索引
ALTER TABLE IF EXISTS public.conversation_delete
    DROP CONSTRAINT IF EXISTS conversation_delete_user_id_conversation_id_conversation_ty_key;
DROP INDEX IF EXISTS idx_conversation_delete_conversation;

-- conversation_id: varchar(40) → bigint
ALTER TABLE IF EXISTS public.conversation_delete
    ALTER COLUMN conversation_id TYPE bigint USING conversation_id::bigint;

-- 重建唯一约束和索引
ALTER TABLE public.conversation_delete
    ADD CONSTRAINT conversation_delete_user_conversation_uk
    UNIQUE (user_id, conversation_id, conversation_type);

CREATE INDEX IF NOT EXISTS idx_conversation_delete_conversation
    ON public.conversation_delete (conversation_id, conversation_type);

-- ============================================================
-- 更新注释
-- ============================================================
COMMENT ON COLUMN public.conversation.user_id IS '发起会话用户ID (TSID bigint)';
COMMENT ON COLUMN public.conversation.peer_id IS '对端ID (TSID bigint)';
COMMENT ON COLUMN public.conversation.last_msg_id IS '最后消息ID (TSID bigint)';
COMMENT ON COLUMN public.conversation_pin.conversation_id IS '会话ID (TSID bigint，单聊为对方UID，群聊为群ID)';
COMMENT ON COLUMN public.conversation_delete.conversation_id IS '会话ID (TSID bigint，单聊为对方UID，群聊为群ID)';
