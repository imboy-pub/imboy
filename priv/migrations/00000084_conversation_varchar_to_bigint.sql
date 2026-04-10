-- Migration: 00000084_conversation_varchar_to_bigint.sql
-- 将 conversation 系列表的 varchar ID 字段迁移为 bigint (TSID)
-- 前提: TSID 迁移(00000080)已将 id 列从 BIGSERIAL 改为 BIGINT
-- 本迁移处理剩余的 varchar(40) ID 字段
--
-- 执行建议：在维护窗口执行（ALTER COLUMN TYPE 会锁表重写）
-- 执行前备份：pg_dump -Fc -t conversation -t conversation_pin -t conversation_delete imboy > pre_084_backup.dump

BEGIN;

-- ============================================================
-- 0. 预检查：仅在列类型为 varchar 时执行迁移（幂等保护）
-- ============================================================

DO $$
DECLARE
    v_user_id_type text;
BEGIN
    SELECT data_type INTO v_user_id_type
    FROM information_schema.columns
    WHERE table_schema = 'public'
      AND table_name = 'conversation'
      AND column_name = 'user_id';

    -- 如果已经是 bigint，跳过整个迁移
    IF v_user_id_type = 'bigint' THEN
        RAISE NOTICE 'conversation.user_id already bigint, skipping migration';
        RETURN;
    END IF;

    -- ============================================================
    -- 1. conversation 表 — 清理空字符串后迁移
    -- ============================================================

    -- 清理空字符串，统一为 '0'（避免 NULL 语义歧义）
    UPDATE public.conversation SET user_id = '0' WHERE user_id = '' OR user_id IS NULL;
    UPDATE public.conversation SET peer_id = '0' WHERE peer_id = '' OR peer_id IS NULL;
    UPDATE public.conversation SET last_msg_id = '0' WHERE last_msg_id = '' OR last_msg_id IS NULL;

    -- user_id: varchar(40) → bigint
    -- 必须先 DROP DEFAULT，否则 varchar 默认值（如 ''）无法自动 cast 到 bigint
    ALTER TABLE public.conversation
        ALTER COLUMN user_id DROP DEFAULT;
    ALTER TABLE public.conversation
        ALTER COLUMN user_id TYPE bigint USING NULLIF(user_id, '')::bigint;
    ALTER TABLE public.conversation
        ALTER COLUMN user_id SET DEFAULT 0;
    ALTER TABLE public.conversation
        ALTER COLUMN user_id SET NOT NULL;

    -- peer_id: varchar(40) → bigint
    ALTER TABLE public.conversation
        ALTER COLUMN peer_id DROP DEFAULT;
    ALTER TABLE public.conversation
        ALTER COLUMN peer_id TYPE bigint USING NULLIF(peer_id, '')::bigint;
    ALTER TABLE public.conversation
        ALTER COLUMN peer_id SET DEFAULT 0;
    ALTER TABLE public.conversation
        ALTER COLUMN peer_id SET NOT NULL;

    -- last_msg_id: varchar(40) → bigint（允许 0 表示无消息）
    ALTER TABLE public.conversation
        ALTER COLUMN last_msg_id DROP DEFAULT;
    ALTER TABLE public.conversation
        ALTER COLUMN last_msg_id TYPE bigint USING NULLIF(last_msg_id, '')::bigint;
    ALTER TABLE public.conversation
        ALTER COLUMN last_msg_id SET DEFAULT 0;

END $$;

-- 重建唯一索引（幂等）
DROP INDEX IF EXISTS uk_cvt_UserId_Type_PeerId;
CREATE UNIQUE INDEX IF NOT EXISTS uk_cvt_UserId_Type_PeerId
    ON public.conversation (user_id, type, peer_id);

-- ============================================================
-- 2. conversation_pin 表 — conversation_id
-- ============================================================

DO $$
DECLARE
    v_col_type text;
BEGIN
    SELECT data_type INTO v_col_type
    FROM information_schema.columns
    WHERE table_schema = 'public'
      AND table_name = 'conversation_pin'
      AND column_name = 'conversation_id';

    IF v_col_type = 'bigint' THEN
        RAISE NOTICE 'conversation_pin.conversation_id already bigint, skipping';
        RETURN;
    END IF;

    -- 清理空字符串
    UPDATE public.conversation_pin SET conversation_id = '0'
    WHERE conversation_id = '' OR conversation_id IS NULL;

    -- 删除依赖 conversation_id 的约束和索引
    ALTER TABLE public.conversation_pin
        DROP CONSTRAINT IF EXISTS conversation_pin_user_id_conversation_id_conversation_type_key;
    ALTER TABLE public.conversation_pin
        DROP CONSTRAINT IF EXISTS conversation_pin_user_conversation_uk;
    DROP INDEX IF EXISTS idx_conversation_pin_conversation;

    -- conversation_id: varchar(40) → bigint
    ALTER TABLE public.conversation_pin
        ALTER COLUMN conversation_id DROP DEFAULT;
    ALTER TABLE public.conversation_pin
        ALTER COLUMN conversation_id TYPE bigint USING NULLIF(conversation_id, '')::bigint;

    -- 重建唯一约束
    ALTER TABLE public.conversation_pin
        ADD CONSTRAINT conversation_pin_user_conversation_uk
        UNIQUE (user_id, conversation_id, conversation_type);

    CREATE INDEX IF NOT EXISTS idx_conversation_pin_conversation
        ON public.conversation_pin (conversation_id, conversation_type);
END $$;

-- ============================================================
-- 3. conversation_delete 表 — conversation_id
-- ============================================================

DO $$
DECLARE
    v_col_type text;
BEGIN
    SELECT data_type INTO v_col_type
    FROM information_schema.columns
    WHERE table_schema = 'public'
      AND table_name = 'conversation_delete'
      AND column_name = 'conversation_id';

    IF v_col_type = 'bigint' THEN
        RAISE NOTICE 'conversation_delete.conversation_id already bigint, skipping';
        RETURN;
    END IF;

    -- 清理空字符串
    UPDATE public.conversation_delete SET conversation_id = '0'
    WHERE conversation_id = '' OR conversation_id IS NULL;

    -- 删除依赖 conversation_id 的约束和索引
    ALTER TABLE public.conversation_delete
        DROP CONSTRAINT IF EXISTS conversation_delete_user_id_conversation_id_conversation_ty_key;
    ALTER TABLE public.conversation_delete
        DROP CONSTRAINT IF EXISTS conversation_delete_user_conversation_uk;
    DROP INDEX IF EXISTS idx_conversation_delete_conversation;

    -- conversation_id: varchar(40) → bigint
    ALTER TABLE public.conversation_delete
        ALTER COLUMN conversation_id DROP DEFAULT;
    ALTER TABLE public.conversation_delete
        ALTER COLUMN conversation_id TYPE bigint USING NULLIF(conversation_id, '')::bigint;

    -- 重建唯一约束
    ALTER TABLE public.conversation_delete
        ADD CONSTRAINT conversation_delete_user_conversation_uk
        UNIQUE (user_id, conversation_id, conversation_type);

    CREATE INDEX IF NOT EXISTS idx_conversation_delete_conversation
        ON public.conversation_delete (conversation_id, conversation_type);
END $$;

-- ============================================================
-- 更新注释
-- ============================================================
COMMENT ON COLUMN public.conversation.user_id IS '发起会话用户ID (TSID bigint)';
COMMENT ON COLUMN public.conversation.peer_id IS '对端ID (TSID bigint)';
COMMENT ON COLUMN public.conversation.last_msg_id IS '最后消息ID (TSID bigint)';
COMMENT ON COLUMN public.conversation_pin.conversation_id IS '会话ID (TSID bigint，单聊为对方UID，群聊为群ID)';
COMMENT ON COLUMN public.conversation_delete.conversation_id IS '会话ID (TSID bigint，单聊为对方UID，群聊为群ID)';

COMMIT;
