-- ============================================================
-- Migration 000069: 延迟执行的跨表补丁 / TSID 类型改造
--
-- 背景：以下补丁原内联在较早编号的建表迁移中：
--   - 000042_msg_c2c：fts / pinned / reply / burn_after 同时操作
--                     msg_c2g(000043)、msg_store(000052)
--   - 000039_conversation：varchar→bigint(TSID) 改造涉及
--                     conversation_pin(000040)、conversation_delete(000041)
-- 它们引用了编号更大的迁移才创建的表，导致全新库按编号顺序 up 时
-- 因 "relation does not exist" 失败。现统一收敛到此处（编号最大），
-- 确保所有依赖表均已创建后再执行。全部语句均为幂等（IF [NOT] EXISTS /
-- information_schema 预检查），可安全重跑。
-- ============================================================

-- ============================================================
-- 段 1：来自 000042 的 msg 跨表补丁
-- ============================================================
BEGIN;

-- fulltext search indexes (000521)
-- 针对私聊/群聊消息创建中文分词(jiebacfg)的全文搜索索引
-- NOTE: TimescaleDB hypertable 不支持 CONCURRENTLY 建索引，故在事务内普通创建
CREATE INDEX IF NOT EXISTS idx_msg_c2c_payload_fts
ON msg_c2c USING GIN (to_tsvector('jiebacfg', payload::text));

CREATE INDEX IF NOT EXISTS idx_msg_c2g_payload_fts
ON msg_c2g USING GIN (to_tsvector('jiebacfg', payload::text));

-- pinned message indexes (000522)
ALTER TABLE msg_c2c ADD COLUMN IF NOT EXISTS pinned BOOLEAN DEFAULT FALSE;
ALTER TABLE msg_c2g ADD COLUMN IF NOT EXISTS pinned BOOLEAN DEFAULT FALSE;
CREATE INDEX IF NOT EXISTS idx_msg_c2c_pinned ON msg_c2c(to_id, pinned) WHERE pinned = TRUE;
CREATE INDEX IF NOT EXISTS idx_msg_c2g_pinned ON msg_c2g(to_id, pinned) WHERE pinned = TRUE;

-- reply column indexes (000523) 引用回复功能
ALTER TABLE public.msg_c2c ADD COLUMN IF NOT EXISTS reply_to_msg_id varchar(40);
ALTER TABLE public.msg_c2c ADD COLUMN IF NOT EXISTS reply_to_from_id bigint;
ALTER TABLE public.msg_c2c ADD COLUMN IF NOT EXISTS reply_snippet text;

COMMENT ON COLUMN public.msg_c2c.reply_to_msg_id IS '被引用回复的消息ID';
COMMENT ON COLUMN public.msg_c2c.reply_to_from_id IS '被引用消息的发送者ID';
COMMENT ON COLUMN public.msg_c2c.reply_snippet IS '被引用消息的摘要（前50字符）';

ALTER TABLE public.msg_c2g ADD COLUMN IF NOT EXISTS reply_to_msg_id varchar(40);
ALTER TABLE public.msg_c2g ADD COLUMN IF NOT EXISTS reply_to_from_id bigint;
ALTER TABLE public.msg_c2g ADD COLUMN IF NOT EXISTS reply_snippet text;

COMMENT ON COLUMN public.msg_c2g.reply_to_msg_id IS '被引用回复的消息ID';
COMMENT ON COLUMN public.msg_c2g.reply_to_from_id IS '被引用消息的发送者ID';
COMMENT ON COLUMN public.msg_c2g.reply_snippet IS '被引用消息的摘要（前50字符）';

CREATE INDEX IF NOT EXISTS idx_msg_c2c_reply ON public.msg_c2c(reply_to_msg_id) WHERE reply_to_msg_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_msg_c2g_reply ON public.msg_c2g(reply_to_msg_id) WHERE reply_to_msg_id IS NOT NULL;

-- burn after column/indexes (000527) 消息自毁
ALTER TABLE public.msg_c2c ADD COLUMN IF NOT EXISTS expire_at TIMESTAMPTZ DEFAULT NULL;
ALTER TABLE public.msg_c2g ADD COLUMN IF NOT EXISTS expire_at TIMESTAMPTZ DEFAULT NULL;

CREATE INDEX IF NOT EXISTS i_c2c_expire_at
    ON public.msg_c2c (expire_at) WHERE expire_at IS NOT NULL;
CREATE INDEX IF NOT EXISTS i_c2g_expire_at
    ON public.msg_c2g (expire_at) WHERE expire_at IS NOT NULL;

-- msg_store 永久归档表也添加（审计需要知道原始自毁设置）
ALTER TABLE public.msg_store ADD COLUMN IF NOT EXISTS expire_at TIMESTAMPTZ DEFAULT NULL;

COMMENT ON COLUMN public.msg_c2c.expire_at IS '消息自毁时间，NULL=不自毁';
COMMENT ON COLUMN public.msg_c2g.expire_at IS '消息自毁时间，NULL=不自毁';
COMMENT ON COLUMN public.msg_store.expire_at IS '消息原始自毁时间（归档保留）';

COMMIT;

-- ============================================================
-- 段 2：来自 000039 的 conversation 系列 varchar→bigint(TSID) 改造
-- 前提：conversation(000039) / conversation_pin(000040) / conversation_delete(000041) 均已创建
-- 幂等保护：仅当列类型仍为 varchar 时执行
-- ============================================================
BEGIN;

-- 1. conversation 表
DO $$
DECLARE
    v_user_id_type text;
BEGIN
    SELECT data_type INTO v_user_id_type
    FROM information_schema.columns
    WHERE table_schema = 'public'
      AND table_name = 'conversation'
      AND column_name = 'user_id';

    IF v_user_id_type = 'bigint' THEN
        RAISE NOTICE 'conversation.user_id already bigint, skipping migration';
        RETURN;
    END IF;

    UPDATE public.conversation SET user_id = '0' WHERE user_id = '' OR user_id IS NULL;
    UPDATE public.conversation SET peer_id = '0' WHERE peer_id = '' OR peer_id IS NULL;
    UPDATE public.conversation SET last_msg_id = '0' WHERE last_msg_id = '' OR last_msg_id IS NULL;

    ALTER TABLE public.conversation ALTER COLUMN user_id DROP DEFAULT;
    ALTER TABLE public.conversation ALTER COLUMN user_id TYPE bigint USING NULLIF(user_id, '')::bigint;
    ALTER TABLE public.conversation ALTER COLUMN user_id SET DEFAULT 0;
    ALTER TABLE public.conversation ALTER COLUMN user_id SET NOT NULL;

    ALTER TABLE public.conversation ALTER COLUMN peer_id DROP DEFAULT;
    ALTER TABLE public.conversation ALTER COLUMN peer_id TYPE bigint USING NULLIF(peer_id, '')::bigint;
    ALTER TABLE public.conversation ALTER COLUMN peer_id SET DEFAULT 0;
    ALTER TABLE public.conversation ALTER COLUMN peer_id SET NOT NULL;

    ALTER TABLE public.conversation ALTER COLUMN last_msg_id DROP DEFAULT;
    ALTER TABLE public.conversation ALTER COLUMN last_msg_id TYPE bigint USING NULLIF(last_msg_id, '')::bigint;
    ALTER TABLE public.conversation ALTER COLUMN last_msg_id SET DEFAULT 0;
END $$;

DROP INDEX IF EXISTS uk_cvt_UserId_Type_PeerId;
CREATE UNIQUE INDEX IF NOT EXISTS uk_cvt_UserId_Type_PeerId
    ON public.conversation (user_id, type, peer_id);

-- 2. conversation_pin 表
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

    UPDATE public.conversation_pin SET conversation_id = '0'
    WHERE conversation_id = '' OR conversation_id IS NULL;

    ALTER TABLE public.conversation_pin
        DROP CONSTRAINT IF EXISTS conversation_pin_user_id_conversation_id_conversation_type_key;
    ALTER TABLE public.conversation_pin
        DROP CONSTRAINT IF EXISTS conversation_pin_user_conversation_uk;
    DROP INDEX IF EXISTS idx_conversation_pin_conversation;

    ALTER TABLE public.conversation_pin ALTER COLUMN conversation_id DROP DEFAULT;
    ALTER TABLE public.conversation_pin
        ALTER COLUMN conversation_id TYPE bigint USING NULLIF(conversation_id, '')::bigint;

    ALTER TABLE public.conversation_pin
        ADD CONSTRAINT conversation_pin_user_conversation_uk
        UNIQUE (user_id, conversation_id, conversation_type);

    CREATE INDEX IF NOT EXISTS idx_conversation_pin_conversation
        ON public.conversation_pin (conversation_id, conversation_type);
END $$;

-- 3. conversation_delete 表
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

    UPDATE public.conversation_delete SET conversation_id = '0'
    WHERE conversation_id = '' OR conversation_id IS NULL;

    ALTER TABLE public.conversation_delete
        DROP CONSTRAINT IF EXISTS conversation_delete_user_id_conversation_id_conversation_ty_key;
    ALTER TABLE public.conversation_delete
        DROP CONSTRAINT IF EXISTS conversation_delete_user_conversation_uk;
    DROP INDEX IF EXISTS idx_conversation_delete_conversation;

    ALTER TABLE public.conversation_delete ALTER COLUMN conversation_id DROP DEFAULT;
    ALTER TABLE public.conversation_delete
        ALTER COLUMN conversation_id TYPE bigint USING NULLIF(conversation_id, '')::bigint;

    ALTER TABLE public.conversation_delete
        ADD CONSTRAINT conversation_delete_user_conversation_uk
        UNIQUE (user_id, conversation_id, conversation_type);

    CREATE INDEX IF NOT EXISTS idx_conversation_delete_conversation
        ON public.conversation_delete (conversation_id, conversation_type);
END $$;

COMMENT ON COLUMN public.conversation.user_id IS '发起会话用户ID (TSID bigint)';
COMMENT ON COLUMN public.conversation.peer_id IS '对端ID (TSID bigint)';
COMMENT ON COLUMN public.conversation.last_msg_id IS '最后消息ID (TSID bigint)';
COMMENT ON COLUMN public.conversation_pin.conversation_id IS '会话ID (TSID bigint，单聊为对方UID，群聊为群ID)';
COMMENT ON COLUMN public.conversation_delete.conversation_id IS '会话ID (TSID bigint，单聊为对方UID，群聊为群ID)';

COMMIT;
