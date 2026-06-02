-- 回滚 000069：删除延迟补丁新增的列/索引/约束（幂等 DROP IF EXISTS）
-- 注意：varchar→bigint 的类型改造不做逆向回退（数据语义不可逆），
-- 仅清理约束与索引；如需完整回退请从备份恢复。

-- ── 段2 回滚：conversation 系列约束/索引 ──
ALTER TABLE IF EXISTS public.conversation_delete
    DROP CONSTRAINT IF EXISTS conversation_delete_user_conversation_uk;
DROP INDEX IF EXISTS idx_conversation_delete_conversation;

ALTER TABLE IF EXISTS public.conversation_pin
    DROP CONSTRAINT IF EXISTS conversation_pin_user_conversation_uk;
DROP INDEX IF EXISTS idx_conversation_pin_conversation;

DROP INDEX IF EXISTS uk_cvt_UserId_Type_PeerId;

-- ── 段1 回滚：msg 跨表补丁 ──
-- burn after
DROP INDEX IF EXISTS i_c2c_expire_at;
DROP INDEX IF EXISTS i_c2g_expire_at;
ALTER TABLE IF EXISTS public.msg_c2c DROP COLUMN IF EXISTS expire_at;
ALTER TABLE IF EXISTS public.msg_c2g DROP COLUMN IF EXISTS expire_at;
ALTER TABLE IF EXISTS public.msg_store DROP COLUMN IF EXISTS expire_at;

-- reply
DROP INDEX IF EXISTS idx_msg_c2c_reply;
DROP INDEX IF EXISTS idx_msg_c2g_reply;
ALTER TABLE IF EXISTS public.msg_c2c DROP COLUMN IF EXISTS reply_to_msg_id;
ALTER TABLE IF EXISTS public.msg_c2c DROP COLUMN IF EXISTS reply_to_from_id;
ALTER TABLE IF EXISTS public.msg_c2c DROP COLUMN IF EXISTS reply_snippet;
ALTER TABLE IF EXISTS public.msg_c2g DROP COLUMN IF EXISTS reply_to_msg_id;
ALTER TABLE IF EXISTS public.msg_c2g DROP COLUMN IF EXISTS reply_to_from_id;
ALTER TABLE IF EXISTS public.msg_c2g DROP COLUMN IF EXISTS reply_snippet;

-- pinned
DROP INDEX IF EXISTS idx_msg_c2c_pinned;
DROP INDEX IF EXISTS idx_msg_c2g_pinned;
ALTER TABLE IF EXISTS public.msg_c2c DROP COLUMN IF EXISTS pinned;
ALTER TABLE IF EXISTS public.msg_c2g DROP COLUMN IF EXISTS pinned;

-- fts
DROP INDEX IF EXISTS idx_msg_c2c_payload_fts;
DROP INDEX IF EXISTS idx_msg_c2g_payload_fts;
