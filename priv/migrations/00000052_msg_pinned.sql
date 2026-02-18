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