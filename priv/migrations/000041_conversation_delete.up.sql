-- 会话删除记录表（软删除）
CREATE TABLE IF NOT EXISTS conversation_delete (
    id BIGSERIAL PRIMARY KEY,
    user_id bigint NOT NULL,
    conversation_id varchar(40) NOT NULL,
    conversation_type varchar(10) NOT NULL,  -- c2c/c2g
    deleted_at timestamptz DEFAULT CURRENT_TIMESTAMP,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    UNIQUE(user_id, conversation_id, conversation_type)
);

-- 创建索引：按用户ID和删除时间倒序查询
CREATE INDEX IF NOT EXISTS idx_conversation_delete_user ON conversation_delete(user_id, deleted_at DESC);

-- 创建索引：按会话ID查询
CREATE INDEX IF NOT EXISTS idx_conversation_delete_conversation ON conversation_delete(conversation_id, conversation_type);

-- 注释
COMMENT ON TABLE conversation_delete IS '会话软删除记录表';
COMMENT ON COLUMN conversation_delete.user_id IS '用户ID';
COMMENT ON COLUMN conversation_delete.conversation_id IS '会话ID（单聊为对方UID，群聊为群ID）';
COMMENT ON COLUMN conversation_delete.conversation_type IS '会话类型：c2c-单聊，c2g-群聊';
COMMENT ON COLUMN conversation_delete.deleted_at IS '删除时间，可用于恢复功能';
COMMENT ON COLUMN conversation_delete.created_at IS '创建时间';
