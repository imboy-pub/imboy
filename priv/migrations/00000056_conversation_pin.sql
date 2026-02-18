-- 会话置顶表
CREATE TABLE IF NOT EXISTS conversation_pin (
    id BIGSERIAL PRIMARY KEY,
    user_id bigint NOT NULL,
    conversation_id varchar(40) NOT NULL,
    conversation_type varchar(10) NOT NULL,  -- c2c/c2g
    pinned_at timestamptz DEFAULT CURRENT_TIMESTAMP,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    UNIQUE(user_id, conversation_id, conversation_type)
);

-- 创建索引：按用户ID和置顶时间倒序查询
CREATE INDEX IF NOT EXISTS idx_conversation_pin_user ON conversation_pin(user_id, pinned_at DESC);

-- 创建索引：按会话ID查询
CREATE INDEX IF NOT EXISTS idx_conversation_pin_conversation ON conversation_pin(conversation_id, conversation_type);

-- 注释
COMMENT ON TABLE conversation_pin IS '会话置顶表';
COMMENT ON COLUMN conversation_pin.user_id IS '用户ID';
COMMENT ON COLUMN conversation_pin.conversation_id IS '会话ID（单聊为对方UID，群聊为群ID）';
COMMENT ON COLUMN conversation_pin.conversation_type IS '会话类型：c2c-单聊，c2g-群聊';
COMMENT ON COLUMN conversation_pin.pinned_at IS '置顶时间';
COMMENT ON COLUMN conversation_pin.created_at IS '创建时间';
