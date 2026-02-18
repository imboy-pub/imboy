-- 消息转发记录表
-- 用于记录消息转发的溯源关系
CREATE TABLE IF NOT EXISTS msg_forward (
    id BIGSERIAL PRIMARY KEY,
    original_msg_id varchar(40) NOT NULL,
    original_from_id bigint NOT NULL,
    original_to_id bigint NOT NULL,
    original_type varchar(10) NOT NULL,  -- c2c/c2g
    forward_msg_id varchar(40) NOT NULL,
    forward_from_id bigint NOT NULL,
    forward_to_id bigint NOT NULL,
    forward_type varchar(10) NOT NULL,  -- c2c/c2g
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- 创建索引以提高查询性能
CREATE INDEX IF NOT EXISTS idx_msg_forward_original ON msg_forward(original_msg_id);
CREATE INDEX IF NOT EXISTS idx_msg_forward_from ON msg_forward(forward_from_id);
CREATE INDEX IF NOT EXISTS idx_msg_forward_to ON msg_forward(forward_to_id);

-- 添加注释
COMMENT ON TABLE msg_forward IS '消息转发记录表，记录消息转发的溯源关系';
COMMENT ON COLUMN msg_forward.original_msg_id IS '原始消息ID';
COMMENT ON COLUMN msg_forward.original_from_id IS '原始消息发送者ID';
COMMENT ON COLUMN msg_forward.original_to_id IS '原始消息接收者ID（单聊为用户ID，群聊为群组ID）';
COMMENT ON COLUMN msg_forward.original_type IS '原始消息类型：c2c（单聊）或c2g（群聊）';
COMMENT ON COLUMN msg_forward.forward_msg_id IS '转发后的消息ID';
COMMENT ON COLUMN msg_forward.forward_from_id IS '转发操作者ID';
COMMENT ON COLUMN msg_forward.forward_to_id IS '转发目标ID（单聊为用户ID，群聊为群组ID）';
COMMENT ON COLUMN msg_forward.forward_type IS '转发目标类型：c2c（单聊）或c2g（群聊）';
