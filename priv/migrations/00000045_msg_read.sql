-- 消息已读回执表
-- 用于跟踪单聊消息的已读状态
CREATE TABLE IF NOT EXISTS msg_read (
    id BIGSERIAL PRIMARY KEY,
    msg_id VARCHAR(64) NOT NULL,           -- 消息ID
    from_uid BIGINT NOT NULL,              -- 发送者ID (整数)
    to_uid BIGINT NOT NULL,                -- 接收者ID (整数)
    to_did VARCHAR(64),                    -- 接收者设备ID
    read_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),  -- 已读时间
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- 创建唯一约束：同一消息、同一接收者、同一设备只能有一条已读记录
-- 这确保了幂等性：重复的已读回执不会产生重复记录
CREATE UNIQUE INDEX IF NOT EXISTS uk_msg_read_msg_to_did ON msg_read(msg_id, to_uid, to_did);

-- 创建索引：按消息ID查询已读状态
CREATE INDEX IF NOT EXISTS idx_msg_read_msg_id ON msg_read(msg_id);

-- 创建索引：按接收者查询已读消息
CREATE INDEX IF NOT EXISTS idx_msg_read_to_uid ON msg_read(to_uid);

-- 创建索引：按发送者查询已读状态（用于通知发送者）
CREATE INDEX IF NOT EXISTS idx_msg_read_from_uid ON msg_read(from_uid);

-- 创建索引：按已读时间查询（用于清理过期数据）
CREATE INDEX IF NOT EXISTS idx_msg_read_read_at ON msg_read(read_at);

-- 添加注释
COMMENT ON TABLE msg_read IS '消息已读回执表';
COMMENT ON COLUMN msg_read.msg_id IS '消息唯一ID';
COMMENT ON COLUMN msg_read.from_uid IS '发送者用户ID (整数)';
COMMENT ON COLUMN msg_read.to_uid IS '接收者用户ID (整数)';
COMMENT ON COLUMN msg_read.to_did IS '接收者设备ID';
COMMENT ON COLUMN msg_read.read_at IS '消息已读时间（服务器时间）';
COMMENT ON COLUMN msg_read.created_at IS '记录创建时间';
