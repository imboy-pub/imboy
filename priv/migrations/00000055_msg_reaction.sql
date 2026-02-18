-- 消息表情回应表
CREATE TABLE msg_reaction (
    id BIGSERIAL PRIMARY KEY,
    msg_id varchar(40) NOT NULL,
    msg_type varchar(10) NOT NULL,      -- c2c/c2g
    user_id bigint NOT NULL,
    emoji varchar(100) NOT NULL,        -- emoji字符或emoji代码
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    UNIQUE(msg_id, msg_type, user_id, emoji)
);

CREATE INDEX idx_msg_reaction_msg ON msg_reaction(msg_id, msg_type);
CREATE INDEX idx_msg_reaction_user ON msg_reaction(user_id);
CREATE INDEX idx_msg_reaction_emoji ON msg_reaction(msg_id, msg_type, emoji);

COMMENT ON TABLE msg_reaction IS '消息表情回应表';
COMMENT ON COLUMN msg_reaction.msg_type IS '消息类型: c2c-单聊, c2g-群聊';
COMMENT ON COLUMN msg_reaction.emoji IS 'emoji字符，如👍❤️😄等';
