-- 群投票功能表
-- 创建时间: 2026-02-17
-- 说明: 支持群内单选/多选投票，可设置匿名和截止时间

-- 群投票主表
CREATE TABLE IF NOT EXISTS group_vote (
    id BIGSERIAL PRIMARY KEY,
    group_id BIGINT NOT NULL,
    vote_id VARCHAR(40) NOT NULL UNIQUE,
    title VARCHAR(200) NOT NULL,
    description TEXT,
    creator_id BIGINT NOT NULL,
    vote_type SMALLINT DEFAULT 1,  -- 1单选 2多选
    is_anonymous BOOLEAN DEFAULT FALSE,
    status SMALLINT DEFAULT 1,  -- 1进行中 2已结束 3已取消
    end_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- 群投票选项表
CREATE TABLE IF NOT EXISTS group_vote_option (
    id BIGSERIAL PRIMARY KEY,
    vote_id VARCHAR(40) NOT NULL,
    option_id VARCHAR(40) NOT NULL,
    option_text VARCHAR(200) NOT NULL,
    sort_order INTEGER DEFAULT 0,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
    UNIQUE(vote_id, option_id)
);

-- 群投票记录表
CREATE TABLE IF NOT EXISTS group_vote_record (
    id BIGSERIAL PRIMARY KEY,
    vote_id VARCHAR(40) NOT NULL,
    user_id BIGINT NOT NULL,
    option_ids TEXT NOT NULL,  -- JSON数组格式存储选项ID列表
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
    UNIQUE(vote_id, user_id)
);

-- 创建索引以提高查询性能
CREATE INDEX IF NOT EXISTS idx_group_vote_group_id ON group_vote(group_id);
CREATE INDEX IF NOT EXISTS idx_group_vote_creator_id ON group_vote(creator_id);
CREATE INDEX IF NOT EXISTS idx_group_vote_status ON group_vote(status);
CREATE INDEX IF NOT EXISTS idx_group_vote_vote_id ON group_vote(vote_id);

CREATE INDEX IF NOT EXISTS idx_group_vote_option_vote_id ON group_vote_option(vote_id);
CREATE INDEX IF NOT EXISTS idx_group_vote_option_option_id ON group_vote_option(option_id);

CREATE INDEX IF NOT EXISTS idx_group_vote_record_vote_id ON group_vote_record(vote_id);
CREATE INDEX IF NOT EXISTS idx_group_vote_record_user_id ON group_vote_record(user_id);

-- 添加注释
COMMENT ON TABLE group_vote IS '群投票主表';
COMMENT ON TABLE group_vote_option IS '群投票选项表';
COMMENT ON TABLE group_vote_record IS '群投票记录表';

COMMENT ON COLUMN group_vote.vote_id IS '投票唯一标识 (UUID)';
COMMENT ON COLUMN group_vote.vote_type IS '投票类型: 1=单选, 2=多选';
COMMENT ON COLUMN group_vote.is_anonymous IS '是否匿名投票';
COMMENT ON COLUMN group_vote.status IS '投票状态: 1=进行中, 2=已结束, 3=已取消';
COMMENT ON COLUMN group_vote.end_at IS '投票截止时间';

COMMENT ON COLUMN group_vote_option.option_id IS '选项唯一标识';
COMMENT ON COLUMN group_vote_option.sort_order IS '选项排序顺序';

COMMENT ON COLUMN group_vote_record.option_ids IS '用户选择的选项ID列表(JSON数组格式)';
