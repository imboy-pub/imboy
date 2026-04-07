-- 群组日程功能
-- 创建群组日程表
CREATE TABLE IF NOT EXISTS group_schedule (
    id BIGSERIAL PRIMARY KEY,
    group_id BIGINT NOT NULL,
    schedule_id VARCHAR(40) NOT NULL UNIQUE,
    title VARCHAR(200) NOT NULL,
    description TEXT,
    location VARCHAR(200),
    creator_id BIGINT NOT NULL,
    start_at TIMESTAMPTZ NOT NULL,
    end_at TIMESTAMPTZ NOT NULL,
    remind_before INTEGER,  -- 提前多少分钟提醒
    status SMALLINT DEFAULT 1,  -- 1待开始 2进行中 3已结束 4已取消
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- 创建群组日程参与人表
CREATE TABLE IF NOT EXISTS group_schedule_participant (
    id BIGSERIAL PRIMARY KEY,
    schedule_id VARCHAR(40) NOT NULL,
    user_id BIGINT NOT NULL,
    status SMALLINT DEFAULT 0,  -- 0待确认 1参加 2不参加
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
    UNIQUE(schedule_id, user_id)
);

-- 创建群组日程提醒表
CREATE TABLE IF NOT EXISTS group_schedule_remind (
    id BIGSERIAL PRIMARY KEY,
    schedule_id VARCHAR(40) NOT NULL,
    user_id BIGINT NOT NULL,
    remind_at TIMESTAMPTZ NOT NULL,
    is_sent BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- 创建索引
CREATE INDEX IF NOT EXISTS idx_group_schedule_group_id ON group_schedule(group_id);
CREATE INDEX IF NOT EXISTS idx_group_schedule_creator_id ON group_schedule(creator_id);
CREATE INDEX IF NOT EXISTS idx_group_schedule_start_at ON group_schedule(start_at);
CREATE INDEX IF NOT EXISTS idx_group_schedule_status ON group_schedule(status);

CREATE INDEX IF NOT EXISTS idx_group_schedule_participant_schedule_id ON group_schedule_participant(schedule_id);
CREATE INDEX IF NOT EXISTS idx_group_schedule_participant_user_id ON group_schedule_participant(user_id);
CREATE INDEX IF NOT EXISTS idx_group_schedule_participant_status ON group_schedule_participant(status);

CREATE INDEX IF NOT EXISTS idx_group_schedule_remind_schedule_id ON group_schedule_remind(schedule_id);
CREATE INDEX IF NOT EXISTS idx_group_schedule_remind_user_id ON group_schedule_remind(user_id);
CREATE INDEX IF NOT EXISTS idx_group_schedule_remind_remind_at ON group_schedule_remind(remind_at);
CREATE INDEX IF NOT EXISTS idx_group_schedule_remind_is_sent ON group_schedule_remind(is_sent);

-- 添加表注释
COMMENT ON TABLE group_schedule IS '群组日程表';
COMMENT ON TABLE group_schedule_participant IS '群组日程参与人表';
COMMENT ON TABLE group_schedule_remind IS '群组日程提醒表';

-- 添加列注释
COMMENT ON COLUMN group_schedule.remind_before IS '提前多少分钟提醒';
COMMENT ON COLUMN group_schedule.status IS '1待开始 2进行中 3已结束 4已取消';
COMMENT ON COLUMN group_schedule_participant.status IS '0待确认 1参加 2不参加';
