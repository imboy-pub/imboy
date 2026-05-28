-- 直播间表
CREATE TABLE IF NOT EXISTS public.live_room (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT NOT NULL,
    title varchar(100) NOT NULL DEFAULT '',
    cover varchar(255) DEFAULT '',
    stream_key varchar(64) NOT NULL UNIQUE,
    status SMALLINT NOT NULL DEFAULT 0, -- 0=准备中 1=直播中 2=已结束
    viewer_count INT DEFAULT 0,
    tag_id INT DEFAULT 0,
    scene SMALLINT DEFAULT 1,           -- 1=普通直播
    updated_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS i_live_room_UserId ON live_room(user_id);
CREATE INDEX IF NOT EXISTS i_live_room_Status ON live_room(status);
