-- ============================================================
-- Migration: 00000076_push_token
-- Description: 推送 Token 管理表
--
-- 设计说明：
--   每个用户设备绑定一个推送 token (FCM / APNs)
--   登录时上报 token，登出时清除
--   发送推送时查询离线用户的 token
-- ============================================================

CREATE TABLE IF NOT EXISTS public.push_token (
    id          BIGSERIAL    PRIMARY KEY,
    user_id     BIGINT       NOT NULL,
    device_id   VARCHAR(128) NOT NULL,
    device_type VARCHAR(16)  NOT NULL,   -- 'android' | 'ios' | 'web'
    platform    VARCHAR(16)  NOT NULL,   -- 'fcm' | 'apns' | 'web_push'
    token       TEXT         NOT NULL,
    status      SMALLINT     NOT NULL DEFAULT 1,  -- 1=active, 0=inactive
    created_at  TIMESTAMPTZ  NOT NULL DEFAULT NOW(),
    updated_at  TIMESTAMPTZ  NOT NULL DEFAULT NOW()
);

ALTER TABLE IF EXISTS public.push_token OWNER TO imboy_user;

-- 每设备只保留一个活跃 token
CREATE UNIQUE INDEX IF NOT EXISTS uk_push_token_user_device
    ON public.push_token (user_id, device_id) WHERE status = 1;

-- 按用户查询活跃 token
CREATE INDEX IF NOT EXISTS i_push_token_user_id
    ON public.push_token (user_id) WHERE status = 1;

COMMENT ON TABLE  public.push_token IS '推送 Token 管理表';
COMMENT ON COLUMN public.push_token.user_id     IS '用户 ID';
COMMENT ON COLUMN public.push_token.device_id   IS '设备唯一标识';
COMMENT ON COLUMN public.push_token.device_type  IS '设备类型: android | ios | web';
COMMENT ON COLUMN public.push_token.platform     IS '推送平台: fcm | apns | web_push';
COMMENT ON COLUMN public.push_token.token        IS '推送 token';
COMMENT ON COLUMN public.push_token.status       IS '状态: 1=活跃 0=无效';
