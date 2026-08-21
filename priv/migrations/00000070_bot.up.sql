-- P2: Bot 基表 + Bot OAuth 授权表（开发者服务，account_type=3）
-- Bot base table + OAuth grant table (developer service, account_type=3)

BEGIN;

-- 1. Bot 表（开发者服务）
CREATE TABLE IF NOT EXISTS public.bot (
    user_id        BIGINT PRIMARY KEY,              -- 关联 user.id
    name           VARCHAR(80) NOT NULL,             -- Bot 名称
    username       VARCHAR(80) UNIQUE,               -- @调用名（唯一）
    description    VARCHAR(500) DEFAULT '',          -- 简介
    avatar         VARCHAR(320) DEFAULT '',          -- 头像
    owner_uid      BIGINT NOT NULL,                  -- 开发者/所有者
    webhook_url    TEXT DEFAULT '',                  -- 消息推送地址
    api_token      VARCHAR(128) UNIQUE,              -- API 认证 token
    verify_token   VARCHAR(128) DEFAULT '',          -- webhook 验签 token
    commands       JSONB NOT NULL DEFAULT '[]',       -- 注册的命令
    permissions    JSONB NOT NULL DEFAULT '[]',       -- 权限列表
    events         JSONB NOT NULL DEFAULT '[]',       -- 订阅的事件类型
    is_public      BOOLEAN DEFAULT false,             -- 是否公开（本实例注册表检索）
    status         SMALLINT DEFAULT 1,               -- -1=deleted, 0=disabled, 1=active
    created_at     TIMESTAMPTZ DEFAULT now(),
    updated_at     TIMESTAMPTZ DEFAULT now(),
    CONSTRAINT fk_bot_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_bot_owner ON public.bot(owner_uid);
CREATE INDEX IF NOT EXISTS idx_bot_username ON public.bot(username);
CREATE INDEX IF NOT EXISTS idx_bot_is_public ON public.bot(is_public) WHERE is_public = true;
CREATE INDEX IF NOT EXISTS idx_bot_status ON public.bot(status);

COMMENT ON TABLE public.bot IS '开发者 Bot（Webhook 驱动的第三方服务，account_type=3）';
COMMENT ON COLUMN public.bot.user_id IS '关联 user.id，Bot 作为一等 user 账号存在（account_type=3）';
COMMENT ON COLUMN public.bot.username IS 'Bot 唯一调用名，用于 @botname 提及';
COMMENT ON COLUMN public.bot.api_token IS 'Bot 调用 IMBoy API 的凭证';
COMMENT ON COLUMN public.bot.verify_token IS 'Webhook 推送时的验签 token（与 api_token 职责分离）';

-- 2. Bot OAuth 授权表
-- 注意 UNIQUE(bot_id, user_id)：重新授权必须复用同一行（UPDATE access_token/scopes/
-- expires_at 并清空 revoked_at），不得 INSERT 新行，否则违反唯一约束。
CREATE TABLE IF NOT EXISTS public.bot_oauth_grant (
    id           BIGINT PRIMARY KEY,
    bot_id       BIGINT NOT NULL,                    -- Bot 的 user_id
    user_id      BIGINT NOT NULL,                    -- 授权用户
    scopes       JSONB NOT NULL DEFAULT '[]',         -- 授权范围
    access_token VARCHAR(128) UNIQUE,                 -- 访问令牌
    expires_at   TIMESTAMPTZ,                         -- 过期时间
    revoked_at   TIMESTAMPTZ,                         -- 撤销时间
    status       SMALLINT DEFAULT 1,                 -- 0=revoked, 1=active
    created_at   TIMESTAMPTZ DEFAULT now(),
    updated_at   TIMESTAMPTZ DEFAULT now(),
    UNIQUE(bot_id, user_id),
    CONSTRAINT fk_bot_grant_bot FOREIGN KEY (bot_id) REFERENCES public.bot(user_id) ON DELETE CASCADE,
    CONSTRAINT fk_bot_grant_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_bot_oauth_grant_bot ON public.bot_oauth_grant(bot_id);
CREATE INDEX IF NOT EXISTS idx_bot_oauth_grant_user ON public.bot_oauth_grant(user_id);
CREATE INDEX IF NOT EXISTS idx_bot_oauth_grant_token ON public.bot_oauth_grant(access_token);

COMMENT ON TABLE public.bot_oauth_grant IS 'Bot OAuth 授权：用户授权 Bot 代表自己操作';

-- 3. account_type 注释更新（扩展枚举，2 的既有语义不变）
COMMENT ON COLUMN public."user".account_type IS '账号类型 0=human 1=agent(平台AI) 2=system_bot(频道webhook bot) 3=bot(开发者服务)';

COMMIT;