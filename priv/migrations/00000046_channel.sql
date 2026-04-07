-- Table: public.channel

-- 频道表：存储频道基础信息
-- 类似于 Telegram Channel，单向关注型消息订阅机制

CREATE TABLE IF NOT EXISTS public.channel
(
    id BIGSERIAL PRIMARY KEY,
    name varchar(200) NOT NULL,
    description varchar(2000) NOT NULL DEFAULT '',
    avatar varchar(320) NOT NULL DEFAULT '',
    type smallint NOT NULL DEFAULT 0,  -- 0: 公开 1: 私有 2: 付费
    custom_id varchar(100) DEFAULT NULL,  -- 自定义ID，可空，唯一
    creator_uid bigint NOT NULL,
    subscriber_count bigint NOT NULL DEFAULT 0,  -- 订阅者数量
    is_verified boolean NOT NULL DEFAULT false,  -- 是否已认证
    tags varchar(500) NOT NULL DEFAULT '[]',  -- JSON 数组格式的标签
    status smallint NOT NULL DEFAULT 1,  -- -1: 删除 0: 禁用 1: 启用
    updated_at timestamptz DEFAULT CURRENT_TIMESTAMP NULL,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,

    CONSTRAINT uk_channel_custom_id UNIQUE (custom_id)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.channel OWNER to imboy_user;

COMMENT ON TABLE public.channel IS '频道表';

COMMENT ON COLUMN public.channel.id IS '主键 自增长ID';
COMMENT ON COLUMN public.channel.name IS '频道名称';
COMMENT ON COLUMN public.channel.description IS '频道描述/简介';
COMMENT ON COLUMN public.channel.avatar IS '频道头像';
COMMENT ON COLUMN public.channel.type IS '类型: 0 公开频道 1 私有频道 2 付费频道';
COMMENT ON COLUMN public.channel.custom_id IS '自定义ID，用于用户友好的频道标识';
COMMENT ON COLUMN public.channel.creator_uid IS '创建者用户ID';
COMMENT ON COLUMN public.channel.subscriber_count IS '订阅者数量';
COMMENT ON COLUMN public.channel.is_verified IS '是否已认证';
COMMENT ON COLUMN public.channel.tags IS '标签列表，JSON数组格式';
COMMENT ON COLUMN public.channel.status IS '状态: -1 删除 0 禁用 1 启用';
COMMENT ON COLUMN public.channel.updated_at IS '最后更新时间';
COMMENT ON COLUMN public.channel.created_at IS '创建时间';

-- 索引
CREATE INDEX IF NOT EXISTS i_channel_creator_uid ON public.channel(creator_uid);
CREATE INDEX IF NOT EXISTS i_channel_status_created ON public.channel(status, created_at DESC);
CREATE INDEX IF NOT EXISTS i_channel_subscriber_count ON public.channel(subscriber_count DESC);
CREATE INDEX IF NOT EXISTS i_channel_custom_id ON public.channel(custom_id) WHERE custom_id IS NOT NULL;

-- 频道订阅表：存储用户订阅关系
CREATE TABLE IF NOT EXISTS public.channel_subscription
(
    id BIGSERIAL PRIMARY KEY,
    channel_id bigint NOT NULL,
    user_id bigint NOT NULL,
    subscribed_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_read_at timestamptz DEFAULT NULL,
    last_message_id bigint DEFAULT 0,
    unread_count int NOT NULL DEFAULT 0,
    notifications_enabled boolean NOT NULL DEFAULT true,
    is_pinned boolean NOT NULL DEFAULT false,
    is_muted boolean NOT NULL DEFAULT false,
    status smallint NOT NULL DEFAULT 1,

    CONSTRAINT uk_channel_subscription UNIQUE (channel_id, user_id),
    CONSTRAINT fk_channel_subscription_channel FOREIGN KEY (channel_id)
        REFERENCES public.channel(id) ON DELETE CASCADE,
    CONSTRAINT fk_channel_subscription_user FOREIGN KEY (user_id)
        REFERENCES public.user(id) ON DELETE CASCADE
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.channel_subscription OWNER to imboy_user;

COMMENT ON TABLE public.channel_subscription IS '频道订阅关系表';

COMMENT ON COLUMN public.channel_subscription.channel_id IS '频道ID';
COMMENT ON COLUMN public.channel_subscription.user_id IS '用户ID';
COMMENT ON COLUMN public.channel_subscription.subscribed_at IS '订阅时间';
COMMENT ON COLUMN public.channel_subscription.last_read_at IS '最后阅读时间';
COMMENT ON COLUMN public.channel_subscription.last_message_id IS '最后已读消息ID';
COMMENT ON COLUMN public.channel_subscription.unread_count IS '未读消息数';
COMMENT ON COLUMN public.channel_subscription.notifications_enabled IS '是否开启通知';
COMMENT ON COLUMN public.channel_subscription.is_pinned IS '是否置顶';
COMMENT ON COLUMN public.channel_subscription.is_muted IS '是否免打扰';
COMMENT ON COLUMN public.channel_subscription.status IS '状态: 0 取消订阅 1 已订阅';

-- 索引
CREATE INDEX IF NOT EXISTS i_channel_subscription_user ON public.channel_subscription(user_id, status);
CREATE INDEX IF NOT EXISTS i_channel_subscription_channel ON public.channel_subscription(channel_id, status);

-- 频道消息表：存储频道发布的消息
CREATE TABLE IF NOT EXISTS public.channel_message
(
    id BIGSERIAL PRIMARY KEY,
    channel_id bigint NOT NULL,
    author_id bigint NOT NULL,  -- 发布者（管理员）ID
    author_name varchar(200) NOT NULL DEFAULT '',
    author_avatar varchar(320) NOT NULL DEFAULT '',
    content text NOT NULL,
    msg_type varchar(50) NOT NULL DEFAULT 'text',  -- text, image, video, audio, file, link, location
    payload text NOT NULL DEFAULT '{}',  -- JSON 格式的扩展数据
    is_pinned boolean NOT NULL DEFAULT false,  -- 是否置顶
    view_count bigint NOT NULL DEFAULT 0,  -- 阅读量
    reaction_summary text NOT NULL DEFAULT '{}',  -- JSON 格式的反应统计
    status smallint NOT NULL DEFAULT 1,
    updated_at timestamptz DEFAULT CURRENT_TIMESTAMP NULL,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,

    CONSTRAINT fk_channel_message_channel FOREIGN KEY (channel_id)
        REFERENCES public.channel(id) ON DELETE CASCADE
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.channel_message OWNER to imboy_user;

COMMENT ON TABLE public.channel_message IS '频道消息表';

COMMENT ON COLUMN public.channel_message.channel_id IS '频道ID';
COMMENT ON COLUMN public.channel_message.author_id IS '发布者用户ID';
COMMENT ON COLUMN public.channel_message.author_name IS '发布者昵称';
COMMENT ON COLUMN public.channel_message.author_avatar IS '发布者头像';
COMMENT ON COLUMN public.channel_message.content IS '消息内容';
COMMENT ON COLUMN public.channel_message.msg_type IS '消息类型';
COMMENT ON COLUMN public.channel_message.payload IS '扩展数据，JSON格式';
COMMENT ON COLUMN public.channel_message.is_pinned IS '是否置顶';
COMMENT ON COLUMN public.channel_message.view_count IS '阅读量';
COMMENT ON COLUMN public.channel_message.reaction_summary IS '反应统计，JSON格式';
COMMENT ON COLUMN public.channel_message.status IS '状态: -1 删除 1 正常';
COMMENT ON COLUMN public.channel_message.updated_at IS '更新时间';
COMMENT ON COLUMN public.channel_message.created_at IS '创建时间';

-- 索引
CREATE INDEX IF NOT EXISTS i_channel_message_channel_created ON public.channel_message(channel_id, created_at DESC);
CREATE INDEX IF NOT EXISTS i_channel_message_author ON public.channel_message(author_id);
CREATE INDEX IF NOT EXISTS i_channel_message_pinned ON public.channel_message(channel_id, is_pinned) WHERE is_pinned = true;

-- 频道管理员表：存储频道管理员信息
CREATE TABLE IF NOT EXISTS public.channel_admin
(
    id BIGSERIAL PRIMARY KEY,
    channel_id bigint NOT NULL,
    user_id bigint NOT NULL,
    role smallint NOT NULL DEFAULT 1,  -- 1: 编辑 2: 管理员 3: 创建者
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,

    CONSTRAINT uk_channel_admin UNIQUE (channel_id, user_id),
    CONSTRAINT fk_channel_admin_channel FOREIGN KEY (channel_id)
        REFERENCES public.channel(id) ON DELETE CASCADE,
    CONSTRAINT fk_channel_admin_user FOREIGN KEY (user_id)
        REFERENCES public.user(id) ON DELETE CASCADE
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.channel_admin OWNER to imboy_user;

COMMENT ON TABLE public.channel_admin IS '频道管理员表';

COMMENT ON COLUMN public.channel_admin.channel_id IS '频道ID';
COMMENT ON COLUMN public.channel_admin.user_id IS '用户ID';
COMMENT ON COLUMN public.channel_admin.role IS '角色: 1 编辑 2 管理员 3 创建者';
COMMENT ON COLUMN public.channel_admin.created_at IS '添加时间';

-- 索引
CREATE INDEX IF NOT EXISTS i_channel_admin_user ON public.channel_admin(user_id);

-- ============================================================================
-- 频道统计相关表 (Channel Statistics)
-- ============================================================================

-- 频道每日统计汇总表
-- 存储频道每天的综合统计数据，用于数据分析和报表展示
CREATE TABLE IF NOT EXISTS public.channel_stats_daily
(
    id BIGSERIAL PRIMARY KEY,
    channel_id bigint NOT NULL,
    stats_date date NOT NULL,  -- 统计日期

    -- 订阅统计
    new_subscribers int NOT NULL DEFAULT 0,  -- 新增订阅数
    unsubscribers int NOT NULL DEFAULT 0,    -- 取消订阅数
    net_subscribers int NOT NULL DEFAULT 0,  -- 净增订阅数

    -- 消息统计
    messages_count int NOT NULL DEFAULT 0,    -- 发布消息数
    total_views bigint NOT NULL DEFAULT 0,    -- 总阅读量
    total_reactions int NOT NULL DEFAULT 0,   -- 总反应数（点赞等）

    -- 活跃度统计
    active_viewers int NOT NULL DEFAULT 0,    -- 活跃阅读者数

    -- 元数据
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamptz DEFAULT CURRENT_TIMESTAMP NULL,

    CONSTRAINT uk_channel_stats_daily UNIQUE (channel_id, stats_date),
    CONSTRAINT fk_channel_stats_daily_channel FOREIGN KEY (channel_id)
        REFERENCES public.channel(id) ON DELETE CASCADE
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.channel_stats_daily OWNER to imboy_user;

COMMENT ON TABLE public.channel_stats_daily IS '频道每日统计汇总表';

COMMENT ON COLUMN public.channel_stats_daily.channel_id IS '频道ID';
COMMENT ON COLUMN public.channel_stats_daily.stats_date IS '统计日期';
COMMENT ON COLUMN public.channel_stats_daily.new_subscribers IS '当日新增订阅数';
COMMENT ON COLUMN public.channel_stats_daily.unsubscribers IS '当日取消订阅数';
COMMENT ON COLUMN public.channel_stats_daily.net_subscribers IS '当日净增订阅数';
COMMENT ON COLUMN public.channel_stats_daily.messages_count IS '当日发布消息数';
COMMENT ON COLUMN public.channel_stats_daily.total_views IS '当日总阅读量';
COMMENT ON COLUMN public.channel_stats_daily.total_reactions IS '当日总反应数';
COMMENT ON COLUMN public.channel_stats_daily.active_viewers IS '当日活跃阅读者数';

-- 索引
CREATE INDEX IF NOT EXISTS i_channel_stats_daily_channel ON public.channel_stats_daily(channel_id, stats_date DESC);
CREATE INDEX IF NOT EXISTS i_channel_stats_daily_date ON public.channel_stats_daily(stats_date DESC);

-- 消息阅读记录表
-- 记录用户对频道消息的阅读行为，用于精确统计阅读量和用户行为分析
CREATE TABLE IF NOT EXISTS public.channel_message_view
(
    id BIGSERIAL PRIMARY KEY,
    message_id bigint NOT NULL,
    channel_id bigint NOT NULL,
    user_id bigint NOT NULL,
    viewed_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,

    CONSTRAINT uk_channel_message_view UNIQUE (message_id, user_id),
    CONSTRAINT fk_channel_message_view_message FOREIGN KEY (message_id)
        REFERENCES public.channel_message(id) ON DELETE CASCADE,
    CONSTRAINT fk_channel_message_view_channel FOREIGN KEY (channel_id)
        REFERENCES public.channel(id) ON DELETE CASCADE,
    CONSTRAINT fk_channel_message_view_user FOREIGN KEY (user_id)
        REFERENCES public.user(id) ON DELETE CASCADE
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.channel_message_view OWNER to imboy_user;

COMMENT ON TABLE public.channel_message_view IS '频道消息阅读记录表';

COMMENT ON COLUMN public.channel_message_view.message_id IS '消息ID';
COMMENT ON COLUMN public.channel_message_view.channel_id IS '频道ID';
COMMENT ON COLUMN public.channel_message_view.user_id IS '阅读者用户ID';
COMMENT ON COLUMN public.channel_message_view.viewed_at IS '阅读时间';

-- 索引
CREATE INDEX IF NOT EXISTS i_channel_message_view_message ON public.channel_message_view(message_id);
CREATE INDEX IF NOT EXISTS i_channel_message_view_channel_user ON public.channel_message_view(channel_id, user_id);
CREATE INDEX IF NOT EXISTS i_channel_message_view_viewed_at ON public.channel_message_view(viewed_at DESC);

-- 消息反应表（点赞、收藏等）
-- 存储用户对频道消息的反应（emoji 表情、点赞、收藏等）
CREATE TABLE IF NOT EXISTS public.channel_reaction
(
    id BIGSERIAL PRIMARY KEY,
    message_id bigint NOT NULL,
    channel_id bigint NOT NULL,
    user_id bigint NOT NULL,
    reaction_type varchar(50) NOT NULL,  -- like, heart, fire, thumbs_up, bookmark 等
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,

    CONSTRAINT uk_channel_reaction UNIQUE (message_id, user_id, reaction_type),
    CONSTRAINT fk_channel_reaction_message FOREIGN KEY (message_id)
        REFERENCES public.channel_message(id) ON DELETE CASCADE,
    CONSTRAINT fk_channel_reaction_channel FOREIGN KEY (channel_id)
        REFERENCES public.channel(id) ON DELETE CASCADE,
    CONSTRAINT fk_channel_reaction_user FOREIGN KEY (user_id)
        REFERENCES public.user(id) ON DELETE CASCADE
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.channel_reaction OWNER to imboy_user;

COMMENT ON TABLE public.channel_reaction IS '频道消息反应表';

COMMENT ON COLUMN public.channel_reaction.message_id IS '消息ID';
COMMENT ON COLUMN public.channel_reaction.channel_id IS '频道ID';
COMMENT ON COLUMN public.channel_reaction.user_id IS '反应者用户ID';
COMMENT ON COLUMN public.channel_reaction.reaction_type IS '反应类型: like, heart, fire, thumbs_up, bookmark 等';
COMMENT ON COLUMN public.channel_reaction.created_at IS '反应时间';

-- 索引
CREATE INDEX IF NOT EXISTS i_channel_reaction_message ON public.channel_reaction(message_id);
CREATE INDEX IF NOT EXISTS i_channel_reaction_channel ON public.channel_reaction(channel_id);
CREATE INDEX IF NOT EXISTS i_channel_reaction_user ON public.channel_reaction(user_id);
CREATE INDEX IF NOT EXISTS i_channel_reaction_type ON public.channel_reaction(reaction_type);

-- ============================================================================
-- 触发器：自动更新消息阅读量
-- ============================================================================
-- 当有新的阅读记录时，自动更新 channel_message.view_count

CREATE OR REPLACE FUNCTION fn_update_channel_message_view_count()
RETURNS TRIGGER AS $$
BEGIN
    UPDATE public.channel_message
    SET view_count = view_count + 1
    WHERE id = NEW.message_id;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS tr_update_channel_message_view_count ON public.channel_message_view;

CREATE TRIGGER tr_update_channel_message_view_count
    AFTER INSERT ON public.channel_message_view
    FOR EACH ROW
    EXECUTE FUNCTION fn_update_channel_message_view_count();

-- ============================================================================
-- 触发器：自动更新消息反应统计
-- ============================================================================
-- 当有新的反应时，自动更新 channel_message.reaction_summary

CREATE OR REPLACE FUNCTION fn_update_channel_message_reaction_summary()
RETURNS TRIGGER AS $$
BEGIN
    UPDATE public.channel_message cm
    SET reaction_summary = (
        SELECT json_object_agg(reaction_type, cnt)
        FROM (
            SELECT reaction_type, COUNT(*) as cnt
            FROM public.channel_reaction
            WHERE message_id = COALESCE(NEW.message_id, OLD.message_id)
            GROUP BY reaction_type
        ) sub
    )
    WHERE id = COALESCE(NEW.message_id, OLD.message_id);
    RETURN COALESCE(NEW, OLD);
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS tr_update_channel_message_reaction_summary ON public.channel_reaction;

CREATE TRIGGER tr_update_channel_message_reaction_summary
    AFTER INSERT OR DELETE ON public.channel_reaction
    FOR EACH ROW
    EXECUTE FUNCTION fn_update_channel_message_reaction_summary();

-- ============================================================================
-- 聚合统计视图：频道实时统计
-- ============================================================================

CREATE OR REPLACE VIEW v_channel_realtime_stats AS
SELECT
    c.id as channel_id,
    c.name as channel_name,
    c.subscriber_count,
    COUNT(DISTINCT cm.id) as total_messages,
    COALESCE(SUM(cm.view_count), 0) as total_views,
    COALESCE(
        (SELECT COUNT(*) FROM public.channel_reaction cr WHERE cr.channel_id = c.id),
        0
    ) as total_reactions,
    MAX(cm.created_at) as last_message_at
FROM public.channel c
LEFT JOIN public.channel_message cm ON c.id = cm.channel_id AND cm.status = 1
WHERE c.status = 1
GROUP BY c.id, c.name, c.subscriber_count;

COMMENT ON VIEW v_channel_realtime_stats IS '频道实时统计视图';

-- ============================================================================
-- 聚合统计视图：用户频道阅读统计
-- ============================================================================

CREATE OR REPLACE VIEW v_user_channel_reading_stats AS
SELECT
    cs.user_id,
    cs.channel_id,
    c.name as channel_name,
    cs.unread_count,
    cs.last_read_at,
    COUNT(DISTINCT cmv.message_id) as viewed_messages,
    MAX(cmv.viewed_at) as last_view_at
FROM public.channel_subscription cs
JOIN public.channel c ON cs.channel_id = c.id
LEFT JOIN public.channel_message_view cmv ON cs.channel_id = cmv.channel_id AND cs.user_id = cmv.user_id
WHERE cs.status = 1
GROUP BY cs.user_id, cs.channel_id, c.name, cs.unread_count, cs.last_read_at;

COMMENT ON VIEW v_user_channel_reading_stats IS '用户频道阅读统计视图';
