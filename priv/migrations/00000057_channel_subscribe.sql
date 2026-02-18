-- ================================================================
-- 频道订阅增强功能 - 数据库迁移
-- 版本: 00000057
-- 日期: 2026-02-16
-- 说明: 私有频道邀请和付费频道购买功能
-- ================================================================
-- 功能模块:
--   1. 私有频道邀请机制
--   2. 付费频道订单管理
--   3. 频道价格配置
-- ================================================================

-- ================================================================
-- 1. 频道价格配置表
-- 用途: 配置付费频道的订阅价格
-- ================================================================

CREATE TABLE IF NOT EXISTS public.channel_price (
    id BIGSERIAL PRIMARY KEY,
    channel_id BIGINT NOT NULL UNIQUE,
    price DECIMAL(10,2) NOT NULL DEFAULT 0,
    currency VARCHAR(10) NOT NULL DEFAULT 'CNY',
    subscription_type SMALLINT NOT NULL DEFAULT 1,  -- 1: 一次性 2: 月订阅 3: 年订阅
    original_price DECIMAL(10,2),                   -- 原价（用于显示折扣）
    description VARCHAR(500),                        -- 价格说明
    status SMALLINT NOT NULL DEFAULT 1,             -- 0: 禁用 1: 启用
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW(),

    CONSTRAINT fk_channel_price_channel FOREIGN KEY (channel_id)
        REFERENCES public.channel(id) ON DELETE CASCADE,
    CONSTRAINT chk_channel_price CHECK (price >= 0)
);

-- 索引
CREATE INDEX IF NOT EXISTS idx_channel_price_channel_id
    ON public.channel_price(channel_id);

CREATE INDEX IF NOT EXISTS idx_channel_price_status
    ON public.channel_price(status);

-- 注释
COMMENT ON TABLE public.channel_price IS '频道价格配置表 - 付费频道的订阅价格';
COMMENT ON COLUMN public.channel_price.id IS '主键';
COMMENT ON COLUMN public.channel_price.channel_id IS '频道ID';
COMMENT ON COLUMN public.channel_price.price IS '当前价格';
COMMENT ON COLUMN public.channel_price.currency IS '货币类型（CNY/USD等）';
COMMENT ON COLUMN public.channel_price.subscription_type IS '订阅类型: 1一次性 2月订阅 3年订阅';
COMMENT ON COLUMN public.channel_price.original_price IS '原价（用于显示折扣）';
COMMENT ON COLUMN public.channel_price.description IS '价格说明';
COMMENT ON COLUMN public.channel_price.status IS '状态: 0禁用 1启用';
COMMENT ON COLUMN public.channel_price.created_at IS '创建时间';
COMMENT ON COLUMN public.channel_price.updated_at IS '更新时间';

-- ================================================================
-- 2. 频道邀请表（私有频道）
-- 用途: 管理私有频道的邀请机制
-- ================================================================

CREATE TABLE IF NOT EXISTS public.channel_invitation (
    id BIGSERIAL PRIMARY KEY,
    channel_id BIGINT NOT NULL,
    inviter_uid BIGINT NOT NULL,        -- 邀请人
    invitee_uid BIGINT NOT NULL,        -- 被邀请人
    invitation_code VARCHAR(64),        -- 邀请码（可选，用于分享链接）
    status SMALLINT NOT NULL DEFAULT 0, -- 0: 待处理 1: 已接受 2: 已拒绝 3: 已过期 4: 已取消
    message VARCHAR(500),               -- 邀请消息
    expires_at TIMESTAMPTZ NOT NULL,    -- 邀请过期时间（默认7天）
    accepted_at TIMESTAMPTZ,            -- 接受时间
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW(),

    -- 每个用户对每个频道只能有一个待处理的邀请
    CONSTRAINT uk_channel_invitation_active UNIQUE (channel_id, invitee_uid),
    CONSTRAINT fk_channel_invitation_channel FOREIGN KEY (channel_id)
        REFERENCES public.channel(id) ON DELETE CASCADE,
    CONSTRAINT chk_channel_invitation_status CHECK (status IN (0, 1, 2, 3, 4))
);

-- 索引
CREATE INDEX IF NOT EXISTS idx_channel_invitation_channel_id
    ON public.channel_invitation(channel_id);

CREATE INDEX IF NOT EXISTS idx_channel_invitation_inviter_uid
    ON public.channel_invitation(inviter_uid);

CREATE INDEX IF NOT EXISTS idx_channel_invitation_invitee_uid
    ON public.channel_invitation(invitee_uid);

CREATE INDEX IF NOT EXISTS idx_channel_invitation_status
    ON public.channel_invitation(status);

CREATE INDEX IF NOT EXISTS idx_channel_invitation_code
    ON public.channel_invitation(invitation_code) WHERE invitation_code IS NOT NULL;

-- 复合索引：查询用户的待处理邀请
CREATE INDEX IF NOT EXISTS idx_channel_invitation_invitee_pending
    ON public.channel_invitation(invitee_uid, status, expires_at)
    WHERE status = 0;

-- 复合索引：查询频道的活跃邀请
CREATE INDEX IF NOT EXISTS idx_channel_invitation_channel_active
    ON public.channel_invitation(channel_id, status, created_at DESC)
    WHERE status IN (0, 1);

-- 注释
COMMENT ON TABLE public.channel_invitation IS '频道邀请表 - 私有频道的邀请机制';
COMMENT ON COLUMN public.channel_invitation.id IS '主键';
COMMENT ON COLUMN public.channel_invitation.channel_id IS '频道ID';
COMMENT ON COLUMN public.channel_invitation.inviter_uid IS '邀请人用户ID';
COMMENT ON COLUMN public.channel_invitation.invitee_uid IS '被邀请人用户ID';
COMMENT ON COLUMN public.channel_invitation.invitation_code IS '邀请码（可选，用于分享链接）';
COMMENT ON COLUMN public.channel_invitation.status IS '状态: 0待处理 1已接受 2已拒绝 3已过期 4已取消';
COMMENT ON COLUMN public.channel_invitation.message IS '邀请消息';
COMMENT ON COLUMN public.channel_invitation.expires_at IS '邀请过期时间';
COMMENT ON COLUMN public.channel_invitation.accepted_at IS '接受时间';
COMMENT ON COLUMN public.channel_invitation.created_at IS '创建时间';
COMMENT ON COLUMN public.channel_invitation.updated_at IS '更新时间';

-- ================================================================
-- 3. 频道订单表（付费频道）
-- 用途: 管理付费频道的订阅订单
-- ================================================================

CREATE TABLE IF NOT EXISTS public.channel_order (
    id BIGSERIAL PRIMARY KEY,
    channel_id BIGINT NOT NULL,
    user_id BIGINT NOT NULL,
    order_no VARCHAR(64) NOT NULL UNIQUE,   -- 订单号
    amount DECIMAL(10,2) NOT NULL,          -- 支付金额
    currency VARCHAR(10) NOT NULL DEFAULT 'CNY',
    status SMALLINT NOT NULL DEFAULT 0,     -- 0: 待支付 1: 已支付 2: 已退款 3: 已取消 4: 已过期
    payment_method VARCHAR(20),             -- 支付方式（mock/alipay/wechat等）
    payment_no VARCHAR(128),                -- 第三方支付流水号
    payment_at TIMESTAMPTZ,                 -- 支付时间
    subscription_start_at TIMESTAMPTZ,      -- 订阅开始时间
    subscription_end_at TIMESTAMPTZ,        -- 订阅结束时间（一次性订阅为null）
    expires_at TIMESTAMPTZ NOT NULL,        -- 订单过期时间
    refund_reason VARCHAR(500),             -- 退款原因
    refund_at TIMESTAMPTZ,                  -- 退款时间
    extra_data JSONB,                       -- 扩展数据
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW(),

    CONSTRAINT fk_channel_order_channel FOREIGN KEY (channel_id)
        REFERENCES public.channel(id) ON DELETE CASCADE,
    CONSTRAINT chk_channel_order_status CHECK (status IN (0, 1, 2, 3, 4)),
    CONSTRAINT chk_channel_order_amount CHECK (amount >= 0)
);

-- 索引
CREATE INDEX IF NOT EXISTS idx_channel_order_channel_id
    ON public.channel_order(channel_id);

CREATE INDEX IF NOT EXISTS idx_channel_order_user_id
    ON public.channel_order(user_id);

CREATE INDEX IF NOT EXISTS idx_channel_order_order_no
    ON public.channel_order(order_no);

CREATE INDEX IF NOT EXISTS idx_channel_order_status
    ON public.channel_order(status);

CREATE INDEX IF NOT EXISTS idx_channel_order_payment_no
    ON public.channel_order(payment_no) WHERE payment_no IS NOT NULL;

-- 复合索引：查询用户的订单
CREATE INDEX IF NOT EXISTS idx_channel_order_user_status
    ON public.channel_order(user_id, status, created_at DESC);

-- 复合索引：查询频道的已支付订单
CREATE INDEX IF NOT EXISTS idx_channel_order_channel_paid
    ON public.channel_order(channel_id, status)
    WHERE status = 1;

-- 注释
COMMENT ON TABLE public.channel_order IS '频道订单表 - 付费频道的订阅订单';
COMMENT ON COLUMN public.channel_order.id IS '主键';
COMMENT ON COLUMN public.channel_order.channel_id IS '频道ID';
COMMENT ON COLUMN public.channel_order.user_id IS '用户ID';
COMMENT ON COLUMN public.channel_order.order_no IS '订单号';
COMMENT ON COLUMN public.channel_order.amount IS '支付金额';
COMMENT ON COLUMN public.channel_order.currency IS '货币类型';
COMMENT ON COLUMN public.channel_order.status IS '状态: 0待支付 1已支付 2已退款 3已取消 4已过期';
COMMENT ON COLUMN public.channel_order.payment_method IS '支付方式';
COMMENT ON COLUMN public.channel_order.payment_no IS '第三方支付流水号';
COMMENT ON COLUMN public.channel_order.payment_at IS '支付时间';
COMMENT ON COLUMN public.channel_order.subscription_start_at IS '订阅开始时间';
COMMENT ON COLUMN public.channel_order.subscription_end_at IS '订阅结束时间';
COMMENT ON COLUMN public.channel_order.expires_at IS '订单过期时间';
COMMENT ON COLUMN public.channel_order.refund_reason IS '退款原因';
COMMENT ON COLUMN public.channel_order.refund_at IS '退款时间';
COMMENT ON COLUMN public.channel_order.extra_data IS '扩展数据';
COMMENT ON COLUMN public.channel_order.created_at IS '创建时间';
COMMENT ON COLUMN public.channel_order.updated_at IS '更新时间';

-- ================================================================
-- 触发器函数：自动更新 updated_at
-- ================================================================

CREATE OR REPLACE FUNCTION fn_channel_subscribe_updated_at()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- 为 channel_price 创建触发器
DROP TRIGGER IF EXISTS tr_channel_price_updated_at ON public.channel_price;
CREATE TRIGGER tr_channel_price_updated_at
    BEFORE UPDATE ON public.channel_price
    FOR EACH ROW
    EXECUTE FUNCTION fn_channel_subscribe_updated_at();

-- 为 channel_invitation 创建触发器
DROP TRIGGER IF EXISTS tr_channel_invitation_updated_at ON public.channel_invitation;
CREATE TRIGGER tr_channel_invitation_updated_at
    BEFORE UPDATE ON public.channel_invitation
    FOR EACH ROW
    EXECUTE FUNCTION fn_channel_subscribe_updated_at();

-- 为 channel_order 创建触发器
DROP TRIGGER IF EXISTS tr_channel_order_updated_at ON public.channel_order;
CREATE TRIGGER tr_channel_order_updated_at
    BEFORE UPDATE ON public.channel_order
    FOR EACH ROW
    EXECUTE FUNCTION fn_channel_subscribe_updated_at();

-- ================================================================
-- 触发器：邀请接受后自动订阅
-- ================================================================

CREATE OR REPLACE FUNCTION fn_channel_invitation_accept()
RETURNS TRIGGER AS $$
BEGIN
    -- 当邀请被接受时，自动创建订阅关系
    IF NEW.status = 1 AND OLD.status = 0 THEN
        INSERT INTO public.channel_subscription (channel_id, user_id, subscribed_at, status)
        VALUES (NEW.channel_id, NEW.invitee_uid, NOW(), 1)
        ON CONFLICT (channel_id, user_id)
        DO UPDATE SET status = 1, subscribed_at = NOW();

        -- 更新邀请人的 accepted_at
        NEW.accepted_at := NOW();
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS tr_channel_invitation_accept ON public.channel_invitation;
CREATE TRIGGER tr_channel_invitation_accept
    BEFORE UPDATE ON public.channel_invitation
    FOR EACH ROW
    EXECUTE FUNCTION fn_channel_invitation_accept();

-- ================================================================
-- 清理函数：过期邀请处理
-- ================================================================

CREATE OR REPLACE FUNCTION cleanup_expired_channel_invitations()
RETURNS INTEGER AS $$
DECLARE
    updated_count INTEGER;
BEGIN
    -- 将过期的待处理邀请标记为已过期
    UPDATE public.channel_invitation
    SET status = 3, updated_at = NOW()
    WHERE status = 0 AND expires_at < NOW();

    GET DIAGNOSTICS updated_count = ROW_COUNT;
    RETURN updated_count;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION cleanup_expired_channel_invitations() IS '清理过期的频道邀请';

-- ================================================================
-- 清理函数：过期订单处理
-- ================================================================

CREATE OR REPLACE FUNCTION cleanup_expired_channel_orders()
RETURNS INTEGER AS $$
DECLARE
    updated_count INTEGER;
BEGIN
    -- 将过期的待支付订单标记为已过期
    UPDATE public.channel_order
    SET status = 4, updated_at = NOW()
    WHERE status = 0 AND expires_at < NOW();

    GET DIAGNOSTICS updated_count = ROW_COUNT;
    RETURN updated_count;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION cleanup_expired_channel_orders() IS '清理过期的频道订单';

-- ================================================================
-- 辅助函数：生成订单号
-- ================================================================

CREATE OR REPLACE FUNCTION generate_channel_order_no()
RETURNS VARCHAR(64) AS $$
DECLARE
    v_order_no VARCHAR(64);
    v_timestamp BIGINT;
    v_random VARCHAR(8);
BEGIN
    -- 格式: CH + 时间戳(13位) + 随机数(6位)
    v_timestamp := EXTRACT(EPOCH FROM NOW()) * 1000;
    v_random := LPAD(FLOOR(RANDOM() * 1000000)::TEXT, 6, '0');
    v_order_no := 'CH' || v_timestamp::BIGINT::TEXT || v_random;
    RETURN v_order_no;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION generate_channel_order_no() IS '生成频道订单号';

-- ================================================================
-- 辅助函数：生成邀请码
-- ================================================================

CREATE OR REPLACE FUNCTION generate_channel_invitation_code()
RETURNS VARCHAR(64) AS $$
DECLARE
    v_code VARCHAR(64);
    v_chars VARCHAR(36) := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
BEGIN
    -- 生成8位随机码
    v_code := '';
    FOR i IN 1..8 LOOP
        v_code := v_code || SUBSTRING(v_chars FROM FLOOR(RANDOM() * 36 + 1)::INT FOR 1);
    END LOOP;
    RETURN v_code;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION generate_channel_invitation_code() IS '生成频道邀请码';

-- ================================================================
-- 视图：频道邀请统计
-- ================================================================

CREATE OR REPLACE VIEW v_channel_invitation_stats AS
SELECT
    channel_id,
    COUNT(*) FILTER (WHERE status = 0) AS pending_count,
    COUNT(*) FILTER (WHERE status = 1) AS accepted_count,
    COUNT(*) FILTER (WHERE status = 2) AS rejected_count,
    COUNT(*) FILTER (WHERE status = 3) AS expired_count,
    COUNT(*) AS total_count,
    MAX(created_at) AS last_invitation_at
FROM public.channel_invitation
GROUP BY channel_id;

COMMENT ON VIEW v_channel_invitation_stats IS '频道邀请统计视图';

-- ================================================================
-- 视图：频道订单统计
-- ================================================================

CREATE OR REPLACE VIEW v_channel_order_stats AS
SELECT
    channel_id,
    COUNT(*) FILTER (WHERE status = 0) AS pending_count,
    COUNT(*) FILTER (WHERE status = 1) AS paid_count,
    COUNT(*) FILTER (WHERE status = 2) AS refunded_count,
    COALESCE(SUM(amount) FILTER (WHERE status = 1), 0) AS total_revenue,
    COALESCE(SUM(amount) FILTER (WHERE status = 1 AND created_at > NOW() - INTERVAL '30 days'), 0) AS revenue_30d,
    MAX(created_at) AS last_order_at
FROM public.channel_order
GROUP BY channel_id;

COMMENT ON VIEW v_channel_order_stats IS '频道订单统计视图';

-- ================================================================
-- 验证查询
-- ================================================================

-- 1. 检查表是否创建成功
SELECT tablename FROM pg_tables
WHERE schemaname = 'public'
  AND tablename LIKE 'channel_%'
ORDER BY tablename;

-- 预期结果:
-- tablename
-- -----------------------
-- channel_admin
-- channel_invitation
-- channel_message
-- channel_message_view
-- channel_order
-- channel_price
-- channel_reaction
-- channel_stats_daily
-- channel_subscription

-- 2. 检查函数是否创建成功
SELECT routine_name, routine_type
FROM information_schema.routines
WHERE routine_schema = 'public'
  AND routine_name LIKE 'channel_%' OR routine_name LIKE 'cleanup_%'
ORDER BY routine_name;

-- ================================================================
-- 完成标记
-- ================================================================
-- 迁移版本: 00000057
-- 新增表: 3 (channel_price, channel_invitation, channel_order)
-- 新增触发器: 4
-- 新增函数: 4
-- 新增视图: 2
-- ================================================================
