-- ============================================================
-- 合并迁移 000004: channel
-- 由 70 个历史迁移基线压缩而成 (fresh-install 等价)。
-- 本文件由 erlang_migrate 整体包裹在单事务中执行。
-- ============================================================


--

CREATE TABLE public.channel (
    id bigint NOT NULL,
    name character varying(200) NOT NULL,
    description character varying(2000) DEFAULT ''::character varying NOT NULL,
    avatar character varying(320) DEFAULT ''::character varying NOT NULL,
    type smallint DEFAULT 0 NOT NULL,
    custom_id character varying(100) DEFAULT NULL::character varying,
    creator_uid bigint NOT NULL,
    subscriber_count bigint DEFAULT 0 NOT NULL,
    is_verified boolean DEFAULT false NOT NULL,
    tags jsonb DEFAULT '[]'::jsonb NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_channel_status CHECK ((status = ANY (ARRAY['-1'::integer, 0, 1]))),
    CONSTRAINT chk_channel_subscriber_count CHECK ((subscriber_count >= 0))
);


--;

--

ALTER TABLE ONLY public.channel
    ADD CONSTRAINT channel_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.channel
    ADD CONSTRAINT uk_channel_custom_id UNIQUE (custom_id);


--;

--

CREATE INDEX i_channel_creator_uid ON public.channel USING btree (creator_uid);


--;

--

CREATE INDEX i_channel_custom_id ON public.channel USING btree (custom_id) WHERE (custom_id IS NOT NULL);


--;

--

CREATE INDEX i_channel_status_created ON public.channel USING btree (status, created_at DESC);


--;

--

CREATE INDEX i_channel_subscriber_count ON public.channel USING btree (subscriber_count DESC);


--;

--

CREATE INDEX i_channel_tags ON public.channel USING gin (tags);


--;

--

COMMENT ON TABLE public.channel IS '频道表';


--;

--

COMMENT ON COLUMN public.channel.id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public.channel.name IS '频道名称';


--;

--

COMMENT ON COLUMN public.channel.description IS '频道描述/简介';


--;

--

COMMENT ON COLUMN public.channel.avatar IS '频道头像';


--;

--

COMMENT ON COLUMN public.channel.type IS '类型: 0 公开频道 1 私有频道 2 付费频道';


--;

--

COMMENT ON COLUMN public.channel.custom_id IS '自定义ID，用于用户友好的频道标识';


--;

--

COMMENT ON COLUMN public.channel.creator_uid IS '创建者用户ID';


--;

--

COMMENT ON COLUMN public.channel.subscriber_count IS '订阅者数量';


--;

--

COMMENT ON COLUMN public.channel.is_verified IS '是否已认证';


--;

--

COMMENT ON COLUMN public.channel.tags IS '标签列表，JSON数组格式';


--;

--

COMMENT ON COLUMN public.channel.status IS '状态: -1 删除 0 禁用 1 启用';


--;

--

COMMENT ON COLUMN public.channel.updated_at IS '最后更新时间';


--;

--

COMMENT ON COLUMN public.channel.created_at IS '创建时间';


--;

--

CREATE TABLE public.channel_admin (
    id bigint NOT NULL,
    channel_id bigint NOT NULL,
    user_id bigint NOT NULL,
    role smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.channel_admin
    ADD CONSTRAINT channel_admin_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.channel_admin
    ADD CONSTRAINT uk_channel_admin UNIQUE (channel_id, user_id);


--;

--

CREATE INDEX i_channel_admin_user ON public.channel_admin USING btree (user_id);


--;

--

COMMENT ON TABLE public.channel_admin IS '频道管理员表';


--;

--

COMMENT ON COLUMN public.channel_admin.channel_id IS '频道ID';


--;

--

COMMENT ON COLUMN public.channel_admin.user_id IS '用户ID';


--;

--

COMMENT ON COLUMN public.channel_admin.role IS '角色: 1 编辑 2 管理员 3 创建者';


--;

--

COMMENT ON COLUMN public.channel_admin.created_at IS '添加时间';


--;

--

CREATE TABLE public.channel_invitation (
    id bigint NOT NULL,
    channel_id bigint NOT NULL,
    inviter_uid bigint NOT NULL,
    invitee_uid bigint NOT NULL,
    invitation_code character varying(64),
    status smallint DEFAULT 0 NOT NULL,
    message character varying(500),
    expires_at timestamp with time zone NOT NULL,
    accepted_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone DEFAULT now(),
    CONSTRAINT chk_channel_invitation_status CHECK ((status = ANY (ARRAY[0, 1, 2, 3, 4])))
);


--;

--

ALTER TABLE ONLY public.channel_invitation
    ADD CONSTRAINT channel_invitation_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_channel_invitation_channel_active ON public.channel_invitation USING btree (channel_id, status, created_at DESC) WHERE (status = ANY (ARRAY[0, 1]));


--;

--

CREATE INDEX idx_channel_invitation_channel_id ON public.channel_invitation USING btree (channel_id);


--;

--

CREATE INDEX idx_channel_invitation_code ON public.channel_invitation USING btree (invitation_code) WHERE (invitation_code IS NOT NULL);


--;

--

CREATE INDEX idx_channel_invitation_invitee_pending ON public.channel_invitation USING btree (invitee_uid, status, expires_at) WHERE (status = 0);


--;

--

CREATE INDEX idx_channel_invitation_invitee_uid ON public.channel_invitation USING btree (invitee_uid);


--;

--

CREATE INDEX idx_channel_invitation_inviter_uid ON public.channel_invitation USING btree (inviter_uid);


--;

--

CREATE INDEX idx_channel_invitation_status ON public.channel_invitation USING btree (status);


--;

--

CREATE UNIQUE INDEX uk_channel_invitation_pending ON public.channel_invitation USING btree (channel_id, invitee_uid) WHERE (status = 0);


--;

--

COMMENT ON TABLE public.channel_invitation IS '频道邀请表 - 私有频道的邀请机制';


--;

--

COMMENT ON COLUMN public.channel_invitation.id IS '主键';


--;

--

COMMENT ON COLUMN public.channel_invitation.channel_id IS '频道ID';


--;

--

COMMENT ON COLUMN public.channel_invitation.inviter_uid IS '邀请人用户ID';


--;

--

COMMENT ON COLUMN public.channel_invitation.invitee_uid IS '被邀请人用户ID';


--;

--

COMMENT ON COLUMN public.channel_invitation.invitation_code IS '邀请码（可选，用于分享链接）';


--;

--

COMMENT ON COLUMN public.channel_invitation.status IS '状态: 0待处理 1已接受 2已拒绝 3已过期 4已取消';


--;

--

COMMENT ON COLUMN public.channel_invitation.message IS '邀请消息';


--;

--

COMMENT ON COLUMN public.channel_invitation.expires_at IS '邀请过期时间';


--;

--

COMMENT ON COLUMN public.channel_invitation.accepted_at IS '接受时间';


--;

--

COMMENT ON COLUMN public.channel_invitation.created_at IS '创建时间';


--;

--

COMMENT ON COLUMN public.channel_invitation.updated_at IS '更新时间';


--;

--

CREATE TABLE public.channel_message (
    id bigint NOT NULL,
    channel_id bigint NOT NULL,
    author_id bigint NOT NULL,
    author_name character varying(200) DEFAULT ''::character varying NOT NULL,
    author_avatar character varying(320) DEFAULT ''::character varying NOT NULL,
    content text NOT NULL,
    msg_type character varying(50) DEFAULT 'text'::character varying NOT NULL,
    payload jsonb DEFAULT '{}'::jsonb NOT NULL,
    is_pinned boolean DEFAULT false NOT NULL,
    view_count bigint DEFAULT 0 NOT NULL,
    reaction_summary jsonb DEFAULT '{}'::jsonb NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    revoked boolean DEFAULT false NOT NULL,
    revoked_at timestamp with time zone,
    revoked_by bigint
);


--;

--

ALTER TABLE ONLY public.channel_message
    ADD CONSTRAINT channel_message_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX i_channel_message_author ON public.channel_message USING btree (author_id);


--;

--

CREATE INDEX i_channel_message_channel_created ON public.channel_message USING btree (channel_id, created_at DESC);


--;

--

CREATE INDEX i_channel_message_pinned ON public.channel_message USING btree (channel_id, is_pinned) WHERE (is_pinned = true);


--;

--

CREATE INDEX i_channel_message_revoked ON public.channel_message USING btree (channel_id, revoked, created_at DESC);


--;

--

COMMENT ON TABLE public.channel_message IS '频道消息表';


--;

--

COMMENT ON COLUMN public.channel_message.channel_id IS '频道ID';


--;

--

COMMENT ON COLUMN public.channel_message.author_id IS '发布者用户ID';


--;

--

COMMENT ON COLUMN public.channel_message.author_name IS '发布者昵称';


--;

--

COMMENT ON COLUMN public.channel_message.author_avatar IS '发布者头像';


--;

--

COMMENT ON COLUMN public.channel_message.content IS '消息内容';


--;

--

COMMENT ON COLUMN public.channel_message.msg_type IS '消息类型';


--;

--

COMMENT ON COLUMN public.channel_message.payload IS '扩展数据，JSON格式';


--;

--

COMMENT ON COLUMN public.channel_message.is_pinned IS '是否置顶';


--;

--

COMMENT ON COLUMN public.channel_message.view_count IS '阅读量';


--;

--

COMMENT ON COLUMN public.channel_message.reaction_summary IS '反应统计，JSON格式';


--;

--

COMMENT ON COLUMN public.channel_message.status IS '状态: -1 删除 1 正常';


--;

--

COMMENT ON COLUMN public.channel_message.updated_at IS '更新时间';


--;

--

COMMENT ON COLUMN public.channel_message.created_at IS '创建时间';


--;

--

COMMENT ON COLUMN public.channel_message.revoked IS '是否已撤回';


--;

--

COMMENT ON COLUMN public.channel_message.revoked_at IS '撤回时间';


--;

--

COMMENT ON COLUMN public.channel_message.revoked_by IS '撤回操作人';


--;

--

CREATE TABLE public.channel_message_view (
    id bigint NOT NULL,
    message_id bigint NOT NULL,
    channel_id bigint NOT NULL,
    user_id bigint NOT NULL,
    viewed_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.channel_message_view
    ADD CONSTRAINT channel_message_view_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.channel_message_view
    ADD CONSTRAINT uk_channel_message_view UNIQUE (message_id, user_id);


--;

--

CREATE INDEX i_channel_message_view_channel_user ON public.channel_message_view USING btree (channel_id, user_id);


--;

--

CREATE INDEX i_channel_message_view_message ON public.channel_message_view USING btree (message_id);


--;

--

CREATE INDEX i_channel_message_view_viewed_at ON public.channel_message_view USING btree (viewed_at DESC);


--;

--

COMMENT ON TABLE public.channel_message_view IS '频道消息阅读记录表';


--;

--

COMMENT ON COLUMN public.channel_message_view.message_id IS '消息ID';


--;

--

COMMENT ON COLUMN public.channel_message_view.channel_id IS '频道ID';


--;

--

COMMENT ON COLUMN public.channel_message_view.user_id IS '阅读者用户ID';


--;

--

COMMENT ON COLUMN public.channel_message_view.viewed_at IS '阅读时间';


--;

--

CREATE TABLE public.channel_order (
    id bigint NOT NULL,
    channel_id bigint NOT NULL,
    user_id bigint NOT NULL,
    order_no character varying(64) NOT NULL,
    amount numeric(10,2) NOT NULL,
    currency character varying(10) DEFAULT 'CNY'::character varying NOT NULL,
    status smallint DEFAULT 0 NOT NULL,
    payment_method character varying(20),
    payment_no character varying(128),
    payment_at timestamp with time zone,
    subscription_start_at timestamp with time zone,
    subscription_end_at timestamp with time zone,
    expires_at timestamp with time zone NOT NULL,
    refund_reason character varying(500),
    refund_at timestamp with time zone,
    extra_data jsonb,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone DEFAULT now(),
    CONSTRAINT chk_channel_order_amount CHECK ((amount >= (0)::numeric)),
    CONSTRAINT chk_channel_order_status CHECK ((status = ANY (ARRAY[0, 1, 2, 3, 4])))
);


--;

--

ALTER TABLE ONLY public.channel_order
    ADD CONSTRAINT channel_order_order_no_key UNIQUE (order_no);


--;

--

ALTER TABLE ONLY public.channel_order
    ADD CONSTRAINT channel_order_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_channel_order_channel_id ON public.channel_order USING btree (channel_id);


--;

--

CREATE INDEX idx_channel_order_channel_paid ON public.channel_order USING btree (channel_id, status) WHERE (status = 1);


--;

--

CREATE INDEX idx_channel_order_order_no ON public.channel_order USING btree (order_no);


--;

--

CREATE INDEX idx_channel_order_payment_no ON public.channel_order USING btree (payment_no) WHERE (payment_no IS NOT NULL);


--;

--

CREATE INDEX idx_channel_order_status ON public.channel_order USING btree (status);


--;

--

CREATE INDEX idx_channel_order_user_id ON public.channel_order USING btree (user_id);


--;

--

CREATE INDEX idx_channel_order_user_status ON public.channel_order USING btree (user_id, status, created_at DESC);


--;

--

COMMENT ON TABLE public.channel_order IS '频道订单表 - 付费频道的订阅订单';


--;

--

COMMENT ON COLUMN public.channel_order.id IS '主键';


--;

--

COMMENT ON COLUMN public.channel_order.channel_id IS '频道ID';


--;

--

COMMENT ON COLUMN public.channel_order.user_id IS '用户ID';


--;

--

COMMENT ON COLUMN public.channel_order.order_no IS '订单号';


--;

--

COMMENT ON COLUMN public.channel_order.amount IS '支付金额';


--;

--

COMMENT ON COLUMN public.channel_order.currency IS '货币类型';


--;

--

COMMENT ON COLUMN public.channel_order.status IS '状态: 0待支付 1已支付 2已退款 3已取消 4已过期';


--;

--

COMMENT ON COLUMN public.channel_order.payment_method IS '支付方式';


--;

--

COMMENT ON COLUMN public.channel_order.payment_no IS '第三方支付流水号';


--;

--

COMMENT ON COLUMN public.channel_order.payment_at IS '支付时间';


--;

--

COMMENT ON COLUMN public.channel_order.subscription_start_at IS '订阅开始时间';


--;

--

COMMENT ON COLUMN public.channel_order.subscription_end_at IS '订阅结束时间';


--;

--

COMMENT ON COLUMN public.channel_order.expires_at IS '订单过期时间';


--;

--

COMMENT ON COLUMN public.channel_order.refund_reason IS '退款原因';


--;

--

COMMENT ON COLUMN public.channel_order.refund_at IS '退款时间';


--;

--

COMMENT ON COLUMN public.channel_order.extra_data IS '扩展数据';


--;

--

COMMENT ON COLUMN public.channel_order.created_at IS '创建时间';


--;

--

COMMENT ON COLUMN public.channel_order.updated_at IS '更新时间';


--;

--

CREATE TABLE public.channel_price (
    id bigint NOT NULL,
    channel_id bigint NOT NULL,
    price numeric(10,2) DEFAULT 0 NOT NULL,
    currency character varying(10) DEFAULT 'CNY'::character varying NOT NULL,
    subscription_type smallint DEFAULT 1 NOT NULL,
    original_price numeric(10,2),
    description character varying(500),
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone DEFAULT now(),
    CONSTRAINT chk_channel_price CHECK ((price >= (0)::numeric))
);


--;

--

ALTER TABLE ONLY public.channel_price
    ADD CONSTRAINT channel_price_channel_id_key UNIQUE (channel_id);


--;

--

ALTER TABLE ONLY public.channel_price
    ADD CONSTRAINT channel_price_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_channel_price_status ON public.channel_price USING btree (status);


--;

--

COMMENT ON TABLE public.channel_price IS '频道价格配置表 - 付费频道的订阅价格';


--;

--

COMMENT ON COLUMN public.channel_price.id IS '主键';


--;

--

COMMENT ON COLUMN public.channel_price.channel_id IS '频道ID';


--;

--

COMMENT ON COLUMN public.channel_price.price IS '当前价格';


--;

--

COMMENT ON COLUMN public.channel_price.currency IS '货币类型（CNY/USD等）';


--;

--

COMMENT ON COLUMN public.channel_price.subscription_type IS '订阅类型: 1一次性 2月订阅 3年订阅';


--;

--

COMMENT ON COLUMN public.channel_price.original_price IS '原价（用于显示折扣）';


--;

--

COMMENT ON COLUMN public.channel_price.description IS '价格说明';


--;

--

COMMENT ON COLUMN public.channel_price.status IS '状态: 0禁用 1启用';


--;

--

COMMENT ON COLUMN public.channel_price.created_at IS '创建时间';


--;

--

COMMENT ON COLUMN public.channel_price.updated_at IS '更新时间';


--;

--

CREATE TABLE public.channel_reaction (
    id bigint NOT NULL,
    message_id bigint NOT NULL,
    channel_id bigint NOT NULL,
    user_id bigint NOT NULL,
    reaction_type character varying(50) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.channel_reaction
    ADD CONSTRAINT channel_reaction_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.channel_reaction
    ADD CONSTRAINT uk_channel_reaction UNIQUE (message_id, user_id, reaction_type);


--;

--

CREATE INDEX i_channel_reaction_channel ON public.channel_reaction USING btree (channel_id);


--;

--

CREATE INDEX i_channel_reaction_message ON public.channel_reaction USING btree (message_id);


--;

--

CREATE INDEX i_channel_reaction_type ON public.channel_reaction USING btree (reaction_type);


--;

--

CREATE INDEX i_channel_reaction_user ON public.channel_reaction USING btree (user_id);


--;

--

COMMENT ON TABLE public.channel_reaction IS '频道消息反应表';


--;

--

COMMENT ON COLUMN public.channel_reaction.message_id IS '消息ID';


--;

--

COMMENT ON COLUMN public.channel_reaction.channel_id IS '频道ID';


--;

--

COMMENT ON COLUMN public.channel_reaction.user_id IS '反应者用户ID';


--;

--

COMMENT ON COLUMN public.channel_reaction.reaction_type IS '反应类型: like, heart, fire, thumbs_up, bookmark 等';


--;

--

COMMENT ON COLUMN public.channel_reaction.created_at IS '反应时间';


--;

--

CREATE TABLE public.channel_stats_daily (
    id bigint NOT NULL,
    channel_id bigint NOT NULL,
    stats_date date NOT NULL,
    new_subscribers integer DEFAULT 0 NOT NULL,
    unsubscribers integer DEFAULT 0 NOT NULL,
    net_subscribers integer DEFAULT 0 NOT NULL,
    messages_count integer DEFAULT 0 NOT NULL,
    total_views bigint DEFAULT 0 NOT NULL,
    total_reactions integer DEFAULT 0 NOT NULL,
    active_viewers integer DEFAULT 0 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);


--;

--

ALTER TABLE ONLY public.channel_stats_daily
    ADD CONSTRAINT channel_stats_daily_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.channel_stats_daily
    ADD CONSTRAINT uk_channel_stats_daily UNIQUE (channel_id, stats_date);


--;

--

CREATE INDEX i_channel_stats_daily_channel ON public.channel_stats_daily USING btree (channel_id, stats_date DESC);


--;

--

CREATE INDEX i_channel_stats_daily_date ON public.channel_stats_daily USING btree (stats_date DESC);


--;

--

COMMENT ON TABLE public.channel_stats_daily IS '频道每日统计汇总表';


--;

--

COMMENT ON COLUMN public.channel_stats_daily.channel_id IS '频道ID';


--;

--

COMMENT ON COLUMN public.channel_stats_daily.stats_date IS '统计日期';


--;

--

COMMENT ON COLUMN public.channel_stats_daily.new_subscribers IS '当日新增订阅数';


--;

--

COMMENT ON COLUMN public.channel_stats_daily.unsubscribers IS '当日取消订阅数';


--;

--

COMMENT ON COLUMN public.channel_stats_daily.net_subscribers IS '当日净增订阅数';


--;

--

COMMENT ON COLUMN public.channel_stats_daily.messages_count IS '当日发布消息数';


--;

--

COMMENT ON COLUMN public.channel_stats_daily.total_views IS '当日总阅读量';


--;

--

COMMENT ON COLUMN public.channel_stats_daily.total_reactions IS '当日总反应数';


--;

--

COMMENT ON COLUMN public.channel_stats_daily.active_viewers IS '当日活跃阅读者数';


--;

--

CREATE TABLE public.channel_subscription (
    id bigint NOT NULL,
    channel_id bigint NOT NULL,
    user_id bigint NOT NULL,
    subscribed_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_read_at timestamp with time zone,
    last_message_id bigint DEFAULT 0,
    unread_count integer DEFAULT 0 NOT NULL,
    notifications_enabled boolean DEFAULT true NOT NULL,
    is_pinned boolean DEFAULT false NOT NULL,
    is_muted boolean DEFAULT false NOT NULL,
    status smallint DEFAULT 1 NOT NULL
);


--;

--

ALTER TABLE ONLY public.channel_subscription
    ADD CONSTRAINT channel_subscription_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.channel_subscription
    ADD CONSTRAINT uk_channel_subscription UNIQUE (channel_id, user_id);


--;

--

CREATE INDEX i_channel_subscription_channel ON public.channel_subscription USING btree (channel_id, status);


--;

--

CREATE INDEX i_channel_subscription_user ON public.channel_subscription USING btree (user_id, status);


--;

--

COMMENT ON TABLE public.channel_subscription IS '频道订阅关系表';


--;

--

COMMENT ON COLUMN public.channel_subscription.channel_id IS '频道ID';


--;

--

COMMENT ON COLUMN public.channel_subscription.user_id IS '用户ID';


--;

--

COMMENT ON COLUMN public.channel_subscription.subscribed_at IS '订阅时间';


--;

--

COMMENT ON COLUMN public.channel_subscription.last_read_at IS '最后阅读时间';


--;

--

COMMENT ON COLUMN public.channel_subscription.last_message_id IS '最后已读消息ID';


--;

--

COMMENT ON COLUMN public.channel_subscription.unread_count IS '未读消息数';


--;

--

COMMENT ON COLUMN public.channel_subscription.notifications_enabled IS '是否开启通知';


--;

--

COMMENT ON COLUMN public.channel_subscription.is_pinned IS '是否置顶';


--;

--

COMMENT ON COLUMN public.channel_subscription.is_muted IS '是否免打扰';


--;

--

COMMENT ON COLUMN public.channel_subscription.status IS '状态: 0 取消订阅 1 已订阅';


--;

