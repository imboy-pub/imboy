-- ============================================================
-- 合并迁移 000005: social
-- 由 70 个历史迁移基线压缩而成 (fresh-install 等价)。
-- 本文件由 erlang_migrate 整体包裹在单事务中执行。
-- ============================================================


--

CREATE TABLE public.moment_post (
    id bigint NOT NULL,
    author_uid bigint NOT NULL,
    content text DEFAULT ''::text NOT NULL,
    media jsonb DEFAULT '[]'::jsonb NOT NULL,
    visibility smallint DEFAULT 1 NOT NULL,
    allow_comment boolean DEFAULT true NOT NULL,
    like_count integer DEFAULT 0 NOT NULL,
    comment_count integer DEFAULT 0 NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_moment_post_comment_count CHECK ((comment_count >= 0)),
    CONSTRAINT chk_moment_post_like_count CHECK ((like_count >= 0)),
    CONSTRAINT chk_moment_post_visibility CHECK ((visibility = ANY (ARRAY[0, 1, 2, 3, 4])))
);


--;

--

ALTER TABLE ONLY public.moment_post
    ADD CONSTRAINT moment_post_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_moment_post_active_id_desc ON public.moment_post USING btree (id DESC) WHERE (status = 1);


--;

--

CREATE INDEX idx_moment_post_author_active_id_desc ON public.moment_post USING btree (author_uid, id DESC) WHERE (status = 1);


--;

--

CREATE INDEX idx_moment_post_author_created ON public.moment_post USING btree (author_uid, created_at DESC);


--;

--

CREATE INDEX idx_moment_post_status_created ON public.moment_post USING btree (status, created_at DESC);


--;

--

CREATE INDEX idx_moment_post_visibility_status ON public.moment_post USING btree (visibility, status, created_at DESC);


--;

--

COMMENT ON TABLE public.moment_post IS '朋友圈动态主表';


--;

--

COMMENT ON COLUMN public.moment_post.author_uid IS '动态发布者用户ID';


--;

--

COMMENT ON COLUMN public.moment_post.content IS '动态文本内容';


--;

--

COMMENT ON COLUMN public.moment_post.media IS '媒体数组(JSONB): image/video 元数据';


--;

--

COMMENT ON COLUMN public.moment_post.visibility IS '可见性: 0公开 1仅好友 2仅自己 3部分可见 4不给谁看';


--;

--

COMMENT ON COLUMN public.moment_post.allow_comment IS '是否允许评论';


--;

--

COMMENT ON COLUMN public.moment_post.like_count IS '点赞数量缓存';


--;

--

COMMENT ON COLUMN public.moment_post.comment_count IS '评论数量缓存';


--;

--

COMMENT ON COLUMN public.moment_post.status IS '状态: -1删除 0禁用 1正常';


--;

--

CREATE TABLE public.moment_post_acl (
    id bigint NOT NULL,
    post_id bigint NOT NULL,
    uid bigint NOT NULL,
    acl_type smallint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_moment_post_acl_type CHECK ((acl_type = ANY (ARRAY[1, 2])))
);


--;

--

ALTER TABLE ONLY public.moment_post_acl
    ADD CONSTRAINT moment_post_acl_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.moment_post_acl
    ADD CONSTRAINT uk_moment_post_acl UNIQUE (post_id, uid, acl_type);


--;

--

CREATE INDEX idx_moment_post_acl_post ON public.moment_post_acl USING btree (post_id, acl_type);


--;

--

CREATE INDEX idx_moment_post_acl_uid ON public.moment_post_acl USING btree (uid, acl_type);


--;

--

COMMENT ON TABLE public.moment_post_acl IS '动态可见性ACL表(allow/deny)';


--;

--

COMMENT ON COLUMN public.moment_post_acl.acl_type IS '1=allow 2=deny';


--;

--

CREATE TABLE public.moment_comment (
    id bigint NOT NULL,
    post_id bigint NOT NULL,
    user_id bigint NOT NULL,
    reply_to_uid bigint,
    content character varying(500) NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--;

--

ALTER TABLE ONLY public.moment_comment
    ADD CONSTRAINT moment_comment_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_moment_comment_post_active_id_desc ON public.moment_comment USING btree (post_id, id DESC) WHERE (status = 1);


--;

--

CREATE INDEX idx_moment_comment_post_created ON public.moment_comment USING btree (post_id, created_at DESC);


--;

--

CREATE INDEX idx_moment_comment_user_created ON public.moment_comment USING btree (user_id, created_at DESC);


--;

--

COMMENT ON TABLE public.moment_comment IS '朋友圈评论表';


--;

--

CREATE TABLE public.moment_like (
    id bigint NOT NULL,
    post_id bigint NOT NULL,
    user_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--;

--

ALTER TABLE ONLY public.moment_like
    ADD CONSTRAINT moment_like_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.moment_like
    ADD CONSTRAINT uk_moment_like_post_user UNIQUE (post_id, user_id);


--;

--

CREATE INDEX idx_moment_like_post_created ON public.moment_like USING btree (post_id, created_at DESC);


--;

--

CREATE INDEX idx_moment_like_post_id_desc ON public.moment_like USING btree (post_id, id DESC);


--;

--

CREATE INDEX idx_moment_like_user_created ON public.moment_like USING btree (user_id, created_at DESC);


--;

--

COMMENT ON TABLE public.moment_like IS '朋友圈点赞关系表';


--;

--

CREATE TABLE public.moment_report (
    id bigint NOT NULL,
    post_id bigint NOT NULL,
    reporter_uid bigint NOT NULL,
    reason character varying(64) DEFAULT ''::character varying NOT NULL,
    description character varying(500) DEFAULT ''::character varying NOT NULL,
    status smallint DEFAULT 0 NOT NULL,
    handled_by bigint,
    handled_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_moment_report_status CHECK ((status = ANY (ARRAY[0, 1, 2])))
);


--;

--

ALTER TABLE ONLY public.moment_report
    ADD CONSTRAINT moment_report_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.moment_report
    ADD CONSTRAINT uk_moment_report_post_reporter UNIQUE (post_id, reporter_uid);


--;

--

CREATE INDEX idx_moment_report_pending_id_desc ON public.moment_report USING btree (id DESC) WHERE (status = 0);


--;

--

CREATE INDEX idx_moment_report_post ON public.moment_report USING btree (post_id, created_at DESC);


--;

--

CREATE INDEX idx_moment_report_status_created ON public.moment_report USING btree (status, created_at DESC);


--;

--

COMMENT ON TABLE public.moment_report IS '朋友圈举报记录表';


--;

--

COMMENT ON COLUMN public.moment_report.status IS '0待处理 1已驳回 2已确认违规';


--;

--

CREATE TABLE public.moment_timeline (
    id bigint NOT NULL,
    recipient_uid bigint NOT NULL,
    post_id bigint NOT NULL,
    author_uid bigint NOT NULL,
    rank_at timestamp with time zone NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--;

--

ALTER TABLE ONLY public.moment_timeline
    ADD CONSTRAINT moment_timeline_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.moment_timeline
    ADD CONSTRAINT uk_moment_timeline_uid_post UNIQUE (recipient_uid, post_id);


--;

--

CREATE INDEX idx_moment_timeline_author ON public.moment_timeline USING btree (author_uid, rank_at DESC);


--;

--

CREATE INDEX idx_moment_timeline_recipient_active_id_desc ON public.moment_timeline USING btree (recipient_uid, id DESC) WHERE (status = 1);


--;

--

CREATE INDEX idx_moment_timeline_uid_rank ON public.moment_timeline USING btree (recipient_uid, rank_at DESC, id DESC);


--;

--

COMMENT ON TABLE public.moment_timeline IS '朋友圈时间线收件箱';


--;

--

COMMENT ON COLUMN public.moment_timeline.rank_at IS '时间线排序时间';


--;

--

CREATE TABLE public.live_room (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    title character varying(100) DEFAULT ''::character varying NOT NULL,
    cover character varying(255) DEFAULT ''::character varying,
    stream_key character varying(64) NOT NULL,
    status smallint DEFAULT 0 NOT NULL,
    viewer_count integer DEFAULT 0 NOT NULL,
    tag_id bigint DEFAULT 0,
    scene smallint DEFAULT 1,
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_live_room_status CHECK ((status = ANY (ARRAY[0, 1, 2])))
);


--;

--

ALTER TABLE ONLY public.live_room
    ADD CONSTRAINT live_room_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.live_room
    ADD CONSTRAINT live_room_stream_key_key UNIQUE (stream_key);


--;

--

CREATE INDEX i_live_room_userid ON public.live_room USING btree (user_id);


--;

--

CREATE INDEX idx_live_room_status_time ON public.live_room USING btree (status, created_at DESC);


--;

--

CREATE TABLE public.e2ee_key_shares (
    id bigint NOT NULL,
    owner_uid bigint NOT NULL,
    trustee_uid bigint NOT NULL,
    encrypted_share text NOT NULL,
    share_index integer NOT NULL,
    threshold integer DEFAULT 2 NOT NULL,
    total_shares integer DEFAULT 3 NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone DEFAULT now(),
    CONSTRAINT e2ee_key_shares_owner_uid_check CHECK ((owner_uid > 0)),
    CONSTRAINT e2ee_key_shares_share_index_check CHECK (((share_index >= 1) AND (share_index <= 5))),
    CONSTRAINT e2ee_key_shares_threshold_check CHECK (((threshold >= 1) AND (threshold <= total_shares))),
    CONSTRAINT e2ee_key_shares_total_shares_check CHECK (((total_shares >= 2) AND (total_shares <= 5))),
    CONSTRAINT e2ee_key_shares_trustee_uid_check CHECK ((trustee_uid > 0))
);


--;

--

ALTER TABLE ONLY public.e2ee_key_shares
    ADD CONSTRAINT e2ee_key_shares_owner_uid_trustee_uid_key UNIQUE (owner_uid, trustee_uid);


--;

--

ALTER TABLE ONLY public.e2ee_key_shares
    ADD CONSTRAINT e2ee_key_shares_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_e2ee_key_shares_owner_uid ON public.e2ee_key_shares USING btree (owner_uid);


--;

--

CREATE INDEX idx_e2ee_key_shares_share_index ON public.e2ee_key_shares USING btree (share_index);


--;

--

CREATE INDEX idx_e2ee_key_shares_trustee_uid ON public.e2ee_key_shares USING btree (trustee_uid);


--;

--

COMMENT ON TABLE public.e2ee_key_shares IS 'E2EE 密钥分片表（社交恢复）';


--;

--

COMMENT ON COLUMN public.e2ee_key_shares.owner_uid IS '密钥所有者 ID';


--;

--

COMMENT ON COLUMN public.e2ee_key_shares.trustee_uid IS '受托人 ID（存储密钥分片的好友）';


--;

--

COMMENT ON COLUMN public.e2ee_key_shares.encrypted_share IS '加密的密钥分片，使用受托人公钥加密';


--;

--

COMMENT ON COLUMN public.e2ee_key_shares.share_index IS '分片索引（1-5）';


--;

--

COMMENT ON COLUMN public.e2ee_key_shares.threshold IS '恢复阈值（需要多少分片才能恢复）';


--;

--

COMMENT ON COLUMN public.e2ee_key_shares.total_shares IS '总分片数（默认 3）';


--;

--

COMMENT ON COLUMN public.e2ee_key_shares.created_at IS '创建时间';


--;

--

COMMENT ON COLUMN public.e2ee_key_shares.updated_at IS '更新时间';


--;

--

CREATE TABLE public.e2ee_local_backups (
    id bigint NOT NULL,
    uid bigint NOT NULL,
    device_id character varying(64) NOT NULL,
    backup_version integer NOT NULL,
    key_checksum character varying(64) NOT NULL,
    file_size bigint,
    user_notes text,
    created_at timestamp with time zone DEFAULT now(),
    CONSTRAINT e2ee_local_backups_backup_version_check CHECK ((backup_version > 0)),
    CONSTRAINT e2ee_local_backups_file_size_check CHECK ((file_size >= 0)),
    CONSTRAINT e2ee_local_backups_uid_check CHECK ((uid > 0))
);


--;

--

ALTER TABLE ONLY public.e2ee_local_backups
    ADD CONSTRAINT e2ee_local_backups_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.e2ee_local_backups
    ADD CONSTRAINT uk_e2ee_local_backup_version UNIQUE (uid, device_id, backup_version);


--;

--

CREATE INDEX idx_e2ee_local_backups_created_at ON public.e2ee_local_backups USING btree (created_at);


--;

--

CREATE INDEX idx_e2ee_local_backups_device_id ON public.e2ee_local_backups USING btree (device_id);


--;

--

CREATE INDEX idx_e2ee_local_backups_uid ON public.e2ee_local_backups USING btree (uid);


--;

--

COMMENT ON TABLE public.e2ee_local_backups IS 'E2EE 本地备份元数据表（仅记录备份历史）';


--;

--

COMMENT ON COLUMN public.e2ee_local_backups.uid IS '用户 ID';


--;

--

COMMENT ON COLUMN public.e2ee_local_backups.device_id IS '设备 ID';


--;

--

COMMENT ON COLUMN public.e2ee_local_backups.backup_version IS '备份版本号（递增）';


--;

--

COMMENT ON COLUMN public.e2ee_local_backups.key_checksum IS '密钥校验和（SHA-256），用于验证备份完整性';


--;

--

COMMENT ON COLUMN public.e2ee_local_backups.file_size IS '备份文件大小（bytes）';


--;

--

COMMENT ON COLUMN public.e2ee_local_backups.user_notes IS '备份备注（用户可选）';


--;

--

COMMENT ON COLUMN public.e2ee_local_backups.created_at IS '创建时间';


--;

--

CREATE TABLE public.e2ee_shard_transmission_log (
    id bigint NOT NULL,
    shard_id character varying(64) NOT NULL,
    key_version character varying(32) NOT NULL,
    uid bigint NOT NULL,
    proxy_uid bigint NOT NULL,
    action character varying(32) NOT NULL,
    direction character varying(16) NOT NULL,
    metadata jsonb,
    ip_address inet,
    user_agent text,
    created_at timestamp with time zone DEFAULT now(),
    CONSTRAINT e2ee_shard_transmission_log_action_check CHECK (((action)::text = ANY ((ARRAY['shard_created'::character varying, 'shard_sent'::character varying, 'shard_stored'::character varying, 'shard_decrypted'::character varying, 'shard_recovered'::character varying])::text[]))),
    CONSTRAINT e2ee_shard_transmission_log_direction_check CHECK (((direction)::text = ANY ((ARRAY['server_to_proxy'::character varying, 'proxy_to_user'::character varying])::text[]))),
    CONSTRAINT e2ee_shard_transmission_log_proxy_uid_check CHECK ((proxy_uid > 0)),
    CONSTRAINT e2ee_shard_transmission_log_uid_check CHECK ((uid > 0))
);


--;

--

ALTER TABLE ONLY public.e2ee_shard_transmission_log
    ADD CONSTRAINT e2ee_shard_transmission_log_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_e2ee_shard_transmission_log_action ON public.e2ee_shard_transmission_log USING btree (action);


--;

--

CREATE INDEX idx_e2ee_shard_transmission_log_created_at ON public.e2ee_shard_transmission_log USING btree (created_at);


--;

--

CREATE INDEX idx_e2ee_shard_transmission_log_key_version ON public.e2ee_shard_transmission_log USING btree (key_version);


--;

--

CREATE INDEX idx_e2ee_shard_transmission_log_proxy_uid ON public.e2ee_shard_transmission_log USING btree (proxy_uid);


--;

--

CREATE INDEX idx_e2ee_shard_transmission_log_shard_action_time ON public.e2ee_shard_transmission_log USING btree (shard_id, action, created_at);


--;

--

CREATE INDEX idx_e2ee_shard_transmission_log_shard_id ON public.e2ee_shard_transmission_log USING btree (shard_id);


--;

--

CREATE INDEX idx_e2ee_shard_transmission_log_uid ON public.e2ee_shard_transmission_log USING btree (uid);


--;

--

CREATE INDEX idx_e2ee_shard_transmission_log_uid_time ON public.e2ee_shard_transmission_log USING btree (uid, created_at DESC);


--;

--

COMMENT ON TABLE public.e2ee_shard_transmission_log IS 'E2EE 分片传输日志表 - 零信任架构下的分片传输审计';


--;

--

COMMENT ON COLUMN public.e2ee_shard_transmission_log.id IS '主键';


--;

--

COMMENT ON COLUMN public.e2ee_shard_transmission_log.shard_id IS '分片唯一标识符';


--;

--

COMMENT ON COLUMN public.e2ee_shard_transmission_log.key_version IS '密钥版本号';


--;

--

COMMENT ON COLUMN public.e2ee_shard_transmission_log.uid IS '密钥所有者用户 ID（原始 ID，未 HashID 编码）';


--;

--

COMMENT ON COLUMN public.e2ee_shard_transmission_log.proxy_uid IS '代理用户 ID（原始 ID，未 HashID 编码）';


--;

--

COMMENT ON COLUMN public.e2ee_shard_transmission_log.action IS '操作类型：shard_created/shard_sent/shard_stored/shard_decrypted/shard_recovered';


--;

--

COMMENT ON COLUMN public.e2ee_shard_transmission_log.direction IS '传输方向：server_to_proxy(S2C) 或 proxy_to_user(C2C)';


--;

--

COMMENT ON COLUMN public.e2ee_shard_transmission_log.metadata IS '额外元数据（JSONB 格式，包含加密状态、时间戳等）';


--;

--

COMMENT ON COLUMN public.e2ee_shard_transmission_log.ip_address IS '客户端 IP 地址';


--;

--

COMMENT ON COLUMN public.e2ee_shard_transmission_log.user_agent IS '客户端 User-Agent';


--;

--

COMMENT ON COLUMN public.e2ee_shard_transmission_log.created_at IS '创建时间';


--;

--

CREATE TABLE public.e2ee_social_shards (
    id bigint NOT NULL,
    uid bigint NOT NULL,
    key_version character varying(32) DEFAULT 'latest'::character varying NOT NULL,
    shard_index integer NOT NULL,
    total_shards integer NOT NULL,
    threshold integer NOT NULL,
    encrypted_shard text NOT NULL,
    proxy_uid bigint NOT NULL,
    shard_id character varying(64) NOT NULL,
    status character varying(20) DEFAULT 'active'::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    used_at timestamp with time zone
);


--;

--

ALTER TABLE ONLY public.e2ee_social_shards
    ADD CONSTRAINT e2ee_social_shards_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.e2ee_social_shards
    ADD CONSTRAINT e2ee_social_shards_shard_id_key UNIQUE (shard_id);


--;

--

CREATE INDEX idx_e2ee_social_shards_key_version ON public.e2ee_social_shards USING btree (key_version);


--;

--

CREATE INDEX idx_e2ee_social_shards_proxy_active ON public.e2ee_social_shards USING btree (proxy_uid, status) WHERE ((status)::text = 'active'::text);


--;

--

CREATE INDEX idx_e2ee_social_shards_proxy_uid ON public.e2ee_social_shards USING btree (proxy_uid);


--;

--

CREATE INDEX idx_e2ee_social_shards_shard_id ON public.e2ee_social_shards USING btree (shard_id);


--;

--

CREATE INDEX idx_e2ee_social_shards_status ON public.e2ee_social_shards USING btree (status);


--;

--

CREATE INDEX idx_e2ee_social_shards_uid ON public.e2ee_social_shards USING btree (uid);


--;

--

CREATE UNIQUE INDEX idx_e2ee_social_shards_unique_active ON public.e2ee_social_shards USING btree (uid, key_version) WHERE ((status)::text = 'active'::text);


--;

--

CREATE INDEX idx_e2ee_social_shards_user_active ON public.e2ee_social_shards USING btree (uid, key_version, status) WHERE ((status)::text = 'active'::text);


--;

--

COMMENT ON TABLE public.e2ee_social_shards IS 'E2EE 社交恢复分片表 - 使用 Shamir Secret Sharing 将密钥分割存储在信任的联系人处';


--;

--

COMMENT ON COLUMN public.e2ee_social_shards.uid IS '用户 ID - 密钥所有者';


--;

--

COMMENT ON COLUMN public.e2ee_social_shards.key_version IS '密钥版本号 - 用于标识不同版本的密钥';


--;

--

COMMENT ON COLUMN public.e2ee_social_shards.shard_index IS '分片索引 - 从 0 开始的编号';


--;

--

COMMENT ON COLUMN public.e2ee_social_shards.total_shards IS '总分片数 - 通常为 3 或 5';


--;

--

COMMENT ON COLUMN public.e2ee_social_shards.threshold IS '恢复阈值 - 需要多少分片才能恢复（通常为 2）';


--;

--

COMMENT ON COLUMN public.e2ee_social_shards.encrypted_shard IS '加密的分片数据 - 使用代理的公钥加密';


--;

--

COMMENT ON COLUMN public.e2ee_social_shards.proxy_uid IS '代理用户 ID - 存储分片的信任联系人';


--;

--

COMMENT ON COLUMN public.e2ee_social_shards.shard_id IS '分片唯一标识符';


--;

--

COMMENT ON COLUMN public.e2ee_social_shards.status IS '状态 - active（活跃）或 used（已使用）';


--;

--

COMMENT ON COLUMN public.e2ee_social_shards.used_at IS '使用时间 - 分片被用于恢复密钥的时间';


--;

--

CREATE TABLE public.e2ee_transfer_sessions (
    id bigint NOT NULL,
    session_id uuid NOT NULL,
    from_uid bigint NOT NULL,
    from_device_id character varying(64) NOT NULL,
    to_uid bigint NOT NULL,
    to_device_id character varying(64),
    status character varying(20) DEFAULT 'pending'::character varying NOT NULL,
    encrypted_key_bundle text NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    confirmed_at timestamp with time zone,
    updated_at timestamp with time zone DEFAULT now(),
    CONSTRAINT chk_e2ee_transfer_sessions_status CHECK (((status)::text = ANY ((ARRAY['pending'::character varying, 'accepted'::character varying, 'confirmed'::character varying, 'expired'::character varying, 'cancelled'::character varying])::text[]))),
    CONSTRAINT e2ee_transfer_sessions_from_uid_check CHECK ((from_uid > 0)),
    CONSTRAINT e2ee_transfer_sessions_to_uid_check CHECK ((to_uid > 0))
);


--;

--

ALTER TABLE ONLY public.e2ee_transfer_sessions
    ADD CONSTRAINT e2ee_transfer_sessions_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.e2ee_transfer_sessions
    ADD CONSTRAINT e2ee_transfer_sessions_session_id_key UNIQUE (session_id);


--;

--

CREATE INDEX idx_e2ee_transfer_sessions_expires_at ON public.e2ee_transfer_sessions USING btree (expires_at);


--;

--

CREATE INDEX idx_e2ee_transfer_sessions_from_uid ON public.e2ee_transfer_sessions USING btree (from_uid);


--;

--

CREATE INDEX idx_e2ee_transfer_sessions_session_id ON public.e2ee_transfer_sessions USING btree (session_id);


--;

--

CREATE INDEX idx_e2ee_transfer_sessions_status ON public.e2ee_transfer_sessions USING btree (status);


--;

--

CREATE INDEX idx_e2ee_transfer_sessions_to_uid ON public.e2ee_transfer_sessions USING btree (to_uid);


--;

--

CREATE INDEX idx_e2ee_transfer_sessions_to_uid_status_expires ON public.e2ee_transfer_sessions USING btree (to_uid, status, expires_at);


--;

--

CREATE UNIQUE INDEX idx_e2ee_transfer_sessions_unique_active ON public.e2ee_transfer_sessions USING btree (from_uid, to_uid) WHERE ((status)::text = ANY ((ARRAY['pending'::character varying, 'accepted'::character varying])::text[]));


--;

--

COMMENT ON TABLE public.e2ee_transfer_sessions IS 'E2EE 设备间传输会话表 - 管理设备间私钥传输会话';


--;

--

COMMENT ON COLUMN public.e2ee_transfer_sessions.id IS '主键';


--;

--

COMMENT ON COLUMN public.e2ee_transfer_sessions.session_id IS '会话标识（UUIDv7 时序 UUID，顺序插入友好，16 字节存储）';


--;

--

COMMENT ON COLUMN public.e2ee_transfer_sessions.from_uid IS '发送方用户 ID';


--;

--

COMMENT ON COLUMN public.e2ee_transfer_sessions.from_device_id IS '发送方设备 ID';


--;

--

COMMENT ON COLUMN public.e2ee_transfer_sessions.to_uid IS '接收方用户 ID';


--;

--

COMMENT ON COLUMN public.e2ee_transfer_sessions.to_device_id IS '接收方设备 ID';


--;

--

COMMENT ON COLUMN public.e2ee_transfer_sessions.status IS '会话状态：pending/accepted/confirmed/expired/cancelled';


--;

--

COMMENT ON COLUMN public.e2ee_transfer_sessions.encrypted_key_bundle IS '加密的密钥包（RSA-OAEP-256 加密）';


--;

--

COMMENT ON COLUMN public.e2ee_transfer_sessions.expires_at IS '过期时间（5 分钟）';


--;

--

COMMENT ON COLUMN public.e2ee_transfer_sessions.created_at IS '创建时间';


--;

--

COMMENT ON COLUMN public.e2ee_transfer_sessions.confirmed_at IS '确认时间';


--;

--

COMMENT ON COLUMN public.e2ee_transfer_sessions.updated_at IS '更新时间';


--;

--

CREATE TABLE public.e2ee_trusted_contacts (
    id bigint NOT NULL,
    uid bigint NOT NULL,
    contact_uid bigint NOT NULL,
    contact_nickname character varying(100),
    status character varying(20) DEFAULT 'active'::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone DEFAULT now(),
    CONSTRAINT e2ee_trusted_contacts_contact_uid_check CHECK ((contact_uid > 0)),
    CONSTRAINT e2ee_trusted_contacts_status_check CHECK (((status)::text = ANY ((ARRAY['active'::character varying, 'removed'::character varying])::text[]))),
    CONSTRAINT e2ee_trusted_contacts_uid_check CHECK ((uid > 0))
);


--;

--

ALTER TABLE ONLY public.e2ee_trusted_contacts
    ADD CONSTRAINT e2ee_trusted_contacts_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.e2ee_trusted_contacts
    ADD CONSTRAINT e2ee_trusted_contacts_uid_contact_uid_key UNIQUE (uid, contact_uid);


--;

--

CREATE INDEX idx_e2ee_trusted_contacts_contact_uid ON public.e2ee_trusted_contacts USING btree (contact_uid);


--;

--

CREATE INDEX idx_e2ee_trusted_contacts_uid ON public.e2ee_trusted_contacts USING btree (uid);


--;

--

CREATE INDEX idx_e2ee_trusted_contacts_uid_active ON public.e2ee_trusted_contacts USING btree (uid) WHERE ((status)::text = 'active'::text);


--;

--

COMMENT ON TABLE public.e2ee_trusted_contacts IS 'E2EE 可信联系人表（社交恢复）';


--;

--

COMMENT ON COLUMN public.e2ee_trusted_contacts.uid IS '用户 ID';


--;

--

COMMENT ON COLUMN public.e2ee_trusted_contacts.contact_uid IS '可信联系人 ID';


--;

--

COMMENT ON COLUMN public.e2ee_trusted_contacts.contact_nickname IS '联系人昵称，方便用户识别';


--;

--

COMMENT ON COLUMN public.e2ee_trusted_contacts.status IS '状态：active（活跃）或 removed（已移除）';


--;

--

COMMENT ON COLUMN public.e2ee_trusted_contacts.created_at IS '创建时间';


--;

--

COMMENT ON COLUMN public.e2ee_trusted_contacts.updated_at IS '更新时间';


--;

--

CREATE TABLE public.compliance_key (
    id bigint NOT NULL,
    key_id character varying(64) NOT NULL,
    public_key text NOT NULL,
    private_key_encrypted text,
    algorithm character varying(64) DEFAULT 'RSA-OAEP-256'::character varying,
    status smallint DEFAULT 1 NOT NULL,
    created_by bigint,
    revoked_at timestamp with time zone,
    revoked_by bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.compliance_key
    ADD CONSTRAINT compliance_key_key_id_key UNIQUE (key_id);


--;

--

ALTER TABLE ONLY public.compliance_key
    ADD CONSTRAINT compliance_key_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_compliance_key_created_by ON public.compliance_key USING btree (created_by) WHERE (created_by IS NOT NULL);


--;

--

CREATE INDEX idx_compliance_key_revoked_by ON public.compliance_key USING btree (revoked_by) WHERE (revoked_by IS NOT NULL);


--;

--

CREATE INDEX idx_compliance_key_status ON public.compliance_key USING btree (status) WHERE (status = 1);


--;

--

COMMENT ON TABLE public.compliance_key IS '合规密钥管理表 - 三层加密架构';


--;

--

COMMENT ON COLUMN public.compliance_key.status IS '0:已撤销 1:活跃 2:已轮换';


--;

--

CREATE TABLE public.feedback (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    device_id character varying(40) NOT NULL,
    client_operating_system character varying(80),
    client_operating_system_vsn character varying(680),
    feedback_md5 character varying(40),
    app_vsn character varying(40),
    type character varying(40),
    rating character varying(40),
    contact_detail character varying(200),
    body text NOT NULL,
    attach jsonb,
    reply_count integer DEFAULT 0 NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_feedback_rating CHECK (((rating IS NULL) OR ((rating)::text = ANY ((ARRAY['bad'::character varying, 'neutral'::character varying, 'good'::character varying])::text[])))),
    CONSTRAINT chk_feedback_status CHECK ((status = ANY (ARRAY['-1'::integer, 0, 1, 2, 3]))),
    CONSTRAINT chk_feedback_type CHECK (((type IS NULL) OR ((type)::text = ANY ((ARRAY['bugReport'::character varying, 'featureRequest'::character varying])::text[]))))
);


--;

--

ALTER TABLE ONLY public.feedback
    ADD CONSTRAINT feedback_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.feedback
    ADD CONSTRAINT uk_feedbackmd5 UNIQUE (feedback_md5);


--;

--

CREATE INDEX idx_feedback_uid ON public.feedback USING btree (user_id);


--;

--

COMMENT ON TABLE public.feedback IS '用户反馈表';


--;

--

COMMENT ON COLUMN public.feedback.id IS '主键 自增长ID 反馈ID';


--;

--

COMMENT ON COLUMN public.feedback.user_id IS '反馈用户ID';


--;

--

COMMENT ON COLUMN public.feedback.device_id IS '设备ID web设备留空';


--;

--

COMMENT ON COLUMN public.feedback.client_operating_system IS '设备类型 web ios android macos windows';


--;

--

COMMENT ON COLUMN public.feedback.client_operating_system_vsn IS '设备版本 {"baseOS":"HUAWEI/CLT-AL00/HWINE:8.1.0/HUAWEICLT-AL00/173(C00):user/release-keys","sdkInt":27,"release":"8.1.0","codename":"REL","incremental":"176(C00)","previewSdkInt":0,"securityPatch":"2018-10-01"}';


--;

--

COMMENT ON COLUMN public.feedback.feedback_md5 IS '反馈唯一标识 md5(user_id + device_id + app_vsn + type + body)';


--;

--

COMMENT ON COLUMN public.feedback.type IS '反馈类型 bugReport featureRequest';


--;

--

COMMENT ON COLUMN public.feedback.rating IS '反馈评级 bad neutral good';


--;

--

COMMENT ON COLUMN public.feedback.contact_detail IS '联系方式';


--;

--

COMMENT ON COLUMN public.feedback.body IS '反馈内容';


--;

--

COMMENT ON COLUMN public.feedback.attach IS '反馈内容附件';


--;

--

COMMENT ON COLUMN public.feedback.reply_count IS '回复数量，回复的回复也计算在内';


--;

--

COMMENT ON COLUMN public.feedback.status IS '状态: -1 删除  0 禁用  1 启用 (待回复）  2 已回复  3 已完结（不允许回复了）';


--;

--

COMMENT ON COLUMN public.feedback.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public.feedback.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

CREATE TABLE public.feedback_reply (
    id bigint NOT NULL,
    feedback_id bigint NOT NULL,
    feedback_reply_pid bigint DEFAULT 0,
    replier_user_id bigint NOT NULL,
    replier_name character varying(40),
    body text NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.feedback_reply
    ADD CONSTRAINT feedback_reply_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_feedback_reply_feedback_id ON public.feedback_reply USING btree (feedback_id, created_at DESC);


--;

--

CREATE INDEX idx_feedback_reply_uid ON public.feedback_reply USING btree (replier_user_id);


--;

--

COMMENT ON TABLE public.feedback_reply IS '用户反馈回复表';


--;

--

COMMENT ON COLUMN public.feedback_reply.id IS '主键 自增长ID 反馈回复ID';


--;

--

COMMENT ON COLUMN public.feedback_reply.feedback_id IS '反馈ID';


--;

--

COMMENT ON COLUMN public.feedback_reply.feedback_reply_pid IS '反馈回复父ID，大于0，是对回复的回复';


--;

--

COMMENT ON COLUMN public.feedback_reply.replier_user_id IS '回复人用户ID';


--;

--

COMMENT ON COLUMN public.feedback_reply.replier_name IS '回复人称呼';


--;

--

COMMENT ON COLUMN public.feedback_reply.body IS '回复内容';


--;

--

COMMENT ON COLUMN public.feedback_reply.status IS '状态: -1 删除  0 禁用  1 启用';


--;

--

COMMENT ON COLUMN public.feedback_reply.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public.feedback_reply.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

CREATE TABLE public.report_ticket (
    id bigint NOT NULL,
    target_type character varying(16) NOT NULL,
    target_id bigint NOT NULL,
    reporter_uid bigint NOT NULL,
    reason character varying(64) DEFAULT ''::character varying NOT NULL,
    description character varying(500) DEFAULT ''::character varying NOT NULL,
    status smallint DEFAULT 0 NOT NULL,
    handled_by bigint,
    handled_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_report_ticket_status CHECK ((status = ANY (ARRAY[0, 1, 2]))),
    CONSTRAINT chk_report_ticket_target_type CHECK (((target_type)::text = ANY ((ARRAY['moment'::character varying, 'group'::character varying, 'channel'::character varying, 'user'::character varying])::text[])))
);


--;

--

ALTER TABLE ONLY public.report_ticket
    ADD CONSTRAINT report_ticket_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.report_ticket
    ADD CONSTRAINT uk_report_ticket_target_reporter UNIQUE (target_type, target_id, reporter_uid);


--;

--

CREATE INDEX idx_report_ticket_handled_by ON public.report_ticket USING btree (handled_by) WHERE (handled_by IS NOT NULL);


--;

--

CREATE INDEX idx_report_ticket_reporter_created ON public.report_ticket USING btree (reporter_uid, created_at DESC);


--;

--

CREATE INDEX idx_report_ticket_status_created ON public.report_ticket USING btree (status, created_at DESC);


--;

--

CREATE INDEX idx_report_ticket_target_status ON public.report_ticket USING btree (target_type, target_id, status, created_at DESC);


--;

--

COMMENT ON TABLE public.report_ticket IS '举报中心工单表（统一 group/channel/user/moment）';


--;

--

COMMENT ON COLUMN public.report_ticket.status IS '0待处理 1已驳回 2已确认违规';


--;

--

CREATE TABLE public.report_action_log (
    id bigint NOT NULL,
    report_id bigint NOT NULL,
    operator_uid bigint NOT NULL,
    result smallint NOT NULL,
    note character varying(500) DEFAULT ''::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_report_action_result CHECK ((result = ANY (ARRAY[1, 2])))
);


--;

--

ALTER TABLE ONLY public.report_action_log
    ADD CONSTRAINT report_action_log_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_report_action_operator_created ON public.report_action_log USING btree (operator_uid, created_at DESC);


--;

--

CREATE INDEX idx_report_action_report_created ON public.report_action_log USING btree (report_id, created_at DESC);


--;

--

COMMENT ON TABLE public.report_action_log IS '举报处理操作日志';


--;

--

CREATE TABLE public.plugin_audit_log (
    id bigint NOT NULL,
    plugin_name character varying(64) NOT NULL,
    operation_id character varying(64),
    event character varying(64) NOT NULL,
    from_state character varying(32),
    to_state character varying(32),
    step character varying(64),
    operator character varying(64) DEFAULT 'system'::character varying,
    started_at bigint NOT NULL,
    ended_at bigint,
    duration_ms integer,
    result character varying(16),
    error_code character varying(64),
    error_detail text,
    metadata jsonb DEFAULT '{}'::jsonb,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_plugin_audit_log_result CHECK (((result IS NULL) OR ((result)::text = ANY ((ARRAY['ok'::character varying, 'failed'::character varying, 'cancelled'::character varying, 'timeout'::character varying])::text[]))))
);


--;

--

ALTER TABLE ONLY public.plugin_audit_log
    ADD CONSTRAINT plugin_audit_log_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX i_plugin_audit_log_event ON public.plugin_audit_log USING btree (event, started_at DESC);


--;

--

CREATE INDEX i_plugin_audit_log_operation ON public.plugin_audit_log USING btree (operation_id);


--;

--

CREATE INDEX i_plugin_audit_log_plugin_started ON public.plugin_audit_log USING btree (plugin_name, started_at DESC);


--;

--

COMMENT ON TABLE public.plugin_audit_log IS '插件操作审计日志';


--;

--

COMMENT ON COLUMN public.plugin_audit_log.id IS 'TSID 分布式 ID';


--;

--

COMMENT ON COLUMN public.plugin_audit_log.plugin_name IS '插件名称';


--;

--

COMMENT ON COLUMN public.plugin_audit_log.operation_id IS 'admin 触发的操作 id（NULL = 内部事件）';


--;

--

COMMENT ON COLUMN public.plugin_audit_log.event IS '事件类型：state_entered / step_done / rollback_started 等';


--;

--

COMMENT ON COLUMN public.plugin_audit_log.from_state IS '转换前状态';


--;

--

COMMENT ON COLUMN public.plugin_audit_log.to_state IS '转换后状态';


--;

--

COMMENT ON COLUMN public.plugin_audit_log.step IS '当前执行步骤';


--;

--

COMMENT ON COLUMN public.plugin_audit_log.operator IS '操作者：admin uid 或 system';


--;

--

COMMENT ON COLUMN public.plugin_audit_log.started_at IS '开始时间（毫秒时间戳，用于精确 duration 计算）';


--;

--

COMMENT ON COLUMN public.plugin_audit_log.ended_at IS '结束时间（毫秒时间戳，NULL = 仍在进行）';


--;

--

COMMENT ON COLUMN public.plugin_audit_log.duration_ms IS '持续毫秒数 = ended_at - started_at';


--;

--

COMMENT ON COLUMN public.plugin_audit_log.result IS '结果：ok / failed / cancelled / timeout';


--;

--

COMMENT ON COLUMN public.plugin_audit_log.error_code IS '失败时分类码';


--;

--

COMMENT ON COLUMN public.plugin_audit_log.error_detail IS '失败时详细信息（脱敏后）';


--;

--

COMMENT ON COLUMN public.plugin_audit_log.metadata IS 'JSONB 任意扩展字段';


--;

--

COMMENT ON COLUMN public.plugin_audit_log.created_at IS '记录创建时间';


--;

--

CREATE TABLE public.wallet (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    balance bigint DEFAULT 0 NOT NULL,
    frozen bigint DEFAULT 0 NOT NULL,
    version integer DEFAULT 0 NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_wallet_balance CHECK ((balance >= 0)),
    CONSTRAINT chk_wallet_frozen CHECK ((frozen >= 0)),
    CONSTRAINT chk_wallet_status CHECK ((status = ANY (ARRAY[0, 1, 2])))
);


--;

--

ALTER TABLE ONLY public.wallet
    ADD CONSTRAINT wallet_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.wallet
    ADD CONSTRAINT wallet_user_id_key UNIQUE (user_id);


--;

--

CREATE TABLE public.wallet_transaction (
    id bigint NOT NULL,
    wallet_id bigint NOT NULL,
    user_id bigint NOT NULL,
    amount bigint NOT NULL,
    balance_after bigint NOT NULL,
    tx_type smallint DEFAULT 1 NOT NULL,
    reference_no character varying(64) DEFAULT ''::character varying,
    remark character varying(200) DEFAULT ''::character varying,
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_wallet_tx_status CHECK ((status = ANY (ARRAY[0, 1, 2]))),
    CONSTRAINT chk_wallet_tx_type CHECK ((tx_type = ANY (ARRAY[1, 2, 3, 4])))
);


--;

--

ALTER TABLE ONLY public.wallet_transaction
    ADD CONSTRAINT wallet_transaction_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX i_wallet_tx_userid ON public.wallet_transaction USING btree (user_id);


--;

--

CREATE INDEX i_wallet_tx_walletid ON public.wallet_transaction USING btree (wallet_id);


--;

--

CREATE INDEX idx_wallet_tx_reference_no ON public.wallet_transaction USING btree (reference_no) WHERE ((reference_no)::text <> ''::text);


--;

