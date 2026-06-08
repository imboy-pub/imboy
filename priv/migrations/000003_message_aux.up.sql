-- ============================================================
-- 合并迁移 000003: message_aux
-- 由 70 个历史迁移基线压缩而成 (fresh-install 等价)。
-- 本文件由 erlang_migrate 整体包裹在单事务中执行。
-- ============================================================


--

CREATE TABLE public.msg_c2g_timeline (
    msg_id character varying(40) NOT NULL,
    to_uid bigint NOT NULL,
    to_gid bigint NOT NULL,
    client_ack boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

-- TimescaleDB: 将 msg_c2g_timeline 转为 hypertable 并配置压缩/保留策略
SELECT public.create_hypertable('public.msg_c2g_timeline', 'created_at', chunk_time_interval => INTERVAL '7 days', if_not_exists => TRUE, migrate_data => TRUE, create_default_indexes => FALSE);
ALTER TABLE public.msg_c2g_timeline SET (timescaledb.compress, timescaledb.compress_segmentby = 'to_gid', timescaledb.compress_orderby = 'created_at DESC');
SELECT public.add_compression_policy('public.msg_c2g_timeline', INTERVAL '3 days', if_not_exists => TRUE);
SELECT public.add_retention_policy('public.msg_c2g_timeline', INTERVAL '30 days', if_not_exists => TRUE);


--

CREATE INDEX idx_c2g_timeline_to_uid_pending ON public.msg_c2g_timeline USING btree (to_uid) WHERE (client_ack = false);


--;

--

CREATE INDEX msg_c2g_timeline_created_at_idx ON public.msg_c2g_timeline USING btree (created_at DESC);


--;

--

CREATE UNIQUE INDEX uk_c2g_timeline_touid_msgid_createdat ON public.msg_c2g_timeline USING btree (to_uid, msg_id, created_at);


--;

--

COMMENT ON TABLE public.msg_c2g_timeline IS '群聊消息时间线';


--;

--

COMMENT ON COLUMN public.msg_c2g_timeline.msg_id IS '消息唯一标识';


--;

--

COMMENT ON COLUMN public.msg_c2g_timeline.to_uid IS '消息接收人user_id';


--;

--

COMMENT ON COLUMN public.msg_c2g_timeline.to_gid IS '消息接收群 group_id';


--;

--

COMMENT ON COLUMN public.msg_c2g_timeline.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

CREATE TABLE public.msg_topic (
    id bigint NOT NULL,
    topic_id bigint DEFAULT 0 NOT NULL,
    user_id bigint DEFAULT 0,
    to_id character varying(40) DEFAULT ''::character varying,
    type character varying(40) DEFAULT ''::character varying,
    title character varying(400) DEFAULT ''::character varying,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_msg_topic_type CHECK (((type)::text = ANY ((ARRAY[''::character varying, 'C2S'::character varying, 'C2G'::character varying])::text[])))
);


--;

--

ALTER TABLE ONLY public.msg_topic
    ADD CONSTRAINT msg_topic_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX i_msg_topic_type_userid_title ON public.msg_topic USING btree (type, user_id, title);


--;

--

COMMENT ON TABLE public.msg_topic IS '客户端会话记录表';


--;

--

COMMENT ON COLUMN public.msg_topic.topic_id IS '话题ID，客户端话题自增长ID';


--;

--

COMMENT ON COLUMN public.msg_topic.user_id IS '发起话题用户ID int';


--;

--

COMMENT ON COLUMN public.msg_topic.type IS '话题类型 C2S(聊天机器人) C2G';


--;

--

CREATE TABLE public.msg_read (
    msg_id character varying(64) NOT NULL,
    from_uid bigint NOT NULL,
    to_uid bigint NOT NULL,
    to_did character varying(64),
    read_at timestamp with time zone DEFAULT now() NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--;

-- TimescaleDB: 将 msg_read 转为 hypertable 并配置压缩/保留策略
SELECT public.create_hypertable('public.msg_read', 'created_at', chunk_time_interval => INTERVAL '30 days', if_not_exists => TRUE, migrate_data => TRUE, create_default_indexes => FALSE);
ALTER TABLE public.msg_read SET (timescaledb.compress, timescaledb.compress_segmentby = 'from_uid', timescaledb.compress_orderby = 'created_at DESC');
SELECT public.add_compression_policy('public.msg_read', INTERVAL '30 days', if_not_exists => TRUE);
SELECT public.add_retention_policy('public.msg_read', INTERVAL '180 days', if_not_exists => TRUE);


--

CREATE INDEX idx_msg_read_from_uid ON public.msg_read USING btree (from_uid);


--;

--

CREATE INDEX idx_msg_read_msg_id ON public.msg_read USING btree (msg_id);


--;

--

CREATE INDEX idx_msg_read_read_at ON public.msg_read USING btree (read_at);


--;

--

CREATE INDEX idx_msg_read_to_uid ON public.msg_read USING btree (to_uid);


--;

--

CREATE INDEX msg_read_created_at_idx ON public.msg_read USING btree (created_at DESC);


--;

--

CREATE UNIQUE INDEX uk_msg_read_msg_to_did_created ON public.msg_read USING btree (msg_id, to_uid, to_did, created_at);


--;

--

COMMENT ON TABLE public.msg_read IS '消息已读回执表';


--;

--

COMMENT ON COLUMN public.msg_read.msg_id IS '消息唯一ID';


--;

--

COMMENT ON COLUMN public.msg_read.from_uid IS '发送者用户ID (整数)';


--;

--

COMMENT ON COLUMN public.msg_read.to_uid IS '接收者用户ID (整数)';


--;

--

COMMENT ON COLUMN public.msg_read.to_did IS '接收者设备ID';


--;

--

COMMENT ON COLUMN public.msg_read.read_at IS '消息已读时间（服务器时间）';


--;

--

COMMENT ON COLUMN public.msg_read.created_at IS '记录创建时间';


--;

--

CREATE TABLE public.msg_forward (
    id bigint NOT NULL,
    original_msg_id character varying(40) NOT NULL,
    original_from_id bigint NOT NULL,
    original_to_id bigint NOT NULL,
    original_type character varying(10) NOT NULL,
    forward_msg_id character varying(40) NOT NULL,
    forward_from_id bigint NOT NULL,
    forward_to_id bigint NOT NULL,
    forward_type character varying(10) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_msg_forward_forward_type CHECK (((forward_type)::text = ANY ((ARRAY['c2c'::character varying, 'c2g'::character varying])::text[]))),
    CONSTRAINT chk_msg_forward_original_type CHECK (((original_type)::text = ANY ((ARRAY['c2c'::character varying, 'c2g'::character varying])::text[])))
);


--;

--

ALTER TABLE ONLY public.msg_forward
    ADD CONSTRAINT msg_forward_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_msg_forward_from ON public.msg_forward USING btree (forward_from_id);


--;

--

CREATE INDEX idx_msg_forward_original ON public.msg_forward USING btree (original_msg_id);


--;

--

CREATE INDEX idx_msg_forward_to ON public.msg_forward USING btree (forward_to_id);


--;

--

COMMENT ON TABLE public.msg_forward IS '消息转发记录表，记录消息转发的溯源关系';


--;

--

COMMENT ON COLUMN public.msg_forward.original_msg_id IS '原始消息ID';


--;

--

COMMENT ON COLUMN public.msg_forward.original_from_id IS '原始消息发送者ID';


--;

--

COMMENT ON COLUMN public.msg_forward.original_to_id IS '原始消息接收者ID（单聊为用户ID，群聊为群组ID）';


--;

--

COMMENT ON COLUMN public.msg_forward.original_type IS '原始消息类型：c2c（单聊）或c2g（群聊）';


--;

--

COMMENT ON COLUMN public.msg_forward.forward_msg_id IS '转发后的消息ID';


--;

--

COMMENT ON COLUMN public.msg_forward.forward_from_id IS '转发操作者ID';


--;

--

COMMENT ON COLUMN public.msg_forward.forward_to_id IS '转发目标ID（单聊为用户ID，群聊为群组ID）';


--;

--

COMMENT ON COLUMN public.msg_forward.forward_type IS '转发目标类型：c2c（单聊）或c2g（群聊）';


--;

--

CREATE TABLE public.msg_mention (
    id bigint NOT NULL,
    msg_id character varying(40) NOT NULL,
    group_id bigint NOT NULL,
    mentioned_uid bigint NOT NULL,
    from_uid bigint NOT NULL,
    is_read boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.msg_mention
    ADD CONSTRAINT msg_mention_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.msg_mention
    ADD CONSTRAINT uk_msg_mention UNIQUE (msg_id, mentioned_uid);


--;

--

CREATE INDEX i_msg_mention_group_uid ON public.msg_mention USING btree (group_id, mentioned_uid, created_at DESC);


--;

--

CREATE INDEX i_msg_mention_msg_id ON public.msg_mention USING btree (msg_id);


--;

--

CREATE INDEX i_msg_mention_uid_read ON public.msg_mention USING btree (mentioned_uid, is_read, created_at DESC);


--;

--

CREATE INDEX idx_msg_mention_from_uid ON public.msg_mention USING btree (from_uid, created_at DESC);


--;

--

COMMENT ON TABLE public.msg_mention IS '@消息记录表，用于快速查询@我的消息';


--;

--

COMMENT ON COLUMN public.msg_mention.msg_id IS '消息ID';


--;

--

COMMENT ON COLUMN public.msg_mention.group_id IS '群组ID';


--;

--

COMMENT ON COLUMN public.msg_mention.mentioned_uid IS '被@的用户ID';


--;

--

COMMENT ON COLUMN public.msg_mention.from_uid IS '@消息的发送者ID';


--;

--

COMMENT ON COLUMN public.msg_mention.is_read IS '是否已读';


--;

--

COMMENT ON COLUMN public.msg_mention.created_at IS '创建时间';


--;

--

CREATE TABLE public.msg_reaction (
    id bigint NOT NULL,
    msg_id character varying(40) NOT NULL,
    msg_type character varying(10) NOT NULL,
    user_id bigint NOT NULL,
    emoji character varying(100) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_msg_reaction_msg_type CHECK (((msg_type)::text = ANY ((ARRAY['c2c'::character varying, 'c2g'::character varying])::text[])))
);


--;

--

ALTER TABLE ONLY public.msg_reaction
    ADD CONSTRAINT msg_reaction_msg_id_msg_type_user_id_emoji_key UNIQUE (msg_id, msg_type, user_id, emoji);


--;

--

ALTER TABLE ONLY public.msg_reaction
    ADD CONSTRAINT msg_reaction_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_msg_reaction_emoji ON public.msg_reaction USING btree (msg_id, msg_type, emoji);


--;

--

CREATE INDEX idx_msg_reaction_user ON public.msg_reaction USING btree (user_id);


--;

--

COMMENT ON TABLE public.msg_reaction IS '消息表情回应表';


--;

--

COMMENT ON COLUMN public.msg_reaction.msg_type IS '消息类型: c2c-单聊, c2g-群聊';


--;

--

COMMENT ON COLUMN public.msg_reaction.emoji IS 'emoji字符，如👍❤️😄等';


--;

--

CREATE TABLE public.msg_store (
    id bigint NOT NULL,
    chat_type character varying(3) NOT NULL,
    conv_key character varying(64) NOT NULL,
    conv_seq bigint NOT NULL,
    msg_id character varying(50) NOT NULL,
    msg_type character varying(50) NOT NULL,
    from_id bigint NOT NULL,
    to_id bigint,
    group_id bigint,
    e2ee jsonb,
    payload text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    server_ts timestamp with time zone DEFAULT now() NOT NULL,
    expire_at timestamp with time zone,
    CONSTRAINT chk_msg_store_chat_type CHECK (((chat_type)::text = ANY ((ARRAY['c2c'::character varying, 'c2g'::character varying])::text[])))
);


--;

--

ALTER TABLE ONLY public.msg_store
    ADD CONSTRAINT msg_store_pkey PRIMARY KEY (id, created_at);


--;

-- TimescaleDB: 将 msg_store 转为 hypertable 并配置压缩/保留策略
SELECT public.create_hypertable('public.msg_store', 'created_at', chunk_time_interval => INTERVAL '30 days', if_not_exists => TRUE, migrate_data => TRUE, create_default_indexes => FALSE);
ALTER TABLE public.msg_store SET (timescaledb.compress, timescaledb.compress_segmentby = 'conv_key', timescaledb.compress_orderby = 'created_at DESC');
SELECT public.add_compression_policy('public.msg_store', INTERVAL '3 days', if_not_exists => TRUE);


--

CREATE INDEX i_msg_store_conv_seq ON public.msg_store USING btree (conv_key, conv_seq);


--;

--

CREATE INDEX i_msg_store_from_id ON public.msg_store USING btree (from_id, created_at DESC);


--;

--

CREATE INDEX i_msg_store_group_id ON public.msg_store USING btree (group_id, created_at DESC) WHERE (group_id IS NOT NULL);


--;

--

CREATE INDEX msg_store_created_at_idx ON public.msg_store USING btree (created_at DESC);


--;

--

CREATE UNIQUE INDEX uk_msg_store_msg_id_created_at ON public.msg_store USING btree (msg_id, created_at);


--;

--

COMMENT ON TABLE public.msg_store IS '消息永久存储表：Worker 写入，不因客户端 ACK 删除';


--;

--

COMMENT ON COLUMN public.msg_store.chat_type IS '聊天类型：c2c | c2g';


--;

--

COMMENT ON COLUMN public.msg_store.conv_key IS '会话键：c2c:{min_uid}:{max_uid} 或 c2g:{gid}';


--;

--

COMMENT ON COLUMN public.msg_store.conv_seq IS 'per-conversation 单调递增序列号，客户端增量同步游标';


--;

--

COMMENT ON COLUMN public.msg_store.msg_id IS '消息全局唯一 ID';


--;

--

COMMENT ON COLUMN public.msg_store.from_id IS '发送者 user_id';


--;

--

COMMENT ON COLUMN public.msg_store.to_id IS '接收者 user_id（C2C）';


--;

--

COMMENT ON COLUMN public.msg_store.group_id IS '群组 ID（C2G）';


--;

--

COMMENT ON COLUMN public.msg_store.e2ee IS 'E2EE 元数据（JSONB，加密消息时非 NULL）';


--;

--

COMMENT ON COLUMN public.msg_store.payload IS '消息内容（明文 JSON 或加密 base64 密文）';


--;

--

COMMENT ON COLUMN public.msg_store.created_at IS '消息创建时间（分区键）';


--;

--

COMMENT ON COLUMN public.msg_store.server_ts IS '服务端接收时间';


--;

--

COMMENT ON COLUMN public.msg_store.expire_at IS '消息原始自毁时间（归档保留）';


--;

--

CREATE TABLE public.msg_store_seq (
    conv_key character varying(64) NOT NULL,
    seq bigint DEFAULT 0 NOT NULL
);


--;

--

ALTER TABLE ONLY public.msg_store_seq
    ADD CONSTRAINT msg_store_seq_pkey PRIMARY KEY (conv_key);


--;

--

COMMENT ON TABLE public.msg_store_seq IS 'per-conversation 序列号计数器';


--;

--

COMMENT ON COLUMN public.msg_store_seq.conv_key IS '会话唯一键：c2c:{min_uid}:{max_uid} 或 c2g:{gid}';


--;

--

COMMENT ON COLUMN public.msg_store_seq.seq IS '当前最大序列号，每次写入原子 +1';


--;

