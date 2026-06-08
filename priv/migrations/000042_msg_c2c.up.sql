-- ============================================================
-- 合并迁移 000042: msg_c2c
-- 由 70 个历史迁移基线压缩而成 (fresh-install 等价)。
-- 本文件由 erlang_migrate 整体包裹在单事务中执行。
-- ============================================================


--

CREATE TABLE public.msg_c2c (
    id bigint NOT NULL,
    from_id bigint NOT NULL,
    to_id bigint NOT NULL,
    msg_id character varying(40) NOT NULL,
    msg_type character varying(40) NOT NULL,
    e2ee jsonb,
    payload text NOT NULL,
    server_ts timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    pinned boolean DEFAULT false,
    reply_to_msg_id character varying(40),
    reply_to_from_id bigint,
    reply_snippet text,
    expire_at timestamp with time zone
);


--;

--

ALTER TABLE ONLY public.msg_c2c
    ADD CONSTRAINT msg_c2c_pkey PRIMARY KEY (id, created_at);


--;

-- TimescaleDB: 将 msg_c2c 转为 hypertable 并配置压缩/保留策略
SELECT public.create_hypertable('public.msg_c2c', 'created_at', chunk_time_interval => INTERVAL '7 days', if_not_exists => TRUE, migrate_data => TRUE, create_default_indexes => FALSE);
ALTER TABLE public.msg_c2c SET (timescaledb.compress, timescaledb.compress_segmentby = 'to_id', timescaledb.compress_orderby = 'created_at DESC');
SELECT public.add_compression_policy('public.msg_c2c', INTERVAL '3 days', if_not_exists => TRUE);
SELECT public.add_retention_policy('public.msg_c2c', INTERVAL '1 year', if_not_exists => TRUE);


--

CREATE INDEX i_c2c_e2ee ON public.msg_c2c USING btree (((e2ee IS NOT NULL))) WHERE (e2ee IS NOT NULL);


--;

--

CREATE INDEX i_c2c_expire_at ON public.msg_c2c USING btree (expire_at) WHERE (expire_at IS NOT NULL);


--;

--

CREATE INDEX i_c2c_fromid ON public.msg_c2c USING btree (from_id);


--;

--

CREATE INDEX i_c2c_msgtype ON public.msg_c2c USING btree (msg_type);


--;

--

CREATE INDEX i_c2c_toid ON public.msg_c2c USING btree (to_id);


--;

--

CREATE INDEX idx_msg_c2c_payload_fts ON public.msg_c2c USING gin (to_tsvector('public.jiebacfg'::regconfig, payload));


--;

--

CREATE INDEX idx_msg_c2c_pinned ON public.msg_c2c USING btree (to_id, pinned) WHERE (pinned = true);


--;

--

CREATE INDEX idx_msg_c2c_reply ON public.msg_c2c USING btree (reply_to_msg_id) WHERE (reply_to_msg_id IS NOT NULL);


--;

--

CREATE INDEX msg_c2c_created_at_idx ON public.msg_c2c USING btree (created_at DESC);


--;

--

CREATE UNIQUE INDEX uk_c2c_msgid_createdat ON public.msg_c2c USING btree (msg_id, created_at);


--;

--

COMMENT ON TABLE public.msg_c2c IS '单聊消息临时存储表';


--;

--

COMMENT ON COLUMN public.msg_c2c.id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public.msg_c2c.from_id IS '消息发送人user id';


--;

--

COMMENT ON COLUMN public.msg_c2c.to_id IS '消息接收人user_id';


--;

--

COMMENT ON COLUMN public.msg_c2c.msg_id IS '消息唯一标识';


--;

--

COMMENT ON COLUMN public.msg_c2c.msg_type IS '消息格式类型： text image audio video file 等';


--;

--

COMMENT ON COLUMN public.msg_c2c.payload IS '消息体明文json格式加密后的文本，数据结构参考文档';


--;

--

COMMENT ON COLUMN public.msg_c2c.server_ts IS '消息服务器接受毫秒时间戳';


--;

--

COMMENT ON COLUMN public.msg_c2c.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public.msg_c2c.reply_to_msg_id IS '被引用回复的消息ID';


--;

--

COMMENT ON COLUMN public.msg_c2c.reply_to_from_id IS '被引用消息的发送者ID';


--;

--

COMMENT ON COLUMN public.msg_c2c.reply_snippet IS '被引用消息的摘要（前50字符）';


--;

--

COMMENT ON COLUMN public.msg_c2c.expire_at IS '消息自毁时间，NULL=不自毁';


--;

