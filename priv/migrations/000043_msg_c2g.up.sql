-- ============================================================
-- 合并迁移 000043: msg_c2g
-- 由 70 个历史迁移基线压缩而成 (fresh-install 等价)。
-- 本文件由 erlang_migrate 整体包裹在单事务中执行。
-- ============================================================


--

CREATE TABLE public.msg_c2g (
    id bigint NOT NULL,
    topic_id bigint DEFAULT 0 NOT NULL,
    from_id bigint NOT NULL,
    to_id bigint NOT NULL,
    msg_id character varying(40) NOT NULL,
    msg_type character varying(40) NOT NULL,
    e2ee jsonb,
    payload jsonb NOT NULL,
    server_ts timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    mentions jsonb DEFAULT '[]'::jsonb,
    pinned boolean DEFAULT false,
    reply_to_msg_id character varying(40),
    reply_to_from_id bigint,
    reply_snippet text,
    expire_at timestamp with time zone
);


--;

--

ALTER TABLE ONLY public.msg_c2g
    ADD CONSTRAINT msg_c2g_pkey PRIMARY KEY (id, created_at);


--;

-- TimescaleDB: 将 msg_c2g 转为 hypertable 并配置压缩/保留策略
SELECT public.create_hypertable('public.msg_c2g', 'created_at', chunk_time_interval => INTERVAL '7 days', if_not_exists => TRUE, migrate_data => TRUE, create_default_indexes => FALSE);
ALTER TABLE public.msg_c2g SET (timescaledb.compress, timescaledb.compress_segmentby = 'to_id', timescaledb.compress_orderby = 'created_at DESC');
SELECT public.add_compression_policy('public.msg_c2g', INTERVAL '3 days', if_not_exists => TRUE);
SELECT public.add_retention_policy('public.msg_c2g', INTERVAL '1 year', if_not_exists => TRUE);


--

CREATE INDEX i_c2g_expire_at ON public.msg_c2g USING btree (expire_at) WHERE (expire_at IS NOT NULL);


--;

--

CREATE INDEX i_c2g_fromid ON public.msg_c2g USING btree (from_id);


--;

--

CREATE INDEX i_c2g_msgtype ON public.msg_c2g USING btree (msg_type);


--;

--

CREATE INDEX i_c2g_toid ON public.msg_c2g USING btree (to_id);


--;

--

CREATE INDEX i_msg_c2g_mentions ON public.msg_c2g USING gin (mentions);


--;

--

CREATE INDEX idx_msg_c2g_payload_fts ON public.msg_c2g USING gin (to_tsvector('public.jiebacfg'::regconfig, (payload)::text));


--;

--

CREATE INDEX idx_msg_c2g_pinned ON public.msg_c2g USING btree (to_id, pinned) WHERE (pinned = true);


--;

--

CREATE INDEX idx_msg_c2g_reply ON public.msg_c2g USING btree (reply_to_msg_id) WHERE (reply_to_msg_id IS NOT NULL);


--;

--

CREATE INDEX msg_c2g_created_at_idx ON public.msg_c2g USING btree (created_at DESC);


--;

--

COMMENT ON TABLE public.msg_c2g IS '群聊消息临时存储表';


--;

--

COMMENT ON COLUMN public.msg_c2g.id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public.msg_c2g.from_id IS '消息发送人user id';


--;

--

COMMENT ON COLUMN public.msg_c2g.to_id IS '消息接收人user_id';


--;

--

COMMENT ON COLUMN public.msg_c2g.msg_id IS '消息唯一标识';


--;

--

COMMENT ON COLUMN public.msg_c2g.msg_type IS '消息格式类型： text image audio video file 等';


--;

--

COMMENT ON COLUMN public.msg_c2g.payload IS '消息体json格式，数据结构参考文档';


--;

--

COMMENT ON COLUMN public.msg_c2g.server_ts IS '消息服务器接受毫秒时间戳';


--;

--

COMMENT ON COLUMN public.msg_c2g.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public.msg_c2g.mentions IS '@提及信息，JSON数组格式，包含被@的用户ID列表，例如: [123, 456] 或 ["all"] 表示@所有人';


--;

--

COMMENT ON COLUMN public.msg_c2g.reply_to_msg_id IS '被引用回复的消息ID';


--;

--

COMMENT ON COLUMN public.msg_c2g.reply_to_from_id IS '被引用消息的发送者ID';


--;

--

COMMENT ON COLUMN public.msg_c2g.reply_snippet IS '被引用消息的摘要（前50字符）';


--;

--

COMMENT ON COLUMN public.msg_c2g.expire_at IS '消息自毁时间，NULL=不自毁';


--;

--

CREATE TRIGGER imboy_for_msg_c2g AFTER INSERT OR DELETE OR UPDATE OF msg_id ON public.msg_c2g FOR EACH ROW EXECUTE FUNCTION public.imboy_msg_c2g_fun();


--;

