-- ============================================================
-- 合并迁移 000045: msg_c2s
-- 由 70 个历史迁移基线压缩而成 (fresh-install 等价)。
-- 本文件由 erlang_migrate 整体包裹在单事务中执行。
-- ============================================================


--

CREATE TABLE public.msg_c2s (
    id bigint NOT NULL,
    topic_id bigint DEFAULT 0 NOT NULL,
    from_id bigint NOT NULL,
    to_id bigint NOT NULL,
    msg_id character varying(40) NOT NULL,
    msg_type character varying(40) NOT NULL,
    e2ee jsonb,
    payload text NOT NULL,
    server_ts timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.msg_c2s
    ADD CONSTRAINT msg_c2s_pkey PRIMARY KEY (id, created_at);


--;

-- TimescaleDB: 将 msg_c2s 转为 hypertable 并配置压缩/保留策略
SELECT public.create_hypertable('public.msg_c2s', 'created_at', chunk_time_interval => INTERVAL '7 days', if_not_exists => TRUE, migrate_data => TRUE, create_default_indexes => FALSE);
ALTER TABLE public.msg_c2s SET (timescaledb.compress, timescaledb.compress_segmentby = 'to_id', timescaledb.compress_orderby = 'created_at DESC');
SELECT public.add_compression_policy('public.msg_c2s', INTERVAL '3 days', if_not_exists => TRUE);
SELECT public.add_retention_policy('public.msg_c2s', INTERVAL '1 year', if_not_exists => TRUE);


--

CREATE INDEX i_c2s_e2ee ON public.msg_c2s USING btree (((e2ee IS NOT NULL))) WHERE (e2ee IS NOT NULL);


--;

--

CREATE INDEX i_c2s_fromid ON public.msg_c2s USING btree (from_id);


--;

--

CREATE INDEX i_c2s_msgtype ON public.msg_c2s USING btree (msg_type);


--;

--

CREATE INDEX i_c2s_toid ON public.msg_c2s USING btree (to_id);


--;

--

CREATE INDEX msg_c2s_created_at_idx ON public.msg_c2s USING btree (created_at DESC);


--;

--

COMMENT ON TABLE public.msg_c2s IS '机器人聊消息等存储表';


--;

--

COMMENT ON COLUMN public.msg_c2s.id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public.msg_c2s.from_id IS '消息发送人user id';


--;

--

COMMENT ON COLUMN public.msg_c2s.to_id IS '消息接收人user_id';


--;

--

COMMENT ON COLUMN public.msg_c2s.msg_id IS '消息唯一标识';


--;

--

COMMENT ON COLUMN public.msg_c2s.msg_type IS '消息格式类型： text image audio video file 等';


--;

--

COMMENT ON COLUMN public.msg_c2s.payload IS '消息体json格式，数据结构参考文档';


--;

--

COMMENT ON COLUMN public.msg_c2s.server_ts IS '消息服务器接受毫秒时间戳';


--;

--

COMMENT ON COLUMN public.msg_c2s.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

