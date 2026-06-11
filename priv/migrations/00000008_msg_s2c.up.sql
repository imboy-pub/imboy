-- ============================================================
-- 合并迁移 000046: msg_s2c
-- 由 70 个历史迁移基线压缩而成 (fresh-install 等价)。
-- 本文件由 erlang_migrate 整体包裹在单事务中执行。
-- ============================================================


--

CREATE TABLE public.msg_s2c (
    id bigint NOT NULL,
    from_id bigint NOT NULL,
    to_id bigint NOT NULL,
    msg_id character varying(40) NOT NULL,
    action character varying(40) NOT NULL,
    msg_type character varying(40) DEFAULT ''::character varying NOT NULL,
    e2ee jsonb,
    payload jsonb NOT NULL,
    server_ts timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.msg_s2c
    ADD CONSTRAINT msg_s2c_pkey PRIMARY KEY (id, created_at);


--;

-- TimescaleDB: 将 msg_s2c 转为 hypertable 并配置压缩/保留策略
SELECT public.create_hypertable('public.msg_s2c', 'created_at', chunk_time_interval => INTERVAL '7 days', if_not_exists => TRUE, migrate_data => TRUE, create_default_indexes => FALSE);
ALTER TABLE public.msg_s2c SET (timescaledb.compress, timescaledb.compress_segmentby = 'to_id', timescaledb.compress_orderby = 'created_at DESC');
SELECT public.add_compression_policy('public.msg_s2c', INTERVAL '3 days', if_not_exists => TRUE);
SELECT public.add_retention_policy('public.msg_s2c', INTERVAL '1 year', if_not_exists => TRUE);


--

CREATE INDEX i_s2c_action ON public.msg_s2c USING btree (action);


--;

--

CREATE INDEX i_s2c_fromid ON public.msg_s2c USING btree (from_id);


--;

--

CREATE INDEX i_s2c_msgtype ON public.msg_s2c USING btree (msg_type);


--;

--

CREATE INDEX i_s2c_toid ON public.msg_s2c USING btree (to_id);


--;

--

CREATE INDEX msg_s2c_created_at_idx ON public.msg_s2c USING btree (created_at DESC);


--;

--

COMMENT ON TABLE public.msg_s2c IS '系统消息临时存储表';


--;

--

COMMENT ON COLUMN public.msg_s2c.id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public.msg_s2c.from_id IS '消息发送人user id';


--;

--

COMMENT ON COLUMN public.msg_s2c.to_id IS '消息接收人user_id';


--;

--

COMMENT ON COLUMN public.msg_s2c.msg_id IS '消息唯一标识';


--;

--

COMMENT ON COLUMN public.msg_s2c.action IS 'S2C消息指令';


--;

--

COMMENT ON COLUMN public.msg_s2c.msg_type IS '消息格式类型： text image audio video file 等';


--;

--

COMMENT ON COLUMN public.msg_s2c.payload IS '消息体json格式，数据结构参考文档';


--;

--

COMMENT ON COLUMN public.msg_s2c.server_ts IS '消息服务器接受毫秒时间戳';


--;

--

COMMENT ON COLUMN public.msg_s2c.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

