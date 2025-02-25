-- Table: public.msg_c2g

-- 导入数据后，需要更新自增长ID
-- select setval('"msg_c2g_id_seq"', (select max(id) from public."msg_c2g"));

-- DROP TABLE IF EXISTS public."msg_c2g";

-- 创建表结构
CREATE TABLE IF NOT EXISTS public."msg_c2g"
(
    id BIGSERIAL,
    topic_id bigint NOT NULL DEFAULT 0,
    from_id bigint NOT NULL,
    to_id bigint NOT NULL,
    msg_id varchar(40) NOT NULL,
    payload text NOT NULL,
    server_ts timestamptz DEFAULT CURRENT_TIMESTAMP,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (id, created_at)
)
TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.msg_c2g OWNER TO imboy_user;

-- 注释（保持原有）
COMMENT ON TABLE public.msg_c2g IS '群聊消息临时存储表';
COMMENT ON COLUMN public.msg_c2g.id IS '主键 自增长ID';
COMMENT ON COLUMN public.msg_c2g.from_id IS '消息发送人user id';
COMMENT ON COLUMN public.msg_c2g.to_id IS '消息接收人user_id';
COMMENT ON COLUMN public.msg_c2g.server_ts IS '消息服务器接受毫秒时间戳';

COMMENT ON COLUMN public.msg_c2g.msg_id IS '消息唯一标识';

COMMENT ON COLUMN public.msg_c2g.payload IS '消息体json格式，数据结构参考文档';
COMMENT ON COLUMN public.msg_c2g.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';

-- https://docs.timescale.com/api/latest/hypertable/create_hypertable/
-- 创建超表（先不启用压缩）
SELECT create_hypertable(
    'msg_c2g',
    'created_at',
    chunk_time_interval => INTERVAL '7 days',
    if_not_exists => TRUE
);

-- index
-- 创建所有必要索引（关键顺序！）
CREATE INDEX i_c2g_TopicId ON public.msg_c2g (topic_id);
CREATE INDEX i_c2g_ToId ON public.msg_c2g (to_id);
CREATE UNIQUE INDEX uk_c2g_MsgId_CreatedAt ON public.msg_c2g (msg_id, created_at);
CREATE INDEX i_c2g_FromId ON public.msg_c2g (from_id);

-- 启用压缩配置
ALTER TABLE msg_c2g SET (
    timescaledb.compress,
    timescaledb.compress_orderby = 'created_at DESC',
    timescaledb.compress_segmentby = 'to_id'
);

-- 添加压缩策略（延迟到索引创建后）
SELECT add_compression_policy('msg_c2g', INTERVAL '3 days');

-- 添加保留策略
SELECT add_retention_policy('msg_c2g', INTERVAL '12 months');
