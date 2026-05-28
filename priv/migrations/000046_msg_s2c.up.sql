-- Table: public.msg_s2c

-- 导入数据后，需要更新自增长ID
-- select setval('"msg_s2c_id_seq"', (select max(id) from public."msg_s2c"));

-- DROP TABLE IF EXISTS public."msg_s2c";

-- 创建表结构
CREATE TABLE IF NOT EXISTS public.msg_s2c
(
    id BIGSERIAL,
    from_id bigint NOT NULL,
    to_id bigint NOT NULL,
    msg_id varchar(40) NOT NULL,
    action varchar(40) NOT NULL,
    msg_type varchar(40) NOT NULL DEFAULT '',
    e2ee JSONB,
    payload text NOT NULL,
    server_ts timestamptz DEFAULT CURRENT_TIMESTAMP,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (id, created_at)
)
TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.msg_s2c OWNER TO imboy_user;

-- 2. 创建其他索引
CREATE INDEX IF NOT EXISTS i_s2c_action ON msg_s2c(action); -- 用于快速查找特定系统消息
CREATE INDEX IF NOT EXISTS i_s2c_ToId ON public.msg_s2c (to_id);
CREATE INDEX IF NOT EXISTS i_s2c_FromId ON public.msg_s2c (from_id);
CREATE INDEX IF NOT EXISTS i_s2c_MsgType ON public.msg_s2c (msg_type);
CREATE INDEX IF NOT EXISTS i_s2c_e2ee ON msg_s2c((e2ee IS NOT NULL)) WHERE e2ee IS NOT NULL;

-- 注释
COMMENT ON TABLE public.msg_s2c IS '系统消息临时存储表';
COMMENT ON COLUMN public.msg_s2c.id IS '主键 自增长ID';

COMMENT ON COLUMN public.msg_s2c.from_id IS '消息发送人user id';
COMMENT ON COLUMN public.msg_s2c.to_id IS '消息接收人user_id';
COMMENT ON COLUMN public.msg_s2c.server_ts IS '消息服务器接受毫秒时间戳';

COMMENT ON COLUMN public.msg_s2c.msg_id IS '消息唯一标识';
COMMENT ON COLUMN public.msg_s2c.msg_type IS '消息格式类型： text image audio video file 等';
COMMENT ON COLUMN public.msg_s2c.action IS 'S2C消息指令';

COMMENT ON COLUMN public.msg_s2c.payload IS '消息体json格式，数据结构参考文档';
COMMENT ON COLUMN public.msg_s2c.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';

-- https://docs.timescale.com/api/latest/hypertable/create_hypertable/
-- 创建超表（先不启用压缩）
SELECT create_hypertable(
    'msg_s2c',
    'created_at',
    chunk_time_interval => INTERVAL '7 days',
    if_not_exists => TRUE
);

-- 启用压缩配置
ALTER TABLE msg_s2c SET (
    timescaledb.compress,
    timescaledb.compress_orderby = 'created_at DESC',
    timescaledb.compress_segmentby = 'to_id'
);

-- 添加压缩策略（延迟到索引创建后）
SELECT add_compression_policy('msg_s2c', INTERVAL '3 days');

-- 添加保留策略
SELECT add_retention_policy('msg_s2c', INTERVAL '12 months');
