-- Table: public.msg_c2c

-- 导入数据后，需要更新自增长ID
-- select setval('"msg_c2c_id_seq"', (select max(id) from public."msg_c2c"));

-- DROP TABLE IF EXISTS public."msg_c2c";

-- 1. 创建普通表（使用复合主键）
CREATE TABLE IF NOT EXISTS public."msg_c2c" (
    id BIGSERIAL,
    from_id bigint NOT NULL,
    to_id bigint NOT NULL,
    msg_id varchar(40) NOT NULL,
    payload text NOT NULL,
    server_ts timestamptz DEFAULT CURRENT_TIMESTAMP,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (id, created_at)  -- 包含分区键的复合主键
);

-- 2. 创建其他索引
CREATE INDEX i_c2c_ToId ON public.msg_c2c (to_id);
CREATE UNIQUE INDEX uk_c2c_MsgId_CreatedAt ON public.msg_c2c (msg_id, created_at);
CREATE INDEX i_c2c_FromId ON public.msg_c2c (from_id);

-- 3. 转换为超表
SELECT create_hypertable(
    'msg_c2c',
    'created_at',
    chunk_time_interval => INTERVAL '7 days',
    if_not_exists => TRUE
);

-- 4. 设置压缩策略(根据实际数据特征调整)
ALTER TABLE msg_c2c SET (
    timescaledb.compress,
    timescaledb.compress_orderby = 'created_at DESC'
);

-- 5. 添加策略(可选)
SELECT add_compression_policy('msg_c2c', INTERVAL '3 days');
SELECT add_retention_policy('msg_c2c', INTERVAL '12 months');

ALTER TABLE IF EXISTS public.msg_c2c OWNER TO imboy_user;

-- 注释
COMMENT ON TABLE public.msg_c2c IS '单聊消息临时存储表';
COMMENT ON COLUMN public.msg_c2c.id IS '主键 自增长ID';

COMMENT ON COLUMN public.msg_c2c.from_id IS '消息发送人user id';
COMMENT ON COLUMN public.msg_c2c.to_id IS '消息接收人user_id';
COMMENT ON COLUMN public.msg_c2c.server_ts IS '消息服务器接受毫秒时间戳';

COMMENT ON COLUMN public.msg_c2c.msg_id IS '消息唯一标识';

COMMENT ON COLUMN public.msg_c2c.payload IS '消息体json格式，数据结构参考文档';
COMMENT ON COLUMN public.msg_c2c.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';
