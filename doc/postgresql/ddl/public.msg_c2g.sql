-- Table: public.msg_c2g

-- 导入数据后，需要更新自增长ID
-- select setval('"msg_c2g_id_seq"', (select max(id) from public."msg_c2g"));

-- DROP TABLE IF EXISTS public."msg_c2g";

CREATE TABLE IF NOT EXISTS public."msg_c2g"
(
    -- id BIGSERIAL PRIMARY KEY,
    ts timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    from_id bigint NOT NULL,
    to_groupid bigint NOT NULL,
    msg_id varchar(40) NOT NULL,
    payload text not null,
    created_at bigint NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.msg_c2g OWNER to imboy_user;

COMMENT ON TABLE public.msg_c2g IS '群聊消息临时存储表';

-- COMMENT ON COLUMN public.msg_c2g.id IS '主键 自增长ID';

COMMENT ON COLUMN public.msg_c2g.from_id IS '消息发送人user id';
COMMENT ON COLUMN public.msg_c2g.to_groupid IS '消息接收群 group_id';

COMMENT ON COLUMN public.msg_c2g.msg_id IS '消息唯一标识';

COMMENT ON COLUMN public.msg_c2g.payload IS '消息体json格式，数据结构参考文档';
COMMENT ON COLUMN public.msg_c2g.created_at IS '创建记录Unix时间戳毫秒单位';

-- index
CREATE INDEX i_c2g_ToId_Ts ON public.msg_c2g (to_groupid, ts desc);
CREATE INDEX i_c2g_MsgId ON public.msg_c2g (msg_id);
-- CREATE UNIQUE INDEX uk_c2g_MsgId ON public.msg_c2g (msg_id);

SELECT create_hypertable('msg_c2g', 'ts', chunk_time_interval => INTERVAL '30 day');

-- 首先需要设置允许超表进行数据压缩
ALTER TABLE public.msg_c2g SET (
  timescaledb.compress,
  timescaledb.compress_orderby = 'ts DESC',
  timescaledb.compress_segmentby = 'msg_id'
);

-- https://docs.timescale.com/api/latest/compression/
-- 自动对两周之前的数据进行压缩，并创建循环规则
-- 在 TimescaleDB  2.3 及更高版本中，您可以将数据插入压缩块并在分布式超表上启用压缩策略。
-- 在TimescaleDB  2.11及更高版本中，您可以更新和删除压缩数据。您还可以使用高级插入语句，例如ON CONFLICT和RETURNING。
--
SELECT add_compression_policy('msg_c2g', INTERVAL '60 day');

-- https://docs.timescale.com/api/latest/data-retention/add_retention_policy/
-- 创建数据保留策略以丢弃 12 个月前创建的块：
SELECT add_retention_policy('msg_c2g', drop_after => INTERVAL '12 months');
-- SELECT add_retention_policy('msg_c2g', drop_after => INTERVAL '3 minutes');
