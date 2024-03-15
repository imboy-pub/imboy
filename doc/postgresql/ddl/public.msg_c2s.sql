-- Table: public.msg_c2s

-- 导入数据后，需要更新自增长ID
-- select setval('"msg_c2s_id_seq"', (select max(id) from public."msg_c2s"));

-- DROP TABLE IF EXISTS public."msg_c2s";


CREATE TABLE IF NOT EXISTS public."msg_c2s"
(
    status integer NOT NULL DEFAULT 0,
    topic_id bigint NOT NULL DEFAULT 0,
    from_id bigint NOT NULL DEFAULT 0,
    to_id varchar(40) NOT NULL,
    msg_id varchar(40) NOT NULL,
    payload text not null,
    created_at bigint NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.msg_c2s OWNER to imboy_user;

COMMENT ON TABLE public.msg_c2s IS '机器人聊消息等存储表';


COMMENT ON COLUMN public.msg_c2s.status IS '消息状态： 10 服务端收到 11 投递给三方  12 收到三方结果 20 已投递客户端';

COMMENT ON COLUMN public.msg_c2s.from_id IS '消息发送人user id';
COMMENT ON COLUMN public.msg_c2s.to_id IS '消息接收人user_id';

COMMENT ON COLUMN public.msg_c2s.msg_id IS '消息唯一标识';

COMMENT ON COLUMN public.msg_c2s.payload IS '消息体json格式，数据结构参考文档';
COMMENT ON COLUMN public.msg_c2s.created_at IS '创建记录Unix时间戳毫秒单位';

-- index
CREATE INDEX i_c2s_TopicId ON public.msg_c2s (topic_id);
CREATE INDEX i_c2s_ToId_CreateAt ON public.msg_c2s (to_id, created_at desc);
CREATE INDEX i_c2s_MsgId ON public.msg_c2s (msg_id);


-- https://docs.timescale.com/api/latest/hypertable/create_hypertable/
SELECT create_hypertable('msg_c2s', by_range('created_at', 86400000));

-- 首先需要设置允许超表进行数据压缩
ALTER TABLE public.msg_c2s SET (
  timescaledb.compress,
  timescaledb.compress_orderby = 'created_at DESC',
  timescaledb.compress_segmentby = 'msg_id'
);

-- 86400000 * 30 30天
-- SELECT remove_compression_policy('msg_c2s');
SELECT add_compression_policy('msg_c2s', BIGINT '2592000000');
