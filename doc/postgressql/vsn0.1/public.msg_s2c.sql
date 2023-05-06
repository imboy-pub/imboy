-- Table: public.msg_s2c

-- DROP TABLE IF EXISTS public."msg_s2c";

-- 导入数据后，需要更新自增长ID
-- select setval('"msg_s2c_id_seq"', (select max(id) from public."msg_s2c"));
CREATE TABLE IF NOT EXISTS public."msg_s2c"
(
    id BIGSERIAL PRIMARY KEY,
    from_id bigint NOT NULL DEFAULT 0,
    to_id bigint NOT NULL,
    msg_id varchar(40) NOT NULL,
    payload json not null,
    server_ts bigint DEFAULT 0,
    created_at bigint NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.msg_s2c OWNER to imboy_user;

COMMENT ON TABLE public.msg_s2c IS '系统消息临时存储表';

COMMENT ON COLUMN public.msg_s2c.id IS '主键 自增长ID';

COMMENT ON COLUMN public.msg_s2c.from_id IS '消息发送人user id';
COMMENT ON COLUMN public.msg_s2c.to_id IS '消息接收人user_id';
COMMENT ON COLUMN public.msg_s2c.server_ts IS '消息服务器接受毫秒时间戳';

COMMENT ON COLUMN public.msg_s2c.msg_id IS '消息唯一标识';

COMMENT ON COLUMN public.msg_s2c.payload IS '消息体json格式，数据结构参考文档';
COMMENT ON COLUMN public.msg_s2c.created_at IS '创建记录Unix时间戳毫秒单位';

-- index
CREATE INDEX i_s2c_ToId ON public.msg_s2c (to_id);
