-- Table: public.msg_c2c

-- DROP TABLE IF EXISTS public."msg_c2c";


-- 导入数据后，需要更新自增长ID
-- select setval('"msg_c2c_id_seq"', (select max(id) from public."msg_c2c"));
CREATE TABLE IF NOT EXISTS public."msg_c2c"
(
    id BIGSERIAL PRIMARY KEY,
    from_id bigint NOT NULL,
    to_id bigint NOT NULL,
    msg_id varchar(40) NOT NULL,
    payload json not null,
    server_ts bigint DEFAULT 0,
    created_at bigint NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.msg_c2c OWNER to imboy_user;

COMMENT ON TABLE public.msg_c2c IS '单聊消息临时存储表';

COMMENT ON COLUMN public.msg_c2c.id IS '主键 自增长ID';

COMMENT ON COLUMN public.msg_c2c.from_id IS '消息发送人user id';
COMMENT ON COLUMN public.msg_c2c.to_id IS '消息接收人user_id';
COMMENT ON COLUMN public.msg_c2c.server_ts IS '消息服务器接受毫秒时间戳';

COMMENT ON COLUMN public.msg_c2c.msg_id IS '消息唯一标识';

COMMENT ON COLUMN public.msg_c2c.payload IS '消息体json格式，数据结构参考文档';
COMMENT ON COLUMN public.msg_c2c.created_at IS '创建记录Unix时间戳毫秒单位';

-- index
CREATE INDEX i_c2c_ToId ON public.msg_c2c (to_id);
CREATE UNIQUE INDEX uk_c2c_MsgId ON public.msg_c2c (msg_id);
