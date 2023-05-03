-- Table: public.msg_c2g

-- DROP TABLE IF EXISTS public."msg_c2g";

CREATE TABLE IF NOT EXISTS public."msg_c2g"
(
    id BIGSERIAL PRIMARY KEY,
    from_id bigint NOT NULL,
    to_groupid bigint NOT NULL,
    msg_id varchar(40) NOT NULL,
    payload json not null,
    server_ts bigint DEFAULT 0,
    created_at bigint NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.msg_c2g OWNER to imboy_user;

COMMENT ON TABLE public.msg_c2g IS '群聊消息临时存储表';

COMMENT ON COLUMN public.msg_c2g.id IS '主键 自增长ID';

COMMENT ON COLUMN public.msg_c2g.from_id IS '消息发送人user id';
COMMENT ON COLUMN public.msg_c2g.to_groupid IS '消息接收群 group_id';
COMMENT ON COLUMN public.msg_c2g.server_ts IS '消息服务器接受毫秒时间戳';

COMMENT ON COLUMN public.msg_c2g.msg_id IS '消息唯一标识';

COMMENT ON COLUMN public.msg_c2g.payload IS '消息体json格式，数据结构参考文档';
COMMENT ON COLUMN public.msg_c2g.created_at IS '创建记录Unix时间戳毫秒单位';

-- index
CREATE INDEX i_c2g_ToId ON public.msg_c2g (to_groupid);
CREATE UNIQUE INDEX uk_c2g_MsgId ON public.msg_c2g (msg_id);
