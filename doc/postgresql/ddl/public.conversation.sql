-- Table: public.conversation

-- 导入数据后，需要更新自增长ID
-- select setval('"conversation_id_seq"', (select max(id) from public."conversation"));

-- DROP TABLE IF EXISTS public."conversation";


CREATE TABLE IF NOT EXISTS public."conversation"
(
    id BIGSERIAL PRIMARY KEY,
    client_id bigint NOT NULL DEFAULT 0,
    user_id varchar(40) DEFAULT '',
    peer_id varchar(40) DEFAULT '',
    avatar varchar(320) DEFAULT '',
    title varchar(400) DEFAULT '',
    subtitle varchar(400) DEFAULT '',
    region varchar(80) DEFAULT '',
    sign varchar(320) DEFAULT '',
    unread_num INT DEFAULT 0,
    type varchar(40) DEFAULT '', -- C2C C2G C2S S2C
    msg_type varchar(40) DEFAULT '',
    is_show INT DEFAULT 0,
    last_time INT DEFAULT 0,
    last_msg_id varchar(40) DEFAULT '',
    last_msg_status INT DEFAULT 0,
    payload TEXT DEFAULT ''
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.conversation OWNER to imboy_user;

COMMENT ON TABLE public.conversation IS '客户端会话记录表';


COMMENT ON COLUMN public.conversation.type IS '会话类型 C2C C2G C2S S2C';

COMMENT ON COLUMN public.conversation.user_id IS '发起会话用户ID int';
-- index
CREATE UNIQUE INDEX uk_cvt_UserId_Type_PeerId ON public.conversation (user_id, type, peer_id);
