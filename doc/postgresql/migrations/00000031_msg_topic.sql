-- Table: public.topic

-- 导入数据后，需要更新自增长ID
-- select setval('"topic_id_seq"', (select max(id) from public."topic"));

-- DROP TABLE IF EXISTS public."msg_topic";

CREATE TABLE IF NOT EXISTS public."msg_topic"
(
    id BIGSERIAL PRIMARY KEY,
    topic_id bigint NOT NULL DEFAULT 0,
    user_id bigint DEFAULT 0,
    to_id varchar(40) DEFAULT '',
    type varchar(40) DEFAULT '', -- C2S C2G
    title varchar(400) DEFAULT '',
    updated_at bigint DEFAULT 0,
    created_at bigint NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.msg_topic OWNER to imboy_user;

COMMENT ON TABLE public.msg_topic IS '客户端会话记录表';


COMMENT ON COLUMN public.msg_topic.type IS '话题类型 C2S(聊天机器人) C2G';

COMMENT ON COLUMN public.msg_topic.user_id IS '发起话题用户ID int';

COMMENT ON COLUMN public.msg_topic.topic_id IS '话题ID，客户端话题自增长ID';
-- index
CREATE INDEX i_msg_topic_type_UserId_Title ON public.msg_topic (type, user_id, title);
