-- Table: public.feedback_reply

-- DROP TABLE IF EXISTS public."feedback_reply";

-- 导入数据后，需要更新自增长ID
-- select setval('"feedback_reply_id_seq"', (select max(id) from public."feedback_reply"));

CREATE TABLE IF NOT EXISTS public."feedback_reply"
(
    id BIGSERIAL PRIMARY KEY,
    feedback_id bigint NOT NULL,
    feedback_reply_pid bigint DEFAULT 0,
    replier_user_id bigint NOT NULL,
    replier_name varchar(40),
    body text not null,

    status smallint NOT NULL DEFAULT 1,
    updated_at timestamptz DEFAULT CURRENT_TIMESTAMP NULL,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.feedback_reply OWNER to imboy_user;

COMMENT ON TABLE public.feedback_reply IS '用户反馈回复表';

COMMENT ON COLUMN public.feedback_reply.id IS '主键 自增长ID 反馈回复ID';

COMMENT ON COLUMN public.feedback_reply.feedback_id IS '反馈ID';
COMMENT ON COLUMN public.feedback_reply.feedback_reply_pid IS '反馈回复父ID，大于0，是对回复的回复';
COMMENT ON COLUMN public.feedback_reply.replier_user_id IS '回复人用户ID';
COMMENT ON COLUMN public.feedback_reply.replier_name IS '回复人称呼';
COMMENT ON COLUMN public.feedback_reply.body IS '回复内容';

COMMENT ON COLUMN public.feedback_reply.status IS '状态: -1 删除  0 禁用  1 启用';
COMMENT ON COLUMN public.feedback_reply.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';
COMMENT ON COLUMN public.feedback_reply.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';
