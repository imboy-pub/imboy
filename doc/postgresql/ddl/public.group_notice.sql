-- Table: public.group_notice

-- DROP TABLE IF EXISTS public."group_notice";

-- 导入数据后，需要更新自增长ID
-- select setval('"group_notice_id_seq"', (select max(id) from public."group_notice"));

CREATE TABLE IF NOT EXISTS public."group_notice"
(
    id BIGSERIAL PRIMARY KEY,
    group_id bigint NOT NULL,
    user_id bigint NOT NULL,
    edit_user_id bigint NOT NULL DEFAULT 0,
    body varchar(2000) DEFAULT '',
    status smallint NOT NULL DEFAULT 0,
    expired_at bigint DEFAULT 0,
    updated_at bigint DEFAULT 0,
    created_at bigint NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.group_notice OWNER to imboy_user;

COMMENT ON TABLE public.group_notice IS '群组公告记录表';

COMMENT ON COLUMN public.group_notice.id IS '主键 自增长ID';

COMMENT ON COLUMN public.group_notice.group_id IS '群组ID';

COMMENT ON COLUMN public.group_notice.user_id IS '创建用户ID';

COMMENT ON COLUMN public.group_notice.body IS '公告类容';
COMMENT ON COLUMN public.group_notice.status IS '状态 0 待发布  1 已发布 2 取消发布';
COMMENT ON COLUMN public.group_notice.updated_at IS '有效期截止时间';
COMMENT ON COLUMN public.group_notice.updated_at IS '更新截止时间';
COMMENT ON COLUMN public.group_notice.created_at IS '创建记录Unix时间戳毫秒单位';

-- index
CREATE INDEX i_Gid_Status_ExpiredAt ON public.group_notice (group_id, status, expired_at asc);

