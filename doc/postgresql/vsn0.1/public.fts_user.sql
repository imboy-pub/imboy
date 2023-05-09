-- Table: public.fts_user

-- DROP TABLE IF EXISTS public."fts_user";

CREATE TABLE IF NOT EXISTS public."fts_user"
(
    user_id bigint NOT NULL,
    allow_search smallint NOT NULL DEFAULT 2,
    token tsvector,
    CONSTRAINT pk_fts_user_uid PRIMARY KEY (user_id)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.fts_user OWNER to imboy_user;

COMMENT ON TABLE public.fts_user IS '用户全文索引矢量信息表';

COMMENT ON COLUMN public.fts_user.user_id IS '用户唯一ID';

COMMENT ON COLUMN public.fts_user.allow_search IS '用户允许被搜索 1 是  2 否';
COMMENT ON COLUMN public.fts_user.token IS '搜索矢量信息';
