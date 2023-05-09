-- Table: public.tag

-- DROP TABLE IF EXISTS public."tag";

CREATE TABLE IF NOT EXISTS public."tag"
(
    id BIGSERIAL PRIMARY KEY,
    creator_user_id bigint NOT NULL,
    name varchar(80) DEFAULT '',
    referer_time int NOT NULL DEFAULT 0,
    status smallint NOT NULL DEFAULT 1,
    updated_at bigint DEFAULT 0,
    created_at bigint NOT NULL,
    CONSTRAINT uk_Name UNIQUE  (name)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.tag OWNER to imboy_user;

COMMENT ON TABLE public.tag IS 'tag记录表';

COMMENT ON COLUMN public.tag.id IS '主键 自增长ID';

COMMENT ON COLUMN public.tag.creator_user_id IS '创建人用户ID';
COMMENT ON COLUMN public.tag.name IS '标签名称';
COMMENT ON COLUMN public.tag.referer_time IS '被引用次数';

COMMENT ON COLUMN public.tag.updated_at IS '更新记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.tag.created_at IS '创建记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.tag.status IS '状态: -1 删除  0 禁用  1 启用';

-- index


CREATE INDEX i_tag_CreatorUserId_Status ON public.tag (creator_user_id,status);

