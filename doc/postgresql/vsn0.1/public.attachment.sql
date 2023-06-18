-- Table: public.attachment

-- DROP TABLE IF EXISTS public."attachment";

CREATE TABLE IF NOT EXISTS public."attachment"
(
    id BIGSERIAL PRIMARY KEY,

    md5 varchar(40) NOT NULL DEFAULT '',
    mime_type varchar(40) NOT NULL DEFAULT '',
    ext varchar(20) NOT NULL DEFAULT '',
    name varchar(160) NOT NULL DEFAULT '',
    path varchar(255) NOT NULL DEFAULT '',
    url varchar(255) NOT NULL DEFAULT '',
    size bigint NOT NULL DEFAULT 0,
    info text DEFAULT '{}',

    referer_time int NOT NULL DEFAULT 0,
    last_referer_user_id bigint NOT NULL DEFAULT 0,
    last_referer_at bigint NOT NULL DEFAULT 0,
    creator_user_id bigint NOT NULL DEFAULT 0,

    updated_at bigint DEFAULT 0,
    created_at bigint NOT NULL,
    status smallint NOT NULL DEFAULT 1,
    CONSTRAINT uk_attachment_md5 UNIQUE  (md5)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.attachment OWNER to imboy_user;

COMMENT ON TABLE public.attachment IS '文件附件表';

COMMENT ON COLUMN public.attachment.id IS '主键 自增长ID';
COMMENT ON COLUMN public.attachment.md5 IS '附件MD5';
COMMENT ON COLUMN public.attachment.mime_type IS '附件mime_type';
COMMENT ON COLUMN public.attachment.ext IS '附件 扩展名';
COMMENT ON COLUMN public.attachment.name IS '附件 名称';
COMMENT ON COLUMN public.attachment.path IS '附件 path';
COMMENT ON COLUMN public.attachment.url IS '附件 访问地址';
COMMENT ON COLUMN public.attachment.size IS '附件 大小，单位 ';
COMMENT ON COLUMN public.attachment.info IS '附近信息json';

COMMENT ON COLUMN public.attachment.referer_time IS '被引用次数';
COMMENT ON COLUMN public.attachment.last_referer_user_id IS '最后引用用户';
COMMENT ON COLUMN public.attachment.last_referer_at IS '最后引用时间';

COMMENT ON COLUMN public.attachment.creator_user_id IS '创建人用户ID';

COMMENT ON COLUMN public.attachment.updated_at IS '更新记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.attachment.created_at IS '创建记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.attachment.status IS '状态: -1 删除  0 禁用  1 启用';

-- index

CREATE INDEX i_attachment_CreatorUserId_Status ON public.attachment (creator_user_id,status);
