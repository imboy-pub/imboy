-- Table: public.config

-- DROP TABLE IF EXISTS public.config;

CREATE TABLE IF NOT EXISTS public.config
(
    tab varchar(20) COLLATE pg_catalog."default" NOT NULL,
    key varchar(40) COLLATE pg_catalog."default" NOT NULL,
    value text NOT NULL,
    title varchar(40) COLLATE pg_catalog."default" NOT NULL,
    sort integer NOT NULL DEFAULT 20,
    remark varchar(200) COLLATE pg_catalog."default" NOT NULL,
    system smallint NOT NULL DEFAULT 0,
    status smallint NOT NULL DEFAULT 1,
    created_at bigint NOT NULL DEFAULT 0,
    updated_at bigint NOT NULL DEFAULT 0,
    CONSTRAINT config_pkey PRIMARY KEY (key)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.config OWNER to imboy_user;

COMMENT ON TABLE public.config IS '系统配置';

COMMENT ON COLUMN public.config.tab IS '配置选项，便于后台分类浏览';

COMMENT ON COLUMN public.config.key IS '主键';

COMMENT ON COLUMN public.config.title IS '标题';

COMMENT ON COLUMN public.config.sort IS '排序 降序排序，大的值在前面';

COMMENT ON COLUMN public.config.system IS '是否为系统配置，系统配置不可删除';

COMMENT ON COLUMN public.config.status IS '状态: -1 删除  0 禁用  1 启用';

COMMENT ON COLUMN public.config.created_at IS '创建记录Unix时间戳毫秒单位';

COMMENT ON COLUMN public.config.updated_at IS '更新记录Unix时间戳毫秒单位';

-- index
