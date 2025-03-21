-- Table: public.config

-- DROP TABLE IF EXISTS public.config;

CREATE TABLE IF NOT EXISTS public.config
(
    tab varchar(20) COLLATE pg_catalog."default" NOT NULL,
    key varchar(40) COLLATE pg_catalog."default" NOT NULL,
    value text NOT NULL,
    title varchar(40) COLLATE pg_catalog."default" NOT NULL,
    sort integer NOT NULL DEFAULT 20,
    remark varchar(200) COLLATE pg_catalog."default" NOT NULL DEFAULT '',
    system smallint NOT NULL DEFAULT 0,
    status smallint NOT NULL DEFAULT 1,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamptz DEFAULT CURRENT_TIMESTAMP NULL,
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

COMMENT ON COLUMN public.config.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';

COMMENT ON COLUMN public.config.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';

-- index



-- data

