-- Table: public.announcement

-- DROP TABLE IF EXISTS public.announcement;

CREATE TABLE IF NOT EXISTS public.announcement
(
    id BIGSERIAL PRIMARY KEY,
    adm_user_id bigint NOT NULL,
    title varchar(200) COLLATE pg_catalog."default" NOT NULL DEFAULT '',
    body text NOT NULL DEFAULT '',
    type varchar(20) COLLATE pg_catalog."default" NOT NULL DEFAULT 'info',
    status smallint NOT NULL DEFAULT 0,
    pinned smallint NOT NULL DEFAULT 0,
    published_at timestamptz,
    expired_at timestamptz,
    updated_at timestamptz DEFAULT CURRENT_TIMESTAMP NULL,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.announcement OWNER to imboy_user;

COMMENT ON TABLE public.announcement IS '全局公告表';

COMMENT ON COLUMN public.announcement.id IS '主键ID';
COMMENT ON COLUMN public.announcement.adm_user_id IS '创建者管理员ID';
COMMENT ON COLUMN public.announcement.title IS '公告标题';
COMMENT ON COLUMN public.announcement.body IS '公告内容';
COMMENT ON COLUMN public.announcement.type IS '公告类型: info/warning/important';
COMMENT ON COLUMN public.announcement.status IS '状态: -1 已删除, 0 草稿, 1 已发布, 2 已撤回';
COMMENT ON COLUMN public.announcement.pinned IS '是否置顶: 0 否, 1 是';
COMMENT ON COLUMN public.announcement.published_at IS '发布时间';
COMMENT ON COLUMN public.announcement.expired_at IS '过期时间';
COMMENT ON COLUMN public.announcement.updated_at IS '更新时间';
COMMENT ON COLUMN public.announcement.created_at IS '创建时间';

-- 索引
CREATE INDEX IF NOT EXISTS idx_announcement_status ON public.announcement USING btree (status);
CREATE INDEX IF NOT EXISTS idx_announcement_pinned ON public.announcement USING btree (pinned);
CREATE INDEX IF NOT EXISTS idx_announcement_created_at ON public.announcement USING btree (created_at desc);
