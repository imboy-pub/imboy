-- Table: public.collect_resource

-- DROP TABLE IF EXISTS public."collect_resource";

CREATE TABLE IF NOT EXISTS public."collect_resource"
(
    id BIGSERIAL PRIMARY KEY,

    kind int NOT NULL DEFAULT 0,
    kind_id varchar(40) NOT NULL DEFAULT '',
    info text DEFAULT '{}',

    referer_time int NOT NULL DEFAULT 0,
    creator_user_id bigint NOT NULL DEFAULT 0,

    status smallint NOT NULL DEFAULT 1,
    updated_at bigint DEFAULT 0,
    created_at bigint NOT NULL,
    CONSTRAINT uk_kind_id UNIQUE  (kind_id)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.collect_resource OWNER to imboy_user;

COMMENT ON TABLE public.collect_resource IS '被收藏资源记录表';

COMMENT ON COLUMN public.collect_resource.id IS '主键 自增长ID';
COMMENT ON COLUMN public.collect_resource.kind IS 'Kind 被收藏的资源种类： 1 文本  2 图片  3 语音  4 视频  5 文件  6 位置消息';
COMMENT ON COLUMN public.collect_resource.kind_id IS '资源唯一标识';
COMMENT ON COLUMN public.collect_resource.referer_time IS '被引用次数';

COMMENT ON COLUMN public.collect_resource.creator_user_id IS '创建人用户ID';
COMMENT ON COLUMN public.collect_resource.info IS '被收藏的资源信息';

COMMENT ON COLUMN public.collect_resource.updated_at IS '更新记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.collect_resource.created_at IS '创建记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.collect_resource.status IS '状态: -1 删除  0 禁用  1 启用';

-- index

CREATE INDEX i_collect_resource_CreatorUserId_Status ON public.collect_resource (creator_user_id,status);
