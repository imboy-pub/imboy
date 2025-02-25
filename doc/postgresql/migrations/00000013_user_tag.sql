-- Table: public.user_tag

-- DROP TABLE IF EXISTS public."user_tag";

CREATE TABLE IF NOT EXISTS public."user_tag"
(
    id BIGSERIAL PRIMARY KEY,
    creator_user_id bigint NOT NULL,
    scene int DEFAULT 0,
    name varchar(80) DEFAULT '',
    referer_time int NOT NULL DEFAULT 0,
    updated_at timestamptz DEFAULT CURRENT_TIMESTAMP NULL,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT uk_Scene_CreatorId_Name UNIQUE  (scene, creator_user_id, name)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.user_tag OWNER to imboy_user;

COMMENT ON TABLE public.user_tag IS 'tag记录表';

COMMENT ON COLUMN public.user_tag.id IS '主键 自增长ID';

COMMENT ON COLUMN public.user_tag.creator_user_id IS '创建人用户ID';
COMMENT ON COLUMN public.user_tag.scene IS '标签应用场景 1  用户收藏记录标签  2 用户朋友标签';

COMMENT ON COLUMN public.user_tag.name IS '标签名称';
COMMENT ON COLUMN public.user_tag.referer_time IS '被引用次数 关联object_id 数量';

COMMENT ON COLUMN public.user_tag.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';
COMMENT ON COLUMN public.user_tag.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';

-- index
