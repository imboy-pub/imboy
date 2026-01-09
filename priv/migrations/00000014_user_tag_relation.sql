-- Table: public.user_tag_relation

-- DROP TABLE IF EXISTS public."user_tag_relation";

CREATE TABLE IF NOT EXISTS public."user_tag_relation"
(
    id BIGSERIAL PRIMARY KEY,
    scene int DEFAULT 0,
    user_id bigint DEFAULT 0,
    tag_id bigint DEFAULT 0,
    object_id varchar(40) NOT NULL DEFAULT '',
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.user_tag_relation OWNER to imboy_user;

COMMENT ON TABLE public.user_tag_relation IS '用户标签记录表 scene 场景下，user_id 给 object_id 打了标签 tag_id';

COMMENT ON COLUMN public.user_tag_relation.id IS '主键 自增长ID';

COMMENT ON COLUMN public.user_tag_relation.scene IS '标签应用场景 1  用户收藏记录标签  2 用户朋友标签';
COMMENT ON COLUMN public.user_tag_relation.user_id IS '记录所属用户ID';
COMMENT ON COLUMN public.user_tag_relation.tag_id IS '标签ID public.tag 表的自增长ID';
COMMENT ON COLUMN public.user_tag_relation.object_id IS '被打标签收藏类型ID （kind_id） or 被打标签用户ID (int 型用户ID)';

COMMENT ON COLUMN public.user_tag_relation.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';

-- index

CREATE UNIQUE INDEX uk_user_tag_relation_Scene_UserId_ObjectId_TagId ON public.user_tag_relation(scene, user_id, object_id, tag_id);
CREATE INDEX i_user_tag_relation_Scene_TagId ON public.user_tag_relation(scene, tag_id);
