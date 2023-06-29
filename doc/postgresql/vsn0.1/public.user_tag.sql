-- Table: public.user_tag

-- DROP TABLE IF EXISTS public."user_tag";

CREATE TABLE IF NOT EXISTS public."user_tag"
(
    id BIGSERIAL PRIMARY KEY,
    scene int DEFAULT 0,
    user_id bigint DEFAULT 0,
    tag_id bigint DEFAULT 0,
    object_id varchar(40) NOT NULL DEFAULT '',
    created_at bigint NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.user_tag OWNER to imboy_user;

COMMENT ON TABLE public.user_tag IS '用户标签记录表 scene 场景下，user_id 给 object_id 打了标签 tag_id';

COMMENT ON COLUMN public.user_tag.id IS '主键 自增长ID';

COMMENT ON COLUMN public.user_tag.scene IS '标签应用场景 1  用户收藏记录标签  2 用户朋友标签';
COMMENT ON COLUMN public.user_tag.user_id IS '记录所属用户ID';
COMMENT ON COLUMN public.user_tag.tag_id IS '标签ID public.tag 表的自增长ID';
COMMENT ON COLUMN public.user_tag.object_id IS '被打标签收藏类型ID （kind_id） or 被打标签用户ID (int 型用户ID)';

COMMENT ON COLUMN public.user_tag.created_at IS '创建记录Unix时间戳毫秒单位';

-- index

CREATE UNIQUE INDEX uk_user_tag_Scene_UserId_ObjectId_TagId ON public.user_tag(scene, user_id, object_id, tag_id);
CREATE INDEX i_user_tag_Scene_TagId ON public.user_tag(scene, tag_id);
