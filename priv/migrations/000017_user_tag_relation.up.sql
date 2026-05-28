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

CREATE UNIQUE INDEX IF NOT EXISTS uk_user_tag_relation_Scene_UserId_ObjectId_TagId ON public.user_tag_relation(scene, user_id, object_id, tag_id);
CREATE INDEX IF NOT EXISTS i_user_tag_relation_Scene_TagId ON public.user_tag_relation(scene, tag_id);

-- tag_relation trigger function
-- FUNCTION: public.imboy_user_tag_relation_fun()

-- DROP FUNCTION IF EXISTS public.imboy_user_tag_relation_fun();

CREATE OR REPLACE FUNCTION public.imboy_user_tag_relation_fun()
    RETURNS trigger
    LANGUAGE 'plpgsql'
    COST 100
    VOLATILE NOT LEAKPROOF
AS $BODY$
begin
  IF (TG_OP = 'DELETE' OR TG_OP = 'TRUNCATE') THEN
    UPDATE public.user_tag SET referer_time = referer_time - 1 WHERE creator_user_id = OLD.user_id AND scene = OLD.scene and id = OLD.tag_id;
    RETURN OLD;
  ELSIF (TG_OP = 'UPDATE') THEN
    -- user_tag 业务上不会有单独修改name的可能性
    -- 所以不要考虑name字段修改的情况
    RETURN NEW;
  ELSIF (TG_OP = 'INSERT') THEN
    UPDATE public.user_tag SET referer_time = referer_time + 1 WHERE creator_user_id = NEW.user_id AND scene = NEW.scene and id = NEW.tag_id;
  return NEW;
  END IF;
end;
$BODY$;

ALTER FUNCTION public.imboy_user_tag_relation_fun()
    OWNER TO imboy_user;

-- tag_relation trigger
DROP TRIGGER IF EXISTS imboy_for_user_tag_relation ON public.user_tag_relation;
CREATE TRIGGER imboy_for_user_tag_relation
    AFTER INSERT OR DELETE OR UPDATE
    OF scene,user_id,tag_id
    ON public.user_tag_relation
    FOR EACH ROW
    EXECUTE FUNCTION public.imboy_user_tag_relation_fun();
