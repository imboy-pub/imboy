DROP TRIGGER IF EXISTS imboy_for_user_tag_relation ON public."ON";

DROP FUNCTION IF EXISTS public.imboy_user_tag_relation_fun;

DROP TABLE IF EXISTS public."user_tag_relation" CASCADE;
DROP INDEX IF EXISTS uk_user_tag_relation_Scene_UserId_ObjectId_TagId;
DROP INDEX IF EXISTS i_user_tag_relation_Scene_TagId;
