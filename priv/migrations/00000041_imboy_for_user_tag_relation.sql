
DROP TRIGGER IF EXISTS imboy_for_user_tag_relation ON public.user_tag_relation;
CREATE TRIGGER imboy_for_user_tag_relation
    AFTER INSERT OR DELETE OR UPDATE
    OF scene,user_id,tag_id
    ON public.user_tag_relation
    FOR EACH ROW
    EXECUTE FUNCTION public.imboy_user_tag_relation_fun();
