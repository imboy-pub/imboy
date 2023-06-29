
DROP TRIGGER IF EXISTS imboy_for_user_tag ON public.user_tag;
CREATE TRIGGER imboy_for_user_tag
    AFTER INSERT OR DELETE OR UPDATE
    OF scene,user_id,tag_id
    ON public.user_tag
    FOR EACH ROW
    EXECUTE FUNCTION public.imboy_user_tag_fun();
