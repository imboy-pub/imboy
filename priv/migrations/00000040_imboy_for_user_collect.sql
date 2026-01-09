
DROP TRIGGER IF EXISTS imboy_for_user_collect ON public.user_collect;
CREATE TRIGGER imboy_for_user_collect
    AFTER INSERT OR DELETE OR UPDATE
    OF attach_md5
    ON public.user_collect
    FOR EACH ROW
    EXECUTE FUNCTION public.imboy_user_collect_fun();
