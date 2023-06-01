

DROP TRIGGER IF EXISTS imboy_for_collect_user ON public.collect_user;
CREATE TRIGGER imboy_for_collect_user
    AFTER INSERT OR DELETE OR UPDATE
    OF kind_id
    ON public.collect_user
    FOR EACH ROW
    EXECUTE FUNCTION public.imboy_collect_user_fun();
