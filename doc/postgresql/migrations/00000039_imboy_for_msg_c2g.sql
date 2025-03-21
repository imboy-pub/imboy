
DROP TRIGGER IF EXISTS imboy_for_msg_c2g ON public.msg_c2g;
CREATE TRIGGER imboy_for_msg_c2g
    AFTER INSERT OR DELETE OR UPDATE
    OF msg_id
    ON public.msg_c2g
    FOR EACH ROW
    EXECUTE FUNCTION public.imboy_msg_c2g_fun();
