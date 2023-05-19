-- 为用户全文索引创建触发器
DROP TRIGGER IF EXISTS imboy_for_fts_user ON public.user;
CREATE TRIGGER imboy_for_fts_user
    AFTER INSERT OR DELETE OR UPDATE
    OF nickname,sign,region
    ON public.user
    FOR EACH ROW
    EXECUTE FUNCTION public.imboy_user_for_fts_fun();
