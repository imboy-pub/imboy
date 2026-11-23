LOCK TABLE public."user" IN ACCESS EXCLUSIVE MODE;
DO $$
BEGIN
    IF EXISTS (SELECT FROM public."user" WHERE region_selection IS NOT NULL) THEN
        RAISE EXCEPTION 'region selection is in use; preserve it before rollback';
    END IF;
END
$$;
DROP TRIGGER trg_user_region_selection_clear_legacy ON public."user";
DROP FUNCTION public.fn_user_region_selection_clear_legacy();
ALTER TABLE public."user"
    DROP CONSTRAINT ck_user_region_selection_binding,
    DROP COLUMN region_selection,
    DROP COLUMN region_selection_legacy;
DROP FUNCTION public.fn_user_region_selection_valid(jsonb);
