DO $$ BEGIN
    IF EXISTS (SELECT 1 FROM public.attachment_avatar_alias) THEN
        RAISE EXCEPTION 'avatar aliases must be migrated before rollback';
    END IF;
END $$;
DROP TRIGGER avatar_alias_hold ON public.attachment;
DROP FUNCTION public.fn_avatar_alias_hold();
DROP TRIGGER avatar_alias_guard ON public.attachment_avatar_alias;
DROP FUNCTION public.fn_avatar_alias_guard();
DROP FUNCTION public.fn_avatar_alias_bound_visible(uuid,text);
DROP FUNCTION public.fn_avatar_winner_fence(text);
DROP TABLE public.attachment_avatar_alias;
