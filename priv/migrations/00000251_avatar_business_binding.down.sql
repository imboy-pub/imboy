DO $$ BEGIN
    IF EXISTS (SELECT 1 FROM public.attachment_avatar_publication WHERE bound_user_id IS NOT NULL) THEN
        RAISE EXCEPTION 'bound avatar publications prevent downgrade';
    END IF;
END $$;
DROP TRIGGER avatar_publication_binding_guard ON public.attachment_avatar_publication;
DROP FUNCTION public.fn_avatar_publication_binding_guard();
ALTER TABLE public.attachment_avatar_publication DROP COLUMN bound_user_id;
