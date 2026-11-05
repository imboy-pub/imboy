DO $$ BEGIN
    IF EXISTS (SELECT 1 FROM public.attachment_avatar_publication WHERE state IN ('reserved', 'unknown', 'complete')) THEN
        RAISE EXCEPTION 'cannot discard avatar publication before business binding';
    END IF;
END $$;
DROP TRIGGER avatar_publication_attachment_guard ON public.attachment;
DROP FUNCTION public.fn_avatar_publication_attachment_guard();
DROP TRIGGER avatar_publication_insert_guard ON public.attachment_avatar_publication;
DROP FUNCTION public.fn_avatar_publication_insert_guard();
DROP TRIGGER avatar_publication_transition ON public.attachment_avatar_publication;
DROP FUNCTION public.fn_avatar_publication_transition();
DROP TABLE public.attachment_avatar_publication;
