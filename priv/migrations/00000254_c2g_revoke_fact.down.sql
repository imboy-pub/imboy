DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM public.c2g_revoke_fact) THEN
        RAISE EXCEPTION 'accepted group revoke facts must not be discarded';
    END IF;
END $$;
DROP TABLE public.c2g_revoke_fact;
DROP TABLE public.c2g_revoke_capture_epoch;
