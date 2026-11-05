DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM public.c2g_forward_source_fact) THEN
        RAISE EXCEPTION 'accepted group source facts must not be discarded';
    END IF;
END $$;
DROP TRIGGER trg_c2g_forward_source_capture ON public.msg_store_staging;
DROP FUNCTION public.fn_c2g_forward_source_capture();
DROP TABLE public.c2g_forward_source_fact;
