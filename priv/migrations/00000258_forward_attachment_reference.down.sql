DO $$ BEGIN
    IF EXISTS (SELECT 1 FROM public.forward_attachment_reference) OR EXISTS (SELECT 1 FROM public.c2g_forward_edit_fact) THEN
        RAISE EXCEPTION 'forward attachment references and edit facts must be settled before rollback';
    END IF;
END $$;
DROP FUNCTION public.fn_forward_attachment_bind(text,text,text,bigint,bigint,text,jsonb,text,text,bigint,bigint,bigint,boolean);
DROP FUNCTION public.fn_forward_source_c2g_validate(text,bigint,bigint,bigint,text,jsonb,boolean);
DROP FUNCTION public.fn_forward_source_c2c_validate(text,bigint,bigint,bigint,text,jsonb);
DROP TRIGGER c2g_forward_edit_capture ON public.msg_store_staging;
DROP FUNCTION public.fn_c2g_forward_edit_capture();
DROP TRIGGER forward_attachment_lifecycle_guard ON public.attachment;
DROP FUNCTION public.fn_forward_attachment_lifecycle_guard();
DROP FUNCTION public.fn_forward_attachment_authorize(text,bigint,boolean);
DROP FUNCTION public.fn_v5_forward_reference_blocks(text);
DROP FUNCTION public.fn_forward_attachment_target_live(text,text,bigint,bigint,bigint,text,boolean);
DROP FUNCTION public.fn_forward_c2c_gc_blocks(text,text,text,bigint,bigint);
DROP FUNCTION public.fn_forward_payload_contains(text,text);
DROP TABLE public.forward_attachment_reference;
DROP TABLE public.c2g_forward_edit_fact;
