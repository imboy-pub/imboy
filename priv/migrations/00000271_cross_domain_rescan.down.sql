LOCK TABLE public.attachment_object_deletion, public.enterprise_asset_delete_queue,
 public.attach_direct_cleanup IN ACCESS EXCLUSIVE MODE;
DO $$ BEGIN
 IF EXISTS (SELECT 1 FROM public.attachment_physical_rescan)
 OR EXISTS (SELECT 1 FROM public.attachment_object_deletion)
 OR EXISTS (SELECT 1 FROM public.enterprise_asset_delete_queue)
 OR EXISTS (SELECT 1 FROM public.attach_direct_cleanup) THEN
  RAISE EXCEPTION 'rescan tombstones must be retained' USING ERRCODE='23514';
 END IF;
END $$;
DROP TRIGGER cross_rescan_ordinary ON public.attachment_object_deletion;
DROP TRIGGER cross_rescan_enterprise ON public.enterprise_asset_delete_queue;
DROP TRIGGER cross_rescan_direct ON public.attach_direct_cleanup;
DROP FUNCTION public.fn_cross_rescan_capture();
DROP FUNCTION public.fn_cross_rescan_claim(uuid);
DROP FUNCTION public.fn_cross_rescan_ack(bigint,uuid,boolean);
DROP TABLE public.attachment_physical_rescan;
