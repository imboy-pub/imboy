LOCK TABLE public.talk_record_job, public.attachment_derived_write, public.attachment_assembly_legacy_unresolved, public.attachment_assembly_resume_audit, public.attachment_assembly_staging, public.attachment_physical_rescan IN ACCESS EXCLUSIVE MODE;
DO $$ BEGIN IF EXISTS (SELECT 1 FROM public.attachment_assembly_resume_audit) OR EXISTS (SELECT 1 FROM public.attachment_assembly_legacy_unresolved) OR EXISTS (SELECT 1 FROM public.attachment_assembly_staging) OR EXISTS (SELECT 1 FROM public.attachment_physical_rescan WHERE domain='staging') THEN RAISE EXCEPTION 'assembly staging tombstones must be retained' USING ERRCODE='23514'; END IF; END $$;
DROP TRIGGER assembly_job_contract_guard ON public.talk_record_job;
DROP FUNCTION public.fn_assembly_job_contract_guard();
DROP FUNCTION public.fn_assembly_legacy_resume(bigint);
DROP TABLE public.attachment_assembly_resume_audit;
DROP FUNCTION public.fn_assembly_legacy_attest(bigint,text);
DROP TABLE public.attachment_assembly_legacy_unresolved;
DROP TRIGGER assembly_staging_release ON public.attachment_derived_write;
DROP FUNCTION public.fn_assembly_staging_release();
DROP FUNCTION public.fn_assembly_staging_register(bigint,text);
CREATE OR REPLACE FUNCTION public.fn_cross_domain_key_guard(p_key text, p_locator text, p_container text)
 RETURNS void
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO 'pg_catalog', 'public', 'pg_temp'
AS $function$
BEGIN
 IF p_key IS NULL OR p_key='' THEN
  RAISE EXCEPTION 'physical key unavailable' USING ERRCODE='23510';
 END IF;
 IF NOT pg_try_advisory_xact_lock(hashtextextended(p_key,1699)) THEN
  RAISE EXCEPTION 'physical key busy' USING ERRCODE='40001';
 END IF;
 IF EXISTS (SELECT 1 FROM public.attachment_object_deletion q
  WHERE coalesce(q.storage_physical_key,q.path)=p_key
   AND (q.storage_locator_sha256 IS NULL OR q.storage_locator_sha256 !~ '^[0-9a-f]{64}$' OR p_locator IS NULL OR p_locator !~ '^[0-9a-f]{64}$' OR q.storage_locator_sha256=p_locator)
   AND (q.storage_container IS NULL OR q.storage_container = '' OR p_container IS NULL OR p_container = '' OR q.storage_container=p_container))
 OR EXISTS (SELECT 1 FROM public.enterprise_asset_delete_queue q
  WHERE coalesce(q.storage_physical_key,q.object_key)=p_key
   AND (q.storage_locator_sha256 IS NULL OR q.storage_locator_sha256 !~ '^[0-9a-f]{64}$' OR p_locator IS NULL OR p_locator !~ '^[0-9a-f]{64}$' OR q.storage_locator_sha256=p_locator)
   AND (q.storage_container IS NULL OR q.storage_container = '' OR p_container IS NULL OR p_container = '' OR q.storage_container=p_container))
 OR EXISTS (SELECT 1 FROM public.attach_direct_cleanup q
  WHERE q.physical_key=p_key
   AND (q.locator_sha256 IS NULL OR q.locator_sha256 !~ '^[0-9a-f]{64}$' OR p_locator IS NULL OR p_locator !~ '^[0-9a-f]{64}$' OR q.locator_sha256=p_locator)
   AND (q.target_container IS NULL OR q.target_container = '' OR p_container IS NULL OR p_container = '' OR q.target_container=p_container)) THEN
  RAISE EXCEPTION 'physical key retired' USING ERRCODE='23505';
 END IF;
END $function$;
ALTER TABLE public.attachment_physical_rescan DROP CONSTRAINT attachment_physical_rescan_domain_check;
ALTER TABLE public.attachment_physical_rescan ADD CONSTRAINT attachment_physical_rescan_domain_check CHECK (domain IN ('ordinary','enterprise','direct'));
DROP TABLE public.attachment_assembly_staging;
CREATE OR REPLACE FUNCTION public.fn_cross_rescan_claim(p_token uuid)
 RETURNS TABLE(out_id bigint, out_physical_key text, out_driver text, out_namespace text, out_container text, out_locator text)
 LANGUAGE plpgsql
 SECURITY DEFINER
 SET search_path TO 'pg_catalog', 'public', 'pg_temp'
AS $function$
DECLARE r public.attachment_physical_rescan%ROWTYPE;
BEGIN
 IF p_token IS NULL THEN RAISE EXCEPTION 'missing token' USING ERRCODE='22023'; END IF;
 FOR r IN SELECT * FROM public.attachment_physical_rescan
  WHERE next_scan_at<=clock_timestamp() AND (state='ready' OR (state='leased' AND lease_until<clock_timestamp()))
  ORDER BY next_scan_at,id FOR UPDATE SKIP LOCKED LIMIT 100 LOOP
  IF NOT pg_try_advisory_xact_lock(hashtextextended(r.physical_key,1699)) THEN
   UPDATE public.attachment_physical_rescan SET next_scan_at=clock_timestamp()+interval '1 minute' WHERE id=r.id;
   CONTINUE;
  END IF;
  IF r.state='leased' THEN
   PERFORM public.fn_cross_delete_finish('rescan',r.id::text,r.lease_token,false);
  END IF;
  IF public.fn_cross_domain_live_reference(r.physical_key,r.locator,r.container,r.domain,r.owner_key) IS NOT FALSE THEN
   UPDATE public.attachment_physical_rescan SET state='ready',lease_token=NULL,lease_until=NULL,
    next_scan_at=clock_timestamp()+interval '1 minute' WHERE id=r.id;
   CONTINUE;
  END IF;
  PERFORM public.fn_cross_delete_begin('rescan',r.id::text,p_token,r.physical_key,r.locator,r.container);
  RETURN QUERY UPDATE public.attachment_physical_rescan x SET state='leased',lease_token=p_token,
   lease_until=clock_timestamp()+interval '120 seconds' WHERE x.id=r.id
   RETURNING x.id,x.physical_key,x.driver,x.namespace,x.container,x.locator;
  RETURN;
 END LOOP;
END $function$
