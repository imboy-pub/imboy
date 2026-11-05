-- Freeze the legacy claim/write boundary before backfilling unknown identities.
SET LOCAL lock_timeout='5s';
LOCK TABLE public.talk_record_job, public.attachment_derived_write IN SHARE ROW EXCLUSIVE MODE;
DO $$ BEGIN
 IF EXISTS (SELECT 1 FROM public.talk_record_job WHERE kind='assemble' AND state='running') THEN
  RAISE EXCEPTION 'assembly worker must be quiesced before migration' USING ERRCODE='23510';
 END IF;
END $$;
CREATE TABLE public.attachment_assembly_legacy_unresolved (
 source_job_id bigint PRIMARY KEY,
 reason text NOT NULL,
 recorded_at timestamptz NOT NULL DEFAULT clock_timestamp(),
 resolved_at timestamptz,
 resolved_by text,
 resolution_evidence text,
 CHECK ((resolved_at IS NULL AND resolved_by IS NULL AND resolution_evidence IS NULL)
     OR (resolved_at IS NOT NULL AND resolved_by IS NOT NULL AND resolution_evidence IS NOT NULL AND length(resolution_evidence)>=16))
);
ALTER TABLE public.attachment_assembly_legacy_unresolved OWNER TO v5_fn_owner;
REVOKE ALL ON public.attachment_assembly_legacy_unresolved FROM PUBLIC;
GRANT SELECT ON public.attachment_assembly_legacy_unresolved TO v5_app_runtime;
INSERT INTO public.attachment_assembly_legacy_unresolved(source_job_id,reason)
 SELECT id,'legacy_assembly_staging_unknown' FROM public.talk_record_job
 WHERE kind='assemble' AND attempt>0
 UNION
 SELECT w.source_job_id,'legacy_assembly_staging_unknown'
 FROM public.attachment_derived_write w LEFT JOIN public.talk_record_job j ON j.id=w.source_job_id
 WHERE w.state IN ('writing','unknown') AND (j.kind='assemble' OR j.id IS NULL)
 ON CONFLICT(source_job_id) DO NOTHING;

-- Operator attestation requires independently verified old-node/Worker/PUT
-- quiescence and staging-object inventory; the evidence reference is not proof.
CREATE FUNCTION public.fn_assembly_legacy_attest(p_job bigint,p_evidence text) RETURNS boolean
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
 IF NOT EXISTS (SELECT 1 FROM pg_catalog.pg_roles WHERE rolname=session_user AND rolsuper) OR
    p_evidence IS NULL OR length(trim(p_evidence)) < 16 THEN
  RAISE EXCEPTION 'privileged evidence required' USING ERRCODE='42501';
 END IF;
 UPDATE public.attachment_assembly_legacy_unresolved SET resolved_at=clock_timestamp(),
  resolved_by=session_user,resolution_evidence=p_evidence
 WHERE source_job_id=p_job AND resolved_at IS NULL;
 RETURN FOUND;
END $$;
ALTER FUNCTION public.fn_assembly_legacy_attest(bigint,text) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_assembly_legacy_attest(bigint,text) FROM PUBLIC;

CREATE TABLE public.attachment_assembly_resume_audit (
 job_id bigint PRIMARY KEY,
 resumed_at timestamptz NOT NULL DEFAULT clock_timestamp(),
 resumed_by text NOT NULL
);
ALTER TABLE public.attachment_assembly_resume_audit OWNER TO v5_fn_owner;
REVOKE ALL ON public.attachment_assembly_resume_audit FROM PUBLIC;
GRANT SELECT ON public.attachment_assembly_resume_audit TO v5_app_runtime;

-- Per-job manual recovery only after all legacy identities have been attested.
CREATE FUNCTION public.fn_assembly_legacy_resume(p_job bigint) RETURNS boolean
LANGUAGE plpgsql SECURITY INVOKER SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
 IF NOT EXISTS (SELECT 1 FROM pg_catalog.pg_roles WHERE rolname=session_user AND rolsuper) THEN
  RAISE EXCEPTION 'privileged operator required' USING ERRCODE='42501';
 END IF;
 IF EXISTS (SELECT 1 FROM public.attachment_assembly_legacy_unresolved WHERE resolved_at IS NULL) THEN
  RAISE EXCEPTION 'legacy assembly evidence incomplete' USING ERRCODE='23510';
 END IF;
 UPDATE public.talk_record_job SET suspended=false,updated_at=clock_timestamp()
 WHERE id=p_job AND kind='assemble' AND state='retry_wait' AND suspended=true
  AND error_code='legacy_staging_unresolved';
 IF NOT FOUND THEN RETURN false; END IF;
 INSERT INTO public.attachment_assembly_resume_audit(job_id,resumed_by)
 VALUES(p_job,session_user) ON CONFLICT(job_id) DO NOTHING;
 RETURN true;
END $$;
ALTER FUNCTION public.fn_assembly_legacy_resume(bigint) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_assembly_legacy_resume(bigint) FROM PUBLIC;

CREATE FUNCTION public.fn_assembly_job_contract_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
 IF NEW.kind='assemble' AND NEW.state='running' AND OLD.state IS DISTINCT FROM 'running'
    AND current_setting('imboy.assembly_staging_contract',true) IS DISTINCT FROM '2' THEN
  RAISE EXCEPTION 'assembly staging contract upgrade required' USING ERRCODE='23510';
 END IF;
 RETURN NEW;
END $$;
CREATE TRIGGER assembly_job_contract_guard BEFORE UPDATE OF state ON public.talk_record_job
 FOR EACH ROW EXECUTE FUNCTION public.fn_assembly_job_contract_guard();

-- Preserve the exact staging identity before the first remote write.
CREATE TABLE public.attachment_assembly_staging (
 attachment_id bigint PRIMARY KEY,
 physical_key text NOT NULL UNIQUE CHECK (physical_key ~ '^assembly-staging/[0-9]+/manifest/[0-9a-f]{32}$'),
 locator_sha256 text NOT NULL CHECK (locator_sha256 ~ '^[0-9a-f]{64}$'),
 storage_container text NOT NULL CHECK (storage_container <> ''),
 created_at timestamptz NOT NULL DEFAULT clock_timestamp()
);
ALTER TABLE public.attachment_assembly_staging OWNER TO v5_fn_owner;
REVOKE ALL ON public.attachment_assembly_staging FROM PUBLIC;
ALTER TABLE public.attachment_physical_rescan DROP CONSTRAINT attachment_physical_rescan_domain_check;
ALTER TABLE public.attachment_physical_rescan ADD CONSTRAINT attachment_physical_rescan_domain_check
 CHECK (domain IN ('ordinary','enterprise','direct','staging'));

-- Preserve the 270 function OID so existing trigger call plans retain this guard.
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
 IF EXISTS (SELECT 1 FROM public.attachment_assembly_staging s WHERE s.physical_key=p_key
   AND (p_locator IS NULL OR p_locator !~ '^[0-9a-f]{64}$' OR s.locator_sha256=p_locator)
   AND (p_container IS NULL OR p_container='' OR s.storage_container=p_container)) THEN
  RAISE EXCEPTION 'physical key retired' USING ERRCODE='23505';
 END IF;
END $function$;


CREATE FUNCTION public.fn_assembly_staging_register(p_aid bigint,p_key text) RETURNS boolean
LANGUAGE plpgsql VOLATILE SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE a record;
BEGIN
 IF EXISTS (SELECT 1 FROM public.attachment_assembly_legacy_unresolved WHERE resolved_at IS NULL) THEN
  RETURN false;
 END IF;
 SELECT a0.path,a0.storage_driver,a0.storage_namespace,a0.storage_container,
        a0.storage_locator_sha256,a0.storage_physical_key,w.source_job_id
 INTO a FROM public.attachment a0 JOIN public.attachment_derived_write w ON w.attachment_id=a0.id
 WHERE a0.id=p_aid AND a0.lifecycle_state='live' AND w.state='writing' FOR UPDATE OF a0,w;
 IF NOT FOUND OR a.storage_driver <> 'garage' OR a.storage_namespace IS NULL OR a.storage_namespace=''
    OR a.storage_container IS NULL OR a.storage_container=''
    OR a.storage_locator_sha256 IS NULL OR a.storage_locator_sha256 !~ '^[0-9a-f]{64}$'
    OR p_key IS NULL OR p_key !~ '^assembly-staging/[0-9]+/manifest/[0-9a-f]{32}$'
    OR split_part(p_key,'/',2) <> a.source_job_id::text
    OR p_key=coalesce(a.storage_physical_key,a.path) THEN
  RAISE EXCEPTION 'invalid assembly staging identity' USING ERRCODE='23514';
 END IF;
 PERFORM public.fn_cross_domain_key_guard(p_key,a.storage_locator_sha256,a.storage_container);
 IF public.fn_cross_domain_live_reference(p_key,a.storage_locator_sha256,a.storage_container,'direct',a.path) IS NOT FALSE THEN
  RAISE EXCEPTION 'assembly staging key in use' USING ERRCODE='23505';
 END IF;
 INSERT INTO public.attachment_assembly_staging(attachment_id,physical_key,locator_sha256,storage_container)
 VALUES(p_aid,p_key,a.storage_locator_sha256,a.storage_container);
 INSERT INTO public.attachment_physical_rescan(domain,source_ref,owner_key,driver,namespace,container,locator,physical_key,state)
 VALUES('staging',p_aid::text,a.path,a.storage_driver,a.storage_namespace,a.storage_container,a.storage_locator_sha256,p_key,'blocked');
 RETURN true;
END $$;
ALTER FUNCTION public.fn_assembly_staging_register(bigint,text) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_assembly_staging_register(bigint,text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_assembly_staging_register(bigint,text) TO v5_app_runtime;

CREATE FUNCTION public.fn_assembly_staging_release() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
 IF NEW.state IN ('complete','unknown') AND NEW.state IS DISTINCT FROM OLD.state THEN
  UPDATE public.attachment_physical_rescan SET state='ready',next_scan_at=clock_timestamp()
  WHERE domain='staging' AND source_ref=NEW.attachment_id::text AND state='blocked';
 END IF;
 RETURN NEW;
END $$;
ALTER FUNCTION public.fn_assembly_staging_release() OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_assembly_staging_release() FROM PUBLIC;
CREATE TRIGGER assembly_staging_release AFTER UPDATE OF state ON public.attachment_derived_write
 FOR EACH ROW EXECUTE FUNCTION public.fn_assembly_staging_release();

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
  IF r.domain='staging' AND EXISTS (SELECT 1 FROM public.attachment_assembly_legacy_unresolved WHERE resolved_at IS NULL) THEN
   UPDATE public.attachment_physical_rescan SET next_scan_at=clock_timestamp()+interval '1 minute' WHERE id=r.id;
   CONTINUE;
  END IF;
  IF public.fn_cross_domain_live_reference(r.physical_key,r.locator,r.container,CASE WHEN r.domain='staging' THEN 'direct' ELSE r.domain END,r.owner_key) IS NOT FALSE THEN
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
