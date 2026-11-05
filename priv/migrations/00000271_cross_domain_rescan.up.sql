CREATE TABLE public.attachment_physical_rescan (
 id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
 domain text NOT NULL CHECK (domain IN ('ordinary','enterprise','direct')),
 source_ref text NOT NULL, owner_key text NOT NULL,
 driver text, namespace text, container text, locator text, physical_key text,
 state text NOT NULL DEFAULT 'ready' CHECK (state IN ('ready','leased','blocked')),
 lease_token uuid, lease_until timestamptz,
 next_scan_at timestamptz NOT NULL DEFAULT clock_timestamp()+interval '1 hour',
 UNIQUE(domain, source_ref),
 CHECK ((state='leased') = (lease_token IS NOT NULL AND lease_until IS NOT NULL))
);
ALTER TABLE public.attachment_physical_rescan OWNER TO v5_fn_owner;
REVOKE ALL ON public.attachment_physical_rescan FROM PUBLIC;

CREATE FUNCTION public.fn_cross_rescan_capture() RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER
SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE d text; ref text; owner text; drv text; ns text; container text; locator text; k text;
BEGIN
 IF NEW.job_state <> 'done' THEN RETURN NEW; END IF;
 IF TG_TABLE_NAME='attachment_object_deletion' THEN
  d:='ordinary'; ref:=NEW.path; owner:=NEW.path; drv:=NEW.storage_driver;
  ns:=NEW.storage_namespace; container:=NEW.storage_container; locator:=NEW.storage_locator_sha256;
  k:=coalesce(NEW.storage_physical_key,NEW.path);
 ELSIF TG_TABLE_NAME='enterprise_asset_delete_queue' THEN
  d:='enterprise'; ref:=NEW.organization_id::text||'/'||NEW.workspace_id::text||'/'||NEW.asset_id::text;
  owner:=NEW.object_key; drv:=NEW.storage_driver; ns:=NEW.storage_namespace;
  container:=NEW.storage_container; locator:=NEW.storage_locator_sha256;
  k:=coalesce(NEW.storage_physical_key,NEW.object_key);
 ELSE
  d:='direct'; ref:=NEW.id::text; owner:=NEW.object_key; drv:=NEW.target_driver;
  ns:=NEW.target_namespace; container:=NEW.target_container; locator:=NEW.locator_sha256; k:=NEW.physical_key;
 END IF;
 INSERT INTO public.attachment_physical_rescan(domain,source_ref,owner_key,driver,namespace,container,locator,physical_key,state)
 VALUES(d,ref,owner,drv,ns,container,locator,k,
  CASE WHEN drv='garage' AND ns IS NOT NULL AND ns<>'' AND container IS NOT NULL AND container<>''
   AND locator ~ '^[0-9a-f]{64}$' AND k IS NOT NULL AND k<>'' THEN 'ready' ELSE 'blocked' END)
 ON CONFLICT(domain,source_ref) DO NOTHING;
 RETURN NEW;
END $$;
ALTER FUNCTION public.fn_cross_rescan_capture() OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_cross_rescan_capture() FROM PUBLIC;
CREATE TRIGGER cross_rescan_ordinary AFTER INSERT OR UPDATE OF job_state ON public.attachment_object_deletion FOR EACH ROW EXECUTE FUNCTION public.fn_cross_rescan_capture();
CREATE TRIGGER cross_rescan_enterprise AFTER INSERT OR UPDATE OF job_state ON public.enterprise_asset_delete_queue FOR EACH ROW EXECUTE FUNCTION public.fn_cross_rescan_capture();
CREATE TRIGGER cross_rescan_direct AFTER INSERT OR UPDATE OF job_state ON public.attach_direct_cleanup FOR EACH ROW EXECUTE FUNCTION public.fn_cross_rescan_capture();

INSERT INTO public.attachment_physical_rescan(domain,source_ref,owner_key,driver,namespace,container,locator,physical_key,state)
 SELECT 'ordinary',path,path,storage_driver,storage_namespace,storage_container,storage_locator_sha256,
 coalesce(storage_physical_key,path), CASE WHEN storage_driver='garage' AND coalesce(storage_namespace,'')<>''
 AND coalesce(storage_container,'')<>'' AND storage_locator_sha256 ~ '^[0-9a-f]{64}$'
 AND coalesce(storage_physical_key,path,'')<>'' THEN 'ready' ELSE 'blocked' END FROM public.attachment_object_deletion WHERE job_state='done';
INSERT INTO public.attachment_physical_rescan(domain,source_ref,owner_key,driver,namespace,container,locator,physical_key,state)
 SELECT 'enterprise',organization_id::text||'/'||workspace_id::text||'/'||asset_id::text,object_key,
 storage_driver,storage_namespace,storage_container,storage_locator_sha256,coalesce(storage_physical_key,object_key),
 CASE WHEN storage_driver='garage' AND coalesce(storage_namespace,'')<>'' AND coalesce(storage_container,'')<>''
 AND storage_locator_sha256 ~ '^[0-9a-f]{64}$' AND coalesce(storage_physical_key,object_key,'')<>''
 THEN 'ready' ELSE 'blocked' END
 FROM public.enterprise_asset_delete_queue WHERE job_state='done';
INSERT INTO public.attachment_physical_rescan(domain,source_ref,owner_key,driver,namespace,container,locator,physical_key,state)
 SELECT 'direct',id::text,object_key,target_driver,target_namespace,target_container,locator_sha256,physical_key,
 CASE WHEN target_driver='garage' AND coalesce(target_namespace,'')<>'' AND coalesce(target_container,'')<>''
 AND locator_sha256 ~ '^[0-9a-f]{64}$' AND coalesce(physical_key,'')<>''
 THEN 'ready' ELSE 'blocked' END
 FROM public.attach_direct_cleanup WHERE job_state='done';

CREATE FUNCTION public.fn_cross_rescan_claim(p_token uuid)
RETURNS TABLE(out_id bigint,out_physical_key text,out_driver text,out_namespace text,out_container text,out_locator text)
LANGUAGE plpgsql VOLATILE SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
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
END $$;
ALTER FUNCTION public.fn_cross_rescan_claim(uuid) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_cross_rescan_claim(uuid) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_cross_rescan_claim(uuid) TO v5_worker;
CREATE FUNCTION public.fn_cross_rescan_ack(p_id bigint,p_token uuid,p_ok boolean) RETURNS text
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
 UPDATE public.attachment_physical_rescan SET state='ready',lease_token=NULL,lease_until=NULL,
 next_scan_at=clock_timestamp()+CASE WHEN p_ok THEN interval '1 hour' ELSE interval '1 minute' END
 WHERE id=p_id AND state='leased' AND lease_token=p_token;
 IF NOT FOUND THEN RETURN 'stale_token'; END IF;
 PERFORM public.fn_cross_delete_finish('rescan',p_id::text,p_token,p_ok);
 RETURN 'ready';
END $$;
ALTER FUNCTION public.fn_cross_rescan_ack(bigint,uuid,boolean) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_cross_rescan_ack(bigint,uuid,boolean) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_cross_rescan_ack(bigint,uuid,boolean) TO v5_worker;
