-- Lock all writers before historical backfill and installing guards. Existing
-- leased jobs are included in the same transaction; lock timeout fails closed.
SET LOCAL lock_timeout = '5s';
LOCK TABLE public.attachment, public.enterprise_asset, public.attach_pending,
 public.attach_direct_attempt, public.attachment_object_deletion,
 public.enterprise_asset_delete_queue, public.attach_direct_cleanup,
 public.enterprise_retention_hold, public.enterprise_direct_retirement_audit IN ACCESS EXCLUSIVE MODE;

-- Freeze the deletion scope before purge can remove the asset row. Historical
-- rows without a surviving asset remain unknown and match every narrower hold.
ALTER TABLE public.enterprise_asset_delete_queue
 ADD COLUMN cross_scope_known boolean NOT NULL DEFAULT false,
 ADD COLUMN cross_conversation_id bigint,
 ADD COLUMN cross_message_id bigint;
UPDATE public.enterprise_asset_delete_queue q SET
 cross_scope_known=true,cross_conversation_id=a.conversation_id,cross_message_id=a.message_id
 FROM public.enterprise_asset a WHERE a.organization_id=q.organization_id
 AND a.workspace_id=q.workspace_id AND a.id=q.asset_id;
CREATE FUNCTION public.fn_cross_enterprise_queue_scope() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
 SELECT true,a.conversation_id,a.message_id INTO NEW.cross_scope_known,NEW.cross_conversation_id,NEW.cross_message_id
 FROM public.enterprise_asset a WHERE a.organization_id=NEW.organization_id AND a.workspace_id=NEW.workspace_id AND a.id=NEW.asset_id;
 IF NOT FOUND THEN NEW.cross_scope_known:=false; NEW.cross_conversation_id:=NULL; NEW.cross_message_id:=NULL; END IF;
 RETURN NEW;
END $$;
ALTER FUNCTION public.fn_cross_enterprise_queue_scope() OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_cross_enterprise_queue_scope() FROM PUBLIC;
CREATE TRIGGER aa_cross_enterprise_queue_scope BEFORE INSERT ON public.enterprise_asset_delete_queue
 FOR EACH ROW EXECUTE FUNCTION public.fn_cross_enterprise_queue_scope();

-- Direct retirement routes through the ordinary deletion queue. Freeze its
-- enterprise scope in the durable retirement audit before the asset disappears.
ALTER TABLE public.enterprise_direct_retirement_audit
 ADD COLUMN cross_scope_known boolean NOT NULL DEFAULT false,
 ADD COLUMN cross_conversation_id bigint,
 ADD COLUMN cross_message_id bigint;
UPDATE public.enterprise_direct_retirement_audit d SET
 cross_scope_known=true,cross_conversation_id=a.conversation_id,cross_message_id=a.message_id
 FROM public.enterprise_asset a WHERE a.organization_id=d.organization_id
 AND a.workspace_id=d.workspace_id AND a.id=d.asset_id AND a.object_key=d.object_key;
GRANT SELECT ON public.enterprise_direct_retirement_audit TO v5_fn_owner;
CREATE FUNCTION public.fn_cross_direct_retirement_scope() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
 SELECT true,a.conversation_id,a.message_id INTO NEW.cross_scope_known,NEW.cross_conversation_id,NEW.cross_message_id
 FROM public.enterprise_asset a WHERE a.organization_id=NEW.organization_id AND a.workspace_id=NEW.workspace_id AND a.id=NEW.asset_id AND a.object_key=NEW.object_key;
 IF NOT FOUND THEN NEW.cross_scope_known:=false; NEW.cross_conversation_id:=NULL; NEW.cross_message_id:=NULL; END IF;
 RETURN NEW;
END $$;
ALTER FUNCTION public.fn_cross_direct_retirement_scope() OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_cross_direct_retirement_scope() FROM PUBLIC;
CREATE TRIGGER aa_cross_direct_retirement_scope BEFORE INSERT ON public.enterprise_direct_retirement_audit
 FOR EACH ROW EXECUTE FUNCTION public.fn_cross_direct_retirement_scope();

-- Each remote DELETE generation is durable: lease expiry cannot prove the
-- previous network operation has finished.
CREATE TABLE public.cross_domain_delete_attempt (
 domain text NOT NULL CHECK (domain IN ('ordinary','enterprise','direct','rescan')),
 source_ref text NOT NULL,
 lease_token uuid NOT NULL,
 physical_key text NOT NULL,
 locator_sha256 text,
 storage_container text,
 state text NOT NULL CHECK (state IN ('pending','settled','unknown')),
 created_at timestamptz NOT NULL DEFAULT clock_timestamp(),
 updated_at timestamptz NOT NULL DEFAULT clock_timestamp(),
 PRIMARY KEY (domain,source_ref,lease_token)
);
ALTER TABLE public.cross_domain_delete_attempt OWNER TO v5_fn_owner;
REVOKE ALL ON public.cross_domain_delete_attempt FROM PUBLIC;

CREATE FUNCTION public.fn_cross_delete_begin(p_domain text,p_ref text,p_token uuid,p_key text,p_locator text,p_container text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
 IF p_domain NOT IN ('ordinary','enterprise','direct','rescan') OR p_ref IS NULL OR p_ref='' OR
    p_token IS NULL OR p_key IS NULL OR p_key='' THEN
  RAISE EXCEPTION 'invalid delete generation' USING ERRCODE='22023';
 END IF;
 INSERT INTO public.cross_domain_delete_attempt(domain,source_ref,lease_token,physical_key,locator_sha256,storage_container,state)
 VALUES(p_domain,p_ref,p_token,p_key,p_locator,p_container,'pending');
END $$;
ALTER FUNCTION public.fn_cross_delete_begin(text,text,uuid,text,text,text) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_cross_delete_begin(text,text,uuid,text,text,text) FROM PUBLIC;

CREATE FUNCTION public.fn_cross_delete_finish(p_domain text,p_ref text,p_token uuid,p_success boolean)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
 IF p_domain IS NULL OR p_ref IS NULL OR p_ref='' OR p_token IS NULL OR p_success IS NULL THEN
  RAISE EXCEPTION 'invalid delete generation outcome' USING ERRCODE='22023';
 END IF;
 UPDATE public.cross_domain_delete_attempt SET state=CASE WHEN p_success IS TRUE THEN 'settled' ELSE 'unknown' END,
  updated_at=clock_timestamp() WHERE domain=p_domain AND source_ref=p_ref AND lease_token=p_token AND state='pending';
 IF NOT FOUND THEN RAISE EXCEPTION 'delete generation unavailable' USING ERRCODE='23510'; END IF;
END $$;
ALTER FUNCTION public.fn_cross_delete_finish(text,text,uuid,boolean) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_cross_delete_finish(text,text,uuid,boolean) FROM PUBLIC;

CREATE FUNCTION public.fn_cross_delete_unresolved(p_key text,p_locator text,p_container text)
RETURNS boolean LANGUAGE sql VOLATILE SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
 SELECT p_key IS NULL OR p_key='' OR EXISTS (
  SELECT 1 FROM public.cross_domain_delete_attempt a WHERE a.physical_key=p_key AND a.state<>'settled'
   AND (a.locator_sha256 IS NULL OR a.locator_sha256 !~ '^[0-9a-f]{64}$' OR p_locator IS NULL OR p_locator !~ '^[0-9a-f]{64}$' OR a.locator_sha256=p_locator)
   AND (a.storage_container IS NULL OR a.storage_container='' OR p_container IS NULL OR p_container='' OR a.storage_container=p_container))
$$;
ALTER FUNCTION public.fn_cross_delete_unresolved(text,text,text) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_cross_delete_unresolved(text,text,text) FROM PUBLIC;

-- Existing attempts may hide a previous timed-out DELETE even when a later
-- attempt reached done; retain a conservative unknown generation.
INSERT INTO public.cross_domain_delete_attempt(domain,source_ref,lease_token,physical_key,locator_sha256,storage_container,state)
 SELECT 'ordinary',q.path,gen_random_uuid(),coalesce(q.storage_physical_key,q.path),q.storage_locator_sha256,q.storage_container,'unknown'
 FROM public.attachment_object_deletion q WHERE q.attempts>0 OR q.job_state<>'ready'
 UNION ALL SELECT 'enterprise',q.organization_id::text||'/'||q.workspace_id::text||'/'||q.asset_id::text,gen_random_uuid(),
 coalesce(q.storage_physical_key,q.object_key),q.storage_locator_sha256,q.storage_container,'unknown'
 FROM public.enterprise_asset_delete_queue q WHERE q.attempts>0 OR q.job_state<>'ready'
 UNION ALL SELECT 'direct',q.id::text,gen_random_uuid(),q.physical_key,q.locator_sha256,q.target_container,'unknown'
 FROM public.attach_direct_cleanup q WHERE q.attempts>0 OR q.job_state<>'ready';
INSERT INTO public.cross_domain_delete_attempt(domain,source_ref,lease_token,physical_key,locator_sha256,storage_container,state)
 SELECT 'ordinary',q.path,q.lease_token,coalesce(q.storage_physical_key,q.path),q.storage_locator_sha256,q.storage_container,'pending'
 FROM public.attachment_object_deletion q WHERE q.job_state='leased' AND q.lease_token IS NOT NULL
 UNION ALL SELECT 'enterprise',q.organization_id::text||'/'||q.workspace_id::text||'/'||q.asset_id::text,q.lease_token,
 coalesce(q.storage_physical_key,q.object_key),q.storage_locator_sha256,q.storage_container,'pending'
 FROM public.enterprise_asset_delete_queue q WHERE q.job_state='leased' AND q.lease_token IS NOT NULL
 UNION ALL SELECT 'direct',q.id::text,q.lease_token,q.physical_key,q.locator_sha256,q.target_container,'pending'
 FROM public.attach_direct_cleanup q WHERE q.job_state='leased' AND q.lease_token IS NOT NULL;

GRANT SELECT (object_key, source_key, target_key, target_driver, target_namespace, target_container, attempt_state)
    ON public.attach_direct_attempt TO v5_fn_owner;
CREATE FUNCTION public.fn_cross_domain_live_reference(
    p_key text, p_locator text, p_container text, p_domain text, p_owner text
) RETURNS boolean LANGUAGE sql VOLATILE SECURITY DEFINER
SET search_path = pg_catalog, public, pg_temp AS $$
    SELECT p_key IS NULL OR p_key = '' OR p_locator IS NULL OR p_locator !~ '^[0-9a-f]{64}$'
        OR p_container IS NULL OR p_container = '' OR p_domain NOT IN ('ordinary','enterprise','direct')
        OR p_domain IS NULL OR (p_domain <> 'direct' AND (p_owner IS NULL OR p_owner = ''))
        OR EXISTS (
        SELECT 1 FROM public.attachment a
        WHERE coalesce(a.storage_physical_key, a.path) = p_key
          AND (a.storage_locator_sha256 IS NULL OR a.storage_locator_sha256 !~ '^[0-9a-f]{64}$' OR a.storage_locator_sha256 = p_locator)
          AND (a.storage_container IS NULL OR a.storage_container = '' OR p_container IS NULL OR p_container = '' OR a.storage_container = p_container)
          AND a.lifecycle_state = 'live'
    ) OR EXISTS (
        SELECT 1 FROM public.enterprise_asset a
        WHERE coalesce(a.storage_physical_key, a.object_key) = p_key
          AND (a.storage_locator_sha256 IS NULL OR a.storage_locator_sha256 !~ '^[0-9a-f]{64}$' OR a.storage_locator_sha256 = p_locator)
          AND (a.storage_container IS NULL OR a.storage_container = '' OR p_container IS NULL OR p_container = '' OR a.storage_container = p_container)
          AND a.lifecycle_state = 'live'
    ) OR EXISTS (
        SELECT 1 FROM public.attachment a
        WHERE coalesce(a.storage_physical_key, a.path) = p_key
          AND (a.storage_locator_sha256 IS NULL OR a.storage_locator_sha256 !~ '^[0-9a-f]{64}$' OR a.storage_locator_sha256 = p_locator)
          AND (a.storage_container IS NULL OR a.storage_container = '' OR a.storage_container = p_container)
          AND (public.fn_talk_record_war_blocks(a.id) OR EXISTS (
              SELECT 1 FROM public.enterprise_attachment_retention ear WHERE ear.attachment_id=a.id
                AND (ear.hold_state='held' OR ear.purge_state IS DISTINCT FROM 'purged'
                     OR ear.retention_until>clock_timestamp())))
    ) OR EXISTS (
        SELECT 1 FROM public.enterprise_asset a
        WHERE coalesce(a.storage_physical_key, a.object_key) = p_key
          AND (a.storage_locator_sha256 IS NULL OR a.storage_locator_sha256 !~ '^[0-9a-f]{64}$' OR a.storage_locator_sha256 = p_locator)
          AND (a.storage_container IS NULL OR a.storage_container = '' OR a.storage_container = p_container)
          AND (a.retain_until>clock_timestamp() OR EXISTS (
              SELECT 1 FROM public.enterprise_retention_hold h
               WHERE h.organization_id=a.organization_id AND h.workspace_id=a.workspace_id
                 AND h.released_at IS NULL AND (h.scope_type='workspace'
                   OR (h.scope_type='conversation' AND h.scope_conversation_id=a.conversation_id)
                   OR (h.scope_type='message' AND h.scope_message_id=a.message_id))))
    ) OR EXISTS (
        SELECT 1 FROM public.enterprise_asset_delete_queue q
        JOIN public.enterprise_retention_hold h ON h.organization_id=q.organization_id AND h.workspace_id=q.workspace_id
        WHERE coalesce(q.storage_physical_key,q.object_key)=p_key
          AND (q.storage_locator_sha256 IS NULL OR q.storage_locator_sha256 !~ '^[0-9a-f]{64}$' OR q.storage_locator_sha256=p_locator)
          AND (q.storage_container IS NULL OR q.storage_container='' OR q.storage_container=p_container)
          AND h.released_at IS NULL AND (h.scope_type='workspace' OR NOT q.cross_scope_known
             OR (h.scope_type='conversation' AND h.scope_conversation_id=q.cross_conversation_id)
             OR (h.scope_type='message' AND h.scope_message_id=q.cross_message_id))
    ) OR EXISTS (
        SELECT 1 FROM public.attachment_object_deletion q
        JOIN public.enterprise_direct_retirement_audit d ON d.object_key=q.path
        JOIN public.enterprise_retention_hold h ON h.organization_id=d.organization_id AND h.workspace_id=d.workspace_id
        WHERE coalesce(q.storage_physical_key,q.path)=p_key
          AND (q.storage_locator_sha256 IS NULL OR q.storage_locator_sha256 !~ '^[0-9a-f]{64}$' OR q.storage_locator_sha256=p_locator)
          AND (q.storage_container IS NULL OR q.storage_container='' OR q.storage_container=p_container)
          AND h.released_at IS NULL AND (h.scope_type='workspace' OR NOT d.cross_scope_known
            OR (h.scope_type='conversation' AND h.scope_conversation_id=d.cross_conversation_id)
            OR (h.scope_type='message' AND h.scope_message_id=d.cross_message_id))
    ) OR EXISTS (
        SELECT 1 FROM public.attach_pending p
        WHERE p.object_key = p_key
          AND (p.storage_locator_sha256 IS NULL OR p.storage_locator_sha256 !~ '^[0-9a-f]{64}$' OR p.storage_locator_sha256 = p_locator)
          AND (p.storage_container IS NULL OR p.storage_container = '' OR p_container IS NULL OR p_container = '' OR p.storage_container = p_container)
          AND p.session_state = 'registered'
          AND (p.upload_expires_at IS NULL OR p.upload_expires_at > clock_timestamp())
    ) OR EXISTS (
        SELECT 1 FROM public.attach_direct_attempt a
        JOIN public.attach_pending p ON p.object_key = a.object_key
        WHERE p_key IN (a.source_key, a.target_key)
          AND (a.target_driver IS DISTINCT FROM p.storage_driver OR a.target_namespace IS DISTINCT FROM p.storage_namespace OR a.target_container IS DISTINCT FROM p.storage_container OR p.storage_locator_sha256 IS NULL OR p.storage_locator_sha256 !~ '^[0-9a-f]{64}$' OR p.storage_locator_sha256 = p_locator)
          AND (a.target_container IS NULL OR a.target_container = '' OR p_container IS NULL OR p_container = '' OR a.target_container = p_container)
          AND (a.attempt_state = 'WINNER' OR
               (p.direct_state IN ('RESERVED', 'DECLARED', 'GRANTED', 'FINALIZING')
                AND a.attempt_state <> 'GC_CLAIMED'))
          AND NOT (p_domain IN ('ordinary', 'enterprise') AND a.object_key = p_owner
                   AND a.attempt_state = 'WINNER' AND a.target_key = p_key)
    )
$$;
ALTER FUNCTION public.fn_cross_domain_live_reference(text,text,text,text,text) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_cross_domain_live_reference(text,text,text,text,text) FROM PUBLIC;

GRANT SELECT (physical_key,target_container,locator_sha256) ON public.attach_direct_cleanup TO v5_fn_owner;
CREATE FUNCTION public.fn_cross_domain_key_guard(p_key text,p_locator text,p_container text)
RETURNS void LANGUAGE plpgsql VOLATILE SECURITY DEFINER
SET search_path=pg_catalog,public,pg_temp AS $$
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
END $$;
ALTER FUNCTION public.fn_cross_domain_key_guard(text,text,text) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_cross_domain_key_guard(text,text,text) FROM PUBLIC;

CREATE FUNCTION public.fn_cross_domain_resource_guard() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE v_key text; v_locator text; v_container text; v_other text; v_attempt record;
BEGIN
 IF TG_TABLE_NAME='attachment' THEN
  IF TG_OP='UPDATE' THEN
   IF (OLD.path,OLD.storage_physical_key,OLD.storage_driver,OLD.storage_namespace,OLD.storage_locator_sha256,OLD.storage_container)
      IS DISTINCT FROM (NEW.path,NEW.storage_physical_key,NEW.storage_driver,NEW.storage_namespace,NEW.storage_locator_sha256,NEW.storage_container) THEN
    PERFORM public.fn_cross_domain_key_guard(coalesce(OLD.storage_physical_key,OLD.path),OLD.storage_locator_sha256,OLD.storage_container);
   END IF;
  END IF;
  IF NEW.lifecycle_state <> 'live' THEN
   IF TG_OP='UPDATE' THEN
    IF (NEW.scope,NEW.scope_ref) IS NOT DISTINCT FROM (OLD.scope,OLD.scope_ref) THEN RETURN NEW; END IF;
   END IF;
  END IF;
  v_key:=coalesce(NEW.storage_physical_key,NEW.path);
  v_locator:=NEW.storage_locator_sha256; v_container:=NEW.storage_container;
 ELSIF TG_TABLE_NAME='enterprise_asset' THEN
  IF TG_OP='UPDATE' THEN
   IF (OLD.object_key,OLD.storage_physical_key,OLD.storage_driver,OLD.storage_namespace,OLD.storage_locator_sha256,OLD.storage_container)
      IS DISTINCT FROM (NEW.object_key,NEW.storage_physical_key,NEW.storage_driver,NEW.storage_namespace,NEW.storage_locator_sha256,NEW.storage_container) THEN
    PERFORM public.fn_cross_domain_key_guard(coalesce(OLD.storage_physical_key,OLD.object_key),OLD.storage_locator_sha256,OLD.storage_container);
   END IF;
  END IF;
  IF NEW.lifecycle_state <> 'live' THEN
   IF TG_OP='UPDATE' THEN
    IF (NEW.organization_id,NEW.workspace_id,NEW.conversation_id,NEW.message_id,NEW.retain_until)
       IS NOT DISTINCT FROM (OLD.organization_id,OLD.workspace_id,OLD.conversation_id,OLD.message_id,OLD.retain_until) THEN RETURN NEW; END IF;
   END IF;
  END IF;
  v_key:=coalesce(NEW.storage_physical_key,NEW.object_key);
  v_locator:=NEW.storage_locator_sha256; v_container:=NEW.storage_container;
 ELSIF TG_TABLE_NAME='attach_pending' THEN
  IF TG_OP='UPDATE' THEN
   IF (OLD.object_key,OLD.storage_driver,OLD.storage_namespace,OLD.storage_locator_sha256,OLD.storage_container)
      IS DISTINCT FROM (NEW.object_key,NEW.storage_driver,NEW.storage_namespace,NEW.storage_locator_sha256,NEW.storage_container) THEN
    PERFORM public.fn_cross_domain_key_guard(OLD.object_key,OLD.storage_locator_sha256,OLD.storage_container);
   END IF;
  END IF;
  IF NEW.session_state <> 'registered' THEN RETURN NEW; END IF;
  v_key:=NEW.object_key; v_locator:=NEW.storage_locator_sha256; v_container:=NEW.storage_container;
 ELSE
  IF TG_OP='UPDATE' THEN
   IF (OLD.object_key,OLD.source_key,OLD.target_key,OLD.target_driver,OLD.target_namespace,OLD.target_container)
      IS DISTINCT FROM (NEW.object_key,NEW.source_key,NEW.target_key,NEW.target_driver,NEW.target_namespace,NEW.target_container) THEN
    SELECT CASE WHEN p.storage_driver=OLD.target_driver AND p.storage_namespace=OLD.target_namespace AND p.storage_container=OLD.target_container THEN p.storage_locator_sha256 ELSE NULL END INTO v_locator FROM public.attach_pending p WHERE p.object_key=OLD.object_key;
    PERFORM public.fn_cross_domain_key_guard(least(OLD.source_key,OLD.target_key),v_locator,OLD.target_container);
    IF OLD.source_key<>OLD.target_key THEN
     PERFORM public.fn_cross_domain_key_guard(greatest(OLD.source_key,OLD.target_key),v_locator,OLD.target_container);
    END IF;
   END IF;
  END IF;
  IF NEW.attempt_state IN ('GC_CLAIMED','REJECTED','ABANDONED') THEN RETURN NEW; END IF;
  SELECT CASE WHEN p.storage_driver=NEW.target_driver AND p.storage_namespace=NEW.target_namespace AND p.storage_container=NEW.target_container THEN p.storage_locator_sha256 ELSE NULL END INTO v_locator FROM public.attach_pending p WHERE p.object_key=NEW.object_key;
  v_container:=NEW.target_container;
  v_key:=least(NEW.source_key,NEW.target_key); v_other:=greatest(NEW.source_key,NEW.target_key);
 END IF;
 PERFORM public.fn_cross_domain_key_guard(v_key,v_locator,v_container);
 IF TG_TABLE_NAME='attach_pending' THEN
  FOR v_attempt IN SELECT source_key AS k,target_container AS c,CASE WHEN target_driver=NEW.storage_driver AND target_namespace=NEW.storage_namespace AND target_container=NEW.storage_container THEN v_locator ELSE NULL END AS loc FROM public.attach_direct_attempt WHERE object_key=NEW.object_key AND attempt_state NOT IN ('GC_CLAIMED','REJECTED','ABANDONED')
      UNION SELECT target_key,target_container,CASE WHEN target_driver=NEW.storage_driver AND target_namespace=NEW.storage_namespace AND target_container=NEW.storage_container THEN v_locator ELSE NULL END FROM public.attach_direct_attempt WHERE object_key=NEW.object_key AND attempt_state NOT IN ('GC_CLAIMED','REJECTED','ABANDONED') ORDER BY k
  LOOP
   PERFORM public.fn_cross_domain_key_guard(v_attempt.k,v_attempt.loc,v_attempt.c);
  END LOOP;
 END IF;
 IF v_other IS NOT NULL AND v_other<>v_key THEN
  PERFORM public.fn_cross_domain_key_guard(v_other,v_locator,v_container);
 END IF;
 RETURN NEW;
END $$;
ALTER FUNCTION public.fn_cross_domain_resource_guard() OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_cross_domain_resource_guard() FROM PUBLIC;
CREATE TRIGGER aa_cross_attachment_guard BEFORE INSERT OR UPDATE OF path,storage_physical_key,storage_driver,storage_namespace,storage_locator_sha256,storage_container,lifecycle_state,status,scope,scope_ref
 ON public.attachment FOR EACH ROW EXECUTE FUNCTION public.fn_cross_domain_resource_guard();
CREATE TRIGGER aa_cross_enterprise_guard BEFORE INSERT OR UPDATE OF object_key,storage_physical_key,storage_driver,storage_namespace,storage_locator_sha256,storage_container,lifecycle_state,status,upload_expires_at,organization_id,workspace_id,conversation_id,message_id,retain_until
 ON public.enterprise_asset FOR EACH ROW EXECUTE FUNCTION public.fn_cross_domain_resource_guard();
CREATE TRIGGER aa_cross_pending_guard BEFORE INSERT OR UPDATE OF object_key,storage_driver,storage_locator_sha256,storage_namespace,storage_container,session_state,direct_state,upload_expires_at
 ON public.attach_pending FOR EACH ROW EXECUTE FUNCTION public.fn_cross_domain_resource_guard();
CREATE TRIGGER aa_cross_attempt_guard BEFORE INSERT OR UPDATE OF object_key,source_key,target_key,target_driver,target_namespace,target_container,attempt_state
 ON public.attach_direct_attempt FOR EACH ROW EXECUTE FUNCTION public.fn_cross_domain_resource_guard();
CREATE FUNCTION public.fn_cross_domain_queue_lock() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE v_key text;
BEGIN
 IF TG_TABLE_NAME='attachment_object_deletion' THEN
  v_key:=coalesce(NEW.storage_physical_key,NEW.path);
 ELSIF TG_TABLE_NAME='enterprise_asset_delete_queue' THEN
  v_key:=coalesce(NEW.storage_physical_key,NEW.object_key);
 ELSE
  v_key:=NEW.physical_key;
 END IF;
 IF v_key IS NULL OR v_key='' THEN RAISE EXCEPTION 'physical key unavailable' USING ERRCODE='23510'; END IF;
 IF NOT pg_try_advisory_xact_lock(hashtextextended(v_key,1699)) THEN
  RAISE EXCEPTION 'physical key busy' USING ERRCODE='40001';
 END IF;
 RETURN NEW;
END $$;
ALTER FUNCTION public.fn_cross_domain_queue_lock() OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_cross_domain_queue_lock() FROM PUBLIC;
CREATE TRIGGER aa_cross_ordinary_queue_lock BEFORE INSERT ON public.attachment_object_deletion
 FOR EACH ROW EXECUTE FUNCTION public.fn_cross_domain_queue_lock();
CREATE TRIGGER aa_cross_enterprise_queue_lock BEFORE INSERT ON public.enterprise_asset_delete_queue
 FOR EACH ROW EXECUTE FUNCTION public.fn_cross_domain_queue_lock();
CREATE TRIGGER aa_cross_direct_queue_lock BEFORE INSERT ON public.attach_direct_cleanup
 FOR EACH ROW EXECUTE FUNCTION public.fn_cross_domain_queue_lock();
CREATE FUNCTION public.fn_cross_domain_queue_immutable() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
 IF TG_OP='DELETE' THEN RAISE EXCEPTION 'physical tombstone permanent' USING ERRCODE='23510'; END IF;
 IF TG_TABLE_NAME='attachment_object_deletion' THEN
  IF (NEW.path,NEW.storage_driver,NEW.storage_namespace,NEW.storage_container,NEW.storage_locator_sha256,NEW.storage_physical_key)
     IS DISTINCT FROM (OLD.path,OLD.storage_driver,OLD.storage_namespace,OLD.storage_container,OLD.storage_locator_sha256,OLD.storage_physical_key) THEN
   RAISE EXCEPTION 'physical tombstone immutable' USING ERRCODE='23510'; END IF;
 ELSIF TG_TABLE_NAME='enterprise_asset_delete_queue' THEN
  IF (NEW.object_key,NEW.storage_driver,NEW.storage_namespace,NEW.storage_container,NEW.storage_locator_sha256,NEW.storage_physical_key,NEW.cross_scope_known,NEW.cross_conversation_id,NEW.cross_message_id)
     IS DISTINCT FROM (OLD.object_key,OLD.storage_driver,OLD.storage_namespace,OLD.storage_container,OLD.storage_locator_sha256,OLD.storage_physical_key,OLD.cross_scope_known,OLD.cross_conversation_id,OLD.cross_message_id) THEN
   RAISE EXCEPTION 'physical tombstone immutable' USING ERRCODE='23510'; END IF;
 ELSE
  IF (NEW.object_key,NEW.physical_key,NEW.target_driver,NEW.target_namespace,NEW.target_container,NEW.locator_sha256)
     IS DISTINCT FROM (OLD.object_key,OLD.physical_key,OLD.target_driver,OLD.target_namespace,OLD.target_container,OLD.locator_sha256) THEN
   RAISE EXCEPTION 'physical tombstone immutable' USING ERRCODE='23510'; END IF;
 END IF;
 RETURN NEW;
END $$;
ALTER FUNCTION public.fn_cross_domain_queue_immutable() OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_cross_domain_queue_immutable() FROM PUBLIC;
CREATE TRIGGER aa_cross_ordinary_queue_immutable BEFORE DELETE OR UPDATE ON public.attachment_object_deletion
 FOR EACH ROW EXECUTE FUNCTION public.fn_cross_domain_queue_immutable();
CREATE TRIGGER aa_cross_enterprise_queue_immutable BEFORE DELETE OR UPDATE ON public.enterprise_asset_delete_queue
 FOR EACH ROW EXECUTE FUNCTION public.fn_cross_domain_queue_immutable();
CREATE TRIGGER aa_cross_direct_queue_immutable BEFORE DELETE OR UPDATE ON public.attach_direct_cleanup
 FOR EACH ROW EXECUTE FUNCTION public.fn_cross_domain_queue_immutable();
CREATE FUNCTION public.fn_cross_domain_claim_guard() RETURNS trigger
LANGUAGE plpgsql VOLATILE SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE v_key text; v_locator text; v_container text; v_owner text; v_domain text; v_ref text;
BEGIN
 IF NEW.job_state <> 'leased' THEN RETURN NEW; END IF;
 IF TG_TABLE_NAME='attachment_object_deletion' THEN
  v_key:=coalesce(NEW.storage_physical_key,NEW.path); v_locator:=NEW.storage_locator_sha256;
  v_container:=NEW.storage_container; v_owner:=NEW.path; v_domain:='ordinary'; v_ref:=NEW.path;
 ELSIF TG_TABLE_NAME='enterprise_asset_delete_queue' THEN
  v_key:=coalesce(NEW.storage_physical_key,NEW.object_key); v_locator:=NEW.storage_locator_sha256;
  v_container:=NEW.storage_container; v_owner:=NEW.object_key; v_domain:='enterprise';
  v_ref:=NEW.organization_id::text||'/'||NEW.workspace_id::text||'/'||NEW.asset_id::text;
 ELSE
  v_key:=NEW.physical_key; v_locator:=NEW.locator_sha256;
  v_container:=NEW.target_container; v_owner:=NEW.object_key; v_domain:='direct'; v_ref:=NEW.id::text;
 END IF;
 IF v_key IS NULL OR v_key='' THEN RAISE EXCEPTION 'physical identity unavailable' USING ERRCODE='23510'; END IF;
 IF NOT pg_try_advisory_xact_lock(hashtextextended(v_key,1699)) THEN
  RAISE EXCEPTION 'physical key busy' USING ERRCODE='40001';
 END IF;
 IF public.fn_cross_domain_live_reference(v_key,v_locator,v_container,v_domain,v_owner) THEN
  RAISE EXCEPTION 'physical key still referenced or identity unavailable' USING ERRCODE='23510';
 END IF;
 IF OLD.job_state='leased' AND OLD.lease_token IS DISTINCT FROM NEW.lease_token THEN
  PERFORM public.fn_cross_delete_finish(v_domain,v_ref,OLD.lease_token,false);
 END IF;
 PERFORM public.fn_cross_delete_begin(v_domain,v_ref,NEW.lease_token,v_key,v_locator,v_container);
 RETURN NEW;
END $$;
ALTER FUNCTION public.fn_cross_domain_claim_guard() OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_cross_domain_claim_guard() FROM PUBLIC;
CREATE TRIGGER aa_cross_ordinary_claim_guard BEFORE UPDATE OF job_state ON public.attachment_object_deletion
 FOR EACH ROW EXECUTE FUNCTION public.fn_cross_domain_claim_guard();
CREATE TRIGGER aa_cross_enterprise_claim_guard BEFORE UPDATE OF job_state ON public.enterprise_asset_delete_queue
 FOR EACH ROW EXECUTE FUNCTION public.fn_cross_domain_claim_guard();
CREATE TRIGGER aa_cross_direct_claim_guard BEFORE UPDATE OF job_state ON public.attach_direct_cleanup
 FOR EACH ROW EXECUTE FUNCTION public.fn_cross_domain_claim_guard();
CREATE FUNCTION public.fn_cross_domain_claim_finish() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE v_domain text; v_ref text;
BEGIN
 IF OLD.job_state<>'leased' OR NEW.job_state='leased' THEN RETURN NEW; END IF;
 IF TG_TABLE_NAME='attachment_object_deletion' THEN
  v_domain:='ordinary'; v_ref:=OLD.path;
 ELSIF TG_TABLE_NAME='enterprise_asset_delete_queue' THEN
  v_domain:='enterprise'; v_ref:=OLD.organization_id::text||'/'||OLD.workspace_id::text||'/'||OLD.asset_id::text;
 ELSE
  v_domain:='direct'; v_ref:=OLD.id::text;
 END IF;
 PERFORM public.fn_cross_delete_finish(v_domain,v_ref,OLD.lease_token,NEW.job_state='done');
 RETURN NEW;
END $$;
ALTER FUNCTION public.fn_cross_domain_claim_finish() OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_cross_domain_claim_finish() FROM PUBLIC;
CREATE TRIGGER aa_cross_ordinary_claim_finish AFTER UPDATE OF job_state ON public.attachment_object_deletion
 FOR EACH ROW EXECUTE FUNCTION public.fn_cross_domain_claim_finish();
CREATE TRIGGER aa_cross_enterprise_claim_finish AFTER UPDATE OF job_state ON public.enterprise_asset_delete_queue
 FOR EACH ROW EXECUTE FUNCTION public.fn_cross_domain_claim_finish();
CREATE TRIGGER aa_cross_direct_claim_finish AFTER UPDATE OF job_state ON public.attach_direct_cleanup
 FOR EACH ROW EXECUTE FUNCTION public.fn_cross_domain_claim_finish();
-- Skip currently shared physical identities instead of repeatedly selecting them.
DO $migration$
DECLARE d text; old text; new text;
BEGIN
 SELECT pg_get_functiondef('public.fn_v5_claim_ordinary(uuid)'::regprocedure) INTO d;
 old:='SELECT path FROM public.attachment_object_deletion' || E'\n' || '                        WHERE attempts-attempt_base < 8';
 new:='SELECT x.path FROM public.attachment_object_deletion x' || E'\n' || '                        WHERE NOT public.fn_cross_domain_live_reference(coalesce(x.storage_physical_key,x.path),x.storage_locator_sha256,x.storage_container,''ordinary'',x.path) AND attempts-attempt_base < 8';
 IF position(old IN d)=0 THEN RAISE EXCEPTION 'unexpected ordinary claim contract'; END IF;
 EXECUTE replace(d,old,new);
 SELECT pg_get_functiondef('public.fn_v5_claim_eb(uuid)'::regprocedure) INTO d;
 old:='FROM enterprise_asset_delete_queue' || E'\n' || '                  WHERE attempts < 8';
 new:='FROM enterprise_asset_delete_queue x' || E'\n' || '                  WHERE NOT public.fn_cross_domain_live_reference(coalesce(x.storage_physical_key,x.object_key),x.storage_locator_sha256,x.storage_container,''enterprise'',x.object_key) AND attempts < 8';
 IF position(old IN d)=0 THEN RAISE EXCEPTION 'unexpected enterprise claim contract'; END IF;
 EXECUTE replace(d,old,new);
 SELECT pg_get_functiondef('public.fn_v5_claim_eb(uuid,bigint,bigint)'::regprocedure) INTO d;
 old:='WHERE q.organization_id = p_org AND q.workspace_id = p_ws';
 new:=old || E'\n' || '          AND NOT public.fn_cross_domain_live_reference(coalesce(q.storage_physical_key,q.object_key),q.storage_locator_sha256,q.storage_container,''enterprise'',q.object_key)';
 IF position(old IN d)=0 THEN RAISE EXCEPTION 'unexpected scoped claim contract'; END IF;
 EXECUTE replace(d,old,new);
 SELECT pg_get_functiondef('public.fn_dv1_tombstone_claim(uuid)'::regprocedure) INTO d;
 old:='WHERE c.locator_sha256 IS NOT NULL AND c.next_scan_at <= clock_timestamp()';
 new:=old || E'\n' || ' AND NOT public.fn_cross_domain_live_reference(c.physical_key,c.locator_sha256,c.target_container,''direct'',c.object_key)';
 IF position(old IN d)=0 THEN RAISE EXCEPTION 'unexpected direct claim contract'; END IF;
 d:=replace(d,old,new);
 old:=$legacy$ AND NOT EXISTS (SELECT 1 FROM public.attach_direct_attempt a
   WHERE a.target_key=c.physical_key AND a.attempt_state='WINNER')
 AND NOT EXISTS (SELECT 1 FROM public.attach_direct_attempt a
   JOIN public.attach_pending p ON p.object_key=a.object_key
   WHERE c.physical_key IN (a.source_key,a.target_key)
     AND p.direct_state IN ('RESERVED','DECLARED','GRANTED','FINALIZING')
     AND a.attempt_state <> 'GC_CLAIMED')
 AND NOT EXISTS (SELECT 1 FROM public.attachment a
   WHERE a.storage_physical_key=c.physical_key AND a.lifecycle_state <> 'deleted')
$legacy$;
 IF position(old IN d)=0 THEN RAISE EXCEPTION 'unexpected direct legacy guards'; END IF;
 EXECUTE replace(d,old,'');
END $migration$;
-- Scope holds must not become effective after a covered asset entered deletion.
CREATE FUNCTION public.fn_cross_enterprise_hold_guard() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE v_asset record;
BEGIN
 IF NEW.released_at IS NOT NULL THEN RETURN NEW; END IF;
 PERFORM 1 FROM public.workspace w
  WHERE w.organization_id=NEW.organization_id AND w.id=NEW.workspace_id FOR UPDATE;
 IF NOT FOUND THEN RAISE EXCEPTION 'hold workspace unavailable' USING ERRCODE='23510'; END IF;
 FOR v_asset IN
  SELECT coalesce(a.storage_physical_key,a.object_key) AS k,a.storage_locator_sha256 AS l,a.storage_container AS c,
   a.lifecycle_state='live' AS is_live,
   EXISTS (SELECT 1 FROM public.enterprise_asset_delete_queue q
    WHERE q.organization_id=a.organization_id AND q.workspace_id=a.workspace_id AND q.asset_id=a.id
     AND q.job_state IN ('ready','retry_wait','leased','blocked')) AS pending_queue
  FROM public.enterprise_asset a
  WHERE a.organization_id=NEW.organization_id AND a.workspace_id=NEW.workspace_id
   AND (NEW.scope_type='workspace'
     OR (NEW.scope_type='conversation' AND NEW.scope_conversation_id=a.conversation_id)
     OR (NEW.scope_type='message' AND NEW.scope_message_id=a.message_id))
  ORDER BY coalesce(a.storage_physical_key,a.object_key) COLLATE "C",a.id
 LOOP
  IF NOT pg_try_advisory_xact_lock(hashtextextended(v_asset.k,1699)) THEN
   RAISE EXCEPTION 'physical key busy' USING ERRCODE='40001';
  END IF;
  IF public.fn_cross_delete_unresolved(v_asset.k,v_asset.l,v_asset.c) THEN
   RAISE EXCEPTION 'physical delete outcome unresolved' USING ERRCODE='23510';
  END IF;
  IF v_asset.is_live OR v_asset.pending_queue THEN
   PERFORM public.fn_cross_domain_key_guard(v_asset.k,v_asset.l,v_asset.c);
  END IF;
 END LOOP;
 -- Direct enterprise retirements use ordinary queue, whose scope is frozen in audit.
 FOR v_asset IN
  SELECT coalesce(q.storage_physical_key,q.path) AS k,q.storage_locator_sha256 AS l,q.storage_container AS c,
   q.job_state IN ('ready','retry_wait','leased','blocked') AS pending_queue
  FROM public.enterprise_direct_retirement_audit d JOIN public.attachment_object_deletion q ON q.path=d.object_key
  WHERE d.organization_id=NEW.organization_id AND d.workspace_id=NEW.workspace_id
   AND (NEW.scope_type='workspace' OR NOT d.cross_scope_known
    OR (NEW.scope_type='conversation' AND NEW.scope_conversation_id=d.cross_conversation_id)
    OR (NEW.scope_type='message' AND NEW.scope_message_id=d.cross_message_id))
  ORDER BY coalesce(q.storage_physical_key,q.path) COLLATE "C",d.asset_id
 LOOP
  IF v_asset.k IS NULL OR v_asset.k='' THEN RAISE EXCEPTION 'physical identity unavailable' USING ERRCODE='23510'; END IF;
  IF NOT pg_try_advisory_xact_lock(hashtextextended(v_asset.k,1699)) THEN RAISE EXCEPTION 'physical key busy' USING ERRCODE='40001'; END IF;
  IF public.fn_cross_delete_unresolved(v_asset.k,v_asset.l,v_asset.c) THEN RAISE EXCEPTION 'physical delete outcome unresolved' USING ERRCODE='23510'; END IF;
  IF v_asset.pending_queue THEN PERFORM public.fn_cross_domain_key_guard(v_asset.k,v_asset.l,v_asset.c); END IF;
 END LOOP;
 -- Queue survives asset deletion; unknown historical scopes match all holds.
 FOR v_asset IN
  SELECT coalesce(q.storage_physical_key,q.object_key) AS k,q.storage_locator_sha256 AS l,q.storage_container AS c,
   q.job_state IN ('ready','retry_wait','leased','blocked') AS pending_queue
  FROM public.enterprise_asset_delete_queue q
  WHERE q.organization_id=NEW.organization_id AND q.workspace_id=NEW.workspace_id
   AND (NEW.scope_type='workspace' OR NOT q.cross_scope_known
     OR (NEW.scope_type='conversation' AND NEW.scope_conversation_id=q.cross_conversation_id)
     OR (NEW.scope_type='message' AND NEW.scope_message_id=q.cross_message_id))
  ORDER BY coalesce(q.storage_physical_key,q.object_key) COLLATE "C",q.asset_id
 LOOP
  IF v_asset.k IS NULL OR v_asset.k='' THEN RAISE EXCEPTION 'physical identity unavailable' USING ERRCODE='23510'; END IF;
  IF NOT pg_try_advisory_xact_lock(hashtextextended(v_asset.k,1699)) THEN
   RAISE EXCEPTION 'physical key busy' USING ERRCODE='40001';
  END IF;
  IF public.fn_cross_delete_unresolved(v_asset.k,v_asset.l,v_asset.c) THEN
   RAISE EXCEPTION 'physical delete outcome unresolved' USING ERRCODE='23510';
  END IF;
  IF v_asset.pending_queue THEN PERFORM public.fn_cross_domain_key_guard(v_asset.k,v_asset.l,v_asset.c); END IF;
 END LOOP;
 RETURN NEW;
END $$;
ALTER FUNCTION public.fn_cross_enterprise_hold_guard() OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_cross_enterprise_hold_guard() FROM PUBLIC;
CREATE TRIGGER aa_cross_enterprise_hold_guard BEFORE INSERT OR UPDATE OF scope_type,scope_conversation_id,scope_message_id,released_at
 ON public.enterprise_retention_hold FOR EACH ROW EXECUTE FUNCTION public.fn_cross_enterprise_hold_guard();
