-- Explicit talk_record-only recovery; never reset lifetime attempts or keys.
SET LOCAL lock_timeout='5s';
ALTER TABLE public.attachment_object_deletion ADD COLUMN retry_generation bigint NOT NULL DEFAULT 0,
 ADD COLUMN attempt_base integer NOT NULL DEFAULT 0,
 ADD CONSTRAINT ck_aod_retry_generation CHECK(retry_generation>=0 AND attempt_base>=0 AND attempt_base<=attempts);
ALTER TABLE talk_record ADD COLUMN cleanup_generation bigint NOT NULL DEFAULT 0
 CHECK(cleanup_generation>=0);
CREATE TABLE public.talk_record_cleanup_retry (
 record_id bigint NOT NULL REFERENCES talk_record(id) ON DELETE RESTRICT,
 actor_user_id bigint NOT NULL, request_key text NOT NULL, payload_hash text NOT NULL,
 generation bigint NOT NULL, reason text NOT NULL, resources jsonb NOT NULL,
 response jsonb NOT NULL, created_at timestamptz NOT NULL DEFAULT now(),
 PRIMARY KEY(record_id,actor_user_id,request_key), UNIQUE(record_id,generation)
);
COMMENT ON TABLE public.talk_record_cleanup_retry IS 'Permanent metadata-only audit/idempotency of scoped cleanup recovery; no transcript, credentials or storage URLs. Actor retained as audit identity, no new user FK.';
REVOKE ALL ON public.talk_record_cleanup_retry FROM PUBLIC;
DO $$ BEGIN
 IF NOT EXISTS(SELECT 1 FROM pg_roles WHERE rolname='talk_record_retry_definer') THEN
  CREATE ROLE talk_record_retry_definer NOLOGIN NOINHERIT NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS;
 END IF;
 IF EXISTS(SELECT 1 FROM pg_auth_members m JOIN pg_roles p ON p.oid=m.roleid OR p.oid=m.member
  WHERE p.rolname='talk_record_retry_definer') THEN
  RAISE EXCEPTION 'retry definer must have no members'; END IF;
 IF EXISTS(SELECT 1 FROM pg_roles WHERE rolname='talk_record_retry_definer'
  AND (rolcanlogin OR rolinherit OR rolsuper OR rolcreaterole OR rolcreatedb OR rolreplication OR rolbypassrls)) THEN
  RAISE EXCEPTION 'retry definer unsafe attributes'; END IF;
END $$;
GRANT USAGE ON SCHEMA public TO talk_record_retry_definer;
GRANT SELECT ON organization,organization_member,workspace,workspace_member,attachment,
 enterprise_attachment_retention,workspace_attachment_retention,attach_pending,group_file,
 user_collect,moment_post,talk_record_attachment_ref,attachment_derived_write,
 talk_record_processor_run,talk_record_job TO talk_record_retry_definer;
-- Row locks need UPDATE rights on the locked columns, not arbitrary write rights.
GRANT UPDATE(id) ON organization,attachment TO talk_record_retry_definer;
GRANT UPDATE(user_id) ON organization_member TO talk_record_retry_definer;
GRANT UPDATE(attachment_id) ON enterprise_attachment_retention,workspace_attachment_retention TO talk_record_retry_definer;
GRANT SELECT,UPDATE ON public.attachment_object_deletion TO talk_record_retry_definer;
GRANT SELECT,UPDATE(cleanup_generation,revision,cleanup_status,updated_at) ON talk_record TO talk_record_retry_definer;
GRANT SELECT,INSERT ON public.talk_record_cleanup_retry TO talk_record_retry_definer;
GRANT INSERT ON talk_record_job TO talk_record_retry_definer;
GRANT SELECT,INSERT ON public.attachment_lifecycle_v5_audit TO talk_record_retry_definer;
GRANT USAGE ON SEQUENCE attachment_lifecycle_v5_audit_id_seq TO talk_record_retry_definer;
GRANT EXECUTE ON FUNCTION public.fn_talk_record_war_blocks(bigint) TO talk_record_retry_definer;
GRANT EXECUTE ON FUNCTION public.fn_v5_lock_identity(text,text,text,text) TO talk_record_retry_definer;

CREATE FUNCTION public.fn_talk_record_cleanup_retry_audit_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN RAISE EXCEPTION 'cleanup retry audit is immutable' USING ERRCODE='23510'; END $$;
CREATE TRIGGER trg_talk_record_cleanup_retry_audit BEFORE UPDATE OR DELETE ON public.talk_record_cleanup_retry
 FOR EACH ROW EXECUTE FUNCTION public.fn_talk_record_cleanup_retry_audit_guard();

CREATE FUNCTION public.fn_talk_record_retry_budget_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
 IF TG_OP='INSERT' THEN
  IF NEW.retry_generation<>0 OR NEW.attempt_base<>0 THEN
   RAISE EXCEPTION 'retry_budget_unauthorized' USING ERRCODE='23510'; END IF;
  RETURN NEW;
 END IF;
 IF NEW.retry_generation IS DISTINCT FROM OLD.retry_generation OR NEW.attempt_base IS DISTINCT FROM OLD.attempt_base THEN
  IF current_user<>'talk_record_retry_definer' OR OLD.job_state<>'blocked'
   OR OLD.last_error_code IS DISTINCT FROM 'max_attempts'
   OR OLD.completed_at IS NOT NULL OR OLD.lease_token IS NOT NULL OR OLD.lease_until IS NOT NULL
   OR NEW.retry_generation<>OLD.retry_generation+1 OR NEW.attempt_base<>OLD.attempts
   OR NEW.attempts<>OLD.attempts OR NEW.job_state<>'ready'
   OR NEW.completed_at IS NOT NULL OR NEW.lease_token IS NOT NULL OR NEW.lease_until IS NOT NULL THEN
   RAISE EXCEPTION 'retry_budget_unauthorized' USING ERRCODE='23510';
  END IF;
 END IF;
 RETURN NEW;
END $$;
CREATE TRIGGER trg_talk_record_retry_budget BEFORE INSERT OR UPDATE ON public.attachment_object_deletion
 FOR EACH ROW EXECUTE FUNCTION public.fn_talk_record_retry_budget_guard();


-- One readonly eligibility source for capability projection and locked mutation.
-- Not exposed to application callers: the resource plan contains frozen identities.
CREATE FUNCTION public.fn_talk_record_retry_plan(p_record bigint) RETURNS jsonb
LANGUAGE plpgsql STABLE SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE r public.talk_record%ROWTYPE; a public.attachment%ROWTYPE;
 q public.attachment_object_deletion%ROWTYPE; resources jsonb:='[]'; protected_count integer:=0; has_queue boolean;
BEGIN
 SELECT * INTO r FROM public.talk_record WHERE id=p_record;
 IF NOT FOUND OR r.deleted_at IS NULL THEN RETURN jsonb_build_object('error','record_not_deleted'); END IF;
 IF r.cleanup_status NOT IN ('failed','pending','retained_by_other_reference','blocked_hold','blocked_retention') THEN
  RETURN jsonb_build_object('error','cleanup_not_retryable'); END IF;
 IF r.retention_until>now() THEN RETURN jsonb_build_object('error','cleanup_protected'); END IF;
 IF EXISTS(SELECT 1 FROM public.talk_record_processor_run pr JOIN public.talk_record_job j ON j.id=pr.job_id
  WHERE j.record_id=p_record AND pr.state IN ('prepared','running','unknown')) THEN
  RETURN jsonb_build_object('error','cleanup_identity_unverified'); END IF;
 FOR a IN SELECT * FROM public.attachment WHERE id IN (
  SELECT attachment_id FROM public.talk_record_attachment_ref WHERE record_id=p_record
  UNION SELECT dw.attachment_id FROM public.attachment_derived_write dw
   JOIN public.talk_record_job j ON j.id=dw.source_job_id WHERE j.record_id=p_record)
  ORDER BY path LOOP
  IF EXISTS(SELECT 1 FROM public.attachment_derived_write WHERE attachment_id=a.id AND state<>'complete') THEN
   RETURN jsonb_build_object('error','cleanup_identity_unverified'); END IF;
  SELECT * INTO q FROM public.attachment_object_deletion WHERE path=a.path;
  has_queue:=FOUND;
  IF has_queue AND ((q.storage_driver,q.storage_namespace,q.storage_container,q.storage_locator_sha256,q.storage_physical_key)
    IS DISTINCT FROM (a.storage_driver,a.storage_namespace,a.storage_container,a.storage_locator_sha256,a.storage_physical_key)) THEN
   RETURN jsonb_build_object('error','cleanup_identity_unverified'); END IF;
  IF has_queue AND q.job_state='done' AND q.completed_at IS NOT NULL THEN CONTINUE; END IF;
  IF public.fn_talk_record_war_blocks(a.id)
   OR EXISTS(SELECT 1 FROM public.enterprise_attachment_retention WHERE attachment_id=a.id
     AND (hold_state='held' OR purge_state IS DISTINCT FROM 'purged' OR retention_until>now()))
   OR EXISTS(SELECT 1 FROM public.talk_record_attachment_ref WHERE attachment_id=a.id AND state='bound'
     AND (lease_until IS NULL OR lease_until>now()))
   OR EXISTS(SELECT 1 FROM public.attach_pending WHERE object_key=a.path AND (upload_expires_at IS NULL OR upload_expires_at>now()))
   OR EXISTS(SELECT 1 FROM public.group_file WHERE id=a.group_file_id)
   OR EXISTS(SELECT 1 FROM public.user_collect WHERE attach_file_hash256<>'' AND attach_file_hash256=a.file_hash256 AND status=1)
   OR EXISTS(SELECT 1 FROM public.moment_post WHERE position(a.path in media::text)>0) THEN
    protected_count:=protected_count+1; CONTINUE; END IF;
  IF NOT has_queue THEN RETURN jsonb_build_object('error','cleanup_identity_unverified'); END IF;
  IF a.lifecycle_state<>'delete_intent' OR NOT a.gc_verified
   OR (q.storage_driver,q.storage_namespace,q.storage_container,q.storage_locator_sha256,q.storage_physical_key)
    IS DISTINCT FROM (a.storage_driver,a.storage_namespace,a.storage_container,a.storage_locator_sha256,a.storage_physical_key)
   OR q.storage_driver IS NULL OR q.storage_namespace IS NULL OR q.storage_container IS NULL
   OR q.storage_locator_sha256 IS NULL
   OR NOT EXISTS(SELECT 1 FROM public.attachment_lifecycle_v5_audit i WHERE i.queue_domain='ordinary'
    AND i.object_key=a.path AND i.action='delete_intent' AND i.storage_namespace=a.storage_namespace
    AND i.details->>'attachment_id'=a.id::text AND i.details->>'locator'=a.storage_locator_sha256) THEN RETURN jsonb_build_object('error','cleanup_identity_unverified'); END IF;
  IF q.job_state='blocked' AND q.last_error_code='max_attempts' AND q.attempts-q.attempt_base>=8
    AND q.lease_token IS NULL AND q.lease_until IS NULL AND q.completed_at IS NULL THEN
   resources:=resources||jsonb_build_array(jsonb_build_object('path',q.path,'generation',q.retry_generation,
    'attempts',q.attempts,'locator',q.storage_locator_sha256));
  ELSIF q.job_state NOT IN ('ready','leased','retry_wait') THEN
   RETURN jsonb_build_object('error','cleanup_identity_unverified');
  END IF;
 END LOOP;

 IF jsonb_array_length(resources)=0 THEN
  RETURN jsonb_build_object('error',CASE WHEN protected_count>0 THEN 'cleanup_protected' ELSE 'cleanup_not_retryable' END);
 END IF;
 RETURN jsonb_build_object('resources',resources,'protected_resources',protected_count,
  'cleanup_status',CASE WHEN protected_count>0 THEN 'retained_by_other_reference' ELSE 'pending' END,
  'reason',CASE WHEN protected_count>0 THEN 'partial_protection' ELSE 'eligible' END);
END $$;
ALTER FUNCTION public.fn_talk_record_retry_plan(bigint) OWNER TO talk_record_retry_definer;
REVOKE ALL ON FUNCTION public.fn_talk_record_retry_plan(bigint) FROM PUBLIC,v5_app_runtime,v5_worker,v5_proof;

CREATE FUNCTION public.fn_talk_record_cleanup_retry_capability(p_actor bigint,p_record bigint) RETURNS jsonb
LANGUAGE plpgsql STABLE SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE r public.talk_record%ROWTYPE; plan jsonb; reason text;
BEGIN
 SELECT * INTO r FROM public.talk_record WHERE id=p_record;
 IF p_actor IS NULL OR p_actor<=0 OR NOT FOUND
  OR (r.scope='personal' AND r.owner_user_id<>p_actor)
  OR (r.scope='organization' AND NOT EXISTS(SELECT 1 FROM public.organization o
   JOIN public.organization_member m ON m.organization_id=o.id
   WHERE o.id=r.organization_id AND o.status='active' AND m.status='active'
    AND m.user_id=p_actor AND m.role IN ('owner','admin'))) THEN
  RETURN jsonb_build_object('allowed',false,'generation',null,'exhausted_resources',0,'reason','permission_denied');
 END IF;
 plan:=public.fn_talk_record_retry_plan(p_record);
 IF plan?'error' THEN
  reason:=CASE plan->>'error' WHEN 'cleanup_protected' THEN 'protected'
    WHEN 'cleanup_not_retryable' THEN 'not_exhausted' WHEN 'record_not_deleted' THEN 'not_deleted'
    ELSE 'identity_unverified' END;
  RETURN jsonb_build_object('allowed',false,'generation',r.cleanup_generation,'exhausted_resources',0,'reason',reason);
 END IF;
 RETURN jsonb_build_object('allowed',true,'generation',r.cleanup_generation,
  'exhausted_resources',jsonb_array_length(plan->'resources'),'reason',plan->>'reason');
END $$;
ALTER FUNCTION public.fn_talk_record_cleanup_retry_capability(bigint,bigint) OWNER TO talk_record_retry_definer;
REVOKE ALL ON FUNCTION public.fn_talk_record_cleanup_retry_capability(bigint,bigint) FROM PUBLIC,v5_worker,v5_proof;
GRANT EXECUTE ON FUNCTION public.fn_talk_record_cleanup_retry_capability(bigint,bigint) TO v5_app_runtime;

CREATE FUNCTION public.fn_talk_record_cleanup_retry(p_actor bigint,p_record bigint,p_revision bigint,
 p_generation bigint,p_key text,p_hash text,p_reason text,p_job bigint) RETURNS jsonb
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE r public.talk_record%ROWTYPE; a public.attachment%ROWTYPE;
 q public.attachment_object_deletion%ROWTYPE; org bigint; role text;
 prior public.talk_record_cleanup_retry%ROWTYPE; resources jsonb:='[]';
 result jsonb; plan jsonb; count_reopened integer:=0;
BEGIN
 IF p_actor IS NULL OR p_job IS NULL OR p_generation IS NULL OR p_revision IS NULL
  OR p_hash IS NULL OR p_actor<=0 OR p_job<=0 OR p_generation<0 OR p_revision<=0 OR p_key IS NULL
  OR length(p_key) NOT BETWEEN 1 AND 128 OR p_hash !~ '^[0-9a-f]{64}$'
  OR p_reason IS NULL OR length(p_reason)>200 THEN
  RETURN jsonb_build_object('error','invalid_body');
 END IF;
 SELECT organization_id INTO org FROM public.talk_record WHERE id=p_record;
 IF org IS NOT NULL THEN
  PERFORM id FROM public.organization WHERE id=org AND status='active' FOR SHARE;
  IF NOT FOUND THEN RETURN jsonb_build_object('error','not_found'); END IF;
  SELECT m.role INTO role FROM public.organization_member m
   WHERE m.organization_id=org AND m.user_id=p_actor AND m.status='active' FOR SHARE;
  IF role IS NULL OR role NOT IN ('owner','admin') THEN RETURN jsonb_build_object('error','not_found'); END IF;
 END IF;
 SELECT * INTO r FROM public.talk_record WHERE id=p_record FOR UPDATE;
 IF NOT FOUND OR (r.scope='personal' AND r.owner_user_id<>p_actor)
  OR (r.scope='organization' AND (org IS NULL OR r.organization_id<>org)) THEN
  RETURN jsonb_build_object('error','not_found');
 END IF;
 IF r.deleted_at IS NULL THEN RETURN jsonb_build_object('error','record_not_deleted'); END IF;
 SELECT * INTO prior FROM public.talk_record_cleanup_retry
  WHERE record_id=p_record AND actor_user_id=p_actor AND request_key=p_key;
 IF FOUND THEN
  IF prior.payload_hash<>p_hash THEN RETURN jsonb_build_object('error','idempotency_conflict'); END IF;
  RETURN prior.response||jsonb_build_object('replayed',true);
 END IF;
 IF r.revision<>p_revision THEN RETURN jsonb_build_object('error','revision_mismatch'); END IF;
 IF r.cleanup_generation<>p_generation THEN RETURN jsonb_build_object('error','cleanup_generation_conflict'); END IF;
 IF r.cleanup_status NOT IN ('failed','pending','retained_by_other_reference','blocked_hold','blocked_retention') THEN RETURN jsonb_build_object('error','cleanup_not_retryable'); END IF;
 -- Lock all authoritative assets in stable order before taking any queue lock.
 FOR a IN SELECT * FROM public.attachment WHERE id IN (
  SELECT attachment_id FROM public.talk_record_attachment_ref WHERE record_id=p_record
  UNION SELECT dw.attachment_id FROM public.attachment_derived_write dw
   JOIN public.talk_record_job j ON j.id=dw.source_job_id WHERE j.record_id=p_record)
  ORDER BY id FOR UPDATE LOOP
  PERFORM attachment_id FROM public.enterprise_attachment_retention WHERE attachment_id=a.id FOR UPDATE;
  PERFORM attachment_id FROM public.workspace_attachment_retention WHERE attachment_id=a.id FOR UPDATE;
 END LOOP;
 -- Queue locks follow the existing asset/identity/queue order. Readonly caps
 -- use the same plan without these locks; POST always re-evaluates after locking.
 FOR a IN SELECT * FROM public.attachment WHERE id IN (
  SELECT attachment_id FROM public.talk_record_attachment_ref WHERE record_id=p_record
  UNION SELECT dw.attachment_id FROM public.attachment_derived_write dw
   JOIN public.talk_record_job j ON j.id=dw.source_job_id WHERE j.record_id=p_record)
  ORDER BY path LOOP
  PERFORM public.fn_v5_lock_identity(a.storage_driver,a.storage_namespace,a.storage_container,a.path);
  PERFORM path FROM public.attachment_object_deletion WHERE path=a.path FOR UPDATE;
 END LOOP;
 plan:=public.fn_talk_record_retry_plan(p_record);
 IF plan?'error' THEN RETURN plan; END IF;
 resources:=plan->'resources';
 FOR result IN SELECT value FROM jsonb_array_elements(resources) LOOP
  UPDATE public.attachment_object_deletion SET retry_generation=retry_generation+1,attempt_base=attempts,
   job_state='ready',next_retry_at=now(),last_error_code=NULL,lease_token=NULL,lease_until=NULL
   WHERE path=result->>'path' AND retry_generation=(result->>'generation')::bigint
    AND job_state='blocked' AND attempts=(result->>'attempts')::integer;
  IF NOT FOUND THEN RAISE EXCEPTION 'cleanup_generation_conflict' USING ERRCODE='40001'; END IF;
  INSERT INTO public.attachment_lifecycle_v5_audit(queue_domain,object_key,storage_namespace,action,reason,actor,details)
   SELECT 'ordinary',path,storage_namespace,'talk_record_retry',p_reason,p_actor::text,
    jsonb_build_object('record_id',p_record,'generation',retry_generation,'attempt_base',attempt_base,'request_key',p_key)
    FROM public.attachment_object_deletion WHERE path=result->>'path';
  count_reopened:=count_reopened+1;
 END LOOP;
 UPDATE public.talk_record SET cleanup_generation=cleanup_generation+1,revision=revision+1,
  cleanup_status=plan->>'cleanup_status',updated_at=now() WHERE id=p_record;
 INSERT INTO public.talk_record_job(id,record_id,kind,dedupe_key)
  VALUES(p_job,p_record,'purge','purge-recovery:'||p_record||':'||(r.cleanup_generation+1));
 result:=jsonb_build_object('job_id',p_job,'revision',r.revision+1,'cleanup_generation',r.cleanup_generation+1,
  'cleanup_status',plan->>'cleanup_status','reopened_resources',count_reopened,
  'protected_resources',(plan->>'protected_resources')::integer,'reason',plan->>'reason','replayed',false);
 INSERT INTO public.talk_record_cleanup_retry(record_id,actor_user_id,request_key,payload_hash,generation,reason,resources,response)
  VALUES(p_record,p_actor,p_key,p_hash,r.cleanup_generation+1,p_reason,resources,result);
 RETURN result;
END $$;
ALTER FUNCTION public.fn_talk_record_cleanup_retry(bigint,bigint,bigint,bigint,text,text,text,bigint)
 OWNER TO talk_record_retry_definer;
REVOKE ALL ON FUNCTION public.fn_talk_record_cleanup_retry(bigint,bigint,bigint,bigint,text,text,text,bigint) FROM PUBLIC,v5_worker,v5_proof;
GRANT EXECUTE ON FUNCTION public.fn_talk_record_cleanup_retry(bigint,bigint,bigint,bigint,text,text,text,bigint) TO v5_app_runtime;

-- Same default eight attempts per explicitly authorized generation.
CREATE OR REPLACE FUNCTION public.fn_v5_claim_ordinary(p_token uuid)
RETURNS TABLE(out_path text, out_bucket text, out_storage_driver text,
              out_storage_namespace text, out_storage_container text,
              out_storage_locator_sha256 text, out_storage_physical_key text,
              out_attempts integer)
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
    -- plpgsql UPDATE..RETURNING 必须有去向（RETURN QUERY 惯用法；修复 179 同型缺陷）
    RETURN QUERY
    UPDATE public.attachment_object_deletion q
        SET job_state = 'leased', lease_token = p_token,
            lease_until = clock_timestamp() + interval '120 seconds',
            attempts = q.attempts + 1, last_error_code = NULL
        WHERE q.attempts-q.attempt_base < 8
          AND q.job_state IN ('ready', 'retry_wait')
          AND q.next_retry_at <= clock_timestamp()
          AND q.path = (SELECT path FROM public.attachment_object_deletion
                        WHERE attempts-attempt_base < 8
                          AND job_state IN ('ready', 'retry_wait')
                          AND next_retry_at <= clock_timestamp()
                        ORDER BY next_retry_at, path
                        FOR UPDATE SKIP LOCKED LIMIT 1)
        RETURNING q.path, q.bucket, q.storage_driver, q.storage_namespace,
                  q.storage_container, q.storage_locator_sha256,
                  q.storage_physical_key, q.attempts;
END;
$$;
ALTER FUNCTION public.fn_v5_claim_ordinary(uuid) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_v5_claim_ordinary(uuid) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_v5_claim_ordinary(uuid) TO v5_worker;
CREATE OR REPLACE FUNCTION public.fn_v5_ack_ordinary(p_path text, p_token uuid, p_ok boolean)
RETURNS text LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE v_state text; v_attempts integer; v_delay interval;
DECLARE v_backoff integer[] := ARRAY[30, 120, 600, 1800, 7200, 21600, 43200];
BEGIN
    SELECT job_state, attempts-attempt_base INTO v_state, v_attempts
        FROM public.attachment_object_deletion WHERE path = p_path FOR UPDATE;
    IF NOT FOUND THEN
        RETURN 'missing';
    END IF;
    IF v_state <> 'leased' THEN
        RETURN 'not_leased';
    END IF;
    IF p_ok THEN
        UPDATE public.attachment_object_deletion
            SET job_state = 'done', completed_at = now(),
                lease_token = NULL, lease_until = NULL, last_error_code = NULL
            WHERE path = p_path
              AND lease_token = p_token AND job_state = 'leased';
        IF NOT FOUND THEN RETURN 'stale_token'; END IF;
        INSERT INTO public.attachment_lifecycle_v5_audit
            (queue_domain, object_key, action, reason, actor)
        VALUES ('ordinary', p_path, 'delete_done', 'worker_ack', current_user);
        RETURN 'done';
    END IF;
    v_delay := v_backoff[least(v_attempts, 7)] * (1 + random() * 0.2) * interval '1 second';
    UPDATE public.attachment_object_deletion
        SET job_state = CASE WHEN v_attempts >= 8 THEN 'blocked' ELSE 'retry_wait' END,
            next_retry_at = now() + v_delay,
            lease_token = NULL, lease_until = NULL,
            last_error_code = CASE WHEN v_attempts >= 8 THEN 'max_attempts'
                                   ELSE 'delete_failed' END
        WHERE path = p_path
          AND lease_token = p_token AND job_state = 'leased';
    IF NOT FOUND THEN RETURN 'stale_token'; END IF;
    RETURN CASE WHEN v_attempts >= 8 THEN 'blocked' ELSE 'retry_wait' END;
END;
$$;
ALTER FUNCTION public.fn_v5_ack_ordinary(text, uuid, boolean) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_v5_ack_ordinary(text, uuid, boolean) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_v5_ack_ordinary(text, uuid, boolean) TO v5_worker;

CREATE OR REPLACE FUNCTION public.fn_v5_reaper_ordinary(p_limit integer DEFAULT 100)
RETURNS integer LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE v_count integer;
BEGIN
    WITH expired AS (
        SELECT path FROM public.attachment_object_deletion
        WHERE job_state = 'leased' AND lease_until < clock_timestamp()
        ORDER BY lease_until, path LIMIT least(p_limit, 100)
        FOR UPDATE SKIP LOCKED)
    UPDATE public.attachment_object_deletion q
        SET job_state = CASE WHEN q.attempts-q.attempt_base >= 8 THEN 'blocked' ELSE 'retry_wait' END,
            next_retry_at = now(),
            lease_token = NULL, lease_until = NULL,
            last_error_code = CASE WHEN q.attempts-q.attempt_base >= 8 THEN 'max_attempts'
                                   ELSE 'lease_expired' END
        FROM expired e WHERE q.path = e.path;
    GET DIAGNOSTICS v_count = ROW_COUNT;
    RETURN v_count;
END;
$$;
ALTER FUNCTION public.fn_v5_reaper_ordinary(integer) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_v5_reaper_ordinary(integer) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_v5_reaper_ordinary(integer) TO v5_worker;

-- Harden the existing invoker helper too: temporary retention tables cannot mask hold.
CREATE OR REPLACE FUNCTION public.fn_talk_record_war_blocks(p_attachment_id bigint) RETURNS boolean
 LANGUAGE sql STABLE SET search_path=pg_catalog,public,pg_temp AS $$
 SELECT EXISTS(SELECT 1 FROM public.workspace_attachment_retention w WHERE w.attachment_id=p_attachment_id
  AND (w.scope<>'talk_record' OR NOT EXISTS(SELECT 1 FROM public.enterprise_attachment_retention e
   WHERE e.attachment_id=w.attachment_id AND e.organization_id=w.organization_id
    AND e.source_kind='talk_record' AND e.talk_record_id::text=w.scope_ref AND e.purge_state='purged')))
$$;
