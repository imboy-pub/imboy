-- Restore main 226 ordinary/retry bodies and migration 220 purged body.
-- Preserve comma-separated collection hash protection while reverting physical identity capture.
-- Existing captured physical keys, queue budgets, audits and business data remain.
-- CREATE OR REPLACE retains existing function ownership and ACLs.
SET LOCAL lock_timeout='5s';

CREATE OR REPLACE FUNCTION public.fn_v5_intent_ordinary_delete(p_path text, p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a attachment%ROWTYPE; v_p attach_pending%ROWTYPE; v_bucket text;
BEGIN
    SELECT * INTO v_a FROM attachment WHERE path = p_path FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 intent unknown attachment' USING ERRCODE = 'P0002';
    END IF;
    -- Eligibility conjunction, re-checked under lock; unknown => not eligible.
    IF v_a.lifecycle_state <> 'live' THEN
        RAISE EXCEPTION 'v5 intent non-live asset' USING ERRCODE = '23510';
    END IF;
    IF NOT v_a.gc_verified THEN
        RAISE EXCEPTION 'v5 intent requires proven reference-zero (gc_verified)'
            USING ERRCODE = '23510';
    END IF;
    IF fn_talk_record_war_blocks(v_a.id) THEN
        RAISE EXCEPTION 'v5 intent blocked by workspace retention'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM enterprise_attachment_retention
               WHERE attachment_id = v_a.id
                 AND (hold_state = 'held'
                      OR purge_state IS DISTINCT FROM 'purged')) THEN
        RAISE EXCEPTION 'v5 intent blocked by EAR' USING ERRCODE = '23510';
    END IF;
    IF v_a.group_file_id IS NOT NULL
       AND EXISTS (SELECT 1 FROM group_file WHERE id = v_a.group_file_id) THEN
        RAISE EXCEPTION 'v5 intent blocked by live group_file binding'
            USING ERRCODE = '23510';
    END IF;
    -- T06（00000212）：talk_record 有效业务引用（含未到期 processing lease）
    -- 阻止普通域删除意图——pre-lock 快速拒绝。
    IF EXISTS (SELECT 1 FROM talk_record_attachment_ref trar
               WHERE trar.attachment_id = v_a.id
                 AND trar.state = 'bound'
                 AND (trar.lease_until IS NULL OR trar.lease_until > now())) THEN
        RAISE EXCEPTION 'v5 intent blocked by talk_record reference'
            USING ERRCODE = '23510';
    END IF;
    SELECT * INTO v_p FROM attach_pending WHERE object_key = p_path;
    IF FOUND AND (v_p.upload_expires_at IS NULL OR v_p.upload_expires_at > now()) THEN
        RAISE EXCEPTION 'v5 intent upload capability not exhausted'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path = p_path) THEN
        RAISE EXCEPTION 'v5 intent job already exists (tombstone)'
            USING ERRCODE = '23505';
    END IF;
    -- Object lock ordering: identity lock, then queue path lock (seed 168).
    PERFORM fn_v5_lock_identity(v_a.storage_driver, v_a.storage_namespace,
                                v_a.storage_container, v_a.path);
    PERFORM pg_advisory_xact_lock(hashtextextended(v_a.path, 168));
    -- Cross-domain same-identity rejection.
    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue q
               WHERE q.storage_namespace = v_a.storage_namespace
                 AND q.storage_container = v_a.storage_container
                 AND q.object_key = v_a.path) THEN
        RAISE EXCEPTION 'v5 intent cross-domain identity conflict'
            USING ERRCODE = '23510';
    END IF;
    -- W3-01：锁后资格复核（02 §4——取锁前的候选查询不构成授权）：
    -- 绑定事务与 intent 在同一对象锁上线性化，先提交者唯一胜出。
    IF v_a.group_file_id IS NOT NULL
       AND EXISTS (SELECT 1 FROM group_file gf WHERE gf.id = v_a.group_file_id) THEN
        RAISE EXCEPTION 'v5 intent blocked by live group_file binding'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM user_collect uc
               WHERE uc.attach_file_hash256 <> '' AND v_a.file_hash256 = ANY(string_to_array(uc.attach_file_hash256, ','))
                 AND uc.status = 1) THEN
        RAISE EXCEPTION 'v5 intent blocked by collect reference'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM moment_post mp
               WHERE position(v_a.path in mp.media::text) > 0) THEN
        RAISE EXCEPTION 'v5 intent blocked by moment media reference'
            USING ERRCODE = '23510';
    END IF;
    -- T06（00000212）：锁后复核 talk_record 有效引用（gc_verified 为缓存标志，
    -- 绑定可能晚于上次 refresh —— 02 §5"intent 在对象锁内重查全部引用"）。
    IF EXISTS (SELECT 1 FROM talk_record_attachment_ref trar
               WHERE trar.attachment_id = v_a.id
                 AND trar.state = 'bound'
                 AND (trar.lease_until IS NULL OR trar.lease_until > now())) THEN
        RAISE EXCEPTION 'v5 intent blocked by talk_record reference'
            USING ERRCODE = '23510';
    END IF;
    UPDATE attachment SET lifecycle_state = 'delete_intent'
        WHERE id = v_a.id AND lifecycle_state = 'live';
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 intent lost eligibility race' USING ERRCODE = '40001';
    END IF;
    INSERT INTO attachment_object_deletion
        (path, bucket, storage_driver, storage_namespace, storage_container,
         storage_locator_sha256, job_state, next_retry_at, last_error_code)
    VALUES (v_a.path, v_a.url, v_a.storage_driver, v_a.storage_namespace,
            v_a.storage_container, v_a.storage_locator_sha256,
            'ready', now(), NULL);
    INSERT INTO attachment_lifecycle_v5_audit
        (queue_domain, object_key, storage_namespace, action, reason, actor, details)
    VALUES ('ordinary', v_a.path, v_a.storage_namespace, 'delete_intent',
            p_reason, current_user,
            jsonb_build_object('attachment_id', v_a.id,
                               'locator', v_a.storage_locator_sha256));
END;
$$;

CREATE OR REPLACE FUNCTION public.fn_v5_intent_purged_delete(p_path text, p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a attachment%ROWTYPE; v_ready boolean;
BEGIN
    PERFORM fn_v5_lock_ordinary_upload(p_path);
    SELECT * INTO v_a FROM attachment WHERE path = p_path FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 purged intent unknown attachment' USING ERRCODE = 'P0002';
    END IF;
    IF v_a.lifecycle_state <> 'live' THEN
        RAISE EXCEPTION 'v5 purged intent non-live asset' USING ERRCODE = '23510';
    END IF;
    IF NOT EXISTS (
        SELECT 1 FROM enterprise_attachment_retention ear
        WHERE ear.attachment_id = v_a.id AND ear.purge_state = 'purged'
    ) THEN
        RAISE EXCEPTION 'v5 purged intent requires EAR purge terminal state'
            USING ERRCODE = '23510';
    END IF;
    -- Talk governance approval does not consume live content refs or upload capabilities.
    IF EXISTS(SELECT 1 FROM enterprise_attachment_retention e WHERE e.attachment_id=v_a.id
              AND e.source_kind='talk_record') THEN
        PERFORM fn_v5_intent_ordinary_delete(p_path,p_reason);
        RETURN;
    END IF;
    IF EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path = p_path) THEN
        RAISE EXCEPTION 'v5 intent job already exists (tombstone)'
            USING ERRCODE = '23505';
    END IF;
    PERFORM fn_v5_lock_identity(v_a.storage_driver, v_a.storage_namespace,
                                v_a.storage_container, v_a.path);
    PERFORM pg_advisory_xact_lock(hashtextextended(v_a.path, 168));
    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue q
               WHERE q.storage_namespace = v_a.storage_namespace
                 AND q.storage_container = v_a.storage_container
                 AND q.object_key = v_a.path) THEN
        RAISE EXCEPTION 'v5 intent cross-domain identity conflict'
            USING ERRCODE = '23510';
    END IF;
    UPDATE attachment SET lifecycle_state = 'delete_intent'
        WHERE id = v_a.id AND lifecycle_state = 'live';
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 purged intent lost eligibility race' USING ERRCODE = '40001';
    END IF;
    v_ready := v_a.storage_driver IS NOT NULL AND v_a.storage_namespace IS NOT NULL
        AND v_a.storage_container IS NOT NULL
        AND v_a.storage_locator_sha256 ~ '^[0-9a-f]{64}$';
    INSERT INTO attachment_object_deletion
        (path, bucket, storage_driver, storage_namespace, storage_container,
         storage_locator_sha256, job_state, next_retry_at, last_error_code)
    VALUES (v_a.path, v_a.url, v_a.storage_driver, v_a.storage_namespace,
            v_a.storage_container, v_a.storage_locator_sha256,
            CASE WHEN v_ready THEN 'ready' ELSE 'blocked' END,
            CASE WHEN v_ready THEN now() ELSE now() END,
            CASE WHEN v_ready THEN NULL ELSE 'legacy_location_unverified' END);
    INSERT INTO attachment_lifecycle_v5_audit
        (queue_domain, object_key, storage_namespace, action, reason, actor, details)
    VALUES ('ordinary', v_a.path, v_a.storage_namespace, 'delete_intent',
            p_reason, current_user,
            jsonb_build_object('attachment_id', v_a.id, 'purge_domain', 'ear',
                               'job_state', CASE WHEN v_ready THEN 'ready'
                                                  ELSE 'blocked' END));
END;
$$;

CREATE OR REPLACE FUNCTION public.fn_talk_record_retry_plan(p_record bigint) RETURNS jsonb
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
   OR EXISTS(SELECT 1 FROM public.user_collect WHERE attach_file_hash256<>'' AND a.file_hash256=ANY(string_to_array(attach_file_hash256,',')) AND status=1)
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
