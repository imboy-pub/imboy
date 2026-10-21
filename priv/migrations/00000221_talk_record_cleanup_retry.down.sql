-- Rollback is forbidden once recovery audit exists; never discard generations.
DO $$ BEGIN
 IF EXISTS(SELECT 1 FROM public.talk_record_cleanup_retry) THEN
  RAISE EXCEPTION 'retained retry audit prevents rollback'; END IF;
END $$;
DROP FUNCTION public.fn_talk_record_cleanup_retry(bigint,bigint,bigint,bigint,text,text,text,bigint);
DROP FUNCTION public.fn_talk_record_cleanup_retry_capability(bigint,bigint);
DROP FUNCTION public.fn_talk_record_retry_plan(bigint);
DROP TRIGGER trg_talk_record_retry_budget ON public.attachment_object_deletion;
DROP FUNCTION public.fn_talk_record_retry_budget_guard();
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
        WHERE q.attempts < 8
          AND q.job_state IN ('ready', 'retry_wait')
          AND q.next_retry_at <= clock_timestamp()
          AND q.path = (SELECT path FROM public.attachment_object_deletion
                        WHERE attempts < 8
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
    SELECT job_state, attempts INTO v_state, v_attempts
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
        SET job_state = CASE WHEN q.attempts >= 8 THEN 'blocked' ELSE 'retry_wait' END,
            next_retry_at = now(),
            lease_token = NULL, lease_until = NULL,
            last_error_code = CASE WHEN q.attempts >= 8 THEN 'max_attempts'
                                   ELSE 'lease_expired' END
        FROM expired e WHERE q.path = e.path;
    GET DIAGNOSTICS v_count = ROW_COUNT;
    RETURN v_count;
END;
$$;
ALTER FUNCTION public.fn_v5_reaper_ordinary(integer) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_v5_reaper_ordinary(integer) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_v5_reaper_ordinary(integer) TO v5_worker;
DROP TABLE public.talk_record_cleanup_retry;
DROP FUNCTION public.fn_talk_record_cleanup_retry_audit_guard();
ALTER TABLE public.attachment_object_deletion DROP CONSTRAINT ck_aod_retry_generation,
 DROP COLUMN retry_generation,DROP COLUMN attempt_base;
ALTER TABLE talk_record DROP COLUMN cleanup_generation;
-- NOLOGIN role intentionally retained; revoking grants is a separate audited operation.

-- Harden the existing invoker helper too: temporary retention tables cannot mask hold.
CREATE OR REPLACE FUNCTION public.fn_talk_record_war_blocks(p_attachment_id bigint) RETURNS boolean
 LANGUAGE sql STABLE SET search_path=pg_catalog,public,pg_temp AS $$
 SELECT EXISTS(SELECT 1 FROM public.workspace_attachment_retention w WHERE w.attachment_id=p_attachment_id
  AND (w.scope<>'talk_record' OR NOT EXISTS(SELECT 1 FROM public.enterprise_attachment_retention e
   WHERE e.attachment_id=w.attachment_id AND e.organization_id=w.organization_id
    AND e.source_kind='talk_record' AND e.talk_record_id::text=w.scope_ref AND e.purge_state='purged')))
$$;
