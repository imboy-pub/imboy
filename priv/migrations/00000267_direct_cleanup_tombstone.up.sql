-- Keep cancelled physical identities as persistent tombstones; a successful DELETE
-- cannot prove a previously accepted PUT/COPY will not arrive later.
ALTER TABLE attach_direct_cleanup ADD COLUMN locator_sha256 text;
ALTER TABLE attach_direct_cleanup ADD COLUMN next_scan_at timestamptz NOT NULL DEFAULT clock_timestamp();
UPDATE attach_direct_cleanup c SET locator_sha256=p.storage_locator_sha256
  FROM attach_pending p WHERE p.object_key=c.object_key
    AND p.storage_driver=c.target_driver AND p.storage_namespace=c.target_namespace
    AND p.storage_container=c.target_container;
UPDATE attach_direct_cleanup SET job_state='blocked', lease_token=NULL, lease_until=NULL,
    last_error_code='location_unverified' WHERE locator_sha256 IS NULL;
CREATE FUNCTION fn_dv1_cleanup_freeze() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public AS $$
BEGIN
 PERFORM pg_advisory_xact_lock(hashtextextended(NEW.physical_key, 271));
 SELECT storage_locator_sha256 INTO NEW.locator_sha256 FROM public.attach_pending
  WHERE object_key=NEW.object_key AND storage_driver=NEW.target_driver
    AND storage_namespace=NEW.target_namespace AND storage_container=NEW.target_container;
 IF NEW.locator_sha256 IS NULL OR NEW.locator_sha256 !~ '^[0-9a-f]{64}$' THEN
   RAISE EXCEPTION 'cleanup location unavailable' USING ERRCODE='23514';
 END IF;
 RETURN NEW;
END $$;
CREATE TRIGGER dv1_cleanup_freeze BEFORE INSERT ON attach_direct_cleanup
 FOR EACH ROW EXECUTE FUNCTION fn_dv1_cleanup_freeze();
-- Attempt keys are never reused, including after a successful tombstone sweep.
CREATE FUNCTION fn_dv1_cleanup_key_guard() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public AS $$
BEGIN
 PERFORM pg_advisory_xact_lock(hashtextextended(least(NEW.source_key,NEW.target_key),271));
 PERFORM pg_advisory_xact_lock(hashtextextended(greatest(NEW.source_key,NEW.target_key),271));
 IF EXISTS (SELECT 1 FROM public.attach_direct_cleanup
    WHERE physical_key IN (NEW.source_key,NEW.target_key)) OR EXISTS (
    SELECT 1 FROM public.attach_direct_attempt
    WHERE source_key IN (NEW.source_key,NEW.target_key)
       OR target_key IN (NEW.source_key,NEW.target_key)) THEN
   RAISE EXCEPTION 'direct physical key already reserved' USING ERRCODE='23505';
 END IF;
 RETURN NEW;
END $$;
CREATE TRIGGER dv1_cleanup_key_guard BEFORE INSERT ON attach_direct_attempt
 FOR EACH ROW EXECUTE FUNCTION fn_dv1_cleanup_key_guard();
CREATE OR REPLACE FUNCTION fn_dv1_cancel(p_key text, p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a record; v_key text;
BEGIN
    PERFORM pg_advisory_xact_lock(hashtextextended(p_key, 168));
    -- 单语句双跳：分步 UPDATE 会落在 ck_dv1_direct_sync 违例态（CANCELLED
    -- 要求 session 已离 registered，反之亦然）；同语句满足 V5 guard 两跳语义。
    UPDATE attach_pending SET
        direct_state = 'CANCELLED', session_state = 'rejected'
        WHERE object_key = p_key
          AND direct_state IN ('GRANTED', 'FINALIZING')
          AND session_state = 'registered';
    IF NOT FOUND THEN
        RAISE EXCEPTION 'dv1 cancel illegal state' USING ERRCODE = '23510';
    END IF;
    -- Lock every physical identity in the same order as attempt insertion.
    FOR v_key IN
        SELECT physical_key FROM (
            SELECT source_key AS physical_key FROM attach_direct_attempt
                WHERE object_key=p_key AND attempt_state <> 'WINNER'
            UNION
            SELECT target_key AS physical_key FROM attach_direct_attempt
                WHERE object_key=p_key AND attempt_state <> 'WINNER'
        ) keys ORDER BY physical_key
    LOOP
        PERFORM pg_advisory_xact_lock(hashtextextended(v_key,271));
    END LOOP;
    -- 物理身份先固化（全部未 WINNER attempt 的 staging+candidate）
    FOR v_a IN
        SELECT source_key, target_key, target_driver, target_namespace, target_container
            FROM attach_direct_attempt
            WHERE object_key = p_key AND attempt_state <> 'WINNER'
    LOOP
        INSERT INTO attach_direct_cleanup
            (object_key, physical_key, target_driver, target_namespace, target_container, reason)
            VALUES (p_key, v_a.source_key, v_a.target_driver, v_a.target_namespace,
                    v_a.target_container, coalesce(p_reason, 'cancel'))
            ON CONFLICT (physical_key) DO NOTHING;
        INSERT INTO attach_direct_cleanup
            (object_key, physical_key, target_driver, target_namespace, target_container, reason)
            VALUES (p_key, v_a.target_key, v_a.target_driver, v_a.target_namespace,
                    v_a.target_container, coalesce(p_reason, 'cancel'))
            ON CONFLICT (physical_key) DO NOTHING;
    END LOOP;
END;
$$;


CREATE INDEX i_dv1_cleanup_scan ON attach_direct_cleanup(next_scan_at,id)
 WHERE job_state IN ('ready','retry_wait','done','leased');
-- The legacy functions cannot safely re-scan tombstones or recover expired leases.
REVOKE EXECUTE ON FUNCTION fn_dv1_cleanup_claim(uuid),fn_dv1_cleanup_ack(bigint,uuid,boolean) FROM PUBLIC;
CREATE FUNCTION fn_dv1_tombstone_claim(p_token uuid)
RETURNS TABLE(out_id bigint,out_object_key text,out_physical_key text,out_driver text,
 out_namespace text,out_container text,out_locator text)
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public AS $$
DECLARE v_id bigint;
BEGIN
 IF p_token IS NULL THEN RAISE EXCEPTION 'missing token' USING ERRCODE='22023'; END IF;
 SELECT c.id INTO v_id FROM public.attach_direct_cleanup c
 WHERE c.locator_sha256 IS NOT NULL AND c.next_scan_at <= clock_timestamp()
 AND (c.job_state IN ('ready','retry_wait','done') OR
      (c.job_state='leased' AND c.lease_until < clock_timestamp()))
 AND NOT EXISTS (SELECT 1 FROM public.attach_direct_attempt a
   WHERE a.target_key=c.physical_key AND a.attempt_state='WINNER')
 AND NOT EXISTS (SELECT 1 FROM public.attach_direct_attempt a
   JOIN public.attach_pending p ON p.object_key=a.object_key
   WHERE c.physical_key IN (a.source_key,a.target_key)
     AND p.direct_state IN ('RESERVED','DECLARED','GRANTED','FINALIZING')
     AND a.attempt_state <> 'GC_CLAIMED')
 AND NOT EXISTS (SELECT 1 FROM public.attachment a
   WHERE a.storage_physical_key=c.physical_key AND a.lifecycle_state <> 'deleted')
 ORDER BY c.next_scan_at,c.id FOR UPDATE SKIP LOCKED LIMIT 1;
 IF v_id IS NULL THEN RETURN; END IF;
 RETURN QUERY UPDATE public.attach_direct_cleanup c SET job_state='leased',lease_token=p_token,
 lease_until=clock_timestamp()+interval '120 seconds',attempts=c.attempts+1
 WHERE c.id=v_id RETURNING c.id,c.object_key,c.physical_key,c.target_driver,
 c.target_namespace,c.target_container,c.locator_sha256;
END $$;
CREATE FUNCTION fn_dv1_tombstone_ack(p_id bigint,p_token uuid,p_ok boolean)
RETURNS text LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public AS $$
BEGIN
 UPDATE public.attach_direct_cleanup SET job_state=CASE WHEN p_ok THEN 'done' ELSE 'retry_wait' END,
 lease_token=NULL,lease_until=NULL,completed_at=CASE WHEN p_ok THEN clock_timestamp() ELSE completed_at END,
 next_scan_at=clock_timestamp()+CASE WHEN p_ok THEN interval '1 hour' ELSE interval '1 minute' END,
 last_error_code=CASE WHEN p_ok THEN NULL ELSE 'delete_failed' END
 WHERE id=p_id AND job_state='leased' AND lease_token=p_token;
 IF NOT FOUND THEN RETURN 'stale_token'; END IF;
 RETURN CASE WHEN p_ok THEN 'done' ELSE 'retry_wait' END;
END $$;
REVOKE ALL ON FUNCTION fn_dv1_tombstone_claim(uuid),fn_dv1_tombstone_ack(bigint,uuid,boolean) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_dv1_tombstone_claim(uuid),fn_dv1_tombstone_ack(bigint,uuid,boolean) TO v5_worker;
-- Preserve the legacy acknowledgement API without violating ck_dv1_cleanup_lease.
CREATE OR REPLACE FUNCTION fn_dv1_cleanup_ack(p_id bigint,p_token uuid,p_ok boolean)
RETURNS text LANGUAGE sql SECURITY DEFINER SET search_path=pg_catalog,public AS $$
 SELECT public.fn_dv1_tombstone_ack(p_id,p_token,p_ok)
$$;
