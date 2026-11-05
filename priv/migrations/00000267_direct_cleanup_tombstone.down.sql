-- A tombstone cannot be represented by the legacy one-shot cleanup protocol.
LOCK TABLE attach_direct_cleanup IN ACCESS EXCLUSIVE MODE;
DO $$ BEGIN
 IF EXISTS (SELECT 1 FROM attach_direct_cleanup LIMIT 1) THEN
  RAISE EXCEPTION 'direct cleanup tombstones prevent downgrade';
 END IF;
END $$;
-- The legacy table has no retry deadline; fail closed without violating its lease check.
CREATE OR REPLACE FUNCTION fn_dv1_cleanup_ack(p_id bigint,p_token uuid,p_ok boolean)
RETURNS text LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public AS $$
BEGIN
 UPDATE public.attach_direct_cleanup SET
  job_state=CASE WHEN p_ok THEN 'done' ELSE 'blocked' END,
  completed_at=CASE WHEN p_ok THEN clock_timestamp() ELSE NULL END,
  lease_token=NULL,lease_until=NULL,
  last_error_code=CASE WHEN p_ok THEN NULL ELSE 'delete_failed' END
 WHERE id=p_id AND job_state='leased' AND lease_token=p_token;
 IF NOT FOUND THEN RETURN 'stale_token'; END IF;
 RETURN CASE WHEN p_ok THEN 'done' ELSE 'blocked' END;
END $$;

DROP FUNCTION fn_dv1_tombstone_ack(bigint,uuid,boolean);
DROP FUNCTION fn_dv1_tombstone_claim(uuid);
GRANT EXECUTE ON FUNCTION fn_dv1_cleanup_claim(uuid),fn_dv1_cleanup_ack(bigint,uuid,boolean) TO PUBLIC;
DROP INDEX i_dv1_cleanup_scan;
CREATE OR REPLACE FUNCTION fn_dv1_cancel(p_key text, p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a record;
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


DROP TRIGGER dv1_cleanup_key_guard ON attach_direct_attempt;
DROP FUNCTION fn_dv1_cleanup_key_guard();
DROP TRIGGER dv1_cleanup_freeze ON attach_direct_cleanup;
DROP FUNCTION fn_dv1_cleanup_freeze();
ALTER TABLE attach_direct_cleanup DROP COLUMN next_scan_at, DROP COLUMN locator_sha256;
