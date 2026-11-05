BEGIN;
DO $$ DECLARE p record; c record; r text; t uuid := '00000000-0000-4000-8000-000000000001'; BEGIN
 SELECT object_key,storage_driver,storage_namespace,storage_container INTO p FROM attach_pending
 WHERE storage_locator_sha256 IS NOT NULL AND storage_driver='garage' LIMIT 1;
 IF p.object_key IS NULL THEN RAISE EXCEPTION 'fixture missing'; END IF;
 INSERT INTO attach_direct_cleanup(object_key,physical_key,target_driver,target_namespace,target_container,reason)
 VALUES (p.object_key,'direct-s/cleanup-267-synthetic',p.storage_driver,p.storage_namespace,p.storage_container,'test');
 SELECT * INTO c FROM fn_dv1_tombstone_claim(t);
 IF c.out_physical_key <> 'direct-s/cleanup-267-synthetic' OR c.out_locator IS NULL THEN RAISE EXCEPTION 'claim mismatch'; END IF;
 SELECT fn_dv1_cleanup_ack(c.out_id,t,false) INTO r;
 IF r <> 'retry_wait' OR EXISTS (SELECT 1 FROM attach_direct_cleanup WHERE id=c.out_id AND lease_until IS NOT NULL) THEN RAISE EXCEPTION 'retry mismatch'; END IF;
 UPDATE attach_direct_cleanup SET next_scan_at=clock_timestamp()-interval '1 second' WHERE id=c.out_id;
 SELECT * INTO c FROM fn_dv1_tombstone_claim(t);
 SELECT fn_dv1_tombstone_ack(c.out_id,t,true) INTO r;
 IF r <> 'done' THEN RAISE EXCEPTION 'ack mismatch'; END IF;
 UPDATE attach_direct_cleanup SET next_scan_at=clock_timestamp()-interval '1 second' WHERE id=c.out_id;
 SELECT * INTO c FROM fn_dv1_tombstone_claim(t);
 IF c.out_id IS NULL THEN RAISE EXCEPTION 'tombstone not rescanned'; END IF;
 UPDATE attach_direct_cleanup SET lease_until=clock_timestamp()-interval '1 second' WHERE id=c.out_id;
 SELECT * INTO c FROM fn_dv1_tombstone_claim('00000000-0000-4000-8000-000000000003');
 IF c.out_id IS NULL THEN RAISE EXCEPTION 'expired lease not reclaimed'; END IF;
 SELECT fn_dv1_tombstone_ack(c.out_id,t,true) INTO r;
 IF r <> 'stale_token' THEN RAISE EXCEPTION 'old token accepted'; END IF;
 SELECT fn_dv1_tombstone_ack(c.out_id,'00000000-0000-4000-8000-000000000002',true) INTO r;
 IF r <> 'stale_token' THEN RAISE EXCEPTION 'stale accepted'; END IF;
END $$;
DO $$ DECLARE a record; p record; c record; rejected boolean := false; BEGIN
 SELECT * INTO a FROM attach_direct_attempt WHERE attempt_state='WINNER' LIMIT 1;
 IF a.id IS NULL THEN RAISE EXCEPTION 'winner fixture missing'; END IF;
 SELECT * INTO p FROM attach_pending WHERE object_key=a.object_key;
 INSERT INTO attach_direct_cleanup(object_key,physical_key,target_driver,target_namespace,target_container,reason)
 VALUES (a.object_key,a.target_key,a.target_driver,a.target_namespace,a.target_container,'winner-guard-test');
 SELECT * INTO c FROM fn_dv1_tombstone_claim('00000000-0000-4000-8000-000000000004');
 IF c.out_id IS NOT NULL THEN RAISE EXCEPTION 'winner claimed'; END IF;
 BEGIN
  INSERT INTO attach_direct_attempt(object_key,attempt_no,attempt_state,source_key,target_key,
   target_driver,target_namespace,target_container,profile_digest,dispatch_cas_uuid)
  VALUES(a.object_key,3,'CREATED',a.source_key,a.target_key,a.target_driver,a.target_namespace,
         a.target_container,a.profile_digest,gen_random_uuid());
 EXCEPTION WHEN unique_violation THEN rejected := true; END;
 IF NOT rejected THEN RAISE EXCEPTION 'physical key reused'; END IF;
END $$;
ROLLBACK;
