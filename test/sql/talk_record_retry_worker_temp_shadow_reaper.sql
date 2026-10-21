-- Controller checks happen before SET ROLE; no new production privileges.
\set ON_ERROR_STOP on
BEGIN;
DO $$ BEGIN
 IF EXISTS(SELECT 1 FROM public.attachment_object_deletion
   WHERE job_state IN ('ready','retry_wait') AND attempts-attempt_base<8 AND next_retry_at<=now())
 OR EXISTS(SELECT 1 FROM public.attachment_object_deletion
   WHERE job_state='leased' AND lease_until<now()) THEN
  RAISE EXCEPTION 'CONTROLLER_PREREQUISITE: dedicated queue not quiescent'; END IF;
 IF EXISTS(SELECT 1 FROM public.attachment_object_deletion
   WHERE path LIKE 't20-temp-shadow-%') THEN
  RAISE EXCEPTION 'CONTROLLER_PREREQUISITE: collision with real path'; END IF;
END $$;
SET LOCAL ROLE v5_worker;
DO $$ BEGIN
 IF current_user<>'v5_worker' OR (SELECT rolsuper FROM pg_roles WHERE rolname=current_user) THEN
  RAISE EXCEPTION 'CONTROLLER_PREREQUISITE: non-superuser worker effective role required'; END IF;
END $$;
-- Explicit columns: worker never SELECTs/LIKEs the real queue or audit tables.
CREATE TEMP TABLE shadow_probe(actor text);
CREATE TEMP TABLE attachment_object_deletion (
 path text,bucket text,job_state text,next_retry_at timestamptz,attempts integer,
 attempt_base integer DEFAULT 0,retry_generation bigint DEFAULT 0,
 lease_token uuid,lease_until timestamptz,last_error_code text,completed_at timestamptz,
 storage_driver text,storage_namespace text,storage_container text,
 storage_locator_sha256 text,storage_physical_key text);
CREATE TEMP TABLE attachment_lifecycle_v5_audit (actor text,queue_domain text,object_key text,
 action text,reason text,storage_namespace text,details jsonb);
GRANT SELECT,UPDATE ON pg_temp.attachment_object_deletion TO v5_fn_owner;
GRANT INSERT ON pg_temp.attachment_lifecycle_v5_audit TO v5_fn_owner;
GRANT INSERT ON pg_temp.shadow_probe TO v5_fn_owner;
INSERT INTO pg_temp.attachment_object_deletion(path,bucket,job_state,next_retry_at,attempts,
 storage_driver,storage_namespace,storage_container,storage_locator_sha256,lease_token,lease_until)
 VALUES('t20-temp-shadow-reaper','owned','leased',now(),1,
 'disk','owned','owned',repeat('a',64),'11111111-2222-4333-8444-555555555555',now()-interval '1 minute');
CREATE FUNCTION pg_temp.shadow_attack() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN INSERT INTO pg_temp.shadow_probe VALUES(TG_TABLE_NAME||':'||current_user); RETURN NEW; END $$;
CREATE TRIGGER attack_queue BEFORE UPDATE ON pg_temp.attachment_object_deletion
 FOR EACH ROW EXECUTE FUNCTION pg_temp.shadow_attack();
CREATE TRIGGER attack_audit BEFORE INSERT ON pg_temp.attachment_lifecycle_v5_audit
 FOR EACH ROW EXECUTE FUNCTION pg_temp.shadow_attack();
DO $$ DECLARE n integer; state text; BEGIN
 n:=public.fn_v5_reaper_ordinary(100);
 IF n<>0 THEN
  IF NOT EXISTS(SELECT 1 FROM pg_temp.shadow_probe WHERE actor='attachment_object_deletion:v5_fn_owner') THEN
   RAISE EXCEPTION 'MUTANT_INCOMPLETE: target did not exercise malicious queue trigger'; END IF;
  RAISE EXCEPTION 'SHADOW_ORACLE: target read attacker queue and executed malicious trigger'; END IF;
 IF EXISTS(SELECT 1 FROM pg_temp.shadow_probe) THEN
  RAISE EXCEPTION 'SHADOW_ORACLE: definer executed attacker trigger'; END IF;
 IF EXISTS(SELECT 1 FROM pg_temp.attachment_lifecycle_v5_audit) THEN
  RAISE EXCEPTION 'SHADOW_ORACLE: definer wrote attacker audit'; END IF;
 IF (SELECT job_state FROM pg_temp.attachment_object_deletion)<>'leased'
  OR (SELECT attempts FROM pg_temp.attachment_object_deletion)<>1 THEN
  RAISE EXCEPTION 'SHADOW_ORACLE: attacker row mutated'; END IF;
END $$;
ROLLBACK;
