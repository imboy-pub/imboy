BEGIN;
DO $$ BEGIN
  IF has_function_privilege('v5_app_runtime','public.fn_eb_direct_worker_heartbeat(text,text,text)','EXECUTE') OR
     has_function_privilege('v5_app_runtime','public.fn_eb_direct_worker_offline(text,text,text)','EXECUTE') OR
     has_table_privilege('v5_app_runtime','public.enterprise_direct_worker_heartbeat','SELECT') OR
     has_function_privilege('eb_direct_proof_worker','public.fn_eb_direct_worker_ready(text,text)','EXECUTE') OR
     has_function_privilege('v5_app_runtime','public.fn_eb_direct_scan_candidates_for_location(text,text,integer)','EXECUTE') THEN
    RAISE EXCEPTION 'heartbeat privilege violation';
  END IF;
END $$;
SET LOCAL ROLE v5_app_runtime;
DO $$ BEGIN
  IF public.fn_eb_direct_worker_ready('ns-a',repeat('a',64)) THEN RAISE EXCEPTION 'missing heartbeat accepted'; END IF;
END $$;
RESET ROLE;
SET LOCAL ROLE eb_direct_proof_worker;
SELECT public.fn_eb_direct_worker_heartbeat('ns-a',repeat('a',64),repeat('1',32));
SELECT public.fn_eb_direct_worker_heartbeat('ns-b',repeat('b',64),repeat('2',32));
RESET ROLE;
SET LOCAL ROLE v5_app_runtime;
DO $$ BEGIN
  IF NOT public.fn_eb_direct_worker_ready('ns-a',repeat('a',64)) OR
     NOT public.fn_eb_direct_worker_ready('ns-b',repeat('b',64)) OR
     public.fn_eb_direct_worker_ready('ns-a',repeat('b',64)) OR
     public.fn_eb_direct_worker_ready('ns-c',repeat('a',64)) THEN
     RAISE EXCEPTION 'location isolation failed';
  END IF;
END $$;
RESET ROLE;
SET LOCAL ROLE eb_direct_proof_worker;
SELECT public.fn_eb_direct_worker_offline('ns-a',repeat('a',64),repeat('1',32));
RESET ROLE;
SET LOCAL ROLE v5_app_runtime;
DO $$ BEGIN
  IF public.fn_eb_direct_worker_ready('ns-a',repeat('a',64)) OR
     NOT public.fn_eb_direct_worker_ready('ns-b',repeat('b',64)) THEN
     RAISE EXCEPTION 'offline leaked across namespaces';
  END IF;
END $$;
RESET ROLE;
UPDATE public.enterprise_direct_worker_heartbeat SET seen_at=clock_timestamp()-interval '91 seconds';
SET LOCAL ROLE v5_app_runtime;
DO $$ BEGIN
  IF public.fn_eb_direct_worker_ready('ns-b',repeat('b',64)) THEN RAISE EXCEPTION 'stale heartbeat accepted'; END IF;
END $$;
RESET ROLE;
ROLLBACK;
