\set ON_ERROR_STOP on
\set EB_DIRECT_KEEP_FIXTURE 1
\ir eb_direct_reservation_pg.sql
-- Synthetic existing winners; real projection and scan still recheck their fence.
SET LOCAL session_replication_role = replica;
UPDATE public.attach_pending SET storage_namespace='proof-other'
 WHERE object_key='enterprise/test/eb-direct-visitor';
UPDATE public.attachment SET storage_namespace='proof-other' WHERE id=99006020;
SET LOCAL session_replication_role = origin;
SET LOCAL ROLE eb_direct_proof_worker;
DO $$ DECLARE v bigint; BEGIN
    SELECT asset_id INTO v FROM public.fn_eb_direct_scan_candidates_for_location('proof-other',repeat('b',64),1);
    IF v IS DISTINCT FROM 99006006 THEN RAISE EXCEPTION 'namespace filter must precede limit'; END IF;
    IF EXISTS (SELECT 1 FROM public.fn_eb_direct_scan_candidates_for_location('proof-other',repeat('c',64),20)) THEN
        RAISE EXCEPTION 'different frozen locator must not be scanned';
    END IF;
END $$;
RESET ROLE;
ROLLBACK;
