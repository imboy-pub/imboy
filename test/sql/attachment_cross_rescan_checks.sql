-- Run only on an isolated migrated test database as a role allowed to SET ROLE v5_worker.
\set ON_ERROR_STOP on
BEGIN;
UPDATE public.attachment_physical_rescan SET next_scan_at='infinity';
INSERT INTO public.attachment(id,path,storage_driver,storage_namespace,storage_container,storage_locator_sha256,storage_physical_key,lifecycle_state)
VALUES(900000000080001,'rescan-synthetic-shared','garage','test','test',repeat('a',64),'rescan-synthetic-shared','live');
INSERT INTO public.attachment_physical_rescan(domain,source_ref,owner_key,driver,namespace,container,locator,physical_key,next_scan_at)
VALUES('enterprise','test-sole','retired','garage','test','test',repeat('a',64),'rescan-synthetic-shared',clock_timestamp());
SET ROLE v5_worker;
DO $$ BEGIN IF EXISTS(SELECT 1 FROM public.fn_cross_rescan_claim('11111111-1111-4111-8111-111111111111')) THEN RAISE EXCEPTION 'live ref was claimed'; END IF; END $$;
RESET ROLE;
UPDATE public.attachment SET lifecycle_state='deleted' WHERE id=900000000080001;
UPDATE public.attachment_physical_rescan SET next_scan_at=clock_timestamp() WHERE source_ref='test-sole';
SET ROLE v5_worker;
DO $$ DECLARE id bigint; r text; BEGIN
 SELECT out_id INTO id FROM public.fn_cross_rescan_claim('11111111-1111-4111-8111-111111111111');
 IF id IS NULL THEN RAISE EXCEPTION 'eligible tombstone not claimed'; END IF;
 SELECT public.fn_cross_rescan_ack(id,'22222222-2222-4222-8222-222222222222',true) INTO r;
 IF r <> 'stale_token' THEN RAISE EXCEPTION 'wrong token accepted'; END IF;
 SELECT public.fn_cross_rescan_ack(id,'11111111-1111-4111-8111-111111111111',true) INTO r;
 IF r <> 'ready' THEN RAISE EXCEPTION 'ack failed'; END IF;
END $$;
RESET ROLE;
DO $$ BEGIN IF NOT EXISTS(SELECT 1 FROM public.attachment_physical_rescan WHERE source_ref='test-sole' AND state='ready' AND next_scan_at>clock_timestamp()) THEN RAISE EXCEPTION 'rescan not rescheduled'; END IF; END $$;
ROLLBACK;

\set ON_ERROR_STOP on
BEGIN;
UPDATE public.attachment_physical_rescan SET next_scan_at='infinity';
INSERT INTO public.attachment(id,path,storage_driver,storage_namespace,storage_container,storage_locator_sha256,storage_physical_key,lifecycle_state)
 SELECT 900000000081000+n, 'rescan-block-'||n, 'garage','test','test',repeat('a',64),'rescan-block-'||n,'live' FROM generate_series(1,100) n;
INSERT INTO public.attachment_physical_rescan(domain,source_ref,owner_key,driver,namespace,container,locator,physical_key,next_scan_at)
 SELECT 'enterprise','block-'||n,'retired','garage','test','test',repeat('a',64),'rescan-block-'||n,clock_timestamp()-interval '2 hours' FROM generate_series(1,100) n;
INSERT INTO public.attachment_physical_rescan(domain,source_ref,owner_key,driver,namespace,container,locator,physical_key,next_scan_at)
VALUES('enterprise','eligible','retired','garage','test','test',repeat('a',64),'rescan-eligible',clock_timestamp()-interval '1 hour');
SET ROLE v5_worker;
DO $$ DECLARE k text; BEGIN
 IF EXISTS(SELECT 1 FROM public.fn_cross_rescan_claim('11111111-1111-4111-8111-111111111111')) THEN RAISE EXCEPTION 'blocked rows claimed'; END IF;
 SELECT out_physical_key INTO k FROM public.fn_cross_rescan_claim('22222222-2222-4222-8222-222222222222');
 IF k IS DISTINCT FROM 'rescan-eligible' THEN RAISE EXCEPTION 'eligible row starved: %',k; END IF;
END $$;
ROLLBACK;

BEGIN;
UPDATE public.attachment_physical_rescan SET next_scan_at='infinity';
INSERT INTO public.attachment_physical_rescan(domain,source_ref,owner_key,driver,namespace,container,locator,physical_key,next_scan_at)
VALUES('enterprise','lease-recovery','retired','garage','test','test',repeat('a',64),'rescan-lease-recovery',clock_timestamp());
SET ROLE v5_worker;
DO $$ BEGIN
 IF NOT EXISTS(SELECT 1 FROM public.fn_cross_rescan_claim('11111111-1111-4111-8111-111111111111')) THEN RAISE EXCEPTION 'first lease missing'; END IF;
END $$;
RESET ROLE;
UPDATE public.attachment_physical_rescan SET lease_until=clock_timestamp()-interval '1 second' WHERE source_ref='lease-recovery';
SET ROLE v5_worker;
DO $$ DECLARE id bigint; BEGIN
 SELECT out_id INTO id FROM public.fn_cross_rescan_claim('22222222-2222-4222-8222-222222222222');
 IF id IS NULL THEN RAISE EXCEPTION 'expired lease not recovered'; END IF;
 IF public.fn_cross_rescan_ack(id,'11111111-1111-4111-8111-111111111111',true)<>'stale_token' THEN RAISE EXCEPTION 'stale lease accepted'; END IF;
 IF public.fn_cross_rescan_ack(id,'22222222-2222-4222-8222-222222222222',true)<>'ready' THEN RAISE EXCEPTION 'recovered lease ack failed'; END IF;
END $$;
RESET ROLE;
DO $$ BEGIN
 IF public.fn_cross_delete_unresolved('rescan-lease-recovery',repeat('a',64),'test') IS NOT TRUE THEN
  RAISE EXCEPTION 'successful newer attempt incorrectly cleared older unknown';
 END IF;
END $$;
ROLLBACK;

BEGIN;
UPDATE public.attachment_physical_rescan SET next_scan_at='infinity';
INSERT INTO public.attachment_physical_rescan(domain,source_ref,owner_key,driver,namespace,container,locator,physical_key,next_scan_at)
VALUES('enterprise','failed-delete','retired','garage','test','test',repeat('a',64),'rescan-failed-delete',clock_timestamp());
SET ROLE v5_worker;
DO $$ DECLARE id bigint; BEGIN
 SELECT out_id INTO id FROM public.fn_cross_rescan_claim('33333333-3333-4333-8333-333333333333');
 IF id IS NULL THEN RAISE EXCEPTION 'first claim missing'; END IF;
 IF public.fn_cross_rescan_ack(id,'33333333-3333-4333-8333-333333333333',false)<>'ready' THEN
  RAISE EXCEPTION 'failed attempt ack failed';
 END IF;
END $$;
RESET ROLE;
DO $$ BEGIN
 IF public.fn_cross_delete_unresolved('rescan-failed-delete',repeat('a',64),'test') IS NOT TRUE THEN
  RAISE EXCEPTION 'failed delete did not stay unknown';
 END IF;
END $$;
UPDATE public.attachment_physical_rescan SET next_scan_at=clock_timestamp() WHERE source_ref='failed-delete';
SET ROLE v5_worker;
DO $$ DECLARE id bigint; BEGIN
 SELECT out_id INTO id FROM public.fn_cross_rescan_claim('44444444-4444-4444-8444-444444444444');
 IF id IS NULL THEN RAISE EXCEPTION 'retry claim missing'; END IF;
 IF public.fn_cross_rescan_ack(id,'44444444-4444-4444-8444-444444444444',true)<>'ready' THEN
  RAISE EXCEPTION 'retry ack failed';
 END IF;
END $$;
RESET ROLE;
DO $$ BEGIN
 IF public.fn_cross_delete_unresolved('rescan-failed-delete',repeat('a',64),'test') IS NOT TRUE THEN
  RAISE EXCEPTION 'retry success erased earlier unknown';
 END IF;
END $$;
ROLLBACK;
