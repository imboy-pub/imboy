LOCK TABLE public.attachment_object_deletion, public.enterprise_asset_delete_queue, public.attach_direct_cleanup, public.cross_domain_delete_attempt, public.enterprise_direct_retirement_audit IN ACCESS EXCLUSIVE MODE;
DO $$ BEGIN
 IF EXISTS (SELECT 1 FROM public.attachment_object_deletion) OR EXISTS (SELECT 1 FROM public.enterprise_asset_delete_queue) OR EXISTS (SELECT 1 FROM public.attach_direct_cleanup) OR EXISTS (SELECT 1 FROM public.cross_domain_delete_attempt) THEN
  RAISE EXCEPTION 'permanent physical tombstones prevent downgrade';
 END IF;
END $$;
DO $migration$
DECLARE d text; old text; new text;
BEGIN
 SELECT pg_get_functiondef('public.fn_v5_claim_ordinary(uuid)'::regprocedure) INTO d;
 old:='SELECT x.path FROM public.attachment_object_deletion x' || E'\n' || '                        WHERE NOT public.fn_cross_domain_live_reference(coalesce(x.storage_physical_key,x.path),x.storage_locator_sha256,x.storage_container,''ordinary'',x.path) AND attempts-attempt_base < 8';
 new:='SELECT path FROM public.attachment_object_deletion' || E'\n' || '                        WHERE attempts-attempt_base < 8';
 IF position(old IN d)=0 THEN RAISE EXCEPTION 'unexpected ordinary claim contract'; END IF; EXECUTE replace(d,old,new);
 SELECT pg_get_functiondef('public.fn_v5_claim_eb(uuid)'::regprocedure) INTO d;
 old:='FROM enterprise_asset_delete_queue x' || E'\n' || '                  WHERE NOT public.fn_cross_domain_live_reference(coalesce(x.storage_physical_key,x.object_key),x.storage_locator_sha256,x.storage_container,''enterprise'',x.object_key) AND attempts < 8';
 new:='FROM enterprise_asset_delete_queue' || E'\n' || '                  WHERE attempts < 8';
 IF position(old IN d)=0 THEN RAISE EXCEPTION 'unexpected enterprise claim contract'; END IF; EXECUTE replace(d,old,new);
 SELECT pg_get_functiondef('public.fn_v5_claim_eb(uuid,bigint,bigint)'::regprocedure) INTO d;
 old:=E'\n' || '          AND NOT public.fn_cross_domain_live_reference(coalesce(q.storage_physical_key,q.object_key),q.storage_locator_sha256,q.storage_container,''enterprise'',q.object_key)';
 IF position(old IN d)=0 THEN RAISE EXCEPTION 'unexpected scoped claim contract'; END IF; EXECUTE replace(d,old,'');
 SELECT pg_get_functiondef('public.fn_dv1_tombstone_claim(uuid)'::regprocedure) INTO d;
 old:=E'\n' || ' AND NOT public.fn_cross_domain_live_reference(c.physical_key,c.locator_sha256,c.target_container,''direct'',c.object_key)';
 IF position(old IN d)=0 THEN RAISE EXCEPTION 'unexpected direct claim contract'; END IF;
 d:=replace(d,old,'');
 old:=$anchor$ ORDER BY c.next_scan_at,c.id FOR UPDATE SKIP LOCKED LIMIT 1;$anchor$;
 new:=$legacy$ AND NOT EXISTS (SELECT 1 FROM public.attach_direct_attempt a
   WHERE a.target_key=c.physical_key AND a.attempt_state='WINNER')
 AND NOT EXISTS (SELECT 1 FROM public.attach_direct_attempt a
   JOIN public.attach_pending p ON p.object_key=a.object_key
   WHERE c.physical_key IN (a.source_key,a.target_key)
     AND p.direct_state IN ('RESERVED','DECLARED','GRANTED','FINALIZING')
     AND a.attempt_state <> 'GC_CLAIMED')
 AND NOT EXISTS (SELECT 1 FROM public.attachment a
   WHERE a.storage_physical_key=c.physical_key AND a.lifecycle_state <> 'deleted')
 ORDER BY c.next_scan_at,c.id FOR UPDATE SKIP LOCKED LIMIT 1;$legacy$;
 IF position(old IN d)=0 THEN RAISE EXCEPTION 'unexpected direct legacy anchor'; END IF; EXECUTE replace(d,old,new);
END $migration$;
DROP FUNCTION public.fn_cross_domain_live_reference(text,text,text,text,text);
REVOKE SELECT (object_key, source_key, target_key, target_driver, target_namespace, target_container, attempt_state)
    ON public.attach_direct_attempt FROM v5_fn_owner;
DROP TRIGGER aa_cross_attachment_guard ON public.attachment;
DROP TRIGGER aa_cross_enterprise_guard ON public.enterprise_asset;
DROP TRIGGER aa_cross_pending_guard ON public.attach_pending;
DROP TRIGGER aa_cross_attempt_guard ON public.attach_direct_attempt;
DROP FUNCTION public.fn_cross_domain_resource_guard();
DROP FUNCTION public.fn_cross_domain_key_guard(text,text,text);
REVOKE SELECT (physical_key,target_container,locator_sha256) ON public.attach_direct_cleanup FROM v5_fn_owner;
DROP TRIGGER aa_cross_ordinary_queue_lock ON public.attachment_object_deletion;
DROP TRIGGER aa_cross_enterprise_queue_lock ON public.enterprise_asset_delete_queue;
DROP TRIGGER aa_cross_direct_queue_lock ON public.attach_direct_cleanup;
DROP FUNCTION public.fn_cross_domain_queue_lock();
DROP TRIGGER aa_cross_ordinary_queue_immutable ON public.attachment_object_deletion;
DROP TRIGGER aa_cross_enterprise_queue_immutable ON public.enterprise_asset_delete_queue;
DROP TRIGGER aa_cross_direct_queue_immutable ON public.attach_direct_cleanup;
DROP FUNCTION public.fn_cross_domain_queue_immutable();
DROP TRIGGER aa_cross_ordinary_claim_guard ON public.attachment_object_deletion;
DROP TRIGGER aa_cross_enterprise_claim_guard ON public.enterprise_asset_delete_queue;
DROP TRIGGER aa_cross_direct_claim_guard ON public.attach_direct_cleanup;
DROP FUNCTION public.fn_cross_domain_claim_guard();
DROP TRIGGER aa_cross_enterprise_hold_guard ON public.enterprise_retention_hold;
DROP FUNCTION public.fn_cross_enterprise_hold_guard();
DROP TRIGGER aa_cross_ordinary_claim_finish ON public.attachment_object_deletion;
DROP TRIGGER aa_cross_enterprise_claim_finish ON public.enterprise_asset_delete_queue;
DROP TRIGGER aa_cross_direct_claim_finish ON public.attach_direct_cleanup;
DROP FUNCTION public.fn_cross_domain_claim_finish();
DROP FUNCTION public.fn_cross_delete_begin(text,text,uuid,text,text,text);
DROP FUNCTION public.fn_cross_delete_finish(text,text,uuid,boolean);
DROP FUNCTION public.fn_cross_delete_unresolved(text,text,text);
DROP TABLE public.cross_domain_delete_attempt;

DROP TRIGGER aa_cross_enterprise_queue_scope ON public.enterprise_asset_delete_queue;
DROP FUNCTION public.fn_cross_enterprise_queue_scope();
ALTER TABLE public.enterprise_asset_delete_queue DROP COLUMN cross_scope_known, DROP COLUMN cross_conversation_id, DROP COLUMN cross_message_id;

DROP TRIGGER aa_cross_direct_retirement_scope ON public.enterprise_direct_retirement_audit;
DROP FUNCTION public.fn_cross_direct_retirement_scope();
REVOKE SELECT ON public.enterprise_direct_retirement_audit FROM v5_fn_owner;
ALTER TABLE public.enterprise_direct_retirement_audit DROP COLUMN cross_scope_known, DROP COLUMN cross_conversation_id, DROP COLUMN cross_message_id;
