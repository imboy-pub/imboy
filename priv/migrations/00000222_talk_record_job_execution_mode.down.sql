-- Migration runner wraps this whole script in one transaction. Lock before the check
-- so no concurrent client insertion can lose its custody when the column is removed.
LOCK TABLE public.talk_record_job IN ACCESS EXCLUSIVE MODE;
-- Never erase device execution provenance and silently admit it to an old worker.
DO $$
BEGIN
 IF EXISTS(SELECT 1 FROM public.talk_record_job WHERE execution_mode='client_byok') THEN
  RAISE EXCEPTION 'cannot remove execution_mode while client_byok jobs exist' USING ERRCODE='23514';
 END IF;
END;
$$;
DROP TRIGGER talk_record_job_execution_mode_immutable ON public.talk_record_job;
DROP FUNCTION public.fn_talk_record_job_execution_mode_immutable();
ALTER TABLE public.talk_record_job DROP CONSTRAINT ck_trj_client_kind,
 DROP CONSTRAINT ck_trj_execution_mode, DROP COLUMN execution_mode;
