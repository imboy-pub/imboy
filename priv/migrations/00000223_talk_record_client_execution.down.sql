LOCK TABLE public.talk_record_client_run,public.talk_record_client_run_part IN ACCESS EXCLUSIVE MODE;
DO $$ BEGIN
 IF EXISTS(SELECT 1 FROM public.talk_record_client_run) THEN
  RAISE EXCEPTION 'client execution history requires explicit retention decision; rollback refused';
 END IF;
END $$;
DROP TRIGGER talk_record_client_job_cancel ON public.talk_record_job;
DROP FUNCTION public.fn_talk_record_client_job_cancel();
DROP TABLE public.talk_record_client_run_part;
DROP TABLE public.talk_record_client_run;
DROP FUNCTION public.fn_talk_record_client_run_guard();
DROP FUNCTION public.fn_talk_record_client_part_guard();
ALTER TABLE public.talk_record_job DROP CONSTRAINT uq_trj_record_id;

DROP INDEX public.uq_trj_client_input;
