-- Route device-owned processing away from the server worker and generic reaper.
ALTER TABLE public.talk_record_job
 ADD COLUMN execution_mode text NOT NULL DEFAULT 'server_local',
 ADD CONSTRAINT ck_trj_execution_mode CHECK (execution_mode IN ('server_local','client_byok')),
 ADD CONSTRAINT ck_trj_client_kind CHECK (execution_mode <> 'client_byok' OR kind IN ('transcribe','summarize'));

CREATE FUNCTION public.fn_talk_record_job_execution_mode_immutable() RETURNS trigger
 LANGUAGE plpgsql SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
 IF NEW.execution_mode IS DISTINCT FROM OLD.execution_mode THEN
  RAISE EXCEPTION 'talk_record_job execution_mode is immutable' USING ERRCODE='23514';
 END IF;
 RETURN NEW;
END;
$$;
CREATE TRIGGER talk_record_job_execution_mode_immutable BEFORE UPDATE ON public.talk_record_job
 FOR EACH ROW EXECUTE FUNCTION public.fn_talk_record_job_execution_mode_immutable();
COMMENT ON COLUMN public.talk_record_job.execution_mode IS
 'Execution custody: server_local for existing jobs; client_byok requires the separate device protocol, never generic retry/reaper';
