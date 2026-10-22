-- Device-owned execution metadata; provider credentials never enter this schema.
-- One logical client job per frozen stage/input across all idempotency keys/devices.
CREATE UNIQUE INDEX uq_trj_client_input ON public.talk_record_job
 (record_id,kind,input_manifest_hash,input_content_revision,coalesce(input_transcript_version,0))
 WHERE execution_mode='client_byok';
ALTER TABLE public.talk_record_job ADD CONSTRAINT uq_trj_record_id UNIQUE(record_id,id);
CREATE TABLE public.talk_record_client_run (
 id bigint PRIMARY KEY,
 record_id bigint NOT NULL,
 job_id bigint NOT NULL,
 attempt integer NOT NULL CHECK(attempt > 0),
 actor_user_id bigint NOT NULL REFERENCES public."user"(id) ON DELETE RESTRICT,
 device_session_id varchar(128) NOT NULL CHECK(length(device_session_id) > 0),
 lease_token_hash char(64) NOT NULL CHECK(lease_token_hash ~ '^[0-9a-f]{64}$'),
 lease_until timestamptz NOT NULL,
 provider varchar(32) NOT NULL CHECK(provider IN ('minimax','zhipu')),
 model varchar(64) NOT NULL,
 policy_version varchar(128) NOT NULL,
 config_version char(64) NOT NULL CHECK(config_version ~ '^[0-9a-f]{64}$'),
 consent_event_id bigint NOT NULL REFERENCES public.talk_record_consent_event(id) ON DELETE RESTRICT,
 input_manifest_hash char(64) NOT NULL CHECK(input_manifest_hash ~ '^[0-9a-f]{64}$'),
 input_content_revision bigint NOT NULL CHECK(input_content_revision > 0),
 input_transcript_version integer,
 input_snapshot jsonb NOT NULL CHECK(jsonb_typeof(input_snapshot)='object'),
 replaces_run_id bigint REFERENCES public.talk_record_client_run(id) ON DELETE RESTRICT,
 ack_possible_duplicate_charge boolean NOT NULL DEFAULT false,
 chunk_plan_hash char(64) CHECK(chunk_plan_hash ~ '^[0-9a-f]{64}$'),
 state text NOT NULL DEFAULT 'active'
  CHECK(state IN ('active','complete','failed','provider_outcome_unknown','cancelled','expired')),
 revision bigint NOT NULL DEFAULT 1 CHECK(revision > 0),
 created_at timestamptz NOT NULL DEFAULT now(),
 updated_at timestamptz NOT NULL DEFAULT now(),
 UNIQUE(job_id,attempt), UNIQUE(record_id,id),
 CHECK(ack_possible_duplicate_charge = (replaces_run_id IS NOT NULL)),
 FOREIGN KEY(record_id,job_id) REFERENCES public.talk_record_job(record_id,id) ON DELETE RESTRICT
);
CREATE UNIQUE INDEX uq_trcr_active ON public.talk_record_client_run(job_id) WHERE state='active';
CREATE INDEX i_trcr_expiry ON public.talk_record_client_run(lease_until) WHERE state='active';
CREATE TABLE public.talk_record_client_run_part (
 run_id bigint NOT NULL REFERENCES public.talk_record_client_run(id) ON DELETE RESTRICT,
 seq integer NOT NULL CHECK(seq >= 0),
 source_part_manifest_hash char(64) NOT NULL CHECK(source_part_manifest_hash ~ '^[0-9a-f]{64}$'),
 source_ranges jsonb NOT NULL CHECK(jsonb_typeof(source_ranges)='array'),
 offset_ms bigint NOT NULL CHECK(offset_ms >= 0),
 duration_ms bigint NOT NULL CHECK(duration_ms >= 0),
 byte_size bigint NOT NULL CHECK(byte_size >= 0),
 dispatch_intent_id bigint,
 request_hash char(64) CHECK(request_hash ~ '^[0-9a-f]{64}$'),
 provider_state text NOT NULL DEFAULT 'not_dispatched'
  CHECK(provider_state IN ('not_dispatched','dispatching','confirmed','provider_outcome_unknown','rejected')),
 result_hash char(64) CHECK(result_hash ~ '^[0-9a-f]{64}$'),
 result jsonb CHECK(result IS NULL OR jsonb_typeof(result)='object'),
 provider_request_id varchar(128),
 reused_from_run_id bigint,
 reused_from_seq integer,
 revision bigint NOT NULL DEFAULT 1 CHECK(revision > 0),
 PRIMARY KEY(run_id,seq),
 FOREIGN KEY(reused_from_run_id,reused_from_seq)
  REFERENCES public.talk_record_client_run_part(run_id,seq) ON DELETE RESTRICT,
 CHECK((provider_state='not_dispatched' AND dispatch_intent_id IS NULL AND request_hash IS NULL)
  OR (provider_state<>'not_dispatched' AND dispatch_intent_id IS NOT NULL AND request_hash IS NOT NULL)),
 CHECK(provider_state<>'confirmed' OR (result_hash IS NOT NULL AND result IS NOT NULL))
);
CREATE FUNCTION public.fn_talk_record_client_run_guard() RETURNS trigger
 LANGUAGE plpgsql SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
 IF TG_OP='INSERT' THEN
  IF NOT EXISTS(SELECT 1 FROM public.talk_record_job j
   JOIN public.talk_record_consent_event ce ON ce.id=NEW.consent_event_id
   WHERE j.id=NEW.job_id AND j.record_id=NEW.record_id AND j.execution_mode='client_byok'
   AND j.state='running' AND j.attempt=NEW.attempt
   AND j.input_manifest_hash=NEW.input_manifest_hash AND j.input_content_revision=NEW.input_content_revision
   AND j.input_transcript_version IS NOT DISTINCT FROM NEW.input_transcript_version
   AND j.processor_input->>'processing_mode'=j.kind
   AND j.processor_input->>'provider'=NEW.provider AND j.processor_input->>'model'=NEW.model
   AND j.processor_input->>'policy_version'=NEW.policy_version
   AND j.processor_input->>'config_version'=NEW.config_version
   AND ce.record_id=NEW.record_id AND ce.actor_id=NEW.actor_user_id
   AND ce.kind='processing_opt_in' AND ce.processing_mode=j.kind
   AND ce.policy_version=NEW.policy_version AND ce.processor_config_version=NEW.config_version
   AND ((j.kind='transcribe' AND NEW.provider='minimax' AND NEW.model='asr-1.0')
    OR (j.kind='summarize' AND NEW.provider='zhipu' AND NEW.model='glm-5.3-flash'))) THEN
   RAISE EXCEPTION 'client execution custody mismatch' USING ERRCODE='23514';
  END IF;
 ELSE
  IF ROW(NEW.job_id,NEW.record_id,NEW.attempt,NEW.actor_user_id,NEW.device_session_id,
   NEW.lease_token_hash,NEW.provider,NEW.model,NEW.policy_version,NEW.config_version,
   NEW.consent_event_id,NEW.input_manifest_hash,NEW.input_content_revision,
   NEW.input_transcript_version,NEW.input_snapshot,NEW.replaces_run_id,NEW.ack_possible_duplicate_charge)
   IS DISTINCT FROM ROW(OLD.job_id,OLD.record_id,OLD.attempt,OLD.actor_user_id,OLD.device_session_id,
   OLD.lease_token_hash,OLD.provider,OLD.model,OLD.policy_version,OLD.config_version,
   OLD.consent_event_id,OLD.input_manifest_hash,OLD.input_content_revision,
   OLD.input_transcript_version,OLD.input_snapshot,OLD.replaces_run_id,OLD.ack_possible_duplicate_charge) THEN
   RAISE EXCEPTION 'client execution pins are immutable' USING ERRCODE='23514';
  END IF;
  IF OLD.chunk_plan_hash IS NOT NULL AND NEW.chunk_plan_hash IS DISTINCT FROM OLD.chunk_plan_hash THEN
   RAISE EXCEPTION 'chunk plan hash is immutable' USING ERRCODE='23514';
  END IF;
  IF OLD.state<>'active' AND NEW IS DISTINCT FROM OLD THEN
   RAISE EXCEPTION 'terminal execution is immutable' USING ERRCODE='23514';
  END IF;
 END IF;
 RETURN NEW;
END; $$;
CREATE TRIGGER talk_record_client_run_guard BEFORE INSERT OR UPDATE ON public.talk_record_client_run
 FOR EACH ROW EXECUTE FUNCTION public.fn_talk_record_client_run_guard();
CREATE FUNCTION public.fn_talk_record_client_part_guard() RETURNS trigger
 LANGUAGE plpgsql SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
 IF ROW(NEW.run_id,NEW.seq,NEW.source_part_manifest_hash,NEW.source_ranges,NEW.offset_ms,
  NEW.duration_ms,NEW.byte_size) IS DISTINCT FROM ROW(OLD.run_id,OLD.seq,
  OLD.source_part_manifest_hash,OLD.source_ranges,OLD.offset_ms,OLD.duration_ms,OLD.byte_size) THEN
  RAISE EXCEPTION 'chunk plan is immutable' USING ERRCODE='23514';
 END IF;
 IF OLD.provider_state IN ('confirmed','provider_outcome_unknown','rejected') AND NEW IS DISTINCT FROM OLD THEN
  RAISE EXCEPTION 'terminal provider fact is immutable' USING ERRCODE='23514';
 END IF;
 IF OLD.dispatch_intent_id IS NOT NULL AND
  ROW(NEW.dispatch_intent_id,NEW.request_hash) IS DISTINCT FROM ROW(OLD.dispatch_intent_id,OLD.request_hash) THEN
  RAISE EXCEPTION 'dispatch intent is immutable' USING ERRCODE='23514';
 END IF;
 IF OLD.provider_state='dispatching' AND NEW.provider_state='not_dispatched' THEN
  RAISE EXCEPTION 'dispatch outcome cannot be reset' USING ERRCODE='23514';
 END IF;
 RETURN NEW;
END; $$;
CREATE TRIGGER talk_record_client_part_guard BEFORE UPDATE ON public.talk_record_client_run_part
 FOR EACH ROW EXECUTE FUNCTION public.fn_talk_record_client_part_guard();
GRANT SELECT,INSERT,UPDATE ON public.talk_record_client_run,public.talk_record_client_run_part TO v5_app_runtime;
GRANT SELECT ON public.talk_record_client_run,public.talk_record_client_run_part TO v5_fn_owner;

-- Every existing deletion/governance cancellation also fences device executions.
-- Preserve a possible paid request as unknown, never as an unstarted cancellation.
CREATE FUNCTION public.fn_talk_record_client_job_cancel() RETURNS trigger
 LANGUAGE plpgsql SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
 UPDATE public.talk_record_client_run r SET
  state=CASE WHEN EXISTS(SELECT 1 FROM public.talk_record_client_run_part p
   WHERE p.run_id=r.id AND p.provider_state IN ('dispatching','provider_outcome_unknown'))
   THEN 'provider_outcome_unknown' ELSE 'cancelled' END,
  revision=revision+1,updated_at=now()
  WHERE job_id=NEW.id AND state='active';
 UPDATE public.talk_record_client_run_part p SET provider_state='provider_outcome_unknown',revision=p.revision+1
  FROM public.talk_record_client_run r WHERE p.run_id=r.id AND r.job_id=NEW.id
   AND p.provider_state='dispatching';
 RETURN NEW;
END; $$;
CREATE TRIGGER talk_record_client_job_cancel AFTER UPDATE OF state ON public.talk_record_job
 FOR EACH ROW WHEN (NEW.execution_mode='client_byok' AND NEW.state='cancelled' AND OLD.state IS DISTINCT FROM NEW.state)
 EXECUTE FUNCTION public.fn_talk_record_client_job_cancel();
