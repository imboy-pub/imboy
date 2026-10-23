-- Durable provider facts contain no credentials, URLs or storage paths.
ALTER TABLE public.talk_record_client_run DROP CONSTRAINT talk_record_client_run_provider_check;
ALTER TABLE public.talk_record_client_run ADD CONSTRAINT talk_record_client_run_provider_check
 CHECK(provider IN ('minimax','doubao','zhipu'));
ALTER TABLE public.talk_record_client_run ADD COLUMN execution_purpose text NOT NULL DEFAULT 'submit'
 CHECK(execution_purpose IN ('submit','query_only'));
ALTER TABLE public.talk_record_client_run ADD COLUMN provider_task_id bigint;
-- Replace only the inherited ack/predecessor CHECK. Historical migration223 stays immutable.
DO $$ DECLARE names name[]; BEGIN
 SELECT array_agg(conname) INTO names FROM pg_constraint
 WHERE conrelid='public.talk_record_client_run'::regclass AND contype='c'
 AND position('ack_possible_duplicate_charge' in pg_get_constraintdef(oid))>0;
 IF cardinality(names) IS DISTINCT FROM 1 THEN
 RAISE EXCEPTION 'unexpected client run acknowledgement constraint'; END IF;
 EXECUTE format('ALTER TABLE public.talk_record_client_run DROP CONSTRAINT %I',names[1]);
END $$;
ALTER TABLE public.talk_record_client_run ADD CONSTRAINT talk_record_client_run_ack_predecessor CHECK
 ((NOT ack_possible_duplicate_charge AND replaces_run_id IS NULL)
 OR (ack_possible_duplicate_charge AND replaces_run_id IS NOT NULL)
 OR (NOT ack_possible_duplicate_charge AND replaces_run_id IS NOT NULL
  AND provider='doubao' AND execution_purpose='submit'));
CREATE OR REPLACE FUNCTION public.fn_talk_record_client_run_guard() RETURNS trigger
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
   AND ((j.kind='transcribe' AND NEW.provider IN ('minimax','doubao') AND
     ((NEW.provider='minimax' AND NEW.model='asr-1.0') OR
      (NEW.provider='doubao' AND NEW.model='volc.seedasr.auc')))
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

CREATE TABLE public.talk_record_provider_task (
 id bigint PRIMARY KEY, record_id bigint NOT NULL, job_id bigint NOT NULL,
 source_run_id bigint NOT NULL, attempt integer NOT NULL CHECK(attempt>0), seq integer NOT NULL CHECK(seq=0),
 actor_user_id bigint NOT NULL, provider text NOT NULL CHECK(provider='doubao'),
 model text NOT NULL CHECK(model='volc.seedasr.auc'), resource_id text NOT NULL CHECK(resource_id='volc.seedasr.auc'),
 model_name text NOT NULL CHECK(model_name='bigmodel'), request_profile jsonb NOT NULL,
 profile_hash char(64) NOT NULL CHECK(profile_hash ~ '^[0-9a-f]{64}$'),
 policy_version text NOT NULL, config_version char(64) NOT NULL, consent_event_id bigint NOT NULL,
 input_manifest_hash char(64) NOT NULL, input_content_revision bigint NOT NULL,
 input_transcript_version integer, assembly jsonb NOT NULL,
 client_request_id uuid NOT NULL UNIQUE, provider_task_id varchar(128),
 source_authorization_id bigint NOT NULL, source_issued_at bigint, source_expires_at bigint,
 intent_id bigint, request_hash char(64), previous_task_id bigint,
 ack_possible_duplicate_charge boolean NOT NULL DEFAULT false,
 state text NOT NULL DEFAULT 'prepared' CHECK(state IN
 ('prepared','submit_outcome_unknown','accepted','processing','result_ready','failed','rejected','published')),
 outcome_unknown_at timestamptz, cancelled_at timestamptz,
 result jsonb, result_hash char(64), query_count integer NOT NULL DEFAULT 0,
 next_query_at timestamptz, revision bigint NOT NULL DEFAULT 1,
 created_at timestamptz NOT NULL DEFAULT now(), updated_at timestamptz NOT NULL DEFAULT now(),
 UNIQUE(source_run_id,seq),
 CONSTRAINT provider_task_source_window CHECK(((source_issued_at IS NULL AND source_expires_at IS NULL AND intent_id IS NULL AND request_hash IS NULL)
 OR (source_issued_at IS NOT NULL AND source_expires_at IS NOT NULL AND intent_id IS NOT NULL AND request_hash IS NOT NULL
 AND source_issued_at>0 AND source_expires_at>source_issued_at AND source_expires_at<=source_issued_at+300
 AND intent_id IS NOT NULL AND request_hash IS NOT NULL)) IS TRUE),
 CHECK(state NOT IN ('accepted','processing','result_ready','published') OR provider_task_id IS NOT NULL),
 CHECK(state NOT IN ('result_ready','published') OR (result IS NOT NULL AND result_hash IS NOT NULL)),
 CHECK(NOT ack_possible_duplicate_charge OR previous_task_id IS NOT NULL),
 CHECK(provider_task_id IS NULL OR provider_task_id ~ '^[A-Za-z0-9_.:-]{1,128}$')
);
-- No attachment FK or new retention reference: current lifecycle authorization remains authoritative.
CREATE INDEX i_trpt_job ON public.talk_record_provider_task(job_id,id);
CREATE FUNCTION public.fn_talk_record_provider_task_guard() RETURNS trigger
 LANGUAGE plpgsql SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
 IF ROW(NEW.id,NEW.record_id,NEW.job_id,NEW.source_run_id,NEW.attempt,NEW.seq,NEW.actor_user_id,
  NEW.provider,NEW.model,NEW.resource_id,NEW.model_name,NEW.request_profile,NEW.profile_hash,
  NEW.policy_version,NEW.config_version,NEW.consent_event_id,NEW.input_manifest_hash,
  NEW.input_content_revision,NEW.input_transcript_version,NEW.assembly,NEW.client_request_id,
  NEW.source_authorization_id,NEW.previous_task_id,NEW.ack_possible_duplicate_charge)
 IS DISTINCT FROM ROW(OLD.id,OLD.record_id,OLD.job_id,OLD.source_run_id,OLD.attempt,OLD.seq,OLD.actor_user_id,
  OLD.provider,OLD.model,OLD.resource_id,OLD.model_name,OLD.request_profile,OLD.profile_hash,
  OLD.policy_version,OLD.config_version,OLD.consent_event_id,OLD.input_manifest_hash,
  OLD.input_content_revision,OLD.input_transcript_version,OLD.assembly,OLD.client_request_id,
  OLD.source_authorization_id,OLD.previous_task_id,OLD.ack_possible_duplicate_charge) THEN
  RAISE EXCEPTION 'provider pins are immutable' USING ERRCODE='23514';
 END IF;
 IF (OLD.provider_task_id IS NOT NULL AND NEW.provider_task_id IS DISTINCT FROM OLD.provider_task_id)
 OR (OLD.intent_id IS NOT NULL AND ROW(NEW.intent_id,NEW.request_hash,NEW.source_issued_at,NEW.source_expires_at)
 IS DISTINCT FROM ROW(OLD.intent_id,OLD.request_hash,OLD.source_issued_at,OLD.source_expires_at))
 OR (OLD.outcome_unknown_at IS NOT NULL AND NEW.outcome_unknown_at IS DISTINCT FROM OLD.outcome_unknown_at)
 OR (OLD.cancelled_at IS NOT NULL AND NEW.cancelled_at IS DISTINCT FROM OLD.cancelled_at)
 OR (OLD.result_hash IS NOT NULL AND ROW(NEW.result_hash,NEW.result) IS DISTINCT FROM ROW(OLD.result_hash,OLD.result)) THEN
  RAISE EXCEPTION 'provider facts are immutable' USING ERRCODE='23514';
 END IF;
 RETURN NEW;
END; $$;
CREATE TRIGGER talk_record_provider_task_guard BEFORE UPDATE ON public.talk_record_provider_task
 FOR EACH ROW EXECUTE FUNCTION public.fn_talk_record_provider_task_guard();
CREATE FUNCTION public.fn_talk_record_async_run_guard() RETURNS trigger
 LANGUAGE plpgsql SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
 IF TG_OP='UPDATE' AND ROW(NEW.execution_purpose,NEW.provider_task_id)
 IS DISTINCT FROM ROW(OLD.execution_purpose,OLD.provider_task_id) THEN
 RAISE EXCEPTION 'execution purpose is immutable' USING ERRCODE='23514'; END IF;
 -- A no-charge predecessor exception is legal only for a latest, exactly pinned
 -- task that never authorized any supplier request, including its original part.
 IF TG_OP='INSERT' AND NEW.provider='doubao' AND NEW.replaces_run_id IS NOT NULL
 AND NOT NEW.ack_possible_duplicate_charge AND NOT EXISTS(
 SELECT 1 FROM public.talk_record_provider_task p
 WHERE p.source_run_id=NEW.replaces_run_id AND p.record_id=NEW.record_id AND p.job_id=NEW.job_id
 AND p.state='prepared' AND p.intent_id IS NULL AND p.request_hash IS NULL
 AND p.provider_task_id IS NULL AND p.source_issued_at IS NULL AND p.source_expires_at IS NULL
 AND p.result IS NULL AND p.result_hash IS NULL AND p.outcome_unknown_at IS NULL AND p.cancelled_at IS NULL
 AND p.input_manifest_hash=NEW.input_manifest_hash AND p.input_content_revision=NEW.input_content_revision
 AND p.input_transcript_version IS NOT DISTINCT FROM NEW.input_transcript_version
 AND p.config_version=NEW.config_version AND p.policy_version=NEW.policy_version
 AND NOT EXISTS(SELECT 1 FROM public.talk_record_provider_task later WHERE later.job_id=p.job_id AND later.id>p.id)
 AND EXISTS(SELECT 1 FROM public.talk_record_client_run_part part WHERE part.run_id=p.source_run_id AND part.seq=0)
 AND NOT EXISTS(SELECT 1 FROM public.talk_record_client_run_part part WHERE part.run_id=p.source_run_id
  AND (part.seq<>0 OR part.provider_state<>'not_dispatched' OR part.dispatch_intent_id IS NOT NULL OR part.request_hash IS NOT NULL))) THEN
 RAISE EXCEPTION 'unstarted preparation predecessor required' USING ERRCODE='23514'; END IF;
 -- Closing an existing run must remain possible after source/task revocation.
 -- Creation, active mutation and publication still require current task authority.
 IF (TG_OP='INSERT' OR NEW.state IN ('active','complete'))
 AND NEW.execution_purpose='query_only' AND NOT EXISTS(SELECT 1 FROM public.talk_record_provider_task t
 WHERE t.id=NEW.provider_task_id AND t.record_id=NEW.record_id AND t.job_id=NEW.job_id
 AND t.provider_task_id IS NOT NULL AND t.cancelled_at IS NULL) THEN
 RAISE EXCEPTION 'query authority missing' USING ERRCODE='23514'; END IF;
 RETURN NEW;
END; $$;
CREATE TRIGGER talk_record_async_run_guard BEFORE INSERT OR UPDATE ON public.talk_record_client_run
 FOR EACH ROW EXECUTE FUNCTION public.fn_talk_record_async_run_guard();
CREATE FUNCTION public.fn_talk_record_provider_task_cancel() RETURNS trigger
 LANGUAGE plpgsql SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
 UPDATE public.talk_record_provider_task SET cancelled_at=coalesce(cancelled_at,now()),
 revision=revision+1,updated_at=now() WHERE job_id=NEW.id AND cancelled_at IS NULL;
 RETURN NEW;
END; $$;
CREATE TRIGGER talk_record_provider_task_cancel AFTER UPDATE OF state ON public.talk_record_job
 FOR EACH ROW WHEN (NEW.state='cancelled' AND OLD.state IS DISTINCT FROM NEW.state)
 EXECUTE FUNCTION public.fn_talk_record_provider_task_cancel();
GRANT SELECT,INSERT,UPDATE ON public.talk_record_provider_task TO v5_app_runtime;
GRANT SELECT ON public.talk_record_provider_task TO v5_fn_owner;
ALTER TABLE public.talk_record_provider_task ADD CONSTRAINT provider_task_profile_exact CHECK
 (request_profile='{"audio":{"format":"aac","rate":16000,"bits":16,"channel":1},"request":{"model_name":"bigmodel","ssd_version":"300","enable_speaker_info":true,"show_utterances":true,"enable_itn":true,"enable_punc":true,"enable_ddc":false}}'::jsonb
 AND profile_hash='d723bf03c21bf27f6d35a38d6134c2cdf33ebe4a6700b35c8bd98ec27fc4cfab');
ALTER TABLE public.talk_record_provider_task ADD CONSTRAINT provider_task_assembly_shape CHECK
 ((CASE WHEN jsonb_typeof(assembly)='object' THEN (assembly ?& ARRAY['id','sha256','byte_size','duration_ms','mime_type']
 AND assembly-ARRAY['id','sha256','byte_size','duration_ms','mime_type']='{}'::jsonb
 AND jsonb_typeof(assembly->'id')='number' AND jsonb_typeof(assembly->'sha256')='string'
 AND jsonb_typeof(assembly->'byte_size')='number' AND jsonb_typeof(assembly->'duration_ms')='number'
 AND jsonb_typeof(assembly->'mime_type')='string'
 AND (assembly->>'id') ~ '^[1-9][0-9]*$' AND (CASE WHEN jsonb_typeof(assembly->'id')='number' THEN (assembly->>'id')::numeric END) BETWEEN 1 AND 9223372036854775807
 AND assembly->>'sha256' ~ '^[0-9a-f]{64}$'
 AND (assembly->>'byte_size') ~ '^[1-9][0-9]*$'
 AND (CASE WHEN jsonb_typeof(assembly->'byte_size')='number' THEN (assembly->>'byte_size')::numeric END)>0 AND (CASE WHEN jsonb_typeof(assembly->'byte_size')='number' THEN (assembly->>'byte_size')::numeric END)<134217728
 AND (assembly->>'duration_ms') ~ '^[1-9][0-9]*$'
 AND (CASE WHEN jsonb_typeof(assembly->'duration_ms')='number' THEN (assembly->>'duration_ms')::numeric END)>0 AND (CASE WHEN jsonb_typeof(assembly->'duration_ms')='number' THEN (assembly->>'duration_ms')::numeric END)<=7200000
 AND assembly->>'mime_type'='audio/aac') ELSE false END) IS TRUE);
CREATE FUNCTION public.fn_talk_record_provider_task_insert_guard() RETURNS trigger
 LANGUAGE plpgsql SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
 IF NOT EXISTS(SELECT 1 FROM public.talk_record_client_run r
 JOIN public.talk_record_job j ON j.id=r.job_id AND j.record_id=r.record_id
 WHERE r.id=NEW.source_run_id AND r.record_id=NEW.record_id AND r.job_id=NEW.job_id
 AND r.attempt=NEW.attempt AND r.actor_user_id=NEW.actor_user_id AND r.execution_purpose='submit'
 AND r.state='active' AND r.lease_until>clock_timestamp() AND r.chunk_plan_hash IS NOT NULL
 AND r.provider=NEW.provider AND r.model=NEW.model AND r.consent_event_id=NEW.consent_event_id
 AND r.policy_version=NEW.policy_version AND r.config_version=NEW.config_version
 AND r.input_manifest_hash=NEW.input_manifest_hash AND r.input_content_revision=NEW.input_content_revision
 AND r.input_transcript_version IS NOT DISTINCT FROM NEW.input_transcript_version
 AND j.kind='transcribe' AND j.execution_mode='client_byok' AND j.state='running'
 AND r.ack_possible_duplicate_charge=NEW.ack_possible_duplicate_charge
 AND (NEW.previous_task_id IS NOT NULL OR NOT EXISTS(SELECT 1 FROM public.talk_record_provider_task p
  WHERE p.job_id=NEW.job_id AND p.intent_id IS NOT NULL))
 AND ((NEW.previous_task_id IS NULL AND r.replaces_run_id IS NULL) OR EXISTS(
  SELECT 1 FROM public.talk_record_provider_task p WHERE p.id=NEW.previous_task_id
  AND p.source_run_id=r.replaces_run_id AND p.job_id=r.job_id AND p.record_id=r.record_id
  AND NOT EXISTS(SELECT 1 FROM public.talk_record_provider_task later WHERE later.job_id=p.job_id AND later.id>p.id)
  AND ((NEW.ack_possible_duplicate_charge AND (p.state='rejected' OR (p.state='submit_outcome_unknown' AND p.provider_task_id IS NULL)))
   OR (NOT NEW.ack_possible_duplicate_charge AND p.state='prepared' AND p.intent_id IS NULL
    AND p.request_hash IS NULL AND p.provider_task_id IS NULL AND p.source_issued_at IS NULL
    AND p.source_expires_at IS NULL AND p.result IS NULL AND p.result_hash IS NULL
    AND p.outcome_unknown_at IS NULL AND p.cancelled_at IS NULL))))) THEN
 RAISE EXCEPTION 'provider task source custody mismatch' USING ERRCODE='23514'; END IF;
 RETURN NEW;
END; $$;
CREATE TRIGGER talk_record_provider_task_insert_guard BEFORE INSERT ON public.talk_record_provider_task
 FOR EACH ROW EXECUTE FUNCTION public.fn_talk_record_provider_task_insert_guard();
