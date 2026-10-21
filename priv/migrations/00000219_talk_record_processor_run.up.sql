-- Execution/cleanup proof only; no duplicate attachment metadata or delete worker.
CREATE TABLE talk_record_processor_run (
    job_id bigint NOT NULL REFERENCES talk_record_job(id),
    attempt integer NOT NULL CHECK(attempt>0),
    lease_token uuid NOT NULL,
    state text NOT NULL CHECK(state IN ('prepared','running','stopped','unknown')),
    os_pid integer,
    created_at timestamptz NOT NULL DEFAULT now(),
    updated_at timestamptz NOT NULL DEFAULT now(),
    PRIMARY KEY(job_id,attempt)
);
CREATE FUNCTION fn_talk_record_processor_run_transition() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
    IF ROW(NEW.job_id,NEW.attempt,NEW.lease_token) IS DISTINCT FROM ROW(OLD.job_id,OLD.attempt,OLD.lease_token)
       OR (NEW.state<>OLD.state AND NOT (
         (OLD.state='prepared' AND NEW.state IN ('running','stopped','unknown')) OR
         (OLD.state='running' AND NEW.state IN ('stopped','unknown')) OR
         (OLD.state='unknown' AND NEW.state='stopped'))) THEN
        RAISE EXCEPTION 'invalid processor execution transition' USING ERRCODE='23514';
    END IF;
    RETURN NEW;
END $$;
CREATE TRIGGER talk_record_processor_run_transition BEFORE UPDATE ON talk_record_processor_run
FOR EACH ROW EXECUTE FUNCTION fn_talk_record_processor_run_transition();
GRANT SELECT ON talk_record_processor_run TO v5_fn_owner;
GRANT SELECT,INSERT,UPDATE ON talk_record_processor_run TO v5_app_runtime;
