-- Forward fix rollback: restore the migration-000179 guard body (it rejects
-- ANY storage-tuple change, including the first NULL->value fill).  Reverting
-- re-deadlocks fn_v5_proof_activate_ordinary; kept for provenance symmetry
-- with the other V5 CREATE OR REPLACE down files.
SET LOCAL lock_timeout = '5s';

CREATE OR REPLACE FUNCTION public.fn_v5_queue_job_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path = public AS $$
BEGIN
    IF TG_OP = 'UPDATE' THEN
        IF (NEW.storage_driver, NEW.storage_namespace, NEW.storage_container,
            NEW.storage_locator_sha256)
           IS DISTINCT FROM
           (OLD.storage_driver, OLD.storage_namespace, OLD.storage_container,
            OLD.storage_locator_sha256) THEN
            RAISE EXCEPTION 'v5 job identity immutable after fill'
                USING ERRCODE = '23510';
        END IF;
        IF NEW.job_state = 'done' AND OLD.job_state <> 'done'
           AND NEW.completed_at IS NULL THEN
            RAISE EXCEPTION 'v5 done requires completed_at' USING ERRCODE = '23514';
        END IF;
        IF OLD.job_state = 'done' AND NEW.job_state <> 'done' THEN
            RAISE EXCEPTION 'v5 done tombstone cannot revive'
                USING ERRCODE = '23510';
        END IF;
        IF TG_TABLE_NAME = 'attachment_object_deletion' THEN
            IF NEW.attempts < OLD.attempts THEN
                RAISE EXCEPTION 'v5 attempts cannot reset' USING ERRCODE = '23510';
            END IF;
        END IF;
    END IF;
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION 'v5 job rows are permanent tombstones (no DELETE)'
            USING ERRCODE = '23510';
    END IF;
    RETURN NEW;
END;
$$;
