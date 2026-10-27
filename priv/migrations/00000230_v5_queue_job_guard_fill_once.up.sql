-- V5.5 forward fix (W1-02): fn_v5_queue_job_guard refused the FIRST
-- NULL->value storage fill, deadlocking fn_v5_proof_activate_ordinary
-- (its only legitimate UPDATE is exactly that one-time fill, performed under
-- the function's own fail-closed preconditions: blocked/attempts=0/no lease/
-- legacy_location_unverified/v5_proof-only EXECUTE/attachment live or
-- delete_intent).  Identity immutability now starts AFTER the fill: any
-- change while OLD already carries a non-NULL storage tuple stays rejected
-- with 23510, and the DELETE tombstone and done/revive/attempts rules are
-- unchanged.  Shared by trg_v5_aod_job_guard and trg_v5_ebdq_job_guard;
-- ebdq has no NULL->value UPDATE path (copy_eb_recovery_tombstone INSERTs),
-- so it only gains the same fill-once semantics.
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

CREATE OR REPLACE FUNCTION public.fn_v5_queue_job_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path = public AS $$
BEGIN
    IF TG_OP = 'UPDATE' THEN
        IF (NEW.storage_driver, NEW.storage_namespace, NEW.storage_container,
            NEW.storage_locator_sha256)
           IS DISTINCT FROM
           (OLD.storage_driver, OLD.storage_namespace, OLD.storage_container,
            OLD.storage_locator_sha256)
           AND (OLD.storage_driver IS NOT NULL
                OR OLD.storage_namespace IS NOT NULL
                OR OLD.storage_container IS NOT NULL
                OR OLD.storage_locator_sha256 IS NOT NULL) THEN
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
