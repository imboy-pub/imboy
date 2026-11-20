LOCK TABLE public.user_deletion_job IN ACCESS EXCLUSIVE MODE;

DO $$
BEGIN
    IF EXISTS (
        SELECT 1 FROM public.user_deletion_job WHERE request_status_at_execution IS NOT NULL
    ) THEN
        RAISE EXCEPTION 'cannot drop persisted account deletion request history';
    END IF;
END
$$;

ALTER TABLE public.user_deletion_job
    DROP CONSTRAINT user_deletion_job_request_snapshot_check,
    DROP COLUMN request_status_at_execution,
    DROP COLUMN request_approved_at;
